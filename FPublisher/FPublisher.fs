namespace FPublisher
open Fake.Core
open Fake.Tools.Git
open Microsoft.FSharp.Core.Operators
open Fake.IO
open Utils
open Newtonsoft.Json
open Fake.IO.FileSystemOperators
open FakeHelper
open GitHub
open FakeHelper.Build
open Fake.IO.Globbing.Operators
open System.IO
open Nuget
open Git
open FSharp.Control.Tasks.V2.ContextInsensitive
open Types
open System.Diagnostics
open FPublisher

module FPublisher =


    /// paket github feed          
    /// e.g. https://github.com/humhei/Paket_NugetServer/tree/NugetStore/
    /// 
    type PaketGitHubServerPublisher = 
        { Workspace: Workspace
          BranchName: string }
    with 
        member x.RepoDir = x.Workspace.WorkingDir

        member x.PackageVersionCacheFile = x.RepoDir </> "hash.json"

        member x.PackageVersionMap = 
            if not <| File.exists x.PackageVersionCacheFile then Map.empty
            else
                JsonConvert.DeserializeObject<Map<string,string * string>>(File.readAsString x.PackageVersionCacheFile)     
        
        member x.StoreDir =
            x.RepoDir </> ".nuget"
            |> Path.getFullName
            |> Directory.ensureReturn 

    [<RequireQualifiedAccess>]                    
    module PaketGitHubServerPublisher =

        let private commitMsg neighborRepoName (version: SemVerInfo) =
            sprintf "Bump %s version to %s" neighborRepoName (SemVerInfo.normalize version)
        
        let allPackages (paketGitHubServerPublisher: PaketGitHubServerPublisher) = 
            !! (paketGitHubServerPublisher.StoreDir + "/*.nupkg")
            |> List.ofSeq
            |> List.choose NugetPackage.tryCreateByPath

        let currentPackages neighborPackageNames (paketGitHubServerPublisher: PaketGitHubServerPublisher) =
            allPackages paketGitHubServerPublisher
            |> List.filter (fun nugetPackage -> 
                List.contains nugetPackage.Name neighborPackageNames
            )

        let lastestPackages neighborPackageNames (paketGitHubServerPublisher: PaketGitHubServerPublisher) =
            currentPackages neighborPackageNames paketGitHubServerPublisher
            |> List.groupBy (fun nugetPackage -> nugetPackage.Name)
            |> List.map (fun (_,nugetPackages) -> 
                nugetPackages |> Seq.maxBy (fun nugetPackage -> nugetPackage.Version)    
            )
            
        let lastestVersion neighborPackageNames (paketGitHubServerPublisher: PaketGitHubServerPublisher) =
            let lastPackages = 
                lastestPackages neighborPackageNames paketGitHubServerPublisher
                |> List.ofSeq
            
            match Seq.length lastPackages with
            | i when i > 0 -> 
                lastPackages
                |> Seq.maxBy (fun package -> package.Version)
                |> fun package -> package.Version 
                |> Some
            | 0 -> None
            | _ -> failwith "invalid token"   

        let publish neighborPackageNames neighborRepoName (nextVersion: SemVerInfo) (paketGitHubServerPublisher: PaketGitHubServerPublisher) =
            let workspace = paketGitHubServerPublisher.Workspace  
            
            let repoState = Workspace.repoState workspace       
            
            match repoState with 
            | RepoState.Changed ->
                Branches.checkoutBranch paketGitHubServerPublisher.RepoDir paketGitHubServerPublisher.BranchName
                
                let currentPackages = currentPackages neighborPackageNames paketGitHubServerPublisher
                
                let newPackages, oldPackages = 
                    currentPackages |> List.ofSeq |> List.partition (fun (nugetPackage) -> 
                        nugetPackage.Version = nextVersion
                    )

                oldPackages
                |> List.map (fun nugetPackage -> nugetPackage.Path)
                |> File.deleteAll

                ( Workspace.gitPush (commitMsg neighborRepoName nextVersion) workspace )

            | RepoState.None -> async {()}



    [<RequireQualifiedAccess>]
    type PublishTarget =
        | Release 
        | Build

    type RetrievedVersionInfo =
        { FromPaketGitHubServer: SemVerInfo option 
          FromReleaseNotes: SemVerInfo
          FromNugetServer: SemVerInfo option }

    [<RequireQualifiedAccess>]      
    module RetrievedVersionInfo = 
        let currentVersion retrievedVersionInfo =
            [ retrievedVersionInfo.FromPaketGitHubServer
              Some retrievedVersionInfo.FromReleaseNotes
              retrievedVersionInfo.FromNugetServer ]
            |> List.choose id
            |> List.max     

        let nugetOrPaketGitHubServerVersion retrievedVersionInfo =
            [ retrievedVersionInfo.FromPaketGitHubServer
              retrievedVersionInfo.FromNugetServer ]
            |> List.choose id
            |> function 
                | versions when versions.Length > 0 -> Some (List.max versions)
                | _ -> None 

        let nextVersion (releaseNotes: ReleaseNotes.ReleaseNotes) publishTarget retrievedVersionInfo =
            let currentVersion = currentVersion retrievedVersionInfo
            
            let nextVersion = 
                match publishTarget with 
                | PublishTarget.Build -> 
                    SemVerInfo.nextBetaVersion currentVersion
                | PublishTarget.Release -> 
                    
                    if releaseNotes.Notes.IsEmpty || releaseNotes.Date <> None 
                    then failwith "Please write release notes of new version first"

                    let releaseVersion = retrievedVersionInfo.FromReleaseNotes

                    match nugetOrPaketGitHubServerVersion retrievedVersionInfo with 
                    | None ->
                        match releaseVersion.PreRelease with
                        | Some _ -> SemVerInfo.nextBetaVersion releaseVersion
                        | None -> releaseVersion
                        
                    | Some nugetVersion ->
                        if releaseVersion < nugetVersion 
                        then SemVerInfo.nextBetaVersion nugetVersion                         
                        else 
                            match releaseVersion.PreRelease with 
                            | Some _ -> SemVerInfo.nextBetaVersion releaseVersion
                            | None -> releaseVersion   

            if nextVersion >= currentVersion then nextVersion      
            else failwithf "next version %s is smaller than current version %s" nextVersion.AsString currentVersion.AsString                           


             
        let currentVersionText retrievedVersionInfo =
            currentVersion retrievedVersionInfo
            |> SemVerInfo.normalize

    [<RequireQualifiedAccess>]
    type VersionControllerStatus =
        | Init
        | WriteReleaseNotesToNextVersionAndPushToRemoteRepository of ReleaseNotes.ReleaseNotes
        | GitHubDraftAndPublishWhenRelease of ReleaseNotes.ReleaseNotes

    type VersionController =
        { VersionFromPaketGitHubServer: SemVerInfo option
          PublishTarget: PublishTarget
          Workspace: Workspace
          VersionFromNugetServer: SemVerInfo option
          CurrentVersion: SemVerInfo
          ReleaseNotes: ReleaseNotes.ReleaseNotes
          NextVersion: SemVerInfo
          GitHubData: GitHubData
          Status: VersionControllerStatus } 
    with 
        member x.ReleaseNotesFile = x.Workspace.WorkingDir </> "RELEASE_NOTES.md"

        member x.VersionFromReleaseNotes = x.ReleaseNotes.SemVer  

        member x.WorkingDir: string = x.Workspace.WorkingDir
        

    [<RequireQualifiedAccess>]
    module VersionController = 


        let internal writeReleaseNotesToNextVersionAndPushToRemoteRepositoryTupledStatus (versionController: VersionController) = 
            match versionController.Status with 
            | VersionControllerStatus.Init ->            
                match versionController.PublishTarget with 
                | PublishTarget.Build -> 
                    let releaseNotes = versionController.ReleaseNotes
                    (
                        { versionController with 
                            Status = 
                                VersionControllerStatus.WriteReleaseNotesToNextVersionAndPushToRemoteRepository 
                                    releaseNotes },
                        releaseNotes
                    )
                | PublishTarget.Release ->

                    let nextVersion = versionController.NextVersion

                    let releaseNotesFile = versionController.ReleaseNotesFile

                    let releaseNotesWithNextVersion = ReleaseNotes.updateWithSemVerInfo nextVersion versionController.ReleaseNotes

                    ReleaseNotes.writeToNext versionController.ReleaseNotesFile releaseNotesWithNextVersion

                    Workspace.git (sprintf "add %s" releaseNotesFile) versionController.Workspace |> ignore

                    Commit.exec versionController.WorkingDir (sprintf "Bump version to %s" nextVersion.AsString)

                    Branches.push versionController.WorkingDir
                    (
                        { versionController with 
                            Status = 
                                VersionControllerStatus.WriteReleaseNotesToNextVersionAndPushToRemoteRepository 
                                    releaseNotesWithNextVersion },
                        releaseNotesWithNextVersion
                    )              

            | VersionControllerStatus.WriteReleaseNotesToNextVersionAndPushToRemoteRepository releaseNotes
            | VersionControllerStatus.GitHubDraftAndPublishWhenRelease releaseNotes -> (versionController, releaseNotes)


        let writeReleaseNotesToNextVersionAndPushToRemoteRepository (versionController: VersionController) =
            writeReleaseNotesToNextVersionAndPushToRemoteRepositoryTupledStatus versionController
            |> fst


        let rec internal gitHubDraftAndPublishWhenReleaseTupledStatus (versionController: VersionController) = async {
            match versionController.Status with
            | VersionControllerStatus.Init -> 
                return! gitHubDraftAndPublishWhenReleaseTupledStatus (writeReleaseNotesToNextVersionAndPushToRemoteRepository versionController)
            | VersionControllerStatus.WriteReleaseNotesToNextVersionAndPushToRemoteRepository releaseNotes ->

                match versionController.PublishTarget with 
                | PublishTarget.Build -> ()
                | PublishTarget.Release -> do! GitHubData.draftAndPublishWithNewRelease releaseNotes versionController.GitHubData
                return 
                    ({ versionController with 
                            Status = 
                                VersionControllerStatus.GitHubDraftAndPublishWhenRelease releaseNotes }, 
                        releaseNotes)
                            

            | VersionControllerStatus.GitHubDraftAndPublishWhenRelease releaseNotes -> return (versionController, releaseNotes)               
        }

        let gitHubDraftAndPublishWhenRelease publisher = async {
            let! (versionController, _) = gitHubDraftAndPublishWhenReleaseTupledStatus publisher
            return versionController            
        }


    [<RequireQualifiedAccess>]
    type NugetPackageState =
        | Changed
        | None

    type PublisherConfig =
        { NugetPacker: NugetPacker
          WorkingDir: string            
          EnvironmentConfig: EnvironmentConfig
          PublishTarget: PublishTarget
          Logger: Logger
          BuildingPaketGitHubServerPublisher: option<PaketGitHubServerPublisher -> PaketGitHubServerPublisher> }

    with 
        static member DefaultValue = 
            { WorkingDir = Directory.GetCurrentDirectory() 
              EnvironmentConfig = 
                { NugetApiKey = "NugetApiKey" 
                  GitHubToken = "github_token"
                  GitHubReleaseUser = "github_release_user" } 
              NugetPacker = 
                { Authors = NugetAuthors.GithubLoginName 
                  BuildingPackOptions = id 
                  GenerateDocumentationFile = false 
                  PackageIconUrl = None
                  SourceLinkCreate = false }
              PublishTarget = PublishTarget.Build              
              BuildingPaketGitHubServerPublisher = None
              Logger = Logger.Minimal }        

    [<RequireQualifiedAccess>]
    module PublisherConfig =
        let toVersionController githubData paketGitHubServerPublisherOp (publisherConfig: PublisherConfig) = task {
            let workspace = Workspace publisherConfig.WorkingDir 
            
            let packageNames = Workspace.nugetPackageNames workspace
            
            let releaseNotes = 
                let releaseNotesFile = workspace.WorkingDir </> "RELEASE_NOTES.md"
                ReleaseNotes.loadTbd releaseNotesFile

            Logger.infots "Begin fetch version from nuget server"
            let! versionFromNugetServer = Workspace.versionFromNugetServer workspace   
            Logger.infots "End fetch version from nuget server"


            let retrievedVersionInfo =
                { FromNugetServer = versionFromNugetServer
                  FromPaketGitHubServer = 
                    paketGitHubServerPublisherOp
                    |> Option.map (PaketGitHubServerPublisher.lastestVersion packageNames)
                    |> Option.flatten 
                  FromReleaseNotes = releaseNotes.SemVer }

            return 
                { GitHubData = githubData 
                  Workspace = workspace
                  VersionFromPaketGitHubServer = 
                    paketGitHubServerPublisherOp
                    |> Option.map (PaketGitHubServerPublisher.lastestVersion packageNames)
                    |> Option.flatten  
                  ReleaseNotes = releaseNotes                
                  CurrentVersion = RetrievedVersionInfo.currentVersion retrievedVersionInfo
                  NextVersion = RetrievedVersionInfo.nextVersion releaseNotes publisherConfig.PublishTarget retrievedVersionInfo
                  PublishTarget = publisherConfig.PublishTarget
                  VersionFromNugetServer = versionFromNugetServer
                  Status = VersionControllerStatus.Init }
        }

    [<RequireQualifiedAccess>]
    type PublishToServerStatus =
        | NugetServer
        | PaketGitServer 
        | None 
        | All 
        | GitHubPublishAndDraftNewRelease

    with 
        static member  (+) (status1: PublishToServerStatus, status2: PublishToServerStatus) =
            
            match (status1, status2) with 
            | PublishToServerStatus.All, _ -> PublishToServerStatus.All
            | _, PublishToServerStatus.All -> PublishToServerStatus.All
            | PublishToServerStatus.NugetServer, PublishToServerStatus.PaketGitServer -> PublishToServerStatus.All
            | PublishToServerStatus.PaketGitServer, PublishToServerStatus.NugetServer -> PublishToServerStatus.All
            | PublishToServerStatus.None, status -> status
            | status, PublishToServerStatus.None -> status
            | PublishToServerStatus.PaketGitServer, _ -> PublishToServerStatus.PaketGitServer
            | PublishToServerStatus.NugetServer, _ -> PublishToServerStatus.NugetServer
            | _ -> failwith "unexcepted token"

    [<RequireQualifiedAccess>]
    type PublishStatus =
        | Init
        | CheckGitChangesAllPushedWhenRelease
        | WriteReleaseNotesToNextVersionAndPushToRemoteRepository of ReleaseNotes.ReleaseNotes
        | Packed of ReleaseNotes.ReleaseNotes
        | PublishToServerStatus of PublishToServerStatus

    type Publisher =
        { PaketGitHubServerPublisher: PaketGitHubServerPublisher option
          NugetPacker: NugetPacker
          NugetPublisher: NugetPublisher
          Status: PublishStatus
          VersionController: VersionController }
    with 
        member x.VersionFromNugetServer = x.VersionController.VersionFromNugetServer

        member x.ReleaseNotesFile = x.VersionController.ReleaseNotesFile  

        member x.ReleaseNotes = x.VersionController.ReleaseNotes

        member x.NextVersion = x.VersionController.NextVersion

        member x.NextVersionText = SemVerInfo.normalize x.VersionController.NextVersion

        member x.CurrentVersion = x.VersionController.CurrentVersion

        member x.GitHubData = x.VersionController.GitHubData
        
        member x.Workspace = x.VersionController.Workspace

        member x.PublishTarget = x.VersionController.PublishTarget

        member x.RepoName = x.GitHubData.RepoName

        member x.WorkingDir = x.Workspace.WorkingDir

        member x.PackageNames = Workspace.nugetPackageNames x.Workspace

        member x.RetrievedVersionInfo =  
            { FromPaketGitHubServer = 
                x.PaketGitHubServerPublisher 
                |> Option.map (PaketGitHubServerPublisher.lastestVersion x.PackageNames)
                |> Option.flatten
              FromReleaseNotes = x.ReleaseNotes.SemVer
              FromNugetServer = x.VersionFromNugetServer }


    [<RequireQualifiedAccess>]
    module Publisher =

        let setPublishTarget publishTarget (publisher: Publisher) =
            { publisher with 
                VersionController = 
                    {publisher.VersionController with 
                        PublishTarget = publishTarget } }

        let nugetPackageState (publisher: Publisher) = 
            match publisher.PaketGitHubServerPublisher with 
            | None -> NugetPackageState.Changed
            | Some paketGitHubServerPublisher ->
                match Map.tryFind publisher.GitHubData.RepoName paketGitHubServerPublisher.PackageVersionMap with 
                | Some (commitHashLocal, version) ->
                    if commitHashLocal = publisher.GitHubData.CommitHashRemote 
                    then NugetPackageState.None
                    else NugetPackageState.Changed
                | None -> NugetPackageState.Changed            


        let createAsync (buidingPublisherConfig: PublisherConfig -> PublisherConfig) = task {
            
            let publisherConfig = buidingPublisherConfig PublisherConfig.DefaultValue
            
            let publisherConfig = { publisherConfig with WorkingDir = Path.getFullName publisherConfig.WorkingDir }

            Logger.setDefaultLogger publisherConfig.Logger

            Logger.info "fpublisher logger working in %A level" publisherConfig.Logger
            
            let workspace = Workspace publisherConfig.WorkingDir 
            
            Logger.infots "Begin fetch github data"
            let! githubData = GitHubData.fetch workspace publisherConfig.EnvironmentConfig
            Logger.infots "End fetch github data" 
            Logger.info "Github data is %A"  githubData

            let paketGitHubServerPublisher = 
                match (githubData.IsLogin, publisherConfig.BuildingPaketGitHubServerPublisher) with 
                | true, Some buidingPublisherConfig ->
                    { BranchName = "NugetStore"
                      Workspace = 
                        Path.getFullName (workspace.WorkingDir </> "../Paket_NugetServer")
                        |> Workspace }
                    |> buidingPublisherConfig
                    |> Some
                | _ -> None

            let! versionController = PublisherConfig.toVersionController githubData paketGitHubServerPublisher publisherConfig
            Logger.info "VersionController is %A" versionController 

            return 
                { PaketGitHubServerPublisher = paketGitHubServerPublisher
                  NugetPacker = publisherConfig.NugetPacker
                  NugetPublisher = { ApiEnvironmentName = publisherConfig.EnvironmentConfig.NugetApiKey }                
                  Status = PublishStatus.Init
                  VersionController = versionController }
        }



        let create (buidingPublisherConfig: PublisherConfig -> PublisherConfig) = 
            let task = createAsync buidingPublisherConfig
            task.Result


        let traceBaseInfo (publisher: Publisher) =

            let githubData = publisher.GitHubData
            
            [ ("RepoName is " + githubData.RepoName)
              ("Current branch name is " + githubData.BranchName)
              ("Current version is " + RetrievedVersionInfo.currentVersionText publisher.RetrievedVersionInfo)
              ("Next version is " + publisher.NextVersionText) ] 
            |> String.concat "\n"
            |> Trace.trace            
        

        let packTargetDir (publisher: Publisher) =
            match (nugetPackageState publisher, publisher.PaketGitHubServerPublisher) with 
            | NugetPackageState.Changed,Some paketGitHubServerPublisher -> 
                Some paketGitHubServerPublisher.StoreDir
            | NugetPackageState.Changed, None -> Path.GetTempPath() |> Some
            | _ -> None

        let ensureGitChangesAllPushedWhenRelease (publisher: Publisher) =
            match publisher.Status with 
            | PublishStatus.Init ->
                match (Workspace.repoState publisher.Workspace, publisher.PublishTarget) with 
                | RepoState.Changed, PublishTarget.Release -> failwith "Please push all changes to git server before you draft new a release"
                | _ -> { publisher with Status = PublishStatus.CheckGitChangesAllPushedWhenRelease }
            | _ -> publisher

        let rec writeReleaseNotesToNextVersionAndPushToRemoteRepository publisher = 
            match publisher.Status with 
            | PublishStatus.Init -> 
                ensureGitChangesAllPushedWhenRelease publisher 
                |> writeReleaseNotesToNextVersionAndPushToRemoteRepository
            | PublishStatus.CheckGitChangesAllPushedWhenRelease ->       
                let newVersionController, releaseNotes = VersionController.writeReleaseNotesToNextVersionAndPushToRemoteRepositoryTupledStatus publisher.VersionController
                { publisher with 
                    VersionController = newVersionController
                    Status = PublishStatus.WriteReleaseNotesToNextVersionAndPushToRemoteRepository releaseNotes }
            | _ -> publisher

        let rec gitHubDraftAndPublishWhenRelease publisher = async {
            match publisher.Status with 
            | PublishStatus.Init -> 
                return! 
                    ensureGitChangesAllPushedWhenRelease publisher 
                    |> gitHubDraftAndPublishWhenRelease

            | PublishStatus.CheckGitChangesAllPushedWhenRelease ->       
                let! newVersionController = VersionController.gitHubDraftAndPublishWhenRelease publisher.VersionController
                return 
                    { publisher with 
                        VersionController = newVersionController
                        Status = PublishStatus.PublishToServerStatus PublishToServerStatus.GitHubPublishAndDraftNewRelease }
            | _ -> return publisher        
        }

        let rec pack (publisher: Publisher) =
            
            match publisher.Status with 
            | PublishStatus.Init 
            | PublishStatus.CheckGitChangesAllPushedWhenRelease ->
                writeReleaseNotesToNextVersionAndPushToRemoteRepository publisher |> pack

            | PublishStatus.WriteReleaseNotesToNextVersionAndPushToRemoteRepository releaseNotes ->            

                let packer = publisher.NugetPacker
                
                match (nugetPackageState publisher, publisher.PaketGitHubServerPublisher) with 
                | NugetPackageState.Changed, Some paketGitHubServerPublisher -> 
                    let newJsonCacheText = 
                        let newMap = Map.add publisher.RepoName (publisher.GitHubData.CommitHashLocal, publisher.NextVersionText) paketGitHubServerPublisher.PackageVersionMap
                        JsonConvert.SerializeObject(newMap)

                    File.writeString false paketGitHubServerPublisher.PackageVersionCacheFile newJsonCacheText
                    
                    NugetPacker.pack publisher.Workspace publisher.GitHubData paketGitHubServerPublisher.StoreDir releaseNotes packer
                    
                | NugetPackageState.Changed, None ->
                    NugetPacker.pack publisher.Workspace publisher.GitHubData (Path.GetTempPath()) releaseNotes packer
                | _ -> Trace.trace "find packages in paketGitHubServer: all packages are up to date"
                
                { publisher with 
                    Status = PublishStatus.Packed releaseNotes }

            | PublishStatus.Packed _
            | PublishStatus.PublishToServerStatus _ -> publisher  


        let nextPackagePaths publisher = 
            match packTargetDir publisher with 
            | Some targetDir ->
                let packageNames = Workspace.nugetPackageNames publisher.Workspace
                packageNames 
                |> Seq.map (fun packageName -> targetDir </> packageName + publisher.NextVersionText + ".nupkg") 
                |> List.ofSeq
                |> Some

            | None -> None            



        let rec internal publishToNugetServerTupledStatus publisher = async {
            match publisher.Status with 
            | PublishStatus.Init
            | PublishStatus.CheckGitChangesAllPushedWhenRelease 
            | PublishStatus.WriteReleaseNotesToNextVersionAndPushToRemoteRepository _ -> 
                return! publishToNugetServerTupledStatus (pack publisher)
                
            | PublishStatus.Packed releaseNotes ->
                match (nextPackagePaths publisher, publisher.PublishTarget) with 
                | Some newPackages, PublishTarget.Release -> 
                    do! NugetPublisher.publish newPackages publisher.NugetPublisher 

                    let publishToServerStatus = PublishToServerStatus.NugetServer

                    return ({publisher with Status = PublishStatus.PublishToServerStatus publishToServerStatus}, publishToServerStatus) 
                | _ -> 
                    let publishToServerStatus = PublishToServerStatus.None
                    return ({publisher with Status = PublishStatus.PublishToServerStatus publishToServerStatus}, publishToServerStatus) 

                           
            | PublishStatus.PublishToServerStatus publishToServerStatus  -> return (publisher, publishToServerStatus)
        }
        
        let publishToNugetServer publisher = async {
            let! (publisher, _) = publishToNugetServerTupledStatus publisher
            return publisher
        }
 
        let rec internal publishToPaketGitHubServerTupledStatus publisher = async {
            match publisher.Status with 
            | PublishStatus.Init
            | PublishStatus.CheckGitChangesAllPushedWhenRelease 
            | PublishStatus.WriteReleaseNotesToNextVersionAndPushToRemoteRepository _ -> 
                return! publishToPaketGitHubServerTupledStatus (pack publisher)
                
            | PublishStatus.Packed releaseNotes ->
                match publisher.PaketGitHubServerPublisher with 
                | Some paketGithubServer -> 
                    let packageNames = Workspace.nugetPackageNames publisher.Workspace
                    do! PaketGitHubServerPublisher.publish packageNames publisher.RepoName publisher.NextVersion paketGithubServer
                    
                    let publishToServerStatus = PublishToServerStatus.PaketGitServer
                    
                    return 
                        ({publisher with 
                                Status = 
                                    PublishStatus.PublishToServerStatus publishToServerStatus},
                            publishToServerStatus)
                | None ->                
                    let publishToServerStatus = PublishToServerStatus.None
                    return 
                        ({publisher with 
                            Status = 
                                PublishStatus.PublishToServerStatus publishToServerStatus},
                                publishToServerStatus )
                 
            | PublishStatus.PublishToServerStatus publishToServerStatus -> return (publisher, publishToServerStatus)
        }

        let publishToPaketGitHubServer publisher = async {
            let! (publisher, _) = publishToPaketGitHubServerTupledStatus publisher
            return publisher            
        }
            

        let private summary (elapsed: int64) publisher = 
            [ sprintf "current publisher status is %A" publisher.Status
              sprintf "current version controller status is %A" publisher.VersionController.Status
              sprintf "elapsed %dms" elapsed ]
            |> String.concat "\n"          
    

        /// publish and draft all
        let publishAndDraftAllAsync publisher = async {
            let stopwatch = Stopwatch.StartNew()
            traceBaseInfo publisher
            let rec loop publisher = 
                async {
                    Logger.importantts "running target %A" publisher.Status
                    match publisher.Status with 
                    | PublishStatus.Init
                    | PublishStatus.CheckGitChangesAllPushedWhenRelease
                    | PublishStatus.WriteReleaseNotesToNextVersionAndPushToRemoteRepository _ -> 
                        return! loop (pack publisher)
                    | PublishStatus.Packed releaseNotes ->

                        let publisher = pack publisher 

                        let! publishTask1 = publishToNugetServerTupledStatus publisher |> Async.StartChild
                        let! publishTask2 = publishToPaketGitHubServerTupledStatus publisher |> Async.StartChild
                        let! publishTask3 = gitHubDraftAndPublishWhenRelease publisher |> Async.StartChild
                        
                        let! publisher1, publisherToServerStatus1 = publishTask1
                        let! publisher2, publisherToServerStatus2 = publishTask2
                        let! publisherWithNewVersionController = publishTask3

                        let result = 
                            { publisher with 
                                VersionController = publisherWithNewVersionController.VersionController
                                Status = 
                                    publisherToServerStatus1 + publisherToServerStatus2
                                    |> PublishStatus.PublishToServerStatus }
                        return result
                    | PublishStatus.PublishToServerStatus _ -> return publisher            
                }
            let! result = loop publisher   
            Trace.trace (summary stopwatch.ElapsedMilliseconds result)
        }
        /// publish and draft all
        let publishAndDraftAll publisher = 
            publishAndDraftAllAsync publisher
            |> Async.RunSynchronously

        /// update paket and fake dependencies
        let updateDependencies publisher = 
            let workspace = publisher.Workspace
            Workspace.createSln workspace

            let fakeCacheDir = workspace.FakeCacheDir
            !! (fakeCacheDir + "./**/assemblies.txt")
            |> File.deleteAll 

            Workspace.paket ["update"] workspace
            Workspace.fake ["build"] workspace
            Workspace.dotnet "restore" [workspace.SlnPath] workspace                     

