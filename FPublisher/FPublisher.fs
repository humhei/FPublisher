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
        | WriteReleaseNotes
        | DraftAndPublish

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

        let private setStatus versionStatus (versionController: VersionController) =
            { versionController with Status = versionStatus }

        let writeReleaseNotesToNextVersionAndPushToRemoteRepository (versionController: VersionController) = 
            match versionController.Status with 
            | VersionControllerStatus.Init ->            
                match versionController.PublishTarget with 
                | PublishTarget.Build -> ()
                | PublishTarget.Release ->

                    let nextVersion = versionController.NextVersion

                    let releaseNotesFile = versionController.ReleaseNotesFile

                    let releaseNotes = ReleaseNotes.updateWithSemVerInfo nextVersion versionController.ReleaseNotes

                    ReleaseNotes.writeToNext versionController.ReleaseNotesFile releaseNotes

                    Workspace.git (sprintf "add %s" releaseNotesFile) versionController.Workspace |> ignore

                    Commit.exec versionController.WorkingDir (sprintf "Bump version to %s" nextVersion.AsString)

                    Branches.push versionController.WorkingDir

                { versionController with Status = VersionControllerStatus.WriteReleaseNotes }                

            | VersionControllerStatus.WriteReleaseNotes
            | VersionControllerStatus.DraftAndPublish -> versionController

        let gitHubDraftAndPublish (versionController: VersionController) = async {
            match versionController.Status with
            | VersionControllerStatus.Init -> return failwith "invalid token"
            | VersionControllerStatus.WriteReleaseNotes ->
                let versionController = writeReleaseNotesToNextVersionAndPushToRemoteRepository versionController

                match versionController.PublishTarget with 
                | PublishTarget.Build -> ()
                | PublishTarget.Release ->
                    let releaseNotes =  
                        versionController.ReleaseNotes
                        |> ReleaseNotes.updateDateToToday

                    do! GitHubData.draftAndPublishWithNewRelease releaseNotes versionController.GitHubData
                return setStatus VersionControllerStatus.DraftAndPublish versionController

            | VersionControllerStatus.DraftAndPublish -> return versionController               
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

            let! versionFromNugetServer = Workspace.versionFromNugetServer workspace   

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
    type PublisherStatus =
        | Init
        | Packed
        | Published

    type Publisher =
        { PaketGitHubServerPublisher: PaketGitHubServerPublisher option
          NugetPacker: NugetPacker
          NugetPublisher: NugetPublisher
          Status: PublisherStatus
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

        let ensureGitChangesAllPushedWhenRelease (publisher: Publisher) =
            match (Workspace.repoState publisher.Workspace, publisher.PublishTarget) with 
            | RepoState.Changed, PublishTarget.Release -> failwith "Please push all changes to git server before you draft new a release"
            | _ -> ()

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

            let workspace = Workspace publisherConfig.WorkingDir 
            
            Logger.info "Begin fetch github data" publisherConfig.Logger
            let! githubData = GitHubData.fetch workspace publisherConfig.EnvironmentConfig
            Logger.info "End fetch github data" publisherConfig.Logger
            Logger.infofn publisherConfig.Logger "Github data is %A"  githubData

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
            Logger.infofn publisherConfig.Logger "VersionController is %A" versionController 

            return 
                { PaketGitHubServerPublisher = paketGitHubServerPublisher
                  NugetPacker = publisherConfig.NugetPacker
                  NugetPublisher = { ApiEnvironmentName = publisherConfig.EnvironmentConfig.NugetApiKey }                
                  Status = PublisherStatus.Init
                  VersionController = versionController }
        }

        let create (buidingPublisherConfig: PublisherConfig -> PublisherConfig) = 
            let task = createAsync buidingPublisherConfig
            task.Result


        let traceGitHubData (publisher: Publisher) =

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

        let writeReleaseNotesToNextVersionAndPushToRemoteRepository publisher =
            { publisher with 
                VersionController = VersionController.writeReleaseNotesToNextVersionAndPushToRemoteRepository publisher.VersionController }

        let pack (publisher: Publisher) =
            ensureGitChangesAllPushedWhenRelease publisher
            
            match publisher.Status with 
            | PublisherStatus.Init -> 
                let publisher = writeReleaseNotesToNextVersionAndPushToRemoteRepository publisher 

                let packer = publisher.NugetPacker
                
                match (nugetPackageState publisher, publisher.PaketGitHubServerPublisher) with 
                | NugetPackageState.Changed, Some paketGitHubServerPublisher -> 
                    let newJsonCacheText = 
                        let newMap = Map.add publisher.RepoName (publisher.GitHubData.CommitHashLocal, publisher.NextVersionText) paketGitHubServerPublisher.PackageVersionMap
                        JsonConvert.SerializeObject(newMap)

                    File.writeString false paketGitHubServerPublisher.PackageVersionCacheFile newJsonCacheText
                    
                    NugetPacker.pack publisher.Workspace publisher.GitHubData paketGitHubServerPublisher.StoreDir publisher.ReleaseNotes packer
                    
                | NugetPackageState.Changed, None ->
                    NugetPacker.pack publisher.Workspace publisher.GitHubData (Path.GetTempPath()) publisher.ReleaseNotes packer
                | _ -> Trace.trace "find packages in paketGitHubServer: all packages are up to date"
                
                { publisher with 
                    Status = PublisherStatus.Packed }

            | PublisherStatus.Packed 
            | PublisherStatus.Published -> publisher  


        let nextPackagePaths publisher = 
            match packTargetDir publisher with 
            | Some targetDir ->
                let packageNames = Workspace.nugetPackageNames publisher.Workspace
                packageNames 
                |> Seq.map (fun packageName -> targetDir </> packageName + publisher.NextVersionText + ".nupkg") 
                |> List.ofSeq
                |> Some

            | None -> None            


        let publishToNugetServer publisher = 
            ensureGitChangesAllPushedWhenRelease publisher
            let publisher = pack publisher
            match publisher.Status with 
            | PublisherStatus.Init 
            | PublisherStatus.Published -> failwith "invalid token"
            | PublisherStatus.Packed ->
                match (nextPackagePaths publisher, publisher.PublishTarget) with 
                | Some newPackages, PublishTarget.Release -> NugetPublisher.publish newPackages publisher.NugetPublisher
                | _ -> async {()}
 
        let publishToPaketGitHubServer publisher =
            ensureGitChangesAllPushedWhenRelease publisher
            let publisher = pack publisher
            match publisher.Status with 
            | PublisherStatus.Init 
            | PublisherStatus.Published -> failwith "invalid token"
            | PublisherStatus.Packed ->
                match publisher.PaketGitHubServerPublisher with 
                | Some paketGithubServer -> 
                    let packageNames = Workspace.nugetPackageNames publisher.Workspace

                    PaketGitHubServerPublisher.publish packageNames publisher.RepoName publisher.NextVersion paketGithubServer
                | _ -> async {()}

        let private summary (elapsed: int64) publisher = 
            [ sprintf "current publisher status is %A" publisher.Status
              sprintf "current version controller status is %A" publisher.VersionController.Status
              sprintf "elapsed %dms" elapsed ]
            |> String.concat "\n"          
    

        /// publish and draft all
        let publishAndDraftAllAsync publisher = async {
            traceGitHubData publisher
            let stopwatch = Stopwatch.StartNew()

            let publisher = pack publisher 

            let! publishTask1 = publishToNugetServer publisher |> Async.StartChild
            let! publishTask2 = publishToPaketGitHubServer publisher |> Async.StartChild
            let! publishTask3 = VersionController.gitHubDraftAndPublish publisher.VersionController |> Async.StartChild
            
            let! _ = publishTask1
            let! _ = publishTask2
            let! versionController = publishTask3

            let result = 
                {publisher with 
                    VersionController = versionController
                    Status = PublisherStatus.Published }

            Trace.trace (summary stopwatch.ElapsedMilliseconds result)
        }

        /// publish and draft all
        let publishAndDraftAll publisher = 
            publishAndDraftAllAsync publisher
            |> Async.RunSynchronously

        let updateDependencies publisher = 
            let workspace = publisher.Workspace
            Workspace.createSln workspace

            let fakeCacheDir = workspace.FakeCacheDir
            !! (fakeCacheDir + "./**/assemblies.txt")
            |> File.deleteAll 

            Workspace.paket ["update"] workspace
            Workspace.fake ["build"] workspace
            Workspace.dotnet "restore" [workspace.SlnPath] workspace                     

