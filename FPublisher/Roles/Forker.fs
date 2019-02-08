namespace FPublisher.Roles
open FPublisher
open FPublisher.Nuget
open Fake.IO.FileSystemOperators
open Primitives
open Fake.Core
open FPublisher.GitHub
open FPublisher.FakeHelper.Build
open System.Threading.Tasks
open Octokit
open FSharp.Control.Tasks.V2.ContextInsensitive
open Fake.Api
open FPublisher.Git

 

[<RequireQualifiedAccess>]
module Forker =

    type GitHubData =
        { Topics: Topics 
          Repository: Repository
          License: RepositoryContentLicense }

    [<RequireQualifiedAccess>]
    module GitHubData =
        let fetch workspace githubTokenEnv = task {
                               
            let githubToken = Environment.environVarOrFail githubTokenEnv
            
            let! client = GitHub.createClientWithToken githubToken

            let! repository = 
                let repoName = Workspace.repoName workspace 
                GitHubClient.repository repoName client

            let! license = 
                let logger = repository.Owner.Login
                client.Repository.GetLicenseContents(logger, repository.Name)
            
            let! topics = Repository.topicsAsync repository
            
            return 
                { Topics = topics
                  Repository = repository
                  License = license }
        }

    type VersionController =
        { Workspace: Workspace
          VersionFromOfficalNugetServer: SemVerInfo option
          LastReleaseNotes: ReleaseNotes.ReleaseNotes
          TbdReleaseNotes: ReleaseNotes.ReleaseNotes
          GitHubData: GitHubData }

    with   

        member x.VersionFromLastReleaseNotes = x.LastReleaseNotes.SemVer

        member x.VersionFromTbdReleaseNotes = x.TbdReleaseNotes.SemVer
   
        member x.ReleaseNotesFile = x.Workspace.WorkingDir </> "RELEASE_NOTES.md"

        member x.WorkingDir = x.Workspace.WorkingDir


    [<RequireQualifiedAccess>]
    module VersionController =

        let versionFromLocalNugetServer solution (localNugetServer: LocalNugetServer) (versionController: VersionController) = async {
            logger.Infots "Begin fetch version from local nuget server"
            let! versionFromLocalNugetServer = 
                NugetServer.getLastVersion solution localNugetServer.AsNugetServer
            logger.Infots "End fetch version from local nuget server"
            return versionFromLocalNugetServer
        }

        let currentVersion (versionFromLocalNugetServer: SemVerInfo option) versionController = 
            [ versionController.VersionFromOfficalNugetServer
              versionFromLocalNugetServer
              Some versionController.VersionFromLastReleaseNotes ]
            |> List.choose id
            |> List.max

        let nextReleaseNotes nextVersion versionController =
            versionController.TbdReleaseNotes
            |> ReleaseNotes.updateWithSemVerInfo (nextVersion)
            |> ReleaseNotes.updateDateToToday  

        let fetch solution githubTokenEnv workspace =
            let task = 
                task {
                    logger.Infots "Begin fetch forker versionController"
                    logger.Infots "Begin fetch github data"
                    let! githubData = GitHubData.fetch workspace githubTokenEnv 
                    logger.Infots "End fetch github data"        

                    let tbdReleaseNotes,lastReleaseNotes = 
                        let releaseNotesFile = workspace.WorkingDir </> "RELEASE_NOTES.md"
                        ReleaseNotes.loadTbd releaseNotesFile,ReleaseNotes.load releaseNotesFile

                    logger.Infots "Begin fetch version from nuget server"
                    let! versionFromOfficalNugetServer = Solution.lastVersionFromOfficalNugetServer solution
                    logger.Infots "End fetch version from nuget server"
                    logger.Infots "End fetch forker versionController"



                    return
                        { GitHubData = githubData 
                          Workspace = workspace
                          TbdReleaseNotes = tbdReleaseNotes
                          LastReleaseNotes = lastReleaseNotes
                          VersionFromOfficalNugetServer = versionFromOfficalNugetServer }
                }
            task  

    [<RequireQualifiedAccess>]
    type Msg =
        | NonGit of NonGit.Msg
        | Pack of nextReleaseNotes: ReleaseNotes.ReleaseNotes
        | PublishToLocalNugetServer of LocalNugetServer

    let (!^) (nonGitMsg: NonGit.Msg) = Msg.NonGit nonGitMsg

    let upcastMsg (nonGitMsg: NonGit.Msg) = Msg.NonGit nonGitMsg

    type TargetState =
        { NonGit: NonGit.TargetState
          Pack: BoxedState
          PublishToLocalNugetServer: BoxedState }

    [<RequireQualifiedAccess>]
    module TargetState =
        let init =
            { NonGit = NonGit.TargetState.init
              Pack = State.Init
              PublishToLocalNugetServer = State.Init }

    type Role =
        { NonGit: NonGit.Role
          TargetState: TargetState
          NugetPacker: NugetPacker
          VersionController: Task<VersionController>
          GitTokenEnv: string }
    with
        member x.Solution = x.NonGit.Solution

        member x.Workspace = x.NonGit.Workspace

        member x.GitHubData = x.VersionController.Result.GitHubData

        interface IRole<TargetState>

    [<RequireQualifiedAccess>]
    module Role =
        let nextVersion versionFromLocalNugetServer (role: Role) =
            let versionController = role.VersionController.Result
            let currentVersion = VersionController.currentVersion versionFromLocalNugetServer versionController
            SemVerInfo.nextBuildVersion currentVersion

        let nextVersionWithFetch localNugetServer (role: Role) = async {
            let! versionFromLocalNugetServer = VersionController.versionFromLocalNugetServer role.Solution localNugetServer role.VersionController.Result
            return nextVersion versionFromLocalNugetServer role
        }
        
        let nextReleaseNotes versionFromLocalNugetServer role =
            let nextVersion = nextVersion versionFromLocalNugetServer role
            VersionController.nextReleaseNotes nextVersion role.VersionController.Result

        let nextReleaseNotesWithFetch localNugetServer (role: Role) = async {
            let! versionFromLocalNugetServer = VersionController.versionFromLocalNugetServer role.Solution localNugetServer role.VersionController.Result
            return nextReleaseNotes versionFromLocalNugetServer role
        }



    let create githubTokenEnv nugetPacker (workspace: Workspace) = 
        let nonGit = NonGit.create workspace 

        { NonGit = nonGit
          NugetPacker = nugetPacker
          TargetState = TargetState.init
          VersionController = VersionController.fetch nonGit.Solution githubTokenEnv workspace
          GitTokenEnv = githubTokenEnv }
        
    let internal roleAction role = function
        | Msg.NonGit nonGitMsg ->
            let roleAction = NonGit.roleAction nonGitMsg
            { PreviousMsgs = []
              Action = fun role -> 
                roleAction.Action role.NonGit
                none } 

        | Msg.Pack nextReleaseNotes -> 
            { PreviousMsgs = [!^ NonGit.Msg.Test]
              Action = fun role -> 
                let githubData = role.GitHubData

                NugetPacker.pack 
                    role.Solution 
                    true 
                    githubData.Topics 
                    githubData.License 
                    githubData.Repository
                    nextReleaseNotes
                    role.NugetPacker
                |> box
            } 

        | Msg.PublishToLocalNugetServer localNugetServer ->
            let nextReleaseNotes = Role.nextReleaseNotesWithFetch localNugetServer role |> Async.RunSynchronously
            { PreviousMsgs = [ Msg.Pack nextReleaseNotes ]
              Action = fun role -> 
                let newPackages = State.getResult role.TargetState.Pack
                NugetServer.publish newPackages localNugetServer.AsNugetServer |> Async.RunSynchronously
                none
            } 

    let run = Role.updateComplex roleAction