namespace FPublisher
open Fake.Core
open Newtonsoft.Json
open Fake.Api
open Octokit
open FSharp.Data
open FSharp.Data.HttpRequestHeaders
open FSharp.Control.Tasks.V2.ContextInsensitive
open Utils
open Types
open Git

module GitHub = 
    type Topics =
        { names: string list }
    with 
        member x.AsString = String.separated ";" x.names            

    [<RequireQualifiedAccess>]
    module Repository =
        let topics (repository: Repository)  = async { 
            return
                Http.RequestString
                  ( sprintf "https://api.github.com/repos/%s/%s/topics" repository.Owner.Login repository.Name, httpMethod = "GET", headers = 
                        [ Accept "application/vnd.github.mercy-preview+json"
                          UserAgent "userAgent" ] )
                |> JsonConvert.DeserializeObject<Topics> 

        }

    [<RequireQualifiedAccess>]
    module GitHubClient =
        let repository repoName (client: GitHubClient) = task {
            let! searchedRepositoryResult =  
                let request = SearchRepositoriesRequest(repoName)
                client.Search.SearchRepo(request)

            return 
                searchedRepositoryResult.Items 
                |> Seq.find ( fun result -> String.equalIgnoreCaseAndEdgeSpace result.Name repoName )

        }        
        
        let draftAndPublishWithNewRelease user repoName (release: ReleaseNotes.ReleaseNotes) (client: Async<GitHubClient>) =
            client
            |> GitHub.draftNewRelease user repoName (release.SemVer.AsString) (release.SemVer.PreRelease <> None) release.Notes
            |> GitHub.publishDraft

        
    type GitHubData =
        { Topics: Topics 
          Repository: Repository
          License: RepositoryContentLicense
          ReleaseUserName: string
          ReleaseUserToken: string
          BranchName: string
          CommitHashRemote: string
          CommitHashLocal: string }
    with 
        member x.Owner = x.Repository.Owner

        member x.LoginName = x.Owner.Login

        member x.RepoName = x.Repository.Name 

        member x.IsLogin = x.LoginName = x.ReleaseUserName

    [<RequireQualifiedAccess>]              
    module GitHubData =

        let draftAndPublishWithNewRelease (releaseNotes: ReleaseNotes.ReleaseNotes) (gitHubData: GitHubData) =
            GitHub.createClientWithToken gitHubData.ReleaseUserToken
            |> GitHubClient.draftAndPublishWithNewRelease gitHubData.ReleaseUserName gitHubData.RepoName releaseNotes

        let fetch workspace (environmentConfig: EnvironmentConfig) = task {
                               
            let githubToken = Environment.environVarOrFail environmentConfig.GitHubToken
            
            let! client = GitHub.createClientWithToken githubToken

            let! repository = 
                let repoName = Workspace.repoName workspace 
                GitHubClient.repository repoName client

            let! license = 
                let logger = repository.Owner.Login
                client.Repository.GetLicenseContents(logger, repository.Name)
            
            let! topics = Repository.topics repository
            
            let branchName = Workspace.branchName workspace

            return 
                { Topics = topics
                  Repository = repository
                  License = license
                  ReleaseUserName = Environment.environVarOrFail environmentConfig.GitHubReleaseUser
                  ReleaseUserToken = githubToken
                  BranchName = branchName
                  CommitHashLocal = Workspace.commitHashLocal branchName workspace
                  CommitHashRemote = Workspace.commitHashRemote branchName workspace }
        }