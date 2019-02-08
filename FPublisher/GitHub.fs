namespace FPublisher
open Fake.Core
open Newtonsoft.Json
open Fake.Api
open Octokit
open FSharp.Data
open FSharp.Data.HttpRequestHeaders
open FSharp.Control.Tasks.V2.ContextInsensitive
open Utils
open Git

module GitHub = 
    type Topics =
        { names: string list }
    with 
        member x.AsString = String.separated ";" x.names            

    [<RequireQualifiedAccess>]
    module Repository =
        let topicsAsync (repository: Repository)  = async { 
            return
                Http.RequestString
                  ( sprintf "https://api.github.com/repos/%s/%s/topics" repository.Owner.Login repository.Name, httpMethod = "GET", headers = 
                        [ Accept "application/vnd.github.mercy-preview+json"
                          UserAgent "userAgent" ] )
                |> JsonConvert.DeserializeObject<Topics> 

        }

        let topics (repository: Repository)  = 
            topicsAsync repository
            |> Async.RunSynchronously

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

        
