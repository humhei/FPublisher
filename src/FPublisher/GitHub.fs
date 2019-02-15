namespace FPublisher
open Fake.Core
open Newtonsoft.Json
open Fake.Api
open FSharp.Data
open FSharp.Data.HttpRequestHeaders
open FSharp.Control.Tasks.V2.ContextInsensitive
open Utils
open FakeHelper.Build
open System.Net.Http
open System
open Octokit.Internal
open System.Reflection
open System.Threading
open Octokit
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

        // code adapted from Fake.GitHub.fs
        // wrapper re-implementation of HttpClientAdapter which works around
        // known Octokit bug in which user-supplied timeouts are not passed to HttpClient object
        // https://github.com/octokit/octokit.net/issues/963
        type private HttpClientWithTimeout(timeout : TimeSpan) as this =
            inherit HttpClientAdapter(fun () -> HttpMessageHandlerFactory.CreateDefault())
            let setter = lazy(
                match typeof<HttpClientAdapter>.GetTypeInfo().GetField("_http", BindingFlags.NonPublic ||| BindingFlags.Instance) with
                | null -> ()
                | f ->
                    match f.GetValue(this) with
                    | :? HttpClient as http -> http.Timeout <- timeout
                    | _ -> ())

            interface IHttpClient with
                member __.Send(request : IRequest, ct : CancellationToken) =
                    setter.Force()
                    match request with :? Request as r -> r.Timeout <- timeout | _ -> ()
                    base.Send(request, ct)


        let createWithoutToken() = async {
            let httpClient = new HttpClientWithTimeout(TimeSpan.FromMinutes 20.)
            let connection = Connection(ProductHeaderValue("FAKE"), httpClient)
            return GitHubClient(connection)
        }

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
            |> GitHub.draftNewRelease user repoName (SemVerInfo.normalize release.SemVer) (release.SemVer.PreRelease <> None) release.Notes
            |> GitHub.publishDraft
