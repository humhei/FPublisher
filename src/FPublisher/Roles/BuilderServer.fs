namespace FPublisher.Roles
open FPublisher
open Fake.IO.FileSystemOperators
open Fake.Core
open Fake.DotNet
open System.IO
open Fake.IO
open Primitives
open Solution
open FPublisher.Nuget
open Fake.IO.Globbing.Operators
open Fake.DotNet.NuGet.NuGet


[<RequireQualifiedAccess>]
module BuilerServer =
    [<RequireQualifiedAccess>]
    type Target =
        | NonGit of NonGit.Target
        | PushToGithubPacakges 


    type TargetStates =
        { NonGit: NonGit.TargetStates
          PushToGithubPacakges: BoxedTargetState }

    [<RequireQualifiedAccess>]
    module TargetStates =
        let init =
            { NonGit = NonGit.TargetStates.init
              PushToGithubPacakges = TargetState.Init }

    type Role =
        { NonGit: NonGit.Role
          TargetStates: TargetStates
          github_token: string
          github_release_user: string
          /// create from environment: github_package_source
          GithubPackagesNugetServer: GithubPackagesServer
        }
    with 
        member x.Workspace = x.NonGit.Workspace

        member x.LocalNugetServerV3 = x.NonGit.LocalNugetServerV3

        member x.Solution = x.NonGit.Solution

        interface IRole<TargetStates>



    let tryCreate (args: NonGitArgs) =
        match Environment.environVarOrNone "github_token", Environment.environVarOrNone "github_release_user",Environment.environVarOrNone "github_package_source"  with
        | Some github_token,Some github_release_user,Some github_package_source ->
            TraceSecrets.register "<github_token>" github_token
            TraceSecrets.register "<github_release_user>" github_release_user
            TraceSecrets.register "<github_package_source>" github_package_source

            { NonGit = NonGit.create args 
              TargetStates = TargetStates.init
              github_token = github_token
              github_release_user = github_release_user
              GithubPackagesNugetServer = 
                GithubPackagesServer({Name = github_package_source; Token = github_token; UserName = github_release_user})
            }
            |> Some
        | _ -> None

    let (!^) (nonGitMsg: NonGit.Target) = Target.NonGit nonGitMsg


    let run =
        IRole.Run (fun (role: Role) target ->
            match target with
            | Target.NonGit nonGitTarget ->
                { DependsOn = [] 
                  Action = MapChild (fun (role: Role) -> 
                    { role with 
                        NonGit = NonGit.run nonGitTarget role.NonGit 
                    }
                  )
                }

            | Target.PushToGithubPacakges ->

                let packTarget = 
                    let version = 
                        let packageNames = Solution.getPackageNames role.Solution

                        let lastVersions = 
                            packageNames
                            |> List.choose (fun packageName ->
                                NugetServer.getLastNugetVersionV3 packageName true localNugetServer
                            )
                            |> List.map (SemVer.parse)

                        if lastVersions.IsEmpty then "1.0.0"
                        else 
                            let maxVersion  = (List.max lastVersions)
                            { maxVersion with Patch = maxVersion.Patch + 1u }.Normalize()

                    NonGit.Target.Pack (fun ops ->
                        { ops with 
                            OutputPath = Some outputDirectory 
                            Configuration = DotNet.BuildConfiguration.Release 
                            Version = Some version }
                    )

                { DependsOn = [!^ (NonGit.Target.Pack packOptions)] 
                  Action = MapState (fun role -> 
                    
                    box ""
                    
                  ) 
                }

            | _ -> failwith "Not implemented"
        )