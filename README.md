# Notes: This repo is now in early stage..

# Role-based,composable,stateful and typed building targets
**Built in Targets:**  [e.g. NonGit.Targets](https://github.com/humhei/FPublisher/blob/master/src/FPublisher/Roles/NonGit.fs#L75)

1. CI Tests
2. Version Controller
3. Nuget package publish
4. Github draft



Stable | Prerelease
--- | ---
[![NuGet Badge](https://buildstats.info/nuget/FPublisher)](https://www.nuget.org/packages/FPublisher/) | [![NuGet Badge](https://buildstats.info/nuget/FPublisher?includePreReleases=true)](https://www.nuget.org/packages/FPublisher/)


MacOS/Linux | Windows
--- | ---
[![CircleCI](https://circleci.com/gh/humhei/FPublisher.svg?style=svg)](https://circleci.com/gh/humhei/FPublisher) | [![Build status](https://ci.appveyor.com/api/projects/status/0qnls95ohaytucsi?svg=true)](https://ci.appveyor.com/project/ts2fable-imports/fpublisher)
[![Build History](https://buildstats.info/circleci/chart/humhei/FPublisher)](https://circleci.com/gh/humhei/FPublisher) | [![Build History](https://buildstats.info/appveyor/chart/ts2fable-imports/fpublisher)](https://ci.appveyor.com/project/ts2fable-imports/fpublisher)


---

## Usage:
dotnet tool install --global fpublisher-cli
```
USAGE: fpublisher.exe [--help] [--create-sln] [--clean] [--build] [--test] [--next-release] [--run-ci]

OPTIONS:
    --create-sln          create sln if not exists
    --clean               clean bin and obj
    --build               build solution
    --test                test projects
    --next-release        draft next release
    --run-ci              only invoked by CI
    --help                display this list of options.
```
## Explaintion (See [sample](https://github.com/humhei/FPublisher) )
### Solution file used by fpublisher
FPublisher try to find a solution file in below sequence
1. $GitRepoName.FPubliusher.sln
2. $GitRepoName.sln
3. $FolderName.FPubliusher.sln
4. $FolderName.sln

### Command using the finded solution
#### `` --test``
test projects in solution contain test
#### `` --build``
build all projects in solution
#### `` --nextRelease``
##### Environment Prerequisites
* github_token
* github_release_user

Only for repo [collaborators and repo owner](https://github.com/humhei/FPublisher/blob/master/src/FPublisher/Roles/Collaborator.fs)


Please ensure all the commits are pushed to git server and you are in default branch
ReleaseNotes.MD must exists with a [TBD release_Note](https://github.com/humhei/FPublisher/blob/master/RELEASE_NOTES.md#0127-alpha---tbd)

#### `` --run-ci``
a list of composable FPublisher building targets

**Major CI**:

    Appveyor  (https://ci.appveyor.com/project/ts2fable-imports/fcswatch/builds/23846382)

        Targets:
            install paket packages
            Add source link to projects
            build
            test
            pack
            publish to nuget server (Only trigger when an release is drafted)
            Zip
            Artifacts

**Other CI**
    E.g (circleCI)[https://circleci.com/gh/humhei/FPublisher/265?utm_campaign=vcs-integration-link&utm_medium=referral&utm_source=github-build-link]


        Targets:
            install paket packages
            build
            test




## Features

### Typed
[NonGit.fsx](https://github.com/humhei/FPublisher/blob/master/src/FPublisher/Roles/NonGit.fs)
```fsharp
    let run =
        Role.update (function

            | Msg.Build semverInfoOp ->
                { PreviousMsgs = []
                  Action = MapState (fun role -> Solution.build semverInfoOp role.Solution) }

            | Msg.Test ->
                { PreviousMsgs = [ Msg.Build None ]
                  Action = MapState (fun role -> Solution.test role.Solution) }
        )

```

### Composable
[Forker.fsx](https://github.com/humhei/FPublisher/blob/master/src/FPublisher/Roles/Forker.fs)

`Forker` contains the `NonGit`
Also for `state` and `targets`
```fsharp
type Role =
    { NonGit: NonGit.Role
      TargetState: TargetState
      NugetPacker: NugetPacker
      LocalNugetServer: NugetServer option
      VersionController: Lazy<VersionController> }
```

### Stateful
stored newPackages in `Pack` and use it in `PublishToLocalNugetServer`

[Forker.fsx](https://github.com/humhei/FPublisher/blob/master/src/FPublisher/Roles/Forker.fs)

```fsharp

| Msg.Pack releaseNotes ->
    { PreviousMsgs = [!^ (NonGit.Msg.Build (Some releaseNotes.SemVer)); !^ NonGit.Msg.Test]
      Action = MapState (fun role ->
        let githubData = role.GitHubData
        let newPackages =
            NugetPacker.pack
                role.Solution
                true
                true
                githubData.Topics
                githubData.License
                githubData.Repository
                releaseNotes
                role.NugetPacker
        newPackages
        |> box
        )
    }


| Msg.PublishToLocalNugetServer ->

    match role.LocalNugetServer with
    | Some localNugetServer ->
        let versionFromLocalNugetServer = Role.versionFromLocalNugetServer role |> Async.RunSynchronously
        let nextReleaseNotes = Role.nextReleaseNotes versionFromLocalNugetServer role

        { PreviousMsgs = [ Msg.Pack nextReleaseNotes ]
          Action = MapState (fun role ->
            let (newPackages) = State.getResult role.TargetState.Pack

            let currentVersion = VersionController.currentVersion versionFromLocalNugetServer role.VersionController.Value

            logger.CurrentVersion currentVersion

            let nextVersion = Role.nextVersion versionFromLocalNugetServer role

            logger.ImportantGreen "Next version is %s" (SemVerInfo.normalize nextVersion)

            NugetServer.publish newPackages localNugetServer |> Async.RunSynchronously
            none
            )
        }
    | None -> failwithf "local nuget server is not defined"
```


## More features
Please Directly contribute to this repository [src/FPublisher/Roles](https://github.com/humhei/FPublisher/tree/master/src/FPublisher/Roles)


## Who use FPublisher
[FcsWatch](https://github.com/humhei/FCSWatch/): Run standard fsharp codes in watch mode
