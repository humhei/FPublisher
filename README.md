# Notes: This repo is now in early stage..

# Role-based,composable,stateful and typed building targets

Stable | Prerelease
--- | ---
[![NuGet Badge](https://buildstats.info/nuget/FPublisher)](https://www.nuget.org/packages/FPublisher/) | [![NuGet Badge](https://buildstats.info/nuget/FPublisher?includePreReleases=true)](https://www.nuget.org/packages/FPublisher/)


MacOS/Linux | Windows
--- | ---
[![CircleCI](https://circleci.com/gh/humhei/FPublisher.svg?style=svg)](https://circleci.com/gh/humhei/FPublisher) | [![Build status](https://ci.appveyor.com/api/projects/status/0qnls95ohaytucsi?svg=true)](https://ci.appveyor.com/project/ts2fable-imports/fpublisher)
[![Build History](https://buildstats.info/circleci/chart/humhei/FPublisher)](https://circleci.com/gh/humhei/FPublisher) | [![Build History](https://buildstats.info/appveyor/chart/ts2fable-imports/fpublisher)](https://ci.appveyor.com/project/ts2fable-imports/fpublisher)


---

## Usage:
See [build.fsx](https://github.com/humhei/FPublisher/blob/master/build.fsx)


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
