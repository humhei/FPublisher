namespace FPublisher
open Fake.DotNet
open Fake.Core
open Fake
open Fake.IO
open FakeHelper.Build

[<RequireQualifiedAccess>]
module DotNet =
    type CustomParam =
        { Property: string 
          Value: string }
    with 
        override x.ToString() =
            sprintf "/p:%s=\"%s\"" x.Property x.Value

    module PublishOptions =
        let setVersion (version: SemVerInfo) (ops: DotNet.PublishOptions) =
            { ops with 
                Common = 
                    { ops.Common with 
                        CustomParams =
                            [
                                yield { Property = "Version"; Value = SemVerInfo.normalize version } 
                            ]
                            |> List.map (fun prop -> prop.ToString())
                            |> String.concat " "
                            |> Some
                            
                    }
            }

    [<RequireQualifiedAccess>]
    module BuildOptions =
        let setVersion (version: SemVerInfo) (ops: DotNet.BuildOptions) =
            { ops with 
                Common = 
                    { ops.Common with 
                        CustomParams =
                            [
                                yield { Property = "Version"; Value = SemVerInfo.normalize version } 
                            ]
                            |> List.map (fun prop -> prop.ToString())
                            |> String.concat " "
                            |> Some
                            
                    }
            }

    type PackOptions =
        { NoBuild: bool
          NoRestore: bool
          Configuration: DotNet.BuildConfiguration
          OutputPath: string option 
          Authors: string list
          Version: string option
          GenerateDocumentationFile: bool
          PackageIconUrl: string option
          Description: string option
          ReleaseNotes: ReleaseNotes.ReleaseNotes option
          PackageLicenseUrl: string option
          PackageProjectUrl: string option
          Tags: string list }
    with 
        static member DefaultValue =
            { NoBuild = false
              NoRestore = false
              Version = None
              OutputPath = None
              Configuration = DotNet.BuildConfiguration.Debug
              Authors = [] 
              GenerateDocumentationFile = true 
              PackageIconUrl = None 
              Description = None 
              ReleaseNotes = None 
              PackageLicenseUrl = None 
              PackageProjectUrl = None 
              Tags = [] }

    [<RequireQualifiedAccess>]
    module  PackOptions =



        let asFakePackOptions packOptions : DotNet.PackOptions =
            { NoBuild = packOptions.NoBuild 
              NoLogo = true
              IncludeSymbols = false
              Configuration = packOptions.Configuration 

              VersionSuffix = None 
              BuildBasePath = None 
              OutputPath = packOptions.OutputPath 
              Common = 
                { DotNet.Options.Create() with 
                    CustomParams = 
                        [
                            if packOptions.Tags.Length > 0 then yield { Property = "PackageTags"; Value = String.separated ";" packOptions.Authors }
                            if packOptions.Authors.Length > 0 then yield { Property = "Authors"; Value = String.separated ";" packOptions.Authors }
                            if packOptions.GenerateDocumentationFile then yield { Property = "GenerateDocumentationFile"; Value = "true" }
                            match packOptions.Description with Some description -> yield { Property = "Description"; Value = description } | None -> ()
                            match packOptions.ReleaseNotes with Some releaseNotes -> yield { Property = "PackageReleaseNotes"; Value = String.toLines releaseNotes.Notes } | None -> ()
                            match packOptions.PackageIconUrl with Some packageIconUrl -> yield { Property = "PackageIconUrl"; Value = packageIconUrl } | None -> ()
                            match packOptions.PackageProjectUrl with Some packageProjectUrl -> yield { Property = "PackageProjectUrl"; Value = packageProjectUrl } | None -> ()
                            match packOptions.PackageLicenseUrl with Some packageLicenseUrl -> yield { Property = "PackageLicenseUrl"; Value = packageLicenseUrl } | None -> ()
                            match packOptions.Version with Some version -> yield { Property = "Version"; Value = version } | None -> ()
                        ] 
                        |> List.map (fun prop -> prop.ToString())
                        |> String.concat " "
                        |> Some
                }
              NoRestore = packOptions.NoRestore
              MSBuildParams = 
                { MSBuild.CliArguments.Create() with DisableInternalBinLog = true }
            }

        let asFakeBuildOptions (packOptions: PackOptions) : DotNet.BuildOptions =
            let ops = DotNet.BuildOptions.Create()
            { 
              ops with 
                  NoLogo = true
                  Configuration = packOptions.Configuration 
                  Framework = ops.Framework
                  OutputPath = packOptions.OutputPath 
                  Common = 
                    { DotNet.Options.Create() with 
                        CustomParams = 
                            [
                                if packOptions.Tags.Length > 0 then yield { Property = "PackageTags"; Value = String.separated ";" packOptions.Authors }
                                if packOptions.Authors.Length > 0 then yield { Property = "Authors"; Value = String.separated ";" packOptions.Authors }
                                if packOptions.GenerateDocumentationFile then yield { Property = "GenerateDocumentationFile"; Value = "true" }
                                match packOptions.Description with Some description -> yield { Property = "Description"; Value = description } | None -> ()
                                match packOptions.ReleaseNotes with Some releaseNotes -> yield { Property = "PackageReleaseNotes"; Value = String.toLines releaseNotes.Notes } | None -> ()
                                match packOptions.PackageIconUrl with Some packageIconUrl -> yield { Property = "PackageIconUrl"; Value = packageIconUrl } | None -> ()
                                match packOptions.PackageProjectUrl with Some packageProjectUrl -> yield { Property = "PackageProjectUrl"; Value = packageProjectUrl } | None -> ()
                                match packOptions.PackageLicenseUrl with Some packageLicenseUrl -> yield { Property = "PackageLicenseUrl"; Value = packageLicenseUrl } | None -> ()
                                match packOptions.Version with Some version -> yield { Property = "Version"; Value = version } | None -> ()
                            ] 
                            |> List.map (fun prop -> prop.ToString())
                            |> String.concat " "
                            |> Some
                    }
                  NoRestore = packOptions.NoRestore
                  MSBuildParams = 
                    { MSBuild.CliArguments.Create() with DisableInternalBinLog = true }
            }



    let pack (setParams: PackOptions -> PackOptions) project =
        let options = setParams PackOptions.DefaultValue
        DotNet.pack (fun _ -> PackOptions.asFakePackOptions options) project


