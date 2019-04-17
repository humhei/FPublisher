// Learn more about F# at http://fsharp.org

open System
open Argu
open System.Management.Automation
open Fake.Core
open System.Diagnostics
open System.IO
open FPublisher.Utils.Cli

open Extensions

[<RequireQualifiedAccess>]
type ServiceCommand =
    | Start
with 
    interface IArgParserTemplate with 
        member x.Usage =
            match x with 
            | ServiceCommand.Start ->
                "host an aspNetCore app as a window service and start it"


type ServiceArgs =
    | [<MainCommand; ExactlyOnce>] Command of ServiceCommand
    | ServiceName of string
    | UserName of string
    | Password of string
    | Executable of string

with 
    interface IArgParserTemplate with 
        member x.Usage =
            match x with 
            | Command _ -> "<host an aspNetCore app as a window service and start it | stop service >"
            | ServiceName _ -> "default is current assembly name https://docs.microsoft.com/en-us/aspnet/core/host-and-deploy/windows-service?view=aspnetcore-2.2"
            | UserName _ -> "default is AspNetCoreService, https://docs.microsoft.com/en-us/aspnet/core/host-and-deploy/windows-service?view=aspnetcore-2.2#create-a-user-account"
            | Password _ -> "default is NonPassword, https://docs.microsoft.com/en-us/aspnet/core/host-and-deploy/windows-service?view=aspnetcore-2.2#create-a-user-account"
            | Executable _ -> "publish output dll or exe"

type Service =
    { Command: ServiceCommand
      ServiceName: string 
      UserName: string 
      Password: string option
      Output: string }
with 
    member x.OutputDir = Path.GetDirectoryName x.Output

[<RequireQualifiedAccess>]
module Service =

    let ofParseResults (parseResults: ParseResults<ServiceArgs>) =
        let output = Path.Combine(Directory.GetCurrentDirectory(),parseResults.GetResult(Executable))
        { Command = parseResults.GetResult(Command)
          ServiceName = parseResults.GetResult(ServiceName,Path.GetFileNameWithoutExtension output).Replace("."," ")
          UserName = parseResults.GetResult(UserName,"AspNetCoreService")
          Password = parseResults.TryGetResult(Password)
          Output = output }

    let invoke (service: Service) =
        match service.Command with 
        | ServiceCommand.Start ->
            PowerShell.tryAddUser service.UserName service.Password
            PowerShell.icaclsModifyable service.UserName service.OutputDir
            PowerShell.tryRegiterService service.ServiceName service.Output service.UserName service.Password
            PowerShell.grantServiceLogonRight service.UserName
            PowerShell.tryStartService service.ServiceName


[<RequireQualifiedAccess>]
type Arguments =
    | [<CliPrefix(CliPrefix.None)>] Service of ParseResults<ServiceArgs>

with 
    interface IArgParserTemplate with 
        member x.Usage =
            match x with 
            | Service service -> "https://docs.microsoft.com/en-us/aspnet/core/host-and-deploy/windows-service?view=aspnetcore-2.2"

    

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<Arguments>()
    let parseResults = parser.Parse argv
    try 
        match parseResults.GetSubCommand() with
        | Arguments.Service serviceArg ->
            let service = Service.ofParseResults serviceArg
            Service.invoke service 

    with ex ->
        failwithf "%s\n%s" ex.Message (parser.PrintUsage())
        
    printfn "Hello World from F#!"
    0 // return an integer exit code
