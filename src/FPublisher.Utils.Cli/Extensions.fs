module internal FPublisher.Utils.Cli.Extensions
open System
open Argu
open System.Management.Automation
open Fake.Core
open System.Diagnostics
open System.IO

[<RequireQualifiedAccess>]
module File =
    let writeToTmpFile ext text =
        let file = 
            Path.ChangeExtension(Path.GetTempFileNameEx(), ext)
        File.WriteAllText(file, text, Text.Encoding.UTF8)
        file



let private execAsAdmin tool command args =

    let args = Args.toWindowsCommandLine (command :: args)
    let procInfo = ProcessStartInfo(tool)
    procInfo.Arguments <- args
    procInfo.Verb <- "runas"
    procInfo.RedirectStandardOutput <- true
    procInfo.UseShellExecute <- false
    let proc = Process.Start(procInfo)
    Console.WriteLine(proc.StandardOutput.ReadToEnd());
    proc.WaitForExit()
    assert (proc.ExitCode = 0)

let platformTool tool =
    tool
    |> ProcessUtils.tryFindFileOnPath
    |> function Some t -> t | _ -> failwithf "%s not found" tool

[<RequireQualifiedAccess>]
module PowerShell = 

    let private command name paramters =
        Args.toWindowsCommandLine (name :: paramters)

    let private toolPath = platformTool "pwsh"

    let powershellAdmin command args =
        execAsAdmin toolPath "-Command" (command :: args)

    let powershellAdminWithScriptContent content =
        let psFile = File.writeToTmpFile ".ps1" content
        execAsAdmin toolPath "-File" [psFile]

    let powershellAdminWithScriptLines contents =
        powershellAdminWithScriptContent (String.concat "\n" contents)

    let tryGetServiceByName serviceName =
        let results =
            let ps = PowerShell.Create()
            ps
                .AddCommand("Get-Service")
                .AddParameter("Name",serviceName)
                .Invoke()

        if results.Count = 0 then None
        else Some results.[0]

    let tryAddUser userName (password: string option) =
        let results = 
            let ps = PowerShell.Create()
            ps
                .AddCommand("Get-LocalUser")
                .AddParameter("Name",userName)
                .Invoke()

        if results.Count = 0 then
            let passwordArgs =
                match password with 
                | Some password -> 

                    ["-Password"; sprintf "(%s | ConvertTo-SecureString -AsPlainText -Force)" password ] 
                | None -> ["-NoPassword"]

            let args = 
                ["-Name"; userName] @ passwordArgs

            powershellAdmin "New-LocalUser" args

    let private tryAddSingleQuotes (text: string) =
        if text.StartsWith ''' && text.EndsWith ''' then text
        else "'" + text + "'"

    let private tryAddQuotes (text: string) =
        if text.StartsWith '"' && text.EndsWith '"' then text
        else """ + text + """

    let private tryMakeupUserName (userName: string) =
        if userName.StartsWith Environment.MachineName then 
            userName

        elif not (userName.Contains Environment.MachineName) then
            sprintf "%s\%s" Environment.MachineName userName

        else failwithf "either not contain computer name or start with it %s" Environment.MachineName

    let tryRegiterService serviceName output (userName: string) (password: string option) =
            
        match tryGetServiceByName serviceName with 
        | None ->
            let pwd = 
                match password with 
                | Some password -> sprintf "$pwd = %s | ConvertTo-SecureString -AsPlainText -Force" password
                | None -> sprintf "$pwd = new-object System.Security.SecureString"

            powershellAdminWithScriptLines 
                [
                    pwd
                    sprintf "$c = New-Object -TypeName System.Management.Automation.PSCredential -ArgumentList %s, $pwd" (tryMakeupUserName userName)
                    command
                        "New-Service"
                            [ "-Name"; serviceName;
                              "-DisplayName"; serviceName; 
                              "-Description"; serviceName; 
                              "-BinaryPathName"; ("dotnet '" + output + "'")
                              "-Credential"; "$c"
                              "-StartupType"; "Automatic" ]
                ]

        | Some _ -> ()

    /// https://stackoverflow.com/questions/313831/using-powershell-how-do-i-grant-log-on-as-service-to-an-account
    let grantServiceLogonRight userName =
        printfn "Grant Service %s Log on Right" userName

        if Environment.isWindows then
            let scriptContents =  
                sprintf 
                    """ 
<#
.Synopsis
  Grant logon as a service right to the defined user.
.Parameter computerName
  Defines the name of the computer where the user right should be granted.
  Default is the local computer on which the script is run.
.Parameter username
  Defines the username under which the service should run.
  Use the form: domain\username.
  Default is the user under which the script is run.
.Example
  Usage:
  .\GrantSeServiceLogonRight.ps1 -computerName hostname.domain.com -username "domain\username"
#>
[string] $computerName = "%s"
[string] $username = "%s"
  $tempPath = [System.IO.Path]::GetTempPath()
  $import = Join-Path -Path $tempPath -ChildPath "import.inf"
  if(Test-Path $import) { Remove-Item -Path $import -Force }
  $export = Join-Path -Path $tempPath -ChildPath "export.inf"
  if(Test-Path $export) { Remove-Item -Path $export -Force }
  $secedt = Join-Path -Path $tempPath -ChildPath "secedt.sdb"
  if(Test-Path $secedt) { Remove-Item -Path $secedt -Force }
  try {
    Write-Host ("Granting SeServiceLogonRight to user account: {0} on host: {1}." -f $username, $computerName)
    $sid = ((New-Object System.Security.Principal.NTAccount($username)).Translate([System.Security.Principal.SecurityIdentifier])).Value
    secedit /export /cfg $export
    $sids = (Select-String $export -Pattern "SeServiceLogonRight").Line
    foreach ($line in @("[Unicode]", "Unicode=yes", "[System Access]", "[Event Audit]", "[Registry Values]", "[Version]", "signature=`"`$CHICAGO$`"", "Revision=1", "[Profile Description]", "Description=GrantLogOnAsAService security template", "[Privilege Rights]", "$sids,*$sid")){
      Add-Content $import $line
    }
    secedit /import /db $secedt /cfg $import
    secedit /configure /db $secedt
    gpupdate /force
    Remove-Item -Path $import -Force
    Remove-Item -Path $export -Force
    Remove-Item -Path $secedt -Force
  } catch {
    Write-Host ("Failed to grant SeServiceLogonRight to user account: {0} on host: {1}." -f $username, $computerName)
    $error[0]
  }
                    """ Environment.MachineName (tryMakeupUserName userName)
            powershellAdminWithScriptContent scriptContents
        else 
            printfn "Is not a window system, skip grantSeServiceLogonRight"

    let tryStartService serviceName =
        powershellAdmin "Start-Service" ["-Name"; serviceName]


    let icaclsModifyable (user: string) dir = 
        /// https://github.com/aspnet/AspNetCore.Docs/blob/master/aspnetcore/host-and-deploy/windows-service/scripts/2.x/RegisterService.ps1
        let script =
            [ 
                "#Requires -Version 6.2"
                "#Requires -RunAsAdministrator"
                sprintf "$acl = Get-Acl %s" (tryAddSingleQuotes dir)
                sprintf """
$aclRuleArgs = %s,"Modify","ContainerInherit,ObjectInherit","None", "Allow"
                    """ (tryAddSingleQuotes user)
                "$accessRule = New-Object System.Security.AccessControl.FileSystemAccessRule($aclRuleArgs)"
                "$acl.SetAccessRule($accessRule)"
                sprintf """Set-Acl %s $acl""" (tryAddSingleQuotes dir)
            ]

        powershellAdminWithScriptLines script
