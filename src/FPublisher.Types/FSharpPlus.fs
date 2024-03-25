namespace FPublisher.FSharpPlus
open Fake.IO
open Newtonsoft.Json
open Fake.IO.FileSystemOperators
open System.Diagnostics
open System.IO



[<AbstractClass; JsonObject(MemberSerialization.OptIn)>]
type POCOBase<'T when 'T : equality and 'T : comparison> (pocoKey: 'T) =

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member internal x.POCOKey = pocoKey

    override x.ToString() = x.POCOKey.ToString()

    override x.Equals(y: obj) =
        match x.GetType() = y.GetType() with 
        | true ->
            let y = y :?> POCOBase<'T>
            compare x.POCOKey y.POCOKey  = 0

        | false -> false
      

    interface System.IComparable with 
        member x.CompareTo(y: obj) =
            match x.GetType() = y.GetType() with 
            | true ->
                let y = y :?> POCOBase<'T>
                compare x.POCOKey y.POCOKey

            | false ->  failwithf "Cannot compare different types %s %s" (x.GetType().FullName) (y.GetType().FullName)


    interface System.IComparable<POCOBase<'T>> with 
        member x.CompareTo(y) = compare x.POCOKey y.POCOKey

    interface System.IEquatable<POCOBase<'T>> with 
        member x.Equals(y) = x.POCOKey = (y.POCOKey) 

    interface System.Collections.IStructuralEquatable with 
        member x.Equals(y, comparer) = x.Equals(y)
        member x.GetHashCode(comparer) = x.GetHashCode()

    interface System.Collections.IStructuralComparable with 
        member x.CompareTo(y, comparer) = (x :> System.IComparable).CompareTo(y)


    override x.GetHashCode() = 
        let num = 0
        let arg_34_0 = -1640531527
        arg_34_0 + 
            (hash  x.POCOKey) + ((num <<< 6) + (num >>> 2))


/// Ignore case
[<Sealed>]
type StringIC (value: string)  =
    inherit POCOBase<string>(value.ToLowerInvariant())
    [<JsonProperty>]
    member x.Value: string = value

    member x.Text = value

    member x.LowerInvariantValue = x.Value.ToLowerInvariant()

    member x.Contains(stringIC: StringIC) =
        x.LowerInvariantValue.Contains(stringIC.LowerInvariantValue)

    member x.StartsWith(stringIC: StringIC) =
        x.LowerInvariantValue.StartsWith(stringIC.LowerInvariantValue)

    member x.EndsWith(stringIC: StringIC) =
        x.LowerInvariantValue.EndsWith(stringIC.LowerInvariantValue)

    member x.Length = x.Value.Length

    override x.ToString() = x.Value

    member x.Trim() = x.Value.Trim() |> StringIC

    /// Only trim once
    member x.TrimEnd(ends: StringIC) =
        match x.LowerInvariantValue.EndsWith (ends.LowerInvariantValue) with 
        | true -> 
            let text = x.Value
            text.Substring(0, text.Length - ends.Value.Length)
            |> StringIC

        | false -> ends





[<AutoOpen>]
type FsFullPath = private Private_FsFullPath of fileName: StringIC * path: StringIC
with 
    /// With extension
    member x.FileNameIC =
        let (Private_FsFullPath (v, _)) = x
        v

    /// With extension
    member x.FileName = x.FileNameIC.Value

    member x.PathIC = 
        let (Private_FsFullPath (_, v)) = x
        v

    member x.Path = x.PathIC.Value
       

    static member FsFullPath(path: string) =
        Private_FsFullPath(StringIC (Path.GetFileName path), StringIC(Path.GetFullPath path))

    static member path (fullPath: FsFullPath) = fullPath.Path
    
    static member fileName (fullPath: FsFullPath) = fullPath.FileName



[<AutoOpen>]
module _Extensions =
    type System.String with 
        member x.Contains(text: string, ignoreCase) = 
            if ignoreCase then x.ToLower().Contains(text.ToLower())
            else x.Contains(text)
    
    
    [<RequireQualifiedAccess>]
    module Path =
        let normalizeToUnixCompatible path =
            let path = (Path.getFullName path).Replace('\\','/')
    
            let dir = Path.getDirectory path
    
            let segaments =
                let fileName = Path.GetFileName path
                fileName.Split([|'\\'; '/'|])
    
            let folder dir segament =
                dir </> segament
                |> Path.getFullName
    
            segaments
            |> Array.fold folder dir
    
    