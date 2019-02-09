namespace FPublisher.Roles
#nowarn "0064"
[<AutoOpen>]
module SRTPMsgs =
    type Ext = Ext
        with
            static member Bar (ext : Ext, nonGit : NonGit.Msg) = 
                Forker.upcastMsg nonGit
                |> Collaborator.upcastMsg

            static member Bar (ext : Ext, forker : Forker.Msg) = 
                Collaborator.upcastMsg forker

    let inline (!^) msg =
        ((^b or ^a) : (static member Bar : ^b * ^a -> Collaborator.Msg) (Ext, msg))
