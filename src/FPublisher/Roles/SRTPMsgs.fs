namespace FPublisher.Roles
#nowarn "0064"
[<AutoOpen>]
module SRTPMsgs =
        
    let inline (!^) msg = BuildServer.upcastMsg msg
