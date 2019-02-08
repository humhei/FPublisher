
// ts2fable 0.6.1
module rec Mag

open System
open Fable.Core
open Fable.Import.JS
open Fable.Import.Browser

type Hyperscript = Mithril.Hyperscript

type Attrs = obj
type State = obj

type [<AllowNullLiteral>] IExports =
    /// Manually triggers a redraw of mounted components. 
    abstract redraw: unit -> unit
    /// Renders a vnode structure into a DOM element. 
    abstract render: el: Element * vnodes: Mithril.Children -> unit
    // ...

module Mithril =

    type [<AllowNullLiteral>] Lifecycle<'Attrs, 'State> =
        /// The oninit hook is called before a vnode is touched by the virtual DOM engine. 
        abstract oninit: this: 'State * vnode: Vnode<'Attrs, 'State> -> obj option

    type [<AllowNullLiteral>] Hyperscript =
        /// Creates a virtual element (Vnode). 
        [<Emit "$0($1...)">] abstract Invoke: selector: string * [<ParamArray>] children: ResizeArray<Children> -> Vnode<obj option, obj option>

// ...

// let [<Import("*","module")>] RedrawService: Mithril.RedrawService.Static = jsNative            
// let [<Import("*","module")>] RenderService: Mithril.RenderService.Static = jsNative
// let [<Import("*","module")>] RequestService: Mithril.RequestService.Static = jsNative
// let [<Import("*","module")>] Stream: Mithril.Stream.Static = jsNative
// let [<Import("*","module")>] h: Hyperscript = jsNative
// let [<Import("*","module")>] Mithril: Mithril.Static = jsNative

// my experiment - trying to expose mithril render function!!!
let [<Import("*","module")>] MithrilRender: IExports = jsNative