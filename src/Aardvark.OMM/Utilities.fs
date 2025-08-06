namespace Aardvark.OMM

open System
open System.Runtime.InteropServices

type internal FixedArray(array: Array) =
    let gc = GCHandle.Alloc(array, GCHandleType.Pinned)
    let address = gc.AddrOfPinnedObject()

    member _.Address = address

    interface IDisposable with
        member _.Dispose() = gc.Free()