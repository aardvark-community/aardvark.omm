namespace Aardvark.OMM

open System
open System.Runtime.InteropServices
open Aardvark.Base

#nowarn "51"

type Micromap =
    val private baker : API.Baker
    val mutable private handle : API.CpuBakeResult
    val mutable private inputDesc : API.CpuBakeInputDesc
    val mutable private resultDesc : API.CpuBakeResultDesc

    internal new (baker, handle, inputDesc: inref<_>) =
        let mutable pResultDesc = NativePtr.zero<API.CpuBakeResultDesc>
        API.Omm.cpuGetBakeResultDesc(handle, &pResultDesc) |> Result.check "failed to get bake result"
        { baker = baker; handle = handle; inputDesc = inputDesc; resultDesc = pResultDesc.[0] }

    member this.Handle = this.handle

    /// <summary>
    /// Save debug images to the given path.
    /// </summary>
    /// <param name="path">The path of the destination folder.</param>
    /// <param name="detailedCutout">If true, generates a cropped and zoomed-in view of the micromap.</param>
    member this.SaveDebug(path: string, [<Optional; DefaultParameterValue(false)>] detailedCutout: bool) =
        let mutable desc = API.DebugSaveImagesDesc(path, "", detailedCutout, false, false, not detailedCutout)
        API.Omm.debugSaveAsImages(this.baker, &this.inputDesc, &this.resultDesc, &desc) |> Result.check "failed to save debug images"

    member this.Dispose() =
        if this.handle.IsValid then
            API.Omm.cpuDestroyBakeResult(this.handle) |> Result.check "failed to destroy bake result"
            this.handle <- API.CpuBakeResult.Null

    interface IDisposable with
        member this.Dispose() = this.Dispose()