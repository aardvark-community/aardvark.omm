namespace Aardvark.OMM

open System
open Aardvark.Base
open Aardvark.Rendering.Raytracing
open FSharp.NativeInterop

#nowarn "9"

[<AutoOpen>]
module internal Utilities =

    module NativeInt =
        let toArray<'T when 'T : unmanaged> (count: uint32) (address: nativeint) : 'T[] =
            address |> NativePtr.ofNativeInt |> NativePtr.toArray (int count)

    module NativePtr =
        let toArray (count: uint32) (ptr: nativeptr<'T>) : 'U[] =
            ptr.Address |> NativeInt.toArray count

        let toUsageCountsArray (count: uint32) (ptr: nativeptr<API.CpuOpacityMicromapUsageCount>) =
            let arr = Array.zeroCreate<MicromapUsage> <| int count
            for i = 0 to arr.Length - 1 do
                arr.[i] <- MicromapUsage(ptr.[i].count, uint32 ptr.[i].subdivisionLevel, unbox <| ptr.[i].format)
            arr

[<AbstractClass>]
type CpuMicromap =
    val mutable internal inputDesc  : API.CpuBakeInputDesc
    val mutable internal resultDesc : API.CpuBakeResultDesc

    internal new (inputDesc, resultDesc) =
        { inputDesc = inputDesc; resultDesc = resultDesc }

    /// Returns the micromap data as IMicromapData object.
    member this.Data =
        let data = NativeInt.toArray<uint8> this.resultDesc.arrayDataSize this.resultDesc.arrayData

        let indices : Array =
            match this.resultDesc.indexFormat with
            | API.IndexFormat.UInt8  -> NativeInt.toArray<uint8>  this.resultDesc.indexCount this.resultDesc.indexBuffer
            | API.IndexFormat.UInt16 -> NativeInt.toArray<uint16> this.resultDesc.indexCount this.resultDesc.indexBuffer
            | _                      -> NativeInt.toArray<uint32> this.resultDesc.indexCount this.resultDesc.indexBuffer

        let usageCounts =
            NativePtr.toUsageCountsArray this.resultDesc.descArrayHistogramCount this.resultDesc.descArrayHistogram

        let indexUsageCounts =
            NativePtr.toUsageCountsArray this.resultDesc.indexHistogramCount this.resultDesc.indexHistogram

        let triangles : MicromapTriangle[] =
            NativePtr.toArray this.resultDesc.descArrayCount this.resultDesc.descArray

        { new IMicromapData with
            member _.Data             = data
            member _.Indices          = indices
            member _.UsageCounts      = usageCounts
            member _.IndexUsageCounts = indexUsageCounts
            member _.Triangles        = triangles }

    abstract member Dispose : unit -> unit

    interface IDisposable with
        member this.Dispose() = this.Dispose()

module internal CpuMicromap =

    let ofBakeResult handle (texture: NativeAlphaTexture) inputDesc resultDesc =
        let mutable handle = handle
        texture.Acquire()

        { new CpuMicromap(inputDesc, resultDesc) with
            member _.Dispose() =
                texture.Release()
                API.Omm.cpuDestroyBakeResult(handle) |> Result.check "failed to destroy bake result"
                handle <- API.CpuBakeResult.Null
        }

    let ofDeserializedResult handle inputDesc resultDesc =
        let mutable handle = handle
        { new CpuMicromap(inputDesc, resultDesc) with
            member _.Dispose() =
                API.Omm.cpuDestroyDeserializedResult(handle) |> Result.check "failed to destroy deserialized result"
                handle <- API.CpuDeserializedResult.Null
        }