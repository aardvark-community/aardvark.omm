namespace Aardvark.OMM

open System
open System.Threading
open Aardvark.Base

#nowarn "51"

type internal Texture =
    val private baker : API.Baker
    val mutable private handle : API.CpuTexture
    val mutable private referenceCount : int

    internal new (baker, handle) = { baker = baker; handle = handle; referenceCount = 1 }

    member this.Handle = this.handle

    member this.Acquire() =
        Interlocked.Increment &this.referenceCount |> ignore

    member this.Release() =
        if Interlocked.Decrement &this.referenceCount = 0 && this.handle.IsValid then
            API.Omm.cpuDestroyTexture(this.baker, this.handle) |> Result.check "failed to destroy texture"
            this.handle <- API.CpuTexture.Null

    interface IDisposable with
        member this.Dispose() = this.Release()

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Texture =

    let private getAlphaImage (pi: PixImage<'T>) : PixImage * int64 =
        let pi =
            if pi.ChannelCount = 1 then pi
            else
                let channels = pi.Format.ChannelsOfFormat()
                let channelIndex = channels |> Array.tryFindIndex ((=) Col.Channel.Alpha) |> Option.defaultValue 0
                PixImage<'T>(Col.Format.Alpha, pi.GetChannelInFormatOrder <| int64 channelIndex)

        pi, pi.VolumeInfo.FirstIndex * int64 sizeof<'T>

    let ofPixImage (baker: API.Baker) (pi: PixImage) =
        let (pi, offset), format =
            match pi with
            | :? PixImage<uint8> as pi   -> getAlphaImage pi, API.CpuTextureFormat.UNorm8
            | :? PixImage<float32> as pi -> getAlphaImage pi, API.CpuTextureFormat.Fp32
            | _ ->
                let pi = PixImage<float32>(pi.Format, pi)
                getAlphaImage pi, API.CpuTextureFormat.Fp32

        pi.Array |> NativeInt.pin (fun pData ->
            let mutable mip = API.CpuTextureMipDesc(uint32 pi.WidthL, uint32 pi.HeightL, uint32 pi.VolumeInfo.DY, pData + nativeint offset)
            let mutable desc = API.CpuTextureDesc(format, API.CpuTextureFlags.None, &&mip, 1u)

            let mutable handle = API.CpuTexture.Null
            API.Omm.cpuCreateTexture(baker, &desc, &handle) |> Result.check "failed to create texture"

            new Texture(baker, handle)
        )