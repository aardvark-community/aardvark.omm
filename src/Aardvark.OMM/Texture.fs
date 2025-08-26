namespace Aardvark.OMM

open System
open System.Runtime.InteropServices
open System.Threading
open Aardvark.Base

#nowarn "51"

type IAlphaTexture =
    interface end

type INativeAlphaTexture =
    inherit IAlphaTexture
    inherit IDisposable

/// Alpha texture based on a PixImage.
type AlphaPixImage =
    val Data    : PixImage
    val Channel : Col.Channel

    /// <summary>
    /// Creates a new alpha texture from the given PixImage.
    /// Channel type must be uint8 or convertible to float32.
    /// If the image does not contain the preferred channel, the first channel is used.
    /// </summary>
    /// <param name="data">The image containing the alpha data.</param>
    /// <param name="channel">The channel to retrieve the alpha data from. Default is Alpha.</param>
    new (data: PixImage, [<Optional; DefaultParameterValue(Col.Channel.Alpha)>] channel: Col.Channel) =
        { Data = data; Channel = channel }

    member inline private this.Equals(other: AlphaPixImage) =
        this.Data = other.Data && this.Channel = other.Channel

    override this.Equals(obj: obj) =
        match obj with
        | :? AlphaPixImage as texture -> this.Equals(texture)
        | _ -> false

    override this.GetHashCode() =
        HashCode.Combine(this.Data.GetHashCode(), this.Channel.GetHashCode())

    interface IEquatable<AlphaPixImage> with
        member this.Equals(other) = this.Equals(other)

    interface IAlphaTexture

type internal NativeAlphaTexture =
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

    interface INativeAlphaTexture with
        member this.Dispose() = this.Release()

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal NativeAlphaTexture =

    let private getAlphaImage (channel: Col.Channel) (pi: PixImage<'T>) : PixImage * int64 =
        let pi =
            if pi.ChannelCount = 1 then pi
            else
                let channels = pi.Format.ChannelsOfFormat()
                let channelIndex = channels |> Array.tryFindIndex ((=) channel) |> Option.defaultValue 0
                PixImage<'T>(Col.Format.Alpha, pi.GetChannelInFormatOrder <| int64 channelIndex)

        pi, pi.VolumeInfo.FirstIndex * int64 sizeof<'T>

    let ofPixImage (baker: API.Baker) (channel: Col.Channel) (pi: PixImage) =
        let (pi, offset), format =
            match pi with
            | :? PixImage<uint8> as pi   -> getAlphaImage channel pi, API.CpuTextureFormat.UNorm8
            | :? PixImage<float32> as pi -> getAlphaImage channel pi, API.CpuTextureFormat.Fp32
            | _ ->
                let pi = PixImage<float32>(pi.Format, pi)
                getAlphaImage channel pi, API.CpuTextureFormat.Fp32

        pi.Array |> NativeInt.pin (fun pData ->
            let mutable mip = API.CpuTextureMipDesc(uint32 pi.WidthL, uint32 pi.HeightL, uint32 pi.VolumeInfo.DY, pData + nativeint offset)
            let mutable desc = API.CpuTextureDesc(format, API.CpuTextureFlags.None, &&mip, 1u)

            let mutable handle = API.CpuTexture.Null
            API.Omm.cpuCreateTexture(baker, &desc, &handle) |> Result.check "failed to create texture"

            new NativeAlphaTexture(baker, handle)
        )

    let ofAlphaTexture (baker: API.Baker) (texture: IAlphaTexture) =
        match texture with
        | :? AlphaPixImage as image ->
            ofPixImage baker image.Channel image.Data

        | :? NativeAlphaTexture as texture ->
            texture.Acquire()
            texture

        | _ ->
            failwith $"Unsupported texture type: {texture.GetType()}"