namespace Aardvark.OMM

open Aardvark.Base
open Aardvark.Rendering
open System
open System.Runtime.InteropServices
open TypeMeta

#nowarn "9"

[<CLIMutable>]
type AlphaSampler =
    {
        Address     : WrapMode
        Filter      : FilterMode
        BorderAlpha : float32
    }

    static member Default =
        { Address     = WrapMode.Clamp
          Filter      = FilterMode.Linear
          BorderAlpha = 1.0f }

/// Describes the input data for a bake operation.
[<CLIMutable>]
type BakeInput =
    {
        /// The indices of the geometry.
        /// Must be an array of 8-bit, 16-bit, or 32-bit integers.
        Indices       : Array

        /// The texture coordinates of the vertices.
        TextureCoords : V2f[]

        /// The alpha texture to sample.
        /// Channel type must be uint8 or convertible to float32.
        /// If the image does not contain an alpha channel, the first channel is used.
        AlphaTexture  : PixImage

        /// Describes how the alpha texture is sampled.
        /// Should be equivalent to how the texture is sampled at runtime.
        AlphaSampler  : AlphaSampler
    }

[<CLIMutable>]
type BakeSettings =
    {
        /// The format of the micromap.
        Format                  : OpacityFormat

        /// Configures the target resolution when running dynamic subdivision level.
        /// <= 0: disabled.
        /// > 0: The subdivision level be chosen such that a single micro-triangle covers approximatley a DynamicSubdivisionScale *
        /// DynamicSubdivisionScale texel area.
        DynamicSubdivisionScale : float32

        /// Rejection threshold [0,1]. Unless OMMs achieve a rate of at least RejectionThreshold known states OMMs will be discarded
        /// for the primitive. Use this to weed out "poor" OMMs.
        RejectionThreshold      : float32

        /// The alpha cutoff value.
        /// Texels with alpha less than or equal to the cutoff are considered transparent.
        AlphaCutoff             : float32

        /// Determines how to promote mixed states
        UnknownStatePromotion   : UnknownStatePromotion

        /// Determines the state of unresolvable (nan/inf UV-triangles) and disabled triangles.
        /// Note that degenerate triangles (points/lines) will be resolved correctly.
        UnresolvedTriangleState : OpacityState

        /// Micro triangle count is 4^N, where N is the subdivision level.
        /// MaxSubdivisionLevel level must be in range [0, 12].
        /// When DynamicSubdivisionScale is enabled MaxSubdivisionLevel is the max subdivision level allowed.
        /// When DynamicSubdivisionScale is disabled MaxSubdivisionLevel is the subdivision level applied uniformly to all triangles.
        MaxSubdivisionLevel     : uint8

        /// Max allowed size in bytes of ommCpuBakeResultDesc::arrayData
        /// The baker will choose to downsample the most appropriate omm blocks (based on area, reuse, coverage and other factors)
        /// until this limit is met.
        MaxArrayDataSize        : uint32
    }

    static member Default =
        { Format                  = OpacityFormat.Full
          DynamicSubdivisionScale = 2.0f
          RejectionThreshold      = 0.0f
          AlphaCutoff             = 0.5f
          UnknownStatePromotion   = UnknownStatePromotion.ForceOpaque
          UnresolvedTriangleState = OpacityState.UnknownOpaque
          MaxSubdivisionLevel     = 8uy
          MaxArrayDataSize        = UInt32.MaxValue }

[<Flags>]
type BakeFlags =
    | None                         = 0u

    /// Parallelize the baking process by using multi-threading.
    | Parallel                     = 1u

    /// Disable the use of special indices in case the OMM-state is uniform. Only set to true for debug purposes.
    /// Note: This prevents promotion of fully known OMMs to use special indices, however for invalid & degenerate UV triangles
    /// special indices may still be set.
    | DisableSpecialIndices        = 2u

    /// Force 32-bit index format for the output OMM index buffer.
    | Force32BitIndices            = 4u

    /// Will disable reuse of OMMs and instead produce duplicates omm-array data. Generally only needed for debug purposes.
    | DisableDuplicateDetection    = 8u

    /// This enables merging of "similar" OMMs where similarity is measured using hamming distance.
    /// UT and UO are considered identical.
    /// Pros: normally reduces resulting OMM size drastically, especially when there's overlapping UVs.
    /// Cons: The merging comes at the cost of coverage.
    /// The resulting OMM Arrays will have lower fraction of known states. For large working sets it can be quite CPU heavy to
    /// compute. Note: can not be used if DisableDuplicateDetection is set.
    | NearDuplicateDetection       = 16u

    /// Enable additional validation, when enabled additional processing is performed to validate quality and sanity of input data
    /// which may help diagnose omm bake result or longer than expected bake times.
    | Validation                   = 32u

module internal ImageCache =

    [<Literal>]
    let defaultSize = 256L * 1024L * 1024L // 256MiB

/// CPU-based baker for opacity micromaps.
type Baker (messageCallback: Action<MessageSeverity, string>, [<Optional; DefaultParameterValue(ImageCache.defaultSize)>] imageCacheSize: int64) =
    let mutable handle =
        let mutable baker = API.Baker.Null

        let messageCallback =
            if isNull messageCallback then
                null
            else
                API.MessageCallback(fun severity message _ -> messageCallback.Invoke(severity, message))

        let mutable desc = API.BakerCreationDesc(API.BakerType.CPU, messageCallback)
        API.Omm.createBaker(&desc, &baker) |> Result.check "failed to create baker"
        baker

    let textures =
        let getImageSize (pi: PixImage) = int64 pi.PixelSize * pi.WidthL * pi.HeightL

        let createTexture (pi: PixImage) =
            let t = pi |> Texture.ofPixImage handle
            t.Acquire()
            t

        LruCache<PixImage, Texture>(
            imageCacheSize, getImageSize, createTexture,
            fun _ -> _.Release()
        )

    static let defaultMessageLogger (severity: MessageSeverity) (message: string) =
        match severity with
        | MessageSeverity.Error | MessageSeverity.Fatal ->
            Log.error "[OMM] %s" message

        | MessageSeverity.PerfWarning ->
            Log.warn "[OMM] %s" message

        | _ ->
            Log.line "[OMM] %s" message

    new ([<Optional; DefaultParameterValue(true)>] logMessages: bool,
         [<Optional; DefaultParameterValue(ImageCache.defaultSize)>] imageCacheSize: int64) =
        let callback = if logMessages then Action<_, _> defaultMessageLogger else null
        new Baker(callback, imageCacheSize)

    member _.Handle = handle

    static member LibraryVersion =
        let desc = API.Omm.getLibraryDesc()
        Version(int desc.versionMajor, int desc.versionMinor, int desc.versionBuild)

    member _.Bake(input: BakeInput, settings: BakeSettings, [<Optional; DefaultParameterValue(BakeFlags.Parallel)>] flags: BakeFlags) =
        if not handle.IsValid then
            raise <| ObjectDisposedException("Baker")

        let flags =
            if messageCallback = null then
                flags &&& ~~~BakeFlags.Validation
            else
                flags

        let samplerAddress =
            match input.AlphaSampler.Address with
            | WrapMode.Wrap       -> API.TextureAddressMode.Wrap
            | WrapMode.Mirror     -> API.TextureAddressMode.Mirror
            | WrapMode.Clamp      -> API.TextureAddressMode.Clamp
            | WrapMode.Border     -> API.TextureAddressMode.Border
            | WrapMode.MirrorOnce -> API.TextureAddressMode.MirrorOnce
            | mode -> raise <| ArgumentException($"Invalid sampler wrap mode: {mode}")

        let samplerFilter =
            match input.AlphaSampler.Filter with
            | FilterMode.Point  -> API.TextureFilterMode.Nearest
            | FilterMode.Linear -> API.TextureFilterMode.Linear
            | mode -> raise <| ArgumentException($"Invalid sampler filter mode: {mode}")

        let samplerDesc =
            API.SamplerDesc(
                samplerAddress, samplerFilter,
                input.AlphaSampler.BorderAlpha
            )

        let indexFormat =
            match input.Indices.GetType().GetElementType() with
            | Int8 | UInt8   -> API.IndexFormat.UInt8
            | Int16 | UInt16 -> API.IndexFormat.UInt16
            | Int32 | UInt32 -> API.IndexFormat.UInt32
            | typ -> raise <| ArgumentException($"Invalid index buffer type: {typ.Name}[]")

        let format =
            match settings.Format with
            | OpacityFormat.Binary -> API.Format.OC1_2_State
            | OpacityFormat.Full   -> API.Format.OC1_4_State
            | fmt -> raise <| ArgumentException($"Invalid opacity format: {fmt}")

        let unresolvedState =
            match settings.UnresolvedTriangleState with
            | OpacityState.Transparent        -> API.SpecialIndex.FullyTransparent
            | OpacityState.Opaque             -> API.SpecialIndex.FullyOpaque
            | OpacityState.UnknownTransparent -> API.SpecialIndex.FullyUnknownTransparent
            | OpacityState.UnknownOpaque      -> API.SpecialIndex.FullyUnknownOpaque
            | state -> raise <| ArgumentException($"Invalid opacity state: {state}")

        use pTexCoords = fixed input.TextureCoords
        use pIndices = new FixedArray(input.Indices)
        use texture = textures.[input.AlphaTexture]

        let mutable inputDesc = Unchecked.defaultof<API.CpuBakeInputDesc>
        inputDesc.bakeFlags                         <- Enum.convert flags
        inputDesc.texture                           <- texture.Handle
        inputDesc.samplerDesc                       <- samplerDesc
        inputDesc.alphaMode                         <- API.AlphaMode.Test
        inputDesc.texCoordFormat                    <- API.TexCoordFormat.Float32
        inputDesc.texCoords                         <- pTexCoords.Address
        inputDesc.texCoordStride                    <- 0u
        inputDesc.indexFormat                       <- indexFormat
        inputDesc.indexBuffer                       <- pIndices.Address
        inputDesc.indexCount                        <- uint32 input.Indices.LongLength
        inputDesc.dynamicSubdivisionScale           <- settings.DynamicSubdivisionScale
        inputDesc.rejectionThreshold                <- settings.RejectionThreshold
        inputDesc.alphaCutoff                       <- settings.AlphaCutoff
        inputDesc.nearDuplicateDeduplicationFactor  <- 0.15f
        inputDesc.alphaCutoffLessEqual              <- OpacityState.Transparent
        inputDesc.alphaCutoffGreater                <- OpacityState.Opaque
        inputDesc.format                            <- format
        inputDesc.formats                           <- NativePtr.zero
        inputDesc.unknownStatePromotion             <- settings.UnknownStatePromotion
        inputDesc.unresolvedTriState                <- unresolvedState
        inputDesc.maxSubdivisionLevel               <- settings.MaxSubdivisionLevel
        inputDesc.maxArrayDataSize                  <- settings.MaxArrayDataSize
        inputDesc.subdivisionLevels                 <- NativePtr.zero
        inputDesc.maxWorkloadSize                   <- UInt64.MaxValue

        let mutable result = API.CpuBakeResult.Null
        API.Omm.cpuBake(handle, &inputDesc, &result) |> Result.check "failed to bake input"

        new Micromap(handle, result, &inputDesc)

    member inline this.Bake(input: BakeInput, [<Optional; DefaultParameterValue(BakeFlags.Parallel)>] flags: BakeFlags) =
        this.Bake(input, BakeSettings.Default, flags)

    member _.Dispose() =
        if handle.IsValid then
            textures.Capacity <- 0L
            API.Omm.destroyBaker handle |> Result.check "failed to destroy baker"
            handle <- API.Baker.Null

    interface IDisposable with
        member this.Dispose() = this.Dispose()