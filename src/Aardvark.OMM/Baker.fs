namespace Aardvark.OMM

open Aardvark.Base
open Aardvark.Rendering
open FSharp.NativeInterop
open System
open System.IO
open System.Runtime.InteropServices
open TypeMeta

#nowarn "9"
#nowarn "51"

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
        AlphaTexture  : IAlphaTexture

        /// Describes how the alpha texture is sampled.
        /// Should be equivalent to how the texture is sampled at runtime.
        AlphaSampler  : AlphaSampler
    }

type MicromapFormat =
    /// Encode the opacity of a triangle with a single bit (transparent or opaque).
    | OpacityTwoState  = 1us

    /// Encode the opacity of a triangle with two bits (transparent, opaque, and two unknown states that are to be resolved in an anyhit shader).
    | OpacityFourState = 2us

type DuplicateDetection =
    /// Will disable reuse of OMMs and instead produce duplicates omm-array data. Generally only needed for debug purposes.
    | None = 0

    /// Reuse duplicate OMMs.
    | Exact = 1

    /// This enables merging of "similar" OMMs where similarity is measured using hamming distance.
    /// UT and UO are considered identical.
    /// Pros: normally reduces resulting OMM size drastically, especially when there's overlapping UVs.
    /// Cons: The merging comes at the cost of coverage.
    /// The resulting OMM Arrays will have lower fraction of known states. For large working sets it can be quite CPU heavy to
    /// compute.
    | Near = 2

[<CLIMutable>]
type BakeSettings =
    {
        /// The format of the micromap.
        Format                  : MicromapFormat

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
        MaxSubdivisionLevel     : uint32

        /// Max allowed size in bytes of ommCpuBakeResultDesc::arrayData
        /// The baker will choose to downsample the most appropriate omm blocks (based on area, reuse, coverage and other factors)
        /// until this limit is met.
        MaxArrayDataSize        : uint32

        /// Parallelize the baking process by using multi-threading.
        Parallel                : bool

        /// Disable the use of special indices in case the OMM-state is uniform. Only set to true for debug purposes.
        /// Note: This prevents promotion of fully known OMMs to use special indices, however for invalid & degenerate UV triangles
        /// special indices may still be set.
        DisableSpecialIndices   : bool

        /// Force 32-bit index format for the output OMM index buffer.
        Force32BitIndices       : bool

        /// Determines if and how duplicate OMMs are reused.
        DuplicateDetection      : DuplicateDetection

        /// Enable additional validation, when enabled additional processing is performed to validate quality and sanity of input data
        /// which may help diagnose omm bake result or longer than expected bake times.
        Validation              : bool
    }

    static member Default =
        { Format                  = MicromapFormat.OpacityFourState
          DynamicSubdivisionScale = 2.0f
          RejectionThreshold      = 0.0f
          AlphaCutoff             = 0.5f
          UnknownStatePromotion   = UnknownStatePromotion.ForceOpaque
          UnresolvedTriangleState = OpacityState.UnknownOpaque
          MaxSubdivisionLevel     = 8u
          MaxArrayDataSize        = UInt32.MaxValue
          Parallel                = true
          DisableSpecialIndices   = false
          Force32BitIndices       = false
          DuplicateDetection      = DuplicateDetection.Exact
          Validation              = false }


/// CPU-based baker for opacity micromaps.
type Baker (messageCallback: Action<MessageSeverity, string>) =
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

    static let defaultMessageLogger (severity: MessageSeverity) (message: string) =
        match severity with
        | MessageSeverity.Error | MessageSeverity.Fatal ->
            Log.error "[OMM] %s" message

        | MessageSeverity.PerfWarning ->
            Log.warn "[OMM] %s" message

        | _ ->
            Log.line "[OMM] %s" message

    new ([<Optional; DefaultParameterValue(true)>] logMessages: bool) =
        let callback = if logMessages then Action<_, _> defaultMessageLogger else null
        new Baker(callback)

    member _.Handle = handle

    /// The version of the NVIDIA OMM SDK
    static member LibraryVersion =
        let desc = API.Omm.getLibraryDesc()
        Version(int desc.versionMajor, int desc.versionMinor, int desc.versionBuild)

    /// <summary>
    /// Prepares a texture for baking.
    /// </summary>
    /// <param name="texture">The texture to prepare.</param>
    member _.PrepareTexture(texture: IAlphaTexture) : INativeAlphaTexture =
        if not handle.IsValid then
            raise <| ObjectDisposedException("Baker")

        NativeAlphaTexture.ofAlphaTexture handle texture

    /// <summary>
    /// Bake the given input data into a micromap.
    /// </summary>
    /// <param name="input">The input data to process.</param>
    /// <param name="settings">The settings to use for the bake process.</param>
    member _.Bake(input: BakeInput, settings: BakeSettings) : CpuMicromap =
        if not handle.IsValid then
            raise <| ObjectDisposedException("Baker")

        let flags =
            [
                if settings.Parallel then
                    API.CpuBakeFlags.EnableInternalThreads

                if settings.DisableSpecialIndices then
                    API.CpuBakeFlags.DisableSpecialIndices

                if settings.Force32BitIndices then
                    API.CpuBakeFlags.Force32BitIndices

                match settings.DuplicateDetection with
                | DuplicateDetection.None -> API.CpuBakeFlags.DisableDuplicateDetection
                | DuplicateDetection.Near -> API.CpuBakeFlags.EnableNearDuplicateDetection
                | _ -> ()

                if settings.Validation && messageCallback <> null then
                    API.CpuBakeFlags.EnableValidation
            ]
            |> List.fold (|||) API.CpuBakeFlags.None

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
            if input.Indices = null then
                raise <| NullReferenceException("Index buffer must not be null.")

            match input.Indices.GetType().GetElementType() with
            | Int8 | UInt8   -> API.IndexFormat.UInt8
            | Int16 | UInt16 -> API.IndexFormat.UInt16
            | Int32 | UInt32 -> API.IndexFormat.UInt32
            | typ -> raise <| ArgumentException($"Invalid index buffer type: {typ.Name}[]")

        let format =
            match settings.Format with
            | MicromapFormat.OpacityTwoState  -> API.Format.OC1_2_State
            | MicromapFormat.OpacityFourState -> API.Format.OC1_4_State
            | fmt -> raise <| ArgumentException($"Invalid micromap format: {fmt}")

        let unresolvedState =
            match settings.UnresolvedTriangleState with
            | OpacityState.Transparent        -> API.SpecialIndex.FullyTransparent
            | OpacityState.Opaque             -> API.SpecialIndex.FullyOpaque
            | OpacityState.UnknownTransparent -> API.SpecialIndex.FullyUnknownTransparent
            | OpacityState.UnknownOpaque      -> API.SpecialIndex.FullyUnknownOpaque
            | state -> raise <| ArgumentException($"Invalid opacity state: {state}")

        use pTexCoords = fixed input.TextureCoords
        use pIndices = new FixedArray(input.Indices)
        use texture = NativeAlphaTexture.ofAlphaTexture handle input.AlphaTexture

        let mutable inputDesc = Unchecked.defaultof<API.CpuBakeInputDesc>
        inputDesc.bakeFlags                         <- flags
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
        inputDesc.maxSubdivisionLevel               <- uint8 <| min settings.MaxSubdivisionLevel 12u
        inputDesc.maxArrayDataSize                  <- settings.MaxArrayDataSize
        inputDesc.subdivisionLevels                 <- NativePtr.zero
        inputDesc.maxWorkloadSize                   <- UInt64.MaxValue

        let mutable result = API.CpuBakeResult.Null
        API.Omm.cpuBake(handle, &inputDesc, &result) |> Result.check "failed to bake input"

        try
            let mutable pResultDesc = NativePtr.zero<API.CpuBakeResultDesc>
            API.Omm.cpuGetBakeResultDesc(result, &pResultDesc) |> Result.check "failed to get bake result"

            CpuMicromap.ofBakeResult result texture inputDesc pResultDesc.[0]
        with _ ->
            API.Omm.cpuDestroyBakeResult result |> ignore
            reraise()

    /// <summary>
    /// Bakes the given input data into a micromap.
    /// </summary>
    /// <param name="input">The input data to process.</param>
    member inline this.Bake(input: BakeInput) : CpuMicromap =
        this.Bake(input, BakeSettings.Default)

    /// <summary>
    /// Serializes the given micromap and writes it to a stream.
    /// </summary>
    /// <param name="micromap">The micromap to serialize.</param>
    /// <param name="stream">The stream to write the serialized data to.</param>
    /// <param name="compress">Indicates whether the data is compressed. Default is false.</param>
    member _.Serialize(micromap: CpuMicromap, stream: Stream, [<Optional; DefaultParameterValue(false)>] compress: bool) =
        if not handle.IsValid then
            raise <| ObjectDisposedException("Baker")

        let flags = if compress then API.CpuSerializeFlags.Compress else API.CpuSerializeFlags.None

        let mutable desc = API.CpuDeserializedDesc(flags, 1, &&micromap.inputDesc, 1, &&micromap.resultDesc)
        let mutable result = API.CpuSerializedResult.Null
        API.Omm.cpuSerialize(handle, &desc, &result) |> Result.check "failed to serialize micromaps"

        try
            let mutable pBlob = NativePtr.zero<API.CpuBlobDesc>
            API.Omm.cpuGetSerializedResultDesc(result, &pBlob) |> Result.check "failed to retrieve serialized result description"

            use src = new UnmanagedMemoryStream(NativePtr.ofNativeInt pBlob.[0].data, int64 pBlob.[0].size)
            src.CopyTo(stream)
        finally
            API.Omm.cpuDestroySerializedResult result |> Result.check "failed destroy serialized result description"

    /// <summary>
    /// Serializes the given micromap and write it to a file.
    /// </summary>
    /// <param name="micromap">The micromap to serialize.</param>
    /// <param name="path">The path of the file to write the serialized data to.</param>
    /// <param name="compress">Indicates whether the data is compressed. Default is false.</param>
    member inline this.Serialize(micromap: CpuMicromap, path: string, [<Optional; DefaultParameterValue(false)>] compress: bool) =
        use stream = File.OpenWrite(path)
        this.Serialize(micromap, stream, compress)

    /// <summary>
    /// Deserializes a micromap from the given stream.
    /// </summary>
    /// <param name="stream">The stream to read the serialized data from.</param>
    member this.Deserialize(stream: Stream) : CpuMicromap =
        if not handle.IsValid then
            raise <| ObjectDisposedException("Baker")

        let data = Stream.readAllBytes stream
        use pData = fixed data

        let mutable blobDesc = API.CpuBlobDesc(pData.Address, uint64 data.LongLength)
        let mutable result = API.CpuDeserializedResult.Null
        API.Omm.cpuDeserialize(handle, &blobDesc, &result) |> Result.check "failed to deserialize micromaps"

        try
            let mutable pDesc = NativePtr.zero<API.CpuDeserializedDesc>
            API.Omm.cpuGetDeserializedDesc(result, &pDesc) |> Result.check "failed to retrieve deserialize result description"
            let desc = pDesc.[0]

            if desc.numInputDescs <> 1 then
                failwithf $"Deserialized data contains {desc.numInputDescs} input descriptions but expected 1"

            if desc.numResultDescs <> 1 then
                failwithf $"Deserialized data contains {desc.numResultDescs} result descriptions but expected 1"

            CpuMicromap.ofDeserializedResult result desc.inputDescs.[0] desc.resultDescs.[0]
        with _ ->
            API.Omm.cpuDestroyDeserializedResult result |> ignore
            reraise()

    /// <summary>
    /// Deserializes a micromap from the given file.
    /// </summary>
    /// <param name="path">The path of the file to read the serialized data from.</param>
    member inline this.Deserialize(path: string) : CpuMicromap =
        use stream = File.OpenRead(path)
        this.Deserialize(stream)

    /// <summary>
    /// Saves debug images to the given path.
    /// </summary>
    /// <param name="micromap">The micromap to save.</param>
    /// <param name="path">The path of the destination folder, or null for using the working directory.</param>
    /// <param name="detailedCutout">If true, generates a cropped and zoomed-in view of the micromap. Default is false.</param>
    member this.SaveDebugImages(micromap: CpuMicromap,
                                [<Optional; DefaultParameterValue(null : string)>] path: string,
                                [<Optional; DefaultParameterValue(false)>] detailedCutout: bool) =
        if not handle.IsValid then
            raise <| ObjectDisposedException("Baker")

        let path =
            if path = null then
                Directory.GetCurrentDirectory()
            else
                Path.GetFullPath path

        let mutable desc = API.DebugSaveImagesDesc(path, "", detailedCutout, false, false, not detailedCutout)
        API.Omm.debugSaveAsImages(handle, &micromap.inputDesc, &micromap.resultDesc, &desc) |> Result.check "failed to save debug images"

    member _.Dispose() =
        if handle.IsValid then
            API.Omm.destroyBaker handle |> Result.check "failed to destroy baker"
            handle <- API.Baker.Null

    interface IDisposable with
        member this.Dispose() = this.Dispose()