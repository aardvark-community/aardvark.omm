namespace Aardvark.OMM.API

open Aardvark.OMM
open System.Runtime.InteropServices
open System.Security

type Result =
    | Success                   = 0
    | Failure                   = 1
    | InvalidArgument           = 2
    | InsufficientScratchMemory = 3
    | NotImplemented            = 4
    | WorkloadTooBig            = 5

[<SuppressUnmanagedCodeSecurity>]
module internal Omm =

    [<Literal>]
    let private lib = "omm-lib"

    [<DllImport(lib, EntryPoint = "ommGetLibraryDesc")>]
    extern LibraryDesc getLibraryDesc()


    [<DllImport(lib, EntryPoint = "ommCreateBaker")>]
    extern Result createBaker([<In>] BakerCreationDesc& createInfo, [<Out>] Baker& outBaker)

    [<DllImport(lib, EntryPoint = "ommDestroyBaker")>]
    extern Result destroyBaker(Baker baker)


    [<DllImport(lib, EntryPoint = "ommCpuCreateTexture")>]
    extern Result cpuCreateTexture(Baker baker, [<In>] CpuTextureDesc& desc, [<Out>] CpuTexture& outTexture)

    [<DllImport(lib, EntryPoint = "ommCpuGetTextureDesc")>]
    extern Result cpuGetTextureDesc(CpuTexture texture, [<Out>] CpuTextureDesc& outDesc)

    [<DllImport(lib, EntryPoint = "ommCpuDestroyTexture")>]
    extern Result cpuDestroyTexture(Baker baker, CpuTexture texture)


    [<DllImport(lib, EntryPoint = "ommCpuBake")>]
    extern Result cpuBake(Baker baker, [<In>] CpuBakeInputDesc& bakeInputDesc, [<Out>] CpuBakeResult& outBakeResult)

    [<DllImport(lib, EntryPoint = "ommCpuDestroyBakeResult")>]
    extern Result cpuDestroyBakeResult(CpuBakeResult bakeResult)

    [<DllImport(lib, EntryPoint = "ommCpuGetBakeResultDesc")>]
    extern Result cpuGetBakeResultDesc(CpuBakeResult bakeResult, CpuBakeResultDesc* & desc)


    [<DllImport(lib, EntryPoint = "ommCpuSerialize")>]
    extern Result cpuSerialize(Baker baker, [<In>] CpuDeserializedDesc& desc, [<Out>] CpuSerializedResult& outResult)

    [<DllImport(lib, EntryPoint = "ommCpuGetSerializedResultDesc")>]
    extern Result cpuGetSerializedResultDesc(CpuSerializedResult result, CpuBlobDesc* & desc)

    [<DllImport(lib, EntryPoint = "ommCpuDestroySerializedResult")>]
    extern Result cpuDestroySerializedResult(CpuSerializedResult result)


    [<DllImport(lib, EntryPoint = "ommCpuDeserialize")>]
    extern Result cpuDeserialize(Baker baker, [<In>] CpuBlobDesc& desc, [<Out>] CpuDeserializedResult& outResult)

    [<DllImport(lib, EntryPoint = "ommCpuGetDeserializedDesc")>]
    extern Result cpuGetDeserializedDesc(CpuDeserializedResult result, CpuDeserializedDesc* & desc)

    [<DllImport(lib, EntryPoint = "ommCpuDestroyDeserializedResult")>]
    extern Result cpuDestroyDeserializedResult(CpuDeserializedResult result)


    [<DllImport(lib, EntryPoint = "ommDebugSaveAsImages")>]
    extern Result debugSaveAsImages(Baker baker, [<In>] CpuBakeInputDesc& bakeInputDesc, [<In>] CpuBakeResultDesc& res, [<In>] DebugSaveImagesDesc& desc)

    [<DllImport(lib, EntryPoint = "ommDebugGetStats")>]
    extern Result debugGetStats(Baker baker, [<In>] CpuBakeResultDesc& res, [<Out>] DebugStats& out)

    [<DllImport(lib, EntryPoint = "ommDebugGetStats2")>]
    extern Result debugGetStats2(Baker baker, CpuBakeResult res, [<Out>] DebugStats& out)

    [<DllImport(lib, EntryPoint = "ommDebugSaveBinaryToDisk")>]
    extern Result debugSaveBinaryToDisk(Baker baker, [<In>] CpuBlobDesc& data, string path)