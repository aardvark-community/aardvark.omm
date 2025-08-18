namespace Aardvark.OMM.API

open System
open System.Runtime.InteropServices

[<Flags>]
type CpuSerializeFlags =
    | None     = 0
    | Compress = 1

[<Struct; StructLayout(LayoutKind.Sequential)>]
type CpuBlobDesc =
    struct
        val data : nativeint
        val size : uint64
        new (data, size) = { data = data; size = size }
    end

[<Struct; StructLayout(LayoutKind.Sequential)>]
type CpuDeserializedDesc =
    struct
        val flags          : CpuSerializeFlags
        val numInputDescs  : int
        val inputDescs     : nativeptr<CpuBakeInputDesc>
        val numResultDescs : int
        val resultDescs    : nativeptr<CpuBakeResultDesc>

        new (flags, numInputDescs, inputDescs, numResultDescs, resultDescs) =
            {
                flags          = flags
                numInputDescs  = numInputDescs
                inputDescs     = inputDescs
                numResultDescs = numResultDescs
                resultDescs    = resultDescs
            }
    end

[<Struct>]
type CpuSerializedResult =
    private | CpuSerializedResult of nativeint
    member this.IsValid = let (CpuSerializedResult ptr) = this in ptr <> 0n
    static member Null = CpuSerializedResult 0n

[<Struct>]
type CpuDeserializedResult =
    private | CpuDeserializedResult of nativeint
    member this.IsValid = let (CpuDeserializedResult ptr) = this in ptr <> 0n
    static member Null = CpuDeserializedResult 0n