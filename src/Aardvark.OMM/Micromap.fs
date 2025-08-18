namespace Aardvark.OMM

open System

[<AbstractClass>]
type Micromap =
    val mutable internal inputDesc : API.CpuBakeInputDesc
    val mutable internal resultDesc : API.CpuBakeResultDesc

    internal new (inputDesc: inref<_>, resultDesc: inref<_>) =
        { inputDesc = inputDesc; resultDesc = resultDesc }

    abstract member Dispose : unit -> unit

    interface IDisposable with
        member this.Dispose() = this.Dispose()

type internal BakedMicromap =
    inherit Micromap
    val mutable private handle : API.CpuBakeResult

    new (handle, inputDesc: inref<_>, resultDesc: inref<_>) =
        { inherit Micromap(&inputDesc, &resultDesc); handle = handle }

    override this.Dispose() =
        if this.handle.IsValid then
            API.Omm.cpuDestroyBakeResult(this.handle) |> Result.check "failed to destroy bake result"
            this.handle <- API.CpuBakeResult.Null

type internal DeserializedMicromap =
    inherit Micromap
    val mutable private handle : API.CpuDeserializedResult

    new (handle, inputDesc: inref<_>, resultDesc: inref<_>) =
        { inherit Micromap(&inputDesc, &resultDesc); handle = handle }

    override this.Dispose() =
        if this.handle.IsValid then
            API.Omm.cpuDestroyDeserializedResult(this.handle) |> Result.check "failed to destroy deserialized result"
            this.handle <- API.CpuDeserializedResult.Null