namespace Tests

open Aardvark.Base
open Aardvark.OMM
open System.IO
open System.Reflection
open System.Runtime.InteropServices

open FsUnit
open NUnit.Framework

module internal PixImage =
    let equal (input : PixImage) (output : PixImage) =
        if input.Size <> output.Size || input.ChannelCount <> output.ChannelCount || input.PixFormat.Type <> output.PixFormat.Type then
            false
        else
            input.Visit(
                { new IPixImageVisitor<bool> with
                    member x.Visit (input: PixImage<'TData>) =
                        let mutable result = true
                        let output = output :?> PixImage<'TData>

                        for x in 0 .. output.Size.X - 1 do
                            for y in 0 .. output.Size.Y - 1 do
                                for c in 0 .. output.ChannelCount - 1 do
                                    let inputData = input.GetChannel(int64 c)
                                    let outputData = output.GetChannel(int64 c)
                                    result <- result && Unchecked.equals outputData.[x, y] inputData.[x, y]

                        result
                }
            )

module internal Path =

    let rec getRandomName() =
        let name = Path.GetRandomFileName()
        if File.Exists name || Directory.Exists name then getRandomName()
        else name

    let tempDir (action: string -> 'T) =
        let name = getRandomName()
        Directory.CreateDirectory name |> ignore

        try
            action name
        finally
            if Directory.Exists name then Directory.Delete(name, true)

module OmmTests =

    [<OneTimeSetUp>]
    let init() =
        IntrospectionProperties.CustomEntryAssembly <- Assembly.GetExecutingAssembly()
        Aardvark.Init()

    let private checkSize<'T> (expectedSize: int) =
        sizeof<'T> |> should equal expectedSize
        Marshal.SizeOf<'T>() |> should equal expectedSize

    let private checkHandleSize<'T>() =
        checkSize<'T> sizeof<nativeint>

    let private sampleInput() =
        let indices =
            [|
               0; 1; 2
               1; 3; 2
               3; 4; 2
               2; 4; 0
            |]

        let textureCoords =
            [|
                V2f(0.05f, 0.50f)
                V2f(0.50f, 0.05f)
                V2f(0.50f, 0.50f)
                V2f(0.95f, 0.50f)
                V2f(0.50f, 0.95f)
            |]

        let alphaTexture = PixImage<float32>(Col.Format.Alpha, 256L, 256L)
        alphaTexture.Matrix.SetByCoord (fun (coord: V2l) ->
            let uv = V2d coord / V2d (alphaTexture.Size - 1)
            let d = Vec.length (uv - 0.5)
            if d > 0.2 && d < 0.3 then 1.0f else 0.0f
        ) |> ignore

        {
            Indices       = indices
            TextureCoords = textureCoords
            AlphaTexture  = alphaTexture
            AlphaSampler  = AlphaSampler.Default
        }

    [<Test>]
    let ``Struct and handle sizes``() =
        checkHandleSize<API.Baker>()
        checkHandleSize<API.CpuTexture>()
        checkHandleSize<API.CpuBakeResult>()
        checkHandleSize<API.CpuSerializedResult>()
        checkHandleSize<API.CpuDeserializedResult>()
        checkSize<API.CpuBakeInputDesc> 136
        checkSize<API.CpuBakeResultDesc> 80
        checkSize<API.DebugSaveImagesDesc> 24

    [<Test>]
    let ``Library version``() =
        let version = Baker.LibraryVersion
        printfn "Version: %A" version
        version.Major |> should greaterThan 0uy

    [<Test>]
    let ``Create baker``() =
        let baker = new Baker()
        baker.Dispose()

    [<Test>]
    let ``Save debug images``() =
        use baker = new Baker()
        use micromap = baker.Bake(sampleInput())

        Path.tempDir (fun outputDir ->
            baker.SaveDebugImages(micromap, outputDir)
            let files = Directory.GetFiles(outputDir, "*.png")

            files.Length |> should equal 1

            let result = PixImage.Load(files.[0])
            let expected = PixImage.Load(Path.combine [__SOURCE_DIRECTORY__; "data"; "result.png"])

            PixImage.equal expected result |> should be True
        )

    [<Test>]
    let Serialization ([<Values>] compress: bool) =
        use baker = new Baker()
        use original = baker.Bake(sampleInput())

        use stream = new MemoryStream()
        baker.Serialize(original, stream, compress)
        let dataOriginal = stream.ToArray()

        stream.Seek(0L, SeekOrigin.Begin) |> ignore
        let deserialized = baker.Deserialize stream

        stream.Seek(0L, SeekOrigin.Begin) |> ignore
        baker.Serialize(deserialized, stream, compress)
        let dataDeserialized = stream.ToArray()

        dataDeserialized |> should equal dataOriginal