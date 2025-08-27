namespace Demo

open Aardvark.Base
open Aardvark.Application
open Aardvark.OMM
open Aardvark.Rendering
open Aardvark.Rendering.Raytracing
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Raytracing
open FSharp.Data.Adaptive
open System

[<AutoOpen>]
module Semantics =
    module HitGroup =
        let Floor = Sym.ofString "HitGroupFloor"
        let Ceiling = Sym.ofString "HitGroupCeiling"
        let Sphere = Sym.ofString "HitGroupSphere"

    module MissShader =
        let Shadow = Sym.ofString "MissShadow"

module Effect =
    open FShade

    [<AutoOpen>]
    module Shader =

        type UniformScope with
            member x.OutputBuffer : Image2d<Formats.rgba32f> = uniform?OutputBuffer
            member x.RecursionDepth : int    = uniform?RecursionDepth
            member x.Positions      : V3f[]  = uniform?StorageBuffer?Positions
            member x.TextureCoords  : V2f[]  = uniform?StorageBuffer?DiffuseColorCoordinates
            member x.Colors         : V3f[]  = uniform?StorageBuffer?Colors

        type Payload =
            {
                color       : V3f
                origin      : V3f
                direction   : V3f
                attenuation : float32
            }

        let private mainScene =
            scene {
                accelerationStructure uniform?MainScene
            }

        let private textureDiffuse =
            sampler2d {
                texture uniform?TextureDiffuse
                filter Filter.MinMagPointMipLinear
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
            }

        let private textureAlpha =
            sampler2d {
                texture uniform?TextureAlpha
                filter Filter.MinMagPointMipLinear
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
            }

        [<ReflectedDefinition>]
        let trace (origin : V3f) (offset : V2f) (input : RayGenerationInput) =
            let pixelCenter = V2f input.work.id.XY + offset
            let inUV = pixelCenter / V2f input.work.size.XY
            let d = inUV * 2.0f - 1.0f

            let target = uniform.ProjTrafoInv * V4f(d, 1.0f, 1.0f)
            let direction = uniform.ViewTrafoInv * V4f(target.XYZ.Normalized, 0.0f)

            let mutable depth = 0
            let mutable final = V3f.Zero

            let mutable payload =
                { color       = V3f.Zero
                  origin      = origin
                  direction   = direction.XYZ
                  attenuation = 1.0f }

            while depth < uniform.RecursionDepth && payload.attenuation > 0.0f do
                let attenuation = payload.attenuation
                payload <- mainScene.TraceRay<Payload>(payload.origin, payload.direction, payload, flags = RayFlags.CullBackFacingTriangles)
                final <- final + payload.color * attenuation
                depth <- depth + 1

            final

        let rgenMain (input : RayGenerationInput) =
            raygen {
                let origin = uniform.ViewTrafoInv * V4f.WAxis

                let c1 = input |> trace origin.XYZ (V2f(0.25f, 0.25f))
                let c2 = input |> trace origin.XYZ (V2f(0.75f, 0.25f))
                let c3 = input |> trace origin.XYZ (V2f(0.25f, 0.75f))
                let c4 = input |> trace origin.XYZ (V2f(0.75f, 0.75f))
                let final = (c1 + c2 + c3 + c4) / 4.0f

                uniform.OutputBuffer.[input.work.id.XY] <- V4f(final, 1.0f)
            }

        let missSky (input : RayMissInput) =
            let top = V3f(0.25f, 0.5f, 1.0f)
            let bottom = V3f C3f.Plum

            miss {
                let color =
                    if input.ray.direction.Z > 0.0f then
                        input.ray.direction.Z |> lerp bottom top
                    else
                        bottom

                return { unchanged with color = color; attenuation = 0.0f }
            }

        let missShadow =
            miss { return false }

        [<ReflectedDefinition>]
        let fromBarycentric (v0 : V3f) (v1 : V3f) (v2 : V3f) (coords : V2f) =
            let barycentricCoords = V3f(1.0f - coords.X - coords.Y, coords.X, coords.Y)
            v0 * barycentricCoords.X + v1 * barycentricCoords.Y + v2 * barycentricCoords.Z

        [<ReflectedDefinition>]
        let fromBarycentric2d (v0 : V2f) (v1 : V2f) (v2 : V2f) (coords : V2f) =
            let barycentricCoords = V3f(1.0f - coords.X - coords.Y, coords.X, coords.Y)
            v0 * barycentricCoords.X + v1 * barycentricCoords.Y + v2 * barycentricCoords.Z

        [<ReflectedDefinition; Inline>]
        let getPosition (input : RayHitInput<'T, V2f>) =
            let p0 = input.hit.positions.[0]
            let p1 = input.hit.positions.[1]
            let p2 = input.hit.positions.[2]
            input.hit.attribute |> fromBarycentric p0 p1 p2

        [<ReflectedDefinition>]
        let getTextureCoords (indices : V3i) (input : RayHitInput<'T, V2f>) =
            let uv0 = uniform.TextureCoords.[indices.X]
            let uv1 = uniform.TextureCoords.[indices.Y]
            let uv2 = uniform.TextureCoords.[indices.Z]
            input.hit.attribute |> fromBarycentric2d uv0 uv1 uv2

        [<ReflectedDefinition>]
        let diffuseLighting (normal : V3f) (position : V3f) =
            let L = uniform.LightLocation - position |> Vec.normalize
            let NdotL = Vec.dot normal L |> max 0.0f

            let ambient = 0.1f
            ambient + 0.9f * NdotL

        [<ReflectedDefinition>]
        let specularLighting (shininess : float32) (normal : V3f) (position : V3f) =
            let L = uniform.LightLocation - position |> Vec.normalize
            let V = uniform.CameraLocation - position |> Vec.normalize
            let R = Vec.reflect normal -L
            let VdotR = Vec.dot V R |> max 0.0f
            0.8f * pow VdotR shininess

        [<ReflectedDefinition>]
        let lightingWithShadow (diffuse : V3f) (reflectiveness : float32) (specularAmount : float32) (shininess : float32)
                               (position : V3f) (normal : V3f) (input : RayHitInput<Payload>) =

            let shadowed =
                let direction = Vec.normalize (uniform.LightLocation - position)
                let flags = RayFlags.SkipClosestHitShader ||| RayFlags.TerminateOnFirstHit ||| RayFlags.CullFrontFacingTriangles
                mainScene.TraceRay<bool>(position, direction, payload = true, miss = "MissShadow", flags = flags, minT = 0.01f)

            let color =
                let diffuse = diffuse * diffuseLighting normal position

                if shadowed then
                    diffuse * 0.3f
                else
                    let specular = specularLighting shininess normal position
                    diffuse + specularAmount * V3f(specular)

            { color       = color
              origin      = position
              direction   = Vec.reflect normal input.ray.direction
              attenuation = input.payload.attenuation * reflectiveness }

        let chitTextured (reflectiveness: float32) (input : RayHitInput<Payload>) =
            closestHit {
                let info = TraceGeometryInfo.ofRayHit input
                let indices = TraceGeometryInfo.getIndices info input

                let position =
                    let p = getPosition input
                    input.objectSpace.objectToWorld.TransformPos p

                let texCoords =
                    getTextureCoords indices input

                let diffuse = textureDiffuse.Sample(texCoords).XYZ
                return lightingWithShadow diffuse reflectiveness 0.5f 28.0f position V3f.ZAxis input
            }

        let ahitCeiling (input : RayHitInput<Payload>) =
            anyHit {
                let info = TraceGeometryInfo.ofRayHit input
                let indices = TraceGeometryInfo.getIndices info input

                let texCoords = getTextureCoords indices input
                let alpha = textureAlpha.Sample(texCoords).X

                if alpha < 0.5f then
                    ignoreIntersection()
            }

        let chitCeiling () =
            closestHit {
                return { unchanged with color = V3f.One; attenuation = 0.0f }
            }

        let chitSphere (input : RayHitInput<Payload>) =
            closestHit {
                let info = TraceGeometryInfo.ofRayHit input
                let position = input.ray.origin + input.hit.t * input.ray.direction
                let center = input.objectSpace.objectToWorld.TransformPos V3f.Zero
                let normal = Vec.normalize (position - center)

                let diffuse = uniform.Colors.[info.InstanceAttributeIndex]
                return lightingWithShadow diffuse 0.4f 0.8f 28.0f position normal input
            }

        let intersectionSphere (radius : float32) (input : RayIntersectionInput) =
            intersection {
                let origin = input.objectSpace.rayOrigin
                let direction = input.objectSpace.rayDirection

                let a = Vec.dot direction direction
                let b = 2.0f * Vec.dot origin direction
                let c = (Vec.dot origin origin) - (radius * radius)

                let discriminant = b * b - 4.0f * a * c
                if discriminant >= 0.0f then
                    let t = (-b - sqrt discriminant) / (2.0f * a)
                    Intersection.Report(t) |> ignore
            }

    let sphereRadius = 2.0f

    let private hitGroupFloor =
        hitgroup {
            closestHit (chitTextured 0.3f)
        }

    let private hitGroupCeiling =
        hitgroup {
            anyHit ahitCeiling
            closestHit chitCeiling
        }

    let private hitGroupSphere =
        hitgroup {
            closestHit chitSphere
            intersection (intersectionSphere sphereRadius)
        }

    let main =
        raytracingEffect {
            raygen rgenMain
            miss missSky
            miss MissShader.Shadow missShadow
            hitgroup HitGroup.Floor hitGroupFloor
            hitgroup HitGroup.Ceiling hitGroupCeiling
            hitgroup HitGroup.Sphere hitGroupSphere
        }

module Program =

    [<EntryPoint>]
    let main argv =
        Aardvark.Init()

        use win =
            window {
                display Display.Mono
                samples 8
                backend Backend.Vulkan
                debug true
            }

        let runtime = win.Runtime

        let cameraView =
            let initialView = CameraView.LookAt(V3d.One * 10.0, V3d.Zero, V3d.OOI)
            DefaultCameraController.control win.Mouse win.Keyboard win.Time initialView

        let viewTrafo =
            cameraView |> AVal.map CameraView.viewTrafo

        let projTrafo =
            win.Sizes
            |> AVal.map (fun s ->
                Frustum.perspective 60.0 0.1 150.0 (float s.X / float s.Y)
                |> Frustum.projTrafo
            )

        use geometryPool =
            let signature =
                let vertexAttributes =
                    Map.ofList [
                        DefaultSemantic.Normals, typeof<V4f>
                        DefaultSemantic.DiffuseColorCoordinates, typeof<V2f>
                    ]

                let instanceAttributes =
                    Map.ofList [
                        DefaultSemantic.Colors, typeof<V3f>
                    ]

                { IndexType              = IndexType.Int32
                  VertexAttributeTypes   = vertexAttributes
                  FaceAttributeTypes     = Map.empty
                  InstanceAttributeTypes = instanceAttributes
                  GeometryAttributeTypes = Map.empty }

            new ManagedTracePool(runtime, signature)

        let quad (z: float32) =
            let positions = [| V3f(-0.5f, -0.5f, z); V3f(-0.5f, 0.5f, z); V3f(0.5f, -0.5f, z); V3f(0.5f, 0.5f, z); |]
            let indices = [| 0; 1; 2; 2; 1; 3 |]
            let uv = positions |> Array.map (fun p -> p.XY + 0.5f)

            let vertexAttr =
                SymDict.ofList [
                    DefaultSemantic.Positions, positions :> Array
                    DefaultSemantic.DiffuseColorCoordinates, uv :> Array
                ]

            IndexedGeometry(IndexedGeometryMode.TriangleList, indices, vertexAttr, SymDict.empty)

        let floor =
            quad 0.0f
            |> TraceObject.ofIndexedGeometry GeometryFlags.Opaque Trafo3d.Identity
            |> TraceObject.transform (Trafo3d.Scale(48.0))
            |> TraceObject.hitGroup HitGroup.Floor
            |> TraceObject.frontFace WindingOrder.CounterClockwise

        let alphaMask = PixImage.Load(Path.combine [ __SOURCE_DIRECTORY__; "data"; "mask.png" ])

        let ceiling =
            let geometry = quad 0.5f

            let micromap =
                let maxLevel = runtime.GetMaxMicromapSubdivisionLevel MicromapFormat.OpacityFourState

                let input =
                    {
                        Indices       = geometry.IndexArray
                        TextureCoords = unbox geometry.IndexedAttributes.[DefaultSemantic.DiffuseColorCoordinates]
                        AlphaTexture  = AlphaPixImage(alphaMask, Col.Channel.Red)
                        AlphaSampler  = AlphaSampler.Default
                    }

                let settings =
                    { BakeSettings.Default with
                        Validation              = true
                        DynamicSubdivisionScale = 0.1f
                        MaxSubdivisionLevel     = uint32 maxLevel }

                use baker = new Baker()
                use result = baker.Bake(input, settings)
                baker.SaveDebugImages(result)
                Micromap(result.Data)

            geometry
            |> TraceObject.ofIndexedGeometry micromap GeometryFlags.None Trafo3d.Identity
            |> TraceObject.transform (Trafo3d.Scale(128.0, 128.0, 64.0))
            |> TraceObject.hitGroup HitGroup.Ceiling

        let spheres =
            let colors = [|
                C3f.BlueViolet
                C3f.DodgerBlue
                C3f.HoneyDew
                C3f.BlanchedAlmond
                C3f.LimeGreen
                C3f.MistyRose
                C3f.CadetBlue
                C3f.FireBrick
                C3f.Gainsboro
            |]

            List.init 9 (fun i ->
                let x = float (i / 3 - 1)
                let y = float (i % 3 - 1)

                let aabbs =
                    BoundingBoxes.FromCenterAndRadius(V3f.Zero, Effect.sphereRadius, GeometryFlags.Opaque)
                    |> Array.singleton
                    |> TraceGeometry.AABBs

                let trafo =
                    Trafo3d.Translation(x * 16.0, y * 16.0, 2.5)

                TraceObject.ofGeometry aabbs
                |> TraceObject.instanceAttribute (DefaultSemantic.Colors, colors.[i])
                |> TraceObject.hitGroup HitGroup.Sphere
                |> TraceObject.transform trafo
            )

        let lightLocation =
            let startTime = DateTime.Now
            win.Time |> AVal.map (fun t ->
                let t = (t - startTime).TotalSeconds
                V3d(Rot2d(t * Constant.PiQuarter) * V2d(8.0, 0.0), 16.0)
            )

        let cameraLocation =
            cameraView |> AVal.map CameraView.location

        let uniforms =
            let custom =
                uniformMap {
                    value   "RecursionDepth" 4
                    value   "ViewTrafo"      viewTrafo
                    value   "ProjTrafo"      projTrafo
                    value   "CameraLocation" cameraLocation
                    texture "TextureDiffuse" DefaultTextures.checkerboard
                    texture "TextureAlpha"   (PixTexture2d alphaMask)
                    value   "LightLocation"  lightLocation
                }

            UniformProvider.union geometryPool.Uniforms custom

        let scene =
            ASet.ofList ([floor; ceiling] @ spheres)
            |> RaytracingScene.ofPool geometryPool

        let pipeline =
            {
                Effect            = Effect.main
                Scenes            = Map.ofList [Sym.ofString "MainScene", scene]
                Uniforms          = uniforms
                MaxRecursionDepth = AVal.constant 2
            }

        let traceOutput =
            runtime.TraceTo2D(win.Sizes, TextureFormat.Rgba8, "OutputBuffer", pipeline)

        let sg =
            Sg.fullScreenQuad
            |> Sg.diffuseTexture traceOutput
            |> Sg.shader {
                do! DefaultSurfaces.diffuseTexture
            }

        win.Scene <- sg
        win.Run()

        0