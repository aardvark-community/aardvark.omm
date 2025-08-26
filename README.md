# Aardvark.OMM

[![Publish](https://github.com/aardvark-community/aardvark.omm/actions/workflows/publish.yml/badge.svg)](https://github.com/aardvark-community/aardvark.omm/actions/workflows/publish.yml)
[![Nuget](https://badgen.net/nuget/v/aardvark.omm/pre)](https://www.nuget.org/packages/aardvark.omm/)
[![Downloads](https://badgen.net/nuget/dt/aardvark.omm)](https://www.nuget.org/packages/aardvark.omm/)

A simple wrapper around the [NVIDIA OMM SDK](https://github.com/NVIDIA-RTX/OMM) to bake, save, and load opacity micromaps for raytracing.

Supported platforms:
- Windows (x64)
- Linux (x64)

## Basic Usage
```fsharp
open Aardvark.OMM
open Aardvark.Base
open Aardvark.Rendering.Raytracing

let alphaMask = PixImage.Load("mask.png")
let indices = [| ... |]
let textureCoords = [| ... |]

let input =
    { Indices       = indices
      TextureCoords = textureCoords
      AlphaTexture  = AlphaPixImage(alphaMask)
      AlphaSampler  = AlphaSampler.Default }

let settings =
    { BakeSettings.Default with
        ... }

use baker = new Baker()
use result = baker.Bake(input, settings)
Micromap(result.Data)
```