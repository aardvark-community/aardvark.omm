namespace Aardvark.OMM.API

open System
open System.Runtime.InteropServices
open Aardvark.OMM

[<Flags>]
type CpuBakeFlags =
    | None                         = 0u
    | EnableInternalThreads        = 1u
    | DisableSpecialIndices        = 2u
    | Force32BitIndices            = 4u
    | DisableDuplicateDetection    = 8u
    | EnableNearDuplicateDetection = 16u
    | EnableValidation             = 32u
    | Allow8BitIndices             = 64u

type AlphaMode =
    | Test  = 0
    | Blend = 1

type TexCoordFormat =
    | UNorm16 = 0
    | Float16 = 1
    | Float32 = 2

type IndexFormat =
    | UInt16 = 0
    | UInt32 = 1
    | UInt8  = 2

type Format =
    | Invalid     = 0
    | OC1_2_State = 1
    | OC1_4_State = 2

type SpecialIndex =
    | FullyTransparent        = -1
    | FullyOpaque             = -2
    | FullyUnknownTransparent = -3
    | FullyUnknownOpaque      = -4

[<Struct; StructLayout(LayoutKind.Sequential)>]
type CpuBakeInputDesc =
    struct
        val mutable bakeFlags                        : CpuBakeFlags
        val mutable texture                          : CpuTexture
        val mutable samplerDesc                      : SamplerDesc
        val mutable alphaMode                        : AlphaMode
        val mutable texCoordFormat                   : TexCoordFormat
        val mutable texCoords                        : nativeint
        val mutable texCoordStride                   : uint32
        val mutable indexFormat                      : IndexFormat
        val mutable indexBuffer                      : nativeint
        val mutable indexCount                       : uint32
        val mutable dynamicSubdivisionScale          : float32
        val mutable rejectionThreshold               : float32
        val mutable alphaCutoff                      : float32
        val mutable nearDuplicateDeduplicationFactor : float32
        val mutable alphaCutoffLessEqual             : OpacityState
        val mutable alphaCutoffGreater               : OpacityState
        val mutable format                           : Format
        val mutable formats                          : nativeptr<Format>
        val mutable unknownStatePromotion            : UnknownStatePromotion
        val mutable unresolvedTriState               : SpecialIndex
        val mutable maxSubdivisionLevel              : uint8
        val mutable maxArrayDataSize                 : uint32
        val mutable subdivisionLevels                : nativeptr<uint8>
        val mutable maxWorkloadSize                  : uint64

        new(bakeFlags, texture, samplerDesc, alphaMode, texCoordFormat, texCoords, texCoordStride,
            indexFormat, indexBuffer, indexCount, dynamicSubdivisionScale, rejectionThreshold,
            alphaCutoff, nearDuplicateDeduplicationFactor, alphaCutoffLessEqual, alphaCutoffGreater,
            format, formats, unknownStatePromotion, unresolvedTriState, maxSubdivisionLevel, maxArrayDataSize,
            subdivisionLevels, maxWorkloadSize) =
           {
              bakeFlags                        = bakeFlags
              texture                          = texture
              samplerDesc                      = samplerDesc
              alphaMode                        = alphaMode
              texCoordFormat                   = texCoordFormat
              texCoords                        = texCoords
              texCoordStride                   = texCoordStride
              indexFormat                      = indexFormat
              indexBuffer                      = indexBuffer
              indexCount                       = indexCount
              dynamicSubdivisionScale          = dynamicSubdivisionScale
              rejectionThreshold               = rejectionThreshold
              alphaCutoff                      = alphaCutoff
              nearDuplicateDeduplicationFactor = nearDuplicateDeduplicationFactor
              alphaCutoffLessEqual             = alphaCutoffLessEqual
              alphaCutoffGreater               = alphaCutoffGreater
              format                           = format
              formats                          = formats
              unknownStatePromotion            = unknownStatePromotion
              unresolvedTriState               = unresolvedTriState
              maxSubdivisionLevel              = maxSubdivisionLevel
              maxArrayDataSize                 = maxArrayDataSize
              subdivisionLevels                = subdivisionLevels
              maxWorkloadSize                  = maxWorkloadSize
           }
    end