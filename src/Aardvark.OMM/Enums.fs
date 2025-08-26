namespace Aardvark.OMM

type MessageSeverity =
    | Info        = 0
    | PerfWarning = 1
    | Error       = 2
    | Fatal       = 3

type OpacityState =
    | Transparent        = 0
    | Opaque             = 1
    | UnknownTransparent = 2
    | UnknownOpaque      = 3

type UnknownStatePromotion =
    | Nearest          = 0
    | ForceOpaque      = 1
    | ForceTransparent = 2