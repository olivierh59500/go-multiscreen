# DCK version

This directory contains the construction-kit version of go-multiscreen. The original Go sources are preserved at their original paths (revision `2cd2bc12a9df9509ba3b3313c2b14543c3a97acc`), with small asset accessors so both versions use the same embedded resources.

Run the original with `go run ./cmd/multiscreen` and this version with `go run ./dck/cmd/multiscreen` from the repository root.

The choreography and assets remain in this repository. Reusable rendering and
effects come from the published `github.com/olivierh59500/democonstructionkit`
module pinned in `go.mod`. Music is opened with `sound.Open`; DCK selects the decoder from the asset and
provides the configured stereo PCM format. The demo keeps its playback level and loop settings.

## Shared component effects

The Phenomena and TCB scenes use the same `DNAFrames`, `Planes` and `PlaneRenderer` implementations as their standalone DCK demos. Their original timing, raster palettes and the multiscreen camera remain local.

See the [DCK effect configuration guide](../../../lib/democonstructionkit/docs/EFFECT_OPTIONS.md) for the shared API and examples.
