# DCK version

This directory contains the construction-kit version of go-multiscreen. The original Go sources are preserved at their original paths (revision `2cd2bc12a9df9509ba3b3313c2b14543c3a97acc`), with small asset accessors so both versions use the same embedded resources.

Run the original with `go run ./cmd/multiscreen` and this version with `go run ./dck/cmd/multiscreen` from the repository root.

The choreography and assets remain in this repository. Reusable rendering and
effects come from the published `github.com/olivierh59500/democonstructionkit`
module pinned in `go.mod`. Music is opened with `sound.Open`; DCK selects the decoder from the asset and
provides the configured stereo PCM format. The demo keeps its playback level and loop settings.

## Shared component effects

The Phenomena and TCB scenes use the same `DNAFrames`, `Planes` and `PlaneRenderer` implementations as their standalone DCK demos. Their original timing, raster palettes and the multiscreen camera remain local.

The embedded Coco panel now shares `composite.CopperBars` with Bilizir and
standalone Coco. Its integer clocks and 36 cached image strips remain visually
identical across 1,200 compared frames of the isolated panel.

See the [DCK effect configuration guide](../../../lib/democonstructionkit/docs/EFFECT_OPTIONS.md) for the shared API and examples.

Capture the embedded TCB scene without the four-panel camera or audio device:

```sh
go run ./dck/cmd/capture-tcb -frames 1,12,13,38,39,240 -out captures/tcb
```

The TCB panel's central logo uses the same `sprites.AxisFlip` saw cycle and
mirrored source as the standalone screen. Ten isolated captures around its
face changes remain pixel-identical.
Its mountain background also uses the shared `composite.Bands` renderer and
the TCB preset, with integer phase snapping before 2× movement. Twelve
isolated captures around fractional speeds and wraps remain pixel-identical.
The 32-row logo warp now uses `composite.ProfileImage` with the same native
crop, phase reset and parent viewport mapping as the standalone screen. Nine
isolated captures at section joins and wrap remain pixel-identical.
