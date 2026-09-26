# DCK version

This directory contains the construction-kit version of go-multiscreen. The original Go sources are preserved at their original paths (revision `2cd2bc12a9df9509ba3b3313c2b14543c3a97acc`), with small asset accessors so both versions use the same embedded resources.

Run the original with `go run ./cmd/multiscreen` and this version with `go run ./dck/cmd/multiscreen` from the repository root.

The choreography and assets remain in this repository. Reusable rendering and
effects come from the published `github.com/olivierh59500/democonstructionkit`
module pinned in `go.mod`. Music is opened with `sound.Open`; DCK selects the decoder from the asset and
provides the configured stereo PCM format. The demo keeps its playback level and loop settings.

## Shared component effects

The Phenomena and TCB scenes use the same `DNAFrames`, `SliceProgram`, `RecurrentRowWave`, reusable gradient/silhouette materials, `Planes` and `PlaneRenderer` implementations as their standalone DCK demos. Their artwork, authored layer order and the multiscreen camera remain local.

The embedded Coco panel now shares `composite.CopperBars` with Bilizir and
standalone Coco. Its integer clocks and 36 cached image strips remain visually
identical across 1,200 compared frames of the isolated panel.
Its twelve cubes now use `effects.SolidCubeTrain` with the panel's material and
1,024-tick reanchored sine/cosine path. The shared component owns the phases,
rotations and bounded draw batch; the original embedded scene remains intact.
The Coco and Viva backgrounds now use `composite.RotozoomBackground` with two
panel presets. Their separate texture phases and tint are data, while the
harmonic motion and repeated viewport sampling come from DCK.
The embedded Coco text now uses the complete `scrolling.Config.Scanline`
transport. Its preset keeps the panel's proportional font, cumulative wave,
three-pixel strip mesh, bounce, source-window split and continuous message loop;
the local cursor and triangle-building functions have been removed.
The sixteen DMA logos now use `sprites.Group` with a 4 × 4 grid and a shared
recurrent harmonic translation. DCK owns the four phase clocks and their
1,024-tick reanchor; the panel supplies only its image and rendering order.
The top title uses the shared `motion.WaveClock` cosine path. The black banner,
copper bars and logo still draw directly into the panel to avoid another GPU
surface.

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
isolated captures at section joins and wrap match the earlier per-row sine
distortion in all color channels. The complete TCB panel now uses
`effects.MultiPlaneScene`; the Multiscreen camera remains its host.
Projected text now enters through the unified `scrolling.New` facade, with the
same font metrics, plane forms and 2× viewport placement. Eight isolated
captures through frame 8,000 remain pixel-identical.

The isolated Viva panel can also be captured without the four-panel camera or
audio device using `go run ./dck/cmd/capture-viva -frames 1,60,240,600`.
Its four pseudo-3D text banks use the same DCK transport as standalone Viva,
with this panel's messages, atlas slices and 800×600 bounds. Eight captures
through frame 4,800 remain identical in all color and alpha channels.
Its ten logos now use the same `sprites.RecurrentFormation` with the panel's
37.5-pixel vertical amplitude. Eight captures remain identical in every channel.
Its title uses the same `composite.RasterTitle` in direct clipped mode, without
an intermediate title surface. Fourteen captures through wrap and title-cue
boundaries remain identical in every channel.
The horizontal title path uses the same `motion.WaveClock` with no initial
hold; fourteen captures remain identical around the standalone release tick.
