package multiscreen

import (
	"bytes"
	"image/png"
	"math"
	"testing"

	"github.com/hajimehoshi/ebiten/v2"
	kit "github.com/olivierh59500/democonstructionkit"
	"github.com/olivierh59500/democonstructionkit/composite"
	"github.com/olivierh59500/democonstructionkit/effects"
	"github.com/olivierh59500/democonstructionkit/geometry"
	"github.com/olivierh59500/democonstructionkit/presets"
	"github.com/olivierh59500/democonstructionkit/scrolling"
)

func TestTCBScrollShaderCompiles(t *testing.T) {
	shader, err := ebiten.NewShader([]byte(tcbScrollShaderSource))
	if err != nil {
		t.Fatalf("compile TCB scroller shader: %v", err)
	}
	shader.Deallocate()
}

func TestCompositeShaderCompiles(t *testing.T) {
	shader, err := ebiten.NewShader([]byte(compositeShaderSource))
	if err != nil {
		t.Fatalf("compile camera compositor shader: %v", err)
	}
	shader.Deallocate()
}

func TestTCBRasterIsOpaque(t *testing.T) {
	img, err := png.Decode(bytes.NewReader(demo2RastData))
	if err != nil {
		t.Fatalf("decode TCB raster: %v", err)
	}
	for y := img.Bounds().Min.Y; y < img.Bounds().Max.Y; y++ {
		for x := img.Bounds().Min.X; x < img.Bounds().Max.X; x++ {
			_, _, _, alpha := img.At(x, y).RGBA()
			if alpha != 0xffff {
				t.Fatalf("raster alpha at (%d, %d) = %#04x, want 0xffff", x, y, alpha)
			}
		}
	}
}

func TestPhenomenaCharsetIndex(t *testing.T) {
	for want, ch := range charsetPhenomena {
		got, ok := charToFontIndexPhe(ch)
		if !ok || got != want {
			t.Fatalf("charToFontIndexPhe(%q) = (%d, %v), want (%d, true)", ch, got, ok, want)
		}
	}
	if _, ok := charToFontIndexPhe('^'); ok {
		t.Fatal("control character unexpectedly mapped to a glyph")
	}
}

func TestCubeGeometryIsAllocationFree(t *testing.T) {
	cube, err := effects.NewSolidCube(presets.MultiscreenCocoCube(40))
	if err != nil {
		t.Fatal(err)
	}
	defer cube.Close()
	cube.Rotation = geometry.Vec3{X: .3, Y: .7, Z: 1.1}
	vertices, indices := cube.Geometry(400, 300)
	if len(vertices) != 120 || len(indices) != 180 {
		t.Fatalf("unexpected geometry size %d/%d", len(vertices), len(indices))
	}
	if n := testing.AllocsPerRun(1000, func() { cube.Geometry(400, 300) }); n != 0 {
		t.Fatalf("geometry allocated %g objects", n)
	}
}

func TestCocoLogoPresetKeepsAuthoredGridAndClocks(t *testing.T) {
	config := presets.MultiscreenCocoLogoFormation(nil, demoWidth, demoHeight)
	if config.Count != 16 || config.Grid == nil || config.Grid.Columns != 4 ||
		config.Grid.StepX != 200 || config.Grid.StepY != 140 ||
		config.Origin.X != 400 || config.Origin.Y != 336 ||
		config.ScaleX != .5 || config.ScaleY != .5 || config.Opacity != .6 ||
		!config.AlphaOnly || config.RecurrentTranslation == nil || config.Translation != nil {
		t.Fatalf("Coco logo grid changed: %+v", config)
	}
	clocks := config.RecurrentTranslation
	if clocks.PhaseStep != .02 || clocks.ReanchorEvery != 1024 ||
		len(clocks.XStepDeltas) != 2 || len(clocks.YStepDeltas) != 2 ||
		clocks.XStepDeltas[0] != .02*1.35 || clocks.XStepDeltas[1] != .02*1.86 ||
		clocks.YStepDeltas[0] != .02*1.72 || clocks.YStepDeltas[1] != .02*1.63 {
		t.Fatalf("Coco logo recurrence changed: %+v", clocks)
	}
}

func TestCocoPanelScanlinePreservesThreePixelStrips(t *testing.T) {
	config, err := presets.MultiscreenCocoScanlineScroll(nil, cocoScrollText3)
	if err != nil {
		t.Fatal(err)
	}
	if config.ViewportWidth != 800 || config.ViewportHeight != 528 || config.SurfaceWidth != 1600 ||
		config.SourceRows != 36 || config.RowHeight != 3 || config.StripHeight != 3 ||
		config.WaveStep != 15 || config.CursorRows != 36 || config.BounceAmplitude != 18 ||
		config.BounceRate != .1 || config.Scale != 3 || config.DestinationY != 72 ||
		config.Wrap != scrolling.ScanlineSplit || config.UseTime || config.SurfaceUnmanaged ||
		!config.AlternateDiagonal || config.Background != nil {
		t.Fatalf("embedded Coco scroller geometry changed: %+v", config)
	}
}

func TestCocoPanelScanlineKeepsThreeMessageLoops(t *testing.T) {
	decoded, err := png.Decode(bytes.NewReader(demo3FontData))
	if err != nil {
		t.Fatal(err)
	}
	image := ebiten.NewImageFromImage(decoded)
	defer image.Deallocate()
	atlas, err := presets.FontAtlas("multiscreen-coco", image)
	if err != nil {
		t.Fatal(err)
	}
	config, err := presets.MultiscreenCocoScanlineScroll(atlas, cocoScrollText3)
	if err != nil {
		t.Fatal(err)
	}
	scroll, err := scrolling.New(scrolling.Config{Scanline: &config})
	if err != nil {
		t.Fatal(err)
	}
	defer scroll.Close()
	controller := scroll.ScanlineController()
	if controller == nil {
		t.Fatal("missing scanline controller")
	}

	positions := make([]int, 0, len(cocoScrollText3))
	cumulative := 0
	for _, r := range cocoScrollText3 {
		if _, glyph, ok := atlas.ExactGlyph(r); ok {
			cumulative += int(float64(int(glyph.Advance)) * 3)
			positions = append(positions, cumulative)
		}
	}
	target := len(positions) * 3
	firstWrapTick := -1
	lastLetter := 0
	for tick := 0; tick < 6_666_666 && lastLetter < target; tick += 40 {
		if err := scroll.Update(kit.Frame{Tick: uint64(tick)}); err != nil {
			t.Fatal(err)
		}
		state := controller.State()
		if state.WaveStart != tick*15 {
			t.Fatalf("tick %d wave start %d", tick, state.WaveStart)
		}
		offset := composite.CumulativeAt(config.Wave, state.WaveStart, 0)
		for row := 1; row < 36; row++ {
			offset = min(offset, composite.CumulativeAt(config.Wave, state.WaveStart+row, 0))
		}
		offset = max(0, offset)
		start := 0
		if state.Letter > 0 {
			start = composite.CumulativeAt(positions, state.Letter-1, 0)
		}
		end := composite.CumulativeAt(positions, state.Letter, 0)
		if offset < start || offset >= end || state.Decal != start {
			t.Fatalf("tick %d offset %d is outside letter %d interval [%d, %d), decal %d", tick, offset, state.Letter, start, end, state.Decal)
		}
		if firstWrapTick < 0 && state.Letter >= len(positions) {
			firstWrapTick = tick
		}
		lastLetter = state.Letter
	}
	if firstWrapTick < 0 || lastLetter < target {
		t.Fatalf("scroller stopped at letter %d of %d; first wrap tick %d", lastLetter, target, firstWrapTick)
	}
	if err := scroll.Update(kit.Frame{}); err != nil {
		t.Fatal(err)
	}
	if state := controller.State(); state.Letter != 0 {
		t.Fatalf("backward seek retained letter %d", state.Letter)
	}
}

func TestHalfVolumeIntegerMatchesPreviousFloatConversion(t *testing.T) {
	for value := math.MinInt16; value <= math.MaxInt16; value++ {
		got := int16(value) / 2
		want := int16(float64(int16(value)) * 0.5)
		if got != want {
			t.Fatalf("half volume for %d = %d, want %d", value, got, want)
		}
	}
}

func TestSinCosRecurrence(t *testing.T) {
	const start = 1.234
	sinValue, cosValue := math.Sincos(start)
	for i := 1; i <= 240; i++ {
		sinValue, cosValue = stepSinCosForward(sinValue, cosValue, phenomenaWaveSinStep, phenomenaWaveCosStep)
		wantSin, wantCos := math.Sincos(start + float64(i)/36)
		if math.Abs(sinValue-wantSin) > 1e-12 || math.Abs(cosValue-wantCos) > 1e-12 {
			t.Fatalf("recurrence drift at step %d: got (%g, %g), want (%g, %g)", i, sinValue, cosValue, wantSin, wantCos)
		}
	}
}

func BenchmarkCubeGeometry(b *testing.B) {
	cube, err := effects.NewSolidCube(presets.MultiscreenCocoCube(40))
	if err != nil {
		b.Fatal(err)
	}
	defer cube.Close()
	cube.Rotation = geometry.Vec3{X: .3, Y: .7, Z: 1.1}
	b.ReportAllocs()
	for b.Loop() {
		cube.Geometry(400, 300)
	}
}
