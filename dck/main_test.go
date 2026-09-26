package multiscreen

import (
	"bytes"
	"image/png"
	"math"
	"testing"

	"github.com/hajimehoshi/ebiten/v2"
	"github.com/olivierh59500/democonstructionkit/effects"
	"github.com/olivierh59500/democonstructionkit/geometry"
	"github.com/olivierh59500/democonstructionkit/presets"
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

func TestCocoDMALogoRecurrences(t *testing.T) {
	var dmaSin, dmaCos, dmaStepSin, dmaStepCos [4]float64
	for i := range dmaSin {
		dmaSin[i], dmaCos[i] = math.Sincos(cocoDMAPhaseOffsets3[i])
		dmaStepSin[i], dmaStepCos[i] = math.Sincos(cocoDMAPhaseDeltas3[i])
	}

	for iteration := 1; iteration <= 10_000; iteration++ {
		for i := range dmaSin {
			if iteration&1023 == 0 {
				phase := cocoDMAPhaseOffsets3[i] + float64(iteration)*cocoDMAPhaseDeltas3[i]
				dmaSin[i], dmaCos[i] = math.Sincos(math.Mod(phase, 2*math.Pi))
			} else {
				dmaSin[i], dmaCos[i] = stepSinCosForward(dmaSin[i], dmaCos[i], dmaStepSin[i], dmaStepCos[i])
			}
			phase := cocoDMAPhaseOffsets3[i] + float64(iteration)*cocoDMAPhaseDeltas3[i]
			wantSin, wantCos := math.Sincos(phase)
			if math.Abs(dmaSin[i]-wantSin) > 1e-11 || math.Abs(dmaCos[i]-wantCos) > 1e-11 {
				t.Fatalf("DMA recurrence %d drift at iteration %d", i, iteration)
			}
		}
	}
}

func TestCocoScrollLetterWrapsAcrossMultipleMessageLoops(t *testing.T) {
	demo := &CocoDemo{
		position:   []int{10, 20, 30},
		scrollText: "ABC",
	}

	if got, want := demo.getPosition3(4), 40; got != want {
		t.Fatalf("position after wrap = %d, want %d", got, want)
	}

	demo.advanceScrollLetter3(95)
	if got, want := demo.letterNum, 9; got != want {
		t.Fatalf("letter after three loops = %d, want %d", got, want)
	}
	if got, want := demo.getLetter3(demo.letterNum), byte('A'); got != want {
		t.Fatalf("wrapped letter = %q, want %q", got, want)
	}

	// Some curve sections move backwards; the active letter must follow them.
	demo.advanceScrollLetter3(5)
	if got, want := demo.letterNum, 0; got != want {
		t.Fatalf("letter after backwards movement = %d, want %d", got, want)
	}
}

func TestCocoScrollerFollowsWaveAcrossThreeFullMessages(t *testing.T) {
	demo := &CocoDemo{
		curves:     make([][]int, 8),
		scrollText: cocoScrollText3,
	}
	demo.initFontData3()
	demo.createCurves()
	demo.precalcPosition()
	demo.precalcMainWave()

	targetLetter := len(demo.scrollText) * 3
	firstWrapWavePos := -1
	for frontWavePos := 0; frontWavePos < 100_000_000 && demo.letterNum < targetLetter; frontWavePos += 600 {
		decalX := demo.scrollOffset3(frontWavePos)
		demo.advanceScrollLetter3(decalX)
		if firstWrapWavePos < 0 && demo.letterNum >= len(demo.scrollText) {
			firstWrapWavePos = frontWavePos
		}
		start := demo.getPosition3(demo.letterNum)
		end := demo.getPosition3(demo.letterNum + 1)
		if decalX < start || decalX >= end {
			t.Fatalf("offset %d is outside active letter %d interval [%d, %d)", decalX, demo.letterNum, start, end)
		}
	}
	if demo.letterNum < targetLetter {
		t.Fatalf("scroller reached only letter %d, want at least %d", demo.letterNum, targetLetter)
	}
	if firstWrapWavePos < 0 {
		t.Fatal("scroller never completed its first message loop")
	}
	t.Logf("first full message loop reached near wave position %d", firstWrapWavePos)
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
