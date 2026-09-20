package multiscreen

import (
	"bytes"
	"image/png"
	"math"
	"testing"

	"github.com/hajimehoshi/ebiten/v2"
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
	cube := Cube3D{angleX: 0.3, angleY: 0.7, angleZ: 1.1, size: 40}
	vertices := make([]ebiten.Vertex, 0, len(cubeFaces3)*20)
	indices := make([]uint16, 0, len(cubeFaces3)*30)

	vertices, indices = cube.appendGeometry(vertices, indices, 400, 300)
	if got, want := len(vertices), len(cubeFaces3)*20; got != want {
		t.Fatalf("vertex count = %d, want %d", got, want)
	}
	if got, want := len(indices), len(cubeFaces3)*30; got != want {
		t.Fatalf("index count = %d, want %d", got, want)
	}

	allocs := testing.AllocsPerRun(1000, func() {
		vertices, indices = cube.appendGeometry(vertices[:0], indices[:0], 400, 300)
	})
	if allocs != 0 {
		t.Fatalf("cube geometry allocated %.2f objects per call", allocs)
	}
}

func TestCubeRotationRecurrence(t *testing.T) {
	cube := Cube3D{angleX: 0.3, angleY: 0.7, angleZ: 1.1, size: 40}
	const dx, dy, dz = 0.023, 0.037, 0.011
	for i := 0; i < 10_000; i++ {
		cube.Rotate(dx, dy, dz)
		wantSinX, wantCosX := math.Sincos(cube.angleX)
		wantSinY, wantCosY := math.Sincos(cube.angleY)
		wantSinZ, wantCosZ := math.Sincos(cube.angleZ)
		if math.Abs(cube.sinX-wantSinX) > 1e-11 || math.Abs(cube.cosX-wantCosX) > 1e-11 ||
			math.Abs(cube.sinY-wantSinY) > 1e-11 || math.Abs(cube.cosY-wantCosY) > 1e-11 ||
			math.Abs(cube.sinZ-wantSinZ) > 1e-11 || math.Abs(cube.cosZ-wantCosZ) > 1e-11 {
			t.Fatalf("rotation recurrence drift at step %d", i+1)
		}
	}
}

func TestCocoMotionRecurrences(t *testing.T) {
	position := 0.15
	pathSin, pathCos := math.Sincos(position)
	bobSin, bobCos := math.Sincos(position * 2.5)

	var dmaSin, dmaCos, dmaStepSin, dmaStepCos [4]float64
	for i := range dmaSin {
		dmaSin[i], dmaCos[i] = math.Sincos(cocoDMAPhaseOffsets3[i])
		dmaStepSin[i], dmaStepCos[i] = math.Sincos(cocoDMAPhaseDeltas3[i])
	}

	for iteration := 1; iteration <= 10_000; iteration++ {
		position += 0.04
		if iteration&1023 == 0 {
			position = math.Mod(position, 4*math.Pi)
			pathSin, pathCos = math.Sincos(position)
			bobSin, bobCos = math.Sincos(position * 2.5)
		} else {
			pathSin, pathCos = stepSinCosForward(pathSin, pathCos, cocoCubePathSinStep3, cocoCubePathCosStep3)
			bobSin, bobCos = stepSinCosForward(bobSin, bobCos, cocoCubeBobSinStep3, cocoCubeBobCosStep3)
		}
		wantPathSin, wantPathCos := math.Sincos(position)
		wantBobSin, wantBobCos := math.Sincos(position * 2.5)
		if math.Abs(pathSin-wantPathSin) > 1e-11 || math.Abs(pathCos-wantPathCos) > 1e-11 ||
			math.Abs(bobSin-wantBobSin) > 1e-11 || math.Abs(bobCos-wantBobCos) > 1e-11 {
			t.Fatalf("cube motion recurrence drift at iteration %d", iteration)
		}

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

func TestAdvanceScroller4WrapsWithoutModulo(t *testing.T) {
	text := []rune("AB")
	if got := advanceScroller4(120, text); got != 124 {
		t.Fatalf("advanceScroller4 before wrap = %g, want 124", got)
	}
	if got := advanceScroller4(124, text); got != 0 {
		t.Fatalf("advanceScroller4 at wrap = %g, want 0", got)
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
	cube := Cube3D{angleX: 0.3, angleY: 0.7, angleZ: 1.1, size: 40}
	vertices := make([]ebiten.Vertex, 0, len(cubeFaces3)*20)
	indices := make([]uint16, 0, len(cubeFaces3)*30)
	b.ReportAllocs()
	for range b.N {
		vertices, indices = cube.appendGeometry(vertices[:0], indices[:0], 400, 300)
	}
}

var cubeTrigSink float64

func BenchmarkCubeRotationRecurrence(b *testing.B) {
	var cubes [nbCubes3]Cube3D
	for i := range cubes {
		cubes[i] = Cube3D{
			angleX: float64(i) * 0.3,
			angleY: float64(i) * 0.2,
			angleZ: float64(i) * 0.1,
		}
	}
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		for i := range cubes {
			dx := 0.02 * (1 + float64(i)*0.1)
			dy := 0.03 * (1 + float64(i)*0.15)
			dz := 0.01 * (1 + float64(i)*0.05)
			cubes[i].Rotate(dx, dy, dz)
			cubeTrigSink = cubes[i].sinX + cubes[i].cosX + cubes[i].sinY + cubes[i].cosY + cubes[i].sinZ + cubes[i].cosZ
		}
	}
}

func BenchmarkCubeRotationDirectTrig(b *testing.B) {
	var cubes [nbCubes3]Cube3D
	for i := range cubes {
		cubes[i] = Cube3D{
			angleX: float64(i) * 0.3,
			angleY: float64(i) * 0.2,
			angleZ: float64(i) * 0.1,
		}
	}
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		for i := range cubes {
			cubes[i].angleX += 0.02 * (1 + float64(i)*0.1)
			cubes[i].angleY += 0.03 * (1 + float64(i)*0.15)
			cubes[i].angleZ += 0.01 * (1 + float64(i)*0.05)
			sx, cx := math.Sincos(cubes[i].angleX)
			sy, cy := math.Sincos(cubes[i].angleY)
			sz, cz := math.Sincos(cubes[i].angleZ)
			cubeTrigSink = sx + cx + sy + cy + sz + cz
		}
	}
}
