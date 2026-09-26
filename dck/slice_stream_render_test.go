//go:build dck_slice_rendercheck

package multiscreen

import (
	"bytes"
	"fmt"
	"github.com/hajimehoshi/ebiten/v2"
	capture "github.com/olivierh59500/democonstructionkit/fidelity/ebiten"
	"github.com/olivierh59500/democonstructionkit/render"
	"github.com/olivierh59500/democonstructionkit/scrolling"
	"math"
	"os"
	"testing"
)

var dnaCheckFrames = []int{0, 1, 256, 1024, 2048, 4096, 8192, 16384, 24000, 32000, 48000}

type dnaRenderCheck struct {
	scene            *PhenomenaDemo
	reference        *sliceReference
	frame, checked   int
	referenceTime    float64
	actual, expected *ebiten.Image
	a, b             []byte
	err              error
}

func (c *dnaRenderCheck) Layout(int, int) (int, int) { return 800, 600 }
func (c *dnaRenderCheck) Update() error {
	if c.err != nil {
		return c.err
	}
	if !c.reference.pause {
		c.reference.scrollMessage(1)
	} else {
		c.reference.pauseTime--
		if c.reference.pauseTime == 0 {
			c.reference.pause = false
			c.reference.rotSpeed = .35
		}
	}
	if err := c.scene.sliceProgram.Step(); err != nil {
		return err
	}
	c.reference.renderNextFrames(c.reference.rotSpeed)
	c.referenceTime += .30
	if err := c.scene.rowWave.AdvanceFrame(); err != nil {
		return err
	}
	c.frame++
	return nil
}
func (c *dnaRenderCheck) Draw(dst *ebiten.Image) {
	if c.err != nil || c.checked >= len(dnaCheckFrames) || c.frame != dnaCheckFrames[c.checked] {
		return
	}
	c.actual.Clear()
	c.expected.Clear()
	if c.scene.rowWave.FrameTime() != c.referenceTime {
		c.err = fmt.Errorf("DNA row-wave clock changed at frame %d", c.frame)
		return
	}
	c.scene.drawScroller(c.actual)
	t2 := c.referenceTime
	ws, wc := math.Sincos(5*10.50 + c.referenceTime/6)
	c.scene.dnaFrames.DrawSlices(c.expected, c.reference.scrollChars[:], c.reference.scrollHead, scrolling.DNADrawConfig{SliceWidth: 2, ScaleX: 1.67, ScaleY: 1.875, OriginY: 195, Y: func(i int) float64 {
		y := 80.
		if t2 > 5*50-float64(i)*.0033 {
			y = 80 * wc
		}
		t2 += 1. / 6.
		ws, wc = ws*phenomenaWaveCosStep+wc*phenomenaWaveSinStep, wc*phenomenaWaveCosStep-ws*phenomenaWaveSinStep
		return 67 + y
	}})
	c.actual.ReadPixels(c.a)
	c.expected.ReadPixels(c.b)
	if !bytes.Equal(c.a, c.b) {
		c.err = fmt.Errorf("DNA rendering changed at frame %d", c.frame)
	}
	dst.DrawImage(c.actual, nil)
	c.checked++
}
func TestMain(m *testing.M) {
	if code := m.Run(); code != 0 {
		os.Exit(code)
	}
	dir, err := os.MkdirTemp("", "dna-stream-")
	if err != nil {
		panic(err)
	}
	var check *dnaRenderCheck
	err = capture.Run(capture.Config{Directory: dir, Frames: dnaCheckFrames, Width: 800, Height: 600}, func() (ebiten.Game, error) {
		scene := NewPhenomenaDemo()
		if err := scene.Init(); err != nil {
			return nil, err
		}
		reference := &sliceReference{rotSpeed: .35, pauseTime: 250}
		for i := range reference.sineOffsets {
			reference.sineOffsets[i] = math.Sin(float64(i)*.05) * 15
		}
		for i := 0; i < 320; i++ {
			reference.scrollMessage(1)
			reference.renderNextFrames(reference.rotSpeed)
		}
		check = &dnaRenderCheck{scene: scene, reference: reference, actual: render.NewSurface(800, 600), expected: render.NewSurface(800, 600), a: make([]byte, 1920000), b: make([]byte, 1920000)}
		return check, nil
	})
	if err == nil && check != nil {
		err = check.err
		if err == nil && check.checked != len(dnaCheckFrames) {
			err = fmt.Errorf("only %d DNA captures checked", check.checked)
		}
	}
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	fmt.Printf("All %d DNA captures match exactly: %s\n", check.checked, dir)
}
