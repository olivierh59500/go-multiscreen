package multiscreen

import (
	"github.com/olivierh59500/democonstructionkit/scrolling"
	"math"
	"slices"
	"testing"
)

// This oracle retains the original stream arithmetic independently of DCK.
type sliceReference struct {
	msgIndex, sliceCount, scrollHead, pauseTime int
	pause                                       bool
	rotSpeed, scrollerRotation                  float64
	scrollChars                                 [240]scrolling.DNASlice
	sineOffsets                                 [240]float64
}

func (d *sliceReference) scrollMessage(speed int) {
	for i := 0; i < speed; i++ {
		ch := scrollMessage[d.msgIndex]
		isCtrl := ch == '^' || ch == '#' || ch == '&' || ch == '%'

		if isCtrl && d.sliceCount == 0 {
			switch ch {
			case '^':
				d.pause = true
				d.pauseTime = 275
				d.rotSpeed = -1
			case '&':
				d.pause = true
				d.pauseTime = 275
				d.rotSpeed = 1
			case '#':
				d.pause = true
				d.pauseTime = 250
				d.rotSpeed = -1
			case '%':
				d.pause = true
				d.pauseTime = 225
				d.rotSpeed = -1
			}
			d.msgIndex++
			if d.msgIndex >= len(scrollMessage) {
				d.msgIndex = 90
			}
		} else {
			d.shiftLeft()
			d.addSliceOfChar(ch, d.sliceCount)

			d.sliceCount++
			if d.sliceCount > 7 {
				d.sliceCount = 0
				d.msgIndex++
				if d.msgIndex >= len(scrollMessage) {
					d.msgIndex = 90
				}
			}
		}
	}
}

func (d *sliceReference) shiftLeft() {
	d.scrollHead++
	if d.scrollHead == len(d.scrollChars) {
		d.scrollHead = 0
	}
}

func (d *sliceReference) addSliceOfChar(ch byte, slice int) {
	previous := d.scrollHead + len(d.scrollChars) - 2
	if previous >= len(d.scrollChars) {
		previous -= len(d.scrollChars)
	}
	tail := d.scrollHead + len(d.scrollChars) - 1
	if tail >= len(d.scrollChars) {
		tail -= len(d.scrollChars)
	}
	f := d.scrollChars[previous].Frame
	glyph, ok := charToFontIndexPhe(rune(ch))
	if !ok {
		glyph = 0
	}

	d.scrollChars[tail] = scrolling.DNASlice{
		Glyph: glyph,
		Frame: f,
		Slice: slice,
	}
}

func (d *sliceReference) renderNextFrames(speed float64) {
	d.scrollerRotation += speed
	if d.scrollerRotation >= 30 {
		d.scrollerRotation -= 30
	}
	if d.scrollerRotation < 0 {
		d.scrollerRotation += 30
	}

	for i := range d.scrollChars {
		index := d.scrollHead + i
		if index >= len(d.scrollChars) {
			index -= len(d.scrollChars)
		}
		newFrame := d.scrollerRotation + d.sineOffsets[i]
		if newFrame >= 30 {
			newFrame -= 30
		} else if newFrame < 0 {
			newFrame += 30
		}

		d.scrollChars[index].Frame = int(newFrame)
	}
}

func TestSharedSliceTransportMatchesOriginalThroughControlsAndLoops(t *testing.T) {
	scene := NewPhenomenaDemo()
	reference := &sliceReference{rotSpeed: .35, pauseTime: 250}
	for i := range reference.sineOffsets {
		reference.sineOffsets[i] = math.Sin(float64(i)*.05) * 15
	}
	count := len(scrollMessage)*8*3 + 2000
	for tick := 0; tick < count; tick++ {
		if !scene.pause {
			scene.scrollMessage(1)
		} else {
			scene.pauseTime--
			if scene.pauseTime == 0 {
				scene.pause = false
				scene.rotSpeed = .35
			}
		}
		if !reference.pause {
			reference.scrollMessage(1)
		} else {
			reference.pauseTime--
			if reference.pauseTime == 0 {
				reference.pause = false
				reference.rotSpeed = .35
			}
		}
		scene.renderNextFrames(scene.rotSpeed)
		reference.renderNextFrames(reference.rotSpeed)
		token, strip := scene.sliceStream.Cursor()
		if token != reference.msgIndex || strip != reference.sliceCount || scene.sliceStream.Head() != reference.scrollHead || scene.pause != reference.pause || scene.pauseTime != reference.pauseTime || scene.rotSpeed != reference.rotSpeed || scene.scrollerRotation != reference.scrollerRotation {
			t.Fatalf("transport or cue changed at tick %d", tick)
		}
		if tick%97 == 0 || scene.pause {
			if !slices.Equal(scene.sliceStream.Slices(), reference.scrollChars[:]) {
				t.Fatalf("strip history changed at tick %d", tick)
			}
		}
	}
}
