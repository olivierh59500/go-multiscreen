package multiscreen

import (
	"testing"

	"github.com/hajimehoshi/ebiten/v2"
	"github.com/olivierh59500/democonstructionkit/effects"
	"github.com/olivierh59500/democonstructionkit/geometry"
	"github.com/olivierh59500/democonstructionkit/presets"
)

func TestSharedCubeMatchesOriginalRecurrenceAndGeometry(t *testing.T) {
	for instance := 0; instance < 12; instance++ {
		cube, err := effects.NewSolidCube(presets.MultiscreenCocoCube(40))
		if err != nil {
			t.Fatal(err)
		}
		defer cube.Close()
		old := legacyMultiCube{size: 40, angleX: float64(instance) * .3, angleY: float64(instance) * .2, angleZ: float64(instance) * .1}
		cube.Rotation = geometry.Vec3{X: old.angleX, Y: old.angleY, Z: old.angleZ}
		wantV := make([]ebiten.Vertex, 0, 120)
		wantI := make([]uint16, 0, 180)
		for frame := 0; frame < 2100; frame++ {
			wantV, wantI = old.appendGeometry(wantV[:0], wantI[:0], 400, 300)
			gotV, gotI := cube.Geometry(400, 300)
			if len(gotV) != len(wantV) || len(gotI) != len(wantI) {
				t.Fatalf("instance %d frame %d count mismatch", instance, frame)
			}
			for i := range gotV {
				if gotV[i] != wantV[i] {
					t.Fatalf("instance %d frame %d vertex %d got=%+v want=%+v", instance, frame, i, gotV[i], wantV[i])
				}
			}
			for i := range gotI {
				if gotI[i] != wantI[i] {
					t.Fatalf("index %d changed", i)
				}
			}
			dx, dy, dz := .02*(1+float64(instance)*.1), .03*(1+float64(instance)*.15), .01*(1+float64(instance)*.05)
			old.Rotate(dx, dy, dz)
			cube.Rotate(dx, dy, dz)
		}
	}
}
