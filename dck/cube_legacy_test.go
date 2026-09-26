package multiscreen

import (
	"github.com/hajimehoshi/ebiten/v2"
	"image/color"
	"math"
)

func stepSinCosForward(sinValue, cosValue, sinStep, cosStep float64) (float64, float64) {
	return sinValue*cosStep + cosValue*sinStep, cosValue*cosStep - sinValue*sinStep
}

// Frozen pre-extraction renderer, retained only as an independent fidelity oracle.
type legacyMultiCube struct {
	angleX float64
	angleY float64
	angleZ float64
	size   float64

	sinX float64
	cosX float64
	sinY float64
	cosY float64
	sinZ float64
	cosZ float64

	stepDX   float64
	stepDY   float64
	stepDZ   float64
	stepSinX float64
	stepCosX float64
	stepSinY float64
	stepCosY float64
	stepSinZ float64
	stepCosZ float64

	rotationCount uint16
	trigReady     bool
	stepReady     bool
}

func (c *legacyMultiCube) Rotate(dx, dy, dz float64) {
	c.ensureTrig()
	if !c.stepReady || dx != c.stepDX || dy != c.stepDY || dz != c.stepDZ {
		c.stepDX, c.stepDY, c.stepDZ = dx, dy, dz
		c.stepSinX, c.stepCosX = math.Sincos(dx)
		c.stepSinY, c.stepCosY = math.Sincos(dy)
		c.stepSinZ, c.stepCosZ = math.Sincos(dz)
		c.stepReady = true
	}

	c.angleX += dx
	c.angleY += dy
	c.angleZ += dz
	c.sinX, c.cosX = stepSinCosForward(c.sinX, c.cosX, c.stepSinX, c.stepCosX)
	c.sinY, c.cosY = stepSinCosForward(c.sinY, c.cosY, c.stepSinY, c.stepCosY)
	c.sinZ, c.cosZ = stepSinCosForward(c.sinZ, c.cosZ, c.stepSinZ, c.stepCosZ)

	c.rotationCount++
	if c.rotationCount == 1024 {
		c.angleX = math.Mod(c.angleX, 2*math.Pi)
		c.angleY = math.Mod(c.angleY, 2*math.Pi)
		c.angleZ = math.Mod(c.angleZ, 2*math.Pi)
		c.rotationCount = 0
		c.trigReady = false
		c.ensureTrig()
	}
}

func (c *legacyMultiCube) ensureTrig() {
	if c.trigReady {
		return
	}
	c.sinX, c.cosX = math.Sincos(c.angleX)
	c.sinY, c.cosY = math.Sincos(c.angleY)
	c.sinZ, c.cosZ = math.Sincos(c.angleZ)
	c.trigReady = true
}

type faceDepth3 struct {
	index int
	depth float64
}

var cubeCorners3 = [8][3]float64{
	{-1, -1, -1}, {1, -1, -1}, {1, 1, -1}, {-1, 1, -1},
	{-1, -1, 1}, {1, -1, 1}, {1, 1, 1}, {-1, 1, 1},
}

var cubeFaces3 = [6][4]int{
	{0, 1, 2, 3}, {4, 5, 6, 7}, {0, 1, 5, 4},
	{2, 3, 7, 6}, {0, 3, 7, 4}, {1, 2, 6, 5},
}

var cubeFaceColors3 = [6]color.RGBA{
	{255, 140, 0, 255}, {255, 165, 50, 255}, {255, 180, 80, 255},
	{255, 120, 0, 255}, {255, 150, 30, 255}, {255, 200, 100, 255},
}

func (c *legacyMultiCube) appendGeometry(vertices []ebiten.Vertex, indices []uint16, centerX, centerY float64) ([]ebiten.Vertex, []uint16) {
	c.ensureTrig()
	sinX, cosX := c.sinX, c.cosX
	sinY, cosY := c.sinY, c.cosY
	sinZ, cosZ := c.sinZ, c.cosZ
	half := c.size / 2

	var rotated [8][3]float64
	var projected [8][2]float32
	for i, corner := range cubeCorners3 {
		x, y, z := corner[0]*half, corner[1]*half, corner[2]*half

		y1 := y*cosX - z*sinX
		z1 := y*sinX + z*cosX
		y, z = y1, z1

		x1 := x*cosY + z*sinY
		z2 := -x*sinY + z*cosY
		x, z = x1, z2

		x2 := x*cosZ - y*sinZ
		y2 := x*sinZ + y*cosZ
		x, y = x2, y2

		rotated[i] = [3]float64{x, y, z}
		x2d, y2d := project3D3(x, y, z)
		projected[i] = [2]float32{float32(centerX + x2d), float32(centerY + y2d)}
	}

	var depths [6]faceDepth3
	for i, face := range cubeFaces3 {
		centerZ := 0.0
		for _, vi := range face {
			centerZ += rotated[vi][2]
		}
		depths[i] = faceDepth3{i, centerZ / 4}
	}

	for i := 1; i < len(depths); i++ {
		item := depths[i]
		j := i
		for j > 0 && depths[j-1].depth > item.depth {
			depths[j] = depths[j-1]
			j--
		}
		depths[j] = item
	}

	for _, fd := range depths {
		face := cubeFaces3[fd.index]
		faceColor := cubeFaceColors3[fd.index]
		points := [4][2]float32{
			projected[face[0]], projected[face[1]], projected[face[2]], projected[face[3]],
		}

		vertices, indices = appendSolidQuad3(vertices, indices, points, faceColor)

		edgeColor := color.RGBA{
			uint8(faceColor.R * 3 / 4),
			uint8(faceColor.G * 3 / 4),
			uint8(faceColor.B * 3 / 4),
			255,
		}
		for i := 0; i < 4; i++ {
			j := (i + 1) % 4
			vertices, indices = appendSolidLine3(vertices, indices, points[i], points[j], 1, edgeColor)
		}
	}
	return vertices, indices
}

func project3D3(x, y, z float64) (float64, float64) {
	perspective := 200.0
	factor := perspective / (perspective + z)
	return x * factor, y * factor
}

func solidVertex3(point [2]float32, clr color.RGBA) ebiten.Vertex {
	const inv255 = 1.0 / 255.0
	return ebiten.Vertex{
		DstX: point[0], DstY: point[1], SrcX: 1, SrcY: 1,
		ColorR: float32(clr.R) * inv255, ColorG: float32(clr.G) * inv255,
		ColorB: float32(clr.B) * inv255, ColorA: float32(clr.A) * inv255,
	}
}

func appendSolidQuad3(vertices []ebiten.Vertex, indices []uint16, points [4][2]float32, clr color.RGBA) ([]ebiten.Vertex, []uint16) {
	base := uint16(len(vertices))
	for _, point := range points {
		vertices = append(vertices, solidVertex3(point, clr))
	}
	indices = append(indices, base, base+1, base+2, base, base+2, base+3)
	return vertices, indices
}

func appendSolidLine3(vertices []ebiten.Vertex, indices []uint16, start, end [2]float32, width float32, clr color.RGBA) ([]ebiten.Vertex, []uint16) {
	dx := end[0] - start[0]
	dy := end[1] - start[1]
	length := float32(math.Sqrt(float64(dx*dx + dy*dy)))
	if length == 0 {
		return vertices, indices
	}
	halfWidth := width / (2 * length)
	ox, oy := -dy*halfWidth, dx*halfWidth
	points := [4][2]float32{
		{start[0] + ox, start[1] + oy},
		{end[0] + ox, end[1] + oy},
		{end[0] - ox, end[1] - oy},
		{start[0] - ox, start[1] - oy},
	}
	return appendSolidQuad3(vertices, indices, points, clr)
}
