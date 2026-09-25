package multiscreen

import (
	"bytes"
	"fmt"
	kit "github.com/olivierh59500/democonstructionkit"
	"image"
	"image/color"
	originalassets "multiscreen-mega-demo"

	"github.com/olivierh59500/democonstructionkit/composite"
	"github.com/olivierh59500/democonstructionkit/effects"
	"github.com/olivierh59500/democonstructionkit/geometry"
	"github.com/olivierh59500/democonstructionkit/motion"
	"github.com/olivierh59500/democonstructionkit/presets"
	"github.com/olivierh59500/democonstructionkit/scrolling"
	"github.com/olivierh59500/democonstructionkit/sound"
	"github.com/olivierh59500/democonstructionkit/sprites"

	_ "image/png"
	"log"
	"math"

	"github.com/hajimehoshi/ebiten/v2"
	"github.com/hajimehoshi/ebiten/v2/vector"

	audio "github.com/olivierh59500/democonstructionkit/sound/output"
)

const (
	demoWidth    = 800
	demoHeight   = 600
	screenWidth  = 1600
	screenHeight = 1200
	sampleRate   = 44100

	viewDuration       = 7.0 // seconds per demo
	transitionDuration = 4.0 // seconds for transitions
)

const compositeShaderSource = `//kage:unit pixels

package main

var CameraCenter vec2
var CameraZoom float

func Fragment(dstPos vec4, srcPos vec2, color vec4) vec4 {
	screenPos := srcPos - imageSrc0Origin()
	worldPos := (screenPos - vec2(400, 300)) / CameraZoom + CameraCenter
	if worldPos.x < 0 || worldPos.y < 0 || worldPos.x >= 1600 || worldPos.y >= 1200 {
		return vec4(0, 0, 0, 1)
	}

	sourceOrigin := imageSrc0Origin()
	if worldPos.y < 600 {
		if worldPos.x < 800 {
			return imageSrc0UnsafeAt(sourceOrigin + worldPos)
		}
		return imageSrc1UnsafeAt(sourceOrigin + worldPos - vec2(800, 0))
	}

	if worldPos.x < 800 {
		return imageSrc3UnsafeAt(sourceOrigin + worldPos - vec2(0, 600))
	}
	return imageSrc2UnsafeAt(sourceOrigin + worldPos - vec2(800, 600))
}
`

var musicData = originalassets.

	// Camera states
	DCKAssetMusicData()

type CameraState int

const (
	StateDemo1 CameraState = iota
	StateTransition1to2
	StateDemo2
	StateTransition2to3
	StateDemo3
	StateTransition3to4
	StateDemo4
	StateTransition4toZoom
	StateZoomOut
	StateLoop
)

// Cubic ease in-out
func easeInOutCubic(t float64) float64 {
	if t < 0.5 {
		return 4 * t * t * t
	}
	u := -2*t + 2
	return 1 - u*u*u/2
}

// ==================== PHENOMENA DEMO (Demo1) ====================

var demo1RasterbarData = originalassets.
	DCKAssetDemo1RasterbarData()

var demo1FontData = originalassets.
	DCKAssetDemo1FontData()

var demo1LogoData = originalassets.
	DCKAssetDemo1LogoData()

var demo1PhotonData = originalassets.DCKAssetDemo1PhotonData()

type PhenomenaDemo struct {
	fontAtlas *scrolling.Atlas
	dnaFrames *scrolling.DNAFrames
	// Demo state
	state       int
	initialized bool

	// Images
	imgRasterbar  *ebiten.Image
	imgFont       *ebiten.Image
	imgLogo       *ebiten.Image
	imgPhoton     *ebiten.Image
	imgPhotonMask *ebiten.Image
	imgTextPage1  *ebiten.Image
	imgTextPage2  *ebiten.Image

	// Animation canvases
	cnvFrames     *ebiten.Image
	rasterGrad640 *ebiten.Image
	rasterGrad800 *ebiten.Image

	// Animation variables
	t                float64
	pause            bool
	pauseTime        int
	scrollSpeed      int
	rotSpeed         float64
	color            float64
	percent          float64
	blackRectWidth   float64
	blackRectShow    bool
	photonY          float64
	photonGravity    float64
	photonBounce     float64
	rasterbarY       float64
	direction        float64
	scrollerRotation float64

	// Scroller data
	sliceStream *scrolling.SliceStream
	sineOffsets [240]float64
}

type GradientStop struct {
	Color  color.RGBA
	Offset float64
}

var (
	gdcRasterBar = []GradientStop{
		{color.RGBA{0x44, 0x00, 0x44, 0xFF}, 0.0},
		{color.RGBA{0xFF, 0xDD, 0xFF, 0xFF}, 0.5},
		{color.RGBA{0x11, 0x11, 0x44, 0xFF}, 1.0},
	}
	gdcRedBar = []GradientStop{
		{color.RGBA{0x00, 0x00, 0x00, 0xFF}, 0.0},
		{color.RGBA{0xFF, 0x33, 0x00, 0xFF}, 0.5},
		{color.RGBA{0x00, 0x00, 0x00, 0xFF}, 1.0},
	}
	gdcSilverBar = []GradientStop{
		{color.RGBA{0x55, 0x55, 0x55, 0xFF}, 0.0},
		{color.RGBA{0xFF, 0xFF, 0xFF, 0xFF}, 0.5},
		{color.RGBA{0x55, 0x55, 0x55, 0xFF}, 1.0},
	}
	gdcPurpleBar = []GradientStop{
		{color.RGBA{0x34, 0x22, 0x55, 0xFF}, 0.0},
		{color.RGBA{0x60, 0x4E, 0x98, 0xFF}, 0.5},
		{color.RGBA{0x34, 0x22, 0x55, 0xFF}, 1.0},
	}
)

const charsetPhenomena = presets.PhenomenaAlphabet

var phenomenaWaveSinStep, phenomenaWaveCosStep = math.Sincos(1.0 / 36.0)

const scrollMessage = `           THIS IS IMPOSSIBLE!            WHAT IS?               THIS IS!!!                    ...SO, ANOTHER DEMO FROM PHENOMENA HAS REACHED YOU...    THIS TIME WITH CODING BY                PHOTON!                ^  RASTA MUSIC BY                    FIREFOX!                &    AND SUPER GFX BY                       TERMINATOR               #  ...SO, SLAYER! HOW DO YOU LIKE @MY@ SCROLLER?  IT'S MUCH IMPOSSIBLER THAN YOURS!    ...   SO DE SO!          DOES ANYONE HAVE A PROGRAM CALLED 'PAGE RENDER 3D'? THEN CONTACT OUR NEW GFX ARTIST AT          0492-41027               % AND ASK FOR MIKAEL. NEWS NEWS NEWS NEWS   !!! LOOK OUT FOR PHENOMENA'S NEW DISK MAG CALLED ' TRANSMISSION ' ! ! ! ! IT'S A MAG ESPECIALLY MADE FOR ALL YOU CODERS OUT THERE, COMPLETE WITH CODER / DEMO / CRACK TOP-TEN,ARTICLES ABOUT CODING / CRACKING, AND SOURCES, ETC,ETC...         HERE'S MY TOP-FIVE DEMO GROUPS 1. SCOOPEX  -SLAYER IS WORKING HARD AND HIS M.H. DEMO IS STILL UNBEATEN-  ...  2. CRYPTOBURNERS  -NICE MD 2 BUT SLOOOW VECTORS-  ... 3. RSI/PARADOX  -NICE DEMOS LATELY, EXCEPT FOR THE 'FOLLOW ME' CRAP-  ...  4. KEFRENS  -ALL YOUR LATEST DEMOS HAVE BEEN PROFESSIONAL!-  ...  5. THE LINK  -ALWAYS COOL IDEAS,GIVE US SOME MORE-  ...  OF COURSE, PHENOMENA IS EXCLUDED FROM THIS LIST...        NOW OVER TO SOME INTERNAL GREETS...  @     BIG 2A-FINISH YOUR DEMO AND BUY AN A500!   @   CORE-GET YOUR HANDS ON A WORKING AMIGA!   @   DANKO-GET BUSY!   @   KLUTTAS O SPIRIT-WAKE UP FROM YOUR COMA!!!!   @   RAVE-SAME TO YOU!       ...     AND NOW, TIME FOR SOME OTHER GREETS... THEY GO TO --- CONAN/TPL-MAKE A GOOD DEMO AND JOIN ANOTHER GROUP!   @   KALLE BALLE/TSL - EVER THOUGHT ABOUT CHANGING YOUR NAME????   @   HAVOK/ECSTASY-JOIN US! I'M JUST A PHONECALL AWAY - 0381-11344 @   MAHONEY/NS-TRY TAKING SOME IDEAS FROM NT 1.2!  @   UNCLE TOM/RAZOR-STOP DRAWING AND DO SOME MUSIC @   SLAYER/SCX-AND ALL OTHER GOOD CODERS-CALL ME FOR SOME COOL TECH-TALK    0381-11344   ZEUS/ADEPT-GOOD LUCK AND CODE HARD!       ---     NOW I DON'T HAVE VERY MUCH ELSE TO SAY, EXCEPT....                    BYE!             @@@@@@@@@@@@@                `

const (
	StateTextPage1Phe = iota
	StateTextPage2Phe
	StateShowLogoPhe
	StateShowUpperRasterbarPhe
	StateShowLowerRasterbarPhe
	StateDropPhotonPhe
	StatePhotonFadeToRedPhe
	StateMainDemoPhe
)

func NewPhenomenaDemo() *PhenomenaDemo {
	d := &PhenomenaDemo{
		state:            StateMainDemoPhe, // Start directly at main demo
		pauseTime:        250,
		rotSpeed:         0.35,
		scrollSpeed:      1,
		blackRectWidth:   800,
		blackRectShow:    false, // No black rect at start
		photonY:          184,
		photonBounce:     -9.50,
		rasterbarY:       -40,
		direction:        1,
		scrollerRotation: 0,
	}

	for i := range d.sineOffsets {
		d.sineOffsets[i] = math.Sin(float64(i)*0.05) * 15
	}

	d.initSliceStream()
	return d
}

func (d *PhenomenaDemo) Init() error {
	if d.initialized {
		return nil
	}

	var (
		err error
		img image.Image
	)

	// Load images
	if d.state != StateMainDemoPhe {
		img, _, err = image.Decode(bytes.NewReader(demo1RasterbarData))
		if err != nil {
			return err
		}
		d.imgRasterbar = ebiten.NewImageFromImage(img)
	}

	img, _, err = image.Decode(bytes.NewReader(demo1FontData))
	if err != nil {
		return err
	}
	d.imgFont = ebiten.NewImageFromImage(img)
	d.fontAtlas, err = presets.FontAtlas("multiscreen-phenomena", d.imgFont)
	if err != nil {
		return err
	}

	img, _, err = image.Decode(bytes.NewReader(demo1LogoData))
	if err != nil {
		return err
	}
	d.imgLogo = ebiten.NewImageFromImage(img)

	img, _, err = image.Decode(bytes.NewReader(demo1PhotonData))
	if err != nil {
		return err
	}
	d.imgPhoton = ebiten.NewImageFromImage(img)
	d.imgPhotonMask = newWhiteAlphaMask(img)
	d.rasterGrad800 = createGradient(800, 12, gdcRasterBar)

	if d.state != StateMainDemoPhe {
		d.rasterGrad640 = createGradient(640, 12, gdcRasterBar)
		d.initTextPages()
	}

	// Init character frames
	d.initCharacterFrames()

	// Bring message to start
	for i := 0; i < 320; i++ {
		d.scrollMessage(1)
		d.renderNextFrames(d.rotSpeed)
	}

	d.initialized = true
	return nil
}

func (d *PhenomenaDemo) initTextPages() {
	texts1 := []struct {
		Y    int
		Text string
	}{
		{18, "   FOR HOT VHS"},
		{75, "  AND SOFTWARE"},
		{133, "SWAPPING, CONTACT"},
		{219, " THE PUNISHER "},
		{291, "      AT..."},
	}
	d.imgTextPage1 = d.makeIntroText("xor", color.Black, texts1)

	texts2 := []struct {
		Y    int
		Text string
	}{
		{78, "    PHENOMENA"},
		{158, "   SKALDEV. 69"},
		{238, "  16142 BROMMA"},
		{334, "     SWEDEN!"},
	}
	d.imgTextPage2 = d.makeIntroText("source-over", nil, texts2)
}

func (d *PhenomenaDemo) makeIntroText(mode string, backColor color.Color, texts []struct {
	Y    int
	Text string
}) *ebiten.Image {
	img := ebiten.NewImage(640, 480)

	if backColor != nil {
		img.Fill(backColor)
	}

	for _, t := range texts {
		x := 48
		for _, ch := range t.Text {
			subImg, _, found := d.fontAtlas.Glyph(ch)

			if found {
				op := &ebiten.DrawImageOptions{}
				op.GeoM.Scale(2, 2)
				op.GeoM.Translate(float64(x), float64(t.Y))

				if mode == "xor" {
					op.ColorM.Scale(-1, -1, -1, 1)
					op.ColorM.Translate(1, 1, 1, 0)
				}

				img.DrawImage(subImg, op)
			}
			x += 32
		}
	}

	return img
}

var charToFontIndexPhe = func() func(rune) (int, bool) {
	lookup, err := presets.TileLookup("multiscreen-phenomena", false)
	if err != nil {
		panic(err)
	}
	return lookup
}()

func createGradient(width, height int, stops []GradientStop) *ebiten.Image {
	img := image.NewRGBA(image.Rect(0, 0, width, height))

	for y := 0; y < height; y++ {
		t := float64(y) / float64(height-1)

		var c color.RGBA
		for i := 0; i < len(stops)-1; i++ {
			if t >= stops[i].Offset && t <= stops[i+1].Offset {
				localT := (t - stops[i].Offset) / (stops[i+1].Offset - stops[i].Offset)
				c = lerpColor(stops[i].Color, stops[i+1].Color, localT)
				break
			}
		}

		row := img.Pix[y*img.Stride : y*img.Stride+width*4]
		for x := 0; x < len(row); x += 4 {
			row[x] = c.R
			row[x+1] = c.G
			row[x+2] = c.B
			row[x+3] = c.A
		}
	}

	return ebiten.NewImageFromImage(img)
}

// newWhiteAlphaMask preserves a source image's silhouette while making every
// visible pixel white. Tinting this with ColorScale is equivalent to replacing
// RGB with a flat colour through ColorM, but it keeps Ebitengine's standard
// shader and therefore remains batchable with adjacent sprites.
func newWhiteAlphaMask(source image.Image) *ebiten.Image {
	bounds := source.Bounds()
	mask := image.NewNRGBA(image.Rect(0, 0, bounds.Dx(), bounds.Dy()))
	for y := bounds.Min.Y; y < bounds.Max.Y; y++ {
		row := mask.Pix[(y-bounds.Min.Y)*mask.Stride:]
		for x := bounds.Min.X; x < bounds.Max.X; x++ {
			_, _, _, alpha := source.At(x, y).RGBA()
			offset := (x - bounds.Min.X) * 4
			row[offset] = 0xff
			row[offset+1] = 0xff
			row[offset+2] = 0xff
			row[offset+3] = uint8(alpha >> 8)
		}
	}
	return ebiten.NewImageFromImage(mask)
}

func lerpColor(c1, c2 color.RGBA, t float64) color.RGBA {
	r := uint8(float64(c1.R)*(1-t) + float64(c2.R)*t)
	g := uint8(float64(c1.G)*(1-t) + float64(c2.G)*t)
	b := uint8(float64(c1.B)*(1-t) + float64(c2.B)*t)
	a := uint8(float64(c1.A)*(1-t) + float64(c2.A)*t)

	return color.RGBA{r, g, b, a}
}

func (d *PhenomenaDemo) initCharacterFrames() {
	core, front, back := createGradient(480, 9, gdcRedBar), createGradient(480, 33, gdcSilverBar), createGradient(480, 33, gdcPurpleBar)
	defer core.Deallocate()
	defer front.Deallocate()
	defer back.Deallocate()
	glyphs, err := scrolling.GridImages(d.imgFont, image.Pt(16, 26), len(charsetPhenomena), len(charsetPhenomena))
	if err != nil {
		panic(err)
	}
	d.dnaFrames, err = scrolling.NewDNAFrames(glyphs, scrolling.DNAFrameConfig{Frames: 30, Height: 33, Step: 2.25, Front: front, Back: back, Core: core, CoreY: 12})
	if err != nil {
		panic(err)
	}
	d.cnvFrames = d.dnaFrames.Image
}

func (d *PhenomenaDemo) initSliceStream() {
	tokens := make([]scrolling.SliceToken, 0, len(scrollMessage))
	for _, ch := range scrollMessage {
		if ch == '^' || ch == '#' || ch == '&' || ch == '%' {
			tokens = append(tokens, scrolling.SliceToken{Control: string(ch)})
			continue
		}
		glyph, ok := charToFontIndexPhe(ch)
		if !ok {
			glyph = 0
		}
		tokens = append(tokens, scrolling.SliceToken{Glyph: glyph, Width: 16})
	}
	var err error
	d.sliceStream, err = scrolling.NewSliceStream(scrolling.SliceStreamConfig{Tokens: tokens, Capacity: len(d.sineOffsets), SliceWidth: 2, Repeat: true, LoopStart: 90})
	if err != nil {
		panic(err)
	}
}

func (d *PhenomenaDemo) scrollMessage(speed int) {
	d.sliceStream.Step(speed, func(event scrolling.SliceControl) bool {
		d.pause = true
		switch event.Name {
		case "^":
			d.pauseTime = 275
			d.rotSpeed = -1
		case "&":
			d.pauseTime = 275
			d.rotSpeed = 1
		case "#":
			d.pauseTime = 250
			d.rotSpeed = -1
		case "%":
			d.pauseTime = 225
			d.rotSpeed = -1
		}
		return false
	})
}

func (d *PhenomenaDemo) renderNextFrames(speed float64) {
	d.scrollerRotation += speed
	if d.scrollerRotation >= 30 {
		d.scrollerRotation -= 30
	}
	if d.scrollerRotation < 0 {
		d.scrollerRotation += 30
	}
	if err := d.sliceStream.SetFrames(d.scrollerRotation, d.sineOffsets[:], 30); err != nil {
		panic(err)
	}
}

func (d *PhenomenaDemo) Update() error {
	if !d.initialized {
		if err := d.Init(); err != nil {
			return err
		}
	}

	switch d.state {
	case StateTextPage1Phe:
		d.rasterbarY += 1.5
		if d.rasterbarY >= 340 {
			d.rasterbarY = 0
			d.percent = 0
			d.state = StateTextPage2Phe
		}

	case StateTextPage2Phe:
		d.percent += d.direction * 1
		if d.percent > 200 {
			d.direction = -1
			d.percent = 100
		}
		if d.percent <= 0 && d.direction == -1 {
			d.percent = 0
			d.state = StateShowLogoPhe
		}

	case StateShowLogoPhe:
		d.percent += 4
		if d.percent >= 200 {
			d.percent = 0
			d.state = StateShowUpperRasterbarPhe
		}

	case StateShowUpperRasterbarPhe:
		d.percent += 4
		if d.percent >= 100 {
			d.percent = 0
			d.state = StateShowLowerRasterbarPhe
		}

	case StateShowLowerRasterbarPhe:
		d.percent += 4
		if d.percent >= 100 {
			d.percent = 0
			d.state = StateDropPhotonPhe
		}

	case StateDropPhotonPhe:
		d.photonGravity += 0.30
		d.photonY += d.photonGravity
		if d.photonY > 445 {
			d.photonGravity = d.photonBounce
			d.photonBounce *= 0.70
		}
		if d.photonBounce >= -0.70 {
			d.percent = 100
			d.state = StatePhotonFadeToRedPhe
		}

	case StatePhotonFadeToRedPhe:
		d.percent -= 4
		if d.percent < 50 {
			d.percent = 0
			d.state = StateMainDemoPhe
		}

	case StateMainDemoPhe:
		d.color += 1.0 / 3.0
		if d.color > 360 {
			d.color = 0
		}

		if !d.pause {
			d.scrollMessage(d.scrollSpeed)
		} else {
			d.pauseTime--
			if d.pauseTime == 0 {
				d.pause = false
				d.rotSpeed = 0.35
				d.scrollSpeed = 1
			}
		}

		d.t += 0.30
		d.renderNextFrames(d.rotSpeed)

		if d.blackRectShow {
			d.blackRectWidth -= 8
			if d.blackRectWidth < 0 {
				d.blackRectWidth = 0
				d.blackRectShow = false
			}
		}
	}

	return nil
}

func (d *PhenomenaDemo) Draw(screen *ebiten.Image) {
	if !d.initialized {
		return
	}

	switch d.state {
	case StateTextPage1Phe:
		screen.Fill(color.Black)
		op := &ebiten.DrawImageOptions{}
		op.GeoM.Translate(0, d.rasterbarY)
		screen.DrawImage(d.imgRasterbar, op)
		screen.DrawImage(d.imgTextPage1, nil)

	case StateTextPage2Phe:
		screen.Fill(color.Black)
		op := &ebiten.DrawImageOptions{}
		brightness := d.percent / 100.0
		if brightness > 1 {
			brightness = 2 - brightness
		}
		op.ColorM.Scale(brightness, brightness, brightness, 1)
		screen.DrawImage(d.imgTextPage2, op)

	case StateShowLogoPhe:
		screen.Fill(color.RGBA{0x00, 0x01, 0x11, 0xFF})

		if d.percent <= 100 {
			op := &ebiten.DrawImageOptions{}
			brightness := d.percent / 100.0
			op.ColorM.Scale(brightness, brightness, brightness, 1)
			screen.DrawImage(d.imgLogo, op)
		} else {
			screen.DrawImage(d.imgLogo, nil)

			op := &ebiten.DrawImageOptions{}
			op.ColorM.Scale(1, 1, 1, 1)
			op.ColorM.Translate(1, 1, 1, 0)
			alpha := (200 - d.percent) / 100.0
			op.ColorM.Scale(1, 1, 1, alpha)
			screen.DrawImage(d.imgLogo, op)
		}

	case StateShowUpperRasterbarPhe, StateShowLowerRasterbarPhe, StateDropPhotonPhe, StatePhotonFadeToRedPhe:
		screen.Fill(color.Black)
		vector.DrawFilledRect(screen, 0, 130, 640, 300, color.RGBA{0x00, 0x01, 0x11, 0xFF}, false)

		screen.DrawImage(d.imgLogo, nil)

		if d.state >= StateShowUpperRasterbarPhe {
			alpha := 1.0
			if d.state == StateShowUpperRasterbarPhe {
				alpha = d.percent / 100.0
			}
			op := &ebiten.DrawImageOptions{}
			op.ColorM.Scale(1, 1, 1, alpha)
			op.GeoM.Translate(0, 129)
			screen.DrawImage(d.rasterGrad640, op)
		}

		if d.state >= StateShowLowerRasterbarPhe {
			alpha := 1.0
			if d.state == StateShowLowerRasterbarPhe {
				alpha = d.percent / 100.0
			}
			op := &ebiten.DrawImageOptions{}
			op.ColorM.Scale(1, 1, 1, alpha)
			op.GeoM.Translate(0, 430)
			screen.DrawImage(d.rasterGrad640, op)
		}

		if d.state >= StateDropPhotonPhe {
			if d.state == StatePhotonFadeToRedPhe {
				op := &ebiten.DrawImageOptions{}
				op.GeoM.Translate(285, 445)

				lightness := d.percent / 100.0
				op.ColorM.Scale(lightness, lightness*0.5, lightness*0.5, 1)

				screen.DrawImage(d.imgPhoton, op)
			} else {
				op := &ebiten.DrawImageOptions{}
				op.GeoM.Translate(285, d.photonY)
				screen.DrawImage(d.imgPhoton, op)
			}
		}

	case StateMainDemoPhe:
		screen.Fill(color.Black)
		// Fill middle section with dark blue background (scaled for 800x600)
		vector.DrawFilledRect(screen, 0, 162, 800, 375, color.RGBA{0x00, 0x01, 0x11, 0xFF}, false)

		// Draw logo centered horizontally (640 wide -> center in 800)
		logoOp := &ebiten.DrawImageOptions{}
		logoOp.GeoM.Translate(80, 0) // Center horizontally: (800-640)/2 = 80
		screen.DrawImage(d.imgLogo, logoOp)

		// Draw upper raster bar (full width)
		op := &ebiten.DrawImageOptions{}
		op.GeoM.Translate(0, 129)
		screen.DrawImage(d.rasterGrad800, op)

		// Draw lower raster bar (full width)
		op.GeoM.Reset()
		op.GeoM.Translate(0, 537)
		screen.DrawImage(d.rasterGrad800, op)

		// Draw photon with color cycling (centered)
		op = &ebiten.DrawImageOptions{}
		hue := d.color / 360.0
		r, g, b := hslToRGB(hue, 1.0, 0.5)
		op.ColorScale.Scale(float32(r), float32(g), float32(b), 1)
		op.GeoM.Translate(365, 555) // Centered: 285 + 80 = 365, bottom adjusted
		screen.DrawImage(d.imgPhotonMask, op)

		// Draw scroller
		d.drawScroller(screen)

		if d.blackRectShow {
			vector.DrawFilledRect(screen, 0, 468, float32(d.blackRectWidth), 69, color.RGBA{0x00, 0x01, 0x11, 0xFF}, false)
		}
	}
}

func (d *PhenomenaDemo) drawScroller(screen *ebiten.Image) {
	t2 := d.t
	ws, wc := math.Sincos(5*10.50 + d.t/6)
	d.dnaFrames.DrawSlices(screen, d.sliceStream.Slices(), d.sliceStream.Head(), scrolling.DNADrawConfig{SliceWidth: 2, ScaleX: 1.67, ScaleY: 1.875, OriginY: 195, Y: func(i int) float64 {
		y := 80.0
		if t2 > 5*50-float64(i)*.0033 {
			y = 80 * wc
		}
		t2 += 1.0 / 6.0
		ws, wc = ws*phenomenaWaveCosStep+wc*phenomenaWaveSinStep, wc*phenomenaWaveCosStep-ws*phenomenaWaveSinStep
		return 67 + y
	}})
}

func hslToRGB(h, s, l float64) (float64, float64, float64) {
	var r, g, b float64

	if s == 0 {
		r, g, b = l, l, l
	} else {
		var hue2rgb = func(p, q, t float64) float64 {
			if t < 0 {
				t += 1
			}
			if t > 1 {
				t -= 1
			}
			if t < 1.0/6.0 {
				return p + (q-p)*6*t
			}
			if t < 1.0/2.0 {
				return q
			}
			if t < 2.0/3.0 {
				return p + (q-p)*(2.0/3.0-t)*6
			}
			return p
		}

		var q float64
		if l < 0.5 {
			q = l * (1 + s)
		} else {
			q = l + s - l*s
		}
		p := 2*l - q
		r = hue2rgb(p, q, h+1.0/3.0)
		g = hue2rgb(p, q, h)
		b = hue2rgb(p, q, h-1.0/3.0)
	}

	return r, g, b
}

var repeatingQuadIndices = [...]uint16{0, 1, 2, 1, 2, 3}

func appendTexturedQuad(vertices []ebiten.Vertex, indices []uint16, dstX, dstY, dstWidth, dstHeight, srcX, srcY, srcWidth, srcHeight float32) ([]ebiten.Vertex, []uint16) {
	base := uint16(len(vertices))
	vertices = append(vertices,
		ebiten.Vertex{DstX: dstX, DstY: dstY, SrcX: srcX, SrcY: srcY, ColorR: 1, ColorG: 1, ColorB: 1, ColorA: 1},
		ebiten.Vertex{DstX: dstX + dstWidth, DstY: dstY, SrcX: srcX + srcWidth, SrcY: srcY, ColorR: 1, ColorG: 1, ColorB: 1, ColorA: 1},
		ebiten.Vertex{DstX: dstX, DstY: dstY + dstHeight, SrcX: srcX, SrcY: srcY + srcHeight, ColorR: 1, ColorG: 1, ColorB: 1, ColorA: 1},
		ebiten.Vertex{DstX: dstX + dstWidth, DstY: dstY + dstHeight, SrcX: srcX + srcWidth, SrcY: srcY + srcHeight, ColorR: 1, ColorG: 1, ColorB: 1, ColorA: 1},
	)
	indices = append(indices, base, base+1, base+2, base+1, base+2, base+3)
	return vertices, indices
}

// drawRepeatingRotozoom renders an infinitely repeated texture directly into
// the destination. This replaces the very large pre-tiled background images
// while preserving the same affine texture mapping.
func drawRepeatingRotozoom(dst, texture *ebiten.Image, cx, cy, zoom, rotation, px, py float64, brightness float32) {
	composite.Repeat(dst, texture, composite.Repetition{CenterX: cx, CenterY: cy, Zoom: zoom, Rotation: rotation, PhaseX: px, PhaseY: py, Color: [4]float32{brightness, brightness, brightness, 1}})
}

// ==================== TCB DEMO (Demo2) ====================

var demo2RastData = originalassets.
	DCKAssetDemo2RastData()

var demo2MountainsData = originalassets.
	DCKAssetDemo2MountainsData()

var demo2LogoData = originalassets.
	DCKAssetDemo2LogoData()

var demo2FontData = originalassets.DCKAssetDemo2FontData()

const tcbScrollShaderSource = scrolling.PlaneShaderSource

type TCBDemo struct {
	initialized bool

	rasters   *ebiten.Image
	mountains *ebiten.Image
	logo      *ebiten.Image
	font      *ebiten.Image

	part *effects.MultiPlaneScene

	scrollText string
}

func NewTCBDemo() *TCBDemo {
	d := &TCBDemo{}
	d.initScrollText()

	return d
}

func (d *TCBDemo) initScrollText() {
	spc := "                             "
	d.scrollText = " ^0" + spc +
		"WOW, THIS DEMO SURE DOES LOOK GREAT..  BUT PERHAPS THE SCROLLINE LOOKS A BIT   TOO ORDINARY. " +
		"WELL, OKEY, LET US SWING IT UP AND DOWN. " +
		"^1 THIS IS THE LITTLE BIT OF EVERYTHING DEMO BY THE CAREBEARS. THERE ARE STAR RAY TYPE OF " +
		"BACKGROUND SCROLLERS, A DISTORTED TCB LOGO, " +
		"SOME GREAT MAD MAX MUSIC AND A SWINGING SCROLLINE OR..... PERHAPS EVEN MORE.............." +
		"^2...........  THIS IS BEGINNING TO LOOK " +
		"LIKE THE XXX INTERNATIONAL BALL DEMO SCREEN.                       " +
		"^3    BUT THEIR SCROLLINE WAS NOT THIS BIG. WE HOPE YOU DO NOT " +
		"THINK THAT WE HAVE TWO DIFFERENTLY SIZED FONTS. WE HAVE MANY MORE... ^4  " +
		"YEAH...  DO NOT LEAVE YET, THERE IS STILL MORE TO COME, JUST " +
		"WAIT AND SEE.  IF YOU THINK THIS IS HARD TO READ, WAIT TILL YOU HAVE " +
		"SEEN WHAT YOU ARE GOING TO SEE IN ABOUT THREE SECONDS.     " +
		"^5 THAT WAS NOT THREE SECONDS, BUT NOW YOU HAVE SEEN OUR THREE DIMENSIONAL " +
		"BENDING.. YOU MIGHT WONDER WHY WE HAVE NO PUNCTUATION EXCEPT " +
		"FOR THESE TWO ., . WE DO NOT EVEN HAVE THE LITTLE BLACK DOT BETWEEN HAVEN AND T, " +
		"HAVEN T, SEE... WELL, NOW THAT WE ARE OUT OF IDEAS WHAT " +
		"TO WRITE, WE CAN AS WELL EXPLAIN WHY. THE PROBLEM IS THAT ALL THE PART DEMOS " +
		"MUST WORK ON HALF A MEG AND EVERY CHARACTER TAKES ABOUT TEN " +
		"KILOBYTES. WE ARE GOING TO GREET SOME FOLKS NOW, SO LET US CHANGE WAVEFORM... " +
		"                        ^6             " +
		"MEGAGREETINGS GO TO ALL THE OTHER MEMBERS OF THE UNION. WE DO NOT FEEL " +
		"LIKE GREETING TO MUCH COZ WE DO NOT HAVE THOSE LITTLE BENT LINES, SO " +
		"WE CAN NOT MAKE COMMENTS. BUT JUST ONCE YOU WILL HAVE TO PRETEND YOU SAW " +
		"ONE OF THOSE, IT SHOULD HAVE COME INSTEAD OF THE SPACE BETWEEN " +
		"THE WORDS COOL AND YOUR. HERE WE GO... HELLO, AN COOL  YOUR NEW INTRO IS " +
		"REALLY SOMETHING .                    ^7 YOU WILL HAVE " +
		"TO READ IN THE MAIN SCROLLTEXT FOR MORE GREETINGS....  BYE.............. " +
		"                                             "
}

func (d *TCBDemo) Init() error {
	if d.initialized {
		return nil
	}

	var err error
	img, _, err := image.Decode(bytes.NewReader(demo2RastData))
	if err != nil {
		log.Printf("Error loading rasters: %v", err)
		d.rasters = ebiten.NewImage(320, 200)
		d.rasters.Fill(color.RGBA{255, 0, 255, 255})
	} else {
		d.rasters = ebiten.NewImageFromImage(img)
	}

	img, _, err = image.Decode(bytes.NewReader(demo2MountainsData))
	if err != nil {
		log.Printf("Error loading mountains: %v", err)
		d.mountains = ebiten.NewImage(1024, 320)
	} else {
		d.mountains = ebiten.NewImageFromImage(img)
	}

	img, _, err = image.Decode(bytes.NewReader(demo2LogoData))
	if err != nil {
		log.Printf("Error loading logo: %v", err)
		d.logo = ebiten.NewImage(320, 48)
	} else {
		d.logo = ebiten.NewImageFromImage(img)
	}

	img, _, err = image.Decode(bytes.NewReader(demo2FontData))
	if err != nil {
		log.Printf("Error loading font: %v", err)
		d.font = ebiten.NewImage(320, 198)
	} else {
		d.font = ebiten.NewImageFromImage(img)
	}
	if err = d.initMultiPlaneScene(); err != nil {
		return err
	}

	d.initialized = true
	return nil
}

func (d *TCBDemo) initMultiPlaneScene() error {
	spec, _ := presets.FindFont("multiscreen-tcb")
	metrics, err := spec.Build(d.font.Bounds())
	if err != nil {
		return err
	}
	scrollConfig := presets.TCBProjectedScroll(d.scrollText, 32, scrolling.Face{Atlas: d.font, Metrics: metrics}, d.rasters)
	scrollConfig.Projected.Draw = scrolling.PlaneDraw{OriginX: 64, OriginY: 60, ScaleX: 2, ScaleY: 2}
	profile, err := motion.CompileWaveTable(presets.TCBLogoWaveSections()...)
	if err != nil {
		return err
	}
	rows := presets.TCBLogoRowProfile(profile, 303)
	rows.ScaleX, rows.ScaleY = 2, 2
	rows.OutputX, rows.OutputY = 64, 60
	d.part, err = effects.NewMultiPlaneScene(effects.MultiPlaneSceneConfig{
		Mountains: d.mountains, Logo: d.logo,
		LogoSource: image.Rect(0, 16, 303, 48), CenterSource: image.Rect(114, 0, 193, 15),
		Bands: presets.TCBMountainBands(), Rows: rows,
		Center: sprites.AxisFlipConfig{
			Saw:       &motion.SawToggleConfig{Start: 0, Velocity: .08, Boundary: 1, Restart: -1},
			UseAnchor: true, AnchorX: 40, AnchorY: 8, BackMirrorY: true, BackMirrorShift: 16,
			Filter: ebiten.FilterNearest, Blend: ebiten.BlendSourceOver,
		},
		Scroll: scrollConfig, Viewport: image.Rect(64, 60, 704, 460),
		StageSize: image.Pt(320, 200), CenterX: 160, CenterY: 88, Filter: ebiten.FilterNearest,
	})
	return err
}

func (d *TCBDemo) Update() error {
	if !d.initialized {
		if err := d.Init(); err != nil {
			return err
		}
	}

	return d.part.Update(kit.Frame{})
}

func (d *TCBDemo) Draw(screen *ebiten.Image) {
	if !d.initialized {
		return
	}

	screen.Fill(color.Black)
	d.part.Draw(screen)
}

// ==================== COCO DEMO (Demo3) ====================

var demo3TitleData = originalassets.
	DCKAssetDemo3TitleData()

var demo3BarsData = originalassets.
	DCKAssetDemo3BarsData()

var demo3CocoData = originalassets.
	DCKAssetDemo3CocoData()

var demo3DmaLogoData = originalassets.
	DCKAssetDemo3DmaLogoData()

var demo3FontData = originalassets.DCKAssetDemo3FontData()

const (
	nbCubes3               = 12
	nbDMALogos3            = 16
	fontHeight3            = 36
	scrollScaleInt3        = 3
	scrollScaleFactor3     = 3.0
	scrollSurfWidthFactor3 = 2.0
	scrollSpeedFactor3     = 15
	cocoScrollPadding3     = "     "
	cocoScrollText3        = cocoScrollPadding3 + cocoScrollPadding3 +
		"WELCOME TO THE COCO IS THE BEST DEMO! " + cocoScrollPadding3 +
		"THIS DEMO COMBINES THE BEST EFFECTS FROM VARIOUS ATARI ST DEMOS. " + cocoScrollPadding3 +
		"GREETINGS TO ALL DEMOSCENE LOVERS! " + cocoScrollPadding3 + cocoScrollPadding3
)

type DMASprite3 struct {
	x, y float64
}

var (
	cocoCubePathSinStep3, cocoCubePathCosStep3 = math.Sincos(0.04)
	cocoCubeBobSinStep3, cocoCubeBobCosStep3   = math.Sincos(0.1)
)

var (
	cocoDMAPhaseOffsets3 = [4]float64{1.25, 0.54, 0.23, 0.98}
	cocoDMAPhaseDeltas3  = [4]float64{0.02 * 1.35, 0.02 * 1.86, 0.02 * 1.72, 0.02 * 1.63}
)

type CocoDemo struct {
	scrollRenderer *scrolling.Scrolling
	initialized    bool

	titleImg   *ebiten.Image
	barsImg    *ebiten.Image
	cocoImg    *ebiten.Image
	dmaLogoImg *ebiten.Image
	fontImg    *ebiten.Image

	scrollSurf *ebiten.Image
	cubeBatch  *effects.SolidCubeBatch

	fontAtlas *scrolling.Atlas

	// 3D Cubes
	cubes         [nbCubes3]*effects.SolidCube
	spritePos     [nbCubes3]float64
	spritePathSin [nbCubes3]float64
	spritePathCos [nbCubes3]float64
	spriteBobSin  [nbCubes3]float64
	spriteBobCos  [nbCubes3]float64

	// DMA logo sprites (16 logos in 4x4 grid)
	dmaSprites [nbDMALogos3]DMASprite3
	dmaSin     [4]float64
	dmaCos     [4]float64
	dmaStepSin [4]float64
	dmaStepCos [4]float64

	// Scrolling text (megatwist style)
	frontWavePos   int
	letterNum      int
	letterDecal    int
	curves         [][]int
	frontMainWave  []int
	position       []int
	scrollText     string
	scrollVertices []ebiten.Vertex
	scrollIndices  []uint16
	lastTextOffset int

	// Rotozoom
	posXi float64
	posZi float64
	posRi float64

	// Title logo animation
	logoX float64

	// Copper bars
	copper *composite.CopperBars

	// VBL counter
	iteration int
}

func NewCocoDemo() *CocoDemo {
	d := &CocoDemo{
		scrollSurf:     ebiten.NewImage(int(float64(demoWidth)*scrollSurfWidthFactor3), int(float64(fontHeight3)*scrollScaleFactor3)),
		cubeBatch:      effects.NewSolidCubeBatch(nbCubes3),
		scrollVertices: make([]ebiten.Vertex, 0, ((demoHeight-72)/scrollScaleInt3)*8),
		scrollIndices:  make([]uint16, 0, ((demoHeight-72)/scrollScaleInt3)*12),
		logoX:          0.5,
		lastTextOffset: -1,
		scrollText:     cocoScrollText3,
	}

	// Init 3D cubes
	for i := 0; i < nbCubes3; i++ {
		var err error
		d.cubes[i], err = effects.NewSolidCube(presets.MultiscreenCocoCube(40))
		if err != nil {
			panic(err)
		}
		d.cubes[i].Rotation = geometry.Vec3{X: float64(i) * .3, Y: float64(i) * .2, Z: float64(i) * .1}
		d.spritePos[i] = float64(0.15) * float64(i+1)
		d.spritePathSin[i], d.spritePathCos[i] = math.Sincos(d.spritePos[i])
		d.spriteBobSin[i], d.spriteBobCos[i] = math.Sincos(d.spritePos[i] * 2.5)
	}
	for i := range d.dmaSin {
		d.dmaSin[i], d.dmaCos[i] = math.Sincos(cocoDMAPhaseOffsets3[i])
		d.dmaStepSin[i], d.dmaStepCos[i] = math.Sincos(cocoDMAPhaseDeltas3[i])
	}

	// Init wave curves
	d.curves = make([][]int, 8)
	d.createCurves()
	d.precalcMainWave()

	return d
}

func (d *CocoDemo) Init() error {
	if d.initialized {
		return nil
	}

	var err error

	img, _, err := image.Decode(bytes.NewReader(demo3TitleData))
	if err != nil {
		log.Printf("Error loading title: %v", err)
	} else {
		d.titleImg = ebiten.NewImageFromImage(img)
	}

	img, _, err = image.Decode(bytes.NewReader(demo3BarsData))
	if err != nil {
		log.Printf("Error loading bars: %v", err)
	} else {
		d.barsImg = ebiten.NewImageFromImage(img)
		d.copper, err = composite.NewCopperBars(presets.BilizirCopperBars(d.barsImg, 72, composite.CopperImages, composite.MaskedClock))
		if err != nil {
			return err
		}
	}

	img, _, err = image.Decode(bytes.NewReader(demo3CocoData))
	if err != nil {
		log.Printf("Error loading coco: %v", err)
	} else {
		d.cocoImg = ebiten.NewImageFromImage(img)
	}

	img, _, err = image.Decode(bytes.NewReader(demo3DmaLogoData))
	if err != nil {
		log.Printf("Error loading DMA logo: %v", err)
	} else {
		d.dmaLogoImg = ebiten.NewImageFromImage(img)
	}

	img, _, err = image.Decode(bytes.NewReader(demo3FontData))
	if err != nil {
		log.Printf("Error loading font: %v", err)
	} else {
		d.fontImg = ebiten.NewImageFromImage(img)
	}
	d.initFontData3()
	d.precalcPosition()

	d.initialized = true
	return nil
}

func (d *CocoDemo) initFontData3() {
	var err error
	d.fontAtlas, err = presets.FontAtlas("multiscreen-coco", d.fontImg)
	if err != nil {
		panic(err)
	}
}

func (d *CocoDemo) Update() error {
	if !d.initialized {
		if err := d.Init(); err != nil {
			return err
		}
	}

	d.iteration++

	// Update copper bars
	if d.copper != nil {
		if err := d.copper.Update(kit.Frame{}); err != nil {
			return err
		}
	}

	// Update 3D cubes
	for i := 0; i < nbCubes3; i++ {
		d.spritePos[i] += 0.04
		if d.iteration&1023 == 0 {
			// 4π is a common period of sin(p) and cos(2.5p).
			d.spritePos[i] = math.Mod(d.spritePos[i], 4*math.Pi)
			d.spritePathSin[i], d.spritePathCos[i] = math.Sincos(d.spritePos[i])
			d.spriteBobSin[i], d.spriteBobCos[i] = math.Sincos(d.spritePos[i] * 2.5)
		} else {
			d.spritePathSin[i], d.spritePathCos[i] = stepSinCosForward(
				d.spritePathSin[i], d.spritePathCos[i], cocoCubePathSinStep3, cocoCubePathCosStep3,
			)
			d.spriteBobSin[i], d.spriteBobCos[i] = stepSinCosForward(
				d.spriteBobSin[i], d.spriteBobCos[i], cocoCubeBobSinStep3, cocoCubeBobCosStep3,
			)
		}
		d.cubes[i].Rotate(
			0.02*(1+float64(i)*0.1),
			0.03*(1+float64(i)*0.15),
			0.01*(1+float64(i)*0.05),
		)
	}

	// Update DMA logo sprites - synchronized movement
	for i := range d.dmaSin {
		if d.iteration&1023 == 0 {
			phase := cocoDMAPhaseOffsets3[i] + float64(d.iteration)*cocoDMAPhaseDeltas3[i]
			d.dmaSin[i], d.dmaCos[i] = math.Sincos(math.Mod(phase, 2*math.Pi))
		} else {
			d.dmaSin[i], d.dmaCos[i] = stepSinCosForward(
				d.dmaSin[i], d.dmaCos[i], d.dmaStepSin[i], d.dmaStepCos[i],
			)
		}
	}
	baseX := 100*d.dmaSin[0] + 100*d.dmaSin[1]
	baseY := 60*d.dmaCos[2] + 60*d.dmaCos[3]

	for i := 0; i < nbDMALogos3; i++ {
		row := i / 4
		col := i % 4
		centerX := float64(demoWidth) / 2
		centerY := 72 + float64(demoHeight-72)/2
		offsetX := (float64(col) - 1.5) * 200
		offsetY := (float64(row) - 1.5) * 140
		d.dmaSprites[i].x = centerX + offsetX + baseX
		d.dmaSprites[i].y = centerY + offsetY + baseY
	}

	// Update rotozoom
	d.posXi += 0.008
	d.posZi += 0.003
	d.posRi += 0.005

	// Update title logo
	d.logoX += 0.0125
	return nil
}

func (d *CocoDemo) Draw(screen *ebiten.Image) {
	if !d.initialized {
		return
	}

	screen.Fill(color.RGBA{0x00, 0x00, 0x30, 0xFF})

	// 1. Rotozoom background
	d.drawRotozoom3(screen)

	// 2. Scrolling text with distortion
	d.drawScrollText3(screen)

	// 3. DMA logo sprites
	d.drawDMALogos3(screen)

	// 4. 3D cubes
	d.draw3DCubes3(screen)

	// 5. Title logo with copper bars on top
	d.drawTitleWithCopperbars3(screen)
}

func (d *CocoDemo) drawRotozoom3(dst *ebiten.Image) {
	zoom := 0.5 + math.Abs(math.Sin(d.posZi)*2.5)
	rot := 360.0 / 4.0 * math.Cos(d.posRi*4-math.Cos(d.posRi-0.01)) * 0.3 * math.Pi / 180

	posXCurve := math.Cos(d.posXi - 0.1)
	oscX := (float64(demoWidth) / 4) * math.Cos(d.posXi*4-posXCurve)
	oscY := (float64(demoHeight) / 2.7) * -math.Sin(d.posXi*2.3-posXCurve)

	centerX := float64(demoWidth)/2 + oscX
	centerY := float64(demoHeight)/2 + oscY

	drawRepeatingRotozoom(dst, d.cocoImg, centerX, centerY, zoom, rot, demoWidth*4, demoHeight*4, 0.5)
}

func (d *CocoDemo) drawDMALogos3(dst *ebiten.Image) {
	if d.dmaLogoImg == nil {
		return
	}

	logoW := float64(d.dmaLogoImg.Bounds().Dx())
	logoH := float64(d.dmaLogoImg.Bounds().Dy())
	scale := 0.5

	for _, sprite := range d.dmaSprites {
		op := &ebiten.DrawImageOptions{}
		op.GeoM.Translate(-logoW/2, -logoH/2)
		op.GeoM.Scale(scale, scale)
		op.GeoM.Translate(sprite.x, sprite.y)
		op.ColorScale.Scale(1, 1, 1, 0.6)
		dst.DrawImage(d.dmaLogoImg, op)
	}
}

func (d *CocoDemo) drawScrollText3(dst *ebiten.Image) {
	d.frontWavePos = d.iteration * scrollSpeedFactor3

	decalX := d.scrollOffset3(d.frontWavePos)

	// letterNum is deliberately unbounded. getPosition3 and getLetter3 repeat
	// their source tables together so the whole message keeps looping instead
	// of becoming clamped to its last character.
	d.advanceScrollLetter3(decalX)

	// Safety check before calling displayText3
	if d.letterNum >= 0 && len(d.scrollText) > 0 {
		d.displayText3(d.letterNum)
	}

	bounce := int(18.0 * math.Abs(math.Sin(float64(d.iteration)*0.1)))

	scrollWidth := d.scrollSurf.Bounds().Dx()

	baseY := 72
	totalLines := demoHeight - 72
	d.scrollVertices = d.scrollVertices[:0]
	d.scrollIndices = d.scrollIndices[:0]
	// Each source-font line is enlarged to exactly three destination lines.
	// Emit one three-pixel strip instead of three equivalent one-pixel strips:
	// this preserves nearest-neighbour sampling while cutting the scrolling
	// mesh and its command-buffer upload to one third of their former size.
	for sourceFontLine := 0; sourceFontLine < totalLines/scrollScaleInt3; sourceFontLine++ {
		ligne := sourceFontLine * scrollScaleInt3
		frontWave := d.getWave3(d.frontWavePos + sourceFontLine)
		scrollXRaw := frontWave - d.letterDecal

		scaledLine := ((sourceFontLine + bounce) % fontHeight3) * scrollScaleInt3

		if scrollXRaw < 0 {
			visibleWidth := demoWidth + scrollXRaw
			if visibleWidth > 0 {
				width := minInt3(visibleWidth, scrollWidth)
				d.scrollVertices, d.scrollIndices = appendTexturedQuad(
					d.scrollVertices, d.scrollIndices,
					float32(-scrollXRaw), float32(baseY+ligne), float32(width), scrollScaleInt3,
					0, float32(scaledLine), float32(width), scrollScaleInt3,
				)
			}
			continue
		}

		scrollX := scrollXRaw % scrollWidth
		if scrollX >= scrollWidth-demoWidth {
			width1 := scrollWidth - scrollX
			if width1 > 0 && width1 <= demoWidth {
				d.scrollVertices, d.scrollIndices = appendTexturedQuad(
					d.scrollVertices, d.scrollIndices,
					0, float32(baseY+ligne), float32(width1), scrollScaleInt3,
					float32(scrollX), float32(scaledLine), float32(width1), scrollScaleInt3,
				)
			}

			width2 := demoWidth - width1
			if width2 > 0 && width2 <= demoWidth {
				d.scrollVertices, d.scrollIndices = appendTexturedQuad(
					d.scrollVertices, d.scrollIndices,
					float32(width1), float32(baseY+ligne), float32(width2), scrollScaleInt3,
					0, float32(scaledLine), float32(width2), scrollScaleInt3,
				)
			}
		} else if scrollX+demoWidth <= scrollWidth {
			d.scrollVertices, d.scrollIndices = appendTexturedQuad(
				d.scrollVertices, d.scrollIndices,
				0, float32(baseY+ligne), demoWidth, scrollScaleInt3,
				float32(scrollX), float32(scaledLine), demoWidth, scrollScaleInt3,
			)
		}
	}
	if len(d.scrollIndices) > 0 {
		op := &ebiten.DrawTrianglesOptions{Filter: ebiten.FilterNearest}
		dst.DrawTriangles(d.scrollVertices, d.scrollIndices, d.scrollSurf, op)
	}
}

func minInt3(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func (d *CocoDemo) displayText3(letterOffset int) {
	if d.fontImg == nil || letterOffset == d.lastTextOffset {
		return
	}
	d.lastTextOffset = letterOffset
	d.scrollSurf.Clear()
	if d.scrollRenderer == nil {
		glyphs := make([]scrolling.Glyph, len(d.scrollText))
		for i := range d.scrollText {
			r := d.scrollText[i]
			glyphImage, letter, ok := d.fontAtlas.ExactGlyph(rune(r))
			if ok {
				glyphs[i] = scrolling.Glyph{Image: glyphImage, Advance: letter.Advance}
			} else {
				glyphs[i] = scrolling.Glyph{Advance: 32}
			}
		}
		var err error
		d.scrollRenderer, err = scrolling.New(scrolling.Config{Glyphs: glyphs})
		if err != nil {
			panic(err)
		}
	}
	state := d.scrollRenderer.Window(letterOffset, float64(d.scrollSurf.Bounds().Dx())/scrollScaleFactor3)
	state.X *= scrollScaleFactor3
	state.ScaleX = scrollScaleFactor3
	state.ScaleY = scrollScaleFactor3
	d.scrollRenderer.DrawAt(d.scrollSurf, state)
}

func (d *CocoDemo) draw3DCubes3(dst *ebiten.Image) {
	d.cubeBatch.Reset()
	for i := 0; i < nbCubes3; i++ {
		xPos := float64((demoWidth-40)/2) + float64((demoWidth-40)/2)*d.spritePathSin[i]
		yPos := float64(demoHeight)/2 + 84*d.spriteBobCos[i]
		d.cubeBatch.Add(d.cubes[i], xPos, yPos)
	}
	d.cubeBatch.Draw(dst)
}

func (d *CocoDemo) drawTitleWithCopperbars3(dst *ebiten.Image) {
	if d.titleImg == nil {
		return
	}

	vector.DrawFilledRect(dst, 0, 0, demoWidth, 72, color.Black, false)
	if d.copper != nil {
		d.copper.Draw(dst)
	}

	titleX := 64 + float64(demoWidth)*math.Cos(d.logoX)
	titleH := float64(d.titleImg.Bounds().Dy())
	scaleY := 72.0 / titleH

	op := &ebiten.DrawImageOptions{}
	op.GeoM.Scale(1.0, scaleY)
	op.GeoM.Translate(titleX, 0)
	dst.DrawImage(d.titleImg, op)
}

func (d *CocoDemo) createCurves() {
	curves, err := presets.RibbonCurves(1)
	if err != nil {
		panic(err)
	}
	d.curves = curves[:8]
}

func (d *CocoDemo) precalcPosition() {
	count := 0
	d.position = make([]int, 0, len(d.scrollText))

	for i := 0; i < len(d.scrollText); i++ {
		if _, letter, ok := d.fontAtlas.ExactGlyph(rune(d.scrollText[i])); ok {
			count += int(float64(int(letter.Advance)) * scrollScaleFactor3)
			d.position = append(d.position, count)
		}
	}
}

func (d *CocoDemo) precalcMainWave() {
	frontMainWaveTable := []int{
		1, 1, 4, 1, 1, 2, 3, 2, 1, 5, 2, 1, 7,
	}

	var err error
	d.frontMainWave, err = composite.JoinDeltaCurves(d.curves, frontMainWaveTable)
	if err != nil {
		panic(err)
	}
}

func (d *CocoDemo) getSum3(arr []int, index, decal int) int {
	return composite.CumulativeAt(arr, index, decal)
}

func (d *CocoDemo) getWave3(i int) int {
	return d.getSum3(d.frontMainWave, i, 0)
}

func (d *CocoDemo) getPosition3(i int) int {
	if i > 0 {
		return d.getSum3(d.position, i-1, 0)
	}
	return 0
}

func (d *CocoDemo) advanceScrollLetter3(decalX int) {
	if len(d.position) == 0 {
		d.letterNum = 0
		d.letterDecal = 0
		return
	}
	for d.letterNum > 0 && decalX < d.getPosition3(d.letterNum) {
		d.letterNum--
	}
	for d.getPosition3(d.letterNum+1) <= decalX {
		d.letterNum++
	}
	d.letterDecal = d.getPosition3(d.letterNum)
}

func (d *CocoDemo) scrollOffset3(frontWavePos int) int {
	decalX := d.getWave3(frontWavePos)
	for line := 1; line < fontHeight3; line++ {
		if wave := d.getWave3(frontWavePos + line); wave < decalX {
			decalX = wave
		}
	}
	if decalX < 0 {
		return 0
	}
	return decalX
}

func (d *CocoDemo) getLetter3(pos int) byte {
	if len(d.scrollText) == 0 {
		return ' '
	}
	return d.scrollText[pos%len(d.scrollText)]
}

// ==================== VIVA TCB DEMO (Demo4) ====================

var demo4LogoData = originalassets.
	DCKAssetDemo4LogoData()

var demo4TitleData = originalassets.
	DCKAssetDemo4TitleData()

var demo4RasterData = originalassets.
	DCKAssetDemo4RasterData()

var demo4TileData = originalassets.
	DCKAssetDemo4TileData()

var demo4FontData = originalassets.DCKAssetDemo4FontData()

const (
	fontCharWidth4  = 42
	fontCharHeight4 = 40
)

type VivaDemo struct {
	pseudoScroll  *scrolling.Scrolling
	logoFormation *sprites.RecurrentFormation
	initialized   bool

	logoImg   *ebiten.Image
	titleImg  *ebiten.Image
	rasterImg *ebiten.Image
	tileImg   *ebiten.Image
	fontImg   *ebiten.Image
	fontAtlas *scrolling.Atlas

	titleMotion *motion.WaveClock
	rasterTitle *composite.RasterTitle

	posXi float64
	posZi float64
	posRi float64

	text1 []rune
	text2 []rune
	text3 []rune
	text4 []rune

	loopCounter int
}

func NewVivaDemo() *VivaDemo {
	d := &VivaDemo{}

	pad := "       "
	d.text1 = []rune(pad + "VIVA THE CAREBEARS!" + pad)
	d.text2 = []rune(pad + "LEGENDS OF THE ATARI ST DEMOSCENE!" + pad)
	d.text3 = []rune(pad + "GREETINGS TO ALL DEMOSCENERS!" + pad)
	d.text4 = []rune(pad + "KEEP THE SCENE ALIVE!" + pad)

	return d
}

func (d *VivaDemo) Init() error {
	if d.initialized {
		return nil
	}

	var err error
	d.titleMotion, err = motion.NewWaveClock(presets.VivaTitleMotion(demoWidth, 0))
	if err != nil {
		return err
	}
	img, _, err := image.Decode(bytes.NewReader(demo4LogoData))
	if err != nil {
		log.Printf("Error loading logo: %v", err)
	} else {
		d.logoImg = ebiten.NewImageFromImage(img)
		formation := presets.VivaLogoFormation(d.logoImg, demoWidth, demoHeight, 150.0/4)
		d.logoFormation, err = sprites.NewRecurrentFormation(formation)
		if err != nil {
			return err
		}
	}

	img, _, err = image.Decode(bytes.NewReader(demo4TitleData))
	if err != nil {
		log.Printf("Error loading title: %v", err)
	} else {
		d.titleImg = ebiten.NewImageFromImage(img)
	}

	img, _, err = image.Decode(bytes.NewReader(demo4RasterData))
	if err != nil {
		log.Printf("Error loading raster: %v", err)
	} else {
		d.rasterImg = ebiten.NewImageFromImage(img)
	}
	if d.titleImg != nil && d.rasterImg != nil {
		d.rasterTitle, err = composite.NewRasterTitle(presets.VivaRasterTitleDirect(d.titleImg, d.rasterImg, demoWidth))
		if err != nil {
			return err
		}
	}

	img, _, err = image.Decode(bytes.NewReader(demo4TileData))
	if err != nil {
		log.Printf("Error loading tile: %v", err)
	} else {
		d.tileImg = ebiten.NewImageFromImage(img)
	}

	img, _, err = image.Decode(bytes.NewReader(demo4FontData))
	if err != nil {
		log.Printf("Error loading font: %v", err)
	} else {
		d.fontImg = ebiten.NewImageFromImage(img)
		d.fontAtlas, err = presets.FontAtlas("multiscreen-viva", d.fontImg)
		if err != nil {
			return err
		}
		pseudo := presets.VivaPseudo3D(scrolling.Face{}, d.fontAtlas,
			[4]string{string(d.text1), string(d.text2), string(d.text3), string(d.text4)}, demoWidth, demoHeight)
		d.pseudoScroll, err = scrolling.New(scrolling.Config{Pseudo3D: &pseudo})
		if err != nil {
			return err
		}
	}

	d.initialized = true
	return nil
}

func stepSinCosForward(sinValue, cosValue, sinStep, cosStep float64) (float64, float64) {
	return sinValue*cosStep + cosValue*sinStep, cosValue*cosStep - sinValue*sinStep
}

func (d *VivaDemo) Update() error {
	if !d.initialized {
		if err := d.Init(); err != nil {
			return err
		}
	}

	// Update effect positions
	d.posXi += 0.008
	d.posZi += 0.003
	d.posRi += 0.005

	// Title animation
	d.titleMotion.Step()

	// Raster animation
	if d.rasterTitle != nil {
		d.rasterTitle.Step()
	}

	d.loopCounter++
	if d.logoFormation != nil {
		if err := d.logoFormation.Update(float64(d.loopCounter)); err != nil {
			return err
		}
	}
	if d.pseudoScroll != nil {
		return d.pseudoScroll.Update(kit.Frame{Tick: uint64(d.loopCounter), Time: float64(d.loopCounter) / 60})
	}
	return nil
}

func (d *VivaDemo) Draw(screen *ebiten.Image) {
	if !d.initialized {
		return
	}

	screen.Fill(color.Black)

	// Draw background with tiles
	zoom := 0.5 + math.Abs(math.Sin(d.posZi)*2.5)
	rot := (360.0 / 4.0 * math.Cos(d.posRi*4-math.Cos(d.posRi-0.01))) * 0.3 * math.Pi / 180

	posXCurve := math.Cos(d.posXi - 0.1)
	oscX := (800.0 / 4) * math.Cos(d.posXi*4-posXCurve)
	oscY := (600.0 / 2.7) * -math.Sin(d.posXi*2.3-posXCurve)

	centerX := 400.0 + oscX
	centerY := 300.0 + oscY

	drawRepeatingRotozoom(screen, d.tileImg, centerX, centerY, zoom, rot, demoWidth*8, demoHeight*8, 1)

	if d.pseudoScroll != nil {
		d.pseudoScroll.Draw(screen)
	}

	// Black bar at top
	vector.DrawFilledRect(screen, 0, 0, demoWidth, 72, color.Black, false)

	if d.rasterTitle != nil {
		d.rasterTitle.DrawAt(screen, d.titleMotion.At(0), 14)
	}

	if d.logoFormation != nil {
		d.logoFormation.Draw(screen)
	}
}

// ==================== MEGA DEMO GAME ====================

type MegaDemoGame struct {
	demo1 *PhenomenaDemo
	demo2 *TCBDemo
	demo3 *CocoDemo
	demo4 *VivaDemo

	demoCanvases      [4]*ebiten.Image
	compositeShader   *ebiten.Shader
	compositeCenter   [2]float32
	compositeUniforms map[string]any

	audioContext *audio.Context
	audioPlayer  *audio.Player
	musicStream  *sound.Stream

	cameraState CameraState
	stateTimer  float64

	cameraX float64
	cameraY float64

	transitionTime float64
	needsRedraw    bool
}

func NewMegaDemoGame() *MegaDemoGame {
	g := &MegaDemoGame{
		demo1: NewPhenomenaDemo(),
		demo2: NewTCBDemo(),
		demo3: NewCocoDemo(),
		demo4: NewVivaDemo(),
		demoCanvases: [4]*ebiten.Image{
			ebiten.NewImage(demoWidth, demoHeight),
			ebiten.NewImage(demoWidth, demoHeight),
			ebiten.NewImage(demoWidth, demoHeight),
			ebiten.NewImage(demoWidth, demoHeight),
		},
		cameraState:  StateDemo1,
		needsRedraw:  true,
		cameraX:      0,
		cameraY:      0,
		audioContext: audio.NewContext(sampleRate),
	}
	var shaderErr error
	g.compositeShader, shaderErr = ebiten.NewShader([]byte(compositeShaderSource))
	if shaderErr != nil {
		log.Printf("Failed to compile camera compositor shader: %v", shaderErr)
	}
	g.compositeUniforms = map[string]any{
		"CameraCenter": g.compositeCenter[:],
		"CameraZoom":   float32(1),
	}

	// Initialize music
	var err error
	g.musicStream, err = sound.Open("music.ym", musicData, sound.Options{SampleRate: sampleRate, Loop: true, PCMFormat: sound.PCM16, Gain: 0.5})
	if err != nil {
		log.Printf("Failed to open music: %v", err)
	} else {
		g.audioPlayer, err = g.audioContext.NewPlayer(g.musicStream)
		if err != nil {
			log.Printf("Failed to create audio player: %v", err)
			g.musicStream.Close()
			g.musicStream = nil
		} else {
			g.audioPlayer.Play()
		}
	}

	return g
}

func (g *MegaDemoGame) Update() error {
	g.needsRedraw = true

	// Keep every screen synchronized so transitions and the zoomed-out view
	// always reveal a continuously running demo.
	if err := g.demo1.Update(); err != nil {
		return fmt.Errorf("phenomena demo: %w", err)
	}
	if err := g.demo2.Update(); err != nil {
		return fmt.Errorf("tcb demo: %w", err)
	}
	if err := g.demo3.Update(); err != nil {
		return fmt.Errorf("coco demo: %w", err)
	}
	if err := g.demo4.Update(); err != nil {
		return fmt.Errorf("viva demo: %w", err)
	}

	// Update state machine
	dt := 1.0 / 60.0
	g.stateTimer += dt

	switch g.cameraState {
	case StateDemo1:
		if g.stateTimer >= viewDuration {
			g.cameraState = StateTransition1to2
			g.transitionTime = 0
			g.stateTimer = 0
		}

	case StateTransition1to2:
		g.transitionTime += dt
		progress := g.transitionTime / transitionDuration
		if progress >= 1.0 {
			g.cameraState = StateDemo2
			g.cameraX = 800
			g.cameraY = 0
			g.stateTimer = 0
		} else {
			eased := easeInOutCubic(progress)
			g.cameraX = 0 + eased*800
			g.cameraY = 0
		}

	case StateDemo2:
		if g.stateTimer >= viewDuration {
			g.cameraState = StateTransition2to3
			g.transitionTime = 0
			g.stateTimer = 0
		}

	case StateTransition2to3:
		g.transitionTime += dt
		progress := g.transitionTime / transitionDuration
		if progress >= 1.0 {
			g.cameraState = StateDemo3
			g.cameraX = 800
			g.cameraY = 600
			g.stateTimer = 0
		} else {
			eased := easeInOutCubic(progress)
			g.cameraX = 800
			g.cameraY = 0 + eased*600
		}

	case StateDemo3:
		if g.stateTimer >= viewDuration {
			g.cameraState = StateTransition3to4
			g.transitionTime = 0
			g.stateTimer = 0
		}

	case StateTransition3to4:
		g.transitionTime += dt
		progress := g.transitionTime / transitionDuration
		if progress >= 1.0 {
			g.cameraState = StateDemo4
			g.cameraX = 0
			g.cameraY = 600
			g.stateTimer = 0
		} else {
			eased := easeInOutCubic(progress)
			g.cameraX = 800 - eased*800
			g.cameraY = 600
		}

	case StateDemo4:
		if g.stateTimer >= viewDuration {
			g.cameraState = StateTransition4toZoom
			g.transitionTime = 0
			g.stateTimer = 0
		}

	case StateTransition4toZoom:
		// Transition handled in Draw with zoom
		g.transitionTime += dt
		progress := g.transitionTime / transitionDuration
		if progress >= 1.0 {
			g.cameraState = StateZoomOut
			g.cameraX = 400
			g.cameraY = 300
			g.stateTimer = 0
		}

	case StateZoomOut:
		if g.stateTimer >= viewDuration {
			g.cameraState = StateLoop
			g.transitionTime = 0
			g.stateTimer = 0
		}

	case StateLoop:
		g.transitionTime += dt
		progress := g.transitionTime / transitionDuration
		if progress >= 1.0 {
			g.cameraState = StateDemo1
			g.cameraX = 0
			g.cameraY = 0
			g.stateTimer = 0
		} else {
			eased := easeInOutCubic(progress)
			g.cameraX = 400 - eased*400
			g.cameraY = 300 - eased*300
		}
	}

	return nil
}

func (g *MegaDemoGame) Draw(screen *ebiten.Image) {
	if !g.needsRedraw {
		return
	}
	g.needsRedraw = false

	// The four stable camera states cover the whole screen. Drawing directly
	// avoids a full 800x600 render target and copy on the common path.
	switch g.cameraState {
	case StateDemo1:
		g.demo1.Draw(screen)
		return
	case StateDemo2:
		g.demo2.Draw(screen)
		return
	case StateDemo3:
		g.demo3.Draw(screen)
		return
	case StateDemo4:
		g.demo4.Draw(screen)
		return
	}

	centerX := g.cameraX + float64(demoWidth)/2
	centerY := g.cameraY + float64(demoHeight)/2
	zoom := 1.0
	mask := visibleDemoMask(g.cameraState)

	switch g.cameraState {
	case StateTransition4toZoom:
		progress := g.transitionTime / transitionDuration
		if progress > 1 {
			progress = 1
		}
		eased := easeInOutCubic(progress)

		startCenterX := float64(demoWidth) / 2
		startCenterY := float64(demoHeight) + float64(demoHeight)/2
		endCenterX := float64(screenWidth) / 2
		endCenterY := float64(screenHeight) / 2

		centerX = startCenterX + (endCenterX-startCenterX)*eased
		centerY = startCenterY + (endCenterY-startCenterY)*eased
		zoom = 1.0 - eased*0.5

	case StateLoop:
		progress := g.transitionTime / transitionDuration
		if progress > 1 {
			progress = 1
		}
		eased := easeInOutCubic(progress)
		startZoom := 0.5
		zoom = startZoom + (1.0-startZoom)*eased
		startCenterX := float64(screenWidth) / 2
		startCenterY := float64(screenHeight) / 2
		endCenterX := float64(demoWidth) / 2
		endCenterY := float64(demoHeight) / 2
		centerX = startCenterX + (endCenterX-startCenterX)*eased
		centerY = startCenterY + (endCenterY-startCenterY)*eased

	case StateZoomOut:
		centerX = float64(screenWidth) / 2
		centerY = float64(screenHeight) / 2
		zoom = 0.5
	}

	for demoIndex := 0; demoIndex < len(g.demoCanvases); demoIndex++ {
		if mask&(1<<demoIndex) == 0 {
			continue
		}
		g.renderDemo(demoIndex)
	}

	if g.compositeShader != nil {
		g.compositeCenter[0], g.compositeCenter[1] = float32(centerX), float32(centerY)
		g.compositeUniforms["CameraZoom"] = float32(zoom)
		op := &ebiten.DrawRectShaderOptions{Uniforms: g.compositeUniforms, Blend: ebiten.BlendCopy}
		for i, canvas := range g.demoCanvases {
			if mask&(1<<i) != 0 {
				op.Images[i] = canvas
			}
		}
		screen.DrawRectShader(demoWidth, demoHeight, g.compositeShader, op)
		return
	}

	// Compilation is covered by tests, but retain a conventional fallback for
	// graphics backends that reject the shader at runtime.
	screen.Fill(color.Black)
	for demoIndex := 0; demoIndex < len(g.demoCanvases); demoIndex++ {
		if mask&(1<<demoIndex) == 0 {
			continue
		}
		originX, originY := demoOrigin(demoIndex)
		op := &ebiten.DrawImageOptions{}
		op.GeoM.Translate(originX-centerX, originY-centerY)
		op.GeoM.Scale(zoom, zoom)
		op.GeoM.Translate(float64(demoWidth)/2, float64(demoHeight)/2)
		screen.DrawImage(g.demoCanvases[demoIndex], op)
	}
}

func visibleDemoMask(state CameraState) uint8 {
	switch state {
	case StateDemo1:
		return 1 << 0
	case StateTransition1to2:
		return 1<<0 | 1<<1
	case StateDemo2:
		return 1 << 1
	case StateTransition2to3:
		return 1<<1 | 1<<2
	case StateDemo3:
		return 1 << 2
	case StateTransition3to4:
		return 1<<2 | 1<<3
	case StateDemo4:
		return 1 << 3
	default:
		return 1<<0 | 1<<1 | 1<<2 | 1<<3
	}
}

func demoOrigin(index int) (float64, float64) {
	switch index {
	case 0:
		return 0, 0
	case 1:
		return demoWidth, 0
	case 2:
		return demoWidth, demoHeight
	case 3:
		return 0, demoHeight
	default:
		panic("invalid demo index")
	}
}

func (g *MegaDemoGame) renderDemo(index int) *ebiten.Image {
	canvas := g.demoCanvases[index]
	switch index {
	case 0:
		g.demo1.Draw(canvas)
	case 1:
		g.demo2.Draw(canvas)
	case 2:
		g.demo3.Draw(canvas)
	case 3:
		g.demo4.Draw(canvas)
	default:
		panic("invalid demo index")
	}
	return canvas
}

func (g *MegaDemoGame) Layout(outsideWidth, outsideHeight int) (int, int) {
	return 800, 600
}
