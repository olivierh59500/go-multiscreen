package multiscreen

import originalassets "multiscreen-mega-demo"

import (
	"bytes"

	"fmt"
	"github.com/olivierh59500/democonstructionkit/composite"
	"github.com/olivierh59500/democonstructionkit/scrolling"
	"image"
	"image/color"
	_ "image/png"
	"io"
	"log"
	"math"
	"sync"

	"github.com/hajimehoshi/ebiten/v2"
	"github.com/hajimehoshi/ebiten/v2/audio"
	"github.com/hajimehoshi/ebiten/v2/vector"
	"github.com/olivierh59500/ym-player/pkg/stsound"
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

// YMPlayer wraps the YM player for Ebiten audio
type YMPlayer struct {
	player *stsound.StSound
	buffer []int16
	mutex  sync.Mutex
	loop   bool
}

func NewYMPlayer(data []byte, sampleRate int, loop bool) (*YMPlayer, error) {
	player := stsound.CreateWithRate(sampleRate)

	if err := player.LoadMemory(data); err != nil {
		player.Destroy()
		return nil, fmt.Errorf("failed to load YM data: %w", err)
	}

	player.SetLoopMode(loop)

	return &YMPlayer{
		player: player,
		buffer: make([]int16, 4096),
		loop:   loop,
	}, nil
}

func (y *YMPlayer) Read(p []byte) (n int, err error) {
	y.mutex.Lock()
	defer y.mutex.Unlock()

	samplesNeeded := len(p) / 4
	processed := 0
	for processed < samplesNeeded {
		chunkSize := samplesNeeded - processed
		if chunkSize > len(y.buffer) {
			chunkSize = len(y.buffer)
		}

		if !y.player.Compute(y.buffer[:chunkSize], chunkSize) {
			if !y.loop {
				clear(p[processed*4 : samplesNeeded*4])
				err = io.EOF
				break
			}
		}

		for i := 0; i < chunkSize; i++ {
			// The demo volume is fixed at 50%. Integer division has the same
			// truncation-toward-zero result as the former float64 conversion.
			sample := y.buffer[i] / 2
			offset := (processed + i) * 4
			p[offset] = byte(sample)
			p[offset+1] = byte(sample >> 8)
			p[offset+2] = byte(sample)
			p[offset+3] = byte(sample >> 8)
		}

		processed += chunkSize
	}

	return samplesNeeded * 4, err
}

func (y *YMPlayer) Close() error {
	y.mutex.Lock()
	defer y.mutex.Unlock()

	if y.player != nil {
		y.player.Destroy()
		y.player = nil
	}
	return nil
}

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
	scrollRenderer *scrolling.Scrolling
	scrollBatch    *composite.QuadBatch
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
	msgIndex         int
	sliceCount       int
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
	scrollChars    [240]ScrollChar
	scrollHead     int
	scrollVertices []ebiten.Vertex
	scrollIndices  []uint16
	sineOffsets    [240]float64
}

type ScrollChar struct {
	glyph uint8
	frame uint8
	slice uint8
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

const charsetPhenomena = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!'?/,.-@"

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
		scrollVertices:   make([]ebiten.Vertex, 0, 240*4),
		scrollIndices:    make([]uint16, 0, 240*6),
	}

	for i := range d.sineOffsets {
		d.sineOffsets[i] = math.Sin(float64(i)*0.05) * 15
	}

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
			idx, found := charToFontIndexPhe(ch)

			if found && idx >= 0 {
				op := &ebiten.DrawImageOptions{}
				op.GeoM.Scale(2, 2)
				op.GeoM.Translate(float64(x), float64(t.Y))

				sx := idx * 16
				sy := 0
				subImg := d.imgFont.SubImage(image.Rect(sx, sy, sx+16, sy+26)).(*ebiten.Image)

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

func charToFontIndexPhe(ch rune) (int, bool) {
	if ch >= 'a' && ch <= 'z' {
		ch -= 'a' - 'A'
	}
	switch {
	case ch == ' ':
		return 0, true
	case ch >= 'A' && ch <= 'Z':
		return int(ch-'A') + 1, true
	case ch >= '0' && ch <= '9':
		return int(ch-'0') + 27, true
	case ch == '!':
		return 37, true
	case ch == '\'':
		return 38, true
	case ch == '?':
		return 39, true
	case ch == '/':
		return 40, true
	case ch == ',':
		return 41, true
	case ch == '.':
		return 42, true
	case ch == '-':
		return 43, true
	case ch == '@':
		return 44, true
	default:
		return 0, false
	}
}

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
	cnvRedBar := createGradient(480, 9, gdcRedBar)
	cnvSilverBar := createGradient(480, 33, gdcSilverBar)
	cnvPurpleBar := createGradient(480, 33, gdcPurpleBar)
	defer cnvRedBar.Dispose()
	defer cnvSilverBar.Dispose()
	defer cnvPurpleBar.Dispose()

	cnvFont := ebiten.NewImage(len(charsetPhenomena)*16, 33)
	cnvFont2 := ebiten.NewImage(len(charsetPhenomena)*16, 33)
	defer cnvFont.Dispose()
	defer cnvFont2.Dispose()

	cnvFont.Fill(color.RGBA{0, 0, 0, 0})
	cnvFont2.Fill(color.RGBA{0, 0, 0, 0})

	for chr := 0; chr < len(charsetPhenomena); chr++ {
		for x := 0; x < 16; x += 2 {
			op := &ebiten.DrawImageOptions{}
			op.GeoM.Translate(float64(chr*16+x), 7)

			sx := chr*16 + x
			subImg := d.imgFont.SubImage(image.Rect(sx, 0, sx+2, 26)).(*ebiten.Image)
			cnvFont.DrawImage(subImg, op)
		}
	}

	for chr := 0; chr < len(charsetPhenomena); chr++ {
		op := &ebiten.DrawImageOptions{}
		op.GeoM.Scale(-1, -1)
		op.GeoM.Translate(float64((chr+1)*16), 26)

		sx := chr * 16
		subImg := cnvFont.SubImage(image.Rect(sx, 0, sx+16, 33)).(*ebiten.Image)
		cnvFont2.DrawImage(subImg, op)
	}

	d.cnvFrames = ebiten.NewImage(480, len(charsetPhenomena)*33)

	for charIndex := 0; charIndex < len(charsetPhenomena); charIndex++ {
		cnvSilverChars := ebiten.NewImage(480, 33)
		posX := 0
		for posY := 33.0; posY > -33; posY -= 2.25 {
			op := &ebiten.DrawImageOptions{}
			op.GeoM.Translate(float64(posX), posY)

			sx := charIndex * 16
			subImg := cnvFont.SubImage(image.Rect(sx, 0, sx+16, 33)).(*ebiten.Image)
			cnvSilverChars.DrawImage(subImg, op)
			posX += 16
		}

		cnvPurpleChars := ebiten.NewImage(480, 33)
		posX = 0
		for posY := 0.0; posY < 33; posY += 2.25 {
			op := &ebiten.DrawImageOptions{}
			op.GeoM.Translate(float64(posX), posY)

			sx := charIndex * 16
			subImg := cnvFont2.SubImage(image.Rect(sx, 0, sx+16, 33)).(*ebiten.Image)
			cnvPurpleChars.DrawImage(subImg, op)
			posX += 16
		}
		for posY := -33.0; posY < 0; posY += 2.25 {
			op := &ebiten.DrawImageOptions{}
			op.GeoM.Translate(float64(posX), posY)

			sx := charIndex * 16
			subImg := cnvFont2.SubImage(image.Rect(sx, 0, sx+16, 33)).(*ebiten.Image)
			cnvPurpleChars.DrawImage(subImg, op)
			posX += 16
		}

		tmpSilver := ebiten.NewImage(480, 33)
		tmpSilver.DrawImage(cnvSilverBar, nil)
		opSilver := &ebiten.DrawImageOptions{}
		opSilver.CompositeMode = ebiten.CompositeModeDestinationIn
		tmpSilver.DrawImage(cnvSilverChars, opSilver)

		tmpPurple := ebiten.NewImage(480, 33)
		tmpPurple.DrawImage(cnvPurpleBar, nil)
		opPurple := &ebiten.DrawImageOptions{}
		opPurple.CompositeMode = ebiten.CompositeModeDestinationIn
		tmpPurple.DrawImage(cnvPurpleChars, opPurple)

		frameY := charIndex * 33

		op := &ebiten.DrawImageOptions{}
		op.GeoM.Translate(0, float64(frameY))
		d.cnvFrames.DrawImage(tmpPurple, op)

		op.GeoM.Reset()
		op.GeoM.Translate(0, float64(frameY+12))
		d.cnvFrames.DrawImage(cnvRedBar, op)

		op.GeoM.Reset()
		op.GeoM.Translate(0, float64(frameY))
		d.cnvFrames.DrawImage(tmpSilver, op)

		cnvSilverChars.Dispose()
		cnvPurpleChars.Dispose()
		tmpSilver.Dispose()
		tmpPurple.Dispose()
	}

}

func (d *PhenomenaDemo) scrollMessage(speed int) {
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

func (d *PhenomenaDemo) shiftLeft() {
	d.scrollHead++
	if d.scrollHead == len(d.scrollChars) {
		d.scrollHead = 0
	}
}

func (d *PhenomenaDemo) addSliceOfChar(ch byte, slice int) {
	previous := d.scrollHead + len(d.scrollChars) - 2
	if previous >= len(d.scrollChars) {
		previous -= len(d.scrollChars)
	}
	tail := d.scrollHead + len(d.scrollChars) - 1
	if tail >= len(d.scrollChars) {
		tail -= len(d.scrollChars)
	}
	f := d.scrollChars[previous].frame
	glyph, ok := charToFontIndexPhe(rune(ch))
	if !ok {
		glyph = 0
	}

	d.scrollChars[tail] = ScrollChar{
		glyph: uint8(glyph),
		frame: f,
		slice: uint8(slice),
	}
}

func (d *PhenomenaDemo) renderNextFrames(speed float64) {
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

		d.scrollChars[index].frame = uint8(newFrame)
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
	if d.scrollRenderer == nil {
		var err error
		d.scrollRenderer, err = scrolling.FromImages(make([]*ebiten.Image, 240), 2)
		if err != nil {
			panic(err)
		}
		d.scrollBatch = composite.NewQuadBatch(240)
		d.scrollBatch.AlternateDiagonal = true
	}
	d.scrollBatch.Begin(screen, d.cnvFrames)
	const scaleX, scaleY, translateY = float32(1.67), float32(1.875), float32(195)
	t2 := d.t
	waveSin, waveCos := math.Sincos(5*10.50 + d.t/6)
	state := scrolling.IdentityState()
	state.Paint = func(dst *ebiten.Image, s scrolling.Sample, op ebiten.DrawImageOptions) {
		i := s.Index
		index := d.scrollHead + i
		if index >= len(d.scrollChars) {
			index -= len(d.scrollChars)
		}
		ch := d.scrollChars[index]
		ypos := 80.0
		if t2 > 5*50-float64(i)*.0033 {
			ypos = 80 * waveCos
		}
		ci := int(ch.glyph)
		sx, sy := int(ch.frame)*16+int(ch.slice)*2, ci*33
		if ci >= 0 && ci < len(charsetPhenomena) && sx >= 0 && sx <= 478 && sy >= 0 && sy <= len(charsetPhenomena)*33-33 {
			d.scrollBatch.Rect(image.Rect(sx, sy, sx+2, sy+33), float32(i*2)*scaleX, translateY+float32(67+ypos)*scaleY, 2*scaleX, 33*scaleY)
		}
		t2 += 1.0 / 6.0
		waveSin, waveCos = waveSin*phenomenaWaveCosStep+waveCos*phenomenaWaveSinStep, waveCos*phenomenaWaveCosStep-waveSin*phenomenaWaveSinStep
	}
	d.scrollRenderer.DrawAt(screen, state)
	d.scrollBatch.Flush()
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

type ScrollForm struct {
	zSize   float64
	zAmount float64
	zSpeed  float64
	zAdd    float64
	ySize   float64
	yAmount float64
	ySpeed  float64
}

type PrintPos struct {
	x, y, z float64
	letter  byte
}

const tcbScrollShaderSource = `//kage:unit pixels

package main

func Fragment(dstPos vec4, srcPos vec2, custom vec4) vec4 {
	glyph := imageSrc0UnsafeAt(srcPos)
	rasterY := floor(custom.r) + 0.5
	// Coordinates passed to imageSrc1* are expressed in image 0's texture
	// space; Kage converts the origin internally for the second image.
	raster := imageSrc1UnsafeAt(imageSrc0Origin() + vec2(0.5, rasterY))
	return vec4(raster.rgb * glyph.a, raster.a * glyph.a)
}
`

type TCBDemo struct {
	initialized bool

	rasters   *ebiten.Image
	mountains *ebiten.Image
	logo      *ebiten.Image
	font      *ebiten.Image

	logoCenter   *ebiten.Image
	scrollShader *ebiten.Shader

	fontTileRects [128]image.Rectangle
	stripVertices []ebiten.Vertex
	stripIndices  []uint16

	bgSpeed [32]float64
	bgPos   [32]float64

	scrollForms       [8]ScrollForm
	form              int
	scrollX           float64
	scrollText        string
	scrollLetters     []byte
	scrollFormChanges []int8
	addi              int
	sinAdder          float64
	printPos          [30]PrintPos

	logoSin  []float64
	dcounter int
	rotPos   float64
	rotAdd   float64
	next     int
}

func NewTCBDemo() *TCBDemo {
	d := &TCBDemo{
		stripVertices: make([]ebiten.Vertex, 0, 64*4),
		stripIndices:  make([]uint16, 0, 64*6),

		addi:    0,
		rotAdd:  1,
		scrollX: 0,
	}

	d.scrollForms = [8]ScrollForm{
		{0, 0, 0, 0, 55, 0, 0},
		{0, 0, 0, 0, 55, 0, 2},
		{0, 0, 0, 0, 55, 20, 2},
		{200, 0, 0, 5, 55, 20, 2},
		{200, 0, 4, 5, 55, 20, 2},
		{200, -30, 4, 0, 55, 30, 2},
		{200, 40, -4, 5, -70, 40, -4},
		{150, 20, -3, 5, 55, 20, 2},
	}
	speeds := []float64{8, 7.5, 7, 6.5, 6, 5.5, 5, 4.5, 4, 3.5, 3, 2.5, 2, 1.5, 1, 0.5}
	for i, speed := range speeds {
		d.bgSpeed[i] = speed
		d.bgSpeed[i+16] = speed
	}

	d.initLogoSin()
	d.initScrollText()
	d.preprocessScrollText()

	return d
}

func (d *TCBDemo) initLogoSin() {
	d.logoSin = make([]float64, 0, 40+(160*5+4)+(160*5+10)+160)

	for i := 0; i < 40; i++ {
		d.logoSin = append(d.logoSin, 0)
	}

	for i := 0; i < 160*5+4; i++ {
		d.logoSin = append(d.logoSin, 8*math.Sin(float64(i)*0.05-2))
	}

	for i := 0; i < 160*5+10; i++ {
		d.logoSin = append(d.logoSin, 8*math.Sin(float64(i)*0.15))
	}

	for i := 0; i < 160; i++ {
		d.logoSin = append(d.logoSin, 0)
	}
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

func (d *TCBDemo) preprocessScrollText() {
	d.scrollLetters = make([]byte, len(d.scrollText))
	d.scrollFormChanges = make([]int8, len(d.scrollText))
	for i := range d.scrollFormChanges {
		d.scrollFormChanges[i] = -1
	}

	for i := range d.scrollText {
		letter := d.scrollText[i]
		if letter == '^' && i+1 < len(d.scrollText) && d.scrollText[i+1] >= '0' && d.scrollText[i+1] <= '7' {
			d.scrollFormChanges[i] = int8(d.scrollText[i+1] - '0')
			letter = d.scrollText[(i-1+len(d.scrollText))%len(d.scrollText)]
		} else if i >= 2 && d.scrollText[i-1] == '^' && letter >= '0' && letter <= '7' {
			letter = d.scrollText[i-2]
		}
		d.scrollLetters[i] = letter
	}
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
	d.cacheFontTiles()
	d.scrollShader, err = ebiten.NewShader([]byte(tcbScrollShaderSource))
	if err != nil {
		return fmt.Errorf("compile TCB scroller shader: %w", err)
	}

	if d.logo != nil {
		d.logoCenter = d.logo.SubImage(image.Rect(114, 0, 193, 15)).(*ebiten.Image)
	}

	d.initialized = true
	return nil
}

func (d *TCBDemo) cacheFontTiles() {
	charMap := [6][10]rune{
		{0, '!', 0, 0, 0, 0, 0, 0, '(', ')'},
		{0, 0, ',', 0, '.', 0, 0, 0, 0, 0},
		{0, 0, 0, 0, 0, 0, ':', ';', 0, 0},
		{0, 0, 0, 'A', 'B', 'C', 'D', 'E', 'F', 'G'},
		{'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q'},
		{'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 0},
	}

	for row := 0; row < 6; row++ {
		for col := 0; col < 10; col++ {
			ch := charMap[row][col]
			if ch != 0 {
				x := col * 32
				y := row * 33
				d.fontTileRects[ch] = image.Rect(x, y, x+32, y+33)
			}
		}
	}

}

func (d *TCBDemo) Update() error {
	if !d.initialized {
		if err := d.Init(); err != nil {
			return err
		}
	}

	for i := range d.bgPos {
		d.bgPos[i] -= d.bgSpeed[i]
		if d.bgPos[i] <= -256 {
			d.bgPos[i] += 256
		}
	}

	d.dcounter++
	if d.dcounter > len(d.logoSin)-80 {
		d.dcounter = 0
	}

	d.rotPos += d.rotAdd * 0.08
	if d.rotPos > 1 {
		d.rotPos = -1
		d.next++
		if d.next > 1 {
			d.next = 0
		}
	}

	d.scroll3D(4)

	return nil
}

func (d *TCBDemo) scroll3D(scrollspeed float64) {
	d.sinAdder += 0.02
	activeForm := -1
	previousCharIdx := -2
	var zSin, zCos, ySin, yCos float64
	var zStepSin, zStepCos, yStepSin, yStepCos float64

	for i := range d.printPos {
		charIdx := d.addi + i
		if charIdx >= len(d.scrollText) {
			charIdx -= len(d.scrollText)
		}

		letter := d.scrollLetters[charIdx]
		if form := d.scrollFormChanges[charIdx]; form >= 0 {
			d.form = int(form)
		}
		sf := d.scrollForms[d.form]

		if activeForm != d.form || charIdx != previousCharIdx+1 {
			if sf.zSize != 0 {
				zSin, zCos = math.Sincos(sf.zAdd + float64(charIdx)*sf.zAmount*0.01 + d.sinAdder*sf.zSpeed)
				zStepSin, zStepCos = math.Sincos(sf.zAmount * 0.01)
			}
			ySin, yCos = math.Sincos(1.5 + float64(charIdx)*sf.yAmount*0.01 + d.sinAdder*sf.ySpeed)
			yStepSin, yStepCos = math.Sincos(sf.yAmount * 0.01)
			activeForm = d.form
		} else {
			if sf.zSize != 0 {
				zSin, zCos = stepSinCosForward(zSin, zCos, zStepSin, zStepCos)
			}
			ySin, yCos = stepSinCosForward(ySin, yCos, yStepSin, yStepCos)
		}
		previousCharIdx = charIdx

		letterZ := sf.zSize*zSin + 150
		letterY := sf.ySize*yCos - 4

		scale := 250.0 / (250.0 + letterZ)

		letterX := -450.0 + float64(i)*32 - d.scrollX
		x2d := ((letterX - 16) * scale) + 160.0
		y2d := ((letterY - 14) * scale) + 100.0

		d.printPos[i] = PrintPos{x: x2d, y: y2d, z: scale, letter: letter}
	}

	// The list is tiny and fixed-size. Insertion sort avoids the reflection and
	// heap escape caused by sort.Slice.
	for i := 1; i < len(d.printPos); i++ {
		item := d.printPos[i]
		j := i
		for j > 0 && d.printPos[j-1].z > item.z {
			d.printPos[j] = d.printPos[j-1]
			j--
		}
		d.printPos[j] = item
	}

	d.scrollX += scrollspeed

	if d.scrollX >= 32 {
		d.scrollX -= 32
		d.addi++
		if d.addi >= len(d.scrollText) {
			d.addi = 0
		}
	}
}

func (d *TCBDemo) Draw(screen *ebiten.Image) {
	if !d.initialized {
		return
	}

	screen.Fill(color.Black)
	d.stripVertices = d.stripVertices[:0]
	d.stripIndices = d.stripIndices[:0]

	for i := 0; i < 16; i++ {
		xPos := int(d.bgPos[i]) * 2
		yPos := i * 10

		d.stripVertices, d.stripIndices = appendTexturedQuad(
			d.stripVertices, d.stripIndices,
			float32(64+xPos), float32(60+yPos), 1024, 10,
			0, float32(i*10), 1024, 10,
		)
		d.stripVertices, d.stripIndices = appendTexturedQuad(
			d.stripVertices, d.stripIndices,
			float32(64+xPos+640), float32(60+yPos), 1024, 10,
			0, float32(i*10), 1024, 10,
		)
	}

	for i := 16; i < 32; i++ {
		xPos := int(d.bgPos[i]) * 2
		yPos := i*10 + 84

		d.stripVertices, d.stripIndices = appendTexturedQuad(
			d.stripVertices, d.stripIndices,
			float32(64+xPos), float32(60+yPos), 1024, 10,
			0, float32(i*10), 1024, 10,
		)
		d.stripVertices, d.stripIndices = appendTexturedQuad(
			d.stripVertices, d.stripIndices,
			float32(64+xPos+640), float32(60+yPos), 1024, 10,
			0, float32(i*10), 1024, 10,
		)
	}
	if len(d.stripIndices) > 0 {
		// The former 640x400 paper canvas clipped the horizontally scrolling
		// strips to this viewport. Drawing through a destination sub-image keeps
		// that clipping while avoiding both the render target and its full copy.
		mountainViewport := screen.SubImage(image.Rect(64, 60, 704, 460)).(*ebiten.Image)
		mountainViewport.DrawTriangles(d.stripVertices, d.stripIndices, d.mountains, nil)
	}

	d.stripVertices = d.stripVertices[:0]
	d.stripIndices = d.stripIndices[:0]
	for i := 0; i < 32; i++ {
		xOffset := d.logoSin[d.dcounter+i]
		d.stripVertices, d.stripIndices = appendTexturedQuad(
			d.stripVertices, d.stripIndices,
			float32(64+2*(8+xOffset)), float32(60+2*(96+i)), 606, 2,
			0, float32(16+i), 303, 1,
		)
	}
	if len(d.stripIndices) > 0 {
		screen.DrawTriangles(d.stripVertices, d.stripIndices, d.logo, nil)
	}

	if d.logoCenter != nil {
		op := &ebiten.DrawImageOptions{}
		if d.next != 0 {
			op.GeoM.Scale(1, -1)
			op.GeoM.Translate(0, 16)
		}
		op.GeoM.Translate(-40, -8)
		op.GeoM.Scale(1, d.rotPos)
		op.GeoM.Translate(160, 88)
		op.GeoM.Scale(2, 2)
		op.GeoM.Translate(64, 60)
		screen.DrawImage(d.logoCenter, op)
	}

	d.drawScroll3D(screen)
}

func (d *TCBDemo) drawScroll3D(screen *ebiten.Image) {
	d.stripVertices = d.stripVertices[:0]
	d.stripIndices = d.stripIndices[:0]
	for i := 0; i < 30; i++ {
		if d.printPos[i].letter == 0 || d.printPos[i].z <= 0 {
			continue
		}

		ch := rune(d.printPos[i].letter)
		var tileRect image.Rectangle
		if ch >= 0 && ch < rune(len(d.fontTileRects)) {
			tileRect = d.fontTileRects[ch]
		}
		if tileRect.Empty() {
			if ch >= 'a' && ch <= 'z' {
				ch = ch - 'a' + 'A'
				if ch < rune(len(d.fontTileRects)) {
					tileRect = d.fontTileRects[ch]
				}
			}
		}

		if !tileRect.Empty() {
			scale := float32(d.printPos[i].z)
			localY := float32(d.printPos[i].y) - 16.5*scale
			vertexBase := len(d.stripVertices)
			d.stripVertices, d.stripIndices = appendTexturedQuad(
				d.stripVertices, d.stripIndices,
				64+2*(float32(d.printPos[i].x)-16*scale),
				60+2*localY,
				64*scale, 66*scale,
				float32(tileRect.Min.X), float32(tileRect.Min.Y), 32, 33,
			)
			// DrawTrianglesShader exposes vertex colours as arbitrary interpolated
			// values. Carry the old 320x200 destination Y so the fragment shader
			// samples the exact raster scanline that SourceAtop used to apply.
			d.stripVertices[vertexBase].ColorR = localY
			d.stripVertices[vertexBase+1].ColorR = localY
			d.stripVertices[vertexBase+2].ColorR = localY + 33*scale
			d.stripVertices[vertexBase+3].ColorR = localY + 33*scale
		}
	}
	if len(d.stripIndices) > 0 && d.scrollShader != nil {
		scrollViewport := screen.SubImage(image.Rect(64, 60, 704, 460)).(*ebiten.Image)
		op := &ebiten.DrawTrianglesShaderOptions{}
		op.Images[0] = d.font
		op.Images[1] = d.rasters
		scrollViewport.DrawTrianglesShader(d.stripVertices, d.stripIndices, d.scrollShader, op)
	}
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

type Cube3D struct {
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

func (c *Cube3D) Rotate(dx, dy, dz float64) {
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

func (c *Cube3D) ensureTrig() {
	if c.trigReady {
		return
	}
	c.sinX, c.cosX = math.Sincos(c.angleX)
	c.sinY, c.cosY = math.Sincos(c.angleY)
	c.sinZ, c.cosZ = math.Sincos(c.angleZ)
	c.trigReady = true
}

type Letter3 struct {
	x, y  int
	width int
}

type DMASprite3 struct {
	x, y float64
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
	solidImage *ebiten.Image

	letterData [128]Letter3
	fontTiles  [128]*ebiten.Image

	// 3D Cubes
	cubes         [nbCubes3]Cube3D
	spritePos     [nbCubes3]float64
	spritePathSin [nbCubes3]float64
	spritePathCos [nbCubes3]float64
	spriteBobSin  [nbCubes3]float64
	spriteBobCos  [nbCubes3]float64
	cubeVertices  []ebiten.Vertex
	cubeIndices   []uint16

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
	cnt        int
	cnt2       int
	copperSin  []int
	copperBars [10]*ebiten.Image

	// VBL counter
	iteration int
}

func NewCocoDemo() *CocoDemo {
	d := &CocoDemo{
		scrollSurf:     ebiten.NewImage(int(float64(demoWidth)*scrollSurfWidthFactor3), int(float64(fontHeight3)*scrollScaleFactor3)),
		solidImage:     ebiten.NewImage(3, 3),
		cubeVertices:   make([]ebiten.Vertex, 0, nbCubes3*len(cubeFaces3)*20),
		cubeIndices:    make([]uint16, 0, nbCubes3*len(cubeFaces3)*30),
		scrollVertices: make([]ebiten.Vertex, 0, ((demoHeight-72)/scrollScaleInt3)*8),
		scrollIndices:  make([]uint16, 0, ((demoHeight-72)/scrollScaleInt3)*12),
		logoX:          0.5,
		lastTextOffset: -1,
		scrollText:     cocoScrollText3,
	}
	d.solidImage.Fill(color.White)

	// Init 3D cubes
	for i := 0; i < nbCubes3; i++ {
		d.cubes[i] = Cube3D{
			angleX: float64(i) * 0.3,
			angleY: float64(i) * 0.2,
			angleZ: float64(i) * 0.1,
			size:   40,
		}
		d.cubes[i].ensureTrig()
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

	// Init copper bars sine table
	d.initCopperSin()

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
		for i := range d.copperBars {
			y := i * 2
			d.copperBars[i] = d.barsImg.SubImage(image.Rect(0, y, d.barsImg.Bounds().Dx(), y+2)).(*ebiten.Image)
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
	data := [...]struct {
		char  byte
		x, y  int
		width int
	}{
		{' ', 0, 0, 32}, {'!', 48, 0, 16}, {'"', 96, 0, 32},
		{'\'', 336, 0, 16}, {'(', 384, 0, 32}, {')', 432, 0, 32},
		{'+', 48, 36, 48}, {',', 96, 36, 16}, {'-', 144, 36, 32},
		{'.', 192, 36, 16}, {'0', 288, 36, 48}, {'1', 336, 36, 48},
		{'2', 384, 36, 48}, {'3', 432, 36, 48}, {'4', 0, 72, 48},
		{'5', 48, 72, 48}, {'6', 96, 72, 48}, {'7', 144, 72, 48},
		{'8', 192, 72, 48}, {'9', 240, 72, 48}, {':', 288, 72, 16},
		{';', 336, 72, 16}, {'<', 384, 72, 32}, {'=', 432, 72, 32},
		{'>', 0, 108, 32}, {'?', 48, 108, 48}, {'A', 144, 108, 48},
		{'B', 192, 108, 48}, {'C', 240, 108, 48}, {'D', 288, 108, 48},
		{'E', 336, 108, 48}, {'F', 384, 108, 48}, {'G', 432, 108, 48},
		{'H', 0, 144, 48}, {'I', 48, 144, 16}, {'J', 96, 144, 48},
		{'K', 144, 144, 48}, {'L', 192, 144, 48}, {'M', 240, 144, 48},
		{'N', 288, 144, 48}, {'O', 336, 144, 48}, {'P', 384, 144, 48},
		{'Q', 432, 144, 48}, {'R', 0, 180, 48}, {'S', 48, 180, 48},
		{'T', 96, 180, 48}, {'U', 144, 180, 48}, {'V', 192, 180, 48},
		{'W', 240, 180, 48}, {'X', 288, 180, 48}, {'Y', 336, 180, 48},
		{'Z', 384, 180, 48},
	}

	for _, dd := range data {
		d.letterData[dd.char] = Letter3{x: dd.x, y: dd.y, width: dd.width}
		if d.fontImg != nil {
			d.fontTiles[dd.char] = d.fontImg.SubImage(
				image.Rect(dd.x, dd.y, dd.x+dd.width, dd.y+fontHeight3),
			).(*ebiten.Image)
		}
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
	d.cnt = (d.cnt + 3) & 0x3ff
	d.cnt2 = (d.cnt2 - 5) & 0x3ff

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
			letter := d.letterData[r]
			if letter.width > 0 {
				glyphs[i] = scrolling.Glyph{Image: d.fontTiles[r], Advance: float64(letter.width)}
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
	d.cubeVertices = d.cubeVertices[:0]
	d.cubeIndices = d.cubeIndices[:0]
	for i := 0; i < nbCubes3; i++ {
		xPos := float64((demoWidth-40)/2) + float64((demoWidth-40)/2)*d.spritePathSin[i]
		yPos := float64(demoHeight)/2 + 84*d.spriteBobCos[i]
		d.cubeVertices, d.cubeIndices = d.cubes[i].appendGeometry(d.cubeVertices, d.cubeIndices, xPos, yPos)
	}
	if len(d.cubeIndices) > 0 {
		dst.DrawTriangles(d.cubeVertices, d.cubeIndices, d.solidImage, nil)
	}
}

func (c *Cube3D) appendGeometry(vertices []ebiten.Vertex, indices []uint16, centerX, centerY float64) ([]ebiten.Vertex, []uint16) {
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

func (d *CocoDemo) drawTitleWithCopperbars3(dst *ebiten.Image) {
	if d.titleImg == nil {
		return
	}

	vector.DrawFilledRect(dst, 0, 0, demoWidth, 72, color.Black, false)
	d.drawCopperBars3(dst)

	titleX := 64 + float64(demoWidth)*math.Cos(d.logoX)
	titleH := float64(d.titleImg.Bounds().Dy())
	scaleY := 72.0 / titleH

	op := &ebiten.DrawImageOptions{}
	op.GeoM.Scale(1.0, scaleY)
	op.GeoM.Translate(titleX, 0)
	dst.DrawImage(d.titleImg, op)
}

func (d *CocoDemo) drawCopperBars3(dst *ebiten.Image) {
	if d.barsImg == nil {
		return
	}

	if d.copperBars[0] == nil {
		return
	}

	cc := 0
	for i := 0; i < 36; i++ {
		val2 := (d.cnt + i*7) & 0x3ff
		val := d.copperSin[val2]
		val2 = (d.cnt2 + i*10) & 0x3ff
		val += d.copperSin[val2]
		val += 60

		xPos := val >> 1
		yPos := i << 1
		height := 72 - yPos

		if height > 0 && yPos < 72 {
			op := &ebiten.DrawImageOptions{}
			scaleY := float64(height) / 2.0
			op.GeoM.Scale(1, scaleY)
			op.GeoM.Translate(float64(xPos), float64(yPos))
			dst.DrawImage(d.copperBars[cc/2], op)
		}

		cc += 2
		if cc >= 20 {
			cc = 0
		}
	}
}

func (d *CocoDemo) initCopperSin() {
	d.copperSin = []int{
		264, 264, 268, 272, 276, 280, 280, 284, 288, 292, 296, 296, 300, 304, 308, 312, 312, 316, 320, 324, 328, 328, 332, 336, 340, 340, 344, 348, 352, 352, 356, 360, 364, 364, 368, 372, 376, 376, 380, 384, 388, 388, 392, 396, 396, 400, 404, 404, 408, 412, 412, 416, 420, 420, 424, 428, 428, 432, 436, 436, 440, 440, 444, 448, 448, 452, 452, 456, 456, 460, 460, 464, 464, 468, 472, 472, 472, 476, 476, 480, 480, 484, 484, 488, 488, 488, 492, 492, 496, 496, 496, 500, 500, 500, 504, 504, 504, 508, 508, 508, 512, 512, 512, 512, 516, 516, 516, 516, 520, 520, 520, 520, 520, 520, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 520, 520, 520, 520, 520, 520, 516, 516, 516, 516, 512, 512, 512, 512, 508, 508, 508, 508, 504, 504, 504, 500, 500, 500, 496, 496, 492, 492, 492, 488, 488, 484, 484, 480, 480, 480, 476, 476, 472, 472, 468, 468, 464, 464, 460, 456, 456, 452, 452, 448, 448, 444, 444, 440, 436, 436, 432, 428, 428, 424, 424, 420, 416, 416, 412, 408, 408, 404, 400, 400, 396, 392, 388, 388, 384, 380, 380, 376, 372, 368, 368, 364, 360, 356, 356, 352, 348, 344, 344, 340, 336, 332, 328, 328, 324, 320, 316, 316, 312, 308, 304, 300, 300, 296, 292, 288, 284, 284, 280, 276, 272, 268, 264, 264, 264, 260, 256, 252, 252, 248, 244, 240, 236, 236, 232, 228, 224, 220, 220, 216, 212, 208, 204, 204, 200, 196, 192, 192, 188, 184, 180, 176, 176, 172, 168, 164, 164, 160, 156, 152, 152, 148, 144, 144, 140, 136, 132, 132, 128, 124, 124, 120, 116, 116, 112, 108, 108, 104, 100, 100, 96, 96, 92, 88, 88, 84, 84, 80, 76, 76, 72, 72, 68, 68, 64, 64, 60, 60, 56, 56, 52, 52, 48, 48, 44, 44, 40, 40, 40, 36, 36, 32, 32, 32, 28, 28, 28, 24, 24, 24, 20, 20, 20, 16, 16, 16, 16, 12, 12, 12, 12, 12, 8, 8, 8, 8, 8, 8, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 8, 8, 8, 8, 8, 8, 12, 12, 12, 12, 12, 16, 16, 16, 20, 20, 20, 20, 24, 24, 24, 28, 28, 28, 32, 32, 36, 36, 36, 40, 40, 44, 44, 44, 48, 48, 52, 52, 56, 56, 60, 60, 64, 64, 68, 68, 72, 72, 76, 80, 80, 84, 84, 88, 92, 92, 96, 96, 100, 104, 104, 108, 112, 112, 116, 120, 120, 124, 128, 128, 132, 136, 136, 140, 144, 148, 148, 152, 156, 156, 160, 164, 168, 168, 172, 176, 180, 180, 184, 188, 192, 196, 196, 200, 204, 208, 212, 212, 216, 220, 224, 224, 228, 232, 236, 240, 244, 244, 248, 252, 256, 260, 260, 264, 264, 268, 272, 276, 280, 280, 284, 288, 292, 296, 296, 300, 304, 308, 312, 312, 316, 320, 324, 328, 328, 332, 336, 340, 340, 344, 348, 352, 352, 356, 360, 364, 364, 368, 372, 376, 376, 380, 384, 388, 388, 392, 396, 396, 400, 404, 404, 408, 412, 412, 416, 420, 420, 424, 428, 428, 432, 436, 436, 440, 440, 444, 448, 448, 452, 452, 456, 456, 460, 460, 464, 464, 468, 472, 472, 472, 476, 476, 480, 480, 484, 484, 488, 488, 488, 492, 492, 496, 496, 496, 500, 500, 500, 504, 504, 504, 508, 508, 508, 512, 512, 512, 512, 516, 516, 516, 516, 520, 520, 520, 520, 520, 520, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 520, 520, 520, 520, 520, 520, 516, 516, 516, 516, 512, 512, 512, 512, 508, 508, 508, 508, 504, 504, 504, 500, 500, 500, 496, 496, 492, 492, 492, 488, 488, 484, 484, 480, 480, 480, 476, 476, 472, 472, 468, 468, 464, 464, 460, 456, 456, 452, 452, 448, 448, 444, 444, 440, 436, 436, 432, 428, 428, 424, 424, 420, 416, 416, 412, 408, 408, 404, 400, 400, 396, 392, 388, 388, 384, 380, 380, 376, 372, 368, 368, 364, 360, 356, 356, 352, 348, 344, 344, 340, 336, 332, 328, 328, 324, 320, 316, 316, 312, 308, 304, 300, 300, 296, 292, 288, 284, 284, 280, 276, 272, 268, 264, 264, 264, 260, 256, 252, 252, 248, 244, 240, 236, 236, 232, 228, 224, 220, 220, 216, 212, 208, 204, 204, 200, 196, 192, 192, 188, 184, 180, 176, 176, 172, 168, 164, 164, 160, 156, 152, 152, 148, 144, 144, 140, 136, 132, 132, 128, 124, 124, 120, 116, 116, 112, 108, 108, 104, 100, 100, 96, 96, 92, 88, 88, 84, 84, 80, 76, 76, 72, 72, 68, 68, 64, 64, 60, 60, 56, 56, 52, 52, 48, 48, 44, 44, 40, 40, 40, 36, 36, 32, 32, 32, 28, 28, 28, 24, 24, 24, 20, 20, 20, 16, 16, 16, 16, 12, 12, 12, 12, 12, 8, 8, 8, 8, 8, 8, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 8, 8, 8, 8, 8, 8, 12, 12, 12, 12, 12, 16, 16, 16, 20, 20, 20, 20, 24, 24, 24, 28, 28, 28, 32, 32, 36, 36, 36, 40, 40, 44, 44, 44, 48, 48, 52, 52, 56, 56, 60, 60, 64, 64, 68, 68, 72, 72, 76, 80, 80, 84, 84, 88, 92, 92, 96, 96, 100, 104, 104, 108, 112, 112, 116, 120, 120, 124, 128, 128, 132, 136, 136, 140, 144, 148, 148, 152, 156, 156, 160, 164, 168, 168, 172, 176, 180, 180, 184, 188, 192, 196, 196, 200, 204, 208, 212, 212, 216, 220, 224, 224, 228, 232, 236, 240, 244, 244, 248, 252, 256, 260, 260,
	}
}

func (d *CocoDemo) createCurves() {
	for funcType := 0; funcType <= 7; funcType++ {
		var step, progress float64

		switch funcType {
		case 0:
			step, progress = 2.25, 0
		case 1:
			step, progress = 0.20, 140
		case 2:
			step, progress = 0.25, 175
		case 3:
			step, progress = 0.30, 210
		case 4:
			step, progress = 0.12, 175
		case 5:
			step, progress = 0.16, 210
		case 6:
			step, progress = 0.20, 245
		case 7:
			step, progress = 0.18, 0
		}

		local := []float64{}
		decal := 0.0
		previous := 0
		maxAngle := 360.0
		if funcType == 7 {
			maxAngle = 720.0
		}

		for i := 0.0; i < maxAngle-step; i += step {
			val := 0.0
			rad := i * math.Pi / 180

			switch funcType {
			case 0:
				val = 0
			case 1:
				val = 100 * math.Sin(rad)
			case 2:
				val = 110 * math.Sin(rad)
			case 3:
				val = 120 * math.Sin(rad)
			case 4:
				val = 100*math.Sin(rad) + 25.0*math.Sin(rad*10)
			case 5:
				val = 110*math.Sin(rad) + 27.5*math.Sin(rad*9)
			case 6:
				val = 120*math.Sin(rad) + 30.0*math.Sin(rad*8)
			case 7:
				dir := 1.0
				if len(local)%2 == 1 {
					dir = -1.0
				}
				amp := 12.0
				if i < 160 {
					amp *= i / 160
				} else if (720 - 160) < i {
					amp *= (720 - i) / 160
				}
				val = 90*math.Sin(rad) + dir*amp*math.Sin(rad*3)
			}
			local = append(local, val)
		}

		d.curves[funcType] = make([]int, len(local))
		for i := 0; i < len(local); i++ {
			nitem := -int(math.Floor(local[i] - decal))
			d.curves[funcType][i] = nitem - previous
			previous = nitem
			decal += progress / float64(len(local))
		}
	}
}

func (d *CocoDemo) precalcPosition() {
	count := 0
	d.position = make([]int, 0, len(d.scrollText))

	for i := 0; i < len(d.scrollText); i++ {
		if letter := d.letterData[d.scrollText[i]]; letter.width > 0 {
			count += int(float64(letter.width) * scrollScaleFactor3)
			d.position = append(d.position, count)
		}
	}
}

func (d *CocoDemo) precalcMainWave() {
	frontMainWaveTable := []int{
		1, 1, 4, 1, 1, 2, 3, 2, 1, 5, 2, 1, 7,
	}

	count := 0
	d.frontMainWave = []int{}

	for _, waveType := range frontMainWaveTable {
		wave := d.curves[waveType]
		for _, val := range wave {
			count += val
			d.frontMainWave = append(d.frontMainWave, count)
		}
	}
}

func (d *CocoDemo) getSum3(arr []int, index, decal int) int {
	n := len(arr)
	if n == 0 {
		return decal
	}

	maxVal := arr[n-1]
	f := index / n
	m := index % n
	return decal + f*maxVal + arr[m]
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

var (
	vivaZSinStep, vivaZCosStep           = math.Sincos(0.75)
	vivaXSinStep, vivaXCosStep           = math.Sincos(18)
	vivaYSinStep, vivaYCosStep           = math.Sincos(0.7)
	vivaLogoXSinStep, vivaLogoXCosStep   = math.Sincos(0.2)
	vivaLogoX2SinStep, vivaLogoX2CosStep = math.Sincos(1.0 / 60.0)
	vivaLogoYSinStep, vivaLogoYCosStep   = math.Sincos(5.0 / 37.0)
	vivaLogoY2SinStep, vivaLogoY2CosStep = math.Sincos(5.0 / 17.0)
)

type VivaDemo struct {
	scrollPrograms [4]*scrolling.Scrolling
	initialized    bool

	logoImg   *ebiten.Image
	titleImg  *ebiten.Image
	rasterImg *ebiten.Image
	tileImg   *ebiten.Image
	fontImg   *ebiten.Image
	fontTiles [59]*ebiten.Image

	logoX    float64
	rasterY1 float64
	rasterY2 float64

	scrollX1 float64
	scrollX2 float64
	scrollX3 float64
	scrollX4 float64

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
	d := &VivaDemo{
		logoX:    1.5,
		rasterY1: 0,
		rasterY2: 72,
	}

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

	img, _, err := image.Decode(bytes.NewReader(demo4LogoData))
	if err != nil {
		log.Printf("Error loading logo: %v", err)
	} else {
		d.logoImg = ebiten.NewImageFromImage(img)
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
		for fontIndex := range d.fontTiles {
			const cols = 10
			srcX := (fontIndex % cols) * fontCharWidth4
			srcY := (fontIndex / cols) * fontCharHeight4
			if srcX+fontCharWidth4 <= d.fontImg.Bounds().Dx() && srcY+fontCharHeight4 <= d.fontImg.Bounds().Dy() {
				d.fontTiles[fontIndex] = d.fontImg.SubImage(
					image.Rect(srcX, srcY, srcX+fontCharWidth4, srcY+fontCharHeight4),
				).(*ebiten.Image)
			}
		}
	}

	d.initialized = true
	return nil
}

func mapCharToFont4(charCode int) int {
	if charCode == ' ' {
		return 0
	}

	if charCode >= 'a' && charCode <= 'z' {
		charCode = charCode - 32
	}

	switch {
	case charCode >= 33 && charCode <= 64:
		return charCode - 32
	case charCode >= 65 && charCode <= 90:
		return (charCode - 65) + 33
	default:
		return 0
	}
}

func stepSinCosBackward(sinValue, cosValue, sinStep, cosStep float64) (float64, float64) {
	return sinValue*cosStep - cosValue*sinStep, cosValue*cosStep + sinValue*sinStep
}

func stepSinCosForward(sinValue, cosValue, sinStep, cosStep float64) (float64, float64) {
	return sinValue*cosStep + cosValue*sinStep, cosValue*cosStep - sinValue*sinStep
}

func (d *VivaDemo) drawScroller(dst *ebiten.Image, text []rune, scrollX float64, scrollerID int, baseY, t, horizontalWave, verticalWave float64) {
	if d.fontImg == nil || len(text) == 0 {
		return
	}
	program := d.scrollPrograms[scrollerID-1]
	if program == nil {
		images := make([]*ebiten.Image, len(text))
		for i, r := range text {
			index := mapCharToFont4(int(r))
			if index >= 0 && index < len(d.fontTiles) {
				images[i] = d.fontTiles[index]
			}
		}
		var err error
		program, err = scrolling.FromImages(images, 64)
		if err != nil {
			panic(err)
		}
		d.scrollPrograms[scrollerID-1] = program
	}
	first := int(scrollX / 64)
	last := first + 8
	zs, zc := math.Sincos((t + float64(last)*.15) * 5)
	xs, xc := math.Sincos(t*7 + float64(last)*18)
	ys, yc := math.Sincos((t + float64(last)*.1) * 7)
	advance := func() {
		zs, zc = stepSinCosBackward(zs, zc, vivaZSinStep, vivaZCosStep)
		xs, xc = stepSinCosBackward(xs, xc, vivaXSinStep, vivaXCosStep)
		ys, yc = stepSinCosBackward(ys, yc, vivaYSinStep, vivaYCosStep)
	}
	for i := last; i >= len(text); i-- {
		advance()
	}
	state := scrolling.IdentityState()
	state.First = first
	state.End = last + 1
	state.Reverse = true
	state.Map = func(s scrolling.Sample, op *ebiten.DrawImageOptions) bool {
		z, xsin, ysin := zs*.5+1.5, xs, ys
		advance()
		x := math.Floor((float64(s.Index)*64 - 40 - xsin*32*horizontalWave - scrollX) * 2)
		y := math.Floor(ysin*42*verticalWave + baseY - z*32)
		scale := z
		if scrollerID == 1 || scrollerID == 2 {
			scale = 3 - z
		}
		if x < -100 || x > float64(demoWidth)+100 || y < -100 || y > float64(demoHeight)+100 || scale <= .1 {
			return false
		}
		op.GeoM.Reset()
		op.GeoM.Scale(scale, scale)
		op.GeoM.Translate(x, y)
		op.ColorScale.Scale(1, 1, 1, .9)
		return true
	}
	program.DrawAt(dst, state)
}

func advanceScroller4(scrollX float64, text []rune) float64 {
	if len(text) == 0 {
		return 0
	}
	scrollX += 4
	if limit := float64(len(text) * 64); scrollX >= limit {
		scrollX -= limit
	}
	return scrollX
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
	d.logoX += 0.0125

	// Raster animation
	d.rasterY1 -= 2
	d.rasterY2 -= 2
	if d.rasterY1 <= -72 {
		d.rasterY1 = 72
	}
	if d.rasterY2 <= -72 {
		d.rasterY2 = 72
	}

	d.loopCounter++
	d.scrollX1 = advanceScroller4(d.scrollX1, d.text1)
	d.scrollX2 = advanceScroller4(d.scrollX2, d.text2)
	d.scrollX3 = advanceScroller4(d.scrollX3, d.text3)
	d.scrollX4 = advanceScroller4(d.scrollX4, d.text4)

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

	t := float64(d.loopCounter)/60 + 19
	wave := math.Sin(t*0.25)*0.5 + 0.5
	horizontalWave := math.Sqrt(1 - wave*wave)
	verticalWave := math.Sin(t*0.5)*0.5 + 0.5
	d.drawScroller(screen, d.text1, d.scrollX1, 1, 500, t, horizontalWave, verticalWave)
	d.drawScroller(screen, d.text2, d.scrollX2, 2, 250, t, horizontalWave, verticalWave)
	d.drawScroller(screen, d.text3, d.scrollX3, 3, 375, t, horizontalWave, verticalWave)
	d.drawScroller(screen, d.text4, d.scrollX4, 4, 125, t, horizontalWave, verticalWave)

	// Black bar at top
	vector.DrawFilledRect(screen, 0, 0, demoWidth, 72, color.Black, false)

	// Draw the moving title directly. A fixed destination sub-image clips the
	// vertically tiled raster exactly like the former 528x36 render target,
	// without forcing an extra render pass and a full-canvas copy.
	titleX := 64 + 800*math.Cos(d.logoX)
	vector.DrawFilledRect(screen, float32(titleX), 14, demoWidth, 72, color.Black, false)
	titleViewport := screen.SubImage(image.Rect(0, 14, demoWidth, 86)).(*ebiten.Image)
	if d.rasterImg != nil {
		rasterScaleX := float64(demoWidth) / float64(d.rasterImg.Bounds().Dx())
		for _, rasterY := range [...]float64{d.rasterY1, d.rasterY2, d.rasterY2 + 72} {
			op := &ebiten.DrawImageOptions{}
			op.GeoM.Scale(rasterScaleX, 2)
			op.GeoM.Translate(titleX, 14+2*rasterY)
			titleViewport.DrawImage(d.rasterImg, op)
		}
	}
	if d.titleImg != nil {
		titleOp := &ebiten.DrawImageOptions{}
		titleOp.GeoM.Scale(float64(demoWidth)/float64(d.titleImg.Bounds().Dx()), 2)
		titleOp.GeoM.Translate(titleX, 14)
		titleViewport.DrawImage(d.titleImg, titleOp)
	}

	// Draw animated logos directly; the old 400x300 canvas was immediately
	// scaled to the same 800x600 destination.
	if d.logoImg != nil {
		midX := 200.0 - 16
		midY := 24.0 + 150.0 - 16
		incY := 150.0 / 4
		base := float64(d.loopCounter)
		xSin, xCos := math.Sincos(base / 25)
		x2Sin, x2Cos := math.Sincos(base / 300)
		ySin, yCos := math.Sincos(base / 37)
		y2Sin, y2Cos := math.Sincos(base / 17)

		for s := 0; s < 10; s++ {
			spX := midX + midX*xSin*x2Cos
			spY := midY + incY*ySin + incY*y2Cos

			op := &ebiten.DrawImageOptions{}
			op.GeoM.Scale(2, 2)
			op.GeoM.Translate(spX*2, spY*2)
			screen.DrawImage(d.logoImg, op)

			xSin, xCos = stepSinCosForward(xSin, xCos, vivaLogoXSinStep, vivaLogoXCosStep)
			x2Sin, x2Cos = stepSinCosForward(x2Sin, x2Cos, vivaLogoX2SinStep, vivaLogoX2CosStep)
			ySin, yCos = stepSinCosForward(ySin, yCos, vivaLogoYSinStep, vivaLogoYCosStep)
			y2Sin, y2Cos = stepSinCosForward(y2Sin, y2Cos, vivaLogoY2SinStep, vivaLogoY2CosStep)
		}
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
	ymPlayer     *YMPlayer

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
	g.ymPlayer, err = NewYMPlayer(musicData, sampleRate, true)
	if err != nil {
		log.Printf("Failed to create YM player: %v", err)
	} else {
		g.audioPlayer, err = g.audioContext.NewPlayer(g.ymPlayer)
		if err != nil {
			log.Printf("Failed to create audio player: %v", err)
			g.ymPlayer.Close()
			g.ymPlayer = nil
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
