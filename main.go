package main

import (
	"bytes"
	_ "embed"
	"fmt"
	"image"
	"image/color"
	_ "image/png"
	"io"
	"log"
	"math"
	"sort"
	"sync"
	"time"

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

//go:embed assets/music.ym
var musicData []byte

// Camera states
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
	player       *stsound.StSound
	sampleRate   int
	buffer       []int16
	mutex        sync.Mutex
	position     int64
	totalSamples int64
	loop         bool
	volume       float64
}

func NewYMPlayer(data []byte, sampleRate int, loop bool) (*YMPlayer, error) {
	player := stsound.CreateWithRate(sampleRate)

	if err := player.LoadMemory(data); err != nil {
		player.Destroy()
		return nil, fmt.Errorf("failed to load YM data: %w", err)
	}

	player.SetLoopMode(loop)

	info := player.GetInfo()
	totalSamples := int64(info.MusicTimeInMs) * int64(sampleRate) / 1000

	return &YMPlayer{
		player:       player,
		sampleRate:   sampleRate,
		buffer:       make([]int16, 4096),
		totalSamples: totalSamples,
		loop:         loop,
		volume:       0.5,
	}, nil
}

func (y *YMPlayer) Read(p []byte) (n int, err error) {
	y.mutex.Lock()
	defer y.mutex.Unlock()

	samplesNeeded := len(p) / 4
	outBuffer := make([]int16, samplesNeeded*2)

	processed := 0
	for processed < samplesNeeded {
		chunkSize := samplesNeeded - processed
		if chunkSize > len(y.buffer) {
			chunkSize = len(y.buffer)
		}

		if !y.player.Compute(y.buffer[:chunkSize], chunkSize) {
			if !y.loop {
				for i := processed * 2; i < len(outBuffer); i++ {
					outBuffer[i] = 0
				}
				err = io.EOF
				break
			}
		}

		for i := 0; i < chunkSize; i++ {
			sample := int16(float64(y.buffer[i]) * y.volume)
			outBuffer[(processed+i)*2] = sample
			outBuffer[(processed+i)*2+1] = sample
		}

		processed += chunkSize
		y.position += int64(chunkSize)
	}

	buf := make([]byte, 0, len(outBuffer)*2)
	for _, sample := range outBuffer {
		buf = append(buf, byte(sample), byte(sample>>8))
	}

	copy(p, buf)
	n = len(buf)
	if n > len(p) {
		n = len(p)
	}

	return n, err
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
	return 1 - math.Pow(-2*t+2, 3)/2
}

// ==================== PHENOMENA DEMO (Demo1) ====================

//go:embed assets/phenomena/rasterbar.png
var demo1RasterbarData []byte

//go:embed assets/phenomena/font.png
var demo1FontData []byte

//go:embed assets/phenomena/logo.png
var demo1LogoData []byte

//go:embed assets/phenomena/photon.png
var demo1PhotonData []byte

type PhenomenaDemo struct {
	// Demo state
	state       int
	initialized bool

	// Images
	imgRasterbar *ebiten.Image
	imgFont      *ebiten.Image
	imgLogo      *ebiten.Image
	imgPhoton    *ebiten.Image
	imgTextPage1 *ebiten.Image
	imgTextPage2 *ebiten.Image

	// Animation canvases
	cnvFrames    *ebiten.Image
	cnvScroller  *ebiten.Image
	cnvPhoton    *ebiten.Image
	cnvLogoWhite *ebiten.Image

	// Animation variables
	t                float64
	msgIndex         int
	sliceCount       int
	pause            bool
	pauseTime        int
	scrollSpeed      float64
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
	scrollChars []ScrollChar
}

type ScrollChar struct {
	char  string
	frame int
	slice int
}

type GradientStop struct {
	Color  color.Color
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

var charsetPhenomena = []string{
	" ", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K",
	"L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V",
	"W", "X", "Y", "Z", "0", "1", "2", "3", "4", "5", "6",
	"7", "8", "9", "!", "'", "?", "/", ",", ".", "-", "@",
}

var ctrlChars = []string{"^", "#", "&", "%"}

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
		scrollSpeed:      1.0,
		blackRectWidth:   800,
		blackRectShow:    false, // No black rect at start
		photonY:          184,
		photonBounce:     -9.50,
		rasterbarY:       -40,
		direction:        1,
		scrollerRotation: 0,
		scrollChars:      make([]ScrollChar, 240),
	}

	for i := range d.scrollChars {
		d.scrollChars[i] = ScrollChar{char: " ", frame: 0, slice: 0}
	}

	return d
}

func (d *PhenomenaDemo) Init() error {
	if d.initialized {
		return nil
	}

	var err error

	// Load images
	img, _, err := image.Decode(bytes.NewReader(demo1RasterbarData))
	if err != nil {
		return err
	}
	d.imgRasterbar = ebiten.NewImageFromImage(img)

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

	// Init text pages
	d.initTextPages()

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
	charStr := string(ch)

	if ch >= 'a' && ch <= 'z' {
		charStr = string(ch - 32)
	}

	for i, c := range charsetPhenomena {
		if c == charStr {
			return i, true
		}
	}

	return 0, false
}

func createGradient(width, height int, stops []GradientStop) *ebiten.Image {
	img := image.NewRGBA(image.Rect(0, 0, width, height))

	for y := 0; y < height; y++ {
		t := float64(y) / float64(height-1)

		var c color.Color
		for i := 0; i < len(stops)-1; i++ {
			if t >= stops[i].Offset && t <= stops[i+1].Offset {
				localT := (t - stops[i].Offset) / (stops[i+1].Offset - stops[i].Offset)
				c = lerpColor(stops[i].Color, stops[i+1].Color, localT)
				break
			}
		}

		for x := 0; x < width; x++ {
			img.Set(x, y, c)
		}
	}

	return ebiten.NewImageFromImage(img)
}

func lerpColor(c1, c2 color.Color, t float64) color.Color {
	r1, g1, b1, a1 := c1.RGBA()
	r2, g2, b2, a2 := c2.RGBA()

	r := uint8((float64(r1>>8)*(1-t) + float64(r2>>8)*t))
	g := uint8((float64(g1>>8)*(1-t) + float64(g2>>8)*t))
	b := uint8((float64(b1>>8)*(1-t) + float64(b2>>8)*t))
	a := uint8((float64(a1>>8)*(1-t) + float64(a2>>8)*t))

	return color.RGBA{r, g, b, a}
}

func (d *PhenomenaDemo) initCharacterFrames() {
	cnvRedBar := createGradient(480, 9, gdcRedBar)
	cnvSilverBar := createGradient(480, 33, gdcSilverBar)
	cnvPurpleBar := createGradient(480, 33, gdcPurpleBar)

	cnvFont := ebiten.NewImage(len(charsetPhenomena)*16, 33)
	cnvFont2 := ebiten.NewImage(len(charsetPhenomena)*16, 33)

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
	}

	d.cnvScroller = ebiten.NewImage(480, 180)
	d.cnvPhoton = ebiten.NewImage(70, 15)
	d.cnvLogoWhite = ebiten.NewImage(640, 129)
}

func (d *PhenomenaDemo) scrollMessage(speed float64) {
	for i := 0; i < int(speed); i++ {
		chStr := string(scrollMessage[d.msgIndex])

		isCtrl := false
		for _, ctrl := range ctrlChars {
			if chStr == ctrl {
				isCtrl = true
				break
			}
		}

		if isCtrl && d.sliceCount == 0 {
			switch chStr {
			case "^":
				d.pause = true
				d.pauseTime = 275
				d.rotSpeed = -1
			case "&":
				d.pause = true
				d.pauseTime = 275
				d.rotSpeed = 1
			case "#":
				d.pause = true
				d.pauseTime = 250
				d.rotSpeed = -1
			case "%":
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
			d.addSliceOfChar(chStr, d.sliceCount)

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
	for i := 0; i < len(d.scrollChars)-1; i++ {
		d.scrollChars[i] = d.scrollChars[i+1]
	}
}

func (d *PhenomenaDemo) addSliceOfChar(ch string, slice int) {
	f := d.scrollChars[len(d.scrollChars)-2].frame

	d.scrollChars[len(d.scrollChars)-1] = ScrollChar{
		char:  ch,
		frame: f,
		slice: slice,
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
		sineOffset := math.Sin(float64(i)*0.05) * 15

		newFrame := d.scrollerRotation + sineOffset

		for newFrame >= 30 {
			newFrame -= 30
		}
		for newFrame < 0 {
			newFrame += 30
		}

		d.scrollChars[i].frame = int(newFrame)
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
		screen.Fill(color.Black)
		screen.Fill(color.RGBA{0x00, 0x01, 0x11, 0xFF})

		if d.percent <= 100 {
			op := &ebiten.DrawImageOptions{}
			brightness := d.percent / 100.0
			op.ColorM.Scale(brightness, brightness, brightness, 1)
			screen.DrawImage(d.imgLogo, op)
		} else {
			screen.DrawImage(d.imgLogo, nil)

			d.cnvLogoWhite.Clear()
			d.cnvLogoWhite.DrawImage(d.imgLogo, nil)
			op := &ebiten.DrawImageOptions{}
			op.ColorM.Scale(1, 1, 1, 1)
			op.ColorM.Translate(1, 1, 1, 0)
			alpha := (200 - d.percent) / 100.0
			op.ColorM.Scale(1, 1, 1, alpha)
			screen.DrawImage(d.cnvLogoWhite, op)
		}

	case StateShowUpperRasterbarPhe, StateShowLowerRasterbarPhe, StateDropPhotonPhe, StatePhotonFadeToRedPhe:
		screen.Fill(color.Black)
		for y := 130; y < 430; y++ {
			for x := 0; x < 640; x++ {
				screen.Set(x, y, color.RGBA{0x00, 0x01, 0x11, 0xFF})
			}
		}

		screen.DrawImage(d.imgLogo, nil)

		if d.state >= StateShowUpperRasterbarPhe {
			alpha := 1.0
			if d.state == StateShowUpperRasterbarPhe {
				alpha = d.percent / 100.0
			}
			op := &ebiten.DrawImageOptions{}
			op.ColorM.Scale(1, 1, 1, alpha)
			op.GeoM.Translate(0, 129)
			rasterGrad := createGradient(640, 12, gdcRasterBar)
			screen.DrawImage(rasterGrad, op)
		}

		if d.state >= StateShowLowerRasterbarPhe {
			alpha := 1.0
			if d.state == StateShowLowerRasterbarPhe {
				alpha = d.percent / 100.0
			}
			op := &ebiten.DrawImageOptions{}
			op.ColorM.Scale(1, 1, 1, alpha)
			op.GeoM.Translate(0, 430)
			rasterGrad := createGradient(640, 12, gdcRasterBar)
			screen.DrawImage(rasterGrad, op)
		}

		if d.state >= StateDropPhotonPhe {
			if d.state == StatePhotonFadeToRedPhe {
				d.cnvPhoton.Clear()
				d.cnvPhoton.DrawImage(d.imgPhoton, nil)

				op := &ebiten.DrawImageOptions{}
				op.GeoM.Translate(285, 445)

				lightness := d.percent / 100.0
				op.ColorM.Scale(lightness, lightness*0.5, lightness*0.5, 1)

				screen.DrawImage(d.cnvPhoton, op)
			} else {
				op := &ebiten.DrawImageOptions{}
				op.GeoM.Translate(285, d.photonY)
				screen.DrawImage(d.imgPhoton, op)
			}
		}

	case StateMainDemoPhe:
		screen.Fill(color.Black)
		// Fill middle section with dark blue background (scaled for 800x600)
		for y := 162; y < 537; y++ {
			for x := 0; x < 800; x++ {
				screen.Set(x, y, color.RGBA{0x00, 0x01, 0x11, 0xFF})
			}
		}

		// Draw logo centered horizontally (640 wide -> center in 800)
		logoOp := &ebiten.DrawImageOptions{}
		logoOp.GeoM.Translate(80, 0) // Center horizontally: (800-640)/2 = 80
		screen.DrawImage(d.imgLogo, logoOp)

		// Draw upper raster bar (full width)
		op := &ebiten.DrawImageOptions{}
		op.GeoM.Translate(0, 129)
		rasterGrad := createGradient(800, 12, gdcRasterBar)
		screen.DrawImage(rasterGrad, op)

		// Draw lower raster bar (full width)
		op.GeoM.Reset()
		op.GeoM.Translate(0, 537)
		rasterGrad2 := createGradient(800, 12, gdcRasterBar)
		screen.DrawImage(rasterGrad2, op)

		// Draw photon with color cycling (centered)
		d.cnvPhoton.Clear()
		d.cnvPhoton.DrawImage(d.imgPhoton, nil)

		op = &ebiten.DrawImageOptions{}
		hue := d.color / 360.0
		r, g, b := hslToRGB(hue, 1.0, 0.5)
		op.ColorM.Scale(0, 0, 0, 1)
		op.ColorM.Translate(r, g, b, 0)
		op.GeoM.Translate(365, 555) // Centered: 285 + 80 = 365, bottom adjusted
		screen.DrawImage(d.cnvPhoton, op)

		// Draw scroller
		d.drawScroller(screen)

		if d.blackRectShow {
			for y := 468; y < 537; y++ {
				for x := 0; x < int(d.blackRectWidth); x++ {
					screen.Set(x, y, color.RGBA{0x00, 0x01, 0x11, 0xFF})
				}
			}
		}
	}
}

func (d *PhenomenaDemo) drawScroller(screen *ebiten.Image) {
	d.cnvScroller.Fill(color.RGBA{0x00, 0x01, 0x11, 0xFF})

	t2 := d.t
	for i := 0; i < 240; i++ {
		var ypos float64
		if t2 > 5*50-float64(i)*0.0033 {
			ypos = 80 * math.Cos(5*10.50+t2/6)
		} else {
			ypos = 80
		}

		charsetIdx := -1
		for j, c := range charsetPhenomena {
			if d.scrollChars[i].char == c {
				charsetIdx = j
				break
			}
		}

		if charsetIdx >= 0 && charsetIdx < len(charsetPhenomena) {
			frame := d.scrollChars[i].frame
			slice := d.scrollChars[i].slice

			sx := frame*16 + slice*2
			sy := charsetIdx * 33

			if sx >= 0 && sx <= 480-2 && sy >= 0 && sy <= len(charsetPhenomena)*33-33 {
				op := &ebiten.DrawImageOptions{}
				op.GeoM.Translate(float64(i*2), 67+ypos)

				subImg := d.cnvFrames.SubImage(image.Rect(sx, sy, sx+2, sy+33)).(*ebiten.Image)
				d.cnvScroller.DrawImage(subImg, op)
			}
		}

		t2 += 1.0 / 6.0
	}

	// Draw scroller to screen scaled and centered
	// Original: 480x180 scaled 2x1.5 = 960x270 starting at y=156
	// For 800x600: scale to fit width, center horizontally
	op := &ebiten.DrawImageOptions{}
	op.GeoM.Scale(1.67, 1.875) // Scale to fill 800 width: 480*1.67=800, height: 180*1.875=337.5
	op.GeoM.Translate(0, 195)  // Adjust vertical position for 800x600
	screen.DrawImage(d.cnvScroller, op)
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

// ==================== TCB DEMO (Demo2) ====================

//go:embed assets/tcb/rast.png
var demo2RastData []byte

//go:embed assets/tcb/mountains.png
var demo2MountainsData []byte

//go:embed assets/tcb/logo.png
var demo2LogoData []byte

//go:embed assets/tcb/bgfont.png
var demo2FontData []byte

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
	letter  string
}

type TCBDemo struct {
	initialized bool

	rasters   *ebiten.Image
	mountains *ebiten.Image
	logo      *ebiten.Image
	font      *ebiten.Image

	mycanvas     *ebiten.Image
	papercanvas  *ebiten.Image
	papercanvas2 *ebiten.Image
	scrollcanvas *ebiten.Image
	lettercanvas *ebiten.Image
	thecanvas    *ebiten.Image
	thecanvas2   *ebiten.Image

	fontTiles map[rune]*ebiten.Image

	bgSpeed []float64
	bgPos   []float64

	scrollForms []ScrollForm
	form        int
	scrollX     float64
	scrollText  string
	addi        int
	sinAdder    float64
	printPos    []PrintPos

	logoSin  []float64
	dcounter int
	rotPos   float64
	rotAdd   float64
	next     int
}

func NewTCBDemo() *TCBDemo {
	d := &TCBDemo{
		mycanvas:     ebiten.NewImage(800, 600),
		papercanvas:  ebiten.NewImage(320, 200),
		papercanvas2: ebiten.NewImage(640, 400),
		scrollcanvas: ebiten.NewImage(320, 200),
		lettercanvas: ebiten.NewImage(32, 32),

		fontTiles: make(map[rune]*ebiten.Image),
		printPos:  make([]PrintPos, 30),

		form:    0,
		addi:    0,
		rotAdd:  1,
		scrollX: 0,
	}

	d.scrollForms = []ScrollForm{
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
	d.bgSpeed = make([]float64, 32)
	d.bgPos = make([]float64, 32)

	copy(d.bgSpeed[:16], speeds)
	copy(d.bgSpeed[16:], speeds)

	d.initLogoSin()
	d.initScrollText()

	return d
}

func (d *TCBDemo) initLogoSin() {
	d.logoSin = make([]float64, 0)

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
		d.cacheFontTiles()
	}

	if d.logo != nil {
		d.thecanvas = ebiten.NewImage(80, 16)
		d.thecanvas2 = ebiten.NewImage(80, 16)

		tcbPart := d.logo.SubImage(image.Rect(114, 0, 193, 15)).(*ebiten.Image)

		op := &ebiten.DrawImageOptions{}
		d.thecanvas.DrawImage(tcbPart, op)

		op2 := &ebiten.DrawImageOptions{}
		op2.GeoM.Scale(1, -1)
		op2.GeoM.Translate(0, 16)
		d.thecanvas2.DrawImage(tcbPart, op2)
	}

	d.initialized = true
	return nil
}

func (d *TCBDemo) cacheFontTiles() {
	charMap := [][]rune{
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
				d.fontTiles[ch] = d.font.SubImage(
					image.Rect(x, y, x+32, y+33),
				).(*ebiten.Image)
			}
		}
	}

	d.fontTiles[' '] = ebiten.NewImage(32, 33)
}

func (d *TCBDemo) Update() error {
	if !d.initialized {
		if err := d.Init(); err != nil {
			return err
		}
	}

	for i := 0; i < 32; i++ {
		d.bgPos[i] = math.Mod(d.bgPos[i]-d.bgSpeed[i], 256)
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

	for i := range d.printPos {
		d.printPos[i] = PrintPos{}
	}

	printIdx := 0
	for i := 0; i < 30; i++ {
		charIdx := d.addi + i
		for charIdx >= len(d.scrollText) {
			charIdx -= len(d.scrollText)
		}

		letter := string(d.scrollText[charIdx])

		if letter == "^" && charIdx+1 < len(d.scrollText) {
			nextChar := d.scrollText[(charIdx+1)%len(d.scrollText)]
			if nextChar >= '0' && nextChar <= '7' {
				d.form = int(nextChar - '0')
				letter = string(d.scrollText[(charIdx-1+len(d.scrollText))%len(d.scrollText)])
			}
		}

		if charIdx > 0 && d.scrollText[(charIdx-1+len(d.scrollText))%len(d.scrollText)] == '^' {
			if d.scrollText[charIdx] >= '0' && d.scrollText[charIdx] <= '7' {
				if charIdx >= 2 {
					letter = string(d.scrollText[(charIdx-2+len(d.scrollText))%len(d.scrollText)])
				}
			}
		}

		sf := d.scrollForms[d.form]

		letterZ := sf.zSize*math.Sin(sf.zAdd+float64(charIdx)*sf.zAmount*0.01+d.sinAdder*sf.zSpeed) + 150
		letterY := sf.ySize*math.Cos(1.5+float64(charIdx)*sf.yAmount*0.01+d.sinAdder*sf.ySpeed) - 4

		scale := 250.0 / (250.0 + letterZ)

		letterX := -450.0 + float64(i)*32 - d.scrollX
		x2d := ((letterX - 16) * scale) + 160.0
		y2d := ((letterY - 14) * scale) + 100.0

		d.printPos[printIdx].x = x2d
		d.printPos[printIdx].y = y2d
		d.printPos[printIdx].z = scale
		d.printPos[printIdx].letter = letter
		printIdx++
	}

	sort.Slice(d.printPos, func(i, j int) bool {
		return d.printPos[i].z < d.printPos[j].z
	})

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

	d.mycanvas.Fill(color.Black)
	d.papercanvas.Clear()
	d.papercanvas2.Clear()
	d.scrollcanvas.Clear()

	for i := 0; i < 16; i++ {
		xPos := int(d.bgPos[i]) * 2
		yPos := i * 10

		srcY := i * 10
		mountainStrip := d.mountains.SubImage(image.Rect(0, srcY, 1024, srcY+10)).(*ebiten.Image)

		op := &ebiten.DrawImageOptions{}
		op.GeoM.Translate(float64(xPos), float64(yPos))
		d.papercanvas2.DrawImage(mountainStrip, op)

		op.GeoM.Translate(640, 0)
		d.papercanvas2.DrawImage(mountainStrip, op)
	}

	for i := 16; i < 32; i++ {
		xPos := int(d.bgPos[i]) * 2
		yPos := i*10 + 84

		srcY := i * 10
		mountainStrip := d.mountains.SubImage(image.Rect(0, srcY, 1024, srcY+10)).(*ebiten.Image)

		op := &ebiten.DrawImageOptions{}
		op.GeoM.Translate(float64(xPos), float64(yPos))
		d.papercanvas2.DrawImage(mountainStrip, op)

		op.GeoM.Translate(640, 0)
		d.papercanvas2.DrawImage(mountainStrip, op)
	}

	op := &ebiten.DrawImageOptions{}
	op.GeoM.Translate(64, 60)
	d.mycanvas.DrawImage(d.papercanvas2, op)

	for i := 0; i < 32; i++ {
		xOffset := d.logoSin[d.dcounter+i]

		src := d.logo.SubImage(image.Rect(0, 16+i, 303, 17+i)).(*ebiten.Image)
		op := &ebiten.DrawImageOptions{}
		op.GeoM.Translate(8+xOffset, float64(96+i))
		d.papercanvas.DrawImage(src, op)
	}

	if d.thecanvas != nil && d.thecanvas2 != nil {
		op = &ebiten.DrawImageOptions{}
		op.GeoM.Translate(-40, -8)
		op.GeoM.Scale(1, d.rotPos)
		op.GeoM.Translate(160, 88)

		if d.next == 0 {
			d.papercanvas.DrawImage(d.thecanvas, op)
		} else {
			d.papercanvas.DrawImage(d.thecanvas2, op)
		}
	}

	d.drawScroll3D()

	op = &ebiten.DrawImageOptions{}
	d.papercanvas.DrawImage(d.scrollcanvas, op)

	op = &ebiten.DrawImageOptions{}
	op.GeoM.Scale(2, 2)
	op.GeoM.Translate(64, 60)
	d.mycanvas.DrawImage(d.papercanvas, op)

	screen.DrawImage(d.mycanvas, nil)
}

func (d *TCBDemo) drawScroll3D() {
	for i := 0; i < 30; i++ {
		if d.printPos[i].letter == "" || d.printPos[i].z <= 0 {
			continue
		}

		ch := rune(d.printPos[i].letter[0])
		tile, ok := d.fontTiles[ch]
		if !ok {
			if ch >= 'a' && ch <= 'z' {
				ch = ch - 'a' + 'A'
				tile, ok = d.fontTiles[ch]
			}
			if !ok {
				tile = d.fontTiles[' ']
			}
		}

		if tile != nil {
			op := &ebiten.DrawImageOptions{}
			op.GeoM.Translate(-16, -16.5)
			op.GeoM.Scale(d.printPos[i].z, d.printPos[i].z)
			op.GeoM.Translate(d.printPos[i].x, d.printPos[i].y)

			op.Filter = ebiten.FilterNearest

			d.scrollcanvas.DrawImage(tile, op)
		}
	}

	op := &ebiten.DrawImageOptions{}
	op.GeoM.Scale(float64(d.scrollcanvas.Bounds().Dx())/float64(d.rasters.Bounds().Dx()), 1)
	op.CompositeMode = ebiten.CompositeModeSourceAtop
	d.scrollcanvas.DrawImage(d.rasters, op)
}

// ==================== COCO DEMO (Demo3) ====================

//go:embed assets/coco/dma-70.png
var demo3TitleData []byte

//go:embed assets/coco/bars.png
var demo3BarsData []byte

//go:embed assets/coco/coco.png
var demo3CocoData []byte

//go:embed assets/coco/small-dma-jelly.png
var demo3DmaLogoData []byte

//go:embed assets/coco/font.png
var demo3FontData []byte

const (
	nbCubes3               = 12
	nbDMALogos3            = 16
	canvasWidth3           = demoWidth * 8
	canvasHeight3          = demoHeight * 8
	fontHeight3            = 36
	scrollScaleInt3        = 3
	scrollScaleFactor3     = 3.0
	scrollSurfWidthFactor3 = 2.0
	scrollSpeedFactor3     = 10.0 * 1.5
)

type Cube3D struct {
	angleX float64
	angleY float64
	angleZ float64
	size   float64
}

func NewCube3D(size float64) *Cube3D {
	return &Cube3D{size: size}
}

func (c *Cube3D) Rotate(dx, dy, dz float64) {
	c.angleX += dx
	c.angleY += dy
	c.angleZ += dz
}

type Letter3 struct {
	x, y  int
	width int
}

type DMASprite3 struct {
	x, y float64
}

type CocoDemo struct {
	initialized bool

	titleImg   *ebiten.Image
	barsImg    *ebiten.Image
	cocoImg    *ebiten.Image
	dmaLogoImg *ebiten.Image
	fontImg    *ebiten.Image

	cocoCanvas  *ebiten.Image
	titleCanvas *ebiten.Image
	scrollSurf  *ebiten.Image

	letterData map[rune]*Letter3

	// 3D Cubes
	cubes     []*Cube3D
	spritePos []float64

	// DMA logo sprites (16 logos in 4x4 grid)
	dmaSprites [nbDMALogos3]DMASprite3
	ctrSprite  float64

	// Scrolling text (megatwist style)
	frontWavePos  int
	letterNum     int
	letterDecal   int
	curves        [][]int
	frontMainWave []int
	position      []int
	scrollText    string
	scrollRunes   []rune

	// Rotozoom
	posXi float64
	posZi float64
	posRi float64

	// Title logo animation
	logoX float64
	hold  int

	// Copper bars
	cnt       int
	cnt2      int
	copperSin []int

	// VBL counter
	vbl       int
	iteration int
}

func NewCocoDemo() *CocoDemo {
	spc := "     "
	d := &CocoDemo{
		letterData:  make(map[rune]*Letter3),
		cocoCanvas:  ebiten.NewImage(canvasWidth3, canvasHeight3),
		titleCanvas: ebiten.NewImage(demoWidth, 72),
		scrollSurf:  ebiten.NewImage(int(float64(demoWidth)*scrollSurfWidthFactor3), int(float64(fontHeight3)*scrollScaleFactor3)),
		spritePos:   make([]float64, nbCubes3),
		logoX:       0.5,
		hold:        0,
		scrollText:  spc + spc + "WELCOME TO THE COCO IS THE BEST DEMO! " + spc + "THIS DEMO COMBINES THE BEST EFFECTS FROM VARIOUS ATARI ST DEMOS. " + spc + "GREETINGS TO ALL DEMOSCENE LOVERS! " + spc + spc,
	}

	d.scrollRunes = []rune(d.scrollText)

	// Init 3D cubes
	d.cubes = make([]*Cube3D, nbCubes3)
	for i := 0; i < nbCubes3; i++ {
		d.cubes[i] = NewCube3D(40.0)
		d.spritePos[i] = float64(0.15) * float64(i+1)
		d.cubes[i].angleX = float64(i) * 0.3
		d.cubes[i].angleY = float64(i) * 0.2
		d.cubes[i].angleZ = float64(i) * 0.1
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
	}

	img, _, err = image.Decode(bytes.NewReader(demo3CocoData))
	if err != nil {
		log.Printf("Error loading coco: %v", err)
	} else {
		d.cocoImg = ebiten.NewImageFromImage(img)
		// Tile the background
		if d.cocoImg != nil {
			tw := d.cocoImg.Bounds().Dx()
			th := d.cocoImg.Bounds().Dy()
			for y := 0; y < canvasHeight3; y += th {
				for x := 0; x < canvasWidth3; x += tw {
					op := &ebiten.DrawImageOptions{}
					op.GeoM.Translate(float64(x), float64(y))
					d.cocoCanvas.DrawImage(d.cocoImg, op)
				}
			}
		}
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
		d.initFontData3()
		d.precalcPosition()
	}

	d.initialized = true
	return nil
}

func (d *CocoDemo) initFontData3() {
	data := []struct {
		char  rune
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
		d.letterData[dd.char] = &Letter3{x: dd.x, y: dd.y, width: dd.width}
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
		d.cubes[i].Rotate(
			0.02*(1+float64(i)*0.1),
			0.03*(1+float64(i)*0.15),
			0.01*(1+float64(i)*0.05),
		)
	}

	// Update DMA logo sprites - synchronized movement
	d.ctrSprite += 0.02
	baseX := 100*math.Sin(d.ctrSprite*1.35+1.25) + 100*math.Sin(d.ctrSprite*1.86+0.54)
	baseY := 60*math.Cos(d.ctrSprite*1.72+0.23) + 60*math.Cos(d.ctrSprite*1.63+0.98)

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
	if d.hold >= 1 {
		d.hold--
	}
	if d.hold <= 0 {
		d.logoX += 0.0125
	}

	d.vbl++
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

	oscX := (float64(demoWidth) / 4) * math.Cos(d.posXi*4-math.Cos(d.posXi-0.1))
	oscY := (float64(demoHeight) / 2.7) * -math.Sin(d.posXi*2.3-math.Cos(d.posXi-0.1))

	centerX := float64(demoWidth)/2 + oscX
	centerY := float64(demoHeight)/2 + oscY

	op := &ebiten.DrawImageOptions{}
	op.GeoM.Translate(-float64(canvasWidth3)/2, -float64(canvasHeight3)/2)
	op.GeoM.Rotate(rot)
	op.GeoM.Scale(zoom, zoom)
	op.GeoM.Translate(centerX, centerY)
	op.ColorScale.Scale(0.5, 0.5, 0.5, 1.0)
	dst.DrawImage(d.cocoCanvas, op)
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
	d.frontWavePos = int(float64(d.iteration) * scrollSpeedFactor3)

	decalX := 999999999
	for ligne := 0; ligne < 36; ligne++ {
		wave := d.getWave3(d.frontWavePos + ligne)
		if wave < decalX {
			decalX = wave
		}
	}

	if decalX < 0 {
		decalX = 0
	}

	i := 0
	dir := 0
	if decalX > d.letterDecal {
		dir = 1
	} else if decalX < d.letterDecal {
		dir = -1
	}

	for decalX < d.getPosition3(d.letterNum+i) || d.getPosition3(d.letterNum+i+1) <= decalX {
		i += dir
		if d.letterNum+i < 0 || d.letterNum+i >= len(d.position) {
			break
		}
	}
	d.letterNum += i
	if d.letterNum < 0 {
		d.letterNum = 0
	}
	if len(d.position) > 0 && d.letterNum >= len(d.position) {
		d.letterNum = len(d.position) - 1
	}
	if d.letterNum < 0 {
		d.letterNum = 0
	}
	d.letterDecal = d.getPosition3(d.letterNum)

	// Safety check before calling displayText3
	if d.letterNum >= 0 && len(d.scrollRunes) > 0 {
		d.displayText3(d.letterNum)
	}

	bounce := int(math.Floor(18.0 * math.Abs(math.Sin(float64(d.iteration)*0.1))))

	scrollWidth := d.scrollSurf.Bounds().Dx()
	scaledFontHeight := fontHeight3 * scrollScaleInt3

	baseY := 72
	totalLines := demoHeight - 72
	for ligne := 0; ligne < totalLines; ligne++ {
		sourceFontLine := ligne / scrollScaleInt3
		frontWave := d.getWave3(d.frontWavePos + sourceFontLine)
		scrollXRaw := frontWave - d.letterDecal

		scaledLine := ((sourceFontLine + bounce) % fontHeight3) * scrollScaleInt3
		scaledLine += ligne % scrollScaleInt3

		if scaledLine >= scaledFontHeight {
			scaledLine = scaledLine % scaledFontHeight
		}

		if scrollXRaw < 0 {
			visibleWidth := demoWidth + scrollXRaw
			if visibleWidth > 0 {
				srcRect := image.Rect(0, scaledLine, minInt3(visibleWidth, scrollWidth), scaledLine+1)
				op := &ebiten.DrawImageOptions{}
				op.GeoM.Translate(float64(-scrollXRaw), float64(baseY+ligne))
				dst.DrawImage(d.scrollSurf.SubImage(srcRect).(*ebiten.Image), op)
			}
			continue
		}

		scrollX := scrollXRaw % scrollWidth
		if scrollX >= scrollWidth-demoWidth {
			width1 := scrollWidth - scrollX
			if width1 > 0 && width1 <= demoWidth {
				srcRect := image.Rect(scrollX, scaledLine, scrollWidth, scaledLine+1)
				op := &ebiten.DrawImageOptions{}
				op.GeoM.Translate(0, float64(baseY+ligne))
				dst.DrawImage(d.scrollSurf.SubImage(srcRect).(*ebiten.Image), op)
			}

			width2 := demoWidth - width1
			if width2 > 0 && width2 <= demoWidth {
				srcRect := image.Rect(0, scaledLine, width2, scaledLine+1)
				op := &ebiten.DrawImageOptions{}
				op.GeoM.Translate(float64(width1), float64(baseY+ligne))
				dst.DrawImage(d.scrollSurf.SubImage(srcRect).(*ebiten.Image), op)
			}
		} else if scrollX+demoWidth <= scrollWidth {
			srcRect := image.Rect(scrollX, scaledLine, scrollX+demoWidth, scaledLine+1)
			op := &ebiten.DrawImageOptions{}
			op.GeoM.Translate(0, float64(baseY+ligne))
			dst.DrawImage(d.scrollSurf.SubImage(srcRect).(*ebiten.Image), op)
		}
	}
}

func minInt3(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func (d *CocoDemo) displayText3(letterOffset int) {
	d.scrollSurf.Clear()

	xPos := 0
	i := 0
	maxWidth := d.scrollSurf.Bounds().Dx() + int(200*scrollScaleFactor3)

	for xPos < maxWidth {
		char := d.getLetter3(i + letterOffset)
		if letter, ok := d.letterData[char]; ok {
			srcRect := image.Rect(letter.x, letter.y, letter.x+letter.width, letter.y+fontHeight3)
			op := &ebiten.DrawImageOptions{}
			op.GeoM.Scale(scrollScaleFactor3, scrollScaleFactor3)
			op.GeoM.Translate(float64(xPos), 0)
			d.scrollSurf.DrawImage(d.fontImg.SubImage(srcRect).(*ebiten.Image), op)
			xPos += int(float64(letter.width) * scrollScaleFactor3)
		}
		i++
	}
}

func (d *CocoDemo) draw3DCubes3(dst *ebiten.Image) {
	for i := 0; i < nbCubes3; i++ {
		xPos := float64((demoWidth-40)/2) + (float64((demoWidth-40)/2) * math.Sin(d.spritePos[i]))
		yPos := float64(demoHeight)/2 + (84 * math.Cos(d.spritePos[i]*2.5))
		d.cubes[i].Draw3(dst, xPos, yPos)
	}
}

func (c *Cube3D) Draw3(screen *ebiten.Image, centerX, centerY float64) {
	vertices := [][3]float64{
		{-c.size / 2, -c.size / 2, -c.size / 2},
		{c.size / 2, -c.size / 2, -c.size / 2},
		{c.size / 2, c.size / 2, -c.size / 2},
		{-c.size / 2, c.size / 2, -c.size / 2},
		{-c.size / 2, -c.size / 2, c.size / 2},
		{c.size / 2, -c.size / 2, c.size / 2},
		{c.size / 2, c.size / 2, c.size / 2},
		{-c.size / 2, c.size / 2, c.size / 2},
	}

	faces := [][4]int{
		{0, 1, 2, 3}, {4, 5, 6, 7}, {0, 1, 5, 4},
		{2, 3, 7, 6}, {0, 3, 7, 4}, {1, 2, 6, 5},
	}

	faceColors := []color.Color{
		color.RGBA{255, 140, 0, 255}, color.RGBA{255, 165, 50, 255},
		color.RGBA{255, 180, 80, 255}, color.RGBA{255, 120, 0, 255},
		color.RGBA{255, 150, 30, 255}, color.RGBA{255, 200, 100, 255},
	}

	rotated := make([][3]float64, len(vertices))
	for i, v := range vertices {
		x, y, z := v[0], v[1], v[2]

		cosX, sinX := math.Cos(c.angleX), math.Sin(c.angleX)
		y1 := y*cosX - z*sinX
		z1 := y*sinX + z*cosX
		y, z = y1, z1

		cosY, sinY := math.Cos(c.angleY), math.Sin(c.angleY)
		x1 := x*cosY + z*sinY
		z2 := -x*sinY + z*cosY
		x, z = x1, z2

		cosZ, sinZ := math.Cos(c.angleZ), math.Sin(c.angleZ)
		x2 := x*cosZ - y*sinZ
		y2 := x*sinZ + y*cosZ
		x, y = x2, y2

		rotated[i] = [3]float64{x, y, z}
	}

	type faceDepth struct {
		index int
		depth float64
	}
	depths := make([]faceDepth, len(faces))

	for i, face := range faces {
		centerZ := 0.0
		for _, vi := range face {
			centerZ += rotated[vi][2]
		}
		depths[i] = faceDepth{i, centerZ / 4}
	}

	for i := 0; i < len(depths)-1; i++ {
		for j := i + 1; j < len(depths); j++ {
			if depths[i].depth > depths[j].depth {
				depths[i], depths[j] = depths[j], depths[i]
			}
		}
	}

	for _, fd := range depths {
		face := faces[fd.index]
		faceColor := faceColors[fd.index]

		points := make([]float64, 0, 8)
		for _, vi := range face {
			v := rotated[vi]
			x2d, y2d := project3D3(v[0], v[1], v[2])
			points = append(points, centerX+x2d, centerY+y2d)
		}

		drawPolygon3(screen, points, faceColor)

		edgeColor := color.RGBA{
			uint8(faceColor.(color.RGBA).R * 3 / 4),
			uint8(faceColor.(color.RGBA).G * 3 / 4),
			uint8(faceColor.(color.RGBA).B * 3 / 4),
			255,
		}
		for i := 0; i < 4; i++ {
			j := (i + 1) % 4
			vector.StrokeLine(screen,
				float32(points[i*2]), float32(points[i*2+1]),
				float32(points[j*2]), float32(points[j*2+1]),
				1, edgeColor, false)
		}
	}
}

func project3D3(x, y, z float64) (float64, float64) {
	perspective := 200.0
	factor := perspective / (perspective + z)
	return x * factor, y * factor
}

func drawPolygon3(screen *ebiten.Image, points []float64, fillColor color.Color) {
	if len(points) < 6 {
		return
	}

	if len(points) >= 8 {
		drawTriangle3(screen,
			float32(points[0]), float32(points[1]),
			float32(points[2]), float32(points[3]),
			float32(points[4]), float32(points[5]),
			fillColor)

		drawTriangle3(screen,
			float32(points[0]), float32(points[1]),
			float32(points[4]), float32(points[5]),
			float32(points[6]), float32(points[7]),
			fillColor)
	}
}

func drawTriangle3(screen *ebiten.Image, x1, y1, x2, y2, x3, y3 float32, clr color.Color) {
	if y1 > y2 {
		x1, y1, x2, y2 = x2, y2, x1, y1
	}
	if y1 > y3 {
		x1, y1, x3, y3 = x3, y3, x1, y1
	}
	if y2 > y3 {
		x2, y2, x3, y3 = x3, y3, x2, y2
	}

	for y := y1; y <= y3; y++ {
		var xStart, xEnd float32

		if y < y2 {
			if y2-y1 > 0 {
				t := (y - y1) / (y2 - y1)
				x12 := x1 + (x2-x1)*t
				t13 := (y - y1) / (y3 - y1)
				x13 := x1 + (x3-x1)*t13
				xStart, xEnd = x12, x13
			}
		} else {
			if y3-y2 > 0 && y3-y1 > 0 {
				t := (y - y2) / (y3 - y2)
				x23 := x2 + (x3-x2)*t
				t13 := (y - y1) / (y3 - y1)
				x13 := x1 + (x3-x1)*t13
				xStart, xEnd = x23, x13
			}
		}

		if xStart > xEnd {
			xStart, xEnd = xEnd, xStart
		}

		vector.StrokeLine(screen, xStart, y, xEnd, y, 1, clr, false)
	}
}

func (d *CocoDemo) drawTitleWithCopperbars3(dst *ebiten.Image) {
	if d.titleImg == nil {
		return
	}

	d.titleCanvas.Fill(color.Black)
	d.drawCopperBars3(d.titleCanvas)

	titleX := 64 + float64(demoWidth)*math.Cos(d.logoX)
	titleH := float64(d.titleImg.Bounds().Dy())
	scaleY := 72.0 / titleH

	op := &ebiten.DrawImageOptions{}
	op.GeoM.Scale(1.0, scaleY)
	op.GeoM.Translate(titleX, 0)
	d.titleCanvas.DrawImage(d.titleImg, op)

	dst.DrawImage(d.titleCanvas, nil)
}

func (d *CocoDemo) drawCopperBars3(dst *ebiten.Image) {
	if d.barsImg == nil {
		return
	}

	barsWidth, barsHeight := d.barsImg.Size()
	if barsHeight < 20 {
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
			srcRect := image.Rect(0, cc, barsWidth, cc+2)
			if srcRect.Max.Y > barsHeight {
				srcRect.Max.Y = barsHeight
			}

			scaleY := float64(height) / 2.0
			op.GeoM.Scale(1, scaleY)
			op.GeoM.Translate(float64(xPos), float64(yPos))
			dst.DrawImage(d.barsImg.SubImage(srcRect).(*ebiten.Image), op)
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
	d.position = []int{}

	for _, r := range d.scrollRunes {
		if letter, ok := d.letterData[r]; ok {
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
	if i > 0 && i <= len(d.position) {
		return d.getSum3(d.position, i-1, 0)
	}
	return 0
}

func (d *CocoDemo) getLetter3(pos int) rune {
	if len(d.scrollRunes) == 0 {
		return ' '
	}
	return d.scrollRunes[pos%len(d.scrollRunes)]
}

// ==================== VIVA TCB DEMO (Demo4) ====================

//go:embed assets/viva/logo.png
var demo4LogoData []byte

//go:embed assets/viva/title.png
var demo4TitleData []byte

//go:embed assets/viva/raster.png
var demo4RasterData []byte

//go:embed assets/viva/tcb_tile.png
var demo4TileData []byte

//go:embed assets/viva/font.png
var demo4FontData []byte

const (
	fontCharWidth4  = 42
	fontCharHeight4 = 40
)

type VivaDemo struct {
	initialized bool

	logoImg   *ebiten.Image
	titleImg  *ebiten.Image
	rasterImg *ebiten.Image
	tileImg   *ebiten.Image
	fontImg   *ebiten.Image

	tileCanvas       *ebiten.Image
	spriteCanvas     *ebiten.Image
	titleCanvas      *ebiten.Image
	titleCanvasSmall *ebiten.Image

	logoX    float64
	hold     int
	rasterY1 float64
	rasterY2 float64

	scrollX1 float64
	scrollX2 float64
	scrollX3 float64
	scrollX4 float64

	fxFlag int
	posXi  float64
	posZi  float64
	posRi  float64

	initX float64
	initR float64

	text1 string
	text2 string
	text3 string
	text4 string

	loopCounter int
	startTime   time.Time
}

func NewVivaDemo() *VivaDemo {
	d := &VivaDemo{
		logoX:     1.5,
		hold:      0,
		rasterY1:  0,
		rasterY2:  72,
		initX:     0,
		initR:     0,
		fxFlag:    0,
		startTime: time.Now(),
	}

	pad := "       "
	d.text1 = pad + "VIVA THE CAREBEARS!" + pad
	d.text2 = pad + "LEGENDS OF THE ATARI ST DEMOSCENE!" + pad
	d.text3 = pad + "GREETINGS TO ALL DEMOSCENERS!" + pad
	d.text4 = pad + "KEEP THE SCENE ALIVE!" + pad

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
	}

	// Create canvases (scaled for 800x600)
	d.spriteCanvas = ebiten.NewImage(400, 300)
	d.titleCanvas = ebiten.NewImage(800, 72)
	d.titleCanvasSmall = ebiten.NewImage(528, 36)
	d.tileCanvas = ebiten.NewImage(800*16, 600*16)

	// Initialize tile pattern
	if d.tileImg != nil {
		tw := d.tileImg.Bounds().Dx()
		th := d.tileImg.Bounds().Dy()
		for y := 0; y < 600*16; y += th {
			for x := 0; x < 800*16; x += tw {
				op := &ebiten.DrawImageOptions{}
				op.GeoM.Translate(float64(x), float64(y))
				d.tileCanvas.DrawImage(d.tileImg, op)
			}
		}
	}

	d.fxFlag = 3 // Start in full demo mode
	d.initialized = true
	d.startTime = time.Now()
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

func (d *VivaDemo) drawScroller(dst *ebiten.Image, text string, scrollX float64, scrollerID int, baseY float64) float64 {
	if d.fontImg == nil {
		return scrollX
	}

	if d.startTime.IsZero() {
		d.startTime = time.Now()
	}

	runes := []rune(text)
	if len(runes) == 0 {
		return scrollX
	}

	t := time.Since(d.startTime).Seconds() + 19
	sp := int(scrollX / 64)
	xs := math.Sin(t*0.25)*0.5 + 0.5
	xs = math.Sqrt(math.Max(0, 1-xs*xs))

	for i := sp + 8; i >= sp; i-- {
		if i < 0 || i >= len(runes) {
			continue
		}

		z := math.Sin((t+float64(i)*0.15)*5)*0.5 + 1.5
		charCode := int(runes[i])
		fontIndex := mapCharToFont4(charCode)

		drawX := math.Floor((float64(i)*64 - 40 - math.Sin(t*7+float64(i)*18)*32*xs - scrollX) * 2)
		drawY := math.Floor(math.Sin((t+float64(i)*0.1)*7)*42*(math.Sin(t*0.5)*0.5+0.5) + baseY - z*32)

		var scale float64
		if scrollerID == 1 || scrollerID == 2 {
			scale = 3 - z
		} else {
			scale = z
		}

		if drawX < -100 || drawX > float64(demoWidth)+100 || drawY < -100 || drawY > float64(demoHeight)+100 {
			continue
		}
		if scale <= 0.1 {
			continue
		}

		cols := 10
		srcX := (fontIndex % cols) * fontCharWidth4
		srcY := (fontIndex / cols) * fontCharHeight4

		if srcY+fontCharHeight4 > d.fontImg.Bounds().Dy() {
			continue
		}

		op := &ebiten.DrawImageOptions{}
		op.GeoM.Scale(scale, scale)
		op.GeoM.Translate(drawX, drawY)
		op.ColorScale.Scale(1, 1, 1, 0.9)

		charImg := d.fontImg.SubImage(image.Rect(srcX, srcY, srcX+fontCharWidth4, srcY+fontCharHeight4)).(*ebiten.Image)
		dst.DrawImage(charImg, op)
	}

	return math.Mod(scrollX+4, float64(len(runes)*64))
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

	oscX := (800.0 / 4) * math.Cos(d.posXi*4-math.Cos(d.posXi-0.1))
	oscY := (600.0 / 2.7) * -math.Sin(d.posXi*2.3-math.Cos(d.posXi-0.1))

	centerX := 400.0 + oscX
	centerY := 300.0 + oscY

	op := &ebiten.DrawImageOptions{}
	op.GeoM.Translate(-float64(800*16)/2, -float64(600*16)/2)
	op.GeoM.Rotate(rot)
	op.GeoM.Scale(zoom, zoom)
	op.GeoM.Translate(centerX, centerY)
	screen.DrawImage(d.tileCanvas, op)

	d.scrollX1 = d.drawScroller(screen, d.text1, d.scrollX1, 1, 500)
	d.scrollX2 = d.drawScroller(screen, d.text2, d.scrollX2, 2, 250)
	d.scrollX3 = d.drawScroller(screen, d.text3, d.scrollX3, 3, 375)
	d.scrollX4 = d.drawScroller(screen, d.text4, d.scrollX4, 4, 125)

	// Black bar at top
	for y := 0; y < 72; y++ {
		for x := 0; x < 800; x++ {
			screen.Set(x, y, color.Black)
		}
	}

	// Draw title with rasters (match original pipeline)
	if d.titleCanvasSmall != nil {
		d.titleCanvasSmall.Fill(color.Black)

		if d.rasterImg != nil {
			titleBounds := d.titleCanvasSmall.Bounds()
			titleWidth := float64(titleBounds.Dx())
			rasterWidth := float64(d.rasterImg.Bounds().Dx())
			if rasterWidth != 0 {
				scaleRasterX := titleWidth / rasterWidth

				op := &ebiten.DrawImageOptions{}
				op.GeoM.Scale(scaleRasterX, 1)
				op.GeoM.Translate(0, d.rasterY1)
				d.titleCanvasSmall.DrawImage(d.rasterImg, op)

				op = &ebiten.DrawImageOptions{}
				op.GeoM.Scale(scaleRasterX, 1)
				op.GeoM.Translate(0, d.rasterY2)
				d.titleCanvasSmall.DrawImage(d.rasterImg, op)

				op = &ebiten.DrawImageOptions{}
				op.GeoM.Scale(scaleRasterX, 1)
				op.GeoM.Translate(0, d.rasterY2+72)
				d.titleCanvasSmall.DrawImage(d.rasterImg, op)
			}
		}

		if d.titleImg != nil {
			d.titleCanvasSmall.DrawImage(d.titleImg, nil)
		}

		if d.titleCanvas != nil {
			d.titleCanvas.Fill(color.Black)
			smallBounds := d.titleCanvasSmall.Bounds()
			scaleX := float64(demoWidth) / float64(smallBounds.Dx())
			scaleY := 72.0 / float64(smallBounds.Dy())
			op := &ebiten.DrawImageOptions{}
			op.GeoM.Scale(scaleX, scaleY)
			d.titleCanvas.DrawImage(d.titleCanvasSmall, op)
		}
	}

	// Draw title canvas with oscillation
	titleX := 64 + 800*math.Cos(d.logoX)
	titleOp := &ebiten.DrawImageOptions{}
	titleOp.GeoM.Translate(titleX, 14)
	screen.DrawImage(d.titleCanvas, titleOp)

	// Draw animated logos
	if d.logoImg != nil {
		d.spriteCanvas.Clear()

		midX := 200.0 - 16
		midY := 24.0 + 150.0 - 16
		incY := 150.0 / 4

		for s := 0; s < 10; s++ {
			nit := float64(d.loopCounter + s*5)
			spX := midX + midX*math.Sin(nit/25)*math.Cos(nit/300)
			spY := midY + incY*math.Sin(nit/37) + incY*math.Cos(nit/17)

			op := &ebiten.DrawImageOptions{}
			op.GeoM.Translate(spX, spY)
			d.spriteCanvas.DrawImage(d.logoImg, op)
		}

		op := &ebiten.DrawImageOptions{}
		op.GeoM.Scale(2, 2)
		screen.DrawImage(d.spriteCanvas, op)
	}
}

// ==================== MEGA DEMO GAME ====================

type MegaDemoGame struct {
	demo1 *PhenomenaDemo
	demo2 *TCBDemo
	demo3 *CocoDemo
	demo4 *VivaDemo

	virtualCanvas *ebiten.Image

	audioContext *audio.Context
	audioPlayer  *audio.Player
	ymPlayer     *YMPlayer

	cameraState CameraState
	stateTimer  float64

	cameraX float64
	cameraY float64

	targetX float64
	targetY float64

	transitionStart float64
	transitionTime  float64
}

func NewMegaDemoGame() *MegaDemoGame {
	g := &MegaDemoGame{
		demo1:         NewPhenomenaDemo(),
		demo2:         NewTCBDemo(),
		demo3:         NewCocoDemo(),
		demo4:         NewVivaDemo(),
		virtualCanvas: ebiten.NewImage(screenWidth, screenHeight),
		cameraState:   StateDemo1,
		cameraX:       0,
		cameraY:       0,
		targetX:       0,
		targetY:       0,
		audioContext:  audio.NewContext(sampleRate),
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
	// Update all demos
	g.demo1.Update()
	g.demo2.Update()
	g.demo3.Update()
	g.demo4.Update()

	// Update state machine
	dt := 1.0 / 60.0
	g.stateTimer += dt

	switch g.cameraState {
	case StateDemo1:
		if g.stateTimer >= viewDuration {
			g.cameraState = StateTransition1to2
			g.transitionStart = 0
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
			g.transitionStart = 0
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
			g.transitionStart = 0
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
			g.transitionStart = 0
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
			g.transitionStart = 0
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
	// Render all demos to virtual canvas
	g.virtualCanvas.Clear()

	// Demo1 at (0, 0)
	demo1Canvas := ebiten.NewImage(800, 600)
	demo1Canvas.Clear()
	g.demo1.Draw(demo1Canvas)
	op := &ebiten.DrawImageOptions{}
	op.GeoM.Translate(0, 0)
	g.virtualCanvas.DrawImage(demo1Canvas, op)

	// Demo2 at (800, 0)
	demo2Canvas := ebiten.NewImage(800, 600)
	demo2Canvas.Clear()
	g.demo2.Draw(demo2Canvas)
	op = &ebiten.DrawImageOptions{}
	op.GeoM.Translate(800, 0)
	g.virtualCanvas.DrawImage(demo2Canvas, op)

	// Demo3 at (800, 600)
	demo3Canvas := ebiten.NewImage(800, 600)
	demo3Canvas.Clear()
	g.demo3.Draw(demo3Canvas)
	op = &ebiten.DrawImageOptions{}
	op.GeoM.Translate(800, 600)
	g.virtualCanvas.DrawImage(demo3Canvas, op)

	// Demo4 at (0, 600)
	demo4Canvas := ebiten.NewImage(800, 600)
	demo4Canvas.Clear()
	g.demo4.Draw(demo4Canvas)
	op = &ebiten.DrawImageOptions{}
	op.GeoM.Translate(0, 600)
	g.virtualCanvas.DrawImage(demo4Canvas, op)

	// Extract viewport based on camera
	if g.cameraState == StateTransition4toZoom {
		// Zoom out transition
		progress := g.transitionTime / transitionDuration
		if progress > 1 {
			progress = 1
		}
		eased := easeInOutCubic(progress)

		startCenterX := float64(demoWidth) / 2
		startCenterY := float64(demoHeight) + float64(demoHeight)/2
		endCenterX := float64(screenWidth) / 2
		endCenterY := float64(screenHeight) / 2

		centerX := startCenterX + (endCenterX-startCenterX)*eased
		centerY := startCenterY + (endCenterY-startCenterY)*eased
		zoom := 1.0 - eased*0.5

		op := &ebiten.DrawImageOptions{}
		op.GeoM.Translate(-centerX, -centerY)
		op.GeoM.Scale(zoom, zoom)
		op.GeoM.Translate(float64(demoWidth)/2, float64(demoHeight)/2)
		screen.DrawImage(g.virtualCanvas, op)

	} else if g.cameraState == StateLoop {
		op := &ebiten.DrawImageOptions{}
		progress := g.transitionTime / transitionDuration
		if progress > 1 {
			progress = 1
		}
		eased := easeInOutCubic(progress)
		startZoom := 0.5
		zoom := startZoom + (1.0-startZoom)*eased
		startCenterX := float64(screenWidth) / 2
		startCenterY := float64(screenHeight) / 2
		endCenterX := float64(demoWidth) / 2
		endCenterY := float64(demoHeight) / 2
		centerX := startCenterX + (endCenterX-startCenterX)*eased
		centerY := startCenterY + (endCenterY-startCenterY)*eased
		op.GeoM.Translate(-centerX, -centerY)
		op.GeoM.Scale(zoom, zoom)
		op.GeoM.Translate(float64(demoWidth)/2, float64(demoHeight)/2)
		screen.DrawImage(g.virtualCanvas, op)

	} else if g.cameraState == StateZoomOut {
		// Fully zoomed out showing all 4 demos
		op := &ebiten.DrawImageOptions{}
		centerX := float64(screenWidth) / 2
		centerY := float64(screenHeight) / 2

		op.GeoM.Translate(-centerX, -centerY)
		op.GeoM.Scale(0.5, 0.5)
		op.GeoM.Translate(float64(demoWidth)/2, float64(demoHeight)/2)
		screen.DrawImage(g.virtualCanvas, op)

	} else {
		// Normal camera viewport extraction
		viewport := g.virtualCanvas.SubImage(
			image.Rect(
				int(g.cameraX),
				int(g.cameraY),
				int(g.cameraX)+800,
				int(g.cameraY)+600,
			),
		).(*ebiten.Image)

		screen.DrawImage(viewport, nil)
	}
}

func (g *MegaDemoGame) Layout(outsideWidth, outsideHeight int) (int, int) {
	return 800, 600
}

func main() {
	ebiten.SetWindowSize(800, 600)
	ebiten.SetWindowTitle("Multiscreen remake or original Mega Demo by DMA")
	ebiten.SetVsyncEnabled(true)

	game := NewMegaDemoGame()

	if err := ebiten.RunGame(game); err != nil {
		log.Fatal(err)
	}
}
