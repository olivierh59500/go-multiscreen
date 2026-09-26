package multiscreen

import (
	"bytes"
	kit "github.com/olivierh59500/democonstructionkit"
	"image"
	"image/color"
	originalassets "multiscreen-mega-demo"

	"github.com/olivierh59500/democonstructionkit/composite"
	"github.com/olivierh59500/democonstructionkit/effects"
	"github.com/olivierh59500/democonstructionkit/motion"
	"github.com/olivierh59500/democonstructionkit/palette"
	"github.com/olivierh59500/democonstructionkit/presets"
	"github.com/olivierh59500/democonstructionkit/scrolling"
	"github.com/olivierh59500/democonstructionkit/sound"
	"github.com/olivierh59500/democonstructionkit/sprites"

	_ "image/png"
	"log"

	"github.com/hajimehoshi/ebiten/v2"
	"github.com/hajimehoshi/ebiten/v2/vector"

	audio "github.com/olivierh59500/democonstructionkit/sound/output"
)

const (
	demoWidth  = 800
	demoHeight = 600
	sampleRate = 44100
)

var musicData = originalassets.DCKAssetMusicData()

// ==================== PHENOMENA DEMO (Demo1) ====================

var demo1FontData = originalassets.
	DCKAssetDemo1FontData()

var demo1LogoData = originalassets.
	DCKAssetDemo1LogoData()

var demo1PhotonData = originalassets.DCKAssetDemo1PhotonData()

type PhenomenaDemo struct {
	dnaFrames   *scrolling.DNAFrames
	initialized bool

	imgFont       *ebiten.Image
	imgLogo       *ebiten.Image
	imgPhotonMask *ebiten.Image
	rasterGrad800 *ebiten.Image

	t float64

	// Scroller data
	sliceProgram *scrolling.SliceProgram
	rowWave      *motion.RecurrentRowWave
	dnaDraw      scrolling.DNADrawConfig
	hueMotion    *motion.WrapBank
}

const charsetPhenomena = presets.PhenomenaAlphabet

const scrollMessage = `           THIS IS IMPOSSIBLE!            WHAT IS?               THIS IS!!!                    ...SO, ANOTHER DEMO FROM PHENOMENA HAS REACHED YOU...    THIS TIME WITH CODING BY                PHOTON!                ^  RASTA MUSIC BY                    FIREFOX!                &    AND SUPER GFX BY                       TERMINATOR               #  ...SO, SLAYER! HOW DO YOU LIKE @MY@ SCROLLER?  IT'S MUCH IMPOSSIBLER THAN YOURS!    ...   SO DE SO!          DOES ANYONE HAVE A PROGRAM CALLED 'PAGE RENDER 3D'? THEN CONTACT OUR NEW GFX ARTIST AT          0492-41027               % AND ASK FOR MIKAEL. NEWS NEWS NEWS NEWS   !!! LOOK OUT FOR PHENOMENA'S NEW DISK MAG CALLED ' TRANSMISSION ' ! ! ! ! IT'S A MAG ESPECIALLY MADE FOR ALL YOU CODERS OUT THERE, COMPLETE WITH CODER / DEMO / CRACK TOP-TEN,ARTICLES ABOUT CODING / CRACKING, AND SOURCES, ETC,ETC...         HERE'S MY TOP-FIVE DEMO GROUPS 1. SCOOPEX  -SLAYER IS WORKING HARD AND HIS M.H. DEMO IS STILL UNBEATEN-  ...  2. CRYPTOBURNERS  -NICE MD 2 BUT SLOOOW VECTORS-  ... 3. RSI/PARADOX  -NICE DEMOS LATELY, EXCEPT FOR THE 'FOLLOW ME' CRAP-  ...  4. KEFRENS  -ALL YOUR LATEST DEMOS HAVE BEEN PROFESSIONAL!-  ...  5. THE LINK  -ALWAYS COOL IDEAS,GIVE US SOME MORE-  ...  OF COURSE, PHENOMENA IS EXCLUDED FROM THIS LIST...        NOW OVER TO SOME INTERNAL GREETS...  @     BIG 2A-FINISH YOUR DEMO AND BUY AN A500!   @   CORE-GET YOUR HANDS ON A WORKING AMIGA!   @   DANKO-GET BUSY!   @   KLUTTAS O SPIRIT-WAKE UP FROM YOUR COMA!!!!   @   RAVE-SAME TO YOU!       ...     AND NOW, TIME FOR SOME OTHER GREETS... THEY GO TO --- CONAN/TPL-MAKE A GOOD DEMO AND JOIN ANOTHER GROUP!   @   KALLE BALLE/TSL - EVER THOUGHT ABOUT CHANGING YOUR NAME????   @   HAVOK/ECSTASY-JOIN US! I'M JUST A PHONECALL AWAY - 0381-11344 @   MAHONEY/NS-TRY TAKING SOME IDEAS FROM NT 1.2!  @   UNCLE TOM/RAZOR-STOP DRAWING AND DO SOME MUSIC @   SLAYER/SCX-AND ALL OTHER GOOD CODERS-CALL ME FOR SOME COOL TECH-TALK    0381-11344   ZEUS/ADEPT-GOOD LUCK AND CODE HARD!       ---     NOW I DON'T HAVE VERY MUCH ELSE TO SAY, EXCEPT....                    BYE!             @@@@@@@@@@@@@                `

func NewPhenomenaDemo() *PhenomenaDemo {
	d := &PhenomenaDemo{}
	programConfig, err := presets.PhenomenaDNAProgram(scrollMessage, charToFontIndexPhe)
	if err != nil {
		panic(err)
	}
	d.sliceProgram, err = scrolling.NewSliceProgram(programConfig)
	if err != nil {
		panic(err)
	}
	wave, err := motion.NewRecurrentRowWave(presets.PhenomenaDNARows())
	if err != nil {
		panic(err)
	}
	d.rowWave = wave
	d.hueMotion, err = motion.NewWrapBank(presets.PhenomenaPhotonHueCycle())
	if err != nil {
		panic(err)
	}
	d.dnaDraw = scrolling.DNADrawConfig{SliceWidth: 2, ScaleX: 1.67, ScaleY: 1.875, OriginY: 195, Y: wave.At}
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
	d.imgPhotonMask, err = composite.NewWhiteSilhouette(img)
	if err != nil {
		return err
	}
	d.rasterGrad800, err = composite.NewGradientImage(palette.GradientConfig{
		Width: 800, Height: 12, Stops: presets.PhenomenaRasterStops(),
	})
	if err != nil {
		return err
	}

	// Init character frames
	if err := d.initCharacterFrames(); err != nil {
		return err
	}

	// Bring message to start
	if err := d.sliceProgram.Warmup(320, 1); err != nil {
		return err
	}

	d.initialized = true
	return nil
}

var charToFontIndexPhe = func() func(rune) (int, bool) {
	lookup, err := presets.TileLookup("multiscreen-phenomena", false)
	if err != nil {
		panic(err)
	}
	return lookup
}()

func (d *PhenomenaDemo) initCharacterFrames() error {
	gradient := func(height int, stops []palette.GradientStop) (*ebiten.Image, error) {
		return composite.NewGradientImage(palette.GradientConfig{Width: 480, Height: height, Stops: stops})
	}
	core, err := gradient(9, presets.PhenomenaCoreStops())
	if err != nil {
		return err
	}
	defer core.Deallocate()
	front, err := gradient(33, presets.PhenomenaFrontStops())
	if err != nil {
		return err
	}
	defer front.Deallocate()
	back, err := gradient(33, presets.PhenomenaBackStops())
	if err != nil {
		return err
	}
	defer back.Deallocate()
	glyphs, err := scrolling.GridImages(d.imgFont, image.Pt(16, 26), len(charsetPhenomena), len(charsetPhenomena))
	if err != nil {
		return err
	}
	d.dnaFrames, err = scrolling.NewDNAFrames(glyphs, scrolling.DNAFrameConfig{Frames: 30, Height: 33, Step: 2.25, Front: front, Back: back, Core: core, CoreY: 12})
	if err != nil {
		return err
	}
	return nil
}

func (d *PhenomenaDemo) Update() error {
	if !d.initialized {
		if err := d.Init(); err != nil {
			return err
		}
	}

	d.hueMotion.Step()
	if err := d.sliceProgram.Step(); err != nil {
		return err
	}
	d.t += .30

	return nil
}

func (d *PhenomenaDemo) Draw(screen *ebiten.Image) {
	if !d.initialized {
		return
	}

	screen.Fill(color.Black)
	// Fill the middle section with the panel's dark blue background.
	vector.DrawFilledRect(screen, 0, 162, 800, 375, color.RGBA{0x00, 0x01, 0x11, 0xFF}, false)

	logoOp := &ebiten.DrawImageOptions{}
	logoOp.GeoM.Translate(80, 0)
	screen.DrawImage(d.imgLogo, logoOp)

	op := &ebiten.DrawImageOptions{}
	op.GeoM.Translate(0, 129)
	screen.DrawImage(d.rasterGrad800, op)

	op.GeoM.Reset()
	op.GeoM.Translate(0, 537)
	screen.DrawImage(d.rasterGrad800, op)

	op = &ebiten.DrawImageOptions{}
	hue := d.hueMotion.At(0) / 360.0
	r, g, b := palette.HSLToRGB(hue, 1.0, 0.5)
	op.ColorScale.Scale(float32(r), float32(g), float32(b), 1)
	op.GeoM.Translate(365, 555)
	screen.DrawImage(d.imgPhotonMask, op)

	d.drawScroller(screen)
}

func (d *PhenomenaDemo) drawScroller(screen *ebiten.Image) {
	if err := d.rowWave.Begin(d.t); err != nil {
		panic(err)
	}
	d.sliceProgram.Draw(screen, d.dnaFrames, d.dnaDraw)
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
	nbCubes3           = 12
	cocoScrollPadding3 = "     "
	cocoScrollText3    = cocoScrollPadding3 + cocoScrollPadding3 +
		"WELCOME TO THE COCO IS THE BEST DEMO! " + cocoScrollPadding3 +
		"THIS DEMO COMBINES THE BEST EFFECTS FROM VARIOUS ATARI ST DEMOS. " + cocoScrollPadding3 +
		"GREETINGS TO ALL DEMOSCENE LOVERS! " + cocoScrollPadding3 + cocoScrollPadding3
)

type CocoDemo struct {
	scroll      *scrolling.Scrolling
	initialized bool

	titleImg   *ebiten.Image
	barsImg    *ebiten.Image
	cocoImg    *ebiten.Image
	dmaLogoImg *ebiten.Image
	fontImg    *ebiten.Image

	// Batched 3D cube procession with reanchored harmonic motion.
	cubeTrain *effects.SolidCubeTrain

	// Shared sixteen-logo grid with reanchored harmonic translation.
	logoFormation *sprites.Group

	// Shared harmonic backdrop with Coco's embedded texture phase.
	roto *composite.RotozoomBackground

	// Shared direct title/copper band; it owns both animation clocks.
	titleBand *composite.CopperTitleBand

	// VBL counter
	iteration int
}

func NewCocoDemo() *CocoDemo {
	d := &CocoDemo{}

	var err error
	d.cubeTrain, err = effects.NewSolidCubeTrain(presets.MultiscreenCocoCubeTrain(demoWidth, demoHeight, 40, nbCubes3))
	if err != nil {
		panic(err)
	}
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
	if d.titleImg != nil {
		d.titleBand, err = composite.NewCopperTitleBand(presets.CocoTitleBand(
			d.titleImg, d.barsImg, demoWidth, composite.CopperTitleDirect))
		if err != nil {
			return err
		}
	}

	img, _, err = image.Decode(bytes.NewReader(demo3CocoData))
	if err != nil {
		log.Printf("Error loading coco: %v", err)
	} else {
		d.cocoImg = ebiten.NewImageFromImage(img)
		program, err := presets.NewVivaRotozoom(presets.MultiscreenCocoRotozoom(demoWidth, demoHeight))
		if err != nil {
			return err
		}
		d.roto, err = composite.NewRotozoomBackground(composite.RotozoomBackgroundConfig{Image: d.cocoImg, Program: program})
		if err != nil {
			return err
		}
	}

	img, _, err = image.Decode(bytes.NewReader(demo3DmaLogoData))
	if err != nil {
		log.Printf("Error loading DMA logo: %v", err)
	} else {
		d.dmaLogoImg = ebiten.NewImageFromImage(img)
		d.logoFormation, err = sprites.NewGroup(presets.MultiscreenCocoLogoFormation(d.dmaLogoImg, demoWidth, demoHeight))
		if err != nil {
			return err
		}
	}

	img, _, err = image.Decode(bytes.NewReader(demo3FontData))
	if err != nil {
		log.Printf("Error loading font: %v", err)
	} else {
		d.fontImg = ebiten.NewImageFromImage(img)
		atlas, err := presets.FontAtlas("multiscreen-coco", d.fontImg)
		if err != nil {
			return err
		}
		config, err := presets.MultiscreenCocoScanlineScroll(atlas, cocoScrollText3)
		if err != nil {
			return err
		}
		d.scroll, err = scrolling.New(scrolling.Config{Scanline: &config})
		if err != nil {
			return err
		}
	}

	d.initialized = true
	return nil
}

func (d *CocoDemo) Update() error {
	if !d.initialized {
		if err := d.Init(); err != nil {
			return err
		}
	}

	d.iteration++
	if d.scroll != nil {
		if err := d.scroll.Update(kit.Frame{Tick: uint64(d.iteration)}); err != nil {
			return err
		}
	}

	if d.titleBand != nil {
		if err := d.titleBand.Advance(1); err != nil {
			return err
		}
	}

	if err := d.cubeTrain.Update(kit.Frame{}); err != nil {
		return err
	}

	if d.logoFormation != nil {
		if err := d.logoFormation.Update(kit.Frame{}); err != nil {
			return err
		}
	}

	if d.roto != nil {
		if err := d.roto.Update(kit.Frame{}); err != nil {
			return err
		}
	}

	return nil
}

func (d *CocoDemo) Draw(screen *ebiten.Image) {
	if !d.initialized {
		return
	}

	screen.Fill(color.RGBA{0x00, 0x00, 0x30, 0xFF})

	// 1. Rotozoom background
	if d.roto != nil {
		d.roto.Draw(screen)
	}

	// 2. Scrolling text with the panel's three-pixel strip configuration.
	if d.scroll != nil {
		d.scroll.Draw(screen)
	}

	// 3. Synchronized DMA logo formation
	if d.logoFormation != nil {
		d.logoFormation.Draw(screen)
	}

	// 4. Batched 3D cubes
	d.cubeTrain.Draw(screen)

	// 5. Title logo with copper bars on top.
	if d.titleBand != nil {
		d.titleBand.Draw(screen)
	}
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

	roto *composite.RotozoomBackground

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
		program, err := presets.NewVivaRotozoom(presets.MultiscreenVivaRotozoom(demoWidth, demoHeight))
		if err != nil {
			return err
		}
		d.roto, err = composite.NewRotozoomBackground(composite.RotozoomBackgroundConfig{Image: d.tileImg, Program: program})
		if err != nil {
			return err
		}
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

func (d *VivaDemo) Update() error {
	if !d.initialized {
		if err := d.Init(); err != nil {
			return err
		}
	}

	if d.roto != nil {
		if err := d.roto.Update(kit.Frame{}); err != nil {
			return err
		}
	}

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

	if d.roto != nil {
		d.roto.Draw(screen)
	}

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

	audioContext *audio.Context
	audioPlayer  *audio.Player
	musicStream  *sound.Stream

	tourRenderer *composite.SceneTour
}

func NewMegaDemoGame() *MegaDemoGame {
	g := &MegaDemoGame{
		demo1:        NewPhenomenaDemo(),
		demo2:        NewTCBDemo(),
		demo3:        NewCocoDemo(),
		demo4:        NewVivaDemo(),
		audioContext: audio.NewContext(sampleRate),
	}
	g.renderer()

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

func (g *MegaDemoGame) renderer() *composite.SceneTour {
	if g.tourRenderer != nil {
		return g.tourRenderer
	}
	camera, err := motion.NewCameraTour(presets.MultiscreenCameraTour())
	if err != nil {
		panic(err)
	}
	config := composite.SceneTourConfig{
		Camera: camera, TileWidth: demoWidth, TileHeight: demoHeight,
		ViewportWidth: demoWidth, ViewportHeight: demoHeight,
		Sources: []composite.TourSource{g.demo1, g.demo2, g.demo3, g.demo4},
		Names:   []string{"phenomena demo", "tcb demo", "coco demo", "viva demo"},
		TileOrigins: []image.Point{
			image.Pt(0, 0), image.Pt(demoWidth, 0),
			image.Pt(demoWidth, demoHeight), image.Pt(0, demoHeight),
		},
		ShaderSource: []byte(presets.MultiscreenCompositeShaderSource),
		OnShaderError: func(err error) {
			log.Printf("Failed to compile camera compositor shader: %v", err)
		},
	}
	g.tourRenderer, err = composite.NewSceneTour(config)
	if err != nil {
		panic(err)
	}
	return g.tourRenderer
}

func (g *MegaDemoGame) Update() error {
	return g.renderer().Update()
}

func (g *MegaDemoGame) Draw(screen *ebiten.Image) {
	g.renderer().Draw(screen)
}

func (g *MegaDemoGame) Layout(outsideWidth, outsideHeight int) (int, int) {
	return 800, 600
}
