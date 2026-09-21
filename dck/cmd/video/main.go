// Command video exports the complete game canvas and its own audio.
package main

import (
	"flag"
	"log"
	"time"

	"github.com/hajimehoshi/ebiten/v2"
	"github.com/olivierh59500/democonstructionkit/video"
	demo "multiscreen-mega-demo/dck"
)

func main() {
	config := video.Config{Output: "go-multiscreen.mp4", Title: "Multiscreen Mega Demo", Width: 1600, Height: 1200, FPS: 60, TPS: 60, SampleRate: 44100, Duration: 3 * time.Minute}
	config.Flags(flag.CommandLine)
	flag.Parse()
	if err := video.Run(config, func() (ebiten.Game, error) {
		return demo.NewMegaDemoGame(), nil
	}); err != nil {
		log.Fatal(err)
	}
}
