package main

import (
	"log"

	"github.com/hajimehoshi/ebiten/v2"
	multiscreen "multiscreen-mega-demo/dck"
)

func main() {
	ebiten.SetWindowSize(800, 600)
	ebiten.SetWindowTitle("Multiscreen remake or original Mega Demo by DMA")
	ebiten.SetVsyncEnabled(true)
	ebiten.SetScreenClearedEveryFrame(false)

	if err := ebiten.RunGame(multiscreen.NewMegaDemoGame()); err != nil {
		log.Fatal(err)
	}
}
