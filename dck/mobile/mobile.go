//go:build android || ios

// Package multiscreenmobile exposes the demo to Ebitengine's native mobile view.
package multiscreenmobile

import (
	"github.com/hajimehoshi/ebiten/v2"
	"github.com/hajimehoshi/ebiten/v2/mobile"
	multiscreen "multiscreen-mega-demo/dck"
)

func init() {
	ebiten.SetScreenClearedEveryFrame(false)
	mobile.SetGame(&game{})
}

// game delays image and audio creation until Android has initialized its
// application context and rendering surface.
type game struct {
	delegate *multiscreen.MegaDemoGame
}

func (g *game) Update() error {
	if g.delegate == nil {
		g.delegate = multiscreen.NewMegaDemoGame()
	}
	return g.delegate.Update()
}

func (g *game) Draw(screen *ebiten.Image) {
	if g.delegate != nil {
		g.delegate.Draw(screen)
	}
}

func (g *game) Layout(_, _ int) (int, int) {
	return 800, 600
}

// Dummy ensures gomobile emits bindings for this package.
func Dummy() {}
