// Command capture-tcb writes deterministic frames of the embedded TCB scene.
package main

import (
	"flag"
	"fmt"
	"os"
	"strconv"
	"strings"

	"github.com/hajimehoshi/ebiten/v2"
	capture "github.com/olivierh59500/democonstructionkit/fidelity/ebiten"
	multiscreen "multiscreen-mega-demo/dck"
)

type tcbGame struct{ *multiscreen.TCBDemo }

func (*tcbGame) Layout(int, int) (int, int) { return 800, 600 }

func main() {
	framesFlag := flag.String("frames", "1,12,13,38,39,240", "comma-separated capture ticks")
	out := flag.String("out", "captures/tcb", "capture directory")
	flag.Parse()
	var frames []int
	for _, part := range strings.Split(*framesFlag, ",") {
		n, err := strconv.Atoi(part)
		if err != nil || n < 0 || len(frames) > 0 && n <= frames[len(frames)-1] {
			fmt.Fprintln(os.Stderr, "frames must be increasing nonnegative integers")
			os.Exit(1)
		}
		frames = append(frames, n)
	}
	if err := capture.Run(capture.Config{Directory: *out, Frames: frames, Width: 800, Height: 600}, func() (ebiten.Game, error) {
		return &tcbGame{TCBDemo: multiscreen.NewTCBDemo()}, nil
	}); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
