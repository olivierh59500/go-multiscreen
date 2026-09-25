package multiscreen

import (
	"testing"

	"github.com/olivierh59500/democonstructionkit/presets"
)

func legacyAtlasIndex(ch rune) (int, bool) {
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
func TestSharedAtlasIndicesMatchOriginalAlphabet(t *testing.T) {
	for r := rune(0); r < 256; r++ {
		got, ok := charToFontIndexPhe(r)
		want, found := legacyAtlasIndex(r)
		if got != want || ok != found {
			t.Fatalf("rune %U: got (%d,%t), want (%d,%t)", r, got, ok, want, found)
		}
	}
}

func legacymapCharToFont4(charCode int) int {
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
func TestSharedmapCharToFont4MatchesOriginal(t *testing.T) {
	lookup, err := presets.TileLookup("multiscreen-viva", false)
	if err != nil {
		t.Fatal(err)
	}
	for r := 0; r < 256; r++ {
		got, _ := lookup(rune(r))
		if want := legacymapCharToFont4(int(r)); got != want {
			t.Fatalf("rune %U: got %d, want %d", r, got, want)
		}
	}
}
