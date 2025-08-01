package parser

import (
	"bufio"
	"strings"
	"testing"
)

func TestCreatePeekRead(t *testing.T) {
	example0 := "a1\n. \n"
	src := &source{
		reader: bufio.NewReader(strings.NewReader(example0)),
		peeked: 0,
	}
	src.peeked, _, _ = src.reader.ReadRune()

	ch := src.peek()
	if ch != 'a' {
		t.Errorf("Սպասվում էր 'a' բայց ստացվել է '%c':", ch)
	}
	ch = src.peek()
	if ch != 'a' {
		t.Errorf("Սպասվում էր 'a' բայց ստացվել է '%c':", ch)
	}
	ch = src.read()
	if ch != 'a' {
		t.Errorf("Սպասվում էր 'a' բայց ստացվել է '%c':", ch)
	}
	ch = src.peek()
	if ch != '1' {
		t.Errorf("Սպասվում էր 'a' բայց ստացվել է '%c':", ch)
	}
}
