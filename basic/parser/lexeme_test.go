package parser

import "testing"

func TestTokenIs(t *testing.T) {
	a0 := &lexeme{token: xNone, value: "", line: 0}
	if !a0.is(xNone) {
		t.Error("Invalid lexeme")
	}
}
