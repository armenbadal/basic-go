package parser

import (
	"bufio"
	"strings"
	"testing"
)

// Կառուցում և վերադարձնում է scanner օբյեկտը՝ տրված տեքստի համար
func scannerWithInput(sr string) *scanner {
	return &scanner{bufio.NewReader(strings.NewReader(sr)), "", 1}
}

func TestScannerOnlySpaces(t *testing.T) {
	scan := scannerWithInput("   \t\r\t")

	x := scan.next()
	if x.token != xEof {
		t.Fail()
	}
}

func TestScannerCommentWithoutNewline(t *testing.T) {
	scan := scannerWithInput("' comment")

	x := scan.next()
	if x.token != xEof {
		t.Fail()
	}
}

func TestComments(t *testing.T) {
	scan := scannerWithInput(" ' this is a comment\n ' another line of comment\n888")

	x := scan.next()
	if x.token != xNewLine {
		t.Fail()
	}

	y := scan.next()
	if y.token != xNewLine {
		t.Fail()
	}
}

func TestKeywords(t *testing.T) {
	scan := scannerWithInput("SUB LET INPUT PRINT IF THEN ELSEIF ELSE WHILE FOR TO STEP CALL END AND OR NOT")

	expected := []int{xSubroutine, xLet, xInput, xPrint, xIf, xThen, xElseIf, xElse,
		xWhile, xFor, xTo, xStep, xCall, xEnd, xAnd, xOr, xNot, xEof}

	for _, ex := range expected {
		x := scan.next()
		if x.token != ex {
			t.Error(x)
		}
	}
}

func TestIdentifiers(t *testing.T) {
	scan := scannerWithInput("a b$ c0 d1$")

	x := scan.next()
	if x.token != xIdent || x.value != "a" {
		t.Fail()
	}

	y := scan.next()
	if y.token != xIdent || y.value != "b$" {
		t.Fail()
	}
}

func TestNumbers(t *testing.T) {
	scan := scannerWithInput("123 4.56")

	x := scan.next()
	if x.token != xNumber || x.value != "123" {
		t.Fail()
	}

	y := scan.next()
	if y.token != xNumber || y.value != "4.56" {
		t.Fail()
	}
}

func TestTextLiteral(t *testing.T) {
	scan := scannerWithInput(`"bambarbia kirgudu"  "invalid text literal `)

	x := scan.next()
	if !x.is(xText) {
		t.Fail()
	}

	y := scan.next()
	if y.token != xEof {
		t.Error(y)
	}
}

func TestOperations(t *testing.T) {
	scan := scannerWithInput("+ - * / \\ ^ & ( ) [ ] , = <> > >= < <=")

	scan.next()
}
