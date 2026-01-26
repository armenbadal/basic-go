package parser

import "slices"

type token int

//go:generate stringer -type=token -linecomment -output=token_string.go
const (
	xNone token = iota

	// լիտերալներ
	xNumber // թիվ
	xText   // տեքստ
	xIdent  // իդենտիֆիկատոր
	xTrue   // TRUE
	xFalse  // FALSE

	// ծառայողական բառեր
	xSubroutine // SUB
	xDim        // DIM
	xLet        // LET
	xInput      // INPUT
	xPrint      // PRINT
	xIf         // IF
	xThen       // THEN
	xElseIf     // ELSEIF
	xElse       // ELSE
	xWhile      // WHILE
	xFor        // FOR
	xTo         // TO
	xStep       // STEP
	xCall       // CALL
	xEnd        // END

	// գործողություններ
	xAdd // +
	xSub // -
	xAmp // &
	xMul // *
	xDiv // /
	xMod // \
	xPow // ^

	xEq // =
	xNe // <>
	xGt // >
	xGe // >=
	xLt // <
	xLe // <=

	xAnd // AND
	xOr  // OR
	xNot // NOT

	// կետադրական նշաններ
	xNewLine  // <-/
	xLeftPar  // (
	xRightPar // )
	xLeftBr   // [
	xRightBr  // ]
	xComma    // ,

	xEof // ֆայլի վերջը
)

type lexeme struct {
	token token  // պիտակ
	value string // արժեք
	line  int    // տողը
}

func (l *lexeme) is(exps ...token) bool {
	return slices.Contains(exps, l.token)
}
