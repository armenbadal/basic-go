package parser

import "fmt"

const (
	xNone = iota

	xNumber // թիվ
	xText   // տեքստ
	xIdent  // իդենտիֆիկատոր

	xSubroutine // SUB
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

	xNewLine  // <-/
	xLeftPar  // (
	xRightPar // )
	xLeftBr   // [
	xRightBr  // ]
	xComma    // ,

	xEof // ֆայլի վերջը
)

type lexeme struct {
	token int
	value string
	line  int
}

func (l *lexeme) is(exps ...int) bool {
	for _, e := range exps {
		if e == l.token {
			return true
		}
	}
	return false
}

func (l *lexeme) ToString() string {
	return fmt.Sprintf("<%d,\t%s,\t%d>", l.token, l.value, l.line)
}
