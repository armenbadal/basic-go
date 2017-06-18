
package parser

import "fmt"

const (
	xNone = iota

	xNumber
	xText
	xIdent

	xSubroutine
	xLet
	xInput
	xPrint
	xIf
	xThen
	xElseIf
	xElse
	xWhile
	xFor
	xTo
	xStep
	xCall
	xEnd
	
	xAdd
	xSub
	xAmp
	xMul
	xDiv
	xMod
	xPow

	xEq
	xNe
	xGt
	xGe
	xLt
	xLe

	xAnd
	xOr
	xNot
	
	xNewLine
	xLeftPar
	xRightPar
	xComma

	xEof
)

type lexeme struct {
	token int
	value string
	line int
}

func (l *lexeme) is(exps ...int) bool {
	for _, e :=range exps {
		if e == l.token {
			return true
		}
	}
	return false
}

func (l *lexeme) ToString() string {
	return fmt.Sprintf("<%d,\t%s,\t%d>", l.token, l.value, l.line)
}



