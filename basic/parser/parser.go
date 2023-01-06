package parser

import (
	"basic/ast"
	"bufio"
	"errors"
	"fmt"
	"os"
	"strconv"
	"strings"
)

// Parser Շարահյուսական վերլուծիչի ստրուկտուրան։
type Parser struct {
	// բառային վերլուծիչի ցուցիչ
	scer *scanner
	// look-a-head սիմվոլ
	lookahead *lexeme

	// վերլուծության ծառի արմատ
	program *ast.Program
}

// New Ստեղծում և վերադարձնում է շարահյուսական վերլուծիչի նոր օբյեկտ։
func New(filename string) (*Parser, error) {
	// բացել ֆայլային հոսքը
	rd, er := os.Open(filename)
	if er != nil {
		return nil, errors.New("ֆայլը բացելը ձախողվեց")
	}
	defer rd.Close()

	// ստեղծել շարահյուսական վերլուծիչի օբյեկտը
	pars := new(Parser)
	pars.scer = new(scanner)
	pars.scer.source = bufio.NewReader(rd)
	pars.scer.line = 1
	pars.scer.read()
	pars.lookahead = pars.scer.next()

	pars.program = &ast.Program{Members: make(map[string]ast.Node)}

	return pars, nil
}

// Parse Վերլուծությունը սկսող արտաքին ֆունկցիա
func (p *Parser) Parse() (*ast.Program, error) {
	// շարահյուսական սխալի արտածում
	defer func() {
		if err := recover(); err != nil {
			// TODO: print error message
			fmt.Printf("Parse Error: %s\n", err)
		}
	}()

	// վերլուծել ծրագիրը
	p.parseProgram()
	return p.program, nil
}

// Վերլուծել ամբողջ ծրագիրը.
//
// Program = { Subroutine }.
func (p *Parser) parseProgram() {
	// բաց թողնել ֆայլի սկզբի դատարկ տողերը
	for p.has(xNewLine) {
		p.lookahead = p.scer.next()
	}

	for p.has(xSubroutine) {
		p0 := p.parseSubroutine()
		p.program.Members[p0.Name] = p0
		p.parseNewLines()
	}
}

// Վերլուծել նոր տողերի նիշերի հաջորդականությունը
//
// NewLines = NEWLINE { NEWLINE }.
func (p *Parser) parseNewLines() {
	p.match(xNewLine)
	for p.lookahead.is(xNewLine) {
		p.match(xNewLine)
	}
}

// Վերլուծել ենթածրագիրը
//
// Subroutine = 'SUB' IDENT ['(' [IDENT {',' IDENT}] ')'] Sequence 'END' SUB'.
func (p *Parser) parseSubroutine() *ast.Subroutine {
	// վերնագրի վերլուծություն
	p.match(xSubroutine)
	name := p.lookahead.value
	p.match(xIdent)
	pars := make([]string, 0)
	if p.has(xLeftPar) {
		p.match(xLeftPar)
		if p.has(xIdent) {
			pnm := p.lookahead.value
			p.match(xIdent)
			pars = append(pars, pnm)
			for p.has(xComma) {
				p.match(xComma)
				pnm = p.lookahead.value
				p.match(xIdent)
				pars = append(pars, pnm)
			}
		}
		p.match(xRightPar)
	}

	// մարմնի վերլուծություն
	body := p.parseSequence()

	p.match(xEnd)
	p.match(xSubroutine)

	// նոր ենթածրագրի օբյեկտ
	return &ast.Subroutine{Name: name, Parameters: pars, Body: body}
}

// Վերլուծել հրամանների հաջորդականություն
//
// Sequence = NewLines { Statement NewLines }.
func (p *Parser) parseSequence() ast.Node {
	p.parseNewLines()
	seq := &ast.Sequence{Items: make([]ast.Node, 0)}
loop:
	for {
		var stat ast.Node
		switch {
		case p.has(xDim):
			stat = p.parseDim()
		case p.has(xLet):
			stat = p.parseLet()
		case p.has(xInput):
			stat = p.parseInput()
		case p.has(xPrint):
			stat = p.parsePrint()
		case p.has(xIf):
			stat = p.parseIf()
		case p.has(xWhile):
			stat = p.parseWhile()
		case p.has(xFor):
			stat = p.parseFor()
		case p.has(xCall):
			stat = p.parseCall()
		default:
			break loop
		}
		p.parseNewLines()
		seq.Items = append(seq.Items, stat)
	}
	return seq
}

// Վերլուծել զանգվածի սահմանման հրամանը
//
// Statement = 'DIM' IDENT '[' Expression ']'.
func (p *Parser) parseDim() ast.Node {
	p.match(xDim)
	nm := p.lookahead.value
	p.match(xIdent)
	p.match(xLeftBr)
	sz := p.parseExpression()
	p.match(xRightBr)
	return &ast.Dim{Name: nm, Size: sz}
}

// Վերլուծել վերագրման հրամանը
//
// Statement = 'LET' IDENT '=' Expression.
func (p *Parser) parseLet() ast.Node {
	p.match(xLet)
	var pl ast.Node = &ast.Variable{Name: p.lookahead.value}
	p.match(xIdent)
	for p.has(xLeftBr) {
		p.match(xLeftBr)
		e := p.parseExpression()
		p.match(xRightBr)
		pl = &ast.Binary{Operation: "[]", Left: pl, Right: e}
	}

	p.match(xEq)
	e0 := p.parseExpression()
	return &ast.Let{Place: pl, Value: e0}
}

// Ներմուծման հրամանի վերլուծությունը.
//
// Statement = 'INPUT' IDENT.
func (p *Parser) parseInput() ast.Node {
	p.match(xInput)
	name := &ast.Variable{Name: p.lookahead.value}
	p.match(xIdent)
	return &ast.Input{Place: name}
}

// Արտածման հրամանի վերլուծությունը.
//
// Statement = 'PRINT' Expression.
func (p *Parser) parsePrint() ast.Node {
	p.match(xPrint)
	e0 := p.parseExpression()
	return &ast.Print{Value: e0}
}

// Ճյուղավորման հրամանի վերլուծությունը.
//
// Statement = 'IF' Expression 'THEN' Sequence
//
//	{ 'ELSEIF' Expression 'THEN' Sequence }
//	[ 'ELSE' Sequence ]
//	'END' 'IF'.
func (p *Parser) parseIf() ast.Node {
	p.match(xIf)
	c0 := p.parseExpression()
	p.match(xThen)
	s0 := p.parseSequence()
	res := &ast.If{Condition: c0, Decision: s0}
	ipe := res
	for p.has(xElseIf) {
		p.match(xElseIf)
		c1 := p.parseExpression()
		p.match(xThen)
		s1 := p.parseSequence()
		alt := &ast.If{Condition: c1, Decision: s1}
		ipe.Alternative = alt
		ipe = alt
	}
	if p.has(xElse) {
		p.match(xElse)
		s2 := p.parseSequence()
		ipe.Alternative = s2
	}
	p.match(xEnd)
	p.match(xIf)
	return res
}

// Նախապայմանով ցիկլի վերլուծությունը
//
// Statement = 'WHILE' Expression Sequence 'END' 'WHILE'.
func (p *Parser) parseWhile() ast.Node {
	p.match(xWhile)
	c0 := p.parseExpression()
	b0 := p.parseSequence()
	p.match(xEnd)
	p.match(xWhile)
	return &ast.While{Condition: c0, Body: b0}
}

// Պարամետրով ցիկլի վերլուծությունը
//
// Statement = 'FOR' IDENT '=' Expression 'TO' Expression
//
//	['STEP' ['+'|'-'] NUMBER Sequence
//	'END' 'FOR'.
func (p *Parser) parseFor() ast.Node {
	p.match(xFor)

	param := &ast.Variable{Name: p.lookahead.value}
	p.match(xIdent)
	p.match(xEq)
	begin := p.parseExpression()

	p.match(xTo)
	end := p.parseExpression()

	var step ast.Node
	if p.has(xStep) {
		p.match(xStep)
		sign := "+"
		if p.has(xSub) {
			p.match(xSub)
			sign = "-"
		} else if p.has(xAdd) {
			p.match(xAdd)
		}

		lex := p.lookahead.value
		p.match(xNumber)
		num, _ := strconv.ParseFloat(lex, 64)
		step = &ast.Number{Value: num}
		if sign == "-" {
			step = &ast.Unary{Operation: sign, Right: step}
		}
	} else {
		step = &ast.Number{Value: 1.0}
	}

	body := p.parseSequence()

	p.match(xEnd)
	p.match(xFor)

	return &ast.For{
		Parameter: param,
		Begin:     begin,
		End:       end,
		Step:      step,
		Body:      body}
}

// Ենթածրագրի կանչի վերլուծությունը
//
// Statement = 'CALL' IDENT [Expression {',' Expression}].
func (p *Parser) parseCall() ast.Node {
	p.match(xCall)
	name := p.lookahead.value
	p.match(xIdent)
	args := make([]ast.Node, 0)
	if p.isExprFirst() {
		e0 := p.parseExpression()
		args = append(args, e0)
		for p.has(xComma) {
			p.match(xComma)
			e1 := p.parseExpression()
			args = append(args, e1)
		}
	}

	return &ast.Call{Callee: name, Arguments: args}
}

// Արտահայտություն
//
// Expression = Conjunction { OR Conjunction }.
func (p *Parser) parseExpression() ast.Node {
	res := p.parseConjunction()
	for p.has(xOr) {
		p.match(xOr)
		e0 := p.parseConjunction()
		res = &ast.Binary{Operation: "OR", Left: res, Right: e0}
	}
	return res
}

// Կոնյունկցիա
//
// Conjunction = Equality { AND Equality }.
func (p *Parser) parseConjunction() ast.Node {
	res := p.parseEquality()
	for p.has(xAnd) {
		p.match(xAnd)
		e0 := p.parseEquality()
		res = &ast.Binary{Operation: "AND", Left: res, Right: e0}
	}
	return res
}

// Հավասարություն
//
// Equality = Comparison [('=' | '<>') Comparison].
func (p *Parser) parseEquality() ast.Node {
	res := p.parseComparison()
	if p.has(xEq, xNe) {
		var opc string
		switch p.lookahead.token {
		case xEq:
			opc = "="
			p.match(xEq)
		case xNe:
			opc = "<>"
			p.match(xNe)
		}
		e0 := p.parseComparison()
		res = &ast.Binary{Operation: opc, Left: res, Right: e0}
	}
	return res
}

// Համեմատություն
//
// Comparison = Addition [('>' | '>=' | '<' | '<=') Addition].
func (p *Parser) parseComparison() ast.Node {
	res := p.parseAddition()
	if p.has(xGt, xGe, xLt, xLe) {
		var opc string
		switch p.lookahead.token {
		case xGt:
			opc = ">"
			p.match(xGt)
		case xGe:
			opc = ">="
			p.match(xGe)
		case xLt:
			opc = "<"
			p.match(xLt)
		case xLe:
			opc = "<="
			p.match(xLe)
		}
		e0 := p.parseAddition()
		res = &ast.Binary{Operation: opc, Left: res, Right: e0}
	}
	return res
}

// Գումարում, հանում կամ տողերի կոնկատենացիա
//
// Addition = Multiplication {('+' | '-' | '&') Multiplication}.
func (p *Parser) parseAddition() ast.Node {
	res := p.parseMultiplication()
	for p.has(xAdd, xSub, xAmp) {
		var opc string
		switch p.lookahead.token {
		case xAdd:
			opc = "+"
			p.match(xAdd)
		case xSub:
			opc = "-"
			p.match(xSub)
		case xAmp:
			opc = "&"
			p.match(xAmp)
		}
		e0 := p.parseMultiplication()
		res = &ast.Binary{Operation: opc, Left: res, Right: e0}
	}
	return res
}

// Բազմապատկում, բաժանում կամ մնացորդ
//
// Multiplication = Power {('*' | '/' | '\') Power}.
func (p *Parser) parseMultiplication() ast.Node {
	res := p.parsePower()
	for p.has(xMul, xDiv, xMod) {
		var opc string
		switch p.lookahead.token {
		case xMul:
			opc = "*"
			p.match(xMul)
		case xDiv:
			opc = "/"
			p.match(xDiv)
		case xMod:
			opc = "\\"
			p.match(xMod)
		}
		e0 := p.parsePower()
		res = &ast.Binary{Operation: opc, Left: res, Right: e0}
	}
	return res
}

// Ատիճան բարձրացնելու գործողությունը
//
// Power = Index ['^' Power].
func (p *Parser) parsePower() ast.Node {
	res := p.parseSubscript()
	if p.has(xPow) {
		p.match(xPow)
		e0 := p.parsePower()
		res = &ast.Binary{Operation: "^", Left: res, Right: e0}
	}
	return res
}

// Ինդեքսավորման գործողությունը
//
// Index = Factor {'[' Expression ']'}.
func (p *Parser) parseSubscript() ast.Node {
	res := p.parseFactor()
	for p.has(xLeftBr) {
		p.match(xLeftBr)
		e0 := p.parseExpression()
		p.match(xRightBr)
		res = &ast.Binary{Operation: "[]", Left: res, Right: e0}
	}
	return res
}

// Պարզագույն արտահայտությունների վերլուծությունը
//
// Factor = TRUE | FALSE | NUMBER | TEXT
//
//	| IDENT
//	| IDENT '(' [Expression {',' Expression}] ')'
//	| '[' [Expression {',' Expression}] ']'
//	| SUB Factor
//	| NOT Factor
//	| '(' Expression ')'.
func (p *Parser) parseFactor() ast.Node {
	var result ast.Node

	switch {
	case p.has(xTrue, xFalse):
		result = p.parseTrueOrFalse()
	case p.has(xNumber):
		result = p.parseNumber()
	case p.has(xText):
		result = p.parseText()
	case p.has(xLeftBr):
		result = p.parseArrayLiteral()
	case p.has(xIdent):
		result = p.parseIdentOrApply()
	case p.has(xSub, xNot):
		result = p.parseUnary()
	case p.has(xLeftPar):
		result = p.parseGrouping()
	default:
		panic("պարզագույն արտահայտության սխալ")
	}

	return result
}

// տրամաբանական լիտերալ, TRUE կամ FALSE
func (p *Parser) parseTrueOrFalse() ast.Node {
	lex := strings.ToUpper(p.lookahead.value)
	p.match(p.lookahead.token)
	return &ast.Boolean{Value: lex == "TRUE"}
}

// թվային լիտերալ
func (p *Parser) parseNumber() ast.Node {
	lex := p.lookahead.value
	p.match(xNumber)
	val, _ := strconv.ParseFloat(lex, 64)
	return &ast.Number{Value: val}
}

// տեքստային լիտերալ
func (p *Parser) parseText() ast.Node {
	val := p.lookahead.value
	p.match(xText)
	return &ast.Text{Value: val}
}

// զանգվածի լիտերալ
func (p *Parser) parseArrayLiteral() ast.Node {
	elems := make([]ast.Node, 0)
	p.match(xLeftBr)
	e := p.parseExpression()
	elems = append(elems, e)
	for p.has(xComma) {
		p.match(xComma)
		e := p.parseExpression()
		elems = append(elems, e)
	}
	p.match(xRightBr)
	return &ast.Array{Elements: elems}
}

// իդենտիֆիկատոր կամ ֆունկցիա-ենթածրագրի կանչ
func (p *Parser) parseIdentOrApply() ast.Node {
	name := p.lookahead.value
	p.match(xIdent)

	if p.has(xLeftPar) {
		p.match(xLeftPar)
		args := make([]ast.Node, 0)
		if p.isExprFirst() {
			e0 := p.parseExpression()
			args = append(args, e0)
			for p.has(xComma) {
				p.match(xComma)
				e1 := p.parseExpression()
				args = append(args, e1)
			}
		}
		p.match(xRightPar)
		return &ast.Apply{Callee: name, Arguments: args}
	}

	return &ast.Variable{Name: name}
}

// ունար գործողություն
func (p *Parser) parseUnary() ast.Node {
	var opc string
	switch p.lookahead.token {
	case xSub:
		opc = "-"
		p.match(xSub)
	case xNot:
		opc = "NOT"
		p.match(xNot)
	}
	res := p.parseFactor()
	res = &ast.Unary{Operation: opc, Right: res}
	return res
}

// փակագծեր
func (p *Parser) parseGrouping() ast.Node {
	p.match(xLeftPar)
	res := p.parseExpression()
	p.match(xRightPar)
	return res
}

func (p *Parser) has(tokens ...int) bool {
	return p.lookahead.is(tokens...)
}

func (p *Parser) isExprFirst() bool {
	return p.has(xTrue, xFalse, xNumber, xText, xIdent, xSub, xNot, xLeftPar, xLeftBr)
}

func (p *Parser) match(exp int) {
	if p.lookahead.is(exp) {
		p.lookahead = p.scer.next()
	} else {
		panic(fmt.Sprintf("Տող %d. Վերլուծության սխալ\n", p.lookahead.line))
	}
}
