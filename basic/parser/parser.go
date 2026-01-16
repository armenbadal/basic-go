package parser

import (
	"basic/ast"
	"bufio"
	"fmt"
	"slices"
	"strconv"
	"strings"
)

// Parser Շարահյուսական վերլուծիչի ստրուկտուրան։
type Parser struct {
	scanner   *scanner // բառային վերլուծիչի ցուցիչ
	lookahead *lexeme  // look-a-head սիմվոլ
}

// Ստեղծում և վերադարձնում է շարահյուսական վերլուծիչի նոր օբյեկտ։
func New(reader *bufio.Reader) *Parser {
	// ստեղծել շարահյուսական վերլուծիչի օբյեկտը
	return &Parser{&scanner{reader, -1, "", 1}, nil}
}

// Վերլուծությունը սկսող արտաքին ֆունկցիա
func (p *Parser) Parse() (*ast.Program, error) {
	p.next()
	return p.parseProgram()
}

// Վերլուծել ամբողջ ծրագիրը.
//
// Program = { Subroutine }.
func (p *Parser) parseProgram() (*ast.Program, error) {
	// բաց թողնել ֆայլի սկզբի դատարկ տողերը
	if p.has(xNewLine) {
		p.parseNewLines()
	}

	// վերլուծվելիք ենթածրագրերի ցուցակ
	subroutines := make(map[string]*ast.Subroutine)
	for p.has(xSubroutine) {
		subr, err := p.parseSubroutine()
		if err != nil {
			return nil, err
		}
		subroutines[subr.Name] = subr
		p.parseNewLines()
	}

	program := &ast.Program{Subroutines: subroutines}
	return program, nil
}

// Վերլուծել նոր տողերի նիշերի հաջորդականությունը
//
// NewLines = NEWLINE { NEWLINE }.
func (p *Parser) parseNewLines() {
	p.match(xNewLine)
	for p.lookahead.is(xNewLine) {
		p.next()
	}
}

// Վերլուծել ենթածրագիրը
//
// Subroutine = 'SUB' IDENT ['(' [IDENT {',' IDENT}] ')'] Sequence 'END' SUB'.
func (p *Parser) parseSubroutine() (*ast.Subroutine, error) {
	// վերնագրի վերլուծություն
	p.next() // SUB
	name, err := p.match(xIdent)
	if err != nil {
		return nil, err
	}

	// պարամետրեր
	var parameters []string
	if p.has(xLeftPar) {
		p.next() // '('
		if p.has(xIdent) {
			parameters, err = p.parseIdentList()
			if err != nil {
				return nil, err
			}
		}
		if _, err := p.match(xRightPar); err != nil {
			return nil, err
		}
	}

	// մարմնի վերլուծություն
	body, err := p.parseSequence()
	if err != nil {
		return nil, err
	}

	if _, err := p.match(xEnd); err != nil {
		return nil, err
	}
	if _, err := p.match(xSubroutine); err != nil {
		return nil, err
	}

	// նոր ենթածրագրի օբյեկտ
	return &ast.Subroutine{
		Name:       name,
		Parameters: parameters,
		Body:       body,
	}, nil
}

// Իդենտիֆիկատորների ցուցակ
//
// IdentList = IDENT { ',' IDENT }.
func (p *Parser) parseIdentList() ([]string, error) {
	identifiers := make([]string, 0)

	value, err := p.match(xIdent)
	if err != nil {
		return nil, err
	}
	identifiers = append(identifiers, value)

	for p.has(xComma) {
		p.next() // ','
		value, err = p.match(xIdent)
		if err != nil {
			return nil, err
		}
		identifiers = append(identifiers, value)
	}

	return identifiers, nil
}

// Վերլուծել հրամանների հաջորդականություն
//
// Sequence = NewLines { Statement NewLines }.
func (p *Parser) parseSequence() (*ast.Sequence, error) {
	p.parseNewLines()

	statements := make([]ast.Statement, 0)
	for p.isStatementFirst() {
		stat, err := p.parseStatement()
		if err != nil {
			return nil, err
		}

		p.parseNewLines()
		statements = append(statements, stat)
	}

	return &ast.Sequence{Items: statements}, nil
}

// Վերլուծել մեկ հրաման
func (p *Parser) parseStatement() (ast.Statement, error) {
	switch {
	case p.has(xDim):
		return p.parseDim()
	case p.has(xLet):
		return p.parseLet()
	case p.has(xInput):
		return p.parseInput()
	case p.has(xPrint):
		return p.parsePrint()
	case p.has(xIf):
		return p.parseIf()
	case p.has(xWhile):
		return p.parseWhile()
	case p.has(xFor):
		return p.parseFor()
	case p.has(xCall):
		return p.parseCall()
	}

	return nil, fmt.Errorf("սպասվում է հրամանի սկիզբ. DIM, LET, INPUT, PRINT, IF, WHILE, FOR, CALL, բայց հանդիպել է %s", p.lookahead.value)
}

// Վերլուծել զանգվածի սահմանման հրամանը
//
// Statement = 'DIM' IDENT '[' Expression ']'.
func (p *Parser) parseDim() (ast.Statement, error) {
	p.next() // DIM
	name, err := p.match(xIdent)
	if err != nil {
		return nil, err
	}
	if _, err := p.match(xLeftBr); err != nil {
		return nil, err
	}
	sz, err := p.parseExpression()
	if err != nil {
		return nil, err
	}
	if _, err := p.match(xRightBr); err != nil {
		return nil, err
	}
	return &ast.Dim{Name: name, Size: sz}, nil
}

// Վերլուծել վերագրման հրամանը
//
// Statement = 'LET' IDENT ['[' Expression ']'] '=' Expression.
func (p *Parser) parseLet() (ast.Statement, error) {
	p.next() // LET
	name, err := p.match(xIdent)
	if err != nil {
		return nil, err
	}
	var place ast.Expression = &ast.Variable{Name: name}

	for p.has(xLeftBr) {
		p.next() // '('
		index, err := p.parseExpression()
		if err != nil {
			return nil, err
		}
		if _, err := p.match(xRightBr); err != nil {
			return nil, err
		}
		place = &ast.Binary{Operation: "[]", Left: place, Right: index}
	}

	if _, err := p.match(xEq); err != nil {
		return nil, err
	}

	assignable, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	return &ast.Let{Place: place, Value: assignable}, nil
}

// Ներմուծման հրամանի վերլուծությունը.
//
// Statement = 'INPUT' IDENT.
func (p *Parser) parseInput() (ast.Statement, error) {
	p.next() // INPUT
	name, err := p.match(xIdent)
	if err != nil {
		return nil, err
	}

	return &ast.Input{Place: &ast.Variable{Name: name}}, nil
}

// Արտածման հրամանի վերլուծությունը.
//
// Statement = 'PRINT' Expression.
func (p *Parser) parsePrint() (ast.Statement, error) {
	p.next() // PRINT
	value, err := p.parseExpression()
	if err != nil {
		return nil, err
	}
	return &ast.Print{Value: value}, nil
}

// Ճյուղավորման հրամանի վերլուծությունը.
//
// Statement = 'IF' Expression 'THEN' Sequence
//
//	{ 'ELSEIF' Expression 'THEN' Sequence }
//	[ 'ELSE' Sequence ]
//	'END' 'IF'.
func (p *Parser) parseIf() (ast.Statement, error) {
	p.next() // IF
	c0, err := p.parseExpression()
	if err != nil {
		return nil, err
	}
	if _, err := p.match(xThen); err != nil {
		return nil, err
	}
	s0, err := p.parseSequence()
	if err != nil {
		return nil, err
	}
	res := &ast.If{Condition: c0, Decision: s0}
	ipe := res
	for p.has(xElseIf) {
		p.next() // ELSEIF
		c1, err := p.parseExpression()
		if err != nil {
			return nil, err
		}
		if _, err := p.match(xThen); err != nil {
			return nil, err
		}
		s1, err := p.parseSequence()
		if err != nil {
			return nil, err
		}
		alt := &ast.If{Condition: c1, Decision: s1}
		ipe.Alternative = alt
		ipe = alt
	}
	if p.has(xElse) {
		p.next() // ELSE
		s2, err := p.parseSequence()
		if err != nil {
			return nil, err
		}
		ipe.Alternative = s2
	}
	if _, err := p.match(xEnd); err != nil {
		return nil, err
	}
	if _, err := p.match(xIf); err != nil {
		return nil, err
	}
	return res, nil
}

// Նախապայմանով ցիկլի վերլուծությունը
//
// Statement = 'WHILE' Expression Sequence 'END' 'WHILE'.
func (p *Parser) parseWhile() (ast.Statement, error) {
	p.next() // WHILE
	c0, err := p.parseExpression()
	if err != nil {
		return nil, err
	}
	b0, err := p.parseSequence()
	if err != nil {
		return nil, err
	}

	if _, err := p.match(xEnd); err != nil {
		return nil, err
	}
	if _, err := p.match(xWhile); err != nil {
		return nil, err
	}
	return &ast.While{Condition: c0, Body: b0}, err
}

// Պարամետրով ցիկլի վերլուծությունը
//
// Statement = 'FOR' IDENT '=' Expression 'TO' Expression
//
//	['STEP' ['+'|'-'] NUMBER Sequence
//	'END' 'FOR'.
func (p *Parser) parseFor() (ast.Statement, error) {
	p.next() // FOR
	name, err := p.match(xIdent)
	if err != nil {
		return nil, err
	}
	param := &ast.Variable{Name: name}
	if _, err := p.match(xEq); err != nil {
		return nil, err
	}
	begin, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	if _, err := p.match(xTo); err != nil {
		return nil, err
	}
	end, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	var step ast.Expression
	if p.has(xStep) {
		p.next() // STEP
		sign := "+"
		if p.has(xSub) {
			p.next() // '-'
			sign = "-"
		} else if p.has(xAdd) {
			p.next() // '+'
		}

		lex, err := p.match(xNumber)
		if err != nil {
			return nil, err
		}
		num, _ := strconv.ParseFloat(lex, 64)
		step = &ast.Number{Value: num}
		if sign == "-" {
			step = &ast.Unary{Operation: sign, Right: step}
		}
	} else {
		step = &ast.Number{Value: 1.0}
	}

	body, err := p.parseSequence()
	if err != nil {
		return nil, err
	}

	if _, err := p.match(xEnd); err != nil {
		return nil, err
	}
	if _, err := p.match(xFor); err != nil {
		return nil, err
	}

	return &ast.For{
		Parameter: param,
		Begin:     begin,
		End:       end,
		Step:      step,
		Body:      body}, nil
}

// Ենթածրագրի կանչի վերլուծությունը
//
// Statement = 'CALL' IDENT [Expression {',' Expression}].
func (p *Parser) parseCall() (ast.Statement, error) {
	p.next() // CALL
	name, err := p.match(xIdent)
	if err != nil {
		return nil, err
	}

	arguments, err := p.parseExpressionList()
	if err != nil {
		return nil, err
	}

	return &ast.Call{Callee: name, Arguments: arguments}, nil
}

// Արտահայտություն

var operation = map[int]string{
	xAdd: "+",
	xSub: "-",
	xAmp: "&",
	xMul: "*",
	xDiv: "/",
	xMod: "\\",
	xPow: "^",
	xEq:  "=",
	xNe:  "<>",
	xGt:  ">",
	xGe:  ">=",
	xLt:  "<",
	xLe:  "<=",
	xAnd: "AND",
	xOr:  "OR",
	xNot: "NOT",
}

// Expression = Conjunction { OR Conjunction }.
func (p *Parser) parseExpression() (ast.Expression, error) {
	res, err := p.parseConjunction()
	if err != nil {
		return nil, err
	}

	for p.has(xOr) {
		p.next() // OR
		right, err := p.parseConjunction()
		if err != nil {
			return nil, err
		}
		res = &ast.Binary{Operation: "OR", Left: res, Right: right}
	}
	return res, nil
}

// ExpressionList = [ Expression { ',' Expression } ].
func (p *Parser) parseExpressionList() ([]ast.Expression, error) {
	elements := make([]ast.Expression, 0)

	if p.isExprFirst() {
		elem, err := p.parseExpression()
		if err != nil {
			return nil, err
		}
		elements = append(elements, elem)

		for p.has(xComma) {
			p.next() // ','
			elem, err := p.parseExpression()
			if err != nil {
				return nil, err
			}
			elements = append(elements, elem)
		}
	}

	return elements, nil
}

// Կոնյունկցիա
//
// Conjunction = Equality { AND Equality }.
func (p *Parser) parseConjunction() (ast.Expression, error) {
	res, err := p.parseEquality()
	if err != nil {
		return nil, err
	}
	for p.has(xAnd) {
		p.next() // AND
		right, err := p.parseEquality()
		if err != nil {
			return nil, err
		}
		res = &ast.Binary{Operation: "AND", Left: res, Right: right}
	}
	return res, nil
}

// Հավասարություն
//
// Equality = Comparison [('=' | '<>') Comparison].
func (p *Parser) parseEquality() (ast.Expression, error) {
	res, err := p.parseComparison()
	if err != nil {
		return nil, err
	}
	if p.has(xEq, xNe) {
		opc := operation[p.lookahead.token]
		p.next() // '=', '<>'
		right, err := p.parseComparison()
		if err != nil {
			return nil, err
		}
		res = &ast.Binary{Operation: opc, Left: res, Right: right}
	}
	return res, nil
}

// Համեմատություն
//
// Comparison = Addition [('>' | '>=' | '<' | '<=') Addition].
func (p *Parser) parseComparison() (ast.Expression, error) {
	res, err := p.parseAddition()
	if err != nil {
		return nil, err
	}
	if p.has(xGt, xGe, xLt, xLe) {
		opc := operation[p.lookahead.token]
		p.next() // '>', '>=', '<', '<='
		right, err := p.parseAddition()
		if err != nil {
			return nil, err
		}
		res = &ast.Binary{Operation: opc, Left: res, Right: right}
	}
	return res, nil
}

// Գումարում, հանում կամ տողերի կոնկատենացիա
//
// Addition = Multiplication {('+' | '-' | '&') Multiplication}.
func (p *Parser) parseAddition() (ast.Expression, error) {
	res, err := p.parseMultiplication()
	if err != nil {
		return nil, err
	}
	for p.has(xAdd, xSub, xAmp) {
		opc := operation[p.lookahead.token]
		p.next() // '+', '-', '&'
		right, err := p.parseMultiplication()
		if err != nil {
			return nil, err
		}
		res = &ast.Binary{Operation: opc, Left: res, Right: right}
	}
	return res, nil
}

// Բազմապատկում, բաժանում կամ մնացորդ
//
// Multiplication = Power {('*' | '/' | '\') Power}.
func (p *Parser) parseMultiplication() (ast.Expression, error) {
	res, err := p.parsePower()
	if err != nil {
		return nil, err
	}
	for p.has(xMul, xDiv, xMod) {
		opc := operation[p.lookahead.token]
		p.next() // '*', '/', '\'
		right, err := p.parsePower()
		if err != nil {
			return nil, err
		}
		res = &ast.Binary{Operation: opc, Left: res, Right: right}
	}
	return res, nil
}

// Ատիճան բարձրացնելու գործողությունը
//
// Power = Unary [ '^' Power ].
func (p *Parser) parsePower() (ast.Expression, error) {
	res, err := p.parseUnary()
	if err != nil {
		return nil, err
	}
	if p.has(xPow) {
		p.next() // '^'
		right, err := p.parsePower()
		if err != nil {
			return nil, err
		}
		res = &ast.Binary{Operation: "^", Left: res, Right: right}
	}
	return res, nil
}

// Ունար գործողություն
//
// Unary = { '+' | '-' | 'NOT' } Subscript.
func (p *Parser) parseUnary() (ast.Expression, error) {
	var ops []string
	for p.has(xAdd, xSub, xNot) {
		opc := operation[p.lookahead.token]
		p.next() // '+', '-', NOT
		ops = slices.Insert(ops, 0, opc)
	}

	right, err := p.parseSubscript()
	if err != nil {
		return nil, err
	}

	for _, opc := range ops {
		right = &ast.Unary{Operation: opc, Right: right}
	}

	return right, nil
}

// Ինդեքսավորման գործողությունը
//
// Subscript = Factor { '[' Expression ']' }.
func (p *Parser) parseSubscript() (ast.Expression, error) {
	res, err := p.parseFactor()
	if err != nil {
		return nil, err
	}
	for p.has(xLeftBr) {
		p.next() // '['
		right, err := p.parseExpression()
		if err != nil {
			return nil, err
		}
		if _, err = p.match(xRightBr); err != nil {
			return nil, err
		}
		res = &ast.Binary{Operation: "[]", Left: res, Right: right}
	}
	return res, nil
}

// Պարզագույն արտահայտությունների վերլուծությունը
//
// Factor = TRUE | FALSE | NUMBER | TEXT | ArrayLiteral | IdentOrApply | Grouping.
func (p *Parser) parseFactor() (ast.Expression, error) {
	switch {
	case p.has(xTrue, xFalse):
		return p.parseTrueOrFalse()
	case p.has(xNumber):
		return p.parseNumber()
	case p.has(xText):
		return p.parseText()
	case p.has(xLeftBr):
		return p.parseArrayLiteral()
	case p.has(xIdent):
		return p.parseIdentOrApply()
	case p.has(xLeftPar):
		return p.parseGrouping()
	default:
		return nil, fmt.Errorf("պարզագույն արտահայտության սխալ")
	}
}

// տրամաբանական լիտերալ, TRUE կամ FALSE
func (p *Parser) parseTrueOrFalse() (ast.Expression, error) {
	lex, err := p.match(p.lookahead.token)
	if err != nil {
		return nil, err
	}

	return &ast.Boolean{Value: strings.ToUpper(lex) == "TRUE"}, nil
}

// թվային լիտերալ
func (p *Parser) parseNumber() (ast.Expression, error) {
	lex := p.lookahead.value
	p.next() // NUMBER
	val, _ := strconv.ParseFloat(lex, 64)
	return &ast.Number{Value: val}, nil
}

// տեքստային լիտերալ
func (p *Parser) parseText() (ast.Expression, error) {
	val := p.lookahead.value
	p.next() // TEXT
	return &ast.Text{Value: val}, nil
}

// զանգվածի լիտերալ
func (p *Parser) parseArrayLiteral() (ast.Expression, error) {
	p.next() // լիտերալի սկիզբը, '['

	// լիտերալի անդամները
	elements, err := p.parseExpressionList()
	if err != nil {
		return nil, err
	}

	// լիտերալի վերջը, ']'
	if _, err := p.match(xRightBr); err != nil {
		return nil, err
	}

	return &ast.Array{Elements: elements}, nil
}

// իդենտիֆիկատոր կամ ֆունկցիա-ենթածրագրի կանչ
func (p *Parser) parseIdentOrApply() (ast.Expression, error) {
	name := p.lookahead.value
	p.next() // IDENT

	if p.has(xLeftPar) {
		p.next() // '('

		arguments, err := p.parseExpressionList()
		if err != nil {
			return nil, err
		}

		if _, err := p.match(xRightPar); err != nil { // ')'
			return nil, err
		}

		return &ast.Apply{Callee: name, Arguments: arguments}, nil
	}

	return &ast.Variable{Name: name}, nil
}

// փակագծեր
func (p *Parser) parseGrouping() (ast.Expression, error) {
	p.next() // '('

	expr, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	if _, err = p.match(xRightPar); err != nil {
		return nil, err
	}

	return expr, nil
}

func (p *Parser) has(tokens ...int) bool {
	return p.lookahead.is(tokens...)
}

func (p *Parser) isStatementFirst() bool {
	return p.has(xDim, xLet, xInput, xPrint, xIf, xWhile, xFor, xCall)
}

func (p *Parser) isExprFirst() bool {
	return p.has(xTrue, xFalse, xNumber, xText, xIdent, xSub, xNot, xLeftPar, xLeftBr)
}

func (p *Parser) next() { p.lookahead = p.scanner.next() }

func (p *Parser) match(exp int) (string, error) {
	if p.lookahead.is(exp) {
		value := p.lookahead.value
		p.next()
		return value, nil
	}

	return "", fmt.Errorf("տող %d. Վերլուծության սխալ", p.lookahead.line)
}
