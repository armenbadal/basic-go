package parser

import (
	"basic/ast"
	"bufio"
	"fmt"
	"strconv"
	"strings"
)

// Parser Շարահյուսական վերլուծիչի ստրուկտուրան։
type Parser struct {
	scanner   *scanner // բառային վերլուծիչի ցուցիչ
	lookahead *lexeme  // look-a-head սիմվոլ
}

// New Ստեղծում և վերադարձնում է շարահյուսական վերլուծիչի նոր օբյեկտ։
func New(reader *bufio.Reader) (*Parser, error) {
	// ստեղծել շարահյուսական վերլուծիչի օբյեկտը
	pars := new(Parser)
	pars.scanner = new(scanner)
	pars.scanner.source = reader
	pars.scanner.line = 1
	pars.scanner.read()
	pars.lookahead = pars.scanner.next()

	return pars, nil
}

// Parse Վերլուծությունը սկսող արտաքին ֆունկցիա
func (p *Parser) Parse() (*ast.Program, error) {
	return p.parseProgram()
}

// Վերլուծել ամբողջ ծրագիրը.
//
// Program = { Subroutine }.
func (p *Parser) parseProgram() (*ast.Program, error) {
	// բաց թողնել ֆայլի սկզբի դատարկ տողերը
	for p.has(xNewLine) {
		p.next()
	}

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
		parameters, err = p.parseIdentList()
		if err != nil {
			return nil, err
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
	subr := &ast.Subroutine{Name: name, Parameters: parameters, Body: body}

	return subr, nil
}

func (p *Parser) parseIdentList() ([]string, error) {
	identifiers := make([]string, 0)
	if p.has(xIdent) {
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
	}
	return identifiers, nil
}

// Վերլուծել հրամանների հաջորդականություն
//
// Sequence = NewLines { Statement NewLines }.
func (p *Parser) parseSequence() (*ast.Sequence, error) {
	p.parseNewLines()
	seq := &ast.Sequence{Items: make([]ast.Statement, 0)}
	var err error
loop:
	for {
		var stat ast.Statement
		switch {
		case p.has(xDim):
			stat, err = p.parseDim()
		case p.has(xLet):
			stat, err = p.parseLet()
		case p.has(xInput):
			stat, err = p.parseInput()
		case p.has(xPrint):
			stat, err = p.parsePrint()
		case p.has(xIf):
			stat, err = p.parseIf()
		case p.has(xWhile):
			stat, err = p.parseWhile()
		case p.has(xFor):
			stat, err = p.parseFor()
		case p.has(xCall):
			stat, err = p.parseCall()
		default:
			break loop
		}
		p.parseNewLines()
		seq.Items = append(seq.Items, stat)
	}
	return seq, err
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
// Statement = 'LET' IDENT '=' Expression.
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
	args := make([]ast.Expression, 0)
	if p.isExprFirst() {
		e0, err := p.parseExpression()
		if err != nil {
			return nil, err
		}
		args = append(args, e0)
		for p.has(xComma) {
			p.next() // ','
			e1, err := p.parseExpression()
			if err != nil {
				return nil, err
			}
			args = append(args, e1)
		}
	}

	return &ast.Call{Callee: name, Arguments: args}, nil
}

// Արտահայտություն
//
// Expression = Conjunction { OR Conjunction }.
func (p *Parser) parseExpression() (ast.Expression, error) {
	res, err := p.parseConjunction()
	if err != nil {
		return nil, err
	}
	for p.has(xOr) {
		p.next() // OR
		e0, err := p.parseConjunction()
		if err != nil {
			return nil, err
		}
		res = &ast.Binary{Operation: "OR", Left: res, Right: e0}
	}
	return res, nil
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
		e0, err := p.parseEquality()
		if err != nil {
			return nil, err
		}
		res = &ast.Binary{Operation: "AND", Left: res, Right: e0}
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
		var opc string
		switch p.lookahead.token {
		case xEq:
			opc = "="
		case xNe:
			opc = "<>"
		}
		p.next() // '=', '<>'
		e0, err := p.parseComparison()
		if err != nil {
			return nil, err
		}
		res = &ast.Binary{Operation: opc, Left: res, Right: e0}
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
		var opc string
		switch p.lookahead.token {
		case xGt:
			opc = ">"
		case xGe:
			opc = ">="
		case xLt:
			opc = "<"
		case xLe:
			opc = "<="
		}
		p.next() // '>', '>=', '<', '<='
		e0, err := p.parseAddition()
		if err != nil {
			return nil, err
		}
		res = &ast.Binary{Operation: opc, Left: res, Right: e0}
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
		var opc string
		switch p.lookahead.token {
		case xAdd:
			opc = "+"
		case xSub:
			opc = "-"
		case xAmp:
			opc = "&"
		}
		p.next() // '+', '-', '&'
		e0, err := p.parseMultiplication()
		if err != nil {
			return nil, err
		}
		res = &ast.Binary{Operation: opc, Left: res, Right: e0}
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
		var opc string
		switch p.lookahead.token {
		case xMul:
			opc = "*"
		case xDiv:
			opc = "/"
		case xMod:
			opc = "\\"
		}
		p.next() // '*', '/', '\'
		e0, err := p.parsePower()
		if err != nil {
			return nil, err
		}
		res = &ast.Binary{Operation: opc, Left: res, Right: e0}
	}
	return res, nil
}

// Ատիճան բարձրացնելու գործողությունը
//
// Power = Index ['^' Power].
func (p *Parser) parsePower() (ast.Expression, error) {
	res, err := p.parseSubscript()
	if err != nil {
		return nil, err
	}
	if p.has(xPow) {
		p.next() // '^'
		e0, err := p.parsePower()
		if err != nil {
			return nil, err
		}
		res = &ast.Binary{Operation: "^", Left: res, Right: e0}
	}
	return res, nil
}

// Ինդեքսավորման գործողությունը
//
// Index = Factor {'[' Expression ']'}.
func (p *Parser) parseSubscript() (ast.Expression, error) {
	res, err := p.parseFactor()
	if err != nil {
		return nil, err
	}
	for p.has(xLeftBr) {
		p.next() // '('
		e0, err := p.parseExpression()
		if err != nil {
			return nil, err
		}
		if _, err = p.match(xRightBr); err != nil {
			return nil, err
		}
		res = &ast.Binary{Operation: "[]", Left: res, Right: e0}
	}
	return res, nil
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
func (p *Parser) parseFactor() (ast.Expression, error) {
	var result ast.Expression
	var err error

	switch {
	case p.has(xTrue, xFalse):
		result, err = p.parseTrueOrFalse()
	case p.has(xNumber):
		result, err = p.parseNumber()
	case p.has(xText):
		result, err = p.parseText()
	case p.has(xLeftBr):
		result, err = p.parseArrayLiteral()
	case p.has(xIdent):
		result, err = p.parseIdentOrApply()
	case p.has(xSub, xNot):
		result, err = p.parseUnary()
	case p.has(xLeftPar):
		result, err = p.parseGrouping()
	default:
		err = fmt.Errorf("պարզագույն արտահայտության սխալ")
	}

	return result, err
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
	elems := make([]ast.Expression, 0)
	if _, err := p.match(xLeftBr); err != nil {
		return nil, err
	}
	e, err := p.parseExpression()
	if err != nil {
		return nil, err
	}
	elems = append(elems, e)
	for p.has(xComma) {
		p.next() // ','
		e, err := p.parseExpression()
		if err != nil {
			return nil, err
		}
		elems = append(elems, e)
	}
	if _, err := p.match(xRightBr); err != nil {
		return nil, err
	}
	return &ast.Array{Elements: elems}, nil
}

// իդենտիֆիկատոր կամ ֆունկցիա-ենթածրագրի կանչ
func (p *Parser) parseIdentOrApply() (ast.Expression, error) {
	name := p.lookahead.value
	p.next() // IDENT

	if p.has(xLeftPar) {
		p.next() // '('
		args := make([]ast.Expression, 0)
		if p.isExprFirst() {
			e0, err := p.parseExpression()
			if err != nil {
				return nil, err
			}
			args = append(args, e0)
			for p.has(xComma) {
				p.next() // ','
				e1, err := p.parseExpression()
				if err != nil {
					return nil, err
				}
				args = append(args, e1)
			}
		}
		if _, err := p.match(xRightPar); err != nil {
			return nil, err
		}
		return &ast.Apply{Callee: name, Arguments: args}, nil
	}

	return &ast.Variable{Name: name}, nil
}

// ունար գործողություն
func (p *Parser) parseUnary() (ast.Expression, error) {
	var opc string
	switch p.lookahead.token {
	case xSub:
		opc = "-"
	case xNot:
		opc = "NOT"
	}
	p.next() // '-', NOT

	res, err := p.parseFactor()
	if err != nil {
		return nil, err
	}

	res = &ast.Unary{Operation: opc, Right: res}
	return res, nil
}

// փակագծեր
func (p *Parser) parseGrouping() (ast.Expression, error) {
	p.next() // '('

	res, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	if _, err = p.match(xRightPar); err != nil {
		return nil, err
	}

	return res, nil
}

func (p *Parser) has(tokens ...int) bool {
	return p.lookahead.is(tokens...)
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
