package parser

import (
	"basic/engine"
	"bufio"
	"container/list"
	"errors"
	"os"
	"strconv"
)

// Parser Շարահյուսական վերլուծիչի ստրուկտուրան։
type Parser struct {
	// բառային վերլուծիչի ցուցիչ
	scer *scanner
	// look-a-head սիմվոլ
	lookahead *lexeme

	// վերլուծության ծառի արմատ
	program *engine.Program

	// վերլուծված ենթածրագրերի ցուցակ
	subrs map[string]*engine.Subroutine

	// անհայտ ենթածրագրիերի կանչերի ցուցակ
	clinks map[string]*list.List

	// ընթացիկ վերլուծվող ենթածրագիր
	current *engine.Subroutine
}

// NewParser Ստեղծում և վերադարձնում է շարահյուսական վերլուծիչի նոր օբյեկտ։
func NewParser(filename string) (*Parser, error) {
	// բացել ֆայլային հոսքը
	rd, er := os.Open(filename)
	if er != nil {
		return nil, errors.New("Ֆայլը բացելը ձախողվեց")
	}
	defer rd.Close()

	// ստեղծել շարահյուսական վերլուծիչի օբյեկտը
	pars := new(Parser)
	pars.scer = new(scanner)
	pars.scer.source = bufio.NewReader(rd)
	pars.scer.line = 1
	pars.scer.read()
	pars.lookahead = pars.scer.next()

	pars.subrs = make(map[string]*engine.Subroutine)
	pars.clinks = make(map[string]*list.List)

	pars.program = engine.NewProgram()

	return pars, nil
}

// Parse Վերլուծությունը սկսող արտաքին ֆունկցիա
func (p *Parser) Parse() *engine.Program {
	// շարահյուսական սխալի արտածում
	defer func() {
		if err := recover(); err != nil {
			// TODO: print error message
			println("Parse Error")
			println(err)
		}
	}()

	// վերլուծել ծրագիրը
	p.parseProgram()
	return p.program
}

// Վերլուծել ամբողջ ծրագիրը.
//
// Program = { Subroutine }.
//
func (p *Parser) parseProgram() {
	// բաց թողնել ֆայլի սկզբի դատարկ տողերը
	for p.has(xNewLine) {
		p.lookahead = p.scer.next()
	}

	for p.has(xSubroutine) {
		p0 := p.parseSubroutine()
		p.program.AddMember(p0)
		p.parseNewLines()
	}
}

// Վերլուծել նոր տողերի նիշերի հաջորդականությունը
//
// NewLines = NEWLINE { NEWLINE }.
//
func (p *Parser) parseNewLines() {
	p.match(xNewLine)
	for p.lookahead.is(xNewLine) {
		p.match(xNewLine)
	}
}

// Վերլուծել ենթածրագիրը
//
// Subroutine = 'SUB' IDENT ['(' [IDENT {',' IDENT}] ')'] Sequence 'END' SUB'.
//
func (p *Parser) parseSubroutine() *engine.Subroutine {
	// վերնագրի վերլուծություն
	p.match(xSubroutine)
	name := p.lookahead.value
	p.match(xIdent)
	pars := make([]engine.Node, 0)
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

	// նոր ենթածրագրի օբյեկտ
	sub := engine.NewSubroutine(name, pars, nil)

	// մարմնի վերլուծություն
	body := p.parseSequence()
	sub.SetBody(body)

	p.match(xEnd)
	p.match(xSubroutine)

	p.subrs[name] = sub

	// գտնել և լուծել «ազատ» հղումները
	if p.clinks[name] != nil {
		for e := p.clinks[name].Front(); e != nil; e = e.Next() {
			switch coa := e.Value.(type) {
			case engine.Call:
				coa.SetCallee(sub)
			case engine.Apply:
				coa.SetCallee(sub)
			}
		}
		delete(p.clinks, name)
	}

	return sub
}

// Վերլուծել հրամանների հաջորդականություն
//
// Sequence = NewLines { Statement NewLines }.
//
func (p *Parser) parseSequence() engine.Node {
	p.parseNewLines()
	res := engine.NewSequence()
loop:
	for {
		var stat engine.Node
		switch {
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
		res.AddItem(stat)
	}
	return res
}

// Վերլուծել վերագրման հրամանը
//
// Statement = 'LET' IDENT '=' Expression.
//
func (p *Parser) parseLet() engine.Node {
	p.match(xLet)
	vn := p.lookahead.value
	p.match(xIdent)
	p.match(xEq)
	e0 := p.parseExpression()
	return engine.NewLet(vn, e0)
}

// Ներմուծման հրամանի վերլուծությունը.
//
// Statement = 'INPUT' IDENT.
//
func (p *Parser) parseInput() engine.Node {
	p.match(xInput)
	nam := p.lookahead.value
	p.match(xIdent)
	return engine.NewInput(nam)
}

// Արտածման հրամանի վերլուծությունը.
//
// Statement = 'PRINT' Expression.
//
func (p *Parser) parsePrint() engine.Node {
	p.match(xPrint)
	e0 := p.parseExpression()
	return engine.NewPrint(e0)
}

// Ճյուղավորման հրամանի վերլուծությունը.
//
// Statement = 'IF' Expression 'THEN' Sequence
//             { 'ELSEIF' Expression 'THEN' Sequence }
//             [ 'ELSE' Sequence ]
//             'END' 'IF'.
//
func (p *Parser) parseIf() engine.Node {
	p.match(xIf)
	c0 := p.parseExpression()
	p.match(xThen)
	s0 := p.parseSequence()
	res := engine.NewIf(c0, s0)
	ipe := res
	for p.has(xElseIf) {
		p.match(xElseIf)
		c1 := p.parseExpression()
		p.match(xThen)
		s1 := p.parseSequence()
		alt := engine.NewIf(c1, s1)
		ipe.SetElse(alt)
		ipe = alt
	}
	if p.has(xElse) {
		p.match(xElse)
		s2 := p.parseSequence()
		ipe.SetElse(s2)
	}
	p.match(xEnd)
	p.match(xIf)
	return res
}

// Նախապայմանով ցիկլի վերլուծությունը
//
// Statement = 'WHILE' Expression Sequence 'END' 'WHILE'.
//
func (p *Parser) parseWhile() engine.Node {
	p.match(xWhile)
	c0 := p.parseExpression()
	b0 := p.parseSequence()
	p.match(xEnd)
	p.match(xWhile)
	return engine.NewWhile(c0, b0)
}

// Պարամետրով ցիկլի վերլուծությունը
//
// Statement = 'FOR' IDENT '=' Expression 'TO' Expression
//             ['STEP' ['+'|'-'] NUMBER Sequence
//             'END' 'FOR'.
//
func (p *Parser) parseFor() engine.Node {
	p.match(xFor)
	param := p.lookahead.value
	p.match(xIdent)
	p.match(xEq)
	b0 := p.parseExpression()
	p.match(xTo)
	e0 := p.parseExpression()
	var num float64 = 1.0
	if p.has(xStep) {
		p.match(xStep)
		posi := true
		if p.has(xSub) {
			p.match(xSub)
			posi = false
		} else if p.has(xAdd) {
			p.match(xAdd)
		}
		lex := p.lookahead.value
		p.match(xNumber)
		num, _ = strconv.ParseFloat(lex, 64)
		if !posi {
			num = -num
		}
	}
	s0 := engine.NewNumber(num)
	dy := p.parseSequence()
	p.match(xEnd)
	p.match(xFor)
	return engine.NewFor(param, b0, e0, s0, dy)
}

// Ենթածրագրի կանչի վերլուծությունը
//
// Statement = 'CALL' IDENT [Expression {',' Expression}].
//
func (p *Parser) parseCall() engine.Node {
	p.match(xCall)
	name := p.lookahead.value
	p.match(xIdent)
	args := make([]engine.Node, 0)
	if p.has(xNumber, xText, xIdent, xSub, xNot, xLeftPar) {
		e0 := p.parseExpression()
		args = append(args, e0)
		for p.has(xComma) {
			p.match(xComma)
			e1 := p.parseExpression()
			args = append(args, e1)
		}
	}

	sp, defined := p.subrs[name]
	if defined {
		return engine.NewCall(sp, args)
	}

	dummy := engine.NewSubroutine("__dummy__", nil, nil)
	dcall := engine.NewCall(dummy, args)
	if p.clinks[name] == nil {
		p.clinks[name] = list.New()
	}
	p.clinks[name].PushBack(dcall)
	return dcall
}

// Արտահայտություն
//
// Expression = Conjunction { OR Conjunction }.
//
func (p *Parser) parseExpression() engine.Node {
	res := p.parseConjunction()
	for p.has(xOr) {
		p.match(xOr)
		e0 := p.parseConjunction()
		res = engine.NewBinary("OR", res, e0)
	}
	return res
}

// Կոնյունկցիա
//
// Conjunction = Equality { AND Equality }.
//
func (p *Parser) parseConjunction() engine.Node {
	res := p.parseEquality()
	for p.has(xAnd) {
		p.match(xAnd)
		e0 := p.parseEquality()
		res = engine.NewBinary("AND", res, e0)
	}
	return res
}

// Հավասարություն
//
// Equality = Comparison [('=' | '<>') Comparison].
//
func (p *Parser) parseEquality() engine.Node {
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
		res = engine.NewBinary(opc, res, e0)
	}
	return res
}

// Համեմատություն
//
// Comparison = Addition [('>' | '>=' | '<' | '<=') Addition].
//
func (p *Parser) parseComparison() engine.Node {
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
		res = engine.NewBinary(opc, res, e0)
	}
	return res
}

// Գումարում, հանում կամ տողերի կոնկատենացիա
//
// Addition = Multiplication {('+' | '-' | '&') Multiplication}.
//
func (p *Parser) parseAddition() engine.Node {
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
		res = engine.NewBinary(opc, res, e0)
	}
	return res
}

// Բազմապատկում, բաժանում կամ մնացորդ
//
// Multiplication = Power {('*' | '/' | '\') Power}.
//
func (p *Parser) parseMultiplication() engine.Node {
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
		res = engine.NewBinary(opc, res, e0)
	}
	return res
}

// Ատիճան բարձրացնելու գործողությունը
//
// Power = Factor '^' Power.
//
func (p *Parser) parsePower() engine.Node {
	res := p.parseFactor()
	if p.has(xPow) {
		p.match(xPow)
		e0 := p.parsePower()
		res = engine.NewBinary("^", res, e0)
	}
	return res
}

// Պարզագույն արտահայտությունների վերլուծությունը
//
// Factor = NUMBER | TEXT | IDENT
//        | SUB Factor
//        | NOT Factor
//        | '(' Expression ')'.
//
func (p *Parser) parseFactor() engine.Node {
	// թվային լիտերալ
	if p.has(xNumber) {
		lex := p.lookahead.value
		p.match(xNumber)
		val, _ := strconv.ParseFloat(lex, 64)
		return engine.NewNumber(val)
	}

	// տեքստային լիտերալ
	if p.has(xText) {
		val := p.lookahead.value
		p.match(xText)
		return engine.NewText(val)
	}

	// ցուցակի լիտերալ
	if p.has(xLeftBr) {
		p.match(xLeftBr)
		p.match(xRightBr)

	}

	// իդենտիֆիկատոր կամ ֆունկցիա-ենթածրագրի կանչ
	if p.has(xIdent) {
		name := p.lookahead.value
		p.match(xIdent)
		if p.has(xLeftPar) {
			p.match(xLeftPar)
			args := make([]engine.Node, 0)
			if p.has(xNumber, xText, xIdent, xSub, xNot, xLeftPar) {
				e0 := p.parseExpression()
				args = append(args, e0)
				for p.has(xComma) {
					p.match(xComma)
					e1 := p.parseExpression()
					args = append(args, e1)
				}
			}
			p.match(xRightPar)
			return engine.NewApply(nil, args)
		}
		return engine.NewVariable(name)
	}

	// ունար գործողություն
	if p.has(xSub, xNot) {
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
		res = engine.NewUnary(opc, res)
		////res.Type = engine.T_NUMBER
		return res
	}

	// փակագծեր
	if p.has(xLeftPar) {
		p.match(xLeftPar)
		res := p.parseExpression()
		p.match(xRightPar)
		return res
	}

	return nil
}

//
func (p *Parser) has(tokens ...int) bool {
	return p.lookahead.is(tokens...)
}

//
func (p *Parser) match(exp int) {
	if p.lookahead.is(exp) {
		p.lookahead = p.scer.next()
	} else {
		println("Տող.")
		println(p.lookahead.line)
		panic("Վերլուծության սխալ")
	}
}
