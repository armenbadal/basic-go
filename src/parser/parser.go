
package parser

import (
	"bufio"
	"container/list"
	"engine"
	"os"
	"strconv"
)


// վերլուծված ենթածրագրերի «գլոբալ» ցուցակ
var subrs = map[string]*engine.Subroutine{}
// անհայտ ենթածրագրիերի կանչերի ցուցակ
var clinks = map[string]*list.List{}


// Շարահյուսական վերլուծիչի ստրուկտուրան։
type Parser struct {
	scer *scanner
	lookahead *lexeme

	program *engine.Program
}

// Ստեղծում և վերադարձնում է շարահյուսական վերլուծիչի նոր օբյեկտ։
func NewParser(filename string) *Parser {
	// բացել ֆայլային հոսքը
	rd, er := os.Open(filename)
	if er != nil {
		// TODO: signal for failure
	}
	defer rd.Close()

	// ստեղծել շարահյուսական վերլուծիչի օբյեկտը
	pars := new(Parser)
	pars.scer = new(scanner)
	pars.scer.source = bufio.NewReader(rd)
	pars.scer.line = 1
	pars.scer.read()
	pars.lookahead = pars.scer.next()

	pars.program = engine.NewProgram()
	
	return pars
}

// Վերլուծությունը սկսող արտաքին ֆունկցիա
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
// Subroutine = 'SUB' IDENT ['(' [IDENT {',' IDENT}] ')'] NewLines
//              { Statement NewLines } 'END' SUB'.
//
func (p *Parser) parseSubroutine() *engine.Subroutine {
	p.match(xSubroutine)
	name := p.lookahead.value
	p.match(xIdent)
	pars := list.New()
	if p.has(xLeftPar) {
		p.match(xLeftPar)
		if p.has(xIdent) {
			pnm := p.lookahead.value
			p.match(xIdent)
			pars.PushBack(pnm)
			for p.has(xComma) {
				p.match(xComma)
				pnm = p.lookahead.value
				p.match(xIdent)
				pars.PushBack(pnm)
			}
		}
		p.match(xRightPar)
	}
	body := p.parseSequence()
	p.match(xEnd)
	p.match(xSubroutine)

	sub := engine.NewSubroutine(name, pars, body)
	subrs[name] = sub

	if clinks[name] != nil {
		for e := clinks[name].Front(); e != nil; e = e.Next() {
			switch coa := e.Value.(type) {
			case engine.Call:
				coa.SetCallee(sub)
			case engine.Apply:
				coa.SetCallee(sub)
			}
		}
		delete(clinks, name)
	}
	
	return sub
}

// Վերլուծել հրամանների հաջորդականություն
func (p *Parser) parseSequence() engine.Statement {
	p.parseNewLines()
	res := engine.NewSequence()
loop:
	for {
		var stat engine.Statement
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
func (p *Parser) parseLet() engine.Statement {
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
func (p *Parser) parseInput() engine.Statement {
	p.match(xInput)
	nam := p.lookahead.value
	p.match(xIdent)
	return engine.NewInput(nam)
}

// Արտածման հրամանի վերլուծությունը.
//
// Statement = 'PRINT' Expression.
//
func (p *Parser) parsePrint() engine.Statement {
	p.match(xPrint)
	e0 := p.parseExpression()
	return engine.NewPrint(e0)
}

// Ճյուղավորման հրամանի վերլուծությունը.
//
// Statement = 'IF' Expression 'THEN' NewLines { Statement NewLines }
//             { 'ELSEIF' Expression 'THEN' NewLines { Statement NewLines } }
//             [ 'ELSE' NewLines { Statement NewLines } ]
//             'END' 'IF'.
//
func (p *Parser) parseIf() engine.Statement {
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
// Statement = 'WHILE' Expression NewLines
//             { Statement NewLines } 'END' 'WHILE'.
//
func (p *Parser) parseWhile() engine.Statement {
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
//             ['STEP' ['+'|'-'] NUMBER { Statement NewLines }
//             'END' 'FOR'.
//
func (p *Parser) parseFor() engine.Statement {
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
func (p *Parser) parseCall() engine.Statement {
	p.match(xCall)
	name := p.lookahead.value
	p.match(xIdent)
	args := list.New()
	if p.has(xNumber, xText, xIdent, xSub, xNot, xLeftPar) {
		e0 := p.parseExpression()
		args.PushBack(e0)
		for p.has(xComma) {
			p.match(xComma)
			e1 := p.parseExpression()
			args.PushBack(e1)
		}
	}

	
	sp, defined := subrs[name]
	if defined {
		return engine.NewCall(sp, args)
	}

	dummy := engine.NewSubroutine("__dummy__", list.New(), nil)
	dcall := engine.NewCall(dummy, args)
	if clinks[name] == nil {
		clinks[name] = list.New()
	}
	clinks[name].PushBack(dcall)
	return dcall
}

//
func (p *Parser) parseExpression() engine.Expression {
	res := p.parseConjunction()
	for p.has(xOr) {
		p.match(xOr)
		e0 := p.parseConjunction()
		res = engine.NewBinary(engine.OR, res, e0)
	}
	return res
}

//
func (p *Parser) parseConjunction() engine.Expression {
	res := p.parseEquality()
	for p.has(xAnd) {
		p.match(xAnd)
		e0 := p.parseEquality()
		res = engine.NewBinary(engine.AND, res, e0)
	}
	return res
}

//
func (p *Parser) parseEquality() engine.Expression {
	res := p.parseComparison()
	if p.has(xEq, xNe) {
		var opc int
		switch p.lookahead.token {
		case xEq:
			opc = engine.EQ
			p.match(xEq)
		case xNe:
			opc = engine.NE
			p.match(xNe)
		}
		e0 := p.parseComparison()
		res = engine.NewBinary(opc, res, e0)
	}
	return res
}

//
func (p *Parser) parseComparison() engine.Expression {
	res := p.parseAddition()
	if p.has(xGt, xGe, xLt, xLe) {
		var opc int
		switch p.lookahead.token {
		case xGt:
			opc = engine.GT
			p.match(xGt)
		case xGe:
			opc = engine.GE
			p.match(xGe)
		case xLt:
			opc = engine.LT
			p.match(xLt)
		case xLe:
			opc = engine.LE
			p.match(xLe)
		}
		e0 := p.parseAddition()
		res = engine.NewBinary(opc, res, e0)
	}
	return res
}

//
func (p *Parser) parseAddition() engine.Expression {
	res := p.parseMultiplication()
	for p.has(xAdd, xSub, xAmp) {
		var opc int
		switch p.lookahead.token {
		case xAdd:
			opc = engine.ADD
			p.match(xAdd)
		case xSub:
			opc = engine.SUB
			p.match(xSub)
		case xAmp:
			opc = engine.CONC
			p.match(xAmp)
		}
		e0 := p.parseMultiplication()
		res = engine.NewBinary(opc, res, e0)
	}
	return res
}

//
func (p *Parser) parseMultiplication() engine.Expression {
	res := p.parsePower()
	for p.has(xMul, xDiv, xMod) {
		var opc int
		switch p.lookahead.token {
		case xMul:
			opc = engine.MUL
			p.match(xMul)
		case xDiv:
			opc = engine.DIV
			p.match(xDiv)
		case xMod:
			opc = engine.MOD
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
func (p *Parser) parsePower() engine.Expression {
	res := p.parseFactor()
	if p.has(xPow) {
		p.match(xPow)
		e0 := p.parsePower()
		res = engine.NewBinary(engine.POW, res, e0)
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
func (p *Parser) parseFactor() engine.Expression {
	if p.has(xNumber) {
		lex := p.lookahead.value
		p.match(xNumber)
		val, _ := strconv.ParseFloat(lex, 64)
		return engine.NewNumber(val)
	}

	if p.has(xText) {
		val := p.lookahead.value
		p.match(xText)
		return engine.NewText(val)
	}

	if p.has(xIdent) {
		name := p.lookahead.value
		p.match(xIdent)
		if p.has(xLeftPar) {
			p.match(xLeftPar)
			args := list.New()
			if p.has(xNumber, xText, xIdent, xSub, xNot, xLeftPar) {
				e0 := p.parseExpression()
				args.PushBack(e0)
				for p.has(xComma) {
					p.match(xComma)
					e1 := p.parseExpression()
					args.PushBack(e1)
				}
			}
			p.match(xRightPar)
			return engine.NewApply(nil, args)
		}
		return engine.NewVariable(name)
	}

	if p.has(xSub, xNot) {
		var opc int
		switch p.lookahead.token {
		case xSub:
			opc = engine.NEG
			p.match(xSub)
		case xNot:
			opc = engine.NOT
			p.match(xNot)
		}
		res := p.parseFactor()
		res = engine.NewUnary(opc, res)
		return res
	}

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

