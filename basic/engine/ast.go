package engine

import (
	"container/list"
	"strings"
)

// Երկու բազային տիպեր
const (
	T_NUMBER = 'N'
	T_TEXT   = 'T'
)

// Ունիվերսալ արժեք
type Value struct {
	Type   rune
	number float64
	text   string
}

// Նոր թվային օբյեկտ
func NewNumber(val float64) *Value {
	return &Value{Type: 'N', number: val}
}

// Նոր տեքստային օբյեկտ
func NewText(val string) *Value {
	return &Value{Type: 'T', text: val}
}

// Արտահայտություններ
type Expression interface {
	evaluate(Environment) *Value
}

//
type Variable struct {
	Type rune
	name string
}

//
func NewVariable(nm string) *Variable {
	yp := 'V'
	if strings.HasSuffix(nm, "$") {
		yp = 'T'
	} else {
		yp = 'N'
	}
	return &Variable{Type: yp, name: nm}
}

// Ունար գործողություն
type Unary struct {
	Type  rune
	opfun func(x *Value) *Value
	expr  Expression
}

//
func NewUnary(op func(x *Value) *Value, eo Expression) *Unary {
	return &Unary{opfun: op, expr: eo}
}

// Բինար գործողություն
type Binary struct {
	Type         rune
	opfun        func(x, y *Value) *Value
	expro, expri Expression
}

//
func NewBinary(op func(x, y *Value) *Value, exo, exi Expression) *Binary {
	return &Binary{opfun: op, expro: exo, expri: exi}
}

// Ֆունկցիայի կիրառում
type Apply struct {
	Type      rune
	callee    *Subroutine
	arguments *list.List
}

//
func NewApply(cl *Subroutine, ags *list.List) *Apply {
	return &Apply{callee: cl, arguments: ags}
}

//
func (a *Apply) SetCallee(sb *Subroutine) {
	a.callee = sb
}

// Հրամաններ
type Statement interface {
	execute(Environment)
}

// Վերագրում
type Let struct {
	varname string
	expr    Expression
}

//
func NewLet(vn string, ex Expression) *Let {
	return &Let{varname: vn, expr: ex}
}

// Ներմուծում
type Input struct {
	varname string
}

//
func NewInput(vn string) *Input {
	return &Input{varname: vn}
}

// Արտածում
type Print struct {
	expr Expression
}

//
func NewPrint(ex Expression) *Print {
	return &Print{expr: ex}
}

// Ճյուղավորում
type If struct {
	condition   Expression
	decision    Statement
	alternative Statement
}

//
func NewIf(co Expression, de Statement) *If {
	return &If{condition: co, decision: de}
}

//
func (s *If) SetElse(el Statement) {
	s.alternative = el
}

// Նախապայմանով ցիկլ
type While struct {
	condition Expression
	body      Statement
}

//
func NewWhile(co Expression, bo Statement) *While {
	return &While{condition: co, body: bo}
}

// Հաշվիչով ցիկլ
type For struct {
	parameter string
	begin     Expression
	end       Expression
	step      Expression
	body      Statement
}

//
func NewFor(p string, b, e, s Expression, d Statement) *For {
	return &For{parameter: p, begin: b, end: e, step: s, body: d}
}

// Ենթածրագիր կանչ
type Call struct {
	callee    *Subroutine
	arguments *list.List
}

//
func NewCall(cl *Subroutine, ags *list.List) *Call {
	return &Call{callee: cl, arguments: ags}
}

//
func (c *Call) SetCallee(sb *Subroutine) {
	c.callee = sb
}

// Հրամանների հաջորդականություն
type Sequence struct {
	items *list.List
}

//
func NewSequence() *Sequence {
	return &Sequence{items: list.New()}
}

//
func (s *Sequence) AddItem(e Statement) {
	s.items.PushBack(e)
}

// Ենթածրագիր
type Subroutine struct {
	name       string
	parameters *list.List
	body       Statement
}

//
func NewSubroutine(nm string, pars *list.List, dy Statement) *Subroutine {
	return &Subroutine{name: nm, parameters: pars, body: dy}
}

// Ծրագիր
type Program struct {
	members map[string]*Subroutine
}

//
func NewProgram() *Program {
	return &Program{members: make(map[string]*Subroutine)}
}

//
func (p *Program) AddMember(su *Subroutine) {
	p.members[su.name] = su
}
