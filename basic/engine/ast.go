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

// Value Ունիվերսալ արժեք
type Value struct {
	Type   rune
	number float64
	text   string
}

// NewNumber Նոր թվային օբյեկտ
func NewNumber(val float64) *Value {
	return &Value{Type: 'N', number: val}
}

// NewText Նոր տեքստային օբյեկտ
func NewText(val string) *Value {
	return &Value{Type: 'T', text: val}
}

// Expression Արտահայտություններ
type Expression interface {
	evaluate(Environment) *Value
}

// Variable ...
type Variable struct {
	Type rune
	name string
}

// NewVariable ...
func NewVariable(nm string) *Variable {
	yp := 'V'
	if strings.HasSuffix(nm, "$") {
		yp = 'T'
	} else {
		yp = 'N'
	}
	return &Variable{Type: yp, name: nm}
}

// Unary Ունար գործողություն
type Unary struct {
	Type  rune
	opfun func(x *Value) *Value
	expr  Expression
}

// NewUnary ...
func NewUnary(op func(x *Value) *Value, eo Expression) *Unary {
	return &Unary{opfun: op, expr: eo}
}

// Binary Բինար գործողություն
type Binary struct {
	Type         rune
	opfun        func(x, y *Value) *Value
	expro, expri Expression
}

// NewBinary ...
func NewBinary(op func(x, y *Value) *Value, exo, exi Expression) *Binary {
	return &Binary{opfun: op, expro: exo, expri: exi}
}

// Apply Ֆունկցիայի կիրառում
type Apply struct {
	Type      rune
	callee    *Subroutine
	arguments *list.List
}

// NewApply ...
func NewApply(cl *Subroutine, ags *list.List) *Apply {
	return &Apply{callee: cl, arguments: ags}
}

// SetCallee ...
func (a *Apply) SetCallee(sb *Subroutine) {
	a.callee = sb
}

// Statement Հրամանների ինտերֆեյսը
type Statement interface {
	execute(Environment)
}

// Let Վերագրում
type Let struct {
	name *Symbol
	expr Expression
}

// NewLet ...
func NewLet(vn string, ex Expression) *Let {
	return &Let{name: NewSymbol(vn), expr: ex}
}

// Input Ներմուծում
type Input struct {
	varname string
}

// NewInput ...
func NewInput(vn string) *Input {
	return &Input{varname: vn}
}

// Print Արտածում
type Print struct {
	expr Expression
}

// NewPrint ...
func NewPrint(ex Expression) *Print {
	return &Print{expr: ex}
}

// If Ճյուղավորում
type If struct {
	condition   Expression
	decision    Statement
	alternative Statement
}

// NewIf ...
func NewIf(co Expression, de Statement) *If {
	return &If{condition: co, decision: de}
}

// SetElse ...
func (s *If) SetElse(el Statement) {
	s.alternative = el
}

// While Նախապայմանով ցիկլ
type While struct {
	condition Expression
	body      Statement
}

// NewWhile ...
func NewWhile(co Expression, bo Statement) *While {
	return &While{condition: co, body: bo}
}

// For Հաշվիչով ցիկլ
type For struct {
	parameter string
	begin     Expression
	end       Expression
	step      Expression
	body      Statement
}

// NewFor ...
func NewFor(p string, b, e, s Expression, d Statement) *For {
	return &For{parameter: p, begin: b, end: e, step: s, body: d}
}

// Call Ենթածրագիր կանչ
type Call struct {
	callee    *Subroutine
	arguments *list.List
}

// NewCall ...
func NewCall(cl *Subroutine, ags *list.List) *Call {
	return &Call{callee: cl, arguments: ags}
}

// SetCallee ...
func (c *Call) SetCallee(sb *Subroutine) {
	c.callee = sb
}

// Sequence Հրամանների հաջորդականություն
type Sequence struct {
	items *list.List
}

// NewSequence ...
func NewSequence() *Sequence {
	return &Sequence{items: list.New()}
}

// AddItem ...
func (s *Sequence) AddItem(e Statement) {
	s.items.PushBack(e)
}

// Subroutine Ենթածրագիր
type Subroutine struct {
	name       string
	parameters *list.List
	body       Statement
}

// NewSubroutine ...
func NewSubroutine(nm string, pars *list.List, dy Statement) *Subroutine {
	return &Subroutine{name: nm, parameters: pars, body: dy}
}

// Program Ծրագիր
type Program struct {
	members map[string]*Subroutine
}

// NewProgram ...
func NewProgram() *Program {
	return &Program{members: make(map[string]*Subroutine)}
}

// AddMember ...
func (p *Program) AddMember(su *Subroutine) {
	p.members[su.name] = su
}
