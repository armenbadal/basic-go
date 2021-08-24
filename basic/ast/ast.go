package ast

// Node Աբստրակտ քերականական ծառի հանգույց
type Node interface{}

// Boolean Բուլյան լիտերալ
type Boolean struct {
	Value bool
}

// NewBoolean Նոր բուլյան օբյեկտ
func NewBoolean(v bool) *Boolean {
	return &Boolean{Value: v}
}

// Number Թվային լիտերալ
type Number struct {
	Value float64
}

// NewNumber Նոր թվային օբյեկտ
func NewNumber(v float64) *Number {
	return &Number{Value: v}
}

// Text Տեքստային լիտերալ
type Text struct {
	Value string
}

// NewText Նոր տեքստային օբյեկտ
func NewText(v string) *Text {
	return &Text{Value: v}
}

// Array Զանգվածի լիտերալ
type Array struct {
	Elements []Node
}

// NewArray Նոր ցուցակ օբյեկտ
func NewArray(v []Node) *Array {
	return &Array{Elements: make([]Node, 0)}
}

// Variable Փոփոխական
type Variable struct {
	Name string
}

// NewVariable Ստեղծում է նոր փոփոխականի օբյեկտ
func NewVariable(nm string) *Variable {
	return &Variable{Name: nm}
}

// Unary Ունար գործողություն
type Unary struct {
	Operation string
	Right     Node
}

// NewUnary Նոր միտեղանի գործողություն
func NewUnary(op string, eo Node) *Unary {
	return &Unary{Operation: op, Right: eo}
}

// Binary Բինար գործողություն
type Binary struct {
	Operation   string
	Left, Right Node
}

// NewBinary Նոր երկտեղանի գործողություն
func NewBinary(op string, exo, exi Node) *Binary {
	return &Binary{oper: op, expro: exo, expri: exi}
}

// Apply Ֆունկցիայի կիրառում
type Apply struct {
	callee    Node
	arguments []Node
}

// NewApply ...
func NewApply(cl Node, ags []Node) *Apply {
	return &Apply{callee: cl, arguments: ags}
}

// SetCallee ...
func (a *Apply) SetCallee(sb *Subroutine) {
	a.callee = sb
}

// Let Վերագրում
type Let struct {
	Place Node
	Value Node
}

// NewLet ...
func NewLet(pl Node, vl Node) *Let {
	return &Let{Place: pl, Value: vl}
}

// Input Ներմուծում
type Input struct {
	Place Node
}

// NewInput ...
func NewInput(pl Node) *Input {
	return &Input{Place: pl}
}

// Print Արտածում
type Print struct {
	Value Node
}

// NewPrint ...
func NewPrint(vl Node) *Print {
	return &Print{Value: vl}
}

// If Ճյուղավորում
type If struct {
	Condition   Node
	Decision    Node
	Alternative Node
}

// NewIf ...
func NewIf(co Node, de Node) *If {
	return &If{Condition: co, Decision: de}
}

// SetElse ...
func (s *If) SetElse(el Node) {
	s.Alternative = el
}

// While Նախապայմանով ցիկլ
type While struct {
	Condition Node
	Body      Node
}

// NewWhile ...
func NewWhile(co Node, bo Node) *While {
	return &While{Condition: co, Body: bo}
}

// For Հաշվիչով ցիկլ
type For struct {
	parameter string
	begin     Node
	end       Node
	step      Node
	body      Node
}

// NewFor ...
func NewFor(p string, b, e, s Node, d Node) *For {
	return &For{parameter: p, begin: b, end: e, step: s, body: d}
}

// Call Ենթածրագիր կանչ
type Call struct {
	callee    Node
	arguments []Node
}

// NewCall ...
func NewCall(cl Node, ags []Node) *Call {
	return &Call{callee: cl, arguments: ags}
}

// SetCallee ...
func (c *Call) SetCallee(sb *Subroutine) {
	c.callee = sb
}

// Sequence Հրամանների հաջորդականություն
type Sequence struct {
	items []Node
}

// NewSequence ...
func NewSequence() *Sequence {
	return &Sequence{items: make([]Node, 0)}
}

// AddItem ...
func (s *Sequence) AddItem(e Node) {
	s.items = append(s.items, e)
}

// Subroutine Ենթածրագիր
type Subroutine struct {
	name       string
	parameters []Node
	body       Node
}

// NewSubroutine Նոր ենթածրագրի օբյեկտ
func NewSubroutine(nm string, pars []Node, dy Node) *Subroutine {
	return &Subroutine{name: nm, parameters: pars, body: dy}
}

// SetBody Լրացնել ենթածրագրի մարմինը
func (s *Subroutine) SetBody(q Node) {
	s.body = q
}

// Program Ծրագիր
type Program struct {
	members map[string]Node
}

// NewProgram ...
func NewProgram() *Program {
	return &Program{members: make(map[string]Node)}
}

// AddMember ...
func (p *Program) AddMember(su Node) {
	sn := su.(*Subroutine).name
	p.members[sn] = su
}