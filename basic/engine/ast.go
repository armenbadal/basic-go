package engine

// Node Աբստրակտ քերականական ծառի հանգույց
type Node interface{}

// Բազային տիպեր
const (
	BOOLEAN = 'B'
	NUMBER  = 'N'
	TEXT    = 'T'
	ARRAY   = 'A'
)

// Boolean Բուլյան լիտերալ
type Boolean struct {
	value bool
}

// NewBoolean Նոր բուլյան օբյեկտ
func NewBoolean(v bool) *Boolean {
	return &Boolean{value: v}
}

// Number Թվային լիտերալ
type Number struct {
	value float64
}

// NewNumber Նոր թվային օբյեկտ
func NewNumber(v float64) *Number {
	return &Number{value: v}
}

// Text Տեքստային լիտերալ
type Text struct {
	value string
}

// NewText Նոր տեքստային օբյեկտ
func NewText(v string) *Text {
	return &Text{value: v}
}

// Array Զանգվածի լիտերալ
type Array struct {
	elements []Node
}

// NewArray Նոր ցուցակ օբյեկտ
func NewArray(v []interface{}) *Array {
	return &Array{elements: make([]Node, 0)}
}

// Variable Փոփոխական
type Variable struct {
	name string
}

// NewVariable Ստեղծում է նոր փոփոխականի օբյեկտ
func NewVariable(nm string) *Variable {
	return &Variable{name: nm}
}

// Unary Ունար գործողություն
type Unary struct {
	oper string
	expr Node
}

// NewUnary Նոր միտեղանի գործողություն
func NewUnary(op string, eo Node) *Unary {
	return &Unary{oper: op, expr: eo}
}

// Binary Բինար գործողություն
type Binary struct {
	oper         string
	expro, expri Node
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
func NewApply(cl *Subroutine, ags []Node) *Apply {
	return &Apply{callee: cl, arguments: ags}
}

// SetCallee ...
func (a *Apply) SetCallee(sb *Subroutine) {
	a.callee = sb
}

// Let Վերագրում
type Let struct {
	name string // ? Variable
	expr Node
}

// NewLet ...
func NewLet(vn string, ex Node) *Let {
	return &Let{name: vn, expr: ex}
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
	expr Node
}

// NewPrint ...
func NewPrint(ex Node) *Print {
	return &Print{expr: ex}
}

// If Ճյուղավորում
type If struct {
	condition   Node
	decision    Node
	alternative Node
}

// NewIf ...
func NewIf(co Node, de Node) *If {
	return &If{condition: co, decision: de}
}

// SetElse ...
func (s *If) SetElse(el Node) {
	s.alternative = el
}

// While Նախապայմանով ցիկլ
type While struct {
	condition Node
	body      Node
}

// NewWhile ...
func NewWhile(co Node, bo Node) *While {
	return &While{condition: co, body: bo}
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
	//p.members[su.name] = su
}
