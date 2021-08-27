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
	return &Array{Elements: v}
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
	return &Binary{Operation: op, Left: exo, Right: exi}
}

// Apply Ֆունկցիայի կիրառում
type Apply struct {
	Callee    Node
	Arguments []Node
}

// NewApply ...
func NewApply(cl Node, ags []Node) *Apply {
	return &Apply{Callee: cl, Arguments: ags}
}

// SetCallee ...
func (a *Apply) SetCallee(sb Node) {
	a.Callee = sb
}

// Dim զանգվածի սահմանում
type Dim struct {
	Name string
	Size Node
}

func NewDim(nm string, sz Node) *Dim {
	return &Dim{Name: nm, Size: sz}
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
	Parameter Node
	Begin     Node
	End       Node
	Step      Node
	Body      Node
}

// NewFor ...
func NewFor(p, b, e, s Node, d Node) *For {
	return &For{Parameter: p, Begin: b, End: e, Step: s, Body: d}
}

// Call Ենթածրագիր կանչ
type Call struct {
	Callee    Node
	Arguments []Node
}

// NewCall ...
func NewCall(cl Node, ags []Node) *Call {
	return &Call{Callee: cl, Arguments: ags}
}

// SetCallee ...
func (c *Call) SetCallee(sb *Subroutine) {
	c.Callee = sb
}

// Sequence Հրամանների հաջորդականություն
type Sequence struct {
	Items []Node
}

// NewSequence ...
func NewSequence() *Sequence {
	return &Sequence{Items: make([]Node, 0)}
}

// AddItem ...
func (s *Sequence) AddItem(e Node) {
	s.Items = append(s.Items, e)
}

// Subroutine Ենթածրագիր
type Subroutine struct {
	Name       string
	Parameters []Node
	Body       Node
}

// NewSubroutine Նոր ենթածրագրի օբյեկտ
func NewSubroutine(nm string, pars []Node, dy Node) *Subroutine {
	return &Subroutine{Name: nm, Parameters: pars, Body: dy}
}

// SetBody Լրացնել ենթածրագրի մարմինը
func (s *Subroutine) SetBody(q Node) {
	s.Body = q
}

// Program Ծրագիր
type Program struct {
	Members map[string]Node
}

// NewProgram ...
func NewProgram() *Program {
	return &Program{Members: make(map[string]Node)}
}

// AddMember ...
func (p *Program) AddMember(su Node) {
	sn := su.(*Subroutine).Name
	p.Members[sn] = su
}
