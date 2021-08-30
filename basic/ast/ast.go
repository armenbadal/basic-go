package ast

// Node Աբստրակտ քերականական ծառի հանգույց
type Node interface{}

// Boolean Բուլյան լիտերալ
type Boolean struct {
	Value bool
}

// Number Թվային լիտերալ
type Number struct {
	Value float64
}

// Text Տեքստային լիտերալ
type Text struct {
	Value string
}

// Array Զանգվածի լիտերալ
type Array struct {
	Elements []Node
}

// Variable Փոփոխական
type Variable struct {
	Name string
}

// Unary Ունար գործողություն
type Unary struct {
	Operation string
	Right     Node
}

// Binary Բինար գործողություն
type Binary struct {
	Operation   string
	Left, Right Node
}

// Apply Ֆունկցիայի կիրառում
type Apply struct {
	Callee    string
	Arguments []Node
}

// Dim զանգվածի սահմանում
type Dim struct {
	Name string
	Size Node
}

// Let Վերագրում
type Let struct {
	Place Node
	Value Node
}

// Input Ներմուծում
type Input struct {
	Place Node
}

// Print Արտածում
type Print struct {
	Value Node
}

// If Ճյուղավորում
type If struct {
	Condition   Node
	Decision    Node
	Alternative Node
}

// While Նախապայմանով ցիկլ
type While struct {
	Condition Node
	Body      Node
}

// For Հաշվիչով ցիկլ
type For struct {
	Parameter Node
	Begin     Node
	End       Node
	Step      Node
	Body      Node
}

// Call Ենթածրագիր կանչ
type Call struct {
	Callee    string
	Arguments []Node
}

// Sequence Հրամանների հաջորդականություն
type Sequence struct {
	Items []Node
}

// Subroutine Ենթածրագիր
type Subroutine struct {
	Name       string
	Parameters []string
	Body       Node
}

// Program Ծրագիր
type Program struct {
	Members map[string]Node
}
