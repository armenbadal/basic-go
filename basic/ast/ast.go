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
	Callee    string // ենթածրագրի անունը
	Arguments []Node // արգումենտները
}

// Dim զանգվածի սահմանում
type Dim struct {
	Name string // զանգվածի անունը
	Size Node   // զանգվածի չափը
}

// Let Վերագրում
type Let struct {
	Place Node // վերագրման տեղը
	Value Node // վերագրվող արժեքը
}

// Input Ներմուծում
type Input struct {
	Place Node // ներմուծված արժեքը պահելու տեղը
}

// Print Արտածում
type Print struct {
	Value Node // արտածվելիք արժեք
}

// If Ճյուղավորում
type If struct {
	Condition   Node // պայման
	Decision    Node // դրական ընտրություն
	Alternative Node // բացասական ընտրություն
}

// While Նախապայմանով ցիկլ
type While struct {
	Condition Node // կատարման պայման
	Body      Node // մարմին
}

// For Հաշվիչով ցիկլ
type For struct {
	Parameter Node // ցիկլի հաշվիչը
	Begin     Node // հաշվիչի սկզբնական արժեք
	End       Node // հաշվիջի վերջնական արժեք
	Step      Node // հաշվիչի քայլը
	Body      Node // ցիկլի մարմինը
}

// Call Ենթածրագիր կանչ, նույնն է թե Apply
type Call = Apply

// Sequence Հրամանների հաջորդականություն
type Sequence struct {
	Items []Node // հրամաններ
}

// Subroutine Ենթածրագիր
type Subroutine struct {
	Name       string   // ենթածրագրի անուն
	Parameters []string // պարամետրերի ցուցակ
	Body       Node     // մարմին
}

// Program Ծրագիր
type Program struct {
	Members map[string]Node // ենթածրագրերի ցուցակ
}
