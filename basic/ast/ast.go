package ast

// Program Ծրագիր
type Program struct {
	Subroutines map[string]*Subroutine // ենթածրագրեր
}

// Subroutine Ենթածրագիր
type Subroutine struct {
	Name       string    // անուն
	Parameters []string  // պարամետրեր
	Body       Statement // մարմին
}

// Statement հրամանների ինտերֆեյս
type Statement any

// Dim զանգվածի սահմանում
type Dim struct {
	Name string     // անունը
	Size Expression // չափը
}

// Let Վերագրում
type Let struct {
	Place Expression // վերագրման տեղը
	Value Expression // վերագրվող արժեքը
}

// Input Ներմուծում
type Input struct {
	Place Expression // ներմուծված արժեքը պահելու տեղը
}

// Print Արտածում
type Print struct {
	Value Expression // արտածվելիք արժեք
}

// If Ճյուղավորում
type If struct {
	Condition   Expression // պայման
	Decision    Statement  // դրական ընտրություն
	Alternative Statement  // բացասական ընտրություն
}

// While Նախապայմանով ցիկլ
type While struct {
	Condition Expression // կատարման պայման
	Body      Statement  // մարմին
}

// For Հաշվիչով ցիկլ
type For struct {
	Parameter Expression // հաշվիչը
	Begin     Expression // հաշվիչի սկզբնական արժեք
	End       Expression // հաշվիջի վերջնական արժեք
	Step      Expression // հաշվիչի քայլը
	Body      Statement  // մարմինը
}

// Call Ենթածրագիր կանչ, նույնն է թե Apply
type Call = Apply

// Sequence Հրամանների հաջորդականություն
type Sequence struct {
	Items []Statement // հրամաններ
}

// Expression Արտահայտությունների ինտերֆեյս
type Expression any

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
	Elements []Expression
}

// Variable Փոփոխական
type Variable struct {
	Name string
}

// Unary Ունար գործողություն
type Unary struct {
	Operation string
	Right     Expression
}

// Binary Բինար գործողություն
type Binary struct {
	Operation string
	Left      Expression
	Right     Expression
}

// Apply Ֆունկցիայի կիրառում
type Apply struct {
	Callee    string       // ենթածրագրի անունը
	Arguments []Expression // արգումենտները
}
