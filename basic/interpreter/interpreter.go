package interpreter

import (
	"basic/ast"
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

// Interpreter Ինտերպրետատորի ստրուկտուրան
type Interpreter struct {
	// Կատարվող ծրագրի ցուցիչը
	program *ast.Program
	// կատարման միջավայրը
	env *environment
}

// NewInterpreter Նոր ինտերպրետատոր օբյեկտ
func New() *Interpreter {
	return &Interpreter{program: nil, env: &environment{}}
}

// Execute Կատարում է ամբողջ ծրագիրը՝ սկսելով Main անունով ենթածրագրից։
func (i *Interpreter) Execute(p *ast.Program) {
	// որսալ սխալն ու արտածել հաղորդագրությունը
	defer func() {
		if err := recover(); err != nil {
			fmt.Printf("Կատարման սխալ: %s։\n", err)
		}
	}()

	// ծրագրի ցուցիչը պահել ենթածրագրերին հղվելու համար
	i.program = p

	i.env.openScope()
	defer i.env.closeScope()

	// Main ֆունկցաիյի մարմնի կատարում
	cmain := ast.Call{Callee: "Main", Arguments: make([]ast.Node, 0)}
	i.execute(&cmain)
}

//
func (i *Interpreter) evaluateBoolean(b *ast.Boolean) *value {
	return &value{kind: vBoolean, boolean: b.Value}
}

//
func (i *Interpreter) evaluateNumber(n *ast.Number) *value {
	return &value{kind: vNumber, number: n.Value}
}

//
func (i *Interpreter) evaluateText(t *ast.Text) *value {
	return &value{kind: vText, text: t.Value}
}

//
func (i *Interpreter) evaluateArray(a *ast.Array) *value {
	els := len(a.Elements)
	res := &value{kind: vArray, array: make([]*value, els)}
	for j, e := range a.Elements {
		res.array[j] = i.evaluate(e)
	}
	return res
}

//
func (i *Interpreter) evaluateVariable(v *ast.Variable) *value {
	if vp := i.env.get(v.Name); vp != nil {
		return vp
	}

	i.env.set(v.Name, &value{})
	return i.env.get(v.Name)
}

//
func (i *Interpreter) evaluateUnary(u *ast.Unary) *value {
	var result *value

	switch u.Operation {
	case "-":
		rv := i.evaluate(u.Right)
		if !rv.isNumber() {
			panic("- գործողության արգումենտը պետք է թիվ լինի")
		}
		result = &value{kind: vNumber, number: -rv.number}
	case "NOT":
		rv := i.evaluate(u.Right)
		if !rv.isBoolean() {
			panic("NOT գործողության արգումենտը պետք է տրամաբանական արժեք լինի")
		}
		result = &value{kind: vBoolean, boolean: !rv.boolean}
	}

	return result
}

//
func (i *Interpreter) evaluateBinary(b *ast.Binary) *value {
	var result *value

	switch b.Operation {
	case "+", "-", "*", "/", "\\", "^":
		rl := i.evaluate(b.Left)
		if !rl.isNumber() {
			panic("Type error")
		}

		rr := i.evaluate(b.Right)
		if !rr.isNumber() {
			panic("Type error")
		}

		return operations[b.Operation](rl, rr)
	case "&":
		rl := i.evaluate(b.Left)
		if !rl.isText() {
			panic("Type error")
		}

		rr := i.evaluate(b.Right)
		if !rr.isText() {
			panic("Type error")
		}

		result = &value{kind: vText, text: rl.text + rr.text}
	case "AND", "OR":
		rl := i.evaluate(b.Left)
		if !rl.isBoolean() {
			panic(b.Operation + " գործողության ձախ կողմում սպասվում է տրամաբանական արժեք")
		}

		rr := i.evaluate(b.Right)
		if !rr.isBoolean() {
			panic(b.Operation + " գործողության աջ կողմում սպասվում է տրամաբանական արժեք")
		}

		return operations[b.Operation](rl, rr)
	case "[]":
		rl := i.evaluate(b.Left)
		if !rl.isArray() {
			panic("[]-ի ձախ կողմում պետք է զանգված լինի")
		}

		rr := i.evaluate(b.Right)
		if !rr.isNumber() {
			panic("[]-ի ինդեքսը պետք է թիվ լինի")
		}

		ix := int(rr.number)
		if ix < 0 || ix >= len(rl.array) {
			panic("ինդեքսը զանգվածի սահմաններից դուրս է")
		}

		result = rl.array[ix]
	case "=", "<>", ">", ">=", "<", "<=":
		rl := i.evaluate(b.Left)
		if rl.isArray() {
			panic("զանգվածը չի կարող համեմատվել")
		}

		rr := i.evaluate(b.Right)
		if rr.isArray() {
			panic("զանգվածը չի կարող համեմատվել")
		}

		if rl.kind != rr.kind {
			panic("կարող են համեմատվել միայն նույն տիպի արժեքները")
		}

		return operations[b.Operation](rl, rr)
	}

	return result
}

//
func (i *Interpreter) evaluateApply(a *ast.Apply) *value {
	// կանչի արգումենտների հաշվարկը
	avals := make([]*value, len(a.Arguments))
	for j, arg := range a.Arguments {
		avals[j] = i.evaluate(arg)
	}

	// նախապատվությունը տալիս ենք ներդրված ենթածրագրերին (?)
	if bf, exists := builtins[a.Callee]; exists {
		return bf(avals...)
	}

	// ծրագրավորողի սահմանած ենթածրագրի կանչ
	if uf, exists := i.program.Members[a.Callee]; exists {
		uds := uf.(*ast.Subroutine)
		if len(avals) != len(uds.Parameters) {
			panic("կիրառության արգումենտների և ենթածրագրի պարամետրերի քանակները հավասար չեն")
		}

		i.env.openScope() // նոր տիրույթ
		defer i.env.closeScope()
		i.env.set(a.Callee, &value{kind: vUndefined})
		// ենթածրագրի պարամետրերի համապատասխանեցումը կանչի արգումենտներին
		for j, p := range uds.Parameters {
			i.env.set(p, avals[j])
		}
		// ենթածրագրի մարմնի կատարում
		i.execute(uds.Body)
		result := i.env.get(a.Callee)

		return result
	}

	panic(a.Callee + ". անծանոթ ենթածրագրի կիրառություն")
}

//
func (i *Interpreter) evaluate(n ast.Node) *value {
	var result *value

	switch e := n.(type) {
	case *ast.Boolean:
		result = i.evaluateBoolean(e)
	case *ast.Number:
		result = i.evaluateNumber(e)
	case *ast.Text:
		result = i.evaluateText(e)
	case *ast.Array:
		result = i.evaluateArray(e)
	case *ast.Variable:
		result = i.evaluateVariable(e)
	case *ast.Unary:
		result = i.evaluateUnary(e)
	case *ast.Binary:
		result = i.evaluateBinary(e)
	case *ast.Apply:
		result = i.evaluateApply(e)
	}

	return result
}

//
func (i *Interpreter) executeDim(d *ast.Dim) {
	sz := i.evaluate(d.Size)
	if !sz.isNumber() {
		panic("Type error")
	}

	arr := &value{kind: vArray, array: make([]*value, int(sz.number))}
	for i := 0; i < len(arr.array); i++ {
		arr.array[i] = &value{}
	}
	i.env.set(d.Name, arr)
}

//
func (i *Interpreter) executeLet(l *ast.Let) {
	p := i.evaluate(l.Place)
	v := i.evaluate(l.Value)
	*p = *v
}

//
func (i *Interpreter) executeInput(s *ast.Input) {
	pl := i.evaluate(s.Place)

	fmt.Print("? ")
	reader := bufio.NewReader(os.Stdin)
	line, _ := reader.ReadString('\n')
	line = strings.Trim(line, " \n\t\r")

	if line == "TRUE" {
		*pl = value{kind: vBoolean, boolean: true}
	}
	if line == "FALSE" {
		*pl = value{kind: vBoolean, boolean: false}
	}

	num, err := strconv.ParseFloat(line, 64)
	if err == nil {
		*pl = value{kind: vNumber, number: num}
	}

	*pl = value{kind: vText, text: line}
}

//
func (i *Interpreter) executePrint(p *ast.Print) {
	e := i.evaluate(p.Value)
	fmt.Println(e)
}

//
func (i *Interpreter) executeIf(b *ast.If) {
	c := i.evaluate(b.Condition)
	if !c.isBoolean() {
		panic("IF հրամանի պայմանը պետք է լինի տրամաբանական արժեք")
	}

	if c.boolean {
		i.execute(b.Decision)
	} else {
		if b.Alternative != nil {
			i.execute(b.Alternative)
		}
	}
}

//
func (i *Interpreter) executeWhile(w *ast.While) {
	for {
		c := i.evaluate(w.Condition)
		if !c.isBoolean() {
			panic("WHILE հրամանի պայմանը պետք է տրամաբանական արժեք լինի")
		}

		if !c.boolean {
			break
		}

		i.execute(w.Body)
	}
}

//
func (i *Interpreter) executeFor(f *ast.For) {
	// TODO type checks

	initialize := &ast.Let{
		Place: f.Parameter,
		Value: f.Begin,
	}

	condition := &ast.Binary{
		Operation: "<=",
		Left:      f.Parameter,
		Right:     f.End,
	}

	increment := &ast.Let{
		Place: f.Parameter,
		Value: &ast.Binary{
			Operation: "+",
			Left:      f.Parameter,
			Right:     f.Step,
		}}

	i.execute(initialize)
	for {
		cv := i.evaluate(condition)
		if !cv.boolean {
			break
		}

		i.execute(f.Body)
		i.execute(increment)
	}
}

//
func (i *Interpreter) executeCall(c *ast.Call) {
	_ = i.evaluateApply(c)
}

//
func (i *Interpreter) executeSequence(s *ast.Sequence) {
	i.env.openScope()
	defer i.env.closeScope()
	for _, st := range s.Items {
		i.execute(st)
	}
}

//
func (i *Interpreter) execute(n ast.Node) {
	switch s := n.(type) {
	case *ast.Dim:
		i.executeDim(s)
	case *ast.Let:
		i.executeLet(s)
	case *ast.Input:
		i.executeInput(s)
	case *ast.Print:
		i.executePrint(s)
	case *ast.If:
		i.executeIf(s)
	case *ast.While:
		i.executeWhile(s)
	case *ast.For:
		i.executeFor(s)
	case *ast.Call:
		i.executeCall(s)
	case *ast.Sequence:
		i.executeSequence(s)
	}
}
