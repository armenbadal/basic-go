package interpreter

import (
	"basic/ast"
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

// interpreter Ինտերպրետատորի ստրուկտուրան
type interpreter struct {
	// Կատարվող ծրագրի ցուցիչը
	program *ast.Program
	// կատարման միջավայրը
	env *environment
}

// Execute Կատարում է ամբողջ ծրագիրը՝ սկսելով Main անունով ենթածրագրից։
func Execute(p *ast.Program) error {
	i := &interpreter{program: p, env: &environment{}}

	// գլոբալ տեսանելիության տիրույթ
	i.env.openScope()
	defer i.env.closeScope()

	// նախապես սահմանված փոփոխականներ
	i.env.set("pi", &value{kind: vNumber, number: math.Pi})

	// Main ֆունկցիայի կատարում
	cmain := ast.Call{Callee: "Main", Arguments: make([]ast.Expression, 0)}
	return i.execute(&cmain)
}

func (i *interpreter) evaluateBoolean(b *ast.Boolean) (*value, error) {
	return &value{kind: vBoolean, boolean: b.Value}, nil
}

func (i *interpreter) evaluateNumber(n *ast.Number) (*value, error) {
	return &value{kind: vNumber, number: n.Value}, nil
}

func (i *interpreter) evaluateText(t *ast.Text) (*value, error) {
	return &value{kind: vText, text: t.Value}, nil
}

func (i *interpreter) evaluateArray(a *ast.Array) (*value, error) {
	els := len(a.Elements)
	res := &value{kind: vArray, array: make([]*value, els)}
	for j, e := range a.Elements {
		val, err := i.evaluate(e)
		if err != nil {
			return nil, err
		}
		res.array[j] = val
	}
	return res, nil
}

func (i *interpreter) evaluateVariable(v *ast.Variable) (*value, error) {
	if vp := i.env.get(v.Name); vp != nil {
		return vp, nil
	}

	// TODO վերանայել այս կետը
	i.env.set(v.Name, &value{})
	return i.env.get(v.Name), nil
}

func (i *interpreter) evaluateUnary(u *ast.Unary) (*value, error) {
	var result *value

	switch u.Operation {
	case "-":
		rv, err := i.evaluate(u.Right)
		if err != nil {
			return nil, err
		}
		if !rv.isNumber() {
			return nil, fmt.Errorf("- գործողության արգումենտը պետք է թիվ լինի")
		}
		result = &value{kind: vNumber, number: -rv.number}
	case "NOT":
		rv, err := i.evaluate(u.Right)
		if err != nil {
			return nil, err
		}
		if !rv.isBoolean() {
			return nil, fmt.Errorf("NOT գործողության արգումենտը պետք է տրամաբանական արժեք լինի")
		}
		result = &value{kind: vBoolean, boolean: !rv.boolean}
	}

	return result, nil
}

// թվաբանական գործողություններ
func (i *interpreter) evaluateArithmetic(b *ast.Binary) (*value, error) {
	rl, erl := i.evaluate(b.Left)
	if erl != nil {
		return nil, erl
	}
	if !rl.isNumber() {
		return nil, fmt.Errorf("%s գործողության ձախ կողմում սպասվում է թվային արժեք", b.Operation)
	}

	rr, err := i.evaluate(b.Right)
	if err != nil {
		return nil, err
	}
	if !rr.isNumber() {
		return nil, fmt.Errorf("%s գործողության աջ կողմում սպասվում է թվային արժեք", b.Operation)
	}

	return operations[b.Operation](rl, rr), nil
}

// տեքստերի միակցում (կոնկատենացիա)
func (i *interpreter) evaluateTextConcatenation(b *ast.Binary) (*value, error) {
	rl, el := i.evaluate(b.Left)
	if el != nil {
		return nil, el
	}
	if !rl.isText() {
		return nil, fmt.Errorf("& գործողության ձախ կողմում սպասվում է տեքստային արժեք")
	}

	rr, er := i.evaluate(b.Right)
	if er != nil {
		return nil, er
	}
	if !rr.isText() {
		return nil, fmt.Errorf("& գործողության աջ կողմում սպասվում է տեքստային արժեք")
	}

	return &value{kind: vText, text: rl.text + rr.text}, nil
}

// տրամաբանական գործողություններ
func (i *interpreter) evaluateLogic(b *ast.Binary) (*value, error) {
	rl, err := i.evaluate(b.Left)
	if err != nil {
		return nil, err
	}
	if !rl.isBoolean() {
		return nil, fmt.Errorf("%s գործողության ձախ կողմում սպասվում է տրամաբանական արժեք", b.Operation)
	}

	rr, err := i.evaluate(b.Right)
	if err != nil {
		return nil, err
	}
	if !rr.isBoolean() {
		return nil, fmt.Errorf("%s գործողության աջ կողմում սպասվում է տրամաբանական արժեք", b.Operation)
	}

	return operations[b.Operation](rl, rr), nil
}

// զանգվածի ինդեքսավորման գործողություն
func (i *interpreter) evaluateIndexing(b *ast.Binary) (*value, error) {
	rl, el := i.evaluate(b.Left)
	if el != nil {
		return nil, el
	}
	if !rl.isArray() {
		return nil, fmt.Errorf("[]-ի ձախ կողմում պետք է զանգված լինի")
	}

	rr, er := i.evaluate(b.Right)
	if er != nil {
		return nil, er
	}
	if !rr.isNumber() {
		return nil, fmt.Errorf("[]-ի ինդեքսը պետք է թիվ լինի")
	}

	ix := int(rr.number)
	if ix < 0 || ix >= len(rl.array) {
		return nil, fmt.Errorf("ինդեքսը զանգվածի սահմաններից դուրս է")
	}

	return rl.array[ix], nil
}

// համեմատման գործողություններ
func (i *interpreter) evaluateComparison(b *ast.Binary) (*value, error) {
	rl, err := i.evaluate(b.Left)
	if err != nil {
		return nil, err
	}
	if rl.isArray() {
		return nil, fmt.Errorf("զանգվածը չի կարող համեմատվել")
	}

	rr, err := i.evaluate(b.Right)
	if err != nil {
		return nil, err
	}
	if rr.isArray() {
		return nil, fmt.Errorf("զանգվածը չի կարող համեմատվել")
	}

	if rl.kind != rr.kind {
		return nil, fmt.Errorf("կարող են համեմատվել միայն նույն տիպի արժեքները")
	}

	return operations[b.Operation](rl, rr), nil
}

func (i *interpreter) evaluateBinary(b *ast.Binary) (*value, error) {
	switch b.Operation {
	case "+", "-", "*", "/", "\\", "^":
		return i.evaluateArithmetic(b)
	case "&":
		return i.evaluateTextConcatenation(b)
	case "AND", "OR":
		return i.evaluateLogic(b)
	case "[]":
		return i.evaluateIndexing(b)
	case "=", "<>", ">", ">=", "<", "<=":
		return i.evaluateComparison(b)
	}

	return nil, fmt.Errorf("անծանոթ երկտեղանի գործողություն")
}

// Արտահայտությունների ցուցակի հաշվարկելը
func (i *interpreter) evaluateExpressionList(es []ast.Expression) ([]*value, error) {
	result := make([]*value, len(es))
	for j, e := range es {
		val, err := i.evaluate(e)
		if err != nil {
			return nil, err
		}
		result[j] = val
	}
	return result, nil
}

// Օգտագործողի սահմանած ենթապրագրի կանչի կատարումը
func (i *interpreter) evaluateSubroutineCall(subr *ast.Subroutine, args []ast.Expression) (*value, error) {
	if len(args) != len(subr.Parameters) {
		return nil, fmt.Errorf("կիրառության արգումենտների և ենթածրագրի պարամետրերի քանակները հավասար չեն")
	}

	// արգումենտների հաշվարկումը
	argValues, err := i.evaluateExpressionList(args)
	if err != nil {
		return nil, err
	}

	// ենթածրագրի պարամետրերի համապատասխանեցումը կանչի արգումենտներին
	for j, p := range subr.Parameters {
		i.env.set(p, argValues[j])
	}

	// ենթածրագրի մարմնի կատարում
	i.env.openScope() // նոր տիրույթ
	defer i.env.closeScope()
	i.env.set(subr.Name, &value{kind: vUndefined})

	err = i.execute(subr.Body)
	if err != nil {
		return nil, err
	}

	result := i.env.get(subr.Name)
	return result, nil
}

func (i *interpreter) evaluateApply(a *ast.Apply) (*value, error) {
	// ծրագրավորողի սահմանած ենթածրագրի կանչ
	if subr, exists := i.program.Subroutines[a.Callee]; exists {
		return i.evaluateSubroutineCall(subr, a.Arguments)
	}

	// ներդրված ենթածրագրի կանչ
	if builtin, exists := builtins[a.Callee]; exists {
		avals, err := i.evaluateExpressionList(a.Arguments)
		if err != nil {
			return nil, err
		}
		return builtin(avals...), nil
	}

	return nil, fmt.Errorf("%s. անծանոթ ենթածրագրի կիրառություն", a.Callee)
}

func (i *interpreter) evaluate(n ast.Expression) (*value, error) {
	var result *value
	var err error

	switch e := n.(type) {
	case *ast.Boolean:
		result, err = i.evaluateBoolean(e)
	case *ast.Number:
		result, err = i.evaluateNumber(e)
	case *ast.Text:
		result, err = i.evaluateText(e)
	case *ast.Array:
		result, err = i.evaluateArray(e)
	case *ast.Variable:
		result, err = i.evaluateVariable(e)
	case *ast.Unary:
		result, err = i.evaluateUnary(e)
	case *ast.Binary:
		result, err = i.evaluateBinary(e)
	case *ast.Apply:
		result, err = i.evaluateApply(e)
	}

	return result, err
}

func (i *interpreter) executeDim(d *ast.Dim) error {
	sz, err := i.evaluate(d.Size)
	if err != nil {
		return err
	}
	if !sz.isNumber() {
		return fmt.Errorf("type error")
	}

	arr := &value{kind: vArray, array: make([]*value, int(sz.number))}
	for i := 0; i < len(arr.array); i++ {
		arr.array[i] = &value{}
	}
	i.env.set(d.Name, arr)
	return nil
}

func (i *interpreter) executeLet(l *ast.Let) error {
	p, err := i.evaluate(l.Place)
	if err != nil {
		return err
	}

	v, err := i.evaluate(l.Value)
	if err != nil {
		return err
	}

	*p = *v
	return nil
}

func (i *interpreter) executeInput(s *ast.Input) error {
	pl, err := i.evaluate(s.Place)
	if err != nil {
		return err
	}

	fmt.Print("? ")
	reader := bufio.NewReader(os.Stdin)
	line, err := reader.ReadString('\n')
	if err != nil {
		return fmt.Errorf("ներմուծման սխալ: %w", err)
	}
	line = strings.Trim(line, " \n\t\r")

	switch line {
	case "TRUE":
		*pl = value{kind: vBoolean, boolean: true}
	case "FALSE":
		*pl = value{kind: vBoolean, boolean: false}
	default:
		num, err := strconv.ParseFloat(line, 64)
		if err == nil {
			*pl = value{kind: vNumber, number: num}
		} else {
			*pl = value{kind: vText, text: line}
		}
	}

	return nil
}

func (i *interpreter) executePrint(p *ast.Print) error {
	str, err := i.evaluate(p.Value)
	if err != nil {
		return err
	}
	fmt.Println(str)
	return nil
}

func (i *interpreter) executeIf(b *ast.If) error {
	i.env.openScope()
	defer i.env.closeScope()

	cond, err := i.evaluate(b.Condition)
	if err != nil {
		return err
	}
	if !cond.isBoolean() {
		return fmt.Errorf("IF հրամանի պայմանը պետք է լինի տրամաբանական արժեք")
	}

	if cond.boolean {
		err := i.execute(b.Decision)
		if err != nil {
			return err
		}
	} else {
		if b.Alternative != nil {
			err := i.execute(b.Alternative)
			if err != nil {
				return err
			}
		}
	}

	return nil
}

func (i *interpreter) executeWhile(w *ast.While) error {
	i.env.openScope()
	defer i.env.closeScope()

	for {
		cond, err := i.evaluate(w.Condition)
		if err != nil {
			return err
		}
		if !cond.isBoolean() {
			return fmt.Errorf("WHILE հրամանի պայմանը պետք է տրամաբանական արժեք լինի")
		}

		if !cond.boolean {
			break
		}

		if err := i.execute(w.Body); err != nil {
			return err
		}
	}

	return nil
}

func (i *interpreter) executeFor(f *ast.For) error {
	i.env.openScope()
	defer i.env.closeScope()

	paramVar, ok := f.Parameter.(*ast.Variable)
	if !ok {
		return fmt.Errorf("FOR հրամանի պարամետրը պետք է լինի փոփոխական")
	}
	param := paramVar.Name
	begin, err := i.evaluate(f.Begin)
	if err != nil {
		return err
	}
	if !begin.isNumber() {
		return fmt.Errorf("FOR հրամանի պարամետրի արժեքը պետք է լինի թիվ")
	}
	i.env.set(param, begin.clone())

	end, err := i.evaluate(f.End)
	if err != nil {
		return err
	}
	if !end.isNumber() {
		return fmt.Errorf("FOR հրամանի պարամետրի արժեքը պետք է լինի թիվ")
	}

	step, err := i.evaluate(f.Step)
	if err != nil {
		return err
	}
	if !step.isNumber() {
		return fmt.Errorf("FOR հրամանի պարամետրի քայլը պետք է լինի թիվ")
	}

	for {
		pv := i.env.get(param)
		if step.number > 0 && pv.number > end.number {
			break
		} else if step.number < 0 && pv.number < end.number {
			break
		}

		err := i.execute(f.Body)
		if err != nil {
			return err
		}

		pv.number += step.number
	}

	return nil
}

func (i *interpreter) executeCall(c *ast.Call) error {
	_, err := i.evaluateApply(&ast.Apply{Callee: c.Callee, Arguments: c.Arguments})
	return err
}

func (i *interpreter) executeSequence(s *ast.Sequence) error {
	i.env.openScope()
	defer i.env.closeScope()

	for _, st := range s.Items {
		err := i.execute(st)
		if err != nil {
			return err
		}
	}

	return nil
}

func (i *interpreter) execute(n ast.Statement) error {
	switch s := n.(type) {
	case *ast.Dim:
		return i.executeDim(s)
	case *ast.Let:
		return i.executeLet(s)
	case *ast.Input:
		return i.executeInput(s)
	case *ast.Print:
		return i.executePrint(s)
	case *ast.If:
		return i.executeIf(s)
	case *ast.While:
		return i.executeWhile(s)
	case *ast.For:
		return i.executeFor(s)
	case *ast.Call:
		return i.executeCall(s)
	case *ast.Sequence:
		return i.executeSequence(s)
	}

	return nil
}
