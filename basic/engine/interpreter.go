package engine

// Value Ունիվերսալ արժեք
type Value struct {
	kind    rune
	boolean bool
	number  float64
	text    string
	array   []*Value
}

// Expression Արտահայտություններ
type Expression interface {
	evaluate(Environment) *Value
}

//
func (e *Value) evaluate(env Environment) *Value {
	return e
}

//
func (e *Variable) evaluate(env Environment) *Value {
	return env.Get(e.name)
}

//
func (e *Unary) evaluate(env Environment) *Value {
	res := e.expr.(Expression).evaluate(env)
	// if e.opcode == NEG {
	// 	res = NewNumber(-res.number)
	// } else if e.opcode == NOT {
	// 	if res.number == 0 {
	// 		res = NewNumber(1.0)
	// 	} else {
	// 		res = NewNumber(0.0)
	// 	}
	// }
	return res
}

//
func (e *Binary) evaluate(env Environment) *Value {
	//evo := e.expro.evaluate(env)
	//evi := e.expri.evaluate(env)
	///return e.opfun(evo, evi)
	return nil
}

//
func (e *Apply) evaluate(env Environment) *Value {
	return nil
}

// Statement Հրամանների ինտերֆեյսը
type Statement interface {
	execute(Environment)
}

//
func (s *Let) execute(env Environment) {
}

//
func (s *Input) execute(env Environment) {
}

//
func (s *Print) execute(env Environment) {
}

//
func (s *If) execute(env Environment) {
}

//
func (s *While) execute(env Environment) {
}

//
func (s *For) execute(env Environment) {
}

//
func (s *Call) execute(env Environment) {
	//clap := NewApply(s.callee, s.arguments)
	//clap.evaluate(env)
}

//
func (s *Sequence) execute(env Environment) {
	//for e := s.items.Front(); e != nil; e = e.Next() {
	//	e.Value.(Statement).execute(env)
	//}
}

// Execute Կատարում է ամբողջ ծրագիրը՝ սկսելով Main անունով ենթածրագրից։
func (p *Program) Execute() {
	//ep, found := p.members["Main"]
	//if found {
	//	entry := NewCall(ep, list.New())
	//	entry.execute(make(Environment))
	//}
}
