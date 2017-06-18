
package engine

import (
	"container/list"
)

//
func (e *Value) evaluate(env Environment) *Value {
	return e
}


//
func (e *Variable) evaluate(env Environment) *Value {
	return env[e.name]
}

//
func (e *Unary) evaluate(env Environment) *Value {
	res := e.expr.evaluate(env)
	if e.opcode == NEG {
		res = NewNumber(-res.number)
	} else if e.opcode == NOT {
		if res.number == 0 {
			res = NewNumber(1.0)
		} else {
			res = NewNumber(0.0)
		}
	}
	return res
}

//
func (e *Binary) evaluate(env Environment) *Value {
	evo := e.expro.evaluate(env)
	evi := e.expri.evaluate(env)

	var res *Value
	switch e.opcode {
	case ADD:
		res = NewNumber(evo.number + evi.number)
	case SUB:
		res = NewNumber(evo.number - evi.number)
//	case CONC:
//		res = NewNumber(evo.number + evi.number)
	case MUL:
		res = NewNumber(evo.number * evi.number)
	case DIV:
		res = NewNumber(evo.number / evi.number)
//	case MOD:
//		res = NewNumber(evo.number + evi.number)
//	case POW:
//		res = NewNumber(evo.number + evi.number)		
	}
	
	return res
}

//
func (e *Apply) evaluate(env Environment) *Value {
	return nil
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
	clap := NewApply(s.callee, s.arguments)
	clap.evaluate(env)
}

//
func (s *Sequence) execute(env Environment) {
	for e := s.items.Front(); e != nil; e = e.Next() {
		e.Value.(Statement).execute(env)
	}
}


// Կատարում է ամբողջ ծրագիրը՝ սկսելով Main անունով ենթածրագրից։
func (p *Program) Execute() {
	ep, found := p.Members["Main"]
	if found {
		entry := NewCall(ep, list.New())
		entry.execute(make(Environment))
	}
}

