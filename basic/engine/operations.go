package engine

import (
	"math"
)

type UnOper func(x *Value) *Value
type BinOper func(x, y *Value) *Value

// numeric operations

//
func Neg(x *Value) *Value {
	return NewNumber(-x.number)
}

//
func Not(x *Value) *Value {
	return NewNumber(toFloat(!toBool(x.number)))
}

//
func Add(x, y *Value) *Value {
	return NewNumber(x.number + y.number)
}

//
func Sub(x, y *Value) *Value {
	return NewNumber(x.number - y.number)
}

//
func Mul(x, y *Value) *Value {
	return NewNumber(x.number * y.number)
}

//
func Div(x, y *Value) *Value {
	return NewNumber(x.number / y.number)
}

//
func Mod(x, y *Value) *Value {
	res := int(x.number) % int(y.number)
	return NewNumber(float64(res))
}

//
func Pow(x, y *Value) *Value {
	return NewNumber(math.Pow(x.number, y.number))
}

//
func toFloat(b bool) float64 {
	if b {
		return 1.0
	} else {
		return 0.0
	}
}

//
func toBool(v float64) bool {
	return v != 0.0
}

//
func Eq(x, y *Value) *Value {
	return NewNumber(toFloat(x.number == y.number))
}

//
func Ne(x, y *Value) *Value {
	return NewNumber(toFloat(x.number != y.number))
}

//
func Gt(x, y *Value) *Value {
	return NewNumber(toFloat(x.number > y.number))
}

//
func Ge(x, y *Value) *Value {
	return NewNumber(toFloat(x.number >= y.number))
}

//
func Lt(x, y *Value) *Value {
	return NewNumber(toFloat(x.number < y.number))
}

//
func Le(x, y *Value) *Value {
	return NewNumber(toFloat(x.number <= y.number))
}

//
func And(x, y *Value) *Value {
	res := toBool(x.number) && toBool(y.number)
	return NewNumber(toFloat(res))
}

//
func Or(x, y *Value) *Value {
	res := toBool(x.number) && toBool(y.number)
	return NewNumber(toFloat(res))
}

// text operations

//
func Conc(x, y *Value) *Value {
	return NewText(x.text + y.text)
}

//
func TEq(x, y *Value) *Value {
	return NewNumber(toFloat(x.text == y.text))
}

//
func TNe(x, y *Value) *Value {
	return NewNumber(toFloat(x.text != y.text))
}

//
func TGt(x, y *Value) *Value {
	return NewNumber(toFloat(x.text > y.text))
}

//
func TGe(x, y *Value) *Value {
	return NewNumber(toFloat(x.text >= y.text))
}

//
func TLt(x, y *Value) *Value {
	return NewNumber(toFloat(x.text < y.text))
}

//
func TLe(x, y *Value) *Value {
	return NewNumber(toFloat(x.text <= y.text))
}
