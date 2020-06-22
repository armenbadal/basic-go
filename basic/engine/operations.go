package engine

import (
	"math"
)

// UnOper ...
type UnOper func(x *Value) *Value

// BinOper ...
type BinOper func(x, y *Value) *Value

// numeric operations

// Neg ...
func Neg(x *Value) *Value {
	return NewNumber(-x.number)
}

// Not ...
func Not(x *Value) *Value {
	return NewNumber(toFloat(!toBool(x.number)))
}

// Add ..
func Add(x, y *Value) *Value {
	return NewNumber(x.number + y.number)
}

// Sub ...
func Sub(x, y *Value) *Value {
	return NewNumber(x.number - y.number)
}

// Mul ...
func Mul(x, y *Value) *Value {
	return NewNumber(x.number * y.number)
}

// Div ...
func Div(x, y *Value) *Value {
	return NewNumber(x.number / y.number)
}

// Mod ...
func Mod(x, y *Value) *Value {
	res := int(x.number) % int(y.number)
	return NewNumber(float64(res))
}

// Pow ...
func Pow(x, y *Value) *Value {
	return NewNumber(math.Pow(x.number, y.number))
}

//
func toFloat(b bool) float64 {
	if b {
		return 1.0
	}
	return 0.0
}

//
func toBool(v float64) bool {
	return v != 0.0
}

// Eq ...
func Eq(x, y *Value) *Value {
	return NewNumber(toFloat(x.number == y.number))
}

// Ne ...
func Ne(x, y *Value) *Value {
	return NewNumber(toFloat(x.number != y.number))
}

// Gt ...
func Gt(x, y *Value) *Value {
	return NewNumber(toFloat(x.number > y.number))
}

// Ge ...
func Ge(x, y *Value) *Value {
	return NewNumber(toFloat(x.number >= y.number))
}

// Lt ...
func Lt(x, y *Value) *Value {
	return NewNumber(toFloat(x.number < y.number))
}

// Le ...
func Le(x, y *Value) *Value {
	return NewNumber(toFloat(x.number <= y.number))
}

// And ...
func And(x, y *Value) *Value {
	res := toBool(x.number) && toBool(y.number)
	return NewNumber(toFloat(res))
}

// Or ...
func Or(x, y *Value) *Value {
	res := toBool(x.number) && toBool(y.number)
	return NewNumber(toFloat(res))
}

// text operations

// Conc ...
func Conc(x, y *Value) *Value {
	return NewText(x.text + y.text)
}

// TEq ...
func TEq(x, y *Value) *Value {
	return NewNumber(toFloat(x.text == y.text))
}

// TNe ...
func TNe(x, y *Value) *Value {
	return NewNumber(toFloat(x.text != y.text))
}

// TGt ...
func TGt(x, y *Value) *Value {
	return NewNumber(toFloat(x.text > y.text))
}

// TGe ...
func TGe(x, y *Value) *Value {
	return NewNumber(toFloat(x.text >= y.text))
}

// TLt ...
func TLt(x, y *Value) *Value {
	return NewNumber(toFloat(x.text < y.text))
}

// TLe ...
func TLe(x, y *Value) *Value {
	return NewNumber(toFloat(x.text <= y.text))
}
