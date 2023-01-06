package interpreter

import (
	"fmt"
	"strings"
)

const (
	vUndefined = '?' // անորոշ
	vBoolean   = 'B' // տրամաբանական
	vNumber    = 'N' // թվային
	vText      = 'T' // տեքստային
	vArray     = 'A' // զանգված
)

// Value Ունիվերսալ արժեք
type value struct {
	kind    rune     // տեսակը
	boolean bool     // տրամաբանական արժեք
	number  float64  // թվային արժեք
	text    string   // տեքստային արժեք
	array   []*value // արժեքների զանգված
}

func (v *value) isBoolean() bool { return v.kind == vBoolean }
func (v *value) isNumber() bool  { return v.kind == vNumber }
func (v *value) isText() bool    { return v.kind == vText }
func (v *value) isArray() bool   { return v.kind == vArray }

func (v *value) String() string {
	res := "<undefined>"
	switch v.kind {
	case vBoolean:
		res = strings.ToUpper(fmt.Sprint(v.boolean))
	case vNumber:
		res = fmt.Sprint(v.number)
	case vText:
		res = v.text
	case vArray:
		res = ""
		for i, e := range v.array {
			if i != 0 {
				res += ", "
			}
			res += e.String()
		}
		res = "[" + res + "]"
	case vUndefined:
	}
	return res
}

func (v *value) clone() *value {
	cloned := &value{}
	*cloned = *v

	if cloned.kind == vArray {
		cloned.array = make([]*value, len(v.array))
		for i, e := range v.array {
			cloned.array[i] = e.clone()
		}
	}

	return cloned
}

func eq(x, y *value) bool {
	switch {
	case x.isBoolean() && y.isBoolean():
		return x.boolean == y.boolean
	case x.isNumber() && y.isNumber():
		return x.number == y.number
	case x.isText() && y.isText():
		return x.text == y.text
	}

	panic("անհամեմատելի արժեքներ")
}

func lt(x, y *value) bool {
	switch {
	case x.isNumber() && y.isNumber():
		return x.number < y.number
	case x.isText() && y.isText():
		return x.text < y.text
	}

	panic("անհամեմատելի արժեքներ")
}
