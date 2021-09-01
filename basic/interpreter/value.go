package interpreter

import "fmt"

const (
	vUndefined = '?' // անորոշ
	vBoolean   = 'B' // տեամաբանական
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

func (v *value) toString() string {
	res := "<undefined>"
	switch v.kind {
	case vBoolean:
		res = fmt.Sprint(v.boolean)
	case vNumber:
		res = fmt.Sprint(v.number)
	case vText:
		res = v.text
	case vArray:
		// TODO create string view of array
		res = "[]"
	case vUndefined:
	}
	return res
}

func (v *value) clone() *value {
	return nil
}
