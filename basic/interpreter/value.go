package interpreter

import "fmt"

// Պարզ տիպեր
const (
	vUndefined = '?'
	vBoolean   = 'B'
	vNumber    = 'N'
	vText      = 'T'
	vArray     = 'A'
)

// Value Ունիվերսալ արժեք
type value struct {
	kind    rune
	boolean bool
	number  float64
	text    string
	array   []*value
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
