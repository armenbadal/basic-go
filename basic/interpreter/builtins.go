package interpreter

import (
	"fmt"
	"math"
	"strconv"
)

var builtins = map[string]func(args ...*value) *value{
	"LEN": func(args ...*value) *value {
		ag := args[0]
		if ag.isArray() {
			return &value{kind: vNumber, number: float64(len(ag.array))}
		}

		if ag.isText() {
			return &value{kind: vNumber, number: float64(len(args[0].text))}
		}

		return &value{}
	},
	"STR": func(args ...*value) *value {
		ag := args[0]
		if ag.isNumber() {
			return &value{kind: vText, text: fmt.Sprintf("%f", ag.number)}
		}

		return &value{}
	},
	"NUM": func(args ...*value) *value {
		ag := args[0]
		if ag.isText() {
			nv, _ := strconv.ParseFloat(ag.text, 64)
			return &value{kind: vNumber, number: nv}
		}

		return &value{}
	},
	"SQR": func(args ...*value) *value {
		ag := args[0]
		if ag.isNumber() {
			return &value{kind: vNumber, number: math.Sqrt(ag.number)}
		}

		return &value{}
	},
	"MID": func(args ...*value) *value {
		if len(args) != 3 {
			return &value{}
		}

		s := args[0]
		b := args[1]
		c := args[2]
		if s.isText() && b.isNumber() && c.isNumber() {
			i0 := int(b.number)
			i1 := int(b.number + c.number)
			rs := s.text[i0:i1]
			return &value{kind: vText, text: rs}
		}

		return &value{}
	},
}
