package interpreter

import "math"

type binary func(l, r *value) *value

var operations = map[string]binary{
	// թվային գործողություններ
	"+": func(l, r *value) *value {
		return &value{kind: vNumber, number: l.number + r.number}
	},
	"-": func(l, r *value) *value {
		return &value{kind: vNumber, number: l.number - r.number}
	},
	"*": func(l, r *value) *value {
		return &value{kind: vNumber, number: l.number * r.number}
	},
	"/": func(l, r *value) *value {
		return &value{kind: vNumber, number: l.number / r.number}
	},
	"\\": func(l, r *value) *value {
		return &value{kind: vNumber, number: float64(int(l.number / r.number))}
	},
	"^": func(l, r *value) *value {
		return &value{kind: vNumber, number: math.Pow(l.number, r.number)}
	},
	// համեմատումներ
	"=": func(l, r *value) *value {
		return &value{kind: vBoolean, boolean: eq(l, r)}
	},
	"<>": func(l, r *value) *value {
		return &value{kind: vBoolean, boolean: !eq(l, r)}
	},
	"<": func(l, r *value) *value {
		return &value{kind: vBoolean, boolean: lt(l, r)}
	},
	"<=": func(l, r *value) *value {
		return &value{kind: vBoolean, boolean: lt(l, r) || eq(l, r)}
	},
	">": func(l, r *value) *value {
		return &value{kind: vBoolean, boolean: !lt(l, r) && !eq(l, r)}
	},
	">=": func(l, r *value) *value {
		return &value{kind: vBoolean, boolean: !lt(l, r)}
	},
	// տրամաբանական գործողություններ
	"AND": func(l, r *value) *value {
		return &value{kind: vBoolean, boolean: l.boolean && r.boolean}
	},
	"OR": func(l, r *value) *value {
		return &value{kind: vBoolean, boolean: l.boolean || r.boolean}
	},
}
