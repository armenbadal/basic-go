package interpreter

import "math"

type binary func(l, r *value) *value

var operations = map[string]binary{
	// թվային գործողություններ
	"+": func(r, l *value) *value {
		return &value{kind: vNumber, number: l.number + r.number}
	},
	"-": func(r, l *value) *value {
		return &value{kind: vNumber, number: l.number - r.number}
	},
	"*": func(r, l *value) *value {
		return &value{kind: vNumber, number: l.number * r.number}
	},
	"/": func(r, l *value) *value {
		return &value{kind: vNumber, number: l.number / r.number}
	},
	"\\": func(r, l *value) *value {
		return &value{kind: vNumber, number: float64(int(l.number) % int(r.number))}
	},
	"^": func(r, l *value) *value {
		return &value{kind: vNumber, number: math.Pow(l.number, r.number)}
	},
	// համեմատումներ
	"=": func(r, l *value) *value {
		return &value{kind: vBoolean}
	},
}
