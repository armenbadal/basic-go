package interpreter

var builtins = map[string]func(args ...*value) *value{
	"LEN": func(args ...*value) *value {
		return &value{kind: vNumber, number: float64(len(args[0].text))}
	},
}
