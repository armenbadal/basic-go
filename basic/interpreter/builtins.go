package interpreter

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
		return nil
	},
	"NUM": func(args ...*value) *value {
		return nil
	},
}
