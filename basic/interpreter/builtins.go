package interpreter

func checkArguments(expected []rune, actual ...*value) {

	panic("արգումենտների անհամապատասխանություն")
}

var builtins = map[string]func(args ...*value) *value{
	"LEN": func(args ...*value) *value {
		exp := []rune{vText}
		checkArguments(exp, args...)
		return &value{kind: vNumber, number: float64(len(args[0].text))}
	},
	"STR": func(args ...*value) *value {
		return nil
	},
	"NUM": func(args ...*value) *value {
		return nil
	},
}
