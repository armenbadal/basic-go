package ast

import (
	"fmt"
	"strings"
)

func (b *Boolean) String() string {
	return strings.ToUpper(fmt.Sprintf("%t", b.Value))
}

func (n *Number) String() string {
	return fmt.Sprintf("%f", n.Value)
}

func (t *Text) String() string {
	return "\"" + t.Value + "\""
}

func (a *Array) String() string {
	els := "["
	for i, e := range a.Elements {
		if i != 0 {
			els += ", "
		}
		els += fmt.Sprint(e)
	}
	els += "]"
	return els
}

func (v *Variable) String() string {
	return v.Name
}

func (u *Unary) String() string {
	return fmt.Sprintf("%s %s", u.Operation, fmt.Sprint(u.Right))
}

func (b *Binary) String() string {
	return fmt.Sprintf("(%s %s %s)", fmt.Sprint(b.Left), b.Operation, fmt.Sprint(b.Right))
}

func (a *Apply) String() string {
	var args string
	for i, e := range a.Arguments {
		if i != 0 {
			args += ", "
		}
		args += fmt.Sprint(e)
	}
	return fmt.Sprintf("%s(%s)", a.Callee, args)
}
