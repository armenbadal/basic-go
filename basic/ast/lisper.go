package ast

import (
	"fmt"
	"strings"
)

type Lisper interface {
	Lisp() string
}

func (b *Boolean) Lisp() string {
	return fmt.Sprintf("#S(BOOLEAN :VALUE %t)", b.Value)
}

func (n *Number) Lisp() string {
	return fmt.Sprintf("#S(NUMBER :VALUE %f)", n.Value)
}

func (t *Text) Lisp() string {
	return fmt.Sprintf("#S(TEXT :VALUE \"%s\")", t.Value)
}

func (a *Array) Lisp() string {
	els := make([]string, len(a.Elements))
	for i, e := range a.Elements {
		els[i] = e.(Lisper).Lisp()
	}
	return fmt.Sprintf("#S(ARRAY :ELEMENTS (list %s))", strings.Join(els, " "))
}

func (v *Variable) Lisp() string {
	return fmt.Sprintf("#S(VARIABLE :NAME %s", v.Name)
}

func (u *Unary) Lisp() string {
	return fmt.Sprintf("#S(UNARY :OPERATION %q :RIGHT %s)", u.Operation, u.Right.(Lisper).Lisp())
}
