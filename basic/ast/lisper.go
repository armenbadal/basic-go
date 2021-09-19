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
	rs := u.Right.(Lisper).Lisp()
	return fmt.Sprintf("#S(UNARY :OPERATION %q :RIGHT %s)", u.Operation, rs)
}

func (b *Binary) Lisp() string {
	ls := b.Left.(Lisper).Lisp()
	rs := b.Right.(Lisper).Lisp()
	return fmt.Sprintf("#S(BINARY :OPERATION %q :LEFT %s :RIGHT %s)", b.Operation, ls, rs)
}

func (b *Apply) Lisp() string {
	asr := fmt.Sprintf("#S(APPLY :CALLEE %q :ARGUMENTS (list ", b.Callee)
	for i, e := range b.Arguments {
		if i != 0 {
			asr += ", "
		}
		asr += e.(Lisper).Lisp()
	}
	asr += "))"
	return asr
}
