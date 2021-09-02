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
	sl := fmt.Sprint(b.Left)
	sr := fmt.Sprint(b.Right)
	return fmt.Sprintf("(%s %s %s)", sl, b.Operation, sr)
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

var spaces = "  "

func indent() { spaces += "  " }
func unindent() {
	l := len(spaces)
	if l >= 2 {
		spaces = spaces[0 : l-2]
	}
}

func (d *Dim) String() string {
	return fmt.Sprintf("%sDIM %s[%s]", spaces, d.Name, fmt.Sprint(d.Size))
}

func (l *Let) String() string {
	sp := fmt.Sprint(l.Place)
	sv := fmt.Sprint(l.Value)
	return fmt.Sprintf("%sLET %s = %s", spaces, sp, sv)
}

func (i *Input) String() string {
	sp := fmt.Sprint(i.Place)
	return fmt.Sprintf("%sINPUT %s", spaces, sp)
}

func (p *Print) String() string {
	sv := fmt.Sprint(p.Value)
	return fmt.Sprintf("%sPRINT %s", spaces, sv)
}

func (i *If) String() string {
	res := fmt.Sprintf("%sIF %s THEN\n", spaces, fmt.Sprint(i.Condition))
	res += fmt.Sprint(i.Decision)
	if i.Alternative != nil {
		res += fmt.Sprintf("%sELSE\n", spaces)
		res += fmt.Sprint(i.Alternative)
	}
	res += fmt.Sprintf("%sEND IF", spaces)
	return res
}

func (w *While) String() string {
	sc := fmt.Sprint(w.Condition)
	sb := fmt.Sprint(w.Body)
	return fmt.Sprintf("%sWHILE %s\n%s%sEND WHILE", spaces, sc, sb, spaces)
}

func (s *Sequence) String() string {
	var res string
	indent()
	for _, e := range s.Items {
		res += fmt.Sprint(e) + "\n"
	}
	unindent()
	return res
}

func (s *Subroutine) String() string {
	sp := ""
	for i, p := range s.Parameters {
		if i != 0 {
			sp += ", "
		}
		sp += fmt.Sprint(p)
	}
	result := fmt.Sprintf("SUB %s(%s)\n", s.Name, sp)
	result += fmt.Sprint(s.Body)
	result += "END SUB"
	return result
}

func (p *Program) String() string {
	var result string
	for _, sb := range p.Members {
		result += fmt.Sprint(sb)
		result += "\n\n"
	}
	return result
}
