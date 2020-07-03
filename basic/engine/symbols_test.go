package engine

import "testing"

func TestCreateScope(t *testing.T) {
	s := NewTable()
	s.Add("a")
	s.Add("b")
	s.Add("c")
	s.Add("e")

	s.OpenScope()
	s.Add("d")
	s.Add("a")

	s.OpenScope()
	s.Add("b")
	s.Add("c")

	v0 := s.Search("c") // 8
	t.Logf("> %s: %d", v0.name, v0.eid)

	v0 = s.Search("d") // 5
	t.Logf("> %s: %d", v0.name, v0.eid)

	v0 = s.Search("e") // 4
	t.Logf("> %s: %d", v0.name, v0.eid)

	//
	p := s.sc
	for p != nil {
		for _, e := range p.items {
			t.Logf("| %s: %d", e.name, e.eid)
		}
		p = p.up
	}

	t.Fail()
}
