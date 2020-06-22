package engine

// Symbol (key, value) զույգ
type Symbol struct {
	name string
	eid  int
}

var symbolIndex int = 0

// NewSymbol ...
func NewSymbol(n string) *Symbol {
	symbolIndex++
	return &Symbol{name: n, eid: symbolIndex}
}

// Scope ...
type Scope struct {
	up    *Scope
	items []*Symbol
}

// NewScope ...
func NewScope() *Scope {
	symbolIndex = 0
	return Extend(nil)
}

// Extend ...
func Extend(s *Scope) *Scope {
	return &Scope{s, make([]*Symbol, 0, 8)}
}

// Add ...
func (s *Scope) Add(n string) {
	s.items = append(s.items, NewSymbol(n))
}

// Search ...
func (s *Scope) Search(n string) *Symbol {
	p := s
	for p != nil {
		for _, e := range p.items {
			if e.name == n {
				return e
			}
		}
		p = p.up
	}
	return nil
}
