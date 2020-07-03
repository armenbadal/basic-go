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

type scope struct {
	up    *scope
	items []*Symbol
}

// Table ...
type Table struct {
	sc *scope
}

// NewTable ...
func NewTable() *Table {
	symbolIndex = 0
	return &Table{sc: &scope{nil, make([]*Symbol, 0, 8)}}
}

// OpenScope ...
func (t *Table) OpenScope() {
	t.sc = &scope{t.sc, make([]*Symbol, 0, 8)}
}

// CloseScope ...
func (t *Table) CloseScope() {
	if t.sc != nil {
		t.sc = t.sc.up
	}
}

// Add ...
func (t *Table) Add(n string) {
	t.sc.items = append(t.sc.items, NewSymbol(n))
}

// Search ...
func (t *Table) Search(n string) *Symbol {
	p := t.sc
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
