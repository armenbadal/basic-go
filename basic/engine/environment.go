package engine

type scope struct {
	items map[string]*Value
	up    *scope
}

// Environment Կատարման միջավայրը
type Environment struct {
	current *scope
}

// NewEnvironment ֆունկցիան ստեղծում է նոր կատարման միջավայր
func NewEnvironment() *Environment {
	return &Environment{}
}

// OpenScope ...
func (e *Environment) OpenScope() {
	e.current = &scope{items: make(map[string]*Value), up: e.current}
}

// CloseScope ...
func (e *Environment) CloseScope() {
	if e.current != nil {
		e.current = e.current.up
	}
}

// Put ֆունկցիան միջավայրում ավելացնում է տրված անվան տրված արժեքը
func (e *Environment) Put(name string, value *Value) {
	e.current.items[name] = value
}

// Get ֆունկցիան միջավայրում որոնում է և վերադարձնում է տրված անվանը
// համապատասխանող արժեքը։
func (e *Environment) Get(name string) *Value {
	p := e.current
	for p != nil {
		for k, v := range p.items {
			if k == name {
				return v
			}
		}
		p = p.up
	}

	return nil
}
