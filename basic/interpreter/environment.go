package interpreter

type scope struct {
	items map[string]*value
	up    *scope
}

// Environment Կատարման միջավայրը
type environment struct {
	current *scope
}

// OpenScope ...
func (e *environment) openScope() {
	e.current = &scope{items: make(map[string]*value), up: e.current}
}

// CloseScope ...
func (e *environment) closeScope() {
	if e.current != nil {
		e.current = e.current.up
	}
}

// Set ֆունկցիան միջավայրում ավելացնում է տրված անվան տրված արժեքը
func (e *environment) set(name string, value *value) {
	e.current.items[name] = value
}

// Get ֆունկցիան միջավայրում որոնում է և վերադարձնում է տրված անվանը
// համապատասխանող արժեքը։
func (e *environment) get(name string) *value {
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
