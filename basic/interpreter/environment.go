package interpreter

type scope struct {
	items map[string]*value // փոփոխականների ընթացիկ արժեքներ
	up    *scope            // ընդգրկող scope-ի ցուցիչ
}

// Environment Կատարման միջավայրը
type environment struct {
	current *scope
}

// OpenScope ...
func (e *environment) openScope() {
	e.current = &scope{
		items: make(map[string]*value),
		up:    e.current,
	}
}

// CloseScope ...
func (e *environment) closeScope() {
	if e.current != nil {
		e.current = e.current.up
	}
}

// set ֆունկցիան միջավայրում ավելացնում է տրված անվան տրված արժեքը
func (e *environment) set(name string, value *value) {
	if e.current != nil {
		e.current.items[name] = value
	}
}

// get ֆունկցիան միջավայրում որոնում է և վերադարձնում է տրված անվանը
// համապատասխանող արժեքը։
func (e *environment) get(name string) *value {
	for p := e.current; p != nil; p = p.up {
		if v, exists := p.items[name]; exists {
			return v
		}
	}

	return nil
}
