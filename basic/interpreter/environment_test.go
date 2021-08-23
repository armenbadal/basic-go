package interpreter

import "testing"

func TestNewEnvironment(t *testing.T) {
	env := &environment{}
	if env.current != nil {
		t.Error("New created environment is not empty")
	}
}

func TestOpenScope(t *testing.T) {
	env := &environment{}
	env.openScope()
	if env.current == nil {
		t.Error("New scope not created")
	}
}

func TestSetValue(t *testing.T) {
	env := &environment{}
	env.openScope()
	env.set("x", &value{})
	env.set("y", &value{})
	if len(env.current.items) != 2 {
		t.Error("Values no added to the environment")
	}
}
