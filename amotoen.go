package amotoen

import (
	"strconv"
)

type Writer interface {
	Write([]byte) (int, error)
}

var Debug = false
var DebugOut Writer

func Manual(root State, input Input, mustConsumeAll bool) Tree {
	result, err := root.Handle(input)
	if err != nil {
		panic(err)
	}
	if mustConsumeAll && !input.AtEOI() {
		panic("Failed to consume all input. Ended at: " + input.LinedCursor() + ", Tree: " + result.String())
	}
	return result
}

type State interface {
	Handle(Input) (Tree, error)
	Stringer
}

type Input interface {
	Peek() (interface{}, error)
	Cursor() int64
	LinedCursor() string
	Consume(n int64)
	Revert(n int64)
	AtEOI() bool
	Debug(State)
}

type Tree interface {
	Append(Tree) Tree
	Stringer
}

type DeferredState struct{ inner State }

func (s *DeferredState) String() string     { return s.inner.String() }
func (s *DeferredState) Set(newState State) { s.inner = newState }
func (s *DeferredState) Handle(input Input) (Tree, error) {
	t, err := s.inner.Handle(input)
	return t, err
}

func c(args ...State) {
	for _, a := range args {
		if a == nil {
			panic("You can't have a nil state - consider using a DeferredState wrapper.")
		}
	}
}

func N(arg State) State     { c(arg); return &nState{"", arg} }
func E(args ...State) State { c(args...); return &eState{"", args} }
func O(arg State) State     { c(arg); return &oState{"", arg} }
func S(args ...State) State { c(args...); return &sState{"", args} }
func Z(arg State) State     { c(arg); return &zState{"", arg} }

func NL(label string, arg State) State     { c(arg); return &nState{label, arg} }
func EL(label string, args ...State) State { c(args...); return &eState{label, args} }
func OL(label string, arg State) State     { c(arg); return &oState{label, arg} }
func SL(label string, args ...State) State { c(args...); return &sState{label, args} }
func ZL(label string, arg State) State     { c(arg); return &zState{label, arg} }

type Stringer interface {
	String() string
}

type internalError struct {
	state   State
	cursor  int64
	message string
	err     error
}

func (e internalError) Error() string {
	if e.err != nil {
		return e.state.String() + "@" + strconv.FormatInt(e.cursor, 10) + " " + e.message + ": " + e.err.Error()
	} else {
		return e.state.String() + "@" + strconv.FormatInt(e.cursor, 10) + " " + e.message + ": <nil error>"
	}
}

func newError(s State, input Input, message string, err error) error {
	return internalError{s, input.Cursor(), message, err}
}
