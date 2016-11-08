package amotoen

import (
	"io"
	"strconv"
	"strings"
)

func ES(arg string) State { return &esState{inner: arg} }
func R(arg rune) State    { return &rState{inner: arg} }

func StringInput(s string) Input {
	return &stringInput{strings.NewReader(s)}
}

type stringInput struct {
	inner *strings.Reader
}

func (i *stringInput) Peek() (interface{}, error) {
	result, n, err := i.inner.ReadRune()
	if err != nil {
		return 0, err
	}
	i.Consume(int64(-1 * n))
	return result, err
}

func (i *stringInput) Cursor() int64 {
	result, err := i.inner.Seek(0, io.SeekCurrent)
	if err != nil {
		panic("Unable to get cursor in string input: " + err.Error())
	}
	return result
}

func (i *stringInput) LinedCursor() string {
	last := i.Cursor()
	_, err := i.inner.Seek(0, io.SeekStart)
	if err != nil {
		panic("Failed to reset seek at start of lined cursor:" + err.Error())
	}
	numberOfLinesSeen := 1
	lastNewlinePosition := int64(0)
	for {
		currentPosition, err := i.inner.Seek(0, io.SeekCurrent)
		if err != nil {
			panic("Failed to get cursor in LinedCursor:" + err.Error())
		}
		r, _, err := i.inner.ReadRune()
		if r == '\n' {
			numberOfLinesSeen++
			lastNewlinePosition = currentPosition
		}
		if currentPosition >= last {
			break
		}
	}
	i.Revert(last)
	return strconv.Itoa(numberOfLinesSeen) + ":" + strconv.FormatInt(last-lastNewlinePosition, 10)
}

func (i *stringInput) Consume(n int64) {
	_, err := i.inner.Seek(n, io.SeekCurrent)
	if err != nil {
		panic("Unable to consume in string input: " + err.Error())
	}
}

func (i *stringInput) Revert(n int64) {
	_, err := i.inner.Seek(n, io.SeekStart)
	if err != nil {
		panic("Unable to revert in string input: " + err.Error())
	}
}

func (i *stringInput) AtEOI() bool {
	return i.inner.Len() == 0
}

func (i *stringInput) Debug(s State) {
	if Debug {
		if len(s.String()) > 0 {
			tmp, _ := i.Peek()
			r := tmp.(rune)
			DebugOut.Write([]byte("In " + s.String() + "@" + strconv.FormatInt(i.Cursor(), 10) + " - '" + string(r) + "'\n"))
		}
	}
}

type esState struct {
	label       string
	inner       string
	postHandler PostHandler
}

func (s *esState) Post(f PostHandler) State {
	s.postHandler = f
	return s
}

func (s *esState) Label(label string) State {
	s.label = label
	return s
}

func (s *esState) String() string {
	if len(s.label) == 0 {
		return "EitherString <no label>"
	} else {
		return "EitherString of: " + s.label
	}
}

func (s *esState) Handle(input Input) (Tree, error) {
	input.Debug(s)
	previous := input.Cursor()
	tmp, err := input.Peek()
	if err != nil {
		input.Revert(previous)
		return nil, newError(s, input, "esState failed to get rune", err)
	}
	r := tmp.(rune)
	if strings.ContainsRune(s.inner, r) {
		input.Consume(1)
		return &node{runeContent(r), s, nil}, nil
	} else {
		input.Revert(previous)
		return nil, newError(s, input, "esState failed", nil)
	}
}

type rState struct {
	label       string
	inner       rune
	postHandler PostHandler
}

func (s *rState) Post(f PostHandler) State {
	s.postHandler = f
	return s
}

func (s *rState) Label(label string) State {
	s.label = label
	return s
}

func (s *rState) String() string {
	if len(s.label) == 0 {
		return "Rune <no label>"
	} else {
		return "Rune of: " + s.label
	}
}

func (s *rState) Handle(input Input) (Tree, error) {
	input.Debug(s)
	previous := input.Cursor()
	tmp, err := input.Peek()
	if err != nil {
		input.Revert(previous)
		return nil, newError(s, input, "rState failed to get rune", err)
	}
	r := tmp.(rune)
	if s.inner == r {
		input.Consume(1)
		return &node{runeContent(r), s, nil}, nil
	} else {
		input.Revert(previous)
		return nil, newError(s, input, "rState failed", nil)
	}
}
