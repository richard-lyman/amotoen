package amotoen

type oState struct {
	label       string
	inner       State
	postHandler PostHandler
}

func (s *oState) PostHandle(t Tree) PostResult {
	return s.postHandler(t)
}

func (s *oState) Post(f PostHandler) State {
	s.postHandler = f
	return s
}

func (s *oState) Label(label string) State {
	s.label = label
	return s
}

func (s *oState) String() string {
	if len(s.label) == 0 {
		return "One or more <no label>"
	} else {
		return "One or more of: " + s.label
	}
}

func (s *oState) Handle(input Input) (result Tree, err error) {
	input.Debug(s)
	result = &node{stringContent(s.label), s, nil}
	previous := input.Cursor()
	tmp, err := s.inner.Handle(input)
	if err != nil {
		input.Revert(previous)
		return nil, err
	}
	result.Append(tmp)
	for {
		previous := input.Cursor()
		tmp, err := s.inner.Handle(input)
		if err != nil {
			input.Revert(previous)
			break
		}
		result.Append(tmp)
	}
	return result, nil
}

type zState struct {
	label       string
	inner       State
	postHandler PostHandler
}

func (s *zState) PostHandle(t Tree) PostResult {
	return s.postHandler(t)
}

func (s *zState) Post(f PostHandler) State {
	s.postHandler = f
	return s
}

func (s *zState) Label(label string) State {
	s.label = label
	return s
}

func (s *zState) String() string {
	if len(s.label) == 0 {
		return "Zero or more <no label>"
	} else {
		return "Zero or more of: " + s.label
	}
}

func (s *zState) Handle(input Input) (result Tree, err error) {
	input.Debug(s)
	result = &node{stringContent(s.label), s, nil}
	for {
		previous := input.Cursor()
		tmp, err := s.inner.Handle(input)
		if err != nil {
			input.Revert(previous)
			break
		}
		result.Append(tmp)
	}
	return result, nil
}

type sState struct {
	label       string
	inner       []State
	postHandler PostHandler
}

func (s *sState) PostHandle(t Tree) PostResult {
	return s.postHandler(t)
}

func (s *sState) Post(f PostHandler) State {
	s.postHandler = f
	return s
}

func (s *sState) Label(label string) State {
	s.label = label
	return s
}

func (s *sState) String() string {
	if len(s.label) == 0 {
		return "Sequence <no label>"
	} else {
		return "Sequence of: " + s.label
	}
}

func (s *sState) Handle(input Input) (Tree, error) {
	input.Debug(s)
	previous := input.Cursor()
	result := &node{stringContent(s.label), s, nil}
	for _, child := range s.inner {
		tmp, err := child.Handle(input)
		if err != nil {
			input.Revert(previous)
			return nil, err
		}
		result.Append(tmp)
	}
	return result, nil
}

type nState struct {
	label       string
	inner       State
	postHandler PostHandler
}

func (s *nState) PostHandle(t Tree) PostResult {
	return s.postHandler(t)
}

func (s *nState) Post(f PostHandler) State {
	s.postHandler = f
	return s
}

func (s *nState) Label(label string) State {
	s.label = label
	return s
}

func (s *nState) String() string {
	if len(s.label) == 0 {
		return "Not <no label>"
	} else {
		return "Not of: " + s.label
	}
}

func (s *nState) Handle(input Input) (Tree, error) {
	input.Debug(s)
	previous := input.Cursor()
	_, err := s.inner.Handle(input)
	if err != nil {
		tmp, err := input.Peek()
		if err != nil {
			input.Revert(previous)
			return nil, newError(s, input, "nState failed to get rune", err)
		}
		r := tmp.(rune)
		input.Consume(1)
		return &node{runeContent(r), s, nil}, nil
	}
	input.Revert(previous)
	return nil, newError(s, input, "nState failed", nil)
}

type eState struct {
	label       string
	inner       []State
	postHandler PostHandler
}

func (s *eState) PostHandle(t Tree) PostResult {
	return s.postHandler(t)
}

func (s *eState) Post(f PostHandler) State {
	s.postHandler = f
	return s
}

func (s *eState) Label(label string) State {
	s.label = label
	return s
}

func (s *eState) String() string {
	if len(s.label) == 0 {
		return "Either <no label>"
	} else {
		return "Either of: " + s.label
	}
}

func (s *eState) Handle(input Input) (Tree, error) {
	input.Debug(s)
	previous := input.Cursor()
	for _, child := range s.inner {
		tmp, err := child.Handle(input)
		if err != nil {
			input.Revert(previous)
			if Debug {
				DebugOut.Write([]byte(s.String() + " failed on: " + child.String() + " with: " + err.Error() + "\n"))
			}
			continue
		}
		return tmp, nil
	}
	input.Revert(previous)
	return nil, newError(s, input, "eState failed", nil)
}
