package amotoen

import (
	"bytes"
)

type node struct {
	content  Stringer
	source   State
	children []Tree
}

func (n *node) Process() interface{} {
	return nil // n.source.processor(n)
}

func (n *node) Source() State {
	return n.source
}

func (n *node) Append(t Tree) Tree {
	// It's expected that when a Tree is provided to append
	// ... it is finished - it's ready to be post-processed...
	// We can access the post-processor through the state which is in the
	// ... source attribute
	n.children = append(n.children, t)
	return t
}

func (n *node) String() string {
	var result bytes.Buffer
	if n.content != nil {
		result.WriteString(n.content.String())
	}
	if len(n.children) > 0 {
		result.WriteString("[")
	}
	for i, c := range n.children {
		result.WriteString(c.String())
		if i < len(n.children)-1 {
			result.WriteString(",")
		}
	}
	if len(n.children) > 0 {
		result.WriteString("]")
	}
	return result.String()
}

type stringContent string

func (s stringContent) String() string { return string(s) }

type runeContent rune

func (r runeContent) String() string { return "'" + string(r) + "'" }
