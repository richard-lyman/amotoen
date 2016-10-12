package main

import (
	"bufio"
	"bytes"
	"log"
	"strings"
)

type ast []astNode

type astNodeType int

type astNode struct {
	t astNodeType
	s string
}

type context struct {
	r rune
	b bytes.Buffer
	a ast
}

type stateStep func(*context) (stateStep, bool)

func l(s string, c *context) { log.Println(s, string(c.r)) }

func nilStep(c *context) (stateStep, bool) { return nilStep, false }

func gatherAndReturn(c *context, returnState stateStep, b bool) (stateStep, bool) {
	c.b.WriteRune(c.r)
	return returnState, b
}

type wrapper func(string) astNode

func addAndReturn(moreThanZeroRequired bool, w wrapper, returnState stateStep) stateStep {
	return func(c *context) (stateStep, bool) {
		s := c.b.String()
		if moreThanZeroRequired && len(s) > 0 { // This should be different than below...
			c.a = append(c.a, w(s))
			c.b.Reset()
		} else if len(s) == 0 {
                        log.Println("len == 0")
			c.a = append(c.a, w(s))
			c.b.Reset()
		}
		return returnState, false
	}
}

const (
	word astNodeType = iota
	paren
)

func toWord(s string) astNode {
	return astNode{t: word, s: s}
}

func toParen(s string) astNode {
        log.Println("toParen called...")
	return astNode{t: paren}
}

func consume(c *context) (stateStep, bool) {
	switch c.r {
	case ' ':
		return addAndReturn(true, toWord, consume), true
	case '(':
		return parens, true
	default:
		return gatherAndReturn(c, consume, true)
	}
}

func parens(c *context) (stateStep, bool) {
	if c.r != ')' {
		return gatherAndReturn(c, parens, true)
	}
        log.Println("marking as paren...")
	return addAndReturn(false, toParen, consume), true
}

func main() {
	in := "this is a (asd) string"
	s := bufio.NewScanner(strings.NewReader(in))
	s.Split(bufio.ScanRunes)
	c := &context{}
	next := true
	depth := 0
	previousCR := ' '
	for state := consume; state != nil; {
		if previousCR == c.r {
			depth = depth + 1
		} else {
			depth = depth - 1
		}
		if depth > 100 {
			break
		}
		if next {
			if !s.Scan() {
				break
			}
			c.r = bytes.Runes(s.Bytes())[0]
		}
		state, next = state(c)
		previousCR = c.r
	}
	addAndReturn(true, toWord, nilStep)(c)
	log.Println(in)
	log.Println(c.a)
	log.Printf("%+v\n", c.a)
}
