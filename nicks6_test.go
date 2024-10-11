package nicks6

import (
	"testing"
	"fmt"
)

func TestFull(t *testing.T) {
	script := ""

	tokens := Lex(script)
	fmt.Println(Pretty(List(tokens), 0))

	parsed, _ := Parse(tokens, 0)
	fmt.Println(Pretty(parsed, 0))

	res := Evaluate(parsed, Dict{})
	fmt.Println(Pretty(res, 0))
}
