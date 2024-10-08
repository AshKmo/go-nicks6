package nicks6

import (
	"testing"
	"fmt"
)

func TestFull(t *testing.T) {
	script := ""

	tokens := Lex(script)
	fmt.Println(Pretty(List(tokens)))

	parsed := Parse(tokens)
	fmt.Println(Pretty(parsed))

	res := Evaluate(parsed)
	fmt.Println(Pretty(res))
}
