package nicks6

import (
	"reflect"
	"strings"
	"strconv"
)

type Element struct {
	Type reflect.Type
	Value any
}

type Context map[string]Element

type Bracket byte

type Special byte

type Null struct{}

type Fraction struct {
	Numerator int
	Denominator uint
}

func gcd(a int, b int) int {
	if b == 0 {
		return a
	}
	return gcd(b, a % b)
}

func simplify(n int, d int) (int, int) {
	g := gcd(n, d)
	if g < 0 {
		g = -g
	}
	if g == 0 {
		g = 1
	}
	return n / g, d / g
}

type String string

type List []Element

type Dictionary map[string]Element

type Closure struct {
	Ctx Context
	Variable string
	Expression Element
}

func Lex(script string) []Element {
	var tokens []Element

	var token strings.Builder
	var ctype reflect.Type

	i := 0

	endToken := func() {
		ctype = reflect.TypeOf(Null{})

		if token.Len() > 0 {
			tokens = append(tokens, Element{ctype, token.String()})
		}

		token.Reset()
	}

	for ; i < len(script); i++ {
		c := script[i]

		if c == '#' {
			i++

			escaped := false

			for ; i < len(script); i++ {
				c := script[i]

				if !escaped && c == '\\' {
					escaped = true
					continue
				}

				if !escaped && c == '#' {
					break
				}

				escaped = false
			}

			continue
		}

		if c == '"' {
			endToken()

			i++

			var str strings.Builder

			escaped := false

			for ; i < len(script); i++ {
				c := script[i]

				if !escaped && c == '\\' {
					escaped = true
					continue
				}

				if !escaped && c == '"' {
					break
				}

				if escaped {
					switch c {
						case 'n':
							str.WriteRune('\n')
						case 't':
							str.WriteRune('\t')
						default:
							str.WriteByte(c)
					}
				} else {
					str.WriteByte(c)
				}

				escaped = false
			}

			tokens = append(tokens, Element{reflect.TypeOf(String("")), String(str.String())})

			continue
		}

		switch c {
			case ' ', '\t', '\n':
				endToken()

			case '(', ')', '[', ']', '{', '}':
				endToken()
				tokens = append(tokens, Element{reflect.TypeOf(Bracket('(')), c})

			case '\\', ':', ',':
				endToken()
				tokens = append(tokens, Element{reflect.TypeOf(Special(',')), c})

			case '_':
				endToken()
				tokens = append(tokens, Element{reflect.TypeOf(Null{}), Null{}})

			case '@':
				endToken()
				tokens = append(tokens, Element{reflect.TypeOf(Fraction{}), Fraction{1, 0}})

			case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
				endToken()
				
				var ns strings.Builder
				dec := -1
				digits := 0

				for ; i < len(script); i++ {
					c := script[i]

					if c == '.' && dec < 0 {
						dec = digits
						continue
					}

					if 48 <= c && c <= 57 {
						ns.WriteByte(c)
						digits++
						continue
					}

					if c == '_' {
						continue
					}

					break
				}

				num, err := strconv.Atoi(ns.String())
				if err != nil {
					panic(err)
				}

				var denom int
				if dec < 0 {
					denom = 1
				} else {
					denom = 1
					for x := 0; x < dec - digits; x++ {
						denom = denom * 10
					}
				}

				num, denom = simplify(num, denom)

				tokens = append(tokens, Element{reflect.TypeOf(Fraction{}), Fraction{num, uint(denom)}})
		}
	}

	return tokens
}
