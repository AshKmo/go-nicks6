package nicks6

import (
	"strings"
	"strconv"
	"fmt"
)

type Unit interface {
}

type Null struct{}

type Fraction struct {
	Numerator int
	Denominator uint
}

type Operator string

type Bracket byte

type Special byte

type Symbol string

type String string

type List []Unit

type Dictionary map[string]Unit

type Function func(Unit) Unit

func gcd(a uint, b uint) uint {
	if b == 0 {
		return a
	}
	return gcd(b, a % b)
}

func simplify(n uint, d uint) (uint, uint) {
	g := gcd(n, d)
	if (g < 0) {
		g = -g
	}
	if (g == 0) {
		g = 1
	}

	return n / g, d / g
}

func abs(x int) uint {
	if x < 0 {
		return uint(-x)
	}
	return uint(x)
}

func sign(x int) int {
	if x < 0 {
		return -1
	}
	return 1
}

func mul(a Fraction, b Fraction) Fraction {
	sign := sign(a.Numerator) * sign(b.Numerator)
	an, ad := simplify(abs(a.Numerator), b.Denominator)
	bn, bd := simplify(abs(b.Numerator), a.Denominator)

	return Fraction{int(an) * int(bn) * sign, ad * bd}
}

func Lex(script string) []Unit {
	tokens := []Unit{}
	var token strings.Builder
	
	isOperator := false

	endToken := func() {
		if token.Len() > 0 {
			if isOperator {
				tokens = append(tokens, Operator(token.String()))
			} else {
				tokens = append(tokens, Symbol(token.String()))
			}
		}

		token.Reset()
	}

	// go through each character and handle it
	for i := 0; i < len(script); i++ {
		c := script[i]

		// comment skipper
		if c == '#' {
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

		// string maker
		if c == '"' {
			endToken()

			escaped := false

			i++

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
					case 't':
						token.WriteByte('\t')
					case 'n':
						token.WriteByte('\n')
					default:
						token.WriteByte(c)
					}
				} else {
					token.WriteByte(c)
				}

				escaped = false
			}

			tokens = append(tokens, String(token.String()))

			token.Reset()

			continue
		}

		switch c {
		case '\n', '\t', ' ':
			endToken()

		case '(', ')', '[', ']', '{', '}':
			endToken()
			tokens = append(tokens, Bracket(c))

		case ':', ',', '\\':
			endToken()
			tokens = append(tokens, Special(c))

		case '_':
			endToken()
			tokens = append(tokens, Null{})

		case '@':
			endToken()
			tokens = append(tokens, Fraction{1, 0})

		// number parser
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			endToken()

			digits := 0
			dec := 0

			L:
			for ; i < len(script); i++ {
				c := script[i]

				switch c {
				case '.':
					dec = digits
				case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
					digits++
					token.WriteByte(c)
				case '_':
				default:
					break L
				}
			}

			numString := token.String()

			token.Reset()

			var num uint = 0
			var m uint = 1
			for i := len(numString) - 1; i >= 0; i-- {
				num += m * (uint(numString[i]) - 48)
				m *= 10
			}

			var denom uint = 1
			if dec != 0 {
				for i := 0; i < digits - dec; i++ {
					denom *= 10
				}
			}

			num, denom = simplify(num, denom)

			tokens = append(tokens, Fraction{int(num), denom})

		case '+', '-', '*', '/', '<', '>', '=', '.', '~', '&', '|', '^':
			if !isOperator {
				endToken()
				isOperator = true
			}

			token.WriteByte(c)

		default:
			if isOperator {
				endToken()
				isOperator = false
			}

			token.WriteByte(c)
		}
	}

	endToken()

	return tokens
}

func Parse(tokens []Unit) Unit {
	// TODO
	return nil
}

func Evaluate(exp Unit) Unit {
	// TODO
	return nil
}

func Interpret(s string) Unit {
	return Evaluate(Parse(Lex(s)))
}

func Pretty(u Unit) string {
	switch v := u.(type) {
	case Null:
		return "_"
	case Symbol:
		return string(v)
	case Operator:
		return string(v)
	case Bracket:
		return string(v)
	case String:
		r := string(v)
		olds := []string{"\\", "\"", "\n", "\r", "\t"}
		news := []string{"\\\\", "\\\"", "\\n", "\\r", "\\t"}
		for i := 0; i < len(olds); i++ {
			r = strings.Replace(r, olds[i], news[i], -1)
		}
		return "\"" + r + "\""
	case Fraction:
		if v.Denominator == 1 {
			return strconv.Itoa(v.Numerator)
		}
		return fmt.Sprintf("(%d / %d)", v.Numerator, v.Denominator)
	case List:
		r := "["
		for i, e := range v {
			r += Pretty(e)

			if i + 1 < len(v) {
				r += ", "
			}
		}

		return r + "]"
	}

	return "<UNKNOWN>"
}
