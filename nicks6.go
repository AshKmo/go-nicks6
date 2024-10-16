package nicks6

import (
	"strings"
	"fmt"
	"slices"
	"reflect"
)

type Unit interface{}

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

type Protolist []Unit

type List []Unit

type Association struct {
	Key Unit
	Value Unit
}

type Protodict []Association

type Dict map[string]Unit

type Expression struct {
	Result Unit
}

type Protofunction struct {
	Variable Symbol
	Result Unit
}

type Function func(Unit) Unit

type Operation struct {
	Name Operator
	A Unit
	B Unit
}

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

func fsign(x Fraction) int {
	if x.Numerator < 0 {
		return -1
	}
	if x.Numerator == 0 {
		return 0
	}
	return 1
}

func merge(a Dict, b Dict) Dict {
	newDict := Dict{}

	for k, v := range a {
		newDict[k] = v
	}

	for k, v := range b {
		newDict[k] = v
	}

	return newDict
}

func mul(a Fraction, b Fraction) Fraction {
	sign := sign(a.Numerator) * sign(b.Numerator)
	an, ad := simplify(abs(a.Numerator), b.Denominator)
	bn, bd := simplify(abs(b.Numerator), a.Denominator)

	return Fraction{int(an) * int(bn) * sign, ad * bd}
}

func div(a Fraction, b Fraction) Fraction {
	sign := sign(a.Numerator) * sign(b.Numerator)
	an, ad := simplify(abs(a.Numerator), abs(b.Numerator))
	bn, bd := simplify(b.Denominator, a.Denominator)

	return Fraction{int(an) * int(bn) * sign, ad * bd}
}

func add(a Fraction, b Fraction) Fraction {
	n := a.Numerator * int(b.Denominator) + b.Numerator * int(a.Denominator)
	d := a.Denominator * b.Denominator
	s := sign(n)

	nn, nd := simplify(abs(n), d)

	return Fraction{int(nn) * s, nd}
}

func sub(a Fraction, b Fraction) Fraction {
	n := a.Numerator * int(b.Denominator) - b.Numerator * int(a.Denominator)
	d := a.Denominator * b.Denominator
	s := sign(n)

	nn, nd := simplify(abs(n), d)

	return Fraction{int(nn) * s, nd}
}

func wholes(n int, d uint) int {
	s := sign(n)
	if n < 0 {
		n = -n
	}
	c := 0
	for {
		if n < int(d) {
			return s * c
		}
		n -= int(d)
		c++
	}
}

func whole(x Fraction) int {
	return wholes(x.Numerator, x.Denominator)
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

		// number cruncher
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '-':
			endToken()

			sign := 1

			if c == '-' {
				sign = -1
				i++
			}

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
					i--
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

			tokens = append(tokens, Fraction{int(num) * sign, denom})

		case '+', '*', '/', '<', '>', '=', '.', '~', '&', '|', '^', '%':
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

func parseList(tokens []Unit, i int) (Protolist, int) {
	lastI := i

	pl := Protolist{}

	for {
		var res Expression
		res, i = Parse(tokens, i)

		t := tokens[i]

		_, tIsBracket := t.(Bracket)

		if tIsBracket && i == lastI {
			break
		}

		pl = append(pl, res)

		if tIsBracket {
			break
		}

		i++
		lastI = i
	}

	return pl, i
}

func parseDict(tokens []Unit, i int) (Protodict, int) {
	pd := Protodict{}

	for {
		var key Expression
		key, i = Parse(tokens, i)

		_, keyIsNull := key.Result.(Null)

		t := tokens[i]

		v, tIsSpecial := t.(Special)
		_, tIsBracket := t.(Bracket)

		if !keyIsNull {
			var val Expression
			if tIsSpecial && v == ',' || tIsBracket {
				val = key
			} else {
				i++
				val, i = Parse(tokens, i)
			}

			pd = append(pd, Association{key.Result, val.Result})
		}

		t = tokens[i]

		_, tIsBracket = t.(Bracket)

		if tIsBracket {
			break
		}

		i++
	}

	return pd, i
}

func Parse(tokens []Unit, i int) (Expression, int) {
	bracketed := []Unit{}

	bracketLoop:
	for ; i < len(tokens); i++ {
		var toAdd Unit

		switch v := tokens[i].(type) {
		case Special:
			if v == '\\' {
				i++

				var variable Symbol

				switch v := tokens[i].(type) {
				case Symbol:
					variable = v
				default:
					variable = ""
				}

				var expression Expression
				expression, i = Parse(tokens, i + 1)

				bracketed = append(bracketed, Protofunction{variable, expression.Result})
			}

			break bracketLoop
		case Bracket:
			switch v {
			case '(':
				toAdd, i = Parse(tokens, i + 1)
			case '[':
				toAdd, i = parseList(tokens, i + 1)
			case '{':
				toAdd, i = parseDict(tokens, i + 1)
			default:
				break bracketLoop
			}
		default:
			toAdd = v
		}

		bracketed = append(bracketed, toAdd)
	}

	for _, operators := range [][]Operator{
		[]Operator{"."},
		[]Operator{".>"},
		[]Operator{"/<", "/>"},
		[]Operator{"*", "/"},
		[]Operator{"+", "~"},
		[]Operator{"<<", ">>"},
		[]Operator{"<=", ">=", "<", ">"},
		[]Operator{"==", "=", "~="},
		[]Operator{"&", "|", "^"},
	} {
		for i := 1; i < len(bracketed); i++ {
			t := bracketed[i]

			switch v := t.(type) {
			case Operator:
				for _, operator := range operators {
					if v == operator {
						i--

						a := bracketed[i]
						b := bracketed[i + 2]

						bracketed = append(bracketed[:i], bracketed[i + 2:]...)

						bracketed[i] = Operation{v, a, b}

						break
					}
				}
			default:
				_, isOperator := bracketed[i - 1].(Operator)
				if isOperator {
					break
				}

				i--
				
				a := bracketed[i]
				b := bracketed[i + 1]

				bracketed = append(bracketed[:i], bracketed[i + 1:]...)

				bracketed[i] = Operation{"", a, b}
			}
		}
	}

	if len(bracketed) == 0 {
		return Expression{Null{}}, i
	}

	return Expression{bracketed[0]}, i
}

func valueOf(x Unit) Fraction {
	switch v := x.(type) {
	case Fraction:
		return v
	case String:
		return Fraction{len(v), 1}
	case List:
		return Fraction{len(v), 1}
	}
	return Fraction{0, 0}
}

func Evaluate(u Unit, s Dict) Unit {
	switch v := u.(type) {
	case Expression:
		return Evaluate(v.Result, s)
	case Symbol:
		val, ok := s[string(v)]
		if ok {
			return val
		}
		return Null{}
	case Operation:
		switch v.Name {
		case "":
			return Evaluate(v.A, s).(Function)(Evaluate(v.B, s))
		case "<<", ">>":
			a := Evaluate(v.A, s).(String)
			b := whole(Evaluate(v.B, s).(Fraction))

			right := v.Name == ">>"

			if b < 0 {
				b = -b
				right = !right
			}

			bd := b / 8
			br := b % 8

			res := make([]byte, len(a))

			if right {
				for i := bd; i < len(a); i++ {
					res[i] = a[i - bd]
				}
			} else {
				for i := len(a) - bd - 1; i >= 0; i-- {
					res[i] = a[i + bd]
				}
			}

			var c byte = 0

			if right {
				for i := 0; i < len(res); i++ {
					d := res[i]
					res[i] = (d >> br) | c
					c = d << (8 - br)
				}
			} else {
				for i := len(res) - 1; i >= 0; i-- {
					d := res[i]
					res[i] = (d << br) | c
					c = d >> (8 - br)
				}
			}

			return String(res)

		case "&", "|", "^":
			a := Evaluate(v.A, s).(String)
			b := Evaluate(v.B, s).(String)
			var res strings.Builder
			for i := 0; i < len(a); i++ {
				var bi byte = 0
				if i < len(b) {
					bi = b[i]
				}
				switch v.Name {
				case "&":
					res.WriteByte(a[i] & bi)
				case "|":
					res.WriteByte(a[i] | bi)
				case "^":
					res.WriteByte(a[i] ^ bi)
				}
			}
			return String(res.String())
		case "<", ">", "<=", ">=", "=":
			comp := fsign(sub(valueOf(Evaluate(v.A, s)), valueOf(Evaluate(v.B, s))))

			t := false

			switch v.Name {
			case "<":
				t = comp < 0
			case ">":
				t = comp > 0
			case "<=":
				t = comp <= 0
			case ">=":
				t = comp >= 0
			case "=":
				t = comp == 0
			}

			if t {
				return Fraction{1, 1}
			} else {
				return Fraction{0, 1}
			}
		case "==":
			t := Evaluate(v.A, s) == Evaluate(v.B, s)
			if t {
				return Fraction{1, 1}
			} else {
				return Fraction{0, 1}
			}
		case "~=":
			eq := reflect.TypeOf(Evaluate(v.A, s)) == reflect.TypeOf(Evaluate(v.B, s))
			if eq {
				return Fraction{1, 1}
			}
			return Fraction{0, 1}
		case "*":
			b := Evaluate(v.B, s)
			switch a := Evaluate(v.A, s).(type) {
			case Fraction:
				return mul(a, b.(Fraction))
			case List:
				l := List{}
				for _, e := range a {
					l = append(l, b.(Function)(e))
				}
				return l
			case Dict:
				l := List{}
				kl := []string{}
				for k, _ := range a {
					kl = append(kl, k)
				}
				slices.Sort(kl)
				for _, k := range kl {
					l = append(l, b.(Function)(String(k)))
				}
				return l
			}
		case "~":
			b := Evaluate(v.B, s)
			switch a := Evaluate(v.A, s).(type) {
			case Fraction:
				return sub(a, b.(Fraction))
			case String:
				return sub(Fraction{len(a), 1}, b.(Fraction))
			case List:
				return sub(Fraction{len(a), 1}, b.(Fraction))
			}
			return Null{}
		case "+":
			b := Evaluate(v.B, s)
			switch a := Evaluate(v.A, s).(type) {
			case Fraction:
				return add(Evaluate(v.A, s).(Fraction), Evaluate(v.B, s).(Fraction))
			case String:
				return a + b.(String)
			case List:
				return append(a, b.(List))
			case Dict:
				return merge(a, b.(Dict))
			}
			return Null{}
		case "/":
			b := Evaluate(v.B, s)
			switch a := Evaluate(v.A, s).(type) {
			case Fraction:
				return div(a, b.(Fraction))
			case String:
				bh := whole(b.(Fraction))
				if bh < 0 {
					return a[len(a) + bh:]
				} else {
					return a[:bh]
				}
			}
		case ".":
			ua := Evaluate(v.A, s)

			var b Unit

			switch bv := v.B.(type) {
			case Symbol:
				b = String(bv)
			default:
				b = Evaluate(bv, s)
			}

			var val Unit
			var ok bool

			switch a := ua.(type) {
			case String:
				ok = b.(Fraction).Denominator == 1 && b.(Fraction).Numerator >= 0 && b.(Fraction).Numerator < len(a)
				if ok {
					val = a[b.(Fraction).Numerator]
				}
			case List:
				ok = b.(Fraction).Denominator == 1 && b.(Fraction).Numerator >= 0 && b.(Fraction).Numerator < len(a)
				if ok {
					val = a[b.(Fraction).Numerator]
				}
			case Dict:
				val, ok = a[string(b.(String))]
			}

			if ok {
				return val
			}
			return Null{}
		}
	case Protolist:
		l := List{}
		for _, e := range v {
			l = append(l, Evaluate(e, s))
		}
		return l
	case Protodict:
		d := Dict{}
		for _, e := range v {
			key, nonStringKey := e.Key.(Symbol)

			val := Evaluate(e.Value, s)

			if nonStringKey {
				d[string(key)] = val
			} else {
				d[string(Evaluate(e.Key, s).(String))] = val
			}
		}
		return d
	case Protofunction:
		return Function(func(x Unit) Unit {
			nCtx := Dict{}
			for k, v := range s {
				nCtx[k] = v
			}
			nCtx[string(v.Variable)] = x
			return Evaluate(v.Result, nCtx)
		})
	}

	return u
}

func Interpret(script string, scope Dict) Unit {
	tokens := Lex(script)
	parsed, _ := Parse(tokens, 0)
	return Evaluate(parsed.Result, scope)
}

func prettyList(l []Unit, tabs int) string {
	r := "["
	for i, e := range l {
		r += Pretty(e, tabs)

		if i + 1 < len(l) {
			r += ", "
		}
	}

	return r + "]"
}

func genTabs(n int) string {
	if n == 0 {
		return ""
	}
	return "\t" + genTabs(n - 1)
}

func Pretty(u Unit, tabs int) string {
	switch v := u.(type) {
	case Operation:
		return Pretty(v.A, tabs) + " " + Pretty(v.Name, tabs) + " " + Pretty(v.B, tabs)
	case Expression:
		return "(" + Pretty(v.Result, tabs) + ")"
	case Null:
		return "_"
	case Special:
		return string(v)
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
			return fmt.Sprintf("%d", v.Numerator)
		}
		return fmt.Sprintf("(%d / %d)", v.Numerator, v.Denominator)
	case Protolist:
		return prettyList(v, tabs)
	case List:
		return prettyList(v, tabs)
	case Protodict:
		r := "{\n"

		for _, a := range v {
			r += genTabs(tabs + 1) + Pretty(a.Key, tabs + 1) + ": " + Pretty(a.Value, tabs + 1) + ",\n"
		}

		return r + genTabs(tabs) + "}"
	case Dict:
		r := "{\n"

		for k, v := range v {
			r += genTabs(tabs + 1) + Pretty(String(k), tabs + 1) + ": " + Pretty(v, tabs + 1) + ",\n"
		}

		return r + genTabs(tabs) + "}"
	case Protofunction:
		return "(\\" + string(v.Variable) + " " + Pretty(v.Result, tabs) + ")"
	case Function:
		return "(\\x)"
	}

	return "<UNKNOWN>"
}
