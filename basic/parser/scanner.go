package parser

import (
	"bufio"
	"unicode"
)

// ծառայողական բառեր
var keywords = map[string]int{
	"SUB":    xSubroutine,
	"DIM":    xDim,
	"LET":    xLet,
	"INPUT":  xInput,
	"PRINT":  xPrint,
	"IF":     xIf,
	"THEN":   xThen,
	"ELSEIF": xElseIf,
	"ELSE":   xElse,
	"WHILE":  xWhile,
	"FOR":    xFor,
	"TO":     xTo,
	"STEP":   xStep,
	"CALL":   xCall,
	"END":    xEnd,
	"AND":    xAnd,
	"OR":     xOr,
	"NOT":    xNot,
	"TRUE":   xTrue,
	"FALSE":  xFalse,
}

// Բառային վերլուծիչի ստրուկտուրան
type scanner struct {
	source *bufio.Reader // կարդալու հոսքը

	ch   rune   // ընթացիկ նիշը
	text string // կարդացված լեքսեմը
	line int    // ընթացիկ տողը
}

func newScanner(reader *bufio.Reader) *scanner {
	sc := &scanner{
		source: reader,
		line: 1,
	}
	sc.read()
	return sc
}

// Կարդում և վերադարձնում է հերթական լեքսեմը։
func (s *scanner) next() *lexeme {
	// հոսքի ավարտը
	if s.ch == 0 {
		return &lexeme{xEof, "EOF", s.line}
	}

	// բաց թողնել բացատանիշերը
	s.skipWhitespaces()

	// բաց թողնել մեկնաբանությունները
	if s.ch == '\'' {
		s.skipComments()
		return s.next()
	}

	// իրական թվեր
	if unicode.IsDigit(s.ch) {
		return s.scanNumber()
	}

	// տեքստային լիտերալ
	if s.ch == '"' {
		return s.scanText()
	}

	// իդենտիֆիկատորներ ու ծառայողական բառեր
	if unicode.IsLetter(s.ch) {
		return s.scanIdentifierOrKeyword()
	}

	// նոր տողի անցման նիշ
	if s.ch == '\n' {
		s.line++
		s.read()
		return &lexeme{xNewLine, "<-/", s.line}
	}

	// գործողություններ և այլ կետադրական ու ղեկավարող նիշեր
	if s.ch == '<' {
		s.read()
		if s.ch == '>' {
			s.read()
			return &lexeme{xNe, "<>", s.line}
		}
		if s.ch == '=' {
			s.read()
			return &lexeme{xLe, "<=", s.line}
		}
		return &lexeme{xLt, "<", s.line}
	}

	if s.ch == '>' {
		s.read()
		if s.ch == '=' {
			s.read()
			return &lexeme{xGe, ">=", s.line}
		}
		return &lexeme{xGt, ">", s.line}
	}

	var kind int
	switch s.ch {
	case '+':
		kind = xAdd
	case '-':
		kind = xSub
	case '*':
		kind = xMul
	case '/':
		kind = xDiv
	case '\\':
		kind = xMod
	case '^':
		kind = xPow
	case '&':
		kind = xAmp
	case '=':
		kind = xEq
	case '(':
		kind = xLeftPar
	case ')':
		kind = xRightPar
	case '[':
		kind = xLeftBr
	case ']':
		kind = xRightBr
	case ',':
		kind = xComma
	default:
		kind = xNone
	}

	res := &lexeme{kind, string(s.ch), s.line}
	s.read()

	return res
}

// Ներմուծման հոսքից կարդում է մեկ նիշ և վերագրում է ch դաշտին
func (s *scanner) read() {
	c, _, e := s.source.ReadRune()
	if e != nil {
		s.ch = 0
	} else {
		s.ch = c
	}
}

// Ներմուծման հոսքից կարդում է pred պրեդիկատին բավարարող նիշերի
// անընդհատ հաջորդականություն։ Կարդացածը պահվում է text դաշտում։
func (s *scanner) scan(pred func(rune) bool) {
	s.text = ""
	for pred(s.ch) && s.ch != 0 {
		s.text += string(s.ch)
		s.read()
	}
}

func (s *scanner) skipWhitespaces() {
	for s.ch == ' ' || s.ch == '\t' || s.ch == '\r' {
		s.read()
	}
}

func (s *scanner) skipComments() {
	for s.ch != '\n' {
		s.read()
	}
}

func (s *scanner) scanNumber() *lexeme {
	s.scan(unicode.IsDigit)
	nuval := s.text
	if s.ch == '.' {
		nuval += "."
		s.read()
	}
	s.scan(unicode.IsDigit)
	nuval += s.text
	return &lexeme{xNumber, nuval, s.line}
}

func (s *scanner) scanText() *lexeme {
	s.read()
	s.scan(func(c rune) bool { return c != '"' })
	if s.ch != '"' {
		return &lexeme{xEof, "EOF", s.line}
	}
	s.read()
	return &lexeme{xText, s.text, s.line}
}

// Հոսքից կարդում է տառերի ու թվանշանների հաջորդականություն։
// Եթե կարդացածը keywords ցուցակից է, ապա վերադարձնում է
// ծառայողական բառի lexeme, հակառակ դեպքում՝ identifier-ի։
func (s *scanner) scanIdentifierOrKeyword() *lexeme {
	s.scan(func(c rune) bool {
		return unicode.IsLetter(c) || unicode.IsDigit(c)
	})
	if s.ch == '$' {
		s.text += "$"
		s.read()
	}
	kw, ok := keywords[s.text]
	if !ok {
		kw = xIdent
	}
	return &lexeme{kw, s.text, s.line}
}
