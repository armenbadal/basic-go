package parser

import (
	"bufio"
	"unicode"
)

// Բառային վերլուծիչի ստրուկտուրան
type scanner struct {
	source *bufio.Reader // կարդալու հոսքը

	text string // կարդացված լեքսեմը
	line int    // ընթացիկ տողը
}

// մետասիմվոլներ․ գորողություններ, կետադրություն
var metasymbols = map[rune]int{
	'+':  xAdd,
	'-':  xSub,
	'*':  xMul,
	'/':  xDiv,
	'\\': xMod,
	'^':  xPow,
	'&':  xAmp,
	'=':  xEq,
	'(':  xLeftPar,
	')':  xRightPar,
	'[':  xLeftBr,
	']':  xRightBr,
	',':  xComma,
}

// Կարդում և վերադարձնում է հերթական լեքսեմը։
func (s *scanner) next() *lexeme {
	ch := s.read()

	// հոսքի ավարտը
	if ch == 0 {
		return &lexeme{xEof, "EOF", s.line}
	}

	// բաց թողնել բացատանիշերը
	if isSpace(ch) {
		s.skipWhitespaces()
		ch = s.read()
		if ch == 0 {
			return &lexeme{xEof, "EOF", s.line}
		}
	}

	// բաց թողնել մեկնաբանությունները
	if ch == '\'' {
		s.skipComments()
		ch = s.read()
		if ch == 0 {
			return &lexeme{xEof, "EOF", s.line}
		}
	}

	// իրական թվեր
	if unicode.IsDigit(ch) {
		s.unread()
		return s.scanNumber()
	}

	// տեքստային լիտերալ
	if ch == '"' {
		return s.scanText()
	}

	// իդենտիֆիկատորներ ու ծառայողական բառեր
	if unicode.IsLetter(ch) {
		s.unread()
		return s.scanIdentifierOrKeyword()
	}

	// նոր տողի անցման նիշ
	if ch == '\n' {
		s.line++
		return &lexeme{xNewLine, "<-/", s.line}
	}

	// գործողություններ և այլ կետադրական ու ղեկավարող նիշեր
	if ch == '<' {
		ch = s.read()
		if ch == '>' {
			return &lexeme{xNe, "<>", s.line}
		}
		if ch == '=' {
			return &lexeme{xLe, "<=", s.line}
		}
		s.unread()
		return &lexeme{xLt, "<", s.line}
	}

	if ch == '>' {
		ch = s.read()
		if ch == '=' {
			return &lexeme{xGe, ">=", s.line}
		}
		s.unread()
		return &lexeme{xGt, ">", s.line}
	}

	kind, exists := metasymbols[ch]
	if !exists {
		kind = xNone
	}
	return &lexeme{kind, string(ch), s.line}
}

// Ներմուծման հոսքից կարդում է մեկ նիշ և վերագրում է ch դաշտին
func (s *scanner) read() rune {
	ch, _, err := s.source.ReadRune()
	if err != nil {
		return 0
	}
	return ch
}

func (s *scanner) unread() {
	s.source.UnreadRune()
}

// Ներմուծման հոսքից կարդում է pred պրեդիկատին բավարարող նիշերի
// անընդհատ հաջորդականություն։ Կարդացածը պահվում է text դաշտում։
func (s *scanner) scan(pred func(rune) bool) {
	s.text = ""
	ch := s.read()
	for ch != 0 && pred(ch) {
		s.text += string(ch)
		ch = s.read()
	}
	s.unread()
}

func isSpace(c rune) bool {
	return c == ' ' || c == '\t' || c == '\r'
}

func (s *scanner) skipWhitespaces() {
	s.scan(isSpace)
}

func (s *scanner) skipComments() {
	s.scan(func(c rune) bool { return c != '\n' })
}

func (s *scanner) scanNumber() *lexeme {
	s.scan(unicode.IsDigit)
	nuval := s.text
	if ch := s.read(); ch == '.' {
		nuval += "."
		s.scan(unicode.IsDigit)
		nuval += s.text
	} else {
		s.unread()
	}
	return &lexeme{xNumber, nuval, s.line}
}

func (s *scanner) scanText() *lexeme {
	s.scan(func(c rune) bool { return c != '"' })
	if ch := s.read(); ch != '"' {
		return &lexeme{xEof, "EOF", s.line}
	}
	return &lexeme{xText, s.text, s.line}
}

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

// Հոսքից կարդում է տառերի ու թվանշանների հաջորդականություն։
// Եթե կարդացածը keywords ցուցակից է, ապա վերադարձնում է
// ծառայողական բառի lexeme, հակառակ դեպքում՝ identifier-ի։
func (s *scanner) scanIdentifierOrKeyword() *lexeme {
	s.scan(isAlphaOrDigit)
	if ch := s.read(); ch == '$' {
		s.text += "$"
	} else {
		s.unread()
	}

	kw, ok := keywords[s.text]
	if !ok {
		kw = xIdent
	}

	return &lexeme{kw, s.text, s.line}
}

func isAlphaOrDigit(c rune) bool {
	return unicode.IsLetter(c) || unicode.IsDigit(c)
}
