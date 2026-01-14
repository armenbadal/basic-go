package parser

import (
	"bufio"
	"unicode"
)

// Բառային վերլուծիչի ստրուկտուրան
type scanner struct {
	source *bufio.Reader // կարդալու հոսքը
	peeked rune          // կարդացած, բայց դեռ չօգտագործած նիշ
	text   string        // կարդացված լեքսեմը
	line   int           // ընթացիկ տողը
}

// Կարդում և վերադարձնում է հերթական լեքսեմը։
func (s *scanner) next() *lexeme {
	// բաց թողնել բացատանիշերը
	if isSpace(s.peek()) {
		s.scan(isSpace)
	}

	// բաց թողնել մեկնաբանությունները
	if s.peek() == '\'' {
		s.scan(func(c rune) bool { return c != '\n' })
	}

	// հոսքի ավարտը
	if s.peek() == eos {
		return &lexeme{xEof, "EOF", s.line}
	}

	// իրական թվեր
	if unicode.IsDigit(s.peek()) {
		return s.scanNumber()
	}

	// իդենտիֆիկատորներ ու ծառայողական բառեր
	if unicode.IsLetter(s.peek()) {
		return s.scanIdentifierOrKeyword()
	}

	// տեքստային լիտերալ
	if s.peek() == '"' {
		return s.scanText()
	}

	// նոր տողի անցման նիշ
	if s.peek() == '\n' {
		s.read()
		return &lexeme{xNewLine, "<-/", s.line}
	}

	// գործողություններ և այլ կետադրական ու ղեկավարող նիշեր
	return s.scanOperationOrMetasymbol()
}

func (s *scanner) read() rune {
	ch := s.peek()
	s.peeked = readRune(s.source)
	return ch
}

func (s *scanner) peek() rune {
	if s.peeked == -1 {
		s.peeked = readRune(s.source)
	}
	return s.peeked
}

const eos = 0

func readRune(r *bufio.Reader) rune {
	ch, _, err := r.ReadRune()
	if err != nil {
		return eos
	}
	return ch
}

// Ներմուծման հոսքից կարդում է pred պրեդիկատին բավարարող նիշերի
// անընդհատ հաջորդականություն։ Կարդացածը պահվում է text դաշտում։
func (s *scanner) scan(pred func(rune) bool) {
	s.text = ""
	for s.peek() != eos && pred(s.peek()) {
		s.text += string(s.read())
	}
}

func isSpace(c rune) bool {
	return c == ' ' || c == '\t' || c == '\r'
}

func (s *scanner) scanNumber() *lexeme {
	s.scan(unicode.IsDigit)
	num := s.text
	if s.peek() == '.' {
		s.read()
		num += "."
		s.scan(unicode.IsDigit)
		num += s.text
	}
	return &lexeme{xNumber, num, s.line}
}

func (s *scanner) scanText() *lexeme {
	s.read()
	s.scan(func(c rune) bool { return c != '"' })
	if s.peek() == eos {
		return &lexeme{xEof, "EOF", s.line}
	}
	s.read()
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
	s.scan(func(c rune) bool {
		return unicode.IsLetter(c) || unicode.IsDigit(c)
	})
	if s.peek() == '$' {
		s.text += "$"
	}

	kw, ok := keywords[s.text]
	if !ok {
		kw = xIdent
	}

	return &lexeme{kw, s.text, s.line}
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

func (s *scanner) scanOperationOrMetasymbol() *lexeme {
	if s.peek() == '<' {
		s.read()
		if s.peek() == '>' {
			s.read()
			return &lexeme{xNe, "<>", s.line}
		}
		if s.peek() == '=' {
			s.read()
			return &lexeme{xLe, "<=", s.line}
		}
		return &lexeme{xLt, "<", s.line}
	}

	if s.peek() == '>' {
		s.read()
		if s.peek() == '=' {
			return &lexeme{xGe, ">=", s.line}
		}
		return &lexeme{xGt, ">", s.line}
	}

	kind, exists := metasymbols[s.peek()]
	if !exists {
		kind = xNone
	}
	return &lexeme{kind, string(s.read()), s.line}
}
