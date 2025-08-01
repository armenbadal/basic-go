package parser

import "bufio"

type source struct {
	reader *bufio.Reader // բուֆերացված հոսք
	peeked rune // դիտարկվող նիշը
}

// հերթական նիշը, եթե EOS է, ապա՝ 0
func (s *source) read() rune {
	result := s.peeked

	ch, count, err := s.reader.ReadRune()
	if err != nil || count == 0 {
		s.peeked = 0
	} else {
		s.peeked = ch
	}

	return result
}

// հերթական նիշը, եթե EOS է, ապա՝ 0
func (s *source) peek() rune {
	return s.peeked
}
