# Շարահյուսական վերլուծություն

## Վերլուծիչի կառուցվածքը

Շարահյուսական վերլուծիչը բաղկացած է երկու տրամաբանական մասերից. _բառային վերլուծիչ_ (`scanner`) և բուն _շարահյուսական վերլուծիչ_ (`Parser`)։ Բառային վերլուծիչը կարդում է ծրագրի տեքստը, առանձնացնում է իմաստ կրող հատվածները՝ _լեքսեմները_ (lexeme), ամեն մեկին համապատասխանեցնում է տիպը որոշող _պիտակ_ (token)։ Շարահյուսական վերլուծիչն արդեն, օգտագործելով պիտակավորված լեքսեմները և հետևելով քերականական կանոններին, կառուցում է վերլուծության ծառը։

![Բառային ու շարահյուսական վերլուծիչները](scanner-parser-interaction.png)

Գծապատկերում Scanner-ն ու Parser-ը միացնող սլաքներով ուզում եմ ցույց տալ, որ այդ երկու բաղադրիչներ աշխատում են իրար հետ համատեղ։ Երբ որոշում կայացնելու համար Parser-ին պետք է հերթական թոքեն-լեքսեմ զույգը, այն կանչում է Scanner-ի `next()` մեթոդը։


## Լեքսեմներ

Լեքսեմներն ու պիտակները սահմանված են `lexeme.go` ֆայլում։ Լեքսեմը ստրուկտուրա է, որը միավորում է պիտակը, լեքսեմի արժեքը և ֆայլի այն տողի համարը, որտեղ հանդիպել է լեքսեմը․

```Go
type lexeme struct {
    token int    // պիտակ
    value string // արժեք
    line  int    // տողը
}
```

Պիտակները սահմանված են որպես հաստատուններ։ Ահա դրանք՝ պայմանականորեն խմբավորված ըստ իմաստների․

```Go
const (
    xNone = iota

    // լիտերալներ
    xNumber // թիվ
    xText   // տեքստ
    xIdent  // իդենտիֆիկատոր
    xTrue   // TRUE
    xFalse  // FALSE

    // ծառայողական բառեր
    xSubroutine // SUB
    xDim        // DIM
    xLet        // LET
    xInput      // INPUT
    xPrint      // PRINT
    xIf         // IF
    xThen       // THEN
    xElseIf     // ELSEIF
    xElse       // ELSE
    xWhile      // WHILE
    xFor        // FOR
    xTo         // TO
    xStep       // STEP
    xCall       // CALL
    xEnd        // END

    // գործողություններ
    xAdd // +
    xSub // -
    xAmp // &
    xMul // *
    xDiv // /
    xMod // \
    xPow // ^

    xEq // =
    xNe // <>
    xGt // >
    xGe // >=
    xLt // <
    xLe // <=

    xAnd // AND
    xOr  // OR
    xNot // NOT

    // կետադրական նշաններ
    xNewLine  // <-/
    xLeftPar  // (
    xRightPar // )
    xLeftBr   // [
    xRightBr  // ]
    xComma    // ,

    xEof // ֆայլի վերջը
)
```

Որևէ լեքսեմի՝ տրված պիտակն ունենալը պարզելու համար է սահմանված `is()` մեթոդը․

```Go
func (l *lexeme) is(exps ...int) bool {
    return slices.Contains(exps, l.token)
}
```

Սա սահմանված է վարիադիկ տարբերակով, որպեսզի `e.is(xAdd) || e.is(xSub) || e.is(xAmp)` արտահայտության փոխարեն հնարավոր լինի կարճ գրել `e.is(xAdd, xSub, xAmp)`:


## Բառային վերլուծություն

Բառային վերլուծության (lexical analysis) ժամանակ Բալ ծրագրում պետք է առանձնացվեն ու պիտակավորվեն _ծառայողական բառերը_, _իդենտիֆիկատորները_, _գործողությունների_ սիմվոլները, _մետասիմվոլները_, _թվային_ ու _տեքստային_ լիտերալները։ Այս գործողության ժամանակ է, որ ծրագրի տեքստից հեռացվում են մեկնաբանություններն ու ավելորդ բացատանիշերը։

Օրինակ, դիտարկենք Բալ ծրագրի մի հատված, որում, պարզապես ցուցադրման համար, `␣` նիշով նշված է բացատը, իսկ `↵` նիշով՝ նոր տողին անցումը։

```basic
IF␣a␣>␣1.8␣THEN↵
␣␣␣␣LET␣s[2]␣=␣a^2↵
END␣IF↵
```

Բառային վերլուծիչը, նիշ առ նիշ կարդալով այս տեքստը, տրոհում է այն իմաստակիր հատվածների՝ _լեքսեմների_, ապա, ըստ լեզվի բառակազմական կանոնների, դրանցից ամեն մեկին համապատասխանեցնում է _պիտակ_։ Այսպես․

| Լեքսեմ | Պիտակ      |
|:------:|------------|
| `IF`   | `xIf`      |
| `a`    | `xIdent`   |
| `>`    | `xGt`      |
| `1.8`  | `xNumber`  |
| `THEN` | `xThen`    |
| `↵`    | `xNewLine` |
| `LET`  | `xLet`     |
| `s`    | `xIdent`   |
| `[`    | `xLeftBr`  |
| `2`    | `xNumber`  |
| `]`    | `xRightBr` |
| `=`    | `xEq`      |
| `a`    | `xIdent`   |
| `^`    | `xPow`     |
| `2`    | `xNumber`  |
| `↵`    | `xNewLine` |
| `END`  | `xEnd`     |
| `IF`   | `xEnd`     |
| `↵`    | `xNewLine` |

Տեսնում ենք, որ բացատներն ու մեկնաբանությունն ընդհանրապես անտեսվել են, իսկ նոր տողի նիշը, որ Բալի քերականության մեջ կարևոր տեղ է զբաղեցնում, ստացել է `xNewLine` պիտակը։

`scanner.go` ֆայլում իրականացված վերլուծիչը մոդելավորում է պարզագույն _վերջավոր ավտոմատ_։ Այդ ավտոմատի կոնտեքստը պահվում է `scanner` ստրուկտուրայում՝ սահմանված հետևյալ կերպ․

```go
type scanner struct {
	source *bufio.Reader // կարդալու հոսքը
	peeked rune          // կարդացած, բայց դեռ չօգտագործած նիշ
	text   string        // կարդացված լեքսեմը
	line   int           // ընթացիկ տողը
}
```

Սրա `source`-ը ծրագրի տեքստը պարունակող ֆայլին կապված ընթերցման հոսքն է, որը, եթե խոսենք վերջավոր ավտոմատների տերմիններով, կատարում է _ժապավենի_ դերը։ Այդ «ժապավենից» հերթական նիշը կարդալու համար նախատեսված է `readRune` ֆունկցիան։

```Go
const eos = 0

func readRune(r *bufio.Reader) rune {
	ch, _, err := r.ReadRune()
	if err != nil {
		return eos
	}
	return ch
}
```

Սա ընդամենը պարզեցնում է կարդալու գործողությունը՝ հոսքի ավարտի համար վերադարձնելով `eos` հաստատունը։

Ինչպես վերջավոր ավտոմատն է ժապավենի հերթական նիշը դիտարկելով որոշում թե հաջորդը որ վիճակին պետք է անցնի, այնպես էլ մեր բառային վերլուծիչն է `peek()` մեթոդով _դիտարկելու_ հոսքի հերթական նիշը և դրանով էլ որոշելու, թե ինչ պրոցեդուրայով պետք է ճանաչի հերթական սիմվոլը։ 

```Go
func (s *scanner) peek() rune {
	if s.peeked == -1 {
		s.peeked = readRune(s.source)
	}
	return s.peeked
}
```

Oրինակ, եթե դիտարկվում է թվանշան, ապա պետք է կարդալ թիվ, եթե դիտարկվում է տառ, ապա պետք է կարդալ իդենտիֆիկատոր կամ ծառայողական բառ ու այսպես շարունակ։ Շեշտենք, որ `peek()` մեթոդը չի «օգտագործում» նիշը, այլ միայն դիտարկում է այն։ Հոսքից նիշ կարդալու համար նախատեսված է `read()` մեթոդը։

```Go
func (s *scanner) read() rune {
	ch := s.peek()
	s.peeked = readRune(s.source)
	return ch
}
```

`scanner`-ի կառուցման ֆունկցիայում առաջին նիշը կարդալը կարևոր է նրանով, որ բառային վերլուծության հաջորդ քայլերի ուղղությունը որոշվում է այս նիշով։ Օրինակ, եթե այդ առաջին նիշը բացատ է, ապա պետք է կարդալ ու դեն նետել հաջորդող բոլոր բացատները, իսկ եթե այն մեկնաբանության սկիզբը ցույց տվող `'` նիշն է, ապա պետք է կարդալ ու դեն նետել մինչև տողի վերջը հանդիպող բոլոր նիշերը, և այսպես շարունակ՝ ըստ լեզվի բառակազմության (լեքսիկայի) կանոնների։

Հոսքից մեկ հերթական լեքսեմը կարդալու ամբողջ տրամաբանությունը, ըստ էության՝ Բալ լեզվի բառակազմական կանոնների ամբողջությունը, ծրագրավորված է `scanner`-ի `next()` մեթոդում։ Նրա աշխատանքի ալգորիթմը հետևյալն է․

1. Քանի դեռ հոսքի հերթական սիմվոլը _բացատանիշ_ է, մասնավորապես՝ `␣`, `\t` կամ `\r`, ապա կարդալ ու դեն նետել դրանք։
2. Եթե հերթական նիշը _ապաթարցն_ `'` է, որը ցույց է տալիս մեկնաբանության սկիզբը, ապա կարդալ ու դեն նետել մինչ տողի վերջը հանդիպող բոլոր նիշերը։
3. Ստուգել հոսքի վիճակը․ եթե այն _ավարտված_ է, ապա վերադարձնել `xEof` պիտակով լեքսեմ։
4. Եթե հոսքի հերթական նիշը _թվանշան_ է, ապա կարդալ `[0-9]+(\.[0-9]+)?` կանոնավոր արտահայտությանը համապատասխանող իրական թիվ ու վերադարձնել `xNumber` պիտակով լեքսեմ։
5. Եթե _տառ_ է, ապա կարդալ `[a-zA-Z][0-9a-zA-Z]*` կանոնավոր արտահայտությանը համապատասխանող հաջորդականություն։ Այնուհետև, օգտագործելով `keywords` աղյուսակը, պարզել, թե արդյոք կարդացած լեքսեմը ծառայողական բառ է, կամ իդենտիֆիկատոր։
6. Եթե հերթական նիշը _չակերտ_ `"` է, ապա կարդալ տեքստային լիտերալ ու վերադարձնել `xText` պիտակով լեքսեմ։
7. Եթե հոսքում _նոր տողին անցման_ նիշ է, ապա մեկով ավելացնել `line`-ի արժեքը ու վերադարձնել `xNewLine` պիտակով լեքսեմ։
8. Եթե գործողության կամ այլ կետադրական ու ղեկավարող նիշ է, ապա կարդալ ու պիտակավորել այն։

Գո լեզվով իրականացումն այս ալգորիթմի ուղղակի իրականացումն է.

```Go
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
```

Ուշադրություն դարձնենք այստեղ օգտագործված `scan()` մեթոդին։ Սա մի ունիվերսալ միջոց է, որ ներմուծման հոսքից կարդում է `pred` պրեդիկատին բավարարող նիշերի անընդհատ հաջորդականություն։ Կարդացածը պահվում է `text` դաշտում։


```Go
func (s *scanner) scan(pred func(rune) bool) {
	s.text = ""
	for s.peek() != eos && pred(s.peek()) {
		s.text += string(s.read())
	}
}
```

Եթե պետք է կարդալ բացատանիշերը, ապա կանչված է `s.scan(isSpace)` տեսքով, որտեղ `isSpace` պրեդիկատը սահմանված է Բալի կանոններով.

```Go
func isSpace(c rune) bool {
	return c == ' ' || c == '\t' || c == '\r'
}
```

Իսկ եթե պետք է կարդալ բոլոր նիշերը մինչև տողի վերջը, ինչպես մեկնաբանությունների դեպքն է, ապա ունենք այսպիսի կոդ.

```Go
if s.peek() == '\'' {
	s.scan(func(c rune) bool { return c != '\n' })
}
```

Նույն այս `scan()` մեթոդն է օգտագործված `scanNumber()`, `scanIdentifierOrKeyword()` և `scanText()` մեթոդներում։ Հետաքրքիր է `scanNumber()՝-ի իրականացումը. այստեղ `scan()`-ը օգտագործված է իրական թվի տասնորդական կետով բաժանված մասերը կարդալու համար։

```Go
func (s *scanner) scanNumber() *lexeme {
	s.scan(unicode.IsDigit) // կետին նախորդող մասը
	num := s.text
	if s.peek() == '.' {
		s.read()
		num += "."
		s.scan(unicode.IsDigit) // կետին հաջորդող մասը, եթե կա
		num += s.text
	}
	return &lexeme{xNumber, num, s.line}
}
```

Երբ `next()` մեթոդում ներմուծման հոսքի հերթական նիշը դիտարկելիս տեսնում ենք տառ՝ որոշված `unicode.IsLetter()` պրոդիկատով, ապա դա կարող է լինել կա՛մ լեզվի ծառայողական բառի, կա՛մ էլ իդենտիֆիկատորի առաջին տառը։ `scanIdentifierOrKeyword()` մեթոդով կարդում ենք տառերի ու թվանշանների հաջորդականություն։ Եթե կարդացած տեքստը ծառայողական բառերի ցուցակից է, ապա ստեղծում ու վերադարձնում ենք համապատասխան լեքսեմը, հակառակ դեպքում՝ հանդիպել ենք իդենտիֆիկատորի, ուրեմն վերադարձնում ենք `xIdent` պիտակով լեքսեմ։

```Go
func (s *scanner) scanIdentifierOrKeyword() *lexeme {
	s.scan(func(c rune) bool {
		return unicode.IsLetter(c) || unicode.IsDigit(c)
	})

	kw, ok := keywords[s.text]
	if !ok {
		kw = xIdent
	}

	return &lexeme{kw, s.text, s.line}
}
```

Հիշատակված `keyword` ցուցակը սովորական `map` է, որը բոլոր ծառայողական բառերը համապատասխանեցում է իրենց պիտակներին.

```Go
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
```

Գործողություններն ու մետասիմվոլները ճանաչող `scanOperationOrMetasymbol` մեթոդն էլ շատ պարզ է։ Այստեղ առանձնացված են այն դեպքերը, երբ գործողության նշանը մի քանի սիմվոլներից է կազմված. `>=`, `<=` և `<>`: Մյուս դեպքերում օգտագործվում է `metasymbols` աղյուսակը։

```Go
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
```

Այսքանը բառային վերլուծության մասին։ Անցնենք առաջ։


## Շարահյուսական վերլուծություն

Հիմա մեր նպատակն է Բալ լեզվի շարահյուսական կանոնները (քերականական հավասարումներ) վերածել Գո կոդի։ Ընտրել ենք շարահյուսական վերլուծիչի իրականացման _ռեկուրսիվ վայրէջքի_ (_recursive descent_) եղանակը։ Սա շատ հարմար է Բալի պես պարզ քերականություն ունեցող լեզուների համար, և իրականացումն էլ որևէ խորամանկ հնարքներ չի պահանջում։

Ուրեմն, ուղղորդվելու ենք շարահյուսական հավասարումներով՝ դրանք մեկ առ մեկ վերածելով վերլուծող պրոցեդուրաների։ Ալգորիթմը, կարելի է ասել, մեխանիկական է. հավասարման ձախ կողմի ոչ տերմինալային սիմվոլը ձևավորում է վերլուծող պրոցեդուրայի անունը, իսկ աջ մասի արտահայտությունը՝ պրոցեդուրայի մարմինը։ Եթե հավասարման սահմանաման մեջ օգտագործված է կրկնման `{...}` կառուցվածքը, ապա պրոցեդուրայի մարմնում դրանով ընդգրկված ենթարտահայտության վերլուծությունը վերցնում ենք ցիկլի մեջ, եթե օգտագործված է `[...]` կառուցվածքը (առկայություն), ապա ենթաարտահայտության վերլուծությունը վերցնում ենք պայմանի մեջ։ Օրինակ, քննարկենք քերականության հենց առաջին հավասարումը.

```
Program = { Subroutine }.
```

Այստեղ հիշատակված են երկու ոչ տերմինալային սիմվոլներ. `Program`-ը ձախ մասում և `Subroutine`-ը աջ մասում։ Ուրեմն, պետք է սահմանված լիեն, օրինակ, `parseProgram` և `parseSubroutine` ֆունկցիաները։ Կրկնություն որոշող `{...}` կառուցվածքը հուշում է, որ `parseSubroutine` ֆունկցիան պետք է կանչվի ցիկլի մեջ։ 

```Go
func parseProgram() {
	for ⟨կրկնման պայման⟩ {
		parseSubroutine()
    }
}
```

Բայց ո՞րն է ցիկլի կրկնության (կամ դադարի) պայմանը, մինչև ե՞րբ պետք է `parseSubroutine` ֆունկցիայով փորձել վերլուծել հերթական ենթածրագիրը։ Գիտենք, որ ենթածրագիրը սկսվում է `SUB` ծառայողական բառով. ուրեմն, քանի դեռ բառային վերլուծիչից կարդացած հերթական լեքսեմն ունի `xSubroutine` պիտակը, կարող ենք կանչել `parseSubroutine` ու վերլուծել հերթական ենթածրագիրը։

```Go
func parseProgram() {
	for ⟨հերթական լեքսեմ⟩.token ≡ xSubroutine {
		parseSubroutine()
    }
}
```

Մի այլ օրինակ՝ վերագրման հրամանի շարահյուսական հավասարումը, որտեղ օգտագործված է `[...]` կաուցվածքը:

```
Let = 'LET' IDENT ['[' Expression ']'] '=' Expression.
```

Այս դեպքում հավասարման աջ մասում հանդիպում են ոչ միայն ոչ տերմինալային սիմվոլներ, ինչպիսին `Expression`-ն է, այլ նաև տերմինալայիններ. `LET`, `IDENT`, `[`, `]` և `=`։ Թե ինչ պետք է անել ոչ տերմինալային սիմվոլների հետ, պարզ է նախորդ օրինակից՝ ծրագրավորել համապատասխան վերլուծող պրոցեդուրայի կանչ։ Տերմինալային սիմվոլների հետ գործն ավելի պարզ է. ընդամենը պետք է համոզվել վերլուծության տվյալ կետում սպասում ենք հենց այն տեմինալային սիմվոլին, որը գրված է հավասարման մեջ։ Ահա փսևդոկոդը.

```Go
func parseLet() error {
	if ⟨հերթական լեքսեմ⟩.token ≠ xLet {
		return "քերականական սխալ։ սպասվում է LET"
	}

	⟨հերթական լեքսեմ⟩ = scanner.next()

	if ⟨հերթական լեքսեմ⟩.token ≠ xIdent {
		return "քերականական սխալ։ սպասվում է IDENT"
	}

	⟨հերթական լեքսեմ⟩ = scanner.next()
	
	if ⟨հերթական լեքսեմ⟩.token ≡ xLeftBr {
		⟨հերթական լեքսեմ⟩ = scanner.next()
		parseExpression()
		if ⟨հերթական լեքսեմ⟩.token ≠ xRightBr {
			return "քերականական սխալ։ սպասվում է ']'"
		}
	}

	if ⟨հերթական լեքսեմ⟩.token ≠ xEq {
		return "քերականական սխալ։ սպասվում է '='"
	}
	
	⟨հերթական լեքսեմ⟩ = scanner.next()

	return parsExpression()
}
```

Արդեն նկատելի է ամեն մի տերմինալային սիմվոլի մշակման կրկնվող կոդը.

```Go
...
	if ⟨հերթական լեքսեմ⟩.token ≠ ⟨սպասվող պիտակ⟩ {
		return "քերականական սխալ։ սպասվում է <սպասվող պիտակ>"
	}
	
	⟨հերթական լեքսեմ⟩ = scanner.next()
...
```

Հարմարության համար սա առանձնացնենք `match()` անունով մի ֆունկցիայում.

```Go
func match(expected int) error {
	if ⟨հերթական լեքսեմ⟩.token ≠ expected {
		return fmt.Errorf("քերականական սխալ։ սպասվում է %v", expected)
	}
	
	⟨հերթական լեքսեմ⟩ = scanner.next()
	return nil
}
```

Ուրեմն վերագրման հրամանի `parseLet()` վերլուծիչի փսևդոկոդը կստանա ավելի համառոտ տեսք.

```Go
func parseLet() error {
	if err := match(xLet); err != nil {
		return err
	}

	if err := match(xIdent); err != nil {
		return err
	}
	
	if ⟨հերթական լեքսեմ⟩.token ≡ xLeftBr {
		match(xLeftBr)
		parseExpression()
		if err := match(xRightBr); err != nil {
			return err
		}
	}

	if err := match(xEx); err != nil {
		return err
	}

	return parsExpression()
}
```

Ահա այսպիսի վերլուծիչ ենթածրագրեր ենք օգտագործելու ամբողջական շարահյուսական վերլուծիչը կառուցելու համար։ Ուրեմն, արդեն անցնենք բուն Գո իրականացմանը։

Շարահյուսական վերլուծիչի կոնտեքստը պահվում է `Parser` ստրուկտուրայում, սահմանված `parser.go` ֆայլում․

```Go
type Parser struct {
	scanner   *scanner // բառային վերլուծիչի ցուցիչ
	lookahead *lexeme  // lookahead սիմվոլ
}
```

Պարզ է, որ `scanner`-ը բառային վերլուծիչի ցուցիչն է։ Իսկ `lookahead`-ը՝ վերը բերված փսևդոկոդում հանդիպող _⟨հերթական լեքսեմ⟩_-ն է։ _lookahead_, այն է՝ _առաջ նայող_, _կանխատեսող_ սիմվոլն ամբողջ շարահյուսական վերլուծիչում օգտագործվում է որոշում կայացնելու հահար. դրանով է որոշվում, թե վերլուծությունն ինչ ճյուղով պետք է ընթանա։

`lookahead`-ի հետ աշխատելու համար սահմանված են `Parser`-ի `next()`, `has()` և `match()` մեթոդները։ Առաջինը պարզապես բառային վերլուծիչից կարդում է նոր արժեք։

```Go
func (p *Parser) next() { p.lookahead = p.scanner.next() }
```

Երկրորդը վարիադիկ մեթոդ է, որը պատասխանում է, թե արդյոք `lookahead`-ի պիտակը համապատասխանում է արգումենտում տրվածներից որևէ մեկին։

```Go
func (p *Parser) has(tokens ...int) bool {
	return p.lookahead.is(tokens...)
}
```

Մյուսն արդեն հիշատակված `match()`-ն է, որը ստուգում է, որ `lookahead`-ի պիտակը համապատասխանի տրված պիտակին։ Եթե այդպես է, ապա բառային վերլուծիչից կարդում է հաջորդ լեքսեմը ու վերադարձնում է առկա լեքսեմի արժեքը, իսկ հակառակ դեպքում գեներացնում է սխալ։ Ըստ էության, սա վերլուծության տարրական քայլն ապահովող գործողությունն է, որին հանդիպելու ենք վերլուծիչի համարյա բոլոր մեթոդներում։

```Go
func (p *Parser) match(exp int) (string, error) {
	if p.has(exp) {
		value := p.lookahead.value
		p.next()
		return value, nil
	}

	return "", fmt.Errorf("տող %d. Վերլուծության սխալ", p.lookahead.line)
}
```

Կոդում օգտագործելու հարմարության համար `match()`-ը վերադարձնում է նաև հաստատված լեքսեմի արժեքը. դրա կիրառությունը կտեսնենք ստորև, օրինակ, իդենտիֆիկատորների հետ աշխատելիս։

> Ինչպես արդեն հասկացանք, lookahead մեխանիզմը թույլ է տալիս վերլուծել քերականություններ, որտեղ վերլուծության ուղղությունը որոշելու համար բավական է դիտարկել միայն մեկ սիմվոլ։ Այդպիսի քերականությունները ունեն են LL(1) անունը: Տես, օրինակ, _Introduction to Automata Theory, Languages, and Computation_:

`Parser`-ի նմուշը ստեղծվում է `New()` ֆունկցիայով, որն իրա արգումենտում ստացված ֆուֆերացված հոսքի ցուցիչն օգտագործելով ստեղծում է `scanner`-ի նմուշ ու այն կապում նոր ստեղծված `Parser`֊ի `scanner` դաշտին։ 

```Go
func New(reader *bufio.Reader) *Parser {
	return &Parser{&scanner{reader, -1, "", 1}, nil}
}
```

Բոլոր վերլուծիչ-պրոցեդուրաները սահմանված են որպես `Parser` ստրուկտուրայի մեթոդներ։ Այդ մեթոդները հիմնականում վերադարձնում են երկու արժեք. AST-ի համապատասխան ենթածառի ցուցիչը և սխալի `error` տիպի արժեքը։ Բնականաբար, եթե պրոցեդուրան հաջողությաբ վերլուծել է իրեն առաջադրված կանոնը, ապա `error`-ը `nil` է։ 

Արտաքին (exported) միակ `Parse()` մեթոդը երկու տող է. այն առաջին անգամ արժեքավորում է `lookahead`-ը, ապա վերադարձնում է `parseProgram()` երկու արժեքները։

```Go
func (p *Parser) Parse() (*ast.Program, error) {
	p.next()
	return p.parseProgram()
}
```

Մինչև առաջ անցնելը ևս մի անգամ կրկնենք. վերլուծող մեթոդների անունները համապատասխանեցված են շարահյուսական հավասարման ձախ կողմի ոչ տերմինալային սիմվոլի հետ, իսկ մեթոդի տրամաբանությունը համապատասխանում է հաավասարման աջ կողմի կառուցվածքին, ինչպես արդեն քննարկեցինք վերևում։ Կանոնը բավականին պարզ է. վերցնում ենք քերականական հավասարման ձախ կողմի ոչ տերմինալային սիմվոլն ու սահմանում ենք մեթոդ այդ անունով (բնականաբար, պարտադիր չէ, որ անունը նույնությամբ համընկնի, սա պայմանականություն է), ապա վերցնում ենք հավասարման աջ կողմի արտահայտությունն ու, ձախից աջ անցնելով, եթե տարրը տերմինալային է, ապա `match()` մեթոդով համոզվում ենք, որ այն մեր սպասածն է ու անցնում ենք առաջ, եթե տարրը ոչ տերմինալային է, ապա կանչում ենք համապատասխան մեթոդը։ Եթե օգտագործված է `[...]` կառուցվածքը (ոչ պարտադիր), ապա բլոկը վերցնում ենք պայմանի տակ։ Եթե օգտագործված է `{...}` կառուցվածքը (կրկնություն), ապա բլոկը վերցնում ենք ցիկլի մեջ։


### Ծրագրի վերլուծությունը

Բալ ծրագրի վերլուծության `parseProgram()` մեթոդը իրականացնում է մի փոքր տարբերվող շարահյուսական կանոնի վերկուծություն, ի տարբերություն վերը հիշատակված օրինակի։ Այն է.

```
Program = [NewLines] { Subroutine NewLines }.
```

Այս կանոնն «ասում է», որ ծրագիրը կարող է սկսվել _ոչ պարտադիր_ դատարկ տողերով (կամ մեկնաբանություններո, որոնք զտվել են բառային վերլուծիչում), և ամեն մի ենթածրագրի (Subroutine) _պարտադիր_ պետք է հաջորդեն նոր տողի նիշեր։

`parseProgram()`-ը ամենասկզբում ստուգում է նոր տողի անցման նիշի առկայությունը և `parseNewLines()`-ով վերլուծում է դրանք։

```Go
func (p *Parser) parseProgram() (*ast.Program, error) {
	// բաց թողնել ֆայլի սկզբի դատարկ տողերը
	if p.has(xNewLine) {
		p.parseNewLines()
	}
```

Այնուհետև սահմանվում է `subroutines` բառարանը, որում համապատասխանության մեջ են դրվելու ենթածրագրի անունն ու ենթածառը։ (Այս արտապատկերումն օգնելու է ինտերպրետացիայի ժամանակ ենթածրագրի օգտագործումը հեշտացնելու համար։)

```Go
	// վերլուծվելիք ենթածրագրերի ցուցակ
	subroutines := make(map[string]*ast.Subroutine)
```

Հետո վերլուծում ենք `{ Subroutine NewLines }` ենթաարտահայտությունը։ Քանի դեռ «տեսնում ենք» `xSubroutine` պիտակը, փորձում ենք վերլուծել հերթական ենթածրագիրը՝ `parseSubroutine()`-ի կանչով։ Եթե այդ կանչը ձախողվում է, ապա վերադարձնում ենք սխալի արժեքը։ Եթե հաջողվում է՝ վերլուծված ենթածրագրի ծառն ավելացնում ենք `subroutines` բառարանում։

```Go
	for p.has(xSubroutine) {
		subr, err := p.parseSubroutine()
		if err != nil {
			return nil, err
		}
		subroutines[subr.Name] = subr
		p.parseNewLines()
	}
```

Վերջում ստեղծվում ու վերադարձվում է `ast.Program` տիպի հանգույցը, բնականաբար, սխալի՝ `nil` արժեքի հետ միասին։

```Go
	return &ast.Program{Subroutines: subroutines}, nil
}
```

Նոր տողերի անցման նիշերի հաջորդության վերլուծությունն էլի իրականացված է `NewLines = NEWLINE { NEWLINE }.` կանոնի ուղղակի ծրագրավորմամբ։

```Go
func (p *Parser) parseNewLines() {
	p.match(xNewLine)
	for p.lookahead.is(xNewLine) {
		p.next()
	}
}
```


### Ենթածրագրի վերլուծությունը

Ենթածրագրի վերլուծության `passeSubroutine()` մեթոդն էլ մանրամասն կքննարկենք. նախ՝ պարզապես ձևավորված գիտելիքի ամրապնդման համար, և հետո՝ հիմնականում նույն հնարքներն ենք օգտագործելու բոլոր վերլուծիչներում:

Հիշենք քերականական կանոնը.

```
Subroutine = 'SUB' IDENT ['(' IdentList ')'] Sequence 'END' SUB'.
```

Արդ, սահմանում ենք `parseSubroutine` մեթոդը՝ դեռ բաց թողնելով AST-ի կառուցման և սխալներին արձագանքման հատվածները։

```Go
func (p *Parser) parseSubroutine() {
	...
```

Ծրագրավորենք ենթածրագրի վերնագիրը՝ `'SUB' IDENT ['(' [IdentList] ')']`։ Այստեղ `SUB`-ը և `IDENT`-ը տերմինալներ են, իսկ պարամետրերի ցուցակը պարտադիր չէ։ Ուրեմն.

```Go
	...
	// վերնագրի վերլուծություն
	p.match(xSub)
	p.match(xIdent)

	if p.has(xLeftPar) {
		p.match(xLeftPar) // '('
		if p.has(xIdent) {
		  p.parseIdentList()
		}
		p.match(xRightPar) // ')
	}
	...
```

Այնուհետ՝ ենթածրագրի մարմինը. `Sequence 'END' SUB'`, որտեղ `Sequence`-ը ոչ տերմինալային է. կանչում ենք դրա մեթոդը, իսկ `END`-ը և `SUB`-ը տերմինալային են. `match`-ով համոզվում ենք ու անցնում առաջ։

```Go
	...
	// մարմնի վերլուծություն
	p.parseSequence()

	p.match(xEnd)
	p.match(xSubroutine)
}
```

Սխեման պարզ է։ Այնուամենայնիվ, ևս մի անգամ ընդգծենք, որ `parseSubroutine` մեթոդի այս սահմանումը տառացիորեն համապատասխանում է քերականական հավասարմանը։

Ուրեմն, վերադառնանք ենթածրագրի վերլուծության `parseSubroutine()` մեթոդին ու արդեն գրենք այն լրիվ տեսքով՝ կառուցելով ենթածրագիր AST-ը։ 

```go
func (p *Parser) parseSubroutine() (*ast.Subroutine, error) {
	// վերնագրի վերլուծություն
	p.next() // SUB
	name, err := p.match(xIdent)
	if err != nil {
		return nil, err
	}

	// պարամետրեր
	var parameters []string
	if p.has(xLeftPar) {
		p.next() // '('
		if p.has(xIdent) {
			parameters, err = p.parseIdentList()
			if err != nil {
				return nil, err
			}
		}
		if _, err := p.match(xRightPar); err != nil {
			return nil, err
		}
	}

	// մարմնի վերլուծություն
	body, err := p.parseSequence()
	if err != nil {
		return nil, err
	}

	if _, err := p.match(xEnd); err != nil {
		return nil, err
	}
	if _, err := p.match(xSubroutine); err != nil {
		return nil, err
	}

	// նոր ենթածրագրի օբյեկտ
	return &ast.Subroutine{
		Name: name, 
		Parameters: parameters, 
		Body: body,
	}, nil
}
```

Այս ենթածրագրի սկզբում տեսնում ենք `match()`-ի այն կիրառությունը, որտեղ օգտագործվում է ոչ միայն սխալի արժեքը, այլ նաև լեքսեմի արժեքը. `name, err := p.match(xIdent)`։ Սա կարելի է կարդալ այսպես. «եթե ⟨հերթական լեքսեմ⟩-ն ունի `xIdent` պիտակը, ապա `name`-ը թող ստանա այդ լեքսեմի արժեքը, հակառակ դեպքում, `err`-ը թող լինի այդ անհամապատասխանությունն արտացոլող սխալի օբյեկտը»։

Իդենտիֆիկատորների ցուցակը, որտեղ տարրերն իրարից անջատված են ստորակետով, վերլուծվում է `parseIdentList` մեթոդով։ Այս մեթոդը AST հանգույց չի ստեղծում, պարզապես վերադարձնում է վերլուծված իդենտիֆիկատորների զանգվածը։

```Go
func (p *Parser) parseIdentList() ([]string, error) {
	identifiers := make([]string, 0)

	value, err := p.match(xIdent)
	if err != nil {
		return nil, err
	}
	identifiers = append(identifiers, value)

	for p.has(xComma) {
		p.next() // ','
		value, err = p.match(xIdent)
		if err != nil {
			return nil, err
		}
		identifiers = append(identifiers, value)
	}

	return identifiers, nil
}
```

Հրամանների հաջորդականությունը, որով ձևավորվում են ենթածրագրի ու առանձին ղեկավարող կառուցվածքների մարմինները, սահմանվում են հետևյալ հավասարմամբ։

```
Sequence = NewLines { Statement NewLines }.
```

`parseSequence()` մեթոդն իր սխեմայով շատ նման է իդենտիֆիկատորների կամ արտահայտություների (տե՛ս ստորև) ցուցակ վերլուծող պրոցեդորաներին։ Սա վերադարձնում է `ast.Sequence` հանգույց, որը պարունակում է վերլուծված հրամանների ենթածառերը։

```Go
func (p *Parser) parseSequence() (*ast.Sequence, error) {
	p.parseNewLines()

	statements := make([]ast.Statement, 0)
	for p.isStatementFirst() {
		var stat, err = p.parseStatement()
		if err != nil {
			return nil, err
		}

		p.parseNewLines()
		statements = append(statements, stat)
	}

	return &ast.Sequence{Items: statements}, nil
}
```

Հրամանների շարքի վերլուծության `for` ցիկլի պայմանում օգտագործված `isStatementFirst` մեթոդը պարզապես ստուգում է, որ հերթական դիտարկվող թոքենը (lookahead-ը) լինի _FIRST(Statement)_ բազմությունից։ _FIRST(Statement)_-ը այն տերմինալային սիմվոլների բազմությունն է, որի տարրերից որևէ մեկով կարող է հրաման սկսվել։ Բալ լեզվի հրամանների դեպքում.

_FIRST(Statement) ≡ { DIM, LET, INPUT, PRINT, IF, WHILE, FOR, CALL }_


### Հրամանների վերլուծությունը

Նախորդ `parseStatement` մեթոդը՝ կողմնորոշվելով `lookahead`-ի ընթացիկ արժեքով, ճյուղավորում է վերլուծության ընթացքը։ Այսպես.

```Go
func (p *Parser) parseStatement() (ast.Statement, error) {
	switch {
	case p.has(xDim):
		return p.parseDim()
	case p.has(xLet):
		return p.parseLet()
	case p.has(xInput):
		return p.parseInput()
	case p.has(xPrint):
		return p.parsePrint()
	case p.has(xIf):
		return p.parseIf()
	case p.has(xWhile):
		return p.parseWhile()
	case p.has(xFor):
		return p.parseFor()
	case p.has(xCall):
		return p.parseCall()
	}

	return nil, fmt.Errorf("սպասվում է հրամանի սկիզբ. DIM, LET, INPUT, PRINT, IF, WHILE, FOR, CALL, բայց հանդիպել է %s", p.lookahead.value)
}
```

Առաջինը միչափ զանգվածը (աում են նաև _վեկտոր_) սահմանող `DIM` հրամանն է։ Սրա քերականական կանոնն է․

```
Statement = 'DIM' IDENT '[' Expression ']'.
```

Ուրեմն, պետք է կարդալ `DIM` ծառայողական բառը, ապա զանգվածի անունը որոշող իդենտիֆիկատորը, հետո էլ՝ զանգվածի տարրերի քանակը ցույց տվող արտահայտությունը՝ առնված `[` և `]` փակագծերի մեջ։ `parseDim()` մեթոդը վերադարձնում է `ast.Dim` հանգույցի ցուցիչ։ 

```go
func (p *Parser) parseDim() (ast.Statement, error) {
	p.next() // DIM
	name, err := p.match(xIdent)
	if err != nil {
		return nil, err
	}
	if _, err := p.match(xLeftBr); err != nil {
		return nil, err
	}
	size, err := p.parseExpression()
	if err != nil {
		return nil, err
	}
	if _, err := p.match(xRightBr); err != nil {
		return nil, err
	}
	return &ast.Dim{Name: name, Size: size}, nil
}
```

Վերագրման `LET` հրամանը փոփոխականին կամ զանգվածի տարրին նոր արժեք նշանակող հրամանն է։ Այս մեթոդում իրականացված է `LET`-ի երկու տարբերակների վերլուծությունը․ սովորական, երբ փոփոխականին է արժեք վերագրվում, և զանգվածի տարրին արժեք նշանակելու դեպքում։

```Go
func (p *Parser) parseLet() (ast.Statement, error) {
	p.next() // LET
	name, err := p.match(xIdent)
	if err != nil {
		return nil, err
	}
	var place ast.Expression = &ast.Variable{Name: name}

	if p.has(xLeftBr) {
		p.next() // '['
		index, err := p.parseExpression()
		if err != nil {
			return nil, err
		}
		if _, err := p.match(xRightBr); err != nil { // ']'
			return nil, err
		}
		place = &ast.Binary{Operation: "[]", Left: place, Right: index}
	}

	if _, err := p.match(xEq); err != nil {
		return nil, err
	}

	assignable, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	return &ast.Let{Place: place, Value: assignable}, nil
}
```

Ներմուծման հրամանը երկու տարր է․ `INPUT` բառը և փոփոխականը, որին պետք է վերագրվի ներմուծված արժեքը։

```Go
func (p *Parser) parseInput() (ast.Statement, error) {
	p.next() // INPUT
	name, err := p.match(xIdent)
	if err != nil {
		return nil, err
	}

	return &ast.Input{Place: &ast.Variable{Name: name}}, nil
}
```

Արտածման հրամանը, կարելի է ասել, սիմետրիկ է ներմուծման հրամանին․ այսպեղ `PRINT` բառն է և արտածվող արտահայտությունը։

```Go
func (p *Parser) parsePrint() (ast.Statement, error) {
	p.next() // PRINT
	value, err := p.parseExpression()
	if err != nil {
		return nil, err
	}
	return &ast.Print{Value: value}, nil
}
```

Ճյուղավորման `IF` հրամանի վերլուծիչը, թերևս, մեր բոլոր վերլուծող ենթածրագրերից ամենաբարդն է (չնայած, ուշադիր կարդալու դեպքում, դրան «բարդ» ասելը մի փոքր չափազանցություն կլինի)։ Հիշենք քերականական կանոնը.

```
Statement = 'IF' Expression 'THEN' Sequence
               { 'ELSEIF' Expression 'THEN' Sequence } 
			   [ 'ELSE' Sequence ]
            'END' 'IF'.
```

Վերլուծող `parseIf()` մեթոդն ունի երեք տրամաբանական մաս։ Նախ վերլուծվում է առաջին, հիմնական պայմանն ու դրան համապատասխան `THEN` բլոկը։

```Go
func (p *Parser) parseIf() (ast.Statement, error) {
	p.next() // IF
	cond, err := p.parseExpression()
	if err != nil {
		return nil, err
	}
	if _, err := p.match(xThen); err != nil {
		return nil, err
	}
	decision, err := p.parseSequence()
	if err != nil {
		return nil, err
	}
	result := &ast.If{Condition: cond, Decision: decision}
```

Այս առաջին հատվածի համար ստեղծվում է `ast.If` հանգույց, որում արժեքվորվում են միայն `Condition` և `Decision` դաշտերը։

Այնուհետև վերլուծվում են `ELSEIF` ճյուղերն իրենց `THEN` բլոկներով։ Այս հատվածի վերլուծությունից ստացված ենթածառը կապվում է նախորդ հատվածում ստեղծված `ast.If` հանգույցի `Alternative` դաշտին։ `ipe` ցուցիչը «սահում» է վերլուծված `ELSEIF` հանգույցների համար ստեղծված `ast.If` օբյեկտների `Alternative` դաշտերով՝ դրանով իսկ ստեղծելով `ast.If` տիպի հանգույցների մի կապակցված ցուցակ։

```Go
	ipe := result
	for p.has(xElseIf) {
		p.next() // ELSEIF
		cond, err := p.parseExpression()
		if err != nil {
			return nil, err
		}
		if _, err := p.match(xThen); err != nil {
			return nil, err
		}
		decision, err := p.parseSequence()
		if err != nil {
			return nil, err
		}
		alternative := &ast.If{Condition: cond, Decision: decision}
		ipe.Alternative = alternative
		ipe = alternative
	}
}
```

Վերջում, եթե հանդիպել է `xElse` պիտակը, վերլուծվում է `ELSE` բլոկը, և ստեղծված ենթածառը կապվում է ամենավերջին `ast.If` հանգույցի `Alternative` դաշտին։

```Go
	if p.has(xElse) {
		p.next() // ELSE
		alternative, err := p.parseSequence()
		if err != nil {
			return nil, err
		}
		ipe.Alternative = alternative
	}
```

Ճյուղավորման կառուցվածքի վերլուծությունն ավարտվում է `END` և `IF` ծառայողական բառերի ակնկալիքով։

```Go
	if _, err := p.match(xEnd); err != nil {
		return nil, err
	}
	if _, err := p.match(xIf); err != nil {
		return nil, err
	}

	return result, nil
}
```

Նախապայմանով `WHILE` ցիկլը վերլուծող մեթոդը մանրամասն նկարագրության կարիք չունի. պարզապես հաջորդաբար վերլուծվում են քերականական հավասարման աջ մասի անդամները։ Արդյունքում ստեղծվում է `ast.While` տիպի հանգույց։

```Go
func (p *Parser) parseWhile() (ast.Statement, error) {
	p.next() // WHILE
	condition, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	body, err := p.parseSequence()
	if err != nil {
		return nil, err
	}

	if _, err := p.match(xEnd); err != nil {
		return nil, err
	}
	if _, err := p.match(xWhile); err != nil {
		return nil, err
	}

	return &ast.While{Condition: condition, Body: body}, err
}
```

Պարամետրով `FOR` ցիկլի վերլուծությունը, չնայած որ ամենաերկարն է, բայց համարյա գծային է՝ վերլուծվում են իրար հաջորդող քերականական տարրերը։ Հիշենք քերականական հավասարումը.

```
Statement = 'FOR' IDENT '=' Expression 'TO' Expression ['STEP' ['+'|'-'] NUMBER] Sequence 'END' 'FOR'.
```

Նախ վերլուծվում է ցիկլի հաշվիչի սկզբնական ու վերջնական արժեքները որոշող կտորը.

```Go
func (p *Parser) parseFor() (ast.Statement, error) {
	p.next() // FOR
	name, err := p.match(xIdent)
	if err != nil {
		return nil, err
	}
	param := &ast.Variable{Name: name}
	if _, err := p.match(xEq); err != nil {
		return nil, err
	}
	begin, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	if _, err := p.match(xTo); err != nil {
		return nil, err
	}
	end, err := p.parseExpression()
	if err != nil {
		return nil, err
	}
```

Այս կտորի արդյունքում ստանում ենք `name`-ը պարամետրի անունի համար, `begin`-ը՝ պարամետրի սկզբնական արժեքի համար և `end`-ը՝ վերջնական արժեքի համար։

Հետո, եթե հանդիպել է `xStep` պիտակով լեքսեմ, վերլուծում ենք `+` կամ `-` ոչ պարտադի նշանով սկսվող թիվ, որը պետք է ծառայի որպես ցիկլի հաշվիչի արժեքի փոփոխմա արժեք։

```Go
	var step ast.Expression
	if p.has(xStep) {
		p.next() // STEP
		sign := "+"
		if p.has(xSub) {
			p.next() // '-'
			sign = "-"
		} else if p.has(xAdd) {
			p.next() // '+'
		}

		lex, err := p.match(xNumber)
		if err != nil {
			return nil, err
		}
		num, _ := strconv.ParseFloat(lex, 64)
		step = &ast.Number{Value: num}
		if sign == "-" {
			step = &ast.Unary{Operation: sign, Right: step}
		}
	} else {
		step = &ast.Number{Value: 1.0}
	}
```

Եթե `STEP` հատվածը բացակայում է, ապա `step`-ին կապում ենք `1.0` արժեքով `ast.Number` հանգույց։


Հետո վերլուծում ենք ցիկլի մարմինը կազմող հրամանների հաջորդականությունն ու կապում `body` փոփոխականին։

```Go
	body, err := p.parseSequence()
	if err != nil {
		return nil, err
	}
```

Ամենավերջում համոզվում ենք, որ `FOR` ցիկլի սահմանումն ավարտվում է `END` և `FOR` բառերով ու վերադարձնում ենք `param`, `begin`, `end`, `step` և `body` ենթածառերով արժեքավորված `ast.For` հանգույց։

```Go
	if _, err := p.match(xEnd); err != nil {
		return nil, err
	}
	if _, err := p.match(xFor); err != nil {
		return nil, err
	}

	return &ast.For{
		Parameter: param,
		Begin:     begin,
		End:       end,
		Step:      step,
		Body:      body}, nil
}
```

Հրամաններից վերջինը ենթածրագրի կանչի `CALL` հրամանն է։ Լեզվի նկարագրության մեջ արդեն նկարագրեն ենք սրա տեսքն ու վարքը, իսկ վերլուծությունը նորից գծային է. `CALL` բառը, ենթածրագրի անունը և արգումենտների ցուցակը, որը կարող է բացակայել։ `parseCall` մեթոդը կառուցում է `ast.Call` տիպի հանգույց։

```Go
func (p *Parser) parseCall() (ast.Statement, error) {
	p.next() // CALL
	name, err := p.match(xIdent)
	if err != nil {
		return nil, err
	}

	arguments, err := p.parseExpressionList()
	if err != nil {
		return nil, err
	}

	return &ast.Call{Callee: name, Arguments: arguments}, nil
}
```

Սրանով ավարտեցինք հրամանների (կամ ղեկավարող կառուցվածքների) վերլուծիչների իրականացումները։ Հաջորդիվ ուսումնասիրելու ենք արտահայտությունների վերլուծությունը, բայց մինչև դրանց անցնելը, նշենք `parseExpressionList()` մեթոդի մասին։ Սա վերլուծում է ստորակետով իրարից անջատված արտահայտությունների ցուցակ ու վերադարձնում է այդ արտահայտությունների ենթածառերի զանգվածը։ 

```Go
func (p *Parser) parseExpressionList() ([]ast.Expression, error) {
	elements := make([]ast.Expression, 0)
	
	// իրականացման սխեման նույնական է parseIdentList()-ին
	
	return elements, nil
}
```


### Արտահայտությունների վերլուծությունը

Լեզվի նկարագրության գլխում խոսեցինք, թե ինչպե՛ս են արտահայտությունները բաժանվված մակարդակների՝ դրանց նախապատվությունը բնականորեն, մաթեմատիկորեն ապահովելու համար։ Վերլուծության մասում այդ հավասարությունները «թարգմանելու» ենք Գո կոդի (ինչպես արեցինք հրամանների՝ ղեկավարող կառուցվածքների համար)։

Սկսենք նախաապատվությունների ամենացածր մակարդակում գտնվող տրամաբանական գործողություններից։ Սրանք երկուսն են. __ԿԱՄ__ ու __ԵՎ__, որոնցից __ԵՎ__-ը, լինելով մուլտիպլիկատիվ գործողություն, ավելի բարձր նախապատվություն ունի քան __ԿԱՄ__-ը։ Ուրեմն, ինչպես և սահմանված է քերականությամբ, դիզյունքցիան `OR` գործողությամբ իրար կապված կոնյունքցիաների շղթա է.

```
Expression = Conjunction { 'OR' Conjunction }.
```

Վերլուծիչն էլ, արդեն զարմանալի չէ, ուղղակիորեն արտահայտում է այս հավասարումը. վերլուծել կոնյունքցիա, ապա, քանի դեռ տեսնում ենք `xOr` պիտակը, վերլուծել ևս մի կոնյունքցիա, ու այդ երկուսն իրար կապել `ast.Binary` հանգույցով։

```Go
func (p *Parser) parseExpression() (ast.Expression, error) {
	result, err := p.parseConjunction()
	if err != nil {
		return nil, err
	}

	for p.has(xOr) {
		p.next() // OR
		right, err := p.parseConjunction()
		if err != nil {
			return nil, err
		}
		result = &ast.Binary{Operation: "OR", Left: result, Right: right}
	}

	return result, nil
}
```

Արդյունքում, եթե, օրինակ, վերլուծենք `TRUE OR FALSE OR TRUE` շղթան, ապա կկառուցվի դեպի _ձախ_ աճող ծառ.

![արտահայտության ծառ](expression-trees-left.png)

Լրիվ նույն կաղապարով են կառուցված բոլոր ձախ-զուգորդական երկտեղանի գործողությունների վերլուծիչները։ Դուրս բերենք կաղապարը փսևդոկոդի տեսքով.

```Go
func (p *Parser) parseBinary() (*ast.Binary, error) {
	result, _ := վերլուծել հաջորդ բարձր նախապատվության մակարդակը

    for տեսնում ենք այս մակարդակի գործողություններից որևէ մեկը {
       op := վերցնել գործողությունը
       right, _ := վերլուծել հաջորդ բարձր նախապատվության մակարդակը
       result := result-ից և right-ից կառուցել ast.Binary հանգույց
	}

    return result, nil
}
```

Ասվածն ամրապնդելու համար քննարկենք `parseAddtion()` մեթոդը, որը վերլուծում է թվաբանական ադիտիվ `+` և `-` գործողություններն ու տողերի միակցման `&` գործողությունը։ Նախապատվությունների ցուցակում ադիտիվներից մի մակարդակ բարձ են թվաբանական մուլտիպլիկատիվ `*`, `/` և `\` գործողությունները, որոնց համար, նորից օգտագործելով `parseBinary` սխեման, իրականացված է `parseMultiplication()` մեթոդը։ Ուրեմն.

```Go
func (p *Parser) parseAddition() (ast.Expression, error) {
	// վերլուծել հաջորդ բարձր նախապատվության մակարդակը
	result, err := p.parseMultiplication()
	if err != nil {
		return nil, err
	}

	// քանի դեռ տեսնում ենք այս մակարդակի գործողություններից որևէ մեկը
	for p.has(xAdd, xSub, xAmp) {
		// վերցնել գործողությունը
		opc := operation[p.lookahead.token]
		p.next() // '+', '-', '&'
		// վերլուծել հաջորդ բարձր նախապատվության մակարդակը
		right, err := p.parseMultiplication()
		if err != nil {
			return nil, err
		}
		// կառուցել Binary հանգույց
		result = &ast.Binary{Operation: opc, Left: result, Right: right}
	}

	// վերադարձնել կառուցված ենթածառի արմատը
	return result, nil
}
```

Այս մեթոդում գործողության պիտակից, օրինակ՝ `xAdd`, գործողության տեքստային ներկայացումը ստանալու համար օգտագործված է `opration` արտապատկերումը՝ սահմանված հետևյալ կերպ.

```Go
var operation = map[int]string{
	xAdd: "+",
	xSub: "-",
	xAmp: "&",
	xMul: "*",
	xDiv: "/",
	xMod: "\\",
	xPow: "^",
	xEq:  "=",
	xNe:  "<>",
	xGt:  ">",
	xGe:  ">=",
	xLt:  "<",
	xLe:  "<=",
	xAnd: "AND",
	xOr:  "OR",
	xNot: "NOT",
}
```

Առաջ գնալու համար նորից նշենք, որ նույն սխեմայով են կառուցված `parseConjunction()`, `parseMultiplication()` մեթոդները։

Նախապատվությունների աղյուսակում տրամաբանական գործողություններին հաջորդող երկու տողերում կանգնած են համեմատման գործողությունները, որոցից `=`-ը և `<>`-ն ունեն ավելի ցածր նախապատվություն, քան `<`, `<=`, `>` և `>`։ Համեմատումները նաև այլ բինար գործողություններից տարբերվում են նրանով, որ դրանցով շղթաներ չեն կազմվում՝ զուգորդական չեն։ Այդ պատճառով էլ `parseEquality()` և `parseComparison()` մեթոդներում `parseBinary` սխեմայի `for` կրկնության փոխարեն օգտագործված է պայմանական `if`-ը։ 

Այստեղ բերենք միայն `parseEquality()` մեթոդի իրականացումը. `parseComparison()`-ը նույնական է, միայն այն տարբերությամբ, որ ավելի բարձր մակարդակի համար կանչում է `parseAddition()`-ը։

```Go
func (p *Parser) parseEquality() (ast.Expression, error) {
	res, err := p.parseComparison()
	if err != nil {
		return nil, err
	}

	if p.has(xEq, xNe) {
		opc := operation[p.lookahead.token]
		p.next() // '=', '<>'
		right, err := p.parseComparison()
		if err != nil {
			return nil, err
		}
		res = &ast.Binary{Operation: opc, Left: res, Right: right}
	}

	return res, nil
}
```

Նկատենք, որ մակարդակների այսպիսի տրոհումն արգելում է կազմել `a = b <> c = d` տիպի շղթաներ. հենց սա է նշանակում «չզուգորդվող», բայց թույլատրում է գրել `a > b = c <= d` արտահայտությունը, որի համար կստեղծվի այսպիսի մի ծառ.

![համեմատությունների համեմատում](expression-comparison.png)

Աստիճան բարձրացնելու երկտեղանի գործողությունը մյուսներից տարբերվում է իր աջ-զուգորդականությամբ։ Այսինքն, եթե գրում ենք աստիճանների շղթա `x^y^z`, ապա ակնկալում ենք, որ այն պետք է հաշվարկվի `x^(y^z)` կանոնով։ `parsePower()` մեթոդի իրականացման մեջ դա ստանում ենք `^` գործողության աջ կողմում `parsePower()`-ի ռեկուրսիվ կանչով։

```Go
func (p *Parser) parsePower() (ast.Expression, error) {
	res, err := p.parseUnary()
	if err != nil {
		return nil, err
	}

	if p.has(xPow) {
		p.next() // '^'
		right, err := p.parsePower()
		if err != nil {
			return nil, err
		}
		res = &ast.Binary{Operation: "^", Left: res, Right: right}
	}

	return res, nil
}
```

Արդյունքում, երբ կվերլուծվի վերը բերված `x^y^z` օրինակը, կկառուցվի դեպի աջ աճող ծառ։

![աստիճան բարձրացնելու ծառը](expression-power.png)

Աստիճանի գործողությանը նախապատվությամբ հաջորդում են միտեղանի (ունար) գործողությունները՝ `+`, `-`, `NOT`: Դրանք բոլորն էլ, բնականաբար, աջ֊զուգորդվող են. `-+-5` արտահայտությունը հաշվարկվում է `-(+(-5))` կանոնով։ 

Վերլուծելիս նախ՝ ցիկլում կարդում ենք բոլոր գործողություններն ու հակառակ կարգով հավաքում `ops` ցուցակում։

```Go
func (p *Parser) parseUnary() (ast.Expression, error) {
	var ops []string
	for p.has(xAdd, xSub, xNot) {
		opc := operation[p.lookahead.token]
		p.next() // '+', '-', NOT
		ops = slices.Insert(ops, 0, opc)
	}
```

Հետո վերլուծում ենք ենթաարտահայտությունը, որին կիրառված է վերջին գործողությունը.

```Go
	right, err := p.parseSubscript()
	if err != nil {
		return nil, err
	}
```

Ապա, նորից անցնելով գործողությունների `ops` ցուցկով, կառուցում ենք `ast.Unary` հանգույցների շղթա։

```Go
	for _, opc := range ops {
		right = &ast.Unary{Operation: opc, Right: right}
	}

	return right, nil
}
```

Զանգվածի տարրին դիմելու, ինդեքսավորման `[]` գործողությունը իրականացրել ենք որպես երկտեղանի գործողություն, որի ձախ կողմում պետք է լինի պարզ արտահայտություն, իսկ աջ կողմում ընդհանուր արտահայտություն։

```
Subscript = Factor { '[' Expression ']' }.
```

Ձախ կողմի արտահայտության հաշվավարկի արդյունքը պետք է լինի զանգված, իսկ աջ կողմի արտահայտությանը՝ թիվ, որպես ինդեքս օգգտագործելու համար։

```Go
func (p *Parser) parseSubscript() (ast.Expression, error) {
	res, err := p.parseFactor()
	if err != nil {
		return nil, err
	}

	for p.has(xLeftBr) {
		p.next() // '['
		right, err := p.parseExpression()
		if err != nil {
			return nil, err
		}
		if _, err = p.match(xRightBr); err != nil {
			return nil, err
		}
		res = &ast.Binary{Operation: "[]", Left: res, Right: right}
	}

	return res, nil
}
```

Վերջապես, հերթը հասավ արտահայտություններից ամենապարզերի վերլուծությանը, որոնց քերականության սահմանման մեջ տվել ենք _Factor_ ընդհանուր անունը։ Դրանք տրամաբանական, թվային, տեքստային ու զանգվածների լիտերալներն են, որոնց գալիս են լրացնելու փոփոխականները, ֆունկցիայի կիրառումը և խմբավորման փակագծերը։

```
Factor = TRUE | FALSE | NUMBER | TEXT | ArrayLiteral | IdentOrApply | Grouping.
```

Ըստ էության, սրանք նաև ամենաբարձր նախապատվություն ունեցող արտահայտություններն են։

Ահա վերին մակարդակի `parseFactor` մեթոդը, որը, `lookahead`-ի ընթացիկ արժեքով ուղղորդվելով, կանչում է համապատասխան պարզ արտահայտության վերլուծման մեթոդը։

```go
func (p *Parser) parseFactor() (ast.Expression, error) {
	switch {
	case p.has(xTrue, xFalse):
		return p.parseTrueOrFalse()
	case p.has(xNumber):
		return p.parseNumber()
	case p.has(xText):
		return p.parseText()
	case p.has(xLeftBr):
		return p.parseArrayLiteral()
	case p.has(xIdent):
		return p.parseIdentOrApply()
	case p.has(xLeftPar):
		return p.parseGrouping()
	default:
		return nil, fmt.Errorf("պարզագույն արտահայտության սխալ")
	}
}
```

Տրամանաբանական արժեքների երկու լիտերալները վերլուծվում են `parseTrueOrFalse` մեթոդով, որի արդյունքում կառուցվում է `ast.Boolean` տիպի հանգույց։

```go
func (p *Parser) parseTrueOrFalse() (ast.Expression, error) {
	lex, err := p.match(p.lookahead.token)
	if err != nil {
		return nil, err
	}

	return &ast.Boolean{Value: strings.ToUpper(lex) == "TRUE"}, nil
}
```

Երբ `lookahead`-ը պիտակված է `xNumber`-ով, ապա գրադարանային `strconv.ParseFloat` ֆունկցիայով լեքսեմը ձևափոխվում է թվային արժեքի, որով էլ արժեքավորվում է վերադարձվող `ast.Number` օբյեկտը։

```go
func (p *Parser) parseNumber() (ast.Expression, error) {
	lex := p.lookahead.value
	p.next() // NUMBER
	val, _ := strconv.ParseFloat(lex, 64)
	return &ast.Number{Value: val}, nil
}
```

Տեքստային լիտերալը վերլուծող `parseText` մեթոդը պարզապես վերցնում է լեքսեմի արժեքը ու ստեղծում `ast.Text` օբյեկտը։

```go
func (p *Parser) parseText() (ast.Expression, error) {
	val := p.lookahead.value
	p.next() // TEXT
	return &ast.Text{Value: val}, nil
}
```

Զանգվածի լիտերալը `[` և `]` փակագծերի մեջ պարփակված արտահայտությունների ցուցակ է, որի տարրերն իրարից անջատված են ստորակետով։

```
ArrayLiteral = '[' [ Expression { ',' Expression } ] ']'.
```

Երբ Factor-ի վերլուծության կետում `lookahead`-ը պիտակված է `xLeftBr`-ով, կանչվում է `parseArrayLiteral` մեթոդը։ Սա բած է թողնում բացվող `[` փակագիծը, ապա կանչում է արտահայտությունների ցուցակը վերլուծող `parseExpressionList` մեթոդը, վերջում էլ սպասում է փակվող `]` փակագծին։ Վերլուծված անդամներով արժեքավորվում է վերադարձվող `ast.Array` օբյեկտը։

```go
func (p *Parser) parseArrayLiteral() (ast.Expression, error) {
	p.next() // լիտերալի սկիզբը, '['

	// լիտերալի անդամները
	elements, err := p.parseExpressionList()
	if err != nil {
		return nil, err
	}

	// լիտերալի վերջը, ']'
	if _, err := p.match(xRightBr); err != nil {
		return nil, err
	}

	return &ast.Array{Elements: elements}, nil
}
```

Երբ Factor-ը վերլուծելիս հանդիպել ենք իդենտիֆիակատորի, ապա դա կարող է նշանակել երկու բան․ ա) փոփոխականի օգտագործում, կամ բ) ֆունկցիայի կիրառում։ Առաջին դեպքում իդենտիֆիկատորի լեքսեմով ստեղծվում է `ast.Variable` օբյեկտ, իսկ երկրորդ դեպքում՝ `ast.Apply` օբյեկտ։ Արդ, `parseIdentOrApply` մեթոդը առաջին հերթին կարդում է իդենտիֆիկատորի լեքսեմը, ապա ստուգում է՝ արդյոք հաջորդ նիշը բացվող փակագիծ է։ Եթե այո, ապա կանչում է ֆունկցիայի արգումենտների ցուցակը վերլուծող `parseExpressionList` մեթոդը, և ստեղծում `ast.Apply` օբյեկտ։ Հակառակ դեպքում՝ ստեղծում է `ast.Variable` օբյեկտ։

```go
func (p *Parser) parseIdentOrApply() (ast.Expression, error) {
	name := p.lookahead.value
	p.next() // IDENT

	if p.has(xLeftPar) {
		p.next() // '('

		arguments, err := p.parseExpressionList()
		if err != nil {
			return nil, err
		}

		if _, err := p.match(xRightPar); err != nil { // ')'
			return nil, err
		}

		return &ast.Apply{Callee: name, Arguments: arguments}, nil
	}

	return &ast.Variable{Name: name}, nil
}
```

Վերջապես, խմբավորման (կամ հաշվարկման նախապատվության բարձրացման) փակագծերի մեջ առնված արտահայտությունը վերլուծում ենք `parseGrouping` մեթոդով։ Այս մեթոդը նոր հանգույց չի կառուցում, պարզապես վերադարձնում է փակագծերի մեջ վերլուծված արտահայտության համար կառուցված ենթածառը։

```go
func (p *Parser) parseGrouping() (ast.Expression, error) {
	p.next() // '('

	expr, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	if _, err = p.match(xRightPar); err != nil {
		return nil, err
	}

	return expr, nil
}
```


## Սխալների հայտնաբերում և ազդարարում

Մեր իրականացրած վերլուծիչը, երբ հանդիպում է շարահյուսական սխալի, դադարեցում է վերլուծությունը ու վերադարձնում է `error` օբյեկտը՝ համապատասխան սխալի հաղորդագրությամբ։ Անշուշտ, սա շատ պարզ, և, մեծ ինտերպրետատորներում, կոմպիլյատորներում անհարմար մոտեցում է։ Եթե, օրինակ, ենթադրենք, որ ծրագրավորողը կոդում թույլ է տվել մի քանի սխալներ, ապա սրանք կհայտնաբերվեն ոչ թե վերլուծիչի մի անցումով, այլ՝ հերթականությամբ։ Ամեն մի հաջորդ սխալը վերլուծիչը կհայտնաբերի, երբ ծրագրավորողը կուղղի դրան նախորդողը։ Վերլուծության որ մակարդակում էլ որ կհանդիպի սխալը, այն, պղպջակի պես, վերլուծող մեթոդների երկրորդ վերադարձվող արժեքով «դուրս կլողա» դեպի վերև, մինչև `Parser.Parse()` մեթոդը կանչող ղեկավարող կոդը։

Ռեկուրսիվ վայրէջքի եղանակով իրականացված վերլուծիչներում, հատկապես, եթե դրանք իրականացվում են կոմպիլյատորների համար, շատ հարմար է, այսպես կոչված, _panic mode recovery_ (թարգմանենք սա. _խուճապի վիճակից վերականգնման_) եղանակը։ Էությունն այն է, որ երբ վերլուծիչը հանդիպում է սխալի, ապա, ոչ թե դադարեցնում է իր աշատանքը, այլ գրանցում է այդ սխալի տվյալները սխալների ցուցակում, նշում է կատարում, որ վերլուծիչն անցնում է «խուճապի վիճակին», կարդում ու դեն է նետում հաջորդ լեքսեմներն այնքան ժամանակ, մինչև կհանդիպի մի կետի, որտեղից վստահաբար կարելի է վերսկսել վերլուծությունը։ Նույնը շարունակվում է մինչև կվերլուծվի ծրագրի ամբողջ տեքստը։ Վերջում, եթե սխալների ցուցակը դատարկ է, ապա, ակնհայտորեն, ծրագրում ամեն ինչ ճիշտ է, կարելի է անցնել ինտերպրետացիային (կամ կոդի գեներացիային՝ կոմպիլյատորների դեպքում)։ Իսկ եթե ցուցակում սխալներ են գրանցվել, ապա դրանք արտածվում են ծրագրավորողի համար ու ինտերպրետատորը (կոմպիլյատորը) դադարեցնում է իր աշխատանքը։ (Նիկլաուս Վիրտի «Կոմպիլյատորների կառուցումը» գրքում, օրինակ, բավականին լավ նկարագրված ու ծրագրավորած է այս մոտեցումը։)

Մեր վերլուծիչի կոդում խուճապի վիճակից վերականգնման եղանակով սխալների հայտնաբերման ու ազդարարման համար պետք է մի քանի փոփոխություններ անել։ Առաջինը, ընդլայնենք `Parser` ստրուկտուրան երկու նոր դաշտերով. մեկը՝ սխալների ցուցակի համար, մյուսը՝ խուճապի վիճակը նշելու համար։

```Go
type Parser struct {
	scanner   *scanner // բառային վերլուծիչի ցուցիչ
	lookahead *lexeme  // look-a-head սիմվոլ
	errors    []error  // սխալների ցուցակ
	panicMode bool     // խուճակի վիճակի ցուցիչ
}
```

Հաջորդը՝ պետք է ձևափոխել `match()` մեթոդը, որը հայտնաբերած սխալը կգրանցի `errors` ցուցակում և, անհրաժեշտության դեպքում, կանջատի կամ կմիացնի խուճապի վիճակի ցուցիչը։ Օրինակ, այսպես.

```Go
func (p *Parser) match(exp int) (string, error) {
	if p.lookahead.is(exp) {
		value := p.lookahead.value
		p.next()
		// ամեն ինչ լավ է, դուրս ենք գալիս խուճապի վիճակից
		p.panicMode = false 
		return value, nil
	}

	// սխալ է, կառուցում ենք սխալի հաղորդագրությունը
	err := fmt.Errorf("Տող %d: սպասվում է %v, բայց հանդիպել է '%s'։", 
		p.lookahead.line, exp, p.lookahead.value)

	// եթե խուճապի վիճակում չենք, գրանցում ենք սխալը 
	// և անցում ենք խուճապի վիճակի
	if !p.panicMode {
		p.errors = append(p.errors, err)
		p.panicMode = true
	}

	return "", err
}
```

Հետո պետք է հարմարեցնել վերլուծիչ մեթոդները։ Օրինակ, `parseDim()`-ը կունենա հետևյալ նոր տեսքը.

```Go
func (p *Parser) parseDim() (ast.Statement, error) {
	p.next() // DIM
	name, _ := p.match(xIdent)
	p.match(xLeftBr)
	
	size, err := p.parseExpression()
	if err != nil {
		p.syncTo(xRightBr, xNewLine)
	}
	
	p.match(xRightBr)
	return &ast.Dim{Name: name, Size: size}, nil
}
```

```Go
func (p *Parser) syncTo(tokens ...int) {
	for !p.lookahead.is(xEOF) && !p.has(tokens...) {
		p.next()
	}
}
```
