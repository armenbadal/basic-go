# Ինտերպրետացիա

Հասել ենք այն կետն, երբ շարահյուսական վերլուծիչն առանց սխալների ավարտել է ծրագրի տեքստի վերլուծությունը և կառուցել է աբստրակտ քերականական ծառ։ Այժմ պետք է ինտերպրետացնենք, կատարենք արդեն ծառի տեսքով ներկայացված ծրագիրը։ Այս գլխում իրականացնելու ենք `interpreter` փաթեթը, որի արտաքին `Execute()` ֆունկցիան ստանում է ծրագրի ծառն ու, ռեկուրսիվ այցելելով դրա հանգույցները, ամեն մի հանգույցին կիրառում է իր տիպին համապատասխան հաշվարկող կամ կատարող ֆունկցիան։ (Ակնհայտորեն այստեղ ծառի համար իրականացված է _visitor_ կաղապարը։)

```Go
func Execute(p *ast.Program) error { /* ... */ }
```

Եթե կատարման ժամանակ որևէ սխալ է հանդիպում, օրինակ, տիպերի անհամապատասխանություն, բաժանում զրոյի վրա և այլն, ապա, ինչպես և շարահյուսական վերլուծիչի դեպքում, այդ սխալը, ինտերպրետացնող ֆունկցիաների վերադարձվող արժեքներով, «լողում» է դեպի վերև, մինչև `Execute()` ու վերադարձվում է այն կանչողին։

Բոլոր ինտերպրետացնող ենթածրագրերը, օրինակ, `executeProgram()`, `executeLet()`, `evaluateBinary()`և այլն, իրականացված են որպես `interpreter` ստրուկտուրայի մեթոդներ։ 

```Go
type interpreter struct {
	program *ast.Program // կատարվող ծրագրի ցուցիչը
	env     *environment // կատարման միջավայրը
}
```

Դաշտերից առաջինը՝ `program`-ը, կատարվելիք ծառի արմատն է՝ ինտերպրետացվող ծրագիրը։ երկրորդը՝ `env`-ը, կատարման միջավայրն է՝ նախատեսված ծրագրի կատարման ընթացքում փոփոխականների արժեքները պահելու համար։

Ինտերպրետացնող մեթոդները բաժանված են երկու խմբի. _կատարողներ_ և _հաշվարկողներ_։ Կատարողները լեզվի հրամանների համար են ու միշտ սկսվում են `execute` բառով, օրինակ, `executeDim`։ Սրանք վերադարձնում են միայն `error` արժեք։ Հաշվարկողները արտահայտությունների համար են ու սկսվում են `evaluate` բառով, օրինակ, `evaluateVaraible`։ Սրանք վերադարձնում են `*value` և `error`. հաշվարկման արժեքն ու սխալը։

Քանի որ AST ստրուկտուրաներում հրամանները ներկայացված են `ast.Statement` ինտերֆեյսով, `interprete` ստրուկտուրայի համար սահմանել ենք ճյուղավորող (dispatcher) մեթոդ՝ `execute()` անունով։

```Go
func (i *interpreter) execute(n ast.Statement) error {
	switch s := n.(type) {
	case *ast.Dim:
		return i.executeDim(s)
	case *ast.Let:
		return i.executeLet(s)
	case *ast.Input:
		return i.executeInput(s)
	case *ast.Print:
		return i.executePrint(s)
	case *ast.If:
		return i.executeIf(s)
	case *ast.While:
		return i.executeWhile(s)
	case *ast.For:
		return i.executeFor(s)
	case *ast.Call:
		return i.executeCall(s)
	case *ast.Sequence:
		return i.executeSequence(s)
	}

	return nil
}
```

Ճիշտ նույն եղանակով արտահայտությունների համար սահմանված է `evaluate()` ճյուղավորող մեթոդը։ Սա, ըստ `ast.Expression` ինտերֆեյով ներկայացող փոփոխականի հետ կապված օբյեկտի իրական տիպի, որոշում է կայացնում, թե որ հաշվարկող մեթոդը պետք է կիրառել։

```Go
func (i *interpreter) evaluate(n ast.Expression) (*value, error) {
	switch e := n.(type) {
	case *ast.Boolean:
		return i.evaluateBoolean(e)
	case *ast.Number:
		return i.evaluateNumber(e)
	case *ast.Text:
		return i.evaluateText(e)
	case *ast.Array:
		return i.evaluateArray(e)
	case *ast.Variable:
		return i.evaluateVariable(e)
	case *ast.Unary:
		return i.evaluateUnary(e)
	case *ast.Binary:
		return i.evaluateBinary(e)
	case *ast.Apply:
		return i.evaluateApply(e)
	}

	return nil, nil
}
```


## Կատարման միջավայր

`environment` ստրուկտուրայով իրականացված _կատարման միջավայրն_ (_execution environment_) ըստ էության փոփոխականների տեսանելիության ու հաշվարկման տիրույթների ստեկ է, որտեղ ամեն մի մակարդակը ներկայացված է `scope` ստրուկտուրայի օբյեկտով.

```Go
type environment struct {
	current *scope
}
```

Այստեղ `current` դաշտը, այն է՝ փոփոխականների՝ տվյալ պահին ակտիվ տեսանելիության տիրույթը, ստեկի սկզբունքով իրար կապված տեսանելիության տիրույթներից վերջինն է՝ ստեկի գագաթը։

_Տեսանելիության տիրույթը_ (_scope_) համապատասխանություն է ստեղծում փոփոխականների ու դրանց ընթացիկ արժեքների միջև. `items` դաշտը `map` օբյեկտով փոփոխականի անունը կապում է արժեքին։ Այս ստրուկտուրան ցուցիչ է պարունակում նաև ընթացիկ տիրությին ընդգրկող տիրույթին (`up`)՝ ապահովելով փոփոխականի որոնումը տիրույթների ամբողջ շղթայով։

```Go
type scope struct {
	items map[string]*value // փոփոխականների ընթացիկ արժեքներ
	up    *scope            // ընդգրկող scope-ի ցուցիչ
}
```

Դիտարկենք հետևյալ ոչ մեծ օրինակը.

```basic
SUB Main
  LET x = pi
  PRINT x

  FOR i = 1 TO 4
    LET y = i * 2
    PRINT y
  END FOR

  LET n = 2
  WHILE n <> 0
    LET y = n^2
    PRINT y
	LET n = n - 1
  END WHILE
END SUB
```

Այստեղ կա չորս տեսանելիության տիրույթ. առաջինը _գլոբալն_ է, որտեղ դեռ միայն `pi` հաստատունի արժեքն է, երկրորդը ենթածրագրի մարմինն է, որում սահմանվում են `x` և `n` փոփոխականները, երրորդը `FOR` հրամանի մարմինն է, որում սահմանվում են `i` և `y` փոփոխականները, չորրորդը `WHILE` հրամանի մարմինն է, որում սահմանվում է `y` փոփոխականը։ `FOR` և `WHILE` հրամանների մարմիններում սահմանված `y`-ները լիովին անկախ ու տարբեր են։

Ինտերպրետացիայի ժամանակ նոր տիրույթը ստեղծվում է `openScope` մեթոդով, իսկ ընթացիկ տիրույթը «փակվում» է, դեն է նետվում `closeScope` մեթոդով։

```Go
func (e *environment) openScope() {
	e.current = &scope{
		items: make(map[string]*value),
		up:    e.current,
	}
}

func (e *environment) closeScope() {
	if e.current != nil {
		e.current = e.current.up
	}
}
```

Կատարման միջավայրի ընթացիկ տեսանելիության տիրույթում նոր փոփոխական-արժեք զույգ ավելացնելու համար օգտագործվում է `set` մեթոդը։

```Go
func (e *environment) set(name string, value *value) {
	if e.current != nil {
		e.current.items[name] = value
	}
}
```

Տրված անունով փոփոխականի արժեքը կատարման միջավայրից ստանալու համար է նախատեսված `get` մեթոդը։

```Go
func (e *environment) get(name string) *value {
	for p := e.current; p != nil; p = p.up {
		if v, exists := p.items[name]; exists {
			return v
		}
	}

	return nil
}
```

Այս մեթոդը որոնումը սկսում է «ամենախորն» ընկած տիրույթից ու շարունակում է դեպի վերև, քանի դեռ չի գտել որոնելին։


## Հաշվարկման արժեք

Ինտերպրետացիայի առանցքային նշանակություն ունեցող էությունը _արժեքն_ է, որ իրականացված է `value` ստրուկտուրայով։ Սա, ինտերպրետացիայի տեսակետից, համապիտանի կառուցվածք է, որը կարող է պահել չորս տիպի արժեքներ. _տրամաբանական_, _թվային_, _տեքստային_ եւ նույն այդ տիպերի _զանգված_։

```Go
type value struct {
	kind    rune     // տեսակը
	boolean bool     // տրամաբանական արժեք
	number  float64  // թվային արժեք
	text    string   // տեքստային արժեք
	array   []*value // արժեքների զանգված
}
```

Սրա `kind` դաշտով որոշվում է, թե `value`-ի տվյալ նմուշն ի՛նչ տիպի արժեք է ներկայացնում։ Ահա հնարավոր տարբերակները.

```Go
const (
	vUndefined = '?' // անորոշ
	vBoolean   = 'B' // տեամաբանական
	vNumber    = 'N' // թվային
	vText      = 'T' // տեքստային
	vArray     = 'A' // զանգված
)
```

Ինտերպրետացիայի ընթացքում բոլոր միջանկյալ արժեքները ներկայացված են որպես `value` ստրուկտուրային նմուշներ։ Օրինակ, AST-ի `Boolean` տիպի հանգույցի `Boolean{Value: true}` նմուշը հաշվարկելու արդյունքում ստեղծվում է `value`-ի `value{kind: vBoolean, boolean: true}` նմուշը։

`value`-ի հետ աշխատանքի հարմարության համար սահմանված են տիպի ստուգման մի քանի պարզ պրեդիկատներ․

```Go
func (v *value) isBoolean() bool { return v.kind == vBoolean }
func (v *value) isNumber() bool  { return v.kind == vNumber }
func (v *value) isText() bool    { return v.kind == vText }
func (v *value) isArray() bool   { return v.kind == vArray }
```

Պետք է լինում օգտագործողի համար արտածել `value`-ի արժեքը, ինտերպրետացիայի վերջում կամ ծրագրի շտկման ընթացքում, օգտագործվում է `Stringer` ինտերֆեյսի `String()` մեթոդը։

```Go
func (v *value) String() string {
	res := "<undefined>"
	switch v.kind {
	case vBoolean:
		res = strings.ToUpper(fmt.Sprint(v.boolean))
	case vNumber:
		res = fmt.Sprintf("%g", v.number)
	case vText:
		res = v.text
	case vArray:
		res = ""
		for i, e := range v.array {
			if i != 0 {
				res += ", "
			}
			res += e.String()
		}
		res = "[" + res + "]"
	case vUndefined:
	}
	return res
}
```

Ինտերպրետացիայի ընթացքում արժեքները ենթածրագրերին կամ ֆունկցիաներին փոխանցելիս պատճենվում են։ `clone()` մեթոդը նախատեսված է դրա համար․

```Go
func (v *value) clone() *value {
	cloned := &value{}
	*cloned = *v

	if cloned.kind == vArray {
		cloned.array = make([]*value, len(v.array))
		for i, e := range v.array {
			cloned.array[i] = e.clone()
		}
	}

	return cloned
}
```

Պարզ դեպքում բավական է `*cloned = *v` վերագրումը։ Զանգվածների դեպքում կատարվում է, այսպես կոչված, «խորը պատճենում»։

Արտահայտությունների հաշվարկման համար պետք է ստուգել երկու `value` օբյեկտների հավասարությունը։ `eq()` ստուգում է, որ իրեն փոխանցված երկու օբյեկտները ներկայացնեն նույն տիպը, ապա ստուգում է արժեքների հավասարությունը։ Զանգվածները համարվում են հավասար, եթե դրանք ունեն նույն քանակի տարրեր ու այդ տարրերն էլ հավասար են նույն `eq()` իմաստով։

```Go
func eq(x, y *value) bool {
	switch {
	case x.isBoolean() && y.isBoolean():
		return x.boolean == y.boolean
	case x.isNumber() && y.isNumber():
		return x.number == y.number
	case x.isText() && y.isText():
		return x.text == y.text
	case x.isArray() && y.isArray():
		if len(x.array) != len(y.array) {
			return false
		}

		for i, e := range x.array {
			if !eq(e, y.array[i]) {
				return false
			}
		}
		return true
	}

	return false
}
```

Բուլյան, թվային, տեքստային ու զանգվածների տիպերից միայն երկուսի, թվերի ու տեքստերի համար է սահմանված «փոքր է» հարաբերությունը։

```Go
func lt(x, y *value) bool {
	switch {
	case x.isNumber() && y.isNumber():
		return x.number < y.number
	case x.isText() && y.isText():
		return x.text < y.text
	}

	return false
}
```

Այսքանը ամենակարևորի՝ ինտերպրետացիայի արժեքի մասին։ Հիմա արդեն պատրաստ ենք անցնել բուն կատարման գործողություններին։


## Ծրագրի կատարում

Բալ ծրագրի կատարումը համարժեք է `Main` անունով ենթածրագրի կատարմանը։ Բայց, որպեսզի կատարումն ընթանա սպասվածի պես, `Execute()` ֆունկցիան պետք է որոշ նախապատրաստական աշխատանք անի։ Այսպես.

1. ստեղծի `interpreter` օբյեկտը. սա ամենակարևորն է,
2. ստեղծի անունների գլոբալ  տիրույթ,
3. գլոբալ տիրույթում ավելացնի որրոշ անուններ,
4. կատարի `Main` ենթածրագիրը։

Ահա ասվածի բառացի իրականացումը Գո լեզվով ու մանրամասն մեկնաբանություններով։

```Go
func Execute(p *ast.Program) error {
	// Ստեղծում է interpreter ստրուկտուրային նմուշ, դրա 
	// դաշտերն արժեքավորելով իրեն փոխանցված ծառի արմատով ու 
	// կատարման դատարկ միջավայրով,
	i := &interpreter{program: p, env: &environment{}}

	// ստեղծում է տեսանելիության գլոբալ տիրույթը ...
	i.env.openScope()
	defer i.env.closeScope()

	// ... ու այնտեղ ավելացնում է pi փոփոխականի արժեքը,
	// հետագայում այստեղ կարող ենա ավելացվել այլ անուններ էլ
	i.env.set("pi", &value{kind: vNumber, number: math.Pi})

	// ստեղծում է `Call` օբյեկտ՝ `Main`-ի կանչի համար,
	// արգումետներում դատարկ ցուցակ է (բայց ոչ nil)
	cmain := ast.Call{Callee: "Main", Arguments: make([]ast.Expression, 0)}

	// կատարում է `Call` օբյեկտը՝ վերադարձնելով դրա սխալի օբյեկտը (կամ՝ nil)
	return i.executeCall(&cmain)
}
```

Նորից նշենք, որ `Execute()`-ը `interpreter` փաթեթի միակ արտաքին, ինտերֆեյասային ֆունկցիան է. սրան ենք փոխանցում ծրագրի ծառային ներկայացումը և սրա աշխատանքի արդյունքում սպասում ենք կա՛մ հաջող կատարում, կա՛մ կատարման ժամանակի սխալը ներկայացնող  `error` օբյեկտ։


## Արտահայտությունների հաշվարկը

Սկսենք `interpreter` ստրուկտուրայի ամենապարզ մեթոդներից։ Դրանք, բնականաբար, նրանք են, որոնք նախատեսված են վերլուծության ծառի տերևների համար։ Իսկ տերևներ են դառնում լիտերալներն ու փոփոխականները։

Բուլյան, թվային ու տեքստային լիտերալների հանգույցների հաշվարկը նույնական է. պարզապես ստեղծվում է նույն արժեքով `value` օբյեկտ։

```Go
func (i *interpreter) evaluateBoolean(b *ast.Boolean) (*value, error) {
	return &value{kind: vBoolean, boolean: b.Value}, nil
}

func (i *interpreter) evaluateNumber(n *ast.Number) (*value, error) {
	return &value{kind: vNumber, number: n.Value}, nil
}

func (i *interpreter) evaluateText(t *ast.Text) (*value, error) {
	return &value{kind: vText, text: t.Value}, nil
}
```

Զանգվածի լիտերալի հաշվարկման դեպքում պետք է նաև հաշվարկել դրանում մասնակցող տարրերը.

```Go
func (i *interpreter) evaluateArray(a *ast.Array) (*value, error) {
	els := len(a.Elements)
	res := &value{kind: vArray, array: make([]*value, els)}
	for j, e := range a.Elements {
		val, err := i.evaluate(e)
		if err != nil {
			return nil, err
		}
		res.array[j] = val
	}
	return res, nil
}
```

Փոփոխականի արժեքը պետք է վերցնել կատարման միջավայրից։ Բայց ի՞նչ անել, եթե դիտարկվող անունը բացակայում է այնտեղ։ Օրինակ, ինչպիսի՞ն պետք է լինի հետևյալ ծրագրի վարքը.

```Basic
SUB Main
   PRINT y
END SUB
```

Կարող է լինել երկու մոտեցում. ա) կատարման սխալ, ինտերպրետացիան պետք է դադարեցնել, բ) վերադաձնել ինչ-որ արժեքով `value` օբյեկտ, որի օգտագործումը, սակայն, մի այլ կետում է հանգեցնելու սխալի։ Մենք ընտրել ենք անմիջապես սխալի մասին հայտարարման տարբերակը։

```Go
func (i *interpreter) evaluateVariable(v *ast.Variable) (*value, error) {
	if vp := i.env.get(v.Name); vp != nil {
		return vp, nil
	}

	return nil, fmt.Errorf("անհայտ փոփոխական՝ %s", v.Name)
}
```

Միտեղնի գործողություններով արտահայտությունների համար է `evaluateUnary()` մեթոդը։ Սա նախ հաշվում է ենթաարտահայտությունը.

```Go
func (i *interpreter) evaluateUnary(u *ast.Unary) (*value, error) {
	result, err := i.evaluate(u.Right)
	if err != nil {
		return nil, err
	}
```

Հետո դիտարկվում են թույլատրելի գործողությունները։ Դրանք այս դեպքում երկուսն են. ունար մինուս և տրամաբանական ժխտում։ Առաջինի դեպքում ստուգվում է, որ նախորդ քայլում հաշվարկված արժեքը թվային լինի։ Երկրորդի դեպքում՝ պետք է բուլյան լինի։ Եթե տիպերի ստուգումը հաջողվում է, ապա `result`-ի արժեքը փոխվում է ըստ օպերատորի ու վերադարձվում է որպես արդյունք։

```Go
	switch u.Operation {
	case "-":
		if !result.isNumber() {
			return nil, fmt.Errorf("- գործողության արգումենտը պետք է թիվ լինի")
		}

		result.number *= -1
	case "NOT":
		if !result.isBoolean() {
			return nil, fmt.Errorf("NOT գործողության արգումենտը պետք է տրամաբանական արժեք լինի")
		}

		result.boolean = !result.boolean
	}

	return result, nil
}
```

Երկտեղանի գործողությունները շատ են ու բազմազան։ Դրա համար էլ `evaluateBinary()` մեթոդը այստեղ ապահովում է ըստ գործողությունների խմբերի ճյուղավորումը. թվաբանական, տեքստերի միակցման, տրամաբանական, համեմատման ու զանգվածի ինդեքսավորման։

```Go
func (i *interpreter) evaluateBinary(b *ast.Binary) (*value, error) {
	switch b.Operation {
	case "+", "-", "*", "/", "\\", "^":
		return i.evaluateArithmetic(b)
	case "&":
		return i.evaluateTextConcatenation(b)
	case "AND", "OR":
		return i.evaluateLogic(b)
	case "[]":
		return i.evaluateIndexing(b)
	case "=", "<>", ">", ">=", "<", "<=":
		return i.evaluateComparison(b)
	}

	return nil, fmt.Errorf("անծանոթ երկտեղանի գործողություն")
}
```

Բոլոր այդ երկտեղանի գործողությունների վարքը ծրագրավորված է `operations` աղյուսակում (տե՛ս `operations.go` ֆայլը)։ Սա գործողության անունին համապատասխանեցնում է դրա հաշվարկող անանուն ֆունկցիան։

```Go
type binary func(l, r *value) *value

var operations = map[string]binary{
	// թվային գործողություններ
	"+": func(l, r *value) *value {
		return &value{kind: vNumber, number: l.number + r.number}
	},
	// ...
	// համեմատումներ
	"=": func(l, r *value) *value {
		return &value{kind: vBoolean, boolean: eq(l, r)}
	},
	// ...
	// տրամաբանական գործողություններ
	"AND": func(l, r *value) *value {
		return &value{kind: vBoolean, boolean: l.boolean && r.boolean}
	},
	// ...
}
```

Ամբողջ աղյուսակն այստեղ չպատճենելու համար բերել ենք միայն `+`, `=` և `AND` գործողությունների օրինակները։ Մյուսները նման են դրանց։


`evaluateBinary()` մեթոդում հիշատակված `evaluateArithmetic()`, `evaluateTextConcatenation()`, `evaluateLogic()`, `evaluateIndexing` և `evaluateComparison` մեթոդներն էլ են կառուցված միևնույն սխեմայով։ Դրանք տարբերվում են միայն տիպերի ստուգման կտորներով։ 

Նորից, տեքստը միօրինակությամբ չծանրաբեռնելու համար, այստեղ կցուցադրենք միայն `evaluateArithmetic()` և `evaluateIndexing()` մեթոդները։

```Go
func (i *interpreter) evaluateArithmetic(b *ast.Binary) (*value, error) {
	// հաշվարկել ձախ արգումենտը
	left, err := i.evaluate(b.Left)
	if err != nil {
		return nil, err
	}
	// ստուգել տիպը
	if !left.isNumber() {
		return nil, fmt.Errorf("%s գործողության ձախ կողմում սպասվում է թվային արժեք", b.Operation)
	}

	// հաշվարկել աջ արգումենտը
	right, err := i.evaluate(b.Right)
	if err != nil {
		return nil, err
	}
	// ստուգել տիպը
	if !right.isNumber() {
		return nil, fmt.Errorf("%s գործողության աջ կողմում սպասվում է թվային արժեք", b.Operation)
	}

	// երբ երկուսն էլ թիվ են՝ կատարել գործողությունը
	return operations[b.Operation](left, right), nil
}
```

Նույն կերպ `evaluateTextConcatenation()`-ում ստուգվում է, երկու արգումենտների արդյունքն էլ տեքստ լինի։ `evaluateLogic()`-ում ստուգվում է, որ երկու արգումենտն էլ տրամաբանական արժեքներ լինեն։ `evaluateComparison`-ում էլ ստուգվում է, որ երկու արգումենտները _համեմատելի_ արժեքներ լինեն։

`evaluateIndexing()` մեթոդը հաշվում է զանգվածի՝ տրված ինդեքսով տարրի արժեքը։ Այս գործողության ձախ կողմը պետք է հաշվարկվի զանգված արդյունքով, իսկ աջ կողմը՝ թիվ արդյունքով։ Ուրեմն, նախ հաշվում ենք ձախ կողմը ու ստուգում ենք դրա տիպը։ Եթե այն զանգված չէ, ապա հաշվարկն ընդհատում ենք սխալի մասին ազդարարելով։


```Go
func (i *interpreter) evaluateIndexing(b *ast.Binary) (*value, error) {
	left, err := i.evaluate(b.Left)
	if err != nil {
		return nil, err
	}
	if !left.isArray() {
		return nil, fmt.Errorf("[]-ի ձախ կողմում պետք է զանգված լինի")
	}
```

Հետո հաշվարկում ենք ինդեքսի արգումենտը ու ստուգում ենք, որ այն թիվ լինի։ Հակառակ դեպքում ինտերպրետացիան շարունակելն անիմաստ է. ազդարարում ենք հաշվարկման սխալիմասին։

```Go
	right, err := i.evaluate(b.Right)
	if err != nil {
		return nil, err
	}
	if !right.isNumber() {
		return nil, fmt.Errorf("[]-ի ինդեքսը պետք է թիվ լինի")
	}
```

Երբ ձախ արգումենտը զանգված է, իսկ աջն էլ թիվ է, պետք է ստուգենք որ այդ ինդեքսը զրոյից փոքր չլինի ու զանգվածի տարրերի քանակից էլ մեծ չլինի։ Երբ ամեն ինչ տեղն է, վերադարձնում ենք զանգվածի համապատասխան տարրը։

```Go
	index := int(right.number)
	if index < 0 || index >= len(left.array) {
		return nil, fmt.Errorf("ինդեքսը զանգվածի սահմաններից դուրս է")
	}

	return left.array[index], nil
}
```


Դե, քանի որ հիշատակեցինք հրամանի կատարումը, իսկ `CALL`-ը հրաման է, հաջորդ ենթագլխում մանրամասն դիտարկենք հրամանների ինտերպրետացիան։


## Հրամանների կատարումը

Ղեկավարող կառուցվածքներում, ենթածրագրի մարմնում հրամանները հիշատակվում են որպես `Statement`: Սա ինտերֆեյս է՝ համարժեք Գոի `any`-ին։ Հետևաբար `interpreter` ստրուկտուրան պիտի մի ճյուղավորող մեթոդ ունենա, որը, ընտրություն կատարելով կոնկրետ հրամանի տիպի համար, կանչի համապատսխան ինտերպրետացնող մեթոդը։ Ոչ մի բարդ բան.


Հիմա արդեն, բոլոր այն տեղերում, որտեղ պետք է հրամանի ինտերպրետացիա, կարող ենք կանչել այս `execute()` մեթոդը և վստահ լինել, որ կատարումը կգնա ճիշտ ուղղով։

__Հաջորդման կատարումը։__ `Sequence` կառուցվածքի կատարում, ինչպես և դրա վերլուծությունն էր, շատ պարզ է. հերթվ անցնում ենք հաջորդականության անդամներով, ու ամեն մեկին կիրառում ենք `execute()` մեթոդը։ Բայց, միչև դա, ստեղծում են նոր տեսանելիության տիրույթ։

```Go
func (i *interpreter) executeSequence(s *ast.Sequence) error {
	i.env.openScope()
	defer i.env.closeScope()

	for _, st := range s.Items {
		err := i.execute(st)
		if err != nil {
			return err
		}
	}

	return nil
}
```

__Զանգվածի ստեղծում։__ `DIM` հրամանով զանգված ստեղծելու համար 


Ինչպես արդեն տեսանք աբստրակտ քերականական ծառի նկարագրության գլխում, արտահայտությունների տարատեսակները ներկայացնող հանգույցների տիպերն ութն են. `Boolean`, `Number`, `Text` և `Array` — լիտերալների համար, `Unary` և `Binary` — միտեղանի ու երկտեղանի գործողությունների համար և `Apply` — ենթածրագիր-ֆունկցիաների կիրառության համար։ Արտահայտության տրված ենթածառի տիպը տարբերակվում և համապատասխան հաշվարկող ֆունկցիան կանչվում է `evaluate` ֆունկցիայում.


