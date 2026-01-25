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

Դե, քանի որ հիշատակեցինք հրամանի կատարումը, իսկ `CALL`-ը հրաման է, հաջորդ ենթագլխում մանրամասն դիտարկենք հրամանների ինտերպրետացիան։


## Հրամանների կատարումը

Ղեկավարող կառուցվածքներում, ենթածրագրի մարմնում հրամանները հիշատակվում են որպես `Statement`: Սա ինտերֆեյս է՝ համարժեք Գոի `any`-ին։ Հետևաբար `interpreter` ստրուկտուրան պիտի մի ճյուղավորող մեթոդ ունենա, որը, ընտրություն կատարելով կոնկրետ հրամանի տիպի համար, կանչի համապատսխան ինտերպրետացնող մեթոդը։ Ոչ մի բարդ բան.

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


## Արտահայտությունների հաշվարկը

Ինչպես արդեն տեսանք աբստրակտ քերականական ծառի նկարագրության գլխում, արտահայտությունների տարատեսակները ներկայացնող հանգույցների տիպերն ութն են. `Boolean`, `Number`, `Text` և `Array` — լիտերալների համար, `Unary` և `Binary` — միտեղանի ու երկտեղանի գործողությունների համար և `Apply` — ենթածրագիր-ֆունկցիաների կիրառության համար։ Արտահայտության տրված ենթածառի տիպը տարբերակվում և համապատասխան հաշվարկող ֆունկցիան կանչվում է `evaluate` ֆունկցիայում.

```Go
func evaluate(n ast.Node, env *environment) *value {
	var result *value

	switch e := n.(type) {
	case *ast.Boolean:
		result = evaluateBoolean(e, env)
	case *ast.Number:
		result = evaluateNumber(e, env)
	case *ast.Text:
		result = evaluateText(e, env)
	case *ast.Array:
		result = evaluateArray(e, env)
	case *ast.Variable:
		result = evaluateVariable(e, env)
	case *ast.Unary:
		result = evaluateUnary(e, env)
	case *ast.Binary:
		result = evaluateBinary(e, env)
	case *ast.Apply:
		result = evaluateApply(e, env)
	}

	return result
}
```

