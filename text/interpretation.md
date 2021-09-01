# Ինտերպրետացիա

Ինչպես լեզվի շարահյուսական վերլուծությունն է կառուցված ռեկուրսիվ ֆունկցիաներով,որոնցից ամեն մեկը վերլուծում է քերականության «իրեն հասանելիք» հատվածը և վերադարձնում է աբստրակտ քերականական ծառի մի ենթածառ, այնպես էլ ինտերպրետատորն է կառուցված ռեկուրսիվ ֆունկցիաներով, որոնցից յուրաքանչյուրը կատարում կամ հաշվարկում է ծառի՝ իրեն վերաբերող ենթածառը։

Ինտերպրետացիան իրականացված է `interpreter` փաթեթում։ Այս փաթեթում են ներառված թե՛ առանձին հանգույցների ինտերպրետացաիյի ֆունկցիաները, թե՛ կատարման միջավայրի իրականացումը, թե՛ ներդրված ենթածրագրերի սահմանումները և թե՛ հաշվարկման ունիվերսալ արժեքի սահմանումը։ 

## Հաշվարկման արժեք

Բոլոր տեսակի արտահայտությունների հաշվարկման արդյունքում ստացվում է `value` տիպի արժեք.

```Go
type value struct {
	kind    rune     // տեսակը
	boolean bool     // տրամաբանական արժեք
	number  float64  // թվային արժեք
	text    string   // տեքստային արժեք
	array   []*value // արժեքների զանգված
}
```

Այս ստրուկտուրային `kind` դաշտով է որոշվում, թե `value`-ի տվյալ նմուշն ի՛նչ տիպի արժեք է ներկայացնում։ Ահա հնարավոր տարբերակները.

```Go
const (
	vUndefined = '?' // անորոշ
	vBoolean   = 'B' // տեամաբանական
	vNumber    = 'N' // թվային
	vText      = 'T' // տեքստային
	vArray     = 'A' // զանգված
)
```

Օրինակներ.

```Go
b0 := &value{kind: vBoolean, boolean: true} // տրամաբանական արժեք
n0 := &value{kind: vNumber, number: 3.1415} // թվային արժեք
```

## Կատարման միջավայր

Ծրագիր ինտերպրետացիայի ընթացքում հաշվարկված արժեքները պահվում են `environment` տիպի օբյեկտում։ Սա հենց _կատարման միջավայրն_ է (execution environment)։ 

```Go
type scope struct {
	items map[string]*value
	up    *scope
}

type environment struct {
	current *scope
}
```

Տեսնում ենք, որ `environment`-ը `scope`-երի՝ _տեսանելիության տիրույթների_ ստեկ է, որի «գագաթին» ցույց է տալիս `current` ցուցիչը։ Նոր տիրույթը ստեղծվում է `openScope` մեթոդով, իսկ ընթացիկ տիրույթը փակվում է, դեն է նետվում `closeScope` մեթոդով։

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

Կատարման միջավայրի ընթացիկ տեսանլիության տիրույթում նոր փոփոխական-արժեք զույգ ավելացնելու համար օգտագործվում է `set` մեթոդը։

```Go
func (e *environment) set(name string, value *value) {
	e.current.items[name] = value
}
```

Տրված անունով փոփոխականի արժեքը 

## Արտահայտությունների հաշվարկում

Ինչպես արդեն տեսանք աբստրակտ քերականական ծառի նկարագրության գլխում, արտահայտությունների տարատեսակները ներկայացնող հանգույցների տիպերն ութն են. `Boolean`, `Number`, `Text` և `Array` — լիտերալների համար, `Unary` և `Binary` — միտեղանի ու երկտեղանի գործողությունների համար և `Apply` — ենթածրագիր-ֆունկցիաների կիրառության համար։ Արտահայտության տրված ենթածառի 

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

## Հրամանների կատարում
