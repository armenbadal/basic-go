# Շարահյուսական վերլուծություն

Շարահյուսական վերլուծիչը բաղկացած է երկու տրամաբանական բասերից. _բառային վերլուծիչ_ (`scanner`) և բուն _շարահյուսական վերլուծիչ_ (`Parser`)։ Բառային վերլուծիչը կարդում է ծրագրի տեքստը, առանձնացնում է իմաստ կրող հատվածները՝ _լեքսեմները_ (lexeme), ամեն մեկին համապատասխանեցնում է տիպը որոշող _պիտակ_ (token)։

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
    for _, e := range exps {
        if e == l.token {
            return true
        }
    }
    return false
}
```
