# Քերականության և բառակազմության սահմանումը

Բալի քերականությունը սահմանելու համար օգտագործելու եմ EBNF գրառումը (Wirth, Compiler construction)։

Բալ _ծրագիրը_՝ `Program`, ենթածրագրերի հաջորդականություն է։

```
Program = { Subroutine }.
```

_Ենթածրագիրը_՝ `Subroutine`, ինքնուրույն, իմաստալից գործողության սահմանումն է։ Այն կարող է
ստանալ արգումենտներ՝ իր պարամետրերի միջոցով, և վերադարձնել արժեք։

```
Subroutine = 'SUB' IDENT ['(' [IDENT {',' IDENT}] ')'] Sequence 'END' SUB'.
```

Ենթածրագրերի մարմնում գործողությունները նկարագրվում են որպես _հրամանների հաջորդականություն_՝
`Sequence`։ Հաջորդականությունը սկսվում է մեկ կամ ավելի նոր տողերի նիշերվ, իսկ ամեն մի հրամանին
նորից պետք է հաջորդեն մեկ կամ ավելի նոր տողեր։

```
Sequence = NewLines { Statement NewLines }.
```

```
NewLines = NEWLINE { NEWLINE }.
```

```
Statement = 'LET' IDENT '=' Expression
          | 'INPUT' IDENT
          | 'PRINT' Expression
          | 'IF' Expression 'THEN' Sequence
            { 'ELSEIF' Expression 'THEN' Sequence }
            [ 'ELSE' Sequence ]
            'END' 'IF'
          | 'WHILE' Expression Sequence 'END' 'WHILE'
          | 'FOR' IDENT '=' Expression 'TO' Expression
            ['STEP' ['+'|'-'] NUMBER Sequence
            'END' 'FOR'
          | 'CALL' IDENT [Expression {',' Expression}]
           .
```

```
Expression = Conjunction { 'OR' Conjunction }.
Conjunction = Equality { 'AND' Equality }.
Equality = Comparison [ ('=' | '<>') Comparison ].
Comparison = Addition [ ('<' | '<=' | '>' | '>=') Addition ].
Addition = Multiplication { ('+' | '-' | '&') Multiplication }.
Multiplication = Power { ('*' | '/' | '\') Power }.
Power = Subscript [ '^' Power ].
Subscript = Factor { '[' Expression ']' }.
Factor = 
```
