
'
' Ռեկուրսիայի օրինակ
'

SUB Factorial(n)
    IF n = 1 THEN
        LET Factorial = 1
    ELSE
        LET Factorial = n * Factorial(n - 1)
    END IF
END SUB


SUB Main
    LET m = Factorial(10)
    PRINT m
END SUB
