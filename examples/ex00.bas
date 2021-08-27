
'
' Առաջին օրինակ
'
SUB Main
    INPUT x
    PRINT "Ողջո՜ւյն։"

    LET a = [1, 2, 3]
    LET t = TRUE AND FALSE
    LET y = LEN(x)
    LET k = MID(x, 2, 4)
END SUB

SUB min(x, y)
    IF x > y THEN
        LET min = x
    ELSE
        LET min = y
    END IF
END SUB
