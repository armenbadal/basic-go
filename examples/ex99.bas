
SUB min(a, b)
    IF a < b THEN
        LET min = a
    ELSE
        LET min = b
    END IF
END SUB

SUB max(ar)
    LET max = ar[0]
    FOR i = 0 TO LEN(ar) - 1
        IF max < ar[i] THEN
            LET max = ar[i]
        END IF
    END FOR
END SUB

SUB Main
    LET abc = [1, 2, 77, 5, 6]
    PRINT max(abc)
    PRINT "----> Ok"

    FOR i = 1 TO 20 STEP 3
        PRINT i
    END FOR

    LET m = min(5, 12)
    PRINT m

    LET k = 1
    WHILE k <= 5
        PRINT k
        LET k = k + 1
    END WHILE

    PRINT 1.2
    PRINT "Text"
    PRINT TRUE
    PRINT [1, 2, 3]

    LET x = 3.1415
    PRINT x

    LET y = "Ողջո՜ւյն"
    PRINT y

    LET z = TRUE
    PRINT z

    LET arr = [3.1415, "Բալ", FALSE]
    PRINT arr
    PRINT arr[0]
    PRINT arr[1]
    PRINT arr[2]

    LET arr[2] = TRUE
    PRINT arr

    DIM m[2]
    PRINT m
    LET m[0] = TRUE
    LET m[1] = FALSE
    PRINT m
END SUB
