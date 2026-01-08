SUB Show(a)
    PRINT a
END SUB

SUB Negate(ar)
    FOR i = 0 TO LEN(ar) - 1
        LET ar[i] = -ar[i]
    END FOR
    LET Negate = ar
END SUB

SUB Main
    DIM a[4]
    LET a[0] = 9
    LET a[1] = 8
    LET a[2] = 7
    LET a[3] = 6
    CALL Show a
    CALL Show Negate(a)
    CALL Show a

    LET b = [1, 2, 3]
    CALL Show -b[0]

    PRINT "Ok"

    PRINT -3
    PRINT -3 ^ 2
    PRINT -(3 ^ 2)
END SUB
