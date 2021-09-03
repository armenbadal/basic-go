
SUB Main
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
