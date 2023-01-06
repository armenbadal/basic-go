SUB Array(a)
    LET a[0] = TRUE
    LET a[1] = FALSE

    PRINT LEN(a)
    PRINT a
    PRINT a[0]
    PRINT a[1]
END SUB

SUB Main
    DIM a[2]
    CALL Array a

    PRINT "After call"
    PRINT a[0]
    PRINT a[1]
    PRINT "Ok"
END SUB

