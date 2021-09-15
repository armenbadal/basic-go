
SUB Main
    PRINT "Simple"
    FOR i = 0 TO 10
        PRINT i
    END FOR

    PRINT "Positive step"
    FOR i = 4 TO 16 STEP 4
        PRINT i
    END FOR

    PRINT "Negative step"
    FOR i = 20 TO 0 STEP -5
        PRINT i
    END FOR

    PRINT "Using expressions"
    LET begin = 0
    FOR i = begin + 3 TO begin + 18 STEP 3
        PRINT i
    END FOR
END SUB
