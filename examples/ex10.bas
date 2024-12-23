SUB Maximum(x, y)
    LET Maximum = x
    IF y > Maximum THEN
        LET Maximum = y
    END IF
END SUB

SUB Main
    PRINT Maximum(4, 12)
    PRINT Maximum(16, -12)
END SUB
