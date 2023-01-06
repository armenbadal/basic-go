SUB CountDigits(n)
    LET count = 0
    WHILE n <> 0
        LET n = n \ 10
        LET count = count + 1
    END WHILE
    LET CountDigits = count
END SUB

SUB Main
    PRINT CountDigits(1981)
    PRINT CountDigits(19812022)
END SUB
