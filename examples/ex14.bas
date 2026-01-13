SUB Main
  LET x = pi
  PRINT "PI = " & STR(x)

  FOR i = 1 TO 4
    LET y = i * 2
    PRINT y
  END FOR

  LET n = 2
  WHILE n <> 0
    LET y = pi^n
    PRINT "PI^" & STR(n) & " = " & STR(y)
	  LET n = n - 1
  END WHILE
END SUB
