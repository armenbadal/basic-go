
SUB makeArray(n)
  DIM arr[n]
  FOR i = 0 TO n - 1
    LET arr[i] = i * i
  END FOR
  LET makeArray = arr
END SUB

SUB Main
  LET arr = makeArray(12)
  PRINT arr
END SUB
