'
' զանգվածի ամենամեծ տարրը
'
SUB maxOf(arr)
  LET maxOf = arr[0]
  FOR i = 1 TO LEN(arr) - 1
    IF arr[i] > maxOf THEN
      LET maxOf = arr[i]
    END IF
  END FOR
END SUB

SUB Main
    LET arr = [1, 2, 3, 9, 8, 7, 4, 5, 6]
    LET r = maxOf(arr)
    PRINT r
END SUB
