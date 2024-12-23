SUB OldArmenianNameOf(planet)
    IF planet = "Մերկուրի" THEN
        LET OldArmenianNameOf = "Փայլածու"
    ELSEIF planet = "Վեներա" THEN
       LET OldArmenianNameOf = "Արուսյակ"
    ELSEIF planet = "Մարս" THEN
       LET OldArmenianNameOf = "Հրատ"
    ELSEIF planet = "Յուպիտեր" THEN
       LET OldArmenianNameOf = "Լուսնթագ"
    ELSEIF planet = "Սատուրն" THEN
       LET OldArmenianNameOf = "Երևակ"
    ELSE
        LET OldArmenianNameOf = planet
    END IF
END SUB


SUB Main
    PRINT OldArmenianNameOf("Մարս")
    PRINT OldArmenianNameOf("Երկիր")
END SUB
