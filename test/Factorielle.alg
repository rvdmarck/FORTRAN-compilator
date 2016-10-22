PROGRAM factorielle

! Compute the factorial of a number.
* If the input number is negative, print -1.
  
  INTEGER nombre, resultat, compteur
  READ*, nombre
  resultat = 1
  
  IF (nombre .GE. 0) THEN
    DO compteur = 1, nombre
      resultat = resultat * compteur
    ENDDO
    PRINT*, resultat
  ELSE
    PRINT*, -1
  ENDIF
END
