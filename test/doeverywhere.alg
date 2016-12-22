PROGRAM doeverywhere

  INTEGER compteur1, compteur2

  DO compteur1 = 1, 5
    DO compteur2 = 2, 3
      PRINT*, compteur1 * compteur2
    ENDDO
  ENDDO

  DO compteur1 = 4, 5
    PRINT*, compteur1 - 3
  ENDDO
END
