      REAL FUNCTION DS1900 (YEAR,DAY)

!     calculate number of days since Jan 1, 1900

      REAL YEAR, DAY
      SAVE

      DS1900 = (YEAR-1900.)*365.+DAY+REAL (INT(YEAR+3.-1900.)/4)

      RETURN
      END
