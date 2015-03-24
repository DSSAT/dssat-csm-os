      MODULE ttutil
      INTERFACE
         FUNCTION lint (Table, iltab, x)
         REAL                               :: lint
         INTEGER, INTENT(IN)                :: iltab
         REAL, DIMENSION(iltab), INTENT(IN) :: Table
         REAL, INTENT(IN)                   :: x
         END FUNCTION lint
      END INTERFACE
      END MODULE ttutil
