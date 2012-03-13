SUBROUTINE SOILFILEEDIT(FILEI2,NL,TKL,SANDX, CLAYX, BD, SOC, SON, SNH4X, SNO3X, SUREA, PLOWPAN)

    USE PUBLIC_MODULE

    IMPLICIT NONE
    
    INTEGER IUNIT, IUNITL, NL, I
    CHARACTER*(*) FILEI2
    REAL SANDX(10), CLAYX(10), BD(10), SOC(10), SON(10)
    REAL TKL(10), SNH4X(10), SNO3X(10), SUREA(10), PLOWPAN
    
    CALL GETLUN('FILEI2',IUNIT)
    OPEN(UNIT=IUNIT, FILE = FILEI2, STATUS='REPLACE',ACTION='WRITE')
    WRITE(IUNIT, '(A)') "*--------------------------------------------------------------------*"
    WRITE(IUNIT, '(A)') "* 1. SOIL PHYSIC INFORMATION                                   *"
    WRITE(IUNIT, '(A)') "*--------------------------------------------------------------------*"
    
    WRITE(IUNIT, 4000) 'NL', NL, '    ! Number of soil layers (maximum is 10) (-)'
    WRITE(IUNIT, 3000) 'TKL', (TKL(I), I = 1, NL)
    WRITE(IUNIT, 3000) 'CLAYX', (CLAYX(I), I=1, NL) 
    WRITE(IUNIT, 3000) 'SANDX', (SANDX(I), I=1, NL) 
    WRITE(IUNIT, 3000) 'BD', (BD(I), I=1, NL) 
    WRITE(IUNIT, '(A)') "*--------------------------------------------------------------------*"
    WRITE(IUNIT, '(A)') "* 1. SOIL CHEMICAL INFORMATION (in kg C or N/ha)                                  *"
    WRITE(IUNIT, '(A)') "*--------------------------------------------------------------------*"
    WRITE(IUNIT, 3000) 'SOC', (SOC(I), I = 1, NL) 
    WRITE(IUNIT, 3000) 'SON', (SON(I), I = 1, NL) 
    WRITE(IUNIT, 3000) 'SNH4X', (SNH4X(I), I = 1, NL) 
    WRITE(IUNIT, 3000) 'SNO3X', (SNO3X(I), I = 1, NL)
    IF(SUM(SUREA).GT.0.0) THEN
        WRITE(IUNIT, 3000) 'SUREA', (SUREA(I), I = 1, NL)  
    END IF
    IF(PLOWPAN.GT.0.0) THEN
        WRITE(IUNIT, 5000) 'PLOWPAN',PLOWPAN
    END IF     
    CLOSE(IUNIT)


    3000 FORMAT(A8,"=",<NL-1>(F12.3,","),F12.3)
    4000 FORMAT(A8, "=",I4, A)
    5000 FORMAT(A8, "=",F12.3, A)

END SUBROUTINE SOILFILEEDIT