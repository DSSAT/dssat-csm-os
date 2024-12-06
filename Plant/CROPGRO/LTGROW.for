C=======================================================================
C  Lint Growth, Subroutine, F. Oliveira, G. Hoogenboom, K.J. Boote
C  Calculates Lint growth for cotton.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  11/08/2023 Written.
C========================================================================

      SUBROUTINE LTGROW(DYNAMIC, FILECC, FILEGC, ECONO,          !Input
     &    WSDDOT, TAVG, TURFAC, NSTRES,                          !Input
     &    LTDOT)                                                 !Output

C-----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE, TABEX
      SAVE

      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = 'LTGROW')

      CHARACTER*6   SECTION
      CHARACTER*6   ECOTYP, ECONO, SPCTLT
      CHARACTER*92  FILECC, FILEGC
      CHARACTER*255 C255

      INTEGER I, LUNCRP, LUNECO, ERR, LINC, LNUM, ISECT
      INTEGER DYNAMIC
      INTEGER FOUND
      
      REAL LTDOT, WSDDOT, NSTRES, TURFAC, TABEX, TAVG
      REAL PCTLT, TEMLT, SWFLT, NSFLT

      REAL XLTTM(5), YLTTM(5)
      REAL XLSWFAC(4), YLSWFAC(4)
      REAL XLNFAC(4), YLNFAC(4)

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
C-----------------------------------------------------------------------
C    Read Ecotype Parameter File
C-----------------------------------------------------------------------
        PCTLT = -99.0
        SPCTLT = ' '
        
        CALL GETLUN('FILEE', LUNECO)
        OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,0)
        ECOTYP = '      '
        LNUM = 0
        DO WHILE (ECOTYP .NE. ECONO)
            CALL IGNORE(LUNECO, LNUM, ISECT, C255)
            IF ((ISECT .EQ. 1) .AND. (C255(1:1) .NE. ' ') .AND.
     &        (C255(1:1) .NE. '*')) THEN
            READ (C255,'(A6,120X,A6)',IOSTAT=ERR)
     &        ECOTYP, SPCTLT
            IF (SPCTLT .EQ. '') CALL ERROR(ERRKEY,10,FILEGC,LNUM)
            READ(SPCTLT,'(F6.0)',IOSTAT=ERR) PCTLT
            IF (PCTLT .LT. 25 .OR. PCTLT .GT. 60) 
     &        CALL ERROR(ERRKEY,11,FILEGC,LNUM)
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)
            IF (ECOTYP .EQ. ECONO) EXIT

            ELSE IF (ISECT .EQ. 0) THEN
            IF (ECONO .EQ. 'DFAULT') CALL ERROR(ERRKEY,35,FILEGC,LNUM)
            ECONO = 'DFAULT'
            REWIND(LUNECO)
            LNUM = 0
            ENDIF
        ENDDO

        CLOSE (LUNECO)
!-----------------------------------------------------------------------
!     Read in values from input file, which were previously input
!       in Subroutine IPCROP.
!-----------------------------------------------------------------------
        CALL GETLUN('FILEC', LUNCRP)      
        OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)
        LNUM = 0
!-----------------------------------------------------------------------
C     ***** READ LINT PARAMETERS ******************
C-----------------------------------------------------------------------
        SECTION = '*LINT '
        CALL FIND(LUNCRP, SECTION, LINC, FOUND); LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
            CALL ERROR(SECTION, 42, FILECC, LNUM)
        ELSE
            CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
            READ(C255,'(5F6.0)',IOSTAT=ERR)(XLTTM(I),I = 1,5)
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

            CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
            READ(C255,'(5F6.0)',IOSTAT=ERR)(YLTTM(I),I = 1,5)
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
            
            CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
            READ(C255,'(4F6.0)',IOSTAT=ERR)(XLSWFAC(I),I = 1,4)
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

            CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
            READ(C255,'(4F6.0)',IOSTAT=ERR)(YLSWFAC(I),I = 1,4)
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
            
            CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
            READ(C255,'(4F6.0)',IOSTAT=ERR)(XLNFAC(I),I = 1,4)
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

            CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
            READ(C255,'(4F6.0)',IOSTAT=ERR)(YLNFAC(I),I = 1,4)
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF
        
        CLOSE (LUNCRP)

!***********************************************************************
!***********************************************************************
!     SEASONAL INITIALIZATION 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
        LTDOT = 0.0
!***********************************************************************
!***********************************************************************
!     DAILY INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
        TEMLT = TABEX(YLTTM,XLTTM,TAVG,5)
        SWFLT = TABEX(YLSWFAC,XLSWFAC,TURFAC,4)
        NSFLT = TABEX(YLNFAC,XLNFAC,NSTRES,4)

        LTDOT = WSDDOT * (PCTLT*0.01) * TEMLT * SWFLT * NSFLT        
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END
!=======================================================================
! Definitions:  
!-----------------------------------------------------------------------
! LTDOT    Net Lint growth rate (g/m2/d)