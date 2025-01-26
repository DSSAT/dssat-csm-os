!============================================================================================
      Subroutine ECO_read(LABEL, Value)

      USE ModuleData
      IMPLICIT NONE
      SAVE
      EXTERNAL ERROR, FIND, GETLUN, IGNORE, IGNORE2, LENSTRING, 
     &    PARSE_HEADERS, UPCASE, WARNING

      CHARACTER*(*), INTENT(IN) :: LABEL
      REAL, INTENT(OUT) :: Value

      INTEGER C1, C2, ERR, FOUND, I, J, ISECT, LINC, LNUM
      INTEGER LENGTH, LUNECO, LUNIO, PATHL, LENSTRING
      INTEGER, PARAMETER :: MAXCOL = 30  !Max number of ecotype columns
      INTEGER iCOUNT, COL(MAXCOL,2)

      CHARACTER*1, PARAMETER :: BLANK = ' '
      CHARACTER*1 UPCASE
      CHARACTER*5 MODEL
      CHARACTER*6 ECONO, ECOTYP, SECTION
      CHARACTER*7, PARAMETER :: ERRKEY = 'IPECO'
      CHARACTER*12 FILEIO, FILEE 
      CHARACTER*15 HTXT
      CHARACTER*80 PATHEC
      CHARACTER*92 FILEGC
      CHARACTER*92 MSG(4)
      CHARACTER*200 HEADERLINE, TEXTLINE

!     Array of headers and text value of ecotype parameters. 
!     Each header can be up to 15 characters long
      CHARACTER*15 HEADER(MAXCOL) 
!     Values are stored as text because they may contain both character and numeric values
      CHARACTER*15 TEXTVAL(MAXCOL) 

      LOGICAL ECOFOUND

      TYPE (ControlType) CONTROL
!***********************************************************************

      IF (TRIM(LABEL) .EQ. 'NEW') THEN
!       This is a new simulation, 
!       Ecotype data has not been extracted yet.
        CALL GET(CONTROL)
        FILEIO  = CONTROL % FILEIO
        LUNIO   = CONTROL % LUNIO

!       Read name and path of ecotype file from FILEIO
        OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
        
        READ (LUNIO,105,IOSTAT=ERR) FILEE, PATHEC; LNUM = LNUM + 1
  105   FORMAT(///////,15X,A12,1X,A80)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
        
!       Read Cultivar Section to get ecotype name
        SECTION = '*CULTI'
        CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
!       Need the 2nd cultivar section at the bottom of FileIO
        CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILEIO, LNUM)
        ELSE
          READ(LUNIO,'(24X,A6)',IOSTAT=ERR) ECONO ; LNUM = LNUM + 1
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
        ENDIF
        
        CLOSE (LUNIO)

!-----------------------------------------------------------------------
!       Open ecotype file
        LNUM = 0
        PATHL  = INDEX(PATHEC,BLANK)
        IF (PATHL .LE. 1) THEN
          FILEGC = FILEE
        ELSE
          FILEGC = PATHEC(1:(PATHL-1)) // FILEE
        ENDIF
        
        CALL GETLUN('FILEE', LUNECO)
        OPEN (LUNECO,FILE = FILEGC, STATUS = 'OLD', IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,0)

        MODEL = FILEE(1:5)

!-----------------------------------------------------------------------

!       Look for 1st header line beginning with '@' in column 1 (ISECT = 3)
        DO WHILE (.TRUE.)   !
          CALL IGNORE2 (LUNECO, LNUM, ISECT, HEADERLINE)
          SELECT CASE(ISECT)
          CASE(0)                                     !End of file
            CALL ERROR(ERRKEY,3,FILEGC,LNUM)
          CASE(1); CYCLE                              !data line
          CASE(2); CYCLE                              !End of section 
          CASE(3); EXIT                               !Header line 
          END SELECT
        ENDDO
        
!       Found header line for ecotype file
        CALL PARSE_HEADERS(HEADERLINE, MAXCOL, HEADER, ICOUNT, COL)
        IF (ICOUNT .LT. 1) CALL ERROR (ERRKEY,3,FILEGC,LNUM)
        DO I = 1, ICOUNT
          HTXT = HEADER(I)
          DO J = 1, LEN(TRIM(HTXT))
            HTXT(J:J) = UPCASE(HTXT(J:J))
          END DO
          HEADER(I) = HTXT
          WRITE(5656,*) HEADER(I), COL(I,1), COL(I,2)
        ENDDO
        
        ECOFOUND = .FALSE.
!       Look for correct ecotype line
        DO WHILE (.TRUE.)   !
          CALL IGNORE (LUNECO, LNUM, ISECT, TEXTLINE)
          SELECT CASE(ISECT)
          CASE(0); EXIT                               !End of file 
        
          CASE(1)                                     !data line
!           Found a line of ecotype data. Is it the right one?
            READ(TEXTLINE(COL(1,1):COL(1,2)+1),*,IOSTAT=ERR) ECOTYP
            IF (ECOTYP .EQ. ECONO) THEN
              ECOFOUND = .TRUE.
              TEXTVAL(1) = ECOTYP
              DO I = 2, ICOUNT
                C1 = COL(I,1)
                C2 = COL(I,2)
                TEXTVAL(I) = TEXTLINE(C1:C2)
                LENGTH = LenString(TEXTVAL(I))
                IF (LENGTH < 1) THEN
                  WRITE(MSG(1),'(A,A)') 
     &              "Ecotype value missing for parameter ", HEADER(I)
                  WRITE(MSG(2),'(A,A)') "Ecotype: ", ECOTYP
                  MSG(3) = FILEGC
                  CALL WARNING(3,ERRKEY,MSG) 
                  CALL ERROR(ERRKEY,4,FILEGC,LNUM)
                ENDIF
              ENDDO
              EXIT
            ENDIF

          CASE(2); EXIT                               !End of section 
          CASE(3); EXIT                               !Header line 
          END SELECT
        ENDDO
        
        IF (.NOT. ECOFOUND) THEN
          WRITE(MSG(1),'(A,A,A)')'Ecotype ',ECONO, ' not found in file:'
          MSG(2) = FILEGC
          MSG(3) = "Program will stop."
          CALL WARNING(3, ERRKEY, MSG)
          CALL ERROR(ERRKEY,3,FILEGC,LNUM)
        ENDIF
        CLOSE (LUNECO)

!       Return a dummy value
        Value = -99.

!-----------------------------------------------------------------------
!     Ecotype info is already in memory, just send back the requested value

      ELSE
        Value = -99.
        DO I = 2, ICOUNT
          IF (TRIM(HEADER(I)) .EQ. TRIM(LABEL)) THEN
            READ (TEXTVAL(I),*,IOSTAT=ERR) Value
            IF (ERR .NE. 0) THEN
              WRITE(MSG(1),'(A,A)') 
     &          HEADER(I),' contains non-numeric data.'
              MSG(2) = "Program will stop."
              CALL WARNING(2, ERRKEY, MSG)
              CALL ERROR(ERRKEY,1,FILEGC,LNUM)
            ENDIF
            EXIT
          ENDIF
        ENDDO

!       Error checking is crop specific
        IF (I > ICOUNT) THEN
!         Parameter not found in ECO file, check to see if it's required for this crop model
          ERR = 4  !assume the missing parameter is needed, check for exclusions below

!         Some strawberry model parameters not needed for other crops
          IF (TRIM(LABEL) == 'XFPHT' .AND. MODEL /= 'SRGRO') ERR = 0
          IF (TRIM(LABEL) == 'XFINT' .AND. MODEL /= 'SRGRO') ERR = 0

!         Some cotton model parameters not needed for other crops
          IF (TRIM(LABEL) == 'PCTLT' .AND. MODEL /= 'COGRO') ERR = 0

!         Tomato, pepper, strawberry, green bean use XMAGE. Other crops don't
          IF (TRIM(LABEL) == 'XMAGE') THEN
            IF (INDEX('TMGRO PRGRO SRGRO GBGRO',MODEL) < 1) ERR = 0
          ENDIF

!         G0GRO
          IF (TRIM(LABEL) == 'THRSH' .AND. MODEL /= 'G0GRO') ERR = 0
          IF (TRIM(LABEL) == 'SDPRO' .AND. MODEL /= 'G0GRO') ERR = 0
          IF (TRIM(LABEL) == 'SDLIP' .AND. MODEL /= 'G0GRO') ERR = 0

          IF (ERR > 0) THEN
            MSG(1) = "Ecotype variable not found."
            MSG(2) = "Variable: " // LABEL
            MSG(3) = "File: " // FILEGC
            MSG(4) = "Simulations terminated."
            CALL WARNING(4, ERRKEY, MSG)
            CALL ERROR(ERRKEY,4,FILEGC,LNUM)
          ENDIF
        ENDIF
      ENDIF

      RETURN
      END SUBROUTINE ECO_read
!============================================================================================
