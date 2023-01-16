!=======================================================================
!  OPGENERIC, Subroutine
!
!  Generates output for simulated data, up to 12 variables
!-----------------------------------------------------------------------
!  Revision history
!
!  09/22/2008 CHP Written
!=======================================================================

      SUBROUTINE OPGENERIC ( 
     &    NVars, Width, HeaderTxt, FormatTxt,  
     &    VAR1, VAR2, VAR3, VAR4, VAR5, VAR6, 
     &    VAR7, VAR8, VAR9, VAR10,VAR11,VAR12)

!-----------------------------------------------------------------------
! FormatTxt = Character string for format, e.g., "(2F8.3)"
! HeaderTxt = Character string for headers, e.g., "    TRWU   SWFAC"
! NVars     = Integer number of variables to be output, e.g. 2
! Width     = Number of characters in header text
! Var1 thru Var10 = Values to be output daily.  Can supply zero values for 
!               dummy variables, e.g., TRWU, SWFAC, 0., 0., 0., 0., 0., etc.
! All variables are real
!-----------------------------------------------------------------------

      USE ModuleDefs
      USE ModuleData
      IMPLICIT  NONE
      EXTERNAL GETLUN, HEADER, YR_DOY
      SAVE

      CHARACTER*80      , INTENT(IN) :: FormatTxt 
      CHARACTER*120     , INTENT(IN) :: HeaderTxt
      INTEGER           , INTENT(IN) :: NVars, Width
      REAL              , INTENT(IN) :: Var1, Var2, Var3, Var4, Var5
      REAL              , INTENT(IN) :: Var6, Var7, Var8, Var9, Var10
      REAL              , INTENT(IN) :: Var11, Var12

      CHARACTER*11, PARAMETER :: OUTG = 'Generic.OUT'
      CHARACTER*95 FMT_STRING
      INTEGER DAS, DOY, DYNAMIC, ERRNUM, LUN
      INTEGER RUN, YEAR, YRDOY
      LOGICAL FEXIST
      TYPE (ControlType) CONTROL
      CALL GET(CONTROL)

      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
!-----------------------------------------------------------------------
        CALL GETLUN('Generic', LUN)
      
        INQUIRE (FILE = OUTG, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = LUN, FILE = OUTG, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = LUN, FILE = OUTG, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(LUN,'("*Generic daily output")')
        ENDIF

!       Write headers
        CALL HEADER(SEASINIT, LUN, RUN)
        WRITE (LUN,'(A,A)') "@YEAR DOY   DAS", HeaderTxt(1:Width)

        FMT_STRING = "(1X,I4,1X,I3.3,1X,I5)"  

!***********************************************************************
!***********************************************************************
!     Daily OUTPUT
!***********************************************************************
      ELSEIF (DYNAMIC == OUTPUT .OR. 
     &        DYNAMIC == INTEGR .OR. 
     &        DYNAMIC == RATE) THEN

        CALL YR_DOY(YRDOY, YEAR, DOY)
        WRITE (LUN,FMT_STRING,ADVANCE='NO') YEAR, DOY, DAS
        SELECT CASE (NVars)
        CASE(1); WRITE(LUN,FormatTxt) VAR1
        CASE(2); WRITE(LUN,FormatTxt) VAR1,VAR2
        CASE(3); WRITE(LUN,FormatTxt) VAR1,VAR2,VAR3
        CASE(4); WRITE(LUN,FormatTxt) VAR1,VAR2,VAR3,VAR4
        CASE(5); WRITE(LUN,FormatTxt) VAR1,VAR2,VAR3,VAR4,VAR5
        CASE(6); WRITE(LUN,FormatTxt) VAR1,VAR2,VAR3,VAR4,VAR5,VAR6
        CASE(7); WRITE(LUN,FormatTxt) VAR1,VAR2,VAR3,VAR4,VAR5,VAR6,VAR7
        CASE(8); WRITE(LUN,FormatTxt) VAR1,VAR2,VAR3,VAR4,VAR5,VAR6,
     &                                VAR7,VAR8
        CASE(9); WRITE(LUN,FormatTxt) VAR1,VAR2,VAR3,VAR4,VAR5,VAR6,
     &                                VAR7,VAR8,VAR9
        CASE(10);WRITE(LUN,FormatTxt) VAR1,VAR2,VAR3,VAR4,VAR5,VAR6,
     &                                VAR7,VAR8,VAR9,VAR10
        CASE(11);WRITE(LUN,FormatTxt) VAR1,VAR2,VAR3,VAR4,VAR5,VAR6,
     &                                VAR7,VAR8,VAR9,VAR10,VAR11
        CASE(12);WRITE(LUN,FormatTxt) VAR1,VAR2,VAR3,VAR4,VAR5,VAR6,
     &                                VAR7,VAR8,VAR9,VAR10,VAR11,VAR12
        END SELECT

!***********************************************************************
!***********************************************************************
!     End of season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
!***********************************************************************
        CLOSE (LUN)

!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OPGENERIC
!=======================================================================
