C=======================================================================
C  OPSWBL, Subroutine, Fabio Oliveira, Gerrit Hoogenboom
C  Generates output for daily soil water data by layer
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  09/14/2024  FO Written
C=======================================================================
      SUBROUTINE OPSWBL(CONTROL, ISWITCH, 
     &    SOILPROP, SW)                                           !Input

!-----------------------------------------------------------------------
      USE ModuleDefs

      IMPLICIT NONE
      EXTERNAL YR_DOY, GETLUN, HEADER, SoilLayerText2
      SAVE

      CHARACTER*1 IDETW, IDETL, ISWWAT, RNMODE, FMOPT
      CHARACTER*13 OUTSWL
      PARAMETER (OUTSWL = 'SoilWater.OUT')

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, L, I
      INTEGER NLAYR, NOUTSW, RUN
      INTEGER YEAR, YRDOY, REPNO

      REAL, DIMENSION(NL) :: SW

      LOGICAL FEXIST, DOPRINT

      CHARACTER*8, DIMENSION(NL) :: LayerText,
     &  LL_txt, DUL_txt, SAT_txt, BD_txt, SW_txt
!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (SoilType)    SoilProp

      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FROP    = CONTROL % FROP
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE
      REPNO   = CONTROL % REPNO
      YRDOY   = CONTROL % YRDOY

      IDETL   = ISWITCH % IDETL
      IDETW   = ISWITCH % IDETW
      ISWWAT  = ISWITCH % ISWWAT
      FMOPT   = ISWITCH % FMOPT

      NLAYR   = SOILPROP % NLAYR

      CALL YR_DOY(YRDOY, YEAR, DOY) 
!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
C-----------------------------------------------------------------------
C   Set initial values to calculate average values
C-----------------------------------------------------------------------
        IF (IDETW == 'N' .OR. ISWWAT == 'N' .OR. IDETL == '0') THEN
          DOPRINT = .FALSE.
        ELSE
          DOPRINT = .TRUE.
        ENDIF

        IF (.NOT. DOPRINT) RETURN
!-----------------------------------------------------------------------
C   Generate headings for output file
C-----------------------------------------------------------------------
        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
        CALL GETLUN('OUTSWL', NOUTSW)
        INQUIRE (FILE = OUTSWL, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTSW, FILE = OUTSWL, STATUS = 'OLD',
     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = NOUTSW, FILE = OUTSWL, STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
          WRITE(NOUTSW,'("*SOIL WATER BY LAYER DAILY OUTPUT FILE")')
        ENDIF
        END IF
C-----------------------------------------------------------------------
C     Variable heading
C-----------------------------------------------------------------------
        IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1) THEN
          IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN
          IF (RNMODE .EQ. 'Q') THEN
            CALL HEADER(SEASINIT, NOUTSW, REPNO)
          ELSE
            CALL HEADER(SEASINIT, NOUTSW, RUN)
          ENDIF
          END IF 

  !       Use revised Soil layer text which uses all soil layers
          CALL SoilLayerText2(SOILPROP%DS, NLAYR,LayerText)
  

          IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN
            IF (.NOT. FEXIST) THEN
              WRITE(NOUTSW,'(A)') '',
     &    '!----------------------------',
     &    '! Variable Descriptions (unit)',
     &    '!----------------------------',
     &    '!YEAR    Year of current date of simulation (YYYY)',
     &    '!DOY     Day of year (d)',
     &    '!DAS     Days after start of simulation (d)',
     &    '!LL#D    Volumetric soil water content at Lower Limit' //
     &    ' in soil layer # (cm3 [water] / cm3 [soil])',
     &    '!DUL#D   Volumetric soil water content at'//
     &    ' Drained Upper Limit in soil layer # (cm3[water]/cm3[soil])',
     &    '!SAT#D   Volumetric soil water content at Saturation'//
     &    ' in soil layer # (cm3 [water] / cm3 [soil])',
     &    '!BD#D    Bulk density in soil layer # ' //
     &    '(g [soil] / cm3 [soil])',
     &    '!SW#D    Volumetric soil water content in soil layer # '//
     &    ' (cm3 [water] / cm3 [soil])',
     &    ''
            ENDIF

!         Write same headers for layered soil properties variables
            WRITE(NOUTSW,'(A,15X)', ADVANCE='NO') '!' 
            DO I = 1, 5
              WRITE(NOUTSW,'(20A8)', ADVANCE='NO') 
     &                       (LayerText(L), L=1,NLAYR)
            ENDDO

            WRITE (NOUTSW,'(/,A,1X)',ADVANCE='NO') '@YEAR DOY   DAS'

            DO L = 1, NLAYR
              IF (L < 10) THEN
                WRITE(LL_txt(L), '("    LL",I1,"D")') L
                WRITE(DUL_txt(L),'("   DUL",I1,"D")') L
                WRITE(SAT_txt(L),'("   SAT",I1,"D")') L
                WRITE(BD_txt(L), '("    BD",I1,"D")') L
                WRITE(SW_txt(L), '("    SW",I1,"D")') L
              ELSE
                WRITE(LL_txt(L), '("   LL",I2,"D")') L
                WRITE(DUL_txt(L),'("  DUL",I2,"D")') L
                WRITE(SAT_txt(L),'("  SAT",I2,"D")') L
                WRITE(BD_txt(L), '("   BD",I2,"D")') L
                WRITE(SW_txt(L), '("   SW",I2,"D")') L
              ENDIF
            ENDDO
            WRITE(NOUTSW,'(5(20(A8)))') 
     &         (LL_txt(L),  L=1,NLAYR),
     &         (DUL_txt(L), L=1,NLAYR),
     &         (SAT_txt(L), L=1,NLAYR),
     &         (BD_txt(L),  L=1,NLAYR),
     &         (SW_txt(L),  L=1,NLAYR)
          END IF
      
        ENDIF
!***********************************************************************
!***********************************************************************
!     Daily Output
!***********************************************************************
      ELSEIF (DYNAMIC == OUTPUT) THEN
C-----------------------------------------------------------------------
        IF (DOPRINT) THEN
          IF (DYNAMIC .EQ. OUTPUT .AND. MOD(DAS, FROP) .EQ. 0) THEN
            IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN 

              WRITE (NOUTSW,1300)YEAR, DOY, MOD(DAS,100000),
     &                      (SOILPROP % LL(L) ,L=1,NLAYR),
     &                      (SOILPROP % DUL(L),L=1,NLAYR),
     &                      (SOILPROP % SAT(L),L=1,NLAYR),
     &                      (SOILPROP % BD(L) ,L=1,NLAYR),
     &                      (SW(L),L=1,NLAYR)

 1300         FORMAT(X,I4,1X,I3.3,1X,I5,1X
     &               5(20(F8.4)))

            ENDIF
          ENDIF
        ENDIF
!***********************************************************************
!***********************************************************************
!     SEASEND - Sesaonal Output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
        CLOSE (NOUTSW)
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OPSWBL
!***********************************************************************
!-----------------------------------------------------------------------
!     OPSWBL VARIABLE DEFINITIONS:  updated 09/14/2024
!-----------------------------------------------------------------------
! CONTROL   Composite variable containing variables related to control 
!             and/or timing of simulation.    See Appendix A. 
! DAS       Days after start of simulation (d)
! DOY       Current day of simulation (d)
! ERRNUM    Error number for input 
! FEXIST    Logical variable 
! FLOODWAT  Composite variable containing information related to bund 
!             management. Structure of variable is defined in ModuleDefs.for. 
! FROP      Frequency of output (d)
! IDETW     Y=detailed water balance output, N=no detailed output 
! ISWITCH   Composite variable containing switches which control flow of 
!             execution for model.  The structure of the variable 
!             (SwitchType) is defined in ModuleDefs.for. 
! ISWWAT    Water simulation control switch (Y or N) 
! LL(L)     Volumetric soil water content in soil layer L at lower limit
!            (cm3 [water] / cm3 [soil])
! DUL(L)    Volumetric soil water content at Drained Upper Limit in soil 
!             layer L (cm3[water]/cm3[soil])
! SAT(L)    Volumetric soil water content in layer L at saturation
!             (cm3 [water] / cm3 [soil])
! BD(L)         Bulk density, soil layer L (g [soil] / cm3 [soil])
! NLAYR     Actual number of soil layers 
! NOUTSW    Unit number for water balance output file 
! REPNO     Replication number for current simulation 
! RNMODE    Simulation run mode (I=Interactive, A=All treatments, 
!             B=Batch mode, E=Sensitivity, D=Debug, N=Seasonal, Q=Sequence)
! RUN       Change in date between two observations for linear 
!             interpolation 
! SW(L)     Volumetric soil water content in layer L
!            (cm3 [water] / cm3 [soil])
! YEAR      Year of current date of simulation 
! YRDOY     Current day of simulation (YYYYDDD)
!-----------------------------------------------------------------------



