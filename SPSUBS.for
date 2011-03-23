C=======================================================================
C  OPSPAM, Subroutine, C.H.Porter from Soil Water portions of OPDAY
C  Generates output for daily soil water data
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  11/16/2001 CHP Written
C  06/07/2002 GH  Modified for crop rotations
C  08/20/2002 GH  Modified for Y2K
C  02/04/2005 CHP Added new variables to Summary.out: EPCM, ESCM
C-----------------------------------------------------------------------
C  Called from:   SPAM
C  Calls:         None
C=======================================================================
      SUBROUTINE OPSPAM(CONTROL, ISWITCH, FLOODWAT,
     &    CEF, CEM, CEO, CEP, CES, CET, EF, EM, 
     &    EO, EOP, EOS, EP, ES, ET, TMAX, TMIN, SRAD,
     &    ES_LYR, SOILPROP)
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      USE FloodModule

      IMPLICIT NONE
      SAVE

      CHARACTER*1  IDETW, ISWWAT, RNMODE
      CHARACTER*8  OUTET
      CHARACTER*50 FMT

      INTEGER DAS, DOY, DYNAMIC, FROP, LUN
      INTEGER NAVWB, RUN, YEAR, YRDOY, L
      INTEGER REPNO, N_LYR

      REAL EF, EM, EO, EP, ES, ET, EOS, EOP
      REAL CEF, CEM, CEO, CEP, CES, CET
      REAL ESAA, EMAA, EPAA, ETAA, EFAA, EOAA, EOPA, EOSA
      REAL AVTMX, AVTMN, AVSRAD
      REAL TMAX, TMIN, SRAD
!      REAL SALB, SWALB, MSALB, CMSALB
      REAL ES_LYR(NL), ES10
      LOGICAL FEXIST

!     Arrays which contain data for printing in SUMMARY.OUT file
      INTEGER, PARAMETER :: SUMNUM = 3
      CHARACTER*4, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH
      TYPE (FloodWatType) FLOODWAT
      TYPE (SoilType) SOILPROP

      ISWWAT  = ISWITCH % ISWWAT
      IF (ISWWAT == 'N') RETURN

      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FROP    = CONTROL % FROP
      REPNO   = CONTROL % REPNO
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE
      YRDOY   = CONTROL % YRDOY

      IDETW   = ISWITCH % IDETW

      CEF     = FLOODWAT % CEF

!      SALB   = SOILPROP % SALB       
!      SWALB  = SOILPROP % SWALB  
!      MSALB  = SOILPROP % MSALB  
!      CMSALB = SOILPROP % CMSALB 

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
        OUTET = 'ET.OUT'
        CALL GETLUN('OUTET', LUN)

        INQUIRE (FILE = OUTET, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = LUN, FILE = OUTET, STATUS = 'OLD',
     &      POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = LUN, FILE = OUTET, STATUS = 'NEW')
          WRITE(LUN,'("*SOIL-PLANT-ATMOSPHERE MODULE OUTPUT FILE")')
        ENDIF

C-----------------------------------------------------------------------
C     Variable heading for ET.OUT
C-----------------------------------------------------------------------
        IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1) THEN

          !For sequenced run, use replicate
          ! number instead of run number in header.
          IF (RNMODE .EQ. 'Q') THEN
            CALL HEADER(SEASINIT, LUN, REPNO)
          ELSE
            CALL HEADER(SEASINIT, LUN, RUN)
          ENDIF

          IF (ISWITCH % MESEV == 'S') THEN
!           Include soil evap by soil layer for Suleiman-Ritchie method

!           Number of soil layers to print between 4 and 10.
            N_LYR = MIN(10, MAX(4,SOILPROP%NLAYR))
            WRITE(LUN,'("!",T146,
     &        "Soil evaporation (mm/d) by soil depth (cm):"
     &        ,/,"!",T141,10A8)') (SoilProp%LayerText(L), L=1,N_LYR)

            WRITE (LUN,120,ADVANCE='NO')
  120       FORMAT('@YEAR DOY   DAS   SRAA  TMAXA  TMINA',
     &      '   EOAA   EOPA   EOSA',
     &      '   ETAA   EPAA   ESAA   EFAA   EMAA',
     &      '    EOAC    ETAC    EPAC    ESAC    EFAC    EMAC') 
!     &      '   SALB  SWALB  MSALB CMSALB',    

            IF (N_LYR < 10) THEN
              WRITE (LUN,121) ("ES",L,"D",L=1,N_LYR)
  121         FORMAT(9("    ",A2,I1,A1))
            ELSE
              WRITE (LUN,122) ("ES",L,"D",L=1,9), "    ES10"
  122         FORMAT(9("    ",A2,I1,A1),A8)
            ENDIF
          ELSE
            WRITE (LUN,120)
          ENDIF
        ENDIF

C-----------------------------------------------------------------------
C   Set initial values to calculate average values
C-----------------------------------------------------------------------
        NAVWB = 0
        EFAA  = 0.
        EMAA  = 0.
        EPAA  = 0.
        ESAA  = 0.
        ETAA  = 0.
        EOAA  = 0.
        EOPA  = 0.
        EOSA  = 0.
        AVTMX = 0.
        AVTMN = 0.
        AVSRAD= 0.

!***********************************************************************
!***********************************************************************
!     Daily integration
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
C-----------------------------------------------------------------------
C   Summations for calculation of average values per print interval
C-----------------------------------------------------------------------
      NAVWB  = NAVWB  + 1
      EFAA   = EFAA   + EF
      EMAA   = EMAA   + EM
      EPAA   = EPAA   + EP
      ESAA   = ESAA   + ES
      ETAA   = ETAA   + ET
      EOAA   = EOAA   + EO
      EOPA   = EOPA   + EOP
      EOSA   = EOSA   + EOS
      AVTMX  = AVTMX  + TMAX
      AVTMN  = AVTMN  + TMIN
      AVSRAD = AVSRAD + SRAD

!***********************************************************************
!***********************************************************************
!     Daily Output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
      IF (IDETW .EQ. 'Y') THEN

        IF ((DYNAMIC .EQ. OUTPUT .AND. MOD(DAS,FROP) .EQ. 0) .OR.
     &      (DYNAMIC .EQ. SEASEND  .AND. MOD(DAS,FROP) .NE. 0) .OR.
     &       DAS == 1) THEN 

C         Calculate average values as a function of the output interval
C-----------------------------------------------------------------------
          IF (NAVWB < 1) NAVWB = 1
          EFAA  = EFAA / NAVWB
          EMAA  = EMAA / NAVWB
          EPAA  = EPAA / NAVWB
          ESAA  = ESAA / NAVWB
          ETAA  = ETAA / NAVWB
          EOAA  = EOAA / NAVWB
          EOPA  = EOPA / NAVWB
          EOSA  = EOSA / NAVWB
          AVTMX = AVTMX / NAVWB
          AVTMN = AVTMN / NAVWB
          AVSRAD= AVSRAD / NAVWB

          CALL YR_DOY(YRDOY, YEAR, DOY) 

          !Daily printout
          FMT = "(1X,I4,1X,I3.3,1X,I5,3(1X,F6.2),8(F7.3),"
          IF (CEO > 1000. .OR. CET > 1000. .OR. CEP > 1000. .OR. 
     &        CES > 1000. .OR. CEF > 1000. .OR. CEM > 1000.) THEN
            FMT = TRIM(FMT) // "6F8.0))"
          ELSE 
            FMT = TRIM(FMT) // "6F8.2))"
          ENDIF

          WRITE (LUN,FMT,ADVANCE='NO') YEAR, DOY, DAS, AVSRAD, AVTMX, 
     &        AVTMN, EOAA, EOPA, EOSA, ETAA, EPAA, ESAA, EFAA, EMAA,  
     &        CEO, CET, CEP, CES, CEF, CEM   
!     &        ,SALB, SWALB, MSALB, CMSALB
!  300     FORMAT(1X,I4,1X,I3.3,1X,I5,3(1X,F6.2),
!     &      8(F7.3),6(F8.2))     
!     &    ,4F7.2 ,10(F7.3))
          IF (ISWITCH % MESEV == 'S') THEN
            IF (SOILPROP % NLAYR < 11) THEN
              WRITE(LUN,'(10F8.3)') ES_LYR(1:N_LYR)
            ELSE
              ES10 = 0.0
              DO L = 10, SOILPROP % NLAYR
                ES10 = ES10 + ES_LYR(L)
              ENDDO
              WRITE(LUN,'(10F8.3)') ES_LYR(1:9), ES10
            ENDIF    
          ELSE
            WRITE(LUN,'(" ")')
          ENDIF

          NAVWB = 0
          EFAA  = 0.
          EMAA  = 0.
          EPAA  = 0.
          ESAA  = 0.
          ETAA  = 0.
          EOAA  = 0.
          EOPA  = 0.
          EOSA  = 0.
          AVTMX = 0.
          AVTMN = 0.
          AVSRAD= 0.

        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal Output
!***********************************************************************
        IF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
          !IF (IDETS .EQ. 'Y' .OR. IDETS .EQ. 'A') THEN
!           Store Summary.out labels and values in arrays to send to
!           OPSUM routines for printing.  Integers are temporarily 
!           saved aS real numbers for placement in real array.
            LABEL(1)  = 'ETCM'; VALUE(1)  = CET
            LABEL(2)  = 'EPCM'; VALUE(2)  = CEP
            LABEL(3)  = 'ESCM'; VALUE(3)  = CES

            !Send labels and values to OPSUM
            CALL SUMVALS (SUMNUM, LABEL, VALUE) 
          !ENDIF

          !Close daily output files.
          CLOSE (LUN)
        ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END !SUBROUTINE OPSPAM
!***********************************************************************
!-----------------------------------------------------------------------
!     OPSPAM VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! EOAA    Average potential evapotranspiration since last printout (mm/d)
! EPAA    Average plant transpiration since last printout (mm/d)
! ESAA    Average soil evaporation since last printout (mm/d)
! EMAA    Average mulch evaporation since last printout (mm/d)
! ETAA    Average evapotranspiration since last printout (mm/d)
! AVSRAD  Average solar radiation since last printout (MJ/m2-d)
! AVTMN   Average min temperature since last printout (�C)
! AVTMX   Average max temperature since last printout (�C)
! AVWTD   Average water table depth since last printout (cm)
! CEP     Cumulative transpiration (mm)
! CES     Cumulative evaporation (mm)
! CET     Cumulative evapotranspiration (mm)
! CRAIN   Cumulative precipitation (mm)
! EM      Actual mulch evaporation rate (mm/d)
! EO      Potential evapotranspiration rate (mm/d)
! EP      Actual plant transpiration rate (mm/d)
! ES      Actual soil evaporation rate (mm/d)
! ET      Actual evapotranspiration rate (mm/d)
! MODEL   Name of CROPGRO executable file 
! NAP     Number of irrigation applications 
! NAVWB   Number of days since last printout (d)
! NL      Maximum number of soil layers = 20 
! LUN     Unit number for spam output file 
! OUTW    Filename for soil water output file (set in IPIBS) 
! ST(L)   Soil temperature in soil layer L (�C)
! SW(L)   Volumetric soil water content in layer L
!           (cm3 [water] / cm3 [soil])
! TDRAIN  Cumulative daily drainage from profile (mm)
! TIMDIF  Integer function which calculates the number of days between two 
!           Julian dates (da)
! TMAX    Maximum daily temperature (�C)
! TMIN    Minimum daily temperature (�C)
! TOTIR   Total seasonal irrigation (mm)
! TRUNOF  Cumulative runoff (mm)
! WTDEP   Water table depth  (cm)
! YRDOY   Current day of simulation (YYDDD)
! YRPLT   Planting date (YYDDD)
!-----------------------------------------------------------------------
!     END OPSPAM Subroutine
!-----------------------------------------------------------------------


C=======================================================================
C  XTRACT, Subroutine, J.T. Ritchie
C  Calculates root extraction for each soil layer.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/89 JR  Written
C  12/05/93 NBP Made into subroutine
C  07/11/96 GH  Set TRWU and RWU to 0 if EP = 0
!  10/13/97 CHP Modified for modular format.
!-----------------------------------------------------------------------
!  Called by: SPAM
!  Calls:     None
C=======================================================================
      SUBROUTINE XTRACT(
     &    NLAYR, DLAYR, LL, SW, SW_AVAIL, TRWUP,          !Input
     &    EP, RWU,                                        !Input/Output
     &    SWDELTX, TRWU)                                  !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
!     NL defined in ModuleDefs.for

      IMPLICIT NONE
      SAVE
      INTEGER NLAYR
      INTEGER L

      REAL EP, TRWU, TRWUP
      REAL WUF
      REAL DLAYR(NL), LL(NL), SW(NL)
      REAL RWU(NL)
      REAL  SWDELTX(NL),    !Change in SW due to root extraction
     &      SWTEMP(NL),     !New SW value based only on root extraction
     &      SW_AVAIL(NL)    !Water available for root extraction

!-----------------------------------------------------------------------
      DO L = 1, NLAYR
        SWDELTX(L) = 0.0
        SWTEMP(L) = SW(L)
        SW_AVAIL(L) = MAX(0.0,SW_AVAIL(L) - LL(L))
      ENDDO
      TRWU = TRWUP

      IF (EP .GT. 0.0) THEN
        IF ((0.1 * EP) .LE. TRWUP) THEN
          WUF = 0.1 * EP / TRWUP
        ELSE
          WUF = 1.0
        ENDIF

        TRWU = 0.0
        DO L = 1, NLAYR
          IF (SWTEMP(L) .GT. LL(L)) THEN
            RWU(L) = RWU(L) * WUF
            IF (RWU(L) / DLAYR(L) .GT. SW_AVAIL(L)) THEN
              RWU(L) = SW_AVAIL(L) * DLAYR(L)
            ENDIF
            SWTEMP(L) = SWTEMP(L) - RWU(L) / DLAYR(L)
          TRWU = TRWU + RWU(L)
          ENDIF
        END DO

        EP = TRWU * 10.

      ELSE        !No root extraction of soil water
        TRWU = 0.0
        RWU  = 0.0
      ENDIF

      DO L = 1, NLAYR
        SWDELTX(L) = SWTEMP(L) - SW(L)
      ENDDO

      RETURN
      END SUBROUTINE XTRACT

!-----------------------------------------------------------------------
!     XTRACT VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! DLAYR(L)    Soil thickness in layer L (cm)
! EP          Actual plant transpiration rate (mm/d)
! LL(L)       Volumetric soil water content in soil layer L at lower limit
!               (cm3/cm3)
! NL          Maximum number of soil layers = 20 
! NLAYR       Actual number of soil layers 
! RWU(L)      Root water uptake from soil layer L (cm/d)
! SW(L)       Volumetric soil water content in layer L
!               (cm3 [water] / cm3 [soil])
! SW_AVAIL(L) Soil water content in layer L available for evaporation, 
!               plant extraction, or movement through soil
!               (cm3 [water] / cm3 [soil])
! SWDELTX (L) Change in soil water content due to root uptake in layer L
!               (cm3/cm3)
! SWTEMP(L)   Soil water content in layer L (temporary value to be modified 
!               based on drainage, root uptake and upward flow through soil 
!               layers). (cm3/cm3)
! TRWU        Total potential daily root water uptake (cm/d)
! WUF         Root water uptake reduction factor 
!-----------------------------------------------------------------------
!     END SUBROUTINE XTRACT
C=======================================================================
!     END SPAM MODULE
!=======================================================================

