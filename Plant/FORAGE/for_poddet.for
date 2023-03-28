C=======================================================================
C  FOR_PODDET, Subroutine, W. D. Batchelor
C-----------------------------------------------------------------------
C  Computes pod detachment rates.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/93 WDB Written.
C  04/24/94 NBP Changed TAIRHR to TGRO.
C  02/02/98 GH  Fixed dimensions of TB,TO,TO2,TM
C  07/18/98 CHP Modified for modular format
C  05/11/99 GH  Incorporated in CROPGRO
C-----------------------------------------------------------------------
!  Called from:  PLANT
!  Calls:        ERROR, FIND, IGNORE
C=======================================================================

      SUBROUTINE FOR_PODDET(
     &  FILECC, TGRO, WTLF, YRDOY, YRNR2,                 !Input
     &  PODWTD, SDNO, SHELN, SWIDOT,                      !Output
     &  WSHIDT, WTSD, WTSHE,                              !Output
     &  DYNAMIC)                                          !Control

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE, CURV
      SAVE

      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = 'PODDET')

      CHARACTER*6 SECTION
      CHARACTER*80 C80
      CHARACTER*92 FILECC

      INTEGER LUNCRP, ERR, LNUM, FOUND, ISECT, I
      INTEGER DYNAMIC, YRDOY
      INTEGER YRNR2, NPP

      REAL SWIDOT,WSHIDT,WTLF
      REAL XPD,PODWTD,DWC
      REAL TPODM,RLMPM,SL10
      REAL FT
      REAL FTHR,CURV
      REAL PR1DET,PR2DET,XP1DET,XP2DET
      REAL SDDAM,SHDAM,SUMSD,SUMSH

      REAL TB(5), TO1(5), TO2(5), TM(5)
      REAL TDLM(20)
      REAL TGRO(TS)
      REAL WTSD(NCOHORTS), SDNO(NCOHORTS), WTSHE(NCOHORTS)
      REAL WPODY(NCOHORTS), SHELN(NCOHORTS), PDET(NCOHORTS)
      REAL DAYS(NCOHORTS), MSHELN(NCOHORTS), DTC(NCOHORTS)

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!     Read in values from input file, which were previously input
!       in Subroutine IPCROP.
!-----------------------------------------------------------------------
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)
!-----------------------------------------------------------------------
!    Find and Read Pod Loss Section
!-----------------------------------------------------------------------
!     Subroutine FIND finds appropriate SECTION in a file by
!     searching for the specified 6-character string at beginning
!     of each line.
!-----------------------------------------------------------------------
      LNUM = 1
      SECTION = '!*POD '
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(6X,5F6.0)',IOSTAT=ERR)
     &    DWC, PR1DET, PR2DET, XP1DET, XP2DET
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

!-----------------------------------------------------------------------
!    Find and Read Phenology Section
!-----------------------------------------------------------------------
      SECTION = '!*PHEN'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4F6.0)',IOSTAT=ERR)TB(1),TO1(1),TO2(1),TM(1)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4F6.0)',IOSTAT=ERR)TB(2),TO1(2),TO2(2),TM(2)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4F6.0)',IOSTAT=ERR)TB(3),TO1(3),TO2(3),TM(3)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF

      CLOSE (LUNCRP)

      WSHIDT = 0.0
      WTSD   = 0.0
      WTSHE  = 0.0

!***********************************************************************
!***********************************************************************
!     EMERGENCE CALCULATIONS - Performed once per season upon emergence
!         or transplanting of plants
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. EMERG) THEN
!-----------------------------------------------------------------------
        DO 10 I = 1, NCOHORTS
        DTC(I)    = 0.0
        MSHELN(I) = 0.0
        WPODY(I)  = 0.0
        DAYS(I)   = 0.0
   10   ENDDO
        PODWTD = 0.0

!***********************************************************************
!***********************************************************************
!     DAILY RATE/INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
C     Compute thermal time using hourly predicted air temperature
C     based on observed max and min temperature.
C--------------------------------------------------------------------
      FT = 0.0
      DO I = 1, 24
        FTHR = CURV('LIN',TB(3),TO1(3),TO2(3),TM(3),TGRO(I))
        FT = FT + FTHR/24.
      END DO
C -------------------------------------------------------------------
C  Compute ratio of leaf area per pod cm2/pod
C  and leaf mass per pod mass g/g
C -------------------------------------------------------------------
      TPODM = 0.0
      RLMPM = 1.0

C -------------------------------------------------------------------
C      Compute 10 day running average of leaf mass and PGAVL
C -------------------------------------------------------------------
      DO 50 I=10,2,-1
        TDLM(I) = TDLM(I-1)
   50 ENDDO
      TDLM(1) = WTLF
C -------------------------------------------------------------------
C     Compute slope of leaf mass curve
C -------------------------------------------------------------------
      SL10 = (TDLM(1) - TDLM(10))/ 10.0

!---------------------------------------------------------------------
      IF (YRNR2 .GE. 0) THEN
!---------------------------------------------------------------------

        DO 40 NPP = 1, YRDOY - YRNR2
        TPODM = TPODM + WTSHE(NPP) + WTSD(NPP)
   40   ENDDO

        IF (TPODM .GT. 10.0) RLMPM = WTLF / TPODM
C -------------------------------------------------------------------
C     Main loop that cycles through detachment model
C--------------------------------------------------------------------
        DO 1000 NPP = 1, YRDOY - YRNR2
C--------------------------------------------------------------------
C     Determine maximum cohort shell mass and accumulate
C     days without carbohydrate on a cohort basis
C--------------------------------------------------------------------
        IF (SHELN(NPP) .GT. MSHELN(NPP)) THEN
        MSHELN(NPP) = SHELN(NPP)
        ENDIF
        IF (WTSD(NPP) + WTSHE(NPP) .GE. 0.01) THEN
        IF (WTSD(NPP) + WTSHE(NPP) .LE. WPODY(NPP) .AND.
     &    WTSD(NPP) .GT. 0.0) THEN
        DAYS(NPP) = DAYS(NPP) + 1.
        ENDIF

        IF (WTSD(NPP) + WTSHE(NPP) .GT. WPODY(NPP)) THEN
        DAYS(NPP) = 0
        ENDIF

C-----------------------------------------------------------------------
C     Accumulate pod detachment thermal time counter (DTC) based on
C     ratio of LFM/PDM and 10 day average slope of the leaf mass curve
C-----------------------------------------------------------------------
!           IF(RLMPM .GT. PR1DET .OR. SL10 .GT. PR2DET) GOTO 700
        IF (RLMPM .LE. PR1DET .AND. SL10 .LE. PR2DET) THEN
        IF((SL10 .LE. PR2DET) .OR. DAYS(NPP) .GT. DWC .OR.
     &    WTLF .LE. 10.) THEN
        DTC(NPP) = DTC(NPP) + FT
        ENDIF
        ELSE
C           Accumulate DTC based on days without carbon before RLMPM < PR1DET
C           and SL10 < PR2DET
        IF (DAYS(NPP) .GT. DWC .OR. WTLF .LE. 10.) THEN
        DTC(NPP) = DTC(NPP) + FT
        ENDIF
        ENDIF
C-----------------------------------------------------------------------
        ENDIF
 1000   ENDDO

C--------------------------------------------------------------------
C     Compute detachment for each cohort
!--------------------------------------------------------------------
        DO 2000 NPP = 1, YRDOY - YRNR2
C       curve based on Drew control, disease and Lowman tag pod cohort study
        IF (DTC(NPP) .GT. 0.) THEN
        XPD = MSHELN(NPP) * (1.0 - XP1DET*EXP(XP2DET*DTC(NPP))/100.)
        XPD = MAX(0.0,XPD)
        IF (SHELN(NPP) .GT. XPD) THEN
        IF (SHELN(NPP) .GE. 0.01 .AND. DTC(NPP) .LE. 34.) THEN
        PDET(NPP) = SHELN(NPP) - XPD
        PDET(NPP) = MAX(0.0,PDET(NPP))
        PODWTD = PODWTD + (WTSHE(NPP) + WTSD(NPP))*PDET(NPP) /
     &    SHELN(NPP)

        SDDAM =  WTSD(NPP) * PDET(NPP) / SHELN(NPP)
        IF (SDDAM .GT. WTSD(NPP)) THEN
        SWIDOT = SWIDOT + WTSD(NPP)
        ELSE
        SWIDOT = SWIDOT + SDDAM
        ENDIF

        SHDAM = WTSHE(NPP) * PDET(NPP) / SHELN(NPP)
        IF (SHDAM .GT. WTSHE(NPP)) THEN
        WSHIDT = WSHIDT + WTSHE(NPP)
        ELSE
        WSHIDT = WSHIDT + SHDAM
        ENDIF

        WTSD(NPP)  = WTSD(NPP) * (1. - PDET(NPP) / SHELN(NPP))
        SDNO(NPP)  = SDNO(NPP) * (1. - PDET(NPP) / SHELN(NPP))
        WTSHE(NPP) = WTSHE(NPP)* (1. - PDET(NPP) / SHELN(NPP))
        SHELN(NPP) = SHELN(NPP)* (1. - PDET(NPP) / SHELN(NPP))

        WTSHE(NPP) = MAX(0.0,WTSHE(NPP))
        SHELN(NPP) = MAX(0.0,SHELN(NPP))
        WTSD(NPP)  = MAX(0.0,WTSD(NPP))
        SDNO(NPP)  = MAX(0.0,SDNO(NPP))
        ENDIF
        ENDIF
        ENDIF
        WPODY(NPP) = WTSD(NPP) + WTSHE(NPP)
 2000   ENDDO

        SUMSD = 0.0
        SUMSH = 0.0
        DO 4000 NPP = 1, YRDOY - YRNR2
        SUMSD = SUMSD + WTSD(NPP)
        SUMSH = SUMSH + WTSHE(NPP)
 4000   ENDDO
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END ! SUBROUTINE FOR_PODDET
!=======================================================================

!***********************************************************************
!       Variable definitions
!***********************************************************************
! CURV      Function subroutine 
! DAYS(J)   Days without carbohydrate on a cohort basis (days)
! DTC       Pod detachment thermal time counter 
! DWC       Threshold number of days without carbon to trigger pod 
!             detachment (days)
! DYNAMIC   Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!             INTEGR, OUTPUT, or SEASEND 
! ERRKEY    Subroutine name for error file 
! FILECC    Path plus filename for species file (*.spe) 
! FT        Temperature function (0-1) 
! FTHR      Used to calculate hourly air temperature (°C)
! LUNCRP    Logical unit number for FILEC (*.spe file) 
! MSHELN(J) Maximum cohort shell mass (#/m2)
! NPP       Cohort number used as index in loops 
! PDET(J)   # of detached pods by cohort (# / m2)
! PODWTD    Mass of detached pods (g[pods] / m2[ground])
! PR1DET    Threshold for comparison of ratio of leaf mass to pod mass 
!             (RLMPM) 
! PR2DET    Threshold for comparison of slope of leaf mass (for 
!             defoliation) 
! RLMPM     Ratio of leaf mass to pod mass 
! SDDAM     Mass of seeds destroyed by detachment (g/m2)
! SDNO(J)   Number of seeds for cohort J (#/m2)
! SHDAM     Mass of shells destroyed by detachment (g/m2)
! SHELN(J)  Number of shells for cohort J (#/m2)
! SL10      Slope of leaf mass curve 
! SUMSD     Total seed mass (g/m2)
! SUMSH     Total shell mass (g/m2)
! SWIDOT    Daily seed mass damage (g/m2/day)
! TB,       |
! TO1,      | Coefficients which define daily temperature distribution:
! TO2,      | TB=base temp, T01=1st optimum, T02=2nd optimum, TM=max temp. (°C)
! TM        |
! TDLM      Last 10 days values of leaf mass (g[leaf] / m2[ground])
! TGRO(I)   Hourly air temperature (°C)
! TPODM     Total pod mass (g/m2)
! TS        Number of intermediate time steps (=24) 
! WPODY(J)  Pod mass  for cohort J (g/m2)
! WSHIDT    Weight of shell tissue consumed by pests today (g[shell]/m2-d)
! WTLF      Dry mass of leaf tissue including C and N
!             (g[leaf] / m2[ground])
! WTSD(J)   Seed mass  for cohort J (g/m2)
! WTSHE(J)  Shell mass  for cohort J (g/m2)
! XP1DET    Coefficient which defines pod detachment equation 
! XP2DET    Coefficient which defines pod detachment equation 
! XPD       Number of shells which can be supported by plant (?) (#/m2)
! YRDOY     Current day of simulation (YYDDD)
! YRNR2     Day when 50% of plants have one peg (peanuts only) (YYDDD)
!***********************************************************************
!      END SUBROUTINE FOR_PODDET
!=======================================================================
