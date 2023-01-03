C=======================================================================
!  FOR_RSTAGES Subroutine Modified from
C      STAGES, Subroutine, J. W. Jones
C  Calculates phenological stages and individual phase durations.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/93 J.W. Jones, K.J. Boote, G. Hoogenboom
!  08/../97 CHP modified for CROPGRO restructuring
C-----------------------------------------------------------------------
!     Called from: PHENOL
!     Calls:       None
C=======================================================================

      SUBROUTINE FOR_RSTAGES(CONTROL,
     &    FNSTR, FPSTR, FSW, FT, FUDAY, ISIMI, NPRIOR,    !Input
     &    PHTHRS, PLME, SDEPTH, YRDOY, YRPLT, YRSIM,      !Input
     &    CropStatus,                                     !Output
     &    JPEND, NDLEAF, NDSET, NDVST, NVALPH, NVEG0,     !Output
     &    NVEG1, NR1, NR2, NR5, NR7, PHZACC, RSTAGE,      !Output
     &    STGDOY, YREMRG, YRNR1, YRNR2, YRNR3,            !Output
     &    YRNR5, YRNR7, MDATE)                            !Output
!     &    DYNAMIC)                                        !Control

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      CHARACTER*1 ISIMI, PLME

      INTEGER DYNAMIC
      INTEGER I, J, NVALP0, DAS, YRDOY, YRPLT, YRSIM, CropStatus
      INTEGER NDLEAF, NDSET, NDVST, JPEND !, TIMDIF
      INTEGER RSTAGE, NVEG0, NVEG1, NR0, NR1, NR2, NR3, NR5, NR7
      INTEGER YRNR1, YRNR2, YRNR3, YRNR5, YRNR7, MDATE, YREMRG
      INTEGER NPRIOR(20), STGDOY(20), NVALPH(20)

      REAL PHTEM, SDEPTH
      REAL FT(20), FUDAY(20), FSW(20), FNSTR(20), FPSTR(20), PHTHRS(20)
      REAL PHZACC(20), PROG(20), REM(20)

      TYPE(ControlType) CONTROL

!      SAVE NVALP0

      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      NVALP0 = 10000
      RSTAGE = 0

      DO I = 1,20
        PHZACC(I) = 0.0
        NVALPH(I) = NVALP0
        STGDOY(I) = 999999
        PROG(I) = 0.
      ENDDO

      NVALPH(1) = 1
      STGDOY(14) = YRSIM
      STGDOY(15) = YRPLT

      NVEG0  = NVALP0
      NVEG1  = NVALP0
      JPEND  = NVALP0
      NR0    = NVALP0
      NR1    = NVALP0
      NR2    = NVALP0
      NR3    = NVALP0
      NR5    = NVALP0
      NDLEAF = NVALP0
      NDVST  = NVALP0
      NDSET  = NVALP0
      NR7    = NVALP0
!      NR8    = NVALP0
      YRNR1  = -99
      YRNR2  = -99
      YRNR3  = -99
      YRNR5  = -99
      YRNR7  = -99
      MDATE  = -99
      YREMRG = -99

C***********************************************************************
C***********************************************************************
C     Daily Integration 
C***********************************************************************
      ELSE IF (DYNAMIC .EQ. INTEGR) THEN

!-----------------------------------------------------------------------
!      DAS   = MAX(0,TIMDIF(YRSIM,YRDOY))
      DO  J = 1,20
        REM(J) = 1.0
      ENDDO

      IF (YRDOY .EQ. YRPLT) THEN
        STGDOY(15) = YRPLT
      ENDIF

C-----------------------------------------------------------------------
C     Transplants
C-----------------------------------------------------------------------
      IF (PLME .EQ. 'T' .AND. YRPLT .EQ. YRDOY) THEN
        NVEG0 = DAS
        NVALPH(2) = NVEG0
        YREMRG    = YRDOY
        IF (PHZACC(2) .GE. PHTHRS(2)) THEN
        NVEG1 = DAS
        NVALPH(3) = NVEG1
        PHZACC(3) = PHZACC(2) - PHTHRS(2)
        IF (PHZACC(3) .GE. PHTHRS(3)) THEN
        JPEND = DAS
        NVALPH(4) = JPEND
        PHZACC(4) = PHZACC(3) - PHTHRS(3)
        IF (PHZACC(4) .GE. PHTHRS(4)) THEN
        NR0 = DAS
        NVALPH(5) = NR0
        RSTAGE    = 0
        PHZACC(5) = PHZACC(4) - PHTHRS(4)
        IF (PHZACC(5) .GE. PHTHRS(5)) THEN
        NR1 = DAS
        NVALPH(6) = NR1
        YRNR1     = YRDOY
        RSTAGE    = 1
        PHZACC(6) = PHZACC(5) - PHTHRS(5)
        ENDIF
        ENDIF
        ENDIF
        ENDIF
      ENDIF

C-----------------------------------------------------------------------
C     Check for emergence, if NVEG0 has been set to less than its 
!         initial value
C-----------------------------------------------------------------------
      IF (NVEG0 .GE. NVALP0) THEN
        PHTEM = PHTHRS(1) + SDEPTH * 0.6
        PROG(1) = FT(1) * FUDAY(1) * MIN(FSW(1),FNSTR(1),FPSTR(1))
        PHZACC(1) = PHZACC(1) + PROG(1)

!       OPEN(150,FILE='TRANS.OUT',STATUS='UNKNOWN',ACCESS='APPEND')
!       WRITE(150,'(I5,3F8.3)') DAS, PHTEM, PROG(1), PHZACC(1)
!       WRITE(150,'(I5)') DAS
!       CLOSE(150)
        
        IF ((PHZACC(1) .GE. PHTEM) .OR. (ISIMI .EQ. 'E')) THEN

C-----------------------------------------------------------------------
C       Emergence, next stage, occurs on day DAS
C-----------------------------------------------------------------------
        NVEG0 = DAS
        NVALPH(2) = NVEG0
        YREMRG    = YRDOY
        STGDOY(1) = YRDOY
C-----------------------------------------------------------------------
C       Account for the part of today that contributes to the next phase(s)
C-------------------------------------------------------------------------------
        IF (ISIMI .NE. 'E') THEN
        REM(2) = (PHZACC(1) - PHTEM)/(PROG(1) + 0.00001)
        ENDIF
        ENDIF
      ENDIF

C-------------------------------------------------------------------------------
C     Check for veg stage, V1
C-------------------------------------------------------------------------------
C     Skip accumulator section if not time to start accumulating for phase 2
C          also skips this accumulator on day when stage 2 occurred, with
C          initial value of PHZACC(2) already computed
C-------------------------------------------------------------------------------
      IF ((DAS .GE. NVALPH(NPRIOR(2))) .AND. (NVEG1 .GE. NVALP0)) THEN
C-------------------------------------------------------------------------------
C     Skip section if stage 3 has already occurred
C-------------------------------------------------------------------------------
        PROG(2) = FT(2) * FUDAY(2) * MIN(FSW(2),FNSTR(2),FPSTR(2))
     &  * REM(NPRIOR(2))
        PHZACC(2) = PHZACC(2) + PROG(2)
        IF (PHZACC(2) .GE. PHTHRS(2)) THEN
C-------------------------------------------------------------------------------
C       V1 occurs on day DAS
C-------------------------------------------------------------------------------
        NVEG1 = DAS
        NVALPH(3) = NVEG1
        STGDOY(2) = YRDOY
        REM(3) = (PHZACC(2) - PHTHRS(2)) / (PROG(2) + 0.00001)
        ENDIF
      ENDIF

C-------------------------------------------------------------------------------
C     Check for end of juvenile phase
C-------------------------------------------------------------------------------
      IF ((DAS .GE. NVALPH(NPRIOR(3))) .AND. (JPEND .GE. NVALP0)) THEN
        PROG(3) = FT(3) * FUDAY(3) * MIN(FSW(3),FNSTR(3),FPSTR(3))
     &  * REM(NPRIOR(3))
        PHZACC(3) = PHZACC(3) + PROG(3)
        IF(PHZACC(3) .GE. PHTHRS(3)) THEN
C-------------------------------------------------------------------------------
C       End of juvenile phase occurs on day DAS
C-------------------------------------------------------------------------------
        JPEND = DAS
        NVALPH(4) = JPEND
        STGDOY(3) = YRDOY
        REM(4) = (PHZACC(3) - PHTHRS(3))/(PROG(3) + 0.00001)
        ENDIF
      ENDIF
C-------------------------------------------------------------------------------
C     Check for floral induction, end of induction phase
C-------------------------------------------------------------------------------
      IF ((DAS .GE. NVALPH(NPRIOR(4))) .AND. (NR0 .GE. NVALP0)) THEN
        PROG(4) = FT(4) * FUDAY(4) * MIN(FSW(4),FNSTR(4),FPSTR(4))
     &  *REM(NPRIOR(4))
        PHZACC(4) = PHZACC(4) + PROG(4)
        IF(PHZACC(4) .GE. PHTHRS(4)) THEN
C-------------------------------------------------------------------------------
C       Floral induction occurs on day DAS, end of phase 4
C-------------------------------------------------------------------------------
        NR0 = DAS
        NVALPH(5) = NR0
        RSTAGE    = 0
        STGDOY(4) = YRDOY
        REM(5) = (PHZACC(4) - PHTHRS(4))/(PROG(4) + 0.00001)
        ENDIF
      ENDIF

C-------------------------------------------------------------------------------
C     Check for first flower, stage 6, end of phase 5
C-------------------------------------------------------------------------------
      IF ((DAS .GE. NVALPH(NPRIOR(5))) .AND. (NR1 .GE. NVALP0)) THEN
        PROG(5) = FT(5) * FUDAY(5) * MIN(FSW(5),FNSTR(5),FPSTR(5))
     &  * REM(NPRIOR(5))
        PHZACC(5) = PHZACC(5) + PROG(5)
        IF(PHZACC(5) .GE. PHTHRS(5)) THEN
C-------------------------------------------------------------------------------
C       First flower occurs on day DAS
C-------------------------------------------------------------------------------
        NR1 = DAS
        STGDOY(5) = YRDOY
        NVALPH(6) = NR1
        YRNR1     = YRDOY
        RSTAGE    = 1
        REM(6) = (PHZACC(5) - PHTHRS(5))/(PROG(5) + 0.00001)
        ENDIF
      ENDIF
C-------------------------------------------------------------------------------
C     Check for beginning ovule (peg), stage 7, end of phase 6
C-------------------------------------------------------------------------------
      IF ((DAS .GE. NVALPH(NPRIOR(6))) .AND. (NR2 .GE. NVALP0)) THEN
        PROG(6) = FT(6) * FUDAY(6) * MIN(FSW(6),FNSTR(6),FPSTR(6))
     &  * REM(NPRIOR(6))
        PHZACC(6) = PHZACC(6) + PROG(6)
        IF(PHZACC(6) .GE. PHTHRS(6)) THEN
C-------------------------------------------------------------------------------
C       First peg occurs on day DAS
C-------------------------------------------------------------------------------
        NR2 = DAS
        STGDOY(6) = YRDOY
        NVALPH(7) = NR2
        YRNR2     = YRDOY
        RSTAGE    = 2
C-------------------------------------------------------------------------------
C       Account for the part of today that contributes to the next phase(s)
C-------------------------------------------------------------------------------
        REM(7) = (PHZACC(6) - PHTHRS(6))/(PROG(6) + 0.00001)
        ENDIF
      ENDIF

C-------------------------------------------------------------------------------
C     Check for stage beginning shell, stage 8, end of phase 7
C-------------------------------------------------------------------------------
      IF ((DAS .GE. NVALPH(NPRIOR(7))) .AND. (NR3 .GE. NVALP0)) THEN
        PROG(7) = FT(7) * FUDAY(7) * MIN(FSW(7),FNSTR(7),FPSTR(7))
     &  * REM(NPRIOR(7))
        PHZACC(7) = PHZACC(7) + PROG(7)
        IF(PHZACC(7) .GE. PHTHRS(7)) THEN
C-------------------------------------------------------------------------------
C       Stage R3 occurs on day DAS
C-------------------------------------------------------------------------------
        NR3 = DAS
        STGDOY(7) = YRDOY
        IF (STGDOY(7) .EQ. STGDOY(6)) STGDOY(7) = 999999
        NVALPH(8) = NR3
        YRNR3     = YRDOY
        RSTAGE    = 3
C-------------------------------------------------------------------------------
C       Account for the part of today that contributes to the next phase(s)
C-------------------------------------------------------------------------------
        REM(8) = (PHZACC(7) - PHTHRS(7))/(PROG(7) + 0.00001)
        ENDIF
      ENDIF

C-------------------------------------------------------------------------------
C     Check for stage beginning seed (R5), stage 9, end of phase 8
C-------------------------------------------------------------------------------
      IF ((DAS .GE. NVALPH(NPRIOR(8))) .AND. (NR5 .GE. NVALP0)) THEN
        PROG(8) = FT(8) * FUDAY(8) * MIN(FSW(8),FNSTR(8),FPSTR(8))
     &  * REM(NPRIOR(8))
        PHZACC(8) = PHZACC(8) + PROG(8)
        IF(PHZACC(8) .GE. PHTHRS(8)) THEN
C-------------------------------------------------------------------------------
C       Stage R5 occurs on day DAS
C-------------------------------------------------------------------------------
        NR5 = DAS
        STGDOY(8) = YRDOY
        NVALPH(9) = NR5
        YRNR5     = YRDOY
        RSTAGE    = 5
C-------------------------------------------------------------------------------
C       Account for the part of today that contributes to the next phase(s)
C-------------------------------------------------------------------------------
        REM(9) = (PHZACC(8) - PHTHRS(8))/(PROG(8) + 0.00001)
        ENDIF
      ENDIF

C-------------------------------------------------------------------------------
C     Check for stage NDSET, stage 10, end of phase 9
C-------------------------------------------------------------------------------
      IF ((DAS .GE. NVALPH(NPRIOR(9))) .AND. (NDSET .GE. NVALP0)) THEN
        PROG(9) = FT(9) * FUDAY(9) * MAX(FSW(9),FNSTR(9),FPSTR(9))
     &  * REM(NPRIOR(9))
        PHZACC(9) = PHZACC(9) + PROG(9)
        IF(PHZACC(9) .GE. PHTHRS(9)) THEN
C-------------------------------------------------------------------------------
C       Stage NDSET occurs on day DAS
C-------------------------------------------------------------------------------
        NDSET = DAS
        STGDOY(9) = YRDOY
        NVALPH(10) = NDSET
C-------------------------------------------------------------------------------
C       Account for the part of today that contributes to the next phase(s)
C-------------------------------------------------------------------------------
        REM(10) = (PHZACC(9) - PHTHRS(9))/(PROG(9) + 0.00001)
        ENDIF
      ENDIF

C-------------------------------------------------------------------------------
C     Check for stage NR7, stage 11, end of phase 10
C-------------------------------------------------------------------------------
      IF ((DAS .GE. NVALPH(NPRIOR(10))) .AND. (NR7 .GE. NVALP0)) THEN
        PROG(10) = FT(10) * FUDAY(10)*MAX(FSW(10),FNSTR(10),FPSTR(10))
     &  * REM(NPRIOR(10))
        PHZACC(10) = PHZACC(10) + PROG(10)
        IF(PHZACC(10) .GE. PHTHRS(10)) THEN
C-------------------------------------------------------------------------------
C       Stage NR7, physiological maturity, occurs on day DAS
C-------------------------------------------------------------------------------
        NR7 = DAS
        STGDOY(10) = YRDOY
        NVALPH(11) = NR7
        YRNR7      = YRDOY
        RSTAGE    = 7
C-------------------------------------------------------------------------------
C       Account for the part of today that contributes to the next phase(s)
C-------------------------------------------------------------------------------
        REM(11) = (PHZACC(10) - PHTHRS(10))/(PROG(10) + 0.00001)
        ENDIF
      ENDIF

C-------------------------------------------------------------------------------
C     Check for stage NR8, stage 12, end of phase 11
C-------------------------------------------------------------------------------
      IF ((DAS .GE. NVALPH(NPRIOR(11))) .AND. (MDATE .LE. YRSIM)) THEN
        PROG(11) = FT(11) * FUDAY(11)*MIN(FSW(11),FNSTR(11),FPSTR(11))
     &  * REM(NPRIOR(11))
        PHZACC(11) = PHZACC(11) + PROG(11)
        IF(PHZACC(11) .GE. PHTHRS(11)) THEN
C-------------------------------------------------------------------------------
C       Stage NR8, harvest maturity, occurs on day DAS
C-------------------------------------------------------------------------------
!          NR8 = DAS
        STGDOY(11) = YRDOY
!          NVALPH(12) = NR8
        NVALPH(12) = DAS
        MDATE      = YRDOY
        CropStatus = 1
        RSTAGE     = 8
C-------------------------------------------------------------------------------
C       Account for the part of today that contributes to the next phase(s)
C-------------------------------------------------------------------------------
        REM(12) = (PHZACC(11) - PHTHRS(11))/(PROG(11) + 0.00001)
        ENDIF
      ENDIF

C-------------------------------------------------------------------------------
C     Check for stage NDVST, end of V-stage addition, stage 13, end of phase 12
C-------------------------------------------------------------------------------
      IF ((DAS .GE. NVALPH(NPRIOR(12))) .AND. (NDVST .GE. NVALP0)) THEN
        PROG(12) = FT(12) * FUDAY(12)*MIN(FSW(12),FNSTR(12),FPSTR(12))
     &  * REM(NPRIOR(12))
        PHZACC(12) = PHZACC(12) + PROG(12)
        IF(PHZACC(12) .GE. PHTHRS(12)) THEN
C-------------------------------------------------------------------------------
C       Stage NDVST, end of V-stage addition, occurs on day DAS
C-------------------------------------------------------------------------------
        NDVST = DAS
        STGDOY(12) = YRDOY
        NVALPH(13) = NDVST
C-------------------------------------------------------------------------------
C       Account for the part of today that contributes to the next phase(s)
C-------------------------------------------------------------------------------
        REM(13) = (PHZACC(12) - PHTHRS(12))/(PROG(12) + 0.00001)
        ENDIF
      ENDIF

C-------------------------------------------------------------------------------
C     Check for stage NDLEAF, end of leaf growth, stage 14, end of phase 13
C-------------------------------------------------------------------------------
      IF ((DAS .GE. NVALPH(NPRIOR(13))) .AND. (NDLEAF .GE. NVALP0)) THEN
        PROG(13) = FT(13) * FUDAY(13)*MIN(FSW(13),FNSTR(13),FPSTR(13))
     &  * REM(NPRIOR(13))
        PHZACC(13) = PHZACC(13) + PROG(13)
        IF(PHZACC(13) .GE. PHTHRS(13)) THEN
C-------------------------------------------------------------------------------
C       Stage NDLEAF, end of leaf growth, occurs on day DAS
C-------------------------------------------------------------------------------
        NDLEAF = DAS
        STGDOY(13) = YRDOY
        NVALPH(14) = NDLEAF
C-------------------------------------------------------------------------------
C       Account for the part of today that contributes to the next phase(s)
C-------------------------------------------------------------------------------
        REM(14) = (PHZACC(13) - PHTHRS(13))/(PROG(13) + 0.00001)
        ENDIF
      ENDIF

!************************************************************************
!************************************************************************
!     End of DYNAMIC IF construct
!************************************************************************
      END IF
!************************************************************************
      RETURN
      END   !SUBROUTINE RSTAGES

!------------------------------------------------------------------------
!     RSTAGES Variables:
!------------------------------------------------------------------------
! DAS       Days after start of simulation (days)
! DYNAMIC   Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!             INTEGR, OUTPUT, or SEASEND 
! FNSTR(I)  Nitrogen stress function (0 to 1) for phase I 
! FPSTR(I)  Phosphorus stress function (0 to 1) for phase I 
! FSW(I)    Water stress function (0.0 to 1.0) for phase I 
! FT(I)     Temperature function (0-1) for phase I 
! FUDAY(I)  Effect of daylength on development progress (0-1) for phase I 
! ISIMI     Start of simulation code:     E = On reported emergence day, I 
!             = When initial conditions measured, P = On reported planting 
!             date, S = On specified date 
! JPEND     Day when juvenile phase ends and plants first become sensitive 
!             to photoperiod (days)
! NDLEAF    Day when leaf expansion ceased (days)
! NDSET     Normal time by which a pod load (full number) should be 
!             achieved with no water stress (days)
! NDVST     Day on which last main stem node formed (days)
! NPRIOR(I) The phase of growth at which phase I accumulator can start 
! NR0       Day when floral induction occurs (days)
! NR1       Day when 50% of plants have at least one flower (days)
! NR2       Day when 50% of plants have one peg (peanuts only) (days)
! NR3       Day when 50% of plants have at least one pod (days)
! NR5       Day when 50% of plants have pods with beginning seeds (days)
! NR7       Day when 50% of plants first have yellowing or maturing pods
!             (days)
! NVALP0    Set to 100,000 in PHENOLOG, used for comparison of times of 
!             plant stages  (days)
! NVALPH(I) Day when stage (I) occurred. (days)
! NVEG0     Day of emergence (days)
! NVEG1     1st day with 50% of plants w/ completely unrolled leaf at 
!             unifoliate node (days)
! PHTEM     Threshold time for emergence (thermal days)
! PHTHRS(I) Threshold time that must accumulate in phase I for the next 
!             stage to occur  (thermal or photothermal days)
! PHZACC(I) Cumulative. time of progression from the start of phase I
!             (thermal or photothermal days)
! PLME      Planting method; T = transplant, S = seed, P = pre-germinated 
!             seed, N = nursery 
! PROG (I)  Thermal or photo-thermal time that occurs in a real day for 
!             Phase I (Thermal or photothermal days)
! REM (I)   Remainder of thermal or photo-thermal time after a threshold is 
!             reached on a day (to be used to start progression into the 
!             next phase) (thermal or photothermal days / day)
! RSTAGE    Number of RSTAGES which have occurred. 
! SDEPTH    Planting depth (cm)
! STGDOY(I) Day when stage I occurred (YYDDD)
! TIMDIF    Integer function which calculates the number of days between 
!             two Julian dates (da)
! YRDOY     Current day of simulation (YYDDD)
! YREMRG    Day of emergence (YYDDD)
! YRNR1     Day when 50% of plants have at least one flower (YYDDD)
! YRNR2     Day when 50% of plants have one peg (peanuts only) (YYDDD)
! YRNR3     Day when 50% of plants have at least one beginning pod (YYDDD)
! YRNR5     Day when 50% of plants have pods with beginning seeds (YYDDD)
! YRNR7     Day when 50% of plants first have yellowing or maturing pods
!             (YYDDD)
! MDATE     Date of harvest maturity (YYDDD)
! YRPLT     Planting date (YYDDD)
! YRSIM     Start of simulation date (YYDDD)
!-----------------------------------------------------------------------
!     End Subroutine RSTAGES
!-----------------------------------------------------------------------
