C=======================================================================
C  SEEDDM, Subroutine
C  Calculates pest damage in seed and shells.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/01/1990 WDB Written
C  02/23/1998 CHP Modified for PEST Module
C  01/12/1999 GH  Incorporated into CROPGRO
C  06/06/2001 CHP Correction for 0 divide
C-----------------------------------------------------------------------
C  Called by: PEST
C  Calls:     None
C=======================================================================
      SUBROUTINE SEEDDM(
     &    DAS, LAGSD, LNGPEG, NR2, PHTIM, PHTHRS8,        !Input
     &    PSDDL, PSDDM, PSDDS, PSHDL, PSHDM, PSHDS,       !Input
     &    NSDDL, NSDDM, NSDDS, NSHDL, NSHDM, NSHDS,       !Input/Output
     &    SDNO, SHELN, SWIDOT,                            !Input/Output
     &    WSDDL, WSDDM, WSDDS,                            !Input/Output
     &    WSHDL, WSHDM, WSHDS, WTSD, WTSHE, WSHIDT,       !Input/Output
     &    CSDM, CSDN, CSHM, CSHN, SDDES, SDIDOT,          !Output
     &    SHIDOT, TSDNOL, TSDNOM, TSDNOS, TSDWTL,         !Output
     &    TSDWTM, TSDWTS, TSHNOL, TSHNOM, TSHNOS,         !Output
     &    TSHWTL, TSHWTM, TSHWTS,                         !Output
     &    DYNAMIC, WSDD, PSDD,SDWT)                       !Control

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
!     NCOHORTS defined in ModuleDefs.for

      IMPLICIT NONE
      SAVE

      INTEGER NR2, NPP, DAS, DYNAMIC

      REAL PHTIM(NCOHORTS)
      REAL PAGE,DEST,WPS
      REAL WTSD(NCOHORTS) ,WTSHE(NCOHORTS)
      REAL SDNO(NCOHORTS), SHELN(NCOHORTS)
      REAL SWIDOT, WSHIDT, SDIDOT, SHIDOT, LAGSD, LNGPEG
C     Seed Variables
      REAL NSDDS, NSDDL, NSDDM, PSDDS, PSDDL, PSDDM, WSDDS, WSDDL, WSDDM
      REAL TSDNO, TSDWT, CSDM, CSDN
      REAL TSDNOS, TSDNOL, TSDNOM, TSDWTS, TSDWTL, TSDWTM
      REAL SDDES(NCOHORTS), SDWDES(NCOHORTS), SDNDES(NCOHORTS)
      REAL WSDD,PSDD,SDWT
C     Shell Variables
      REAL NSHDS,NSHDL,NSHDM,PSHDS,PSHDL,PSHDM,WSHDS,WSHDL,WSHDM
      REAL TSHNO, TSHWT, CSHM, CSHN, PHTHRS8
      REAL TSHNOS,TSHNOL,TSHNOM,TSHWTS,TSHWTL,TSHWTM
      REAL SHWDES(NCOHORTS), SHNDES(NCOHORTS)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      SDWDES = 0.0
      SDNDES = 0.0
      SHWDES = 0.0
      SHNDES = 0.0
      SDDES  = 0.0

      SDIDOT = 0.0
      SHIDOT = 0.0
      SWIDOT = 0.0
      WSHIDT = 0.0

      TSDWTS = 0.0
      TSDNOS = 0.0
      TSHWTS = 0.0
      TSHNOS = 0.0

      TSDWTL = 0.0
      TSDNOL = 0.0
      TSHWTL = 0.0
      TSHNOL = 0.0

      TSDWTM = 0.0
      TSDNOM = 0.0
      TSHWTM = 0.0
      TSHNOM = 0.0

      CSDM = 0.
      CSDN = 0.
      CSHM = 0.
      CSHN = 0.

      TSDNO = 0.
      TSDWT = 0.
      TSHNO = 0.
      TSHWT = 0.

!***********************************************************************
!***********************************************************************
!     Daily rate calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      SDIDOT = 0.0
      SHIDOT = 0.0
      SWIDOT = 0.0
      WSHIDT = 0.0

      SDWDES = 0.0    !
      SDNDES = 0.0    !Arrays
      SHWDES = 0.0    !
      SHNDES = 0.0    !

      TSDWTS = 0.0
      TSDNOS = 0.0
      TSHWTS = 0.0
      TSHNOS = 0.0

      TSDWTL = 0.0
      TSDNOL = 0.0
      TSHWTL = 0.0
      TSHNOL = 0.0

      TSDWTM = 0.0
      TSDNOM = 0.0
      TSHWTM = 0.0
      TSHNOM = 0.0

!-----------------------------------------------------------------------
!     Compute total seed and shell mass and number for small, large and
!         mature pod classes.
!-----------------------------------------------------------------------
      DO NPP = 1, DAS-NR2-1
        PAGE = PHTIM(DAS-NR2) - PHTIM(NPP)
!-----------------------------------------------------------------------
C     Count small seed cohorts less than LAGSD (R3) old
C-----------------------------------------------------------------------
        IF (WTSD(NPP) .LT. 0.001 .AND. SDNO(NPP) .GT. 0.0) THEN
          TSDWTS = TSDWTS + WTSD(NPP)
          TSDNOS = TSDNOS + SDNO(NPP)
          TSHWTS = TSHWTS + WTSHE(NPP)
          TSHNOS = TSHNOS + SHELN(NPP)
        ENDIF
C-----------------------------------------------------------------------
C     Count large seed cohorts between small and full pod (R6)
C-----------------------------------------------------------------------
        IF (WTSD(NPP) .GT. 0.0 .AND. PAGE .GE. LAGSD .AND.
     &        PAGE .LT. PHTHRS8) THEN
          TSDWTL = TSDWTL + WTSD(NPP)
          TSDNOL = TSDNOL + SDNO(NPP)
          TSHWTL = TSHWTL + WTSHE(NPP)
          TSHNOL = TSHNOL + SHELN(NPP)
        ENDIF
C-----------------------------------------------------------------------
C    Count mature cohorts that are older than full pod (R6)
C-----------------------------------------------------------------------
        IF (PAGE .GT. PHTHRS8) THEN
          TSDWTM = TSDWTM + WTSD(NPP)
          TSDNOM = TSDNOM + SDNO(NPP)
          TSHWTM = TSHWTM + WTSHE(NPP)
          TSHNOM = TSHNOM + SHELN(NPP)
        ENDIF
      ENDDO

      TSDNOS = MAX(0.,TSDNOS)
      TSDWTS = MAX(0.,TSDWTS)
      TSHNOS = MAX(0.,TSHNOS)
      TSHWTS = MAX(0.,TSHWTS)

      TSDNOL = MAX(0.,TSDNOL)
      TSDWTL = MAX(0.,TSDWTL)
      TSHNOL = MAX(0.,TSHNOL)
      TSHWTL = MAX(0.,TSHWTL)

      TSDNOM = MAX(0.,TSDNOM)
      TSDWTM = MAX(0.,TSDWTM)
      TSHNOM = MAX(0.,TSHNOM)
      TSHWTM = MAX(0.,TSHWTM)
!-----------------------------------------------------------------------
C      Compute total seed and shell number and mass in the damaged
C      size class (These values not used - chp)
C---------------------------------------------------------------------
      TSDNO = TSDNOS + TSDNOL + TSDNOM
      TSDWT = TSDWTS + TSDWTL + TSDWTM
      TSHNO = TSHNOS + TSHNOL + TSHNOM
      TSHWT = TSHWTS + TSHWTL + TSHWTM

!-----------------------------------------------------------------------
!     Loop through cohorts and apply damage
!-----------------------------------------------------------------------
      DO NPP=1,DAS-NR2-1
        PAGE = PHTIM(DAS-NR2) - PHTIM(NPP)

C-----------------------------------------------------------------------
C    Apply damage as percent of seed number or seed mass
C-----------------------------------------------------------------------
        IF (PSDDS .GT. 0.0 .AND. WTSD(NPP) .LT. 0.0001 .AND.
     &          SDNO(NPP) .GT. 0.0) THEN
          SDWDES(NPP) = SDWDES(NPP) + PSDDS * WTSD(NPP) / 100.0
          SDNDES(NPP) = SDNDES(NPP) + PSDDS * SDNO(NPP) / 100.0
          IF (PSHDS .LT. 0.001 .AND. NSHDS .LT. 0.001) THEN
            SDDES(NPP) = SDDES(NPP) + PSDDS * SDNO(NPP) / 100.0
          ENDIF
        ENDIF

        IF (PSDDL .GT. 0.0 .AND. PAGE .GE. LAGSD .AND.
     &          PAGE .LT. PHTHRS8) THEN
          SDWDES(NPP) = SDWDES(NPP) + PSDDL * WTSD(NPP) / 100.0
          SDNDES(NPP) = SDNDES(NPP) + PSDDL * SDNO(NPP) / 100.0
          IF (PSHDL .LT. 0.001 .AND. NSHDL .LT. 0.001) THEN
            SDDES(NPP) = SDDES(NPP) + PSDDL * SDNO(NPP)/100.0
          ENDIF
        ENDIF

        IF (PSDDM .GT. 0.0 .AND. PAGE .GT. PHTHRS8) THEN
          SDWDES(NPP) = SDWDES(NPP) + PSDDM * WTSD(NPP) / 100.0
          SDNDES(NPP) = SDNDES(NPP) + PSDDM * SDNO(NPP) / 100.0
          IF (PSHDM .LT. 0.001 .AND. NSHDM .LT. 0.001) THEN
            SDDES(NPP) = SDDES(NPP)+ PSDDM * SDNO(NPP)/100.0
          ENDIF
        ENDIF

C-----------------------------------------------------------------------
C     Apply damage as number of seed/m2
C-----------------------------------------------------------------------
        IF (NSDDS .GT. 0.0 .AND. WTSD(NPP) .LT. 0.0001 .AND.
     &      SDNO(NPP) .GT. 0.0 .AND. TSDNOS .GT. 0.0) THEN
          NSDDS = MIN(NSDDS,TSDNOS)
          SDWDES(NPP) = SDWDES(NPP) + WTSD(NPP) * NSDDS/TSDNOS
          SDNDES(NPP) = SDNDES(NPP) + SDNO(NPP) * NSDDS/TSDNOS
          IF (PSHDS .LT. 0.001 .AND. NSHDS .LT. 0.001) THEN
            SDDES(NPP) = SDDES(NPP) +  NSDDS * SDNO(NPP)/TSDNOS
          ENDIF
        ENDIF

        IF (NSDDL .GT. 0.0 .AND. PAGE .GE. LAGSD .AND.
     &       PAGE .LT. PHTHRS8 .AND. TSDNOL .GT. 0.0) THEN
          NSDDL = MIN(NSDDL,TSDNOL)
          SDWDES(NPP) = SDWDES(NPP) + WTSD(NPP) * NSDDL/TSDNOL
          SDNDES(NPP) = SDNDES(NPP) + SDNO(NPP) * NSDDL/TSDNOL
          IF (PSHDL .LT. 0.001 .AND. NSHDM .LT. 0.001) THEN
            SDDES(NPP) = SDDES(NPP) +  NSDDL * SDNO(NPP)/TSDNOL
          ENDIF
        ENDIF

        IF (NSDDM .GT. 0.0 .AND. PAGE .GT. PHTHRS8 .AND.
     &          TSDNOM .GT. 0.0) THEN
          NSDDM = MIN(NSDDM,TSDNOM)
          SDWDES(NPP) = SDWDES(NPP) + WTSD(NPP) * NSDDM/TSDNOM
          SDNDES(NPP) = SDNDES(NPP) + SDNO(NPP) * NSDDM/TSDNOM
          IF (PSHDM .LT. 0.001 .AND. NSHDM .LT. 0.001) THEN
            SDDES(NPP) = SDDES(NPP) + NSDDM * SDNO(NPP)/TSDNOM
          ENDIF
        ENDIF

C-----------------------------------------------------------------------
C     Apply damage as mass of seed
C-----------------------------------------------------------------------
        IF (WTSD(NPP) .LE. 0.001) GOTO 1000
        WPS = SDNO(NPP)/WTSD(NPP)

        IF (WSDDS .GT. 0.0 .AND. PAGE .LT. LAGSD .AND.
     &      PAGE .GT. LNGPEG .AND. TSDNOS .GT. 0.0) THEN
          WSDDS = MIN(WSDDS,TSDWTS)
          SDWDES(NPP) = SDWDES(NPP) + WSDDS*SDNO(NPP)/TSDNOS
          SDNDES(NPP) = SDNDES(NPP) + WSDDS*SDNO(NPP)*WPS/TSDNOS
          IF (PSHDS .GT. 0.0 .AND. NSHDS .GT. 0.0 .AND.
     &            TSDWTS .GT. 0.0001) THEN
            SDDES(NPP) = SDDES(NPP) + WPS*WSDDS*SDNO(NPP)/TSDNOS
          ENDIF
        ENDIF

        IF (WSDDL .GT. 0.0 .AND. PAGE .GE. LAGSD .AND.
     &      PAGE .LT. PHTHRS8 .AND. TSDNOL .GT. 0.0) THEN
          WSDDL = MIN(WSDDL,TSDWTL)
          SDWDES(NPP) = SDWDES(NPP) + WSDDL*SDNO(NPP)/TSDNOL
          SDNDES(NPP) = SDNDES(NPP) + WSDDL*SDNO(NPP)*WPS/TSDNOL
          IF (PSHDL .GT. 0.0 .AND. NSHDL .GT. 0.0) THEN
            SDDES(NPP) = SDDES(NPP) + WPS*WSDDL*SDNO(NPP)/TSDNOL
          ENDIF
        ENDIF

        IF (WSDDM .GT. 0.0 .AND. PAGE .GT. PHTHRS8 .AND.
     &      TSDNOM .GT. 0.0) THEN
          WSDDM = MIN(WSDDM,TSDWTM)
          SDWDES(NPP) = SDWDES(NPP) + WSDDM*SDNO(NPP)/TSDNOM
          SDNDES(NPP) = SDNDES(NPP) + WSDDM*SDNO(NPP)*WPS/TSDNOM
          IF (PSHDM .GT. 0.0 .AND. NSHDM .GT. 0.0) THEN
            SDDES(NPP) = SDDES(NPP) + WPS*WSDDM*SDNO(NPP)/TSDNOM
          ENDIF
        ENDIF
 1000   CONTINUE

C-----------------------------------------------------------------------
C        Apply damage as percent shell mass
C-----------------------------------------------------------------------
        IF (PSHDS .GT. 0.0 .AND. PAGE .GT. LNGPEG .AND.
     &      PAGE .LT. LAGSD) THEN
          SHWDES(NPP) = SHWDES(NPP) + PSHDS * WTSHE(NPP) / 100.0
          SHNDES(NPP) = SHNDES(NPP) + PSHDS * SHELN(NPP) / 100.0
        ENDIF

        IF(PSHDL .GT. 0.0 .AND. PAGE .GE. LAGSD .AND.
     &      PAGE .LT. PHTHRS8) THEN
          SHWDES(NPP) = SHWDES(NPP) + PSHDL * WTSHE(NPP) / 100.0
          SHNDES(NPP) = SHNDES(NPP) + PSHDL * SHELN(NPP) / 100.0
        ENDIF

        IF(PSHDM .GT. 0.0 .AND. PAGE .GE. PHTHRS8) THEN
          SHWDES(NPP) = SHWDES(NPP) + PSHDM * WTSHE(NPP) / 100.0
          SHNDES(NPP) = SHNDES(NPP) + PSHDM * SHELN(NPP) / 100.0
        ENDIF

C-----------------------------------------------------------------------
C     Apply damage as number of shells /m2
C-----------------------------------------------------------------------
        IF (NSHDS .GT. 0.0 .AND. PAGE .GT. LNGPEG .AND.
     &      PAGE .LT. LAGSD .AND. TSHNOS .GT. 0.0) THEN
          NSHDS = MIN(NSHDS,TSHNOS)
          SHWDES(NPP) = SHWDES(NPP) + WTSHE(NPP)*NSHDS/TSHNOS
          SHNDES(NPP) = SHNDES(NPP) + SHELN(NPP)*NSHDS/TSHNOS
        ENDIF

        IF (NSHDL .GT. 0.0 .AND. PAGE .GT. LAGSD .AND.
     &      PAGE .LT. PHTHRS8 .AND. TSHNOL .GT. 0) THEN
          NSHDL = MIN(NSHDL,TSHNOL)
          SHWDES(NPP) = SHWDES(NPP) + WTSHE(NPP)*NSHDL/TSHNOL
          SHNDES(NPP) = SHNDES(NPP) + SHELN(NPP)*NSHDL/TSHNOL
        ENDIF

        IF (NSHDM .GT. 0.0 .AND. PAGE .GT. PHTHRS8 .AND.
     &      TSHNOM .GT. 0.0) THEN
          NSHDM = MIN(NSHDM,TSHNOM)
          SHWDES(NPP) = SHWDES(NPP) + WTSHE(NPP)*NSHDM/TSHNOM
          SHNDES(NPP) = SHNDES(NPP) + SHELN(NPP)*NSHDM/TSHNOM
        ENDIF

C-----------------------------------------------------------------------
C     Apply damage as weight of shell /m2
C-----------------------------------------------------------------------
        IF(WSHDS .GT. 0.0 .AND. PAGE .GT. LNGPEG .AND.
     &      PAGE .LT. LAGSD .AND. TSHNOS .GT. 0.0) THEN
          WSHDS = MIN(WSHDS,TSHWTS)
          SHWDES(NPP) = SHWDES(NPP) + WSHDS*WTSHE(NPP)/TSHWTS
          DEST = WSHDS * WTSHE(NPP)/TSHWTS
          SHNDES(NPP) = SHNDES(NPP) + DEST*SHELN(NPP)/WTSHE(NPP)
        ENDIF

        IF (WSHDL .GT. 0.0 .AND. PAGE .GT. LAGSD .AND.
     &      PAGE .LT. PHTHRS8 .AND. TSHNOL .GT. 0.0) THEN
          WSHDL = MIN(WSHDL,TSHWTL)
          SHWDES(NPP) = SHWDES(NPP) + WSHDL*WTSHE(NPP)/TSHWTL
          DEST = WSHDL * WTSHE(NPP)/TSHWTL
          SHNDES(NPP) = SHNDES(NPP) + DEST*SHELN(NPP)/WTSHE(NPP)
        ENDIF

        IF (WSHDM .GT. 0.0 .AND. PAGE .GT. PHTHRS8 .AND.
     &      TSHNOM .GT. 0.0) THEN
          WSHDM = MIN(WSHDM,TSHWTM)
          SHWDES(NPP) = SHWDES(NPP) + WSHDM*WTSHE(NPP)/TSHWTM
          DEST = WSHDM * WTSHE(NPP)/TSHWTM
          SHNDES(NPP) = SHNDES(NPP) + DEST*SHELN(NPP)/WTSHE(NPP)
        ENDIF

!       Check that mass and number destroyed are not greater than
!             actual mass and number.
        SDWDES(NPP) = MIN(SDWDES(NPP), WTSD(NPP))
        SDNDES(NPP) = MIN(SDNDES(NPP), SDNO(NPP))
        SHWDES(NPP) = MIN(SHWDES(NPP), WTSHE(NPP))
        SHNDES(NPP) = MIN(SHNDES(NPP), SHELN(NPP))

!       Update daily total mass and number of seed and shell destroyed.
        SWIDOT = SWIDOT + SDWDES(NPP)   
        SDIDOT = SDIDOT + SDNDES(NPP)   
        WSHIDT = WSHIDT + SHWDES(NPP)   
        SHIDOT = SHIDOT + SHNDES(NPP)    

      ENDDO       !End of cohort loop



      !----------------------------------------------------------------
      ! This section was added to compute seed damage in CERES MAIZE.
      ! The main difference is that CERES assumes all seeds are the
      ! same age, thus there are no seed chorts. 
      !
      ! Apply damage as absolute (g seed/m2/d)
      IF(WSDD.GT.0) THEN
        SWIDOT = SWIDOT + WSDD
      ENDIF

      ! Apply damage as percent (%/d)
      IF (PSDD .GT. 0) THEN
        SWIDOT = SWIDOT + SDWT*(PSDD/100)
      ENDIF


!***********************************************************************
!***********************************************************************
!     Daily integration
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
C-----------------------------------------------------------------------
      CSDM = CSDM + SWIDOT
      CSDN = CSDN + SDIDOT
      CSHM = CSHM + WSHIDT
      CSHN = CSHN + SHIDOT
      
      DO NPP = 1, DAS - NR2 - 1
        PAGE = PHTIM(DAS - NR2) - PHTIM(NPP)
        WTSD(NPP)  = MAX(0., WTSD(NPP)  - SDWDES(NPP))
        SDNO(NPP)  = MAX(0., SDNO(NPP)  - SDNDES(NPP))
        WTSHE(NPP) = MAX(0., WTSHE(NPP) - SHWDES(NPP))
        SHELN(NPP) = MAX(0., SHELN(NPP) - SHNDES(NPP))
      ENDDO

!***********************************************************************
      ENDIF
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      END   ! SUBROUTINE SEEDDM

!-----------------------------------------------------------------------
!     Variable Definitions
!-----------------------------------------------------------------------
! CSDM     Cumulative seed mass destroyed (g/m2)
! CSDN     Cumulative number of seeds destroyed (#/m2)
! CSHM     Cumulative shell mass destroyed (g/m2)
! CSHN     Cumulative number of shells destroyed (#/m2)
! DAS      Days after start of simulation (d)
! DEST     Percentage of shells destroyed 
! LAGSD    Time required between shell growth and seed growth, per cohort
!            (Photo-thermal days)
! LNGPEG   Time between start of peg (full flower) and shell formation (for 
!            peanuts only).  Defines slow growth period. (Photo-thermal days)
! NPP      Cohort number used as index in loops 
! NR2      Day when 50% of plants have one peg (peanuts only) (d)
! NSDDL    Daily number of large seeds damaged (#/m2/d)
! NSDDM    Daily number of mature seeds damaged (#/m2/d)
! NSDDS    Daily number of small seeds damaged (#/m2/d)
! NSHDL    Daily number of large shells damaged (#/m2/d)
! NSHDM    Daily number of mature shells damaged (#/m2/d)
! NSHDS    Daily number of small shells damaged (#/m2/d)
! PAGE     Photothermal age of each cohort  (Photo-thermal days)
! PHTHRS8  Threshold time that must accumulate in phase 8 for the next 
!            stage to occur.  Equivalent to PHTHRS(8) in Subroutine PHENOLOG.
!            (photothermal days)
! PHTIM    Cumulative photothermal time ages of seeds and shells 
! PSDDL    Percent large seed mass damage (%/d)
! PSDDM    Percent mature seed mass damage (%/d)
! PSDDS    Percent small seed mass damage (%/d)
! PSHDL    Percent large shell mass damage (%/d)
! PSHDM    Percent mature shell mass damage (%/d)
! PSHDS    Percent small shell mass damage (%/d)
! SDDES(J) Number of seeds destroyed today in cohort J when shells are 
!            not destroyed (#/m2/d)
! SDIDOT   Number of seeds destroyed on the current day (#/m2/d)
! SDNDES(J)Number of seeds destroyed in cohort J by pests or disease
!            (#/m2/day)
! SDNO(J)  Number of seeds for cohort J (#/m2)
! SDWT     Seed weight, g/m2
! SDWDES(J)Mass of seeds in cohort J destroyed today by pest or disease 
!            (g/m2/d)
! SHELN(J) Number of shells for cohort J (#/m2)
! SHIDOT   Number of shells destroyed on current day (#/m2/d)
! SWIDOT   Daily seed mass damage (g/m2/day)
! TSDNO    Total seed number in all cohorts (#/m2)
! TSDNOL   Total number of large seeds (#/m2)
! TSDNOM   Total number of mature seeds (#/m2)
! TSDNOS   Total number of small seeds (#/m2)
! TSDWT    Total seed mass in all cohorts (g/m2)
! TSDWTL   Seed mass for large seeds (g/m2)
! TSDWTM   Seed mass for mature seeds (g/m2)
! TSDWTS   Seed mass for small seeds (g/m2)
! TSHNO    Total shell number in all cohorts (#/m2)
! TSHNOL   Number shells with large seeds (#/m2)
! TSHNOM   Number shells with mature seeds (#/m2)
! TSHNOS   Number shells with small seeds (#/m2)
! TSHWT    Total shell mass in all cohorts (g/m2)
! TSHWTL   Shell mass with large seeds (g/m2)
! TSHWTM   Shell mass with mature seeds (g/m2)
! TSHWTS   Shell mass with small seeds (g/m2)
! WPS      Number seeds per gram (#/g)
! WSDDL    Daily mass of large seed damaged (g/m2/day)
! WSDDM    Daily mass of mature seed damaged (g/m2/day)
! WSDDS    Daily mass of small seed damaged (g/m2/day)
! WSHDL    Daily mass of large shell damaged (g/m2/day)
! WSHDM    Daily mass of mature shell damaged (g/m2/day)
! WSHDS    Daily mass of small shell damaged (g/m2/day)
! WSHIDT   Weight of shell tissue consumed by pests today (g[shell]/m2-d)
! WTSD(J)  Seed mass  for cohort J (g/m2)
! WTSHE(J) Shell mass  for cohort J (g/m2)
!-----------------------------------------------------------------------
!     End Subroutine SEEDDM
!-----------------------------------------------------------------------


