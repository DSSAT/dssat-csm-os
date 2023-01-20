C=======================================================================
C  FOR_VEGDM, Subroutine
C  Calculates the reduction in vegetative mass in
C       state and rate variables due to pest damage.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/01/1990 WDB Written
C  02/23/1998 CHP Modified for PEST Module
C  01/12/1999 GH  Incorporated into CROPGRO
C-----------------------------------------------------------------------
C  Called by: PEST
C  Calls:     None
C=======================================================================
      SUBROUTINE FOR_VEGDM(
     &    AREALF, CLW, CSW, PCLMT,                        !Input
     &    PCSTMD, PDLA, PLFAD, PLFMD, PSTMD,              !Input
     &    PVSTGD, SLA, SLDOT, SSDOT, STMWT,               !Input
     &    TDLA, VSTGD, WLFDOT, WSTMD, WTLF,               !Input
     &    TLFAD, TLFMD, VSTAGE, WLIDOT,                   !Input/Output
     &    CLAI, CLFM, CSTEM, DISLA, DISLAP,               !Output
     &    LAIDOT, WSIDOT,                                 !Output
     &    CSRW, PCSTRD, PSTRD, SSRDOT, STRWT,             !Input
     &    WSFDOT, WSRFDOT, WSTRD,                         !Input
     &    CSTRM, WSRIDOT,                                 !Output
     &    CSFRZ, CSRFRZ, DSTOR, SRDAM,                    !Output
     &    DYNAMIC)                                        !Control

!     2023-01-20 CHP Remove unused variables from argument list:
!     PCLMA, 


!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

C     Leaf Variables
      REAL TLFAD,PLFAD,TLFMD,PLFMD,PCLMT  !,PCLMA
      REAL CLSEN,CLAI,CLFM,LAIDOT
      REAL DISLA, DISLAP, AREALF
      REAL PDLA, TDLA
      REAL CLW, PWTLF, WLFDOT, CLFRZ, LAIDAM
C     Stem Variables
      REAL WSTMD,PSTMD,PCSTMD,CSTEM
      REAL DSTEM
      REAL CSW, SSDOT, SDAM
      REAL CSFRZ, WSFDOT
      REAL WTLF,STMWT,WLIDOT,WSIDOT,SLDOT,SLA
      REAL VSTAGE,PVSTGD,VSTGD
      
      REAL LDAM
      INTEGER DYNAMIC

      REAL CSRFRZ, CSRW, CSTRM, DSTOR, PCSTRD, PSTRD, SRDAM, 
     &    SSRDOT, STRWT, WSRFDOT, WSRIDOT, WSTRD

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CLSEN  = 0.0
      CLFRZ  = 0.0
      CLAI   = 0.0
      CLFM   = 0.0
      CSTEM  = 0.0
      CSTRM  = 0.0
      CSFRZ  = 0.0
      CSRFRZ = 0.0
!***********************************************************************
!***********************************************************************
!     Daily rate calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
C-----------------------------------------------------------------------
      WSIDOT = 0.0
      WLIDOT = 0.0
      LAIDOT = 0.0
      DISLA  = 0.0
      DISLAP = 0.0
      LDAM   = 0.0
      LAIDAM = 0.0
      SDAM   = 0.0
      WSRIDOT = 0.0
C-----------------------------------------------------------------------
C     Desired observed cumulative stem damage
C-----------------------------------------------------------------------
C     When stem damage is reported as percent reduction of total 
C     cumulative stem mass produced, use this section.  The total stem
!     mass produced is CSW.  Part of the observed damage comes from 
!     senescence and part from pests.  
C-----------------------------------------------------------------------
      IF (PCSTMD .GT. 0.0) THEN
C     Desired stem mass DSTEM after cumulative damage
        DSTEM = CSW * (1.0 - PCSTMD / 100.0)
        IF ((STMWT - SSDOT - WSFDOT) .GT. DSTEM) THEN
        SDAM = STMWT - SSDOT - WSFDOT - DSTEM
        ELSE
        SDAM = 0.0
        ENDIF
        WSIDOT = WSIDOT + SDAM
      ENDIF

C-----------------------------------------------------------------------
C     Percent daily stem damage
C-----------------------------------------------------------------------
      IF (PSTMD .GT. 0.0) THEN
        SDAM = PSTMD*STMWT/100.0
        WSIDOT = WSIDOT + SDAM
      ENDIF
C-----------------------------------------------------------------------
C     Absolute daily amount of stem mass damaged
C-----------------------------------------------------------------------
      IF(WSTMD .GT. 0.0) THEN
        SDAM = MIN(WSTMD, STMWT)
        WSIDOT = WSIDOT + SDAM
      ENDIF

      WSIDOT = MAX(0.,WSIDOT)
      WSIDOT = MIN(WSIDOT, STMWT)




C-----------------------------------------------------------------------
C     Desired observed cumulative storage organ damage
C-----------------------------------------------------------------------
C     When storage organ damage is reported as percent reduction of total 
C     cumulative storage organ mass produced, use this section.  The total
!     storage organ mass produced is CSRW.  Part of the observed damage 
!     comes from senesence and part from pests.  
C-----------------------------------------------------------------------

      IF (PCSTRD .GT. 0.0) THEN
C     Desired storage organ mass DSTOR after cumulative damage
        DSTOR = CSRW * (1.0 - PCSTRD / 100.0)
        IF ((STRWT - SSRDOT - WSRFDOT) .GT. DSTOR) THEN
        SRDAM = (STRWT - SSRDOT - WSRFDOT) - DSTOR
        ELSE
        SRDAM = 0.0
        ENDIF
        WSRIDOT = WSRIDOT + SRDAM
      ENDIF

C-----------------------------------------------------------------------
C     Percent daily storage organ damage
C-----------------------------------------------------------------------
      IF (PSTRD .GT. 0.0) THEN
        SRDAM = PSTRD*STRWT/100.0
        WSRIDOT = WSRIDOT + SRDAM
      ENDIF
C-----------------------------------------------------------------------
C     Absolute daily amount of storage organ mass damaged
C-----------------------------------------------------------------------
      IF(WSTRD .GT. 0.0) THEN
        SRDAM = MIN(WSTRD, STRWT)
        WSRIDOT = WSRIDOT + SRDAM
      ENDIF

      WSRIDOT = MAX(0.,WSRIDOT)
      WSRIDOT = MIN(WSRIDOT, STRWT)


C-----------------------------------------------------------------------
C     Absolute daily leaf damage
C-----------------------------------------------------------------------
      IF (TLFAD .GT. 0.0 .AND. SLA .GT. 0.0) THEN
        LAIDAM = MIN(TLFAD, WTLF * SLA / 10000.0)
        LDAM = LAIDAM * 10000.0 / SLA
        WLIDOT = WLIDOT + LDAM
        LAIDOT = LAIDOT + LAIDAM
      ENDIF

      IF (TLFMD .GT. 0.0) THEN
        LDAM = MIN(TLFMD,WTLF)
        LAIDAM = LDAM * SLA / 10000.
        WLIDOT = WLIDOT + LDAM
        LAIDOT = LAIDOT + LAIDAM
      ENDIF
C-----------------------------------------------------------------------
C     Percent daily leaf damage
C-----------------------------------------------------------------------
      IF (PLFAD .GT. 0.0) THEN
        LDAM = WTLF*(PLFAD/100.0)
        LAIDAM = LDAM * SLA / 10000.0
        WLIDOT = WLIDOT + LDAM
        LAIDOT= LAIDOT + LAIDAM
      ENDIF

      IF (PLFMD .GT. 0.0) THEN
        LDAM = WTLF*(PLFMD/100.0)
        LAIDAM = LDAM * SLA / 10000.0
        WLIDOT = WLIDOT + LDAM
        LAIDOT= LAIDOT + LAIDAM
      ENDIF

C-----------------------------------------------------------------------
C     Desired observed percent leaf damage
C-----------------------------------------------------------------------
C  When damage is reported as percent reduction of total cumulative
C  leaf mass produced, use this section.  The total leaf mass produced is
C  CLW.  Part of the observed damage comes from senesence and freezing 
C  and part comes from pests.  This section does not require you to 
C  distinguish whether damage came from senesence or pests.  PWTLF is
!  the potential available leaf mass after the observed leaf damage is
!  applied.  
!----------------------------------------------------------------
      IF (PCLMT .GE. 0.0001) THEN
        PWTLF = CLW * (1.0 - PCLMT / 100.0)
        IF ((WTLF - SLDOT - WLFDOT) .GT. PWTLF) THEN
        LDAM = (WTLF - SLDOT - WLFDOT) - PWTLF
        ELSE
        LDAM = 0.0
        ENDIF
!--------------------------------------------------------------
        IF (LDAM .GT. 0.0) THEN
        WLIDOT = WLIDOT + LDAM
        LAIDAM = LDAM * SLA / 10000.0
        LAIDOT= LAIDOT + LAIDAM
        ENDIF
      ENDIF

C-----------------------------------------------------------------------
C     Where is PCLMA?? chp 3/23/01
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C     Percent diseased leaf area; DISLA and TDLA are in units of cm2/m2
!     This code came from GROW subroutine -- chp
C-----------------------------------------------------------------------
      IF (PDLA .GT. 0.0) THEN
        DISLA = WTLF * SLA * PDLA/100.0
      ENDIF

      IF (TDLA .GT. 0.0) THEN
        DISLA = DISLA + TDLA
      ENDIF

      IF (AREALF .GT. 0.) THEN
        DISLAP = DISLA / AREALF * 100.0
      ELSE
        DISLAP = 0.0
      ENDIF

!***********************************************************************
!***********************************************************************
!     Daily integration
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
C-----------------------------------------------------------------------
C     Maintain cumulative values for senesenced and frozen leaf tissue.
C-----------------------------------------------------------------------
      IF (SLDOT  .GT. 0.0) CLSEN = CLSEN + SLDOT
      IF (WLFDOT .GT. 0.0) CLFRZ = CLFRZ + WLFDOT
      IF (WSFDOT .GT. 0.0) CSFRZ = CSFRZ + WSFDOT
      IF (WSRFDOT .GT. 0.0) CSRFRZ = CSRFRZ + WSRFDOT
C-----------------------------------------------------------------------
C     Maintain cumulative leaf, leaf area, and stem damage variables.
C-----------------------------------------------------------------------
      CLFM  = CLFM  + WLIDOT
      CLAI  = CLAI  + LAIDOT
      CSTEM = CSTEM + WSIDOT
      CSTRM  = CSTRM + WSRIDOT
C-----------------------------------------------------------------------
C     Percent V-stage damage
C-----------------------------------------------------------------------
      IF (PVSTGD .GT. 0.0)  THEN
        VSTAGE = VSTAGE * (1.0 - PVSTGD/100.)
      ENDIF

C-----------------------------------------------------------------------
C     Absolute daily amount of VSTAGE damaged
C-----------------------------------------------------------------------
      IF (VSTGD .GT. 0.0) THEN
        VSTAGE = VSTAGE - VSTGD
      ENDIF
      VSTAGE = MAX(0.,VSTAGE)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END  ! SUBROUTINE FOR_VEGDM
!-----------------------------------------------------------------------
!     Variable definitions
!-----------------------------------------------------------------------
! AREALF  Area of leaves (one side) per unit ground area
!           (cm2[leaf] / m2[ground])
! CLAI    Cumulative leaf area index destroyed (m2/m2)
! CLFM    Cumulative leaf mass destroyed  (g/m2)
! CLFRZ   Cumulative frozen leaf tissue (g[leaf]/m2)
! CLSEN   Cumulative leaf senescence (g/m2)
! CLW     Cumulative leaf growth (g[leaf]/m2)
! CSFRZ      Cumulative frozen stem tissue (g[stem]/m2)
! CSRFRZ      Cumulative frozen storage organ tissue (g[storage]/m2)
! CSRW      Cumulative storage organ growth (g[storage]/m2)
! CSTEM   Cumulative stem mass destroyed (g/m2)
! CSTRM      Cumulative storage organ mass destroyed (g/m2)
! CSW     Cumulative stem growth (g[stem]/m2)
! DISLA   Diseased leaf area (cm2[leaf]/m2[ground]/d)
! DISLAP  Percent diseased leaf area (%/d)
! DSTEM   Desired stem mass (g/m2/d)
! DSTOR   Desired storage organ mass (g/m2/d)
! DYNAMIC Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, INTEGR, 
!           OUTPUT, or SEASEND 
! LAIDAM  Change in leaf area index due to current pest
!           (m2[leaf]/m2[ground])
! LAIDOT  Daily change in leaf area index due to pest damage (m2/m2/d)
! LDAM    Daily leaf damage (g/m2/d)
! PCLMA   Percent observed leaf mass (WTLF) damage (%)
! PCLMT   Percent of total leaf mass (WTLF + senescence) destroyed (%)
! PCSTMD  Observed cumulative percentage stem mass damage (%)
! PCSTRD  Observed cumulative percentage storage organ mass damage (%)
! PDLA    Percent diseased leaf area (%)
! PLFAD   Daily percent leaf area damage (%/d)
! PLFMD   Percent leaf mass damage (%/d)
! PSTMD   Daily percent stem mass damage (%)
! PSTRD   Daily percent storage organ mass damage (%)
! PVSTGD  Percent V-stage damage (%)
! PWTLF   Potential available leaf mass after the observed leaf damage is 
!           applied (g[leaf]/m2)
! SDAM    Calculated stem damage (g/m2/d)
! SLA     Specific leaf area (cm2[leaf] / m2[ground])
! SLDOT   Defoliation due to daily leaf senescence (g/m2/day)
! SRDAM   Calculated storage organ damage (g/m2/d)
! SSDOT   Daily senescence of petioles (g / m2 / d)
! SSRDOT      Daily senescence of storage organ(g / m2 / d)
! STMWT   Dry mass of stem tissue, including C and N (g[stem] / m2[ground)
! STRWT   Dry mass of storage organ, including C and N (g[storage] / m2[ground)
! TDLA    Total diseased leaf area (cm2/m2)
! TLFAD   Total leaf area damage (cm2/cm2/d)
! TLFMD   Total leaf mass damage (g/m2/day)
! VSTAGE  Number of nodes on main stem of plant (nodes)
! VSTGD   Absolute daily V-stage damage (nodes/day)
! WLFDOT  Leaf weight losses due to freezing (g[leaf]/m2-d)
! WLIDOT  Daily pest or freeze damage to leaf mass (g/m2/day)
! WSFDOT        Stem weight losses due to freezing (g[stem]/m2-d)
! WSIDOT  Daily pest damage to stem mass (g/m2/day)
! WSRFDOT      Storage organ weight losses due to freezing (g[storage]/m2-d)
! WSRIDOT      Daily pest damage to storage organ mass (g/m2/day)
! WSTMD   Daily absolute stem damage (g/m2/day)
! WSTRD   Daily absolute storage organ damage (g/m2/day)
! WTLF    Dry mass of leaf tissue including C and N (g[leaf] / m2[ground])
!-----------------------------------------------------------------------
!     End Subroutine FOR_VEGDM
!-----------------------------------------------------------------------
