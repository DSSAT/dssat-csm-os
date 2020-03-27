C=======================================================================
C  RI_TILLSUB, Subroutine
C
C  Determines tillering
C-----------------------------------------------------------------------
C  Revision history
C
C  08/07/1993 PWW Header revision and minor changes
C  08/29/2002 CHP/MUS Converted to modular format for inclusion in CSM.
C=======================================================================

      SUBROUTINE RI_TILLSUB (DYNAMIC,
     &    AGEFAC, DTT, FLOOD, G2, G3, GPP, GRNWT, ISTAGE, !Input
     &    LAI, MGPP, MGROLF, MPLAG, NSTRES, P3, P4, P5,   !Input
     &    RGFILL, RTR, SLFN, SLFT, SLFW, SUMDTT, TCARBO,  !Input
     &    TEMF, TFILL, TGPP, TMPFIL, TSHOCK, TURFAC, XN,  !Input
     &    PSTRES1, PSTRES2, KSTRES,                       !Input 
     &    TGROGRN, TGROLF, TGROSTM, TPLAG,                !I/O
     &    TILNO, TLFWT, TSTMWT, TPLA)                     !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      INTEGER DYNAMIC, ISTAGE, ITDUR

      REAL DTT, FLOOD, G2, G3, GPP, GRNWT, LAI, MGROLF, MGPP, MPLAG
      REAL P3, P4, P5, RGFILL, SUMDTT, TCARBO, TFILL, TGROGRN
      REAL TGROLF, TGPP, TGROSTM, TILNO, TLFWT, TMPFIL
      REAL TPLA,  TPLAG, TSHOCK, TSTMWT, XN, YPTILNO

      REAL     B,DPTILNO,DTILNO,ECRAT,GNO,PTILNO,RTR,SENTIL,SLFN,SLFT
      REAL     SLFW,SLW,SWTIL,TILS,TILST,TRSM,TSTAGE,TEMF,TSLOPE
      REAL     TURFAC,NSTRES,AGEFAC, PSTRES1, PSTRES2, KSTRES

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!     Initialization - from inplt:
      TILNO    = 0.0
      YPTILNO  = 0.0 
      ITDUR    = 0
      TPLA     = 0.5

!***********************************************************************
!***********************************************************************
!     Daily rate / integration calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      SWTIL    = 1.0
      TRSM     = 1.0

      IF (FLOOD .LE. 0.0) THEN
         SWTIL = 1.5*EXP(TURFAC-1.0)-0.5
      ENDIF
      SWTIL = AMAX1 (SWTIL,0.0)
      IF (ISTAGE .LT. 3 .OR. (ISTAGE .EQ. 3 .AND. SUMDTT .LT. 80.)) THEN
         ITDUR = ITDUR + 1
         IF (ITDUR .EQ. 1) THEN
            PTILNO = 0.0
            TSTAGE = 0.0
         ENDIF
         YPTILNO = AMAX1 (PTILNO,0.0)
         TSLOPE =  1.35*G3
         PTILNO  = (-0.10+TSLOPE*(XN-4.0/G3)-0.376*(XN-4.0/G3)**2 +
!     &               0.0458*(XN-4.0/G3)**3)*G3                
     &               0.0458*(XN-4.0/G3)**3)*(G3**0.5)  !US              

         IF (LAI .GT. 7.0) THEN
           TRSM    = EXP (-1.1*(LAI-7.0))
         ELSE 
           TRSM = 1.0
         ENDIF

         TILST   = EXP (-0.40*TSTAGE)
         ECRAT   = RTR*TILST*TRSM
         ECRAT   = AMIN1 (ECRAT,1.0)
         DPTILNO = AMAX1 ((PTILNO-YPTILNO),0.0)
         DTILNO  = DPTILNO * ECRAT *
     &	         AMIN1 (AGEFAC**2,SWTIL,TEMF,TSHOCK, PSTRES2, KSTRES)
         TILNO   = TILNO + DTILNO
         TILNO   = AMAX1 (TILNO,0.0)
      ENDIF

      IF (MGROLF .GT. 0.0 .AND. MPLAG .GT. 0.0) THEN
        SLW = MGROLF / MPLAG
      ELSEIF (TGROLF .GT. 0.0 .AND. TPLAG .GT. 0.0) THEN
        SLW = TGROLF/TPLAG
      ELSE
        SLW = 0.0055
      ENDIF

      SELECT CASE (ISTAGE)
        CASE (1)
          TSTAGE  = 0.
          TGROLF  = TCARBO * 0.650     !0.5
          TGROSTM = TCARBO - TGROLF
          TPLAG   = TGROLF / SLW
          TPLA    = TPLA   + TPLAG

        CASE (2)
          TSTAGE  = 0   !1.0    + 0.5*SIND
          TPLAG   = TCARBO * 0.55/SLW *
     &      		  AMIN1 (TURFAC,NSTRES,TEMF, PSTRES1, KSTRES) 
          TGROLF  = TPLAG  * AMAX1 (0.0055,SLW)
          TGROSTM = TCARBO - TGROLF
          TPLA    = TPLA   + TPLAG

        CASE (3)
          TSTAGE  = 0   !1.5+3.0*SUMDTT/P3
          SENTIL  = TILNO*0.010*((SUMDTT/(P3+P4+P5))**2)
          TILS    = AMAX1(TILNO*(1.0-AMIN1(SLFW,SLFT,SLFN)),SENTIL)
          TILNO   = AMAX1(0.0,TILNO-TILS)
          B       = AMAX1((0.55-0.00005*SUMDTT-0.000002*SUMDTT**2),0.35) 
          IF (B .GT. 0.15 .AND. SUMDTT .GT. 0.75*P3) THEN
              B = 0.15
          ENDIF
          IF (SLW .LT. 0.0055) THEN
              SLW =0.0055
          ENDIF
          TPLAG   = B * TCARBO/SLW *
     &	          AMIN1(TURFAC,NSTRES,TEMF, PSTRES1, KSTRES)
          TGROLF  = TPLAG  * 0.0055
          TPLA    = TPLA   + TPLAG
          TGROSTM = TCARBO - TGROLF

        CASE (4)
          SENTIL  = TILNO*0.065*(((SUMDTT+P3)/(P4+P5+P3))**2)*(2.0-AMIN1 
     &    (AGEFAC,TURFAC,TEMF, PSTRES2, KSTRES))   !0.025
          TILS     =SENTIL
          TILNO   = AMAX1 (0.0,TILNO-TILS)
          TGROSTM = 0.015*DTT
          TGROSTM = AMIN1 (TGROSTM,TCARBO)
          IF (SUMDTT .LT. 30.0) THEN
             TGROLF  = TCARBO - TGROSTM
             TGROLF  = AMIN1 (TGROLF,B*TCARBO)
             TGROSTM = TCARBO - TGROLF
             TPLAG   = TGROLF / 0.0055 * 
     &		         AMIN1 (TURFAC,NSTRES,TEMF,PSTRES1,KSTRES)
             TPLA    = TPLA   + TPLAG
           ELSE
             TGROSTM = TCARBO
             TGROLF  = 0.0
             TPLAG   = 0.0
          ENDIF

        CASE (5)
          TGROGRN = (TGPP+MGPP-TFILL)*RGFILL*DTT*TMPFIL
          !Should this be g2*1.05? - chp
          GNO     = GRNWT/(G2*1.15)                  ! V2_1 1.05
          IF (GNO .GT. GPP) THEN
             TGROGRN = 0.0
          ENDIF
          TGROGRN = AMAX1 (TGROGRN,0.0)
          TGROGRN = AMIN1 (TGROGRN,TCARBO)
          TGROSTM = TCARBO - TGROGRN
          IF (TGROGRN .LT. 0.001) THEN
             TGROSTM = 0.0
          ENDIF

        CASE (6)
          IF (SUMDTT .GE. P5*0.90) THEN
             TGROGRN = (TGPP+MGPP-TFILL)*RGFILL*DTT*TMPFIL
             GNO     = GRNWT/(G2*1.05)
             IF (GNO .GT. GPP) THEN
                TGROGRN = 0.0
             ENDIF
             TGROGRN = AMAX1 (TGROGRN,0.0)
             TGROGRN = AMIN1 (TGROGRN,TCARBO)
             TGROSTM = TCARBO - TGROGRN
             IF (TGROGRN .LT. 0.001) THEN
                TGROSTM = 0.0
             ENDIF
          ENDIF
      END SELECT

      TLFWT  = TLFWT  + TGROLF
      TSTMWT = TSTMWT + TGROSTM

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
      RETURN
      END SUBROUTINE RI_TILLSUB
