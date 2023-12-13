C=======================================================================
C  ML_GROSUB, Subroutine
C
C  Growth subroutine for Pearl Millet
C-----------------------------------------------------------------------
C  Revision history
C
C                 Written
C  02/07/1993 PWW Header revision and minor changes         
C  06/  /1994 WTB Made to correspond to Millet 2.1 (7/18/91)
C  06/21/1994 JTR/BDB Updated PCARB calculation          
C  07/31/2002 WDB Converted to modular format               
C. 07/31/2002 WDB Added pest damage, SLPF, satfac           
C  03/12/2003 CHP Changed senescence variable to composite (SENESCE)
C                   as defined in ModuleDefs.for
!  03/30/2006 CHP Added composition of senesced matter for SOM modules
!  April-May 2015 - KJB major revisions
C-----------------------------------------------------------------------
C  Called : ML_CERES
C
C  Calls  : ML_NFACT ML_NUPTAK
C-----------------------------------------------------------------------

      SUBROUTINE ML_GROSUB (
     & AGEFAC, BIOMAS, CARBO, CNSD1,CNSD2, CO2X, CO2Y, 
     & CO2, CUMDTT, CUMPH, DLAYR,DM, DTT,
     & PRFTC, GFLTC, LAITC, PRFTYP,GFLTYP, LAITYP,
     & GPP, GRAINN, GROLF, GRORT, GROSTM, ISTAGE, 
     & ISWNIT, ISWWAT, LAI, LEAFNO, LFWT, LL, LWMIN, NDEF3, 
     & NFAC, NLAYR, NH4,NSTRES, NO3, P3, P4, P5, PAF, PANWT, 
     & PDWI, PGC, PGRORT, PHINT, PLA, PLAN, PLAG, PLAO, PLATO, 
     & PLTPOP, PTF, RANC, RCNP, RLV,ROOTN, ROWSPC, RTWT, 
     & SAT,SEEDRV, SENLA, SHF, SLAN, SLW, SRAD, 
     & STMWT, STOVN, STOVWT, SW, SUMDTT,  
     & SWFAC, TANC, TCNP,TEMF, TEMPM, TILN, 
     & TMAX, TMFAC1, TMIN, TMNC, TRNU,TSIZE, TURFAC,
     & XN,XSTAGE, EOP, TRWUP, RWUEP1,DYNAMIC,UNO3,UNH4,KG2PPM,
     & PORMIN,PARSR,RUE,SLPF,SATFAC, RESERVE,
     & ASMDOT,WLIDOT,WSIDOT,WRIDOT,PPLTD,SWIDOT,ISWDIS,G1,G0,
     & VANC,VMNC,TLAG1, SENESCE, MPLA, TPLA, MPLAG, TPLAG)

      USE ModuleDefs
      USE Interface_SenLig_Ceres
      IMPLICIT NONE
      EXTERNAL ML_NFACT, ML_TILLSUB, ML_NUPTAK, TABEX, CURV
      SAVE

C----------------------------------------------------------------
C      VARIABLES THAT WERE IN COMMON BLOCKS
C----------------------------------------------------------------
      REAL AGEFAC
      REAL ASMDOT
      REAL BIOMAS
      REAL CARBO
      REAL CNSD1
      REAL CNSD2
      REAL CO2X(10)    
      REAL CO2Y(10)
      REAL CO2 
      REAL CUMDTT
      REAL CUMPH
      REAL CURV
      REAL DM
      REAL DTT
      INTEGER DYNAMIC
      REAL EOP
      REAL EP1
      REAL GPP
      REAL GRAINN
      REAL GROLF
      REAL GRORT
      REAL GROSTM
      INTEGER ISTAGE
      CHARACTER ISWDIS*1
      CHARACTER ISWNIT*1
      CHARACTER ISWWAT*1
      CHARACTER*3 PRFTYP, GFLTYP, LAITYP
      REAL    PRFTC(4), GFLTC(4), LAITC(4)
      REAL  RTEMF
      REAL LAI
      INTEGER LEAFNO
      REAL LFWT
      REAL LWMIN
      REAL NDEF3
      REAL NFAC
      REAL NSTRES
      REAL P3
      REAL P4
      REAL P5
      REAL PAF
      REAL PANWT
      REAL PDWI
      REAL PGC
      REAL PGRORT
      REAL PHINT
      REAL PLA
      REAL PLAN
      REAL PLTPOP
      REAL PLAG
      REAL PLAO
      REAL PLATO
      REAL PTF
      REAL RANC
      REAL RCNP
      REAL ROOTN
      REAL ROWSPC
      REAL RTWT
      REAL RWUEP1
      REAL SEEDRV
      REAL SENLA
      REAL SLA 
      REAL SLAN
      REAL SLW
      REAL SRAD
      REAL STMWT
      REAL STOVN
      REAL STOVWT
      REAL SUMDTT
      REAL SWFAC
      REAL TANC
!     REAL TBASE
      REAL TCNP
      REAL TEMF
      REAL TEMPM
      REAL TILN
      REAL TMAX
      REAL TMFAC1(8)
      REAL TMIN
      REAL TMNC
      REAL TRWUP
      REAL TSIZE
      REAL TURFAC
      REAL UNO3(NL)
      REAL UNH4(NL)
      REAL XN
      REAL XSTAGE
C      PEST DAMAGE
      REAL WLIDOT
      REAL LAIDOT
      REAL WSIDOT
      REAL WRIDOT
      REAL PPLTD
      REAL SWIDOT


C-----------------------------------------------------------------
C   Local Variables
C-----------------------------------------------------------------

      INTEGER   I, L
	INTEGER   NLAYR
      REAL      NSINK,NPOOL1,NPOOL2,NPOOL,NSDR
      REAL      TT,PCARB,PRFT,TTMP,PC
      REAL      GRF,RGFILL,GROPAN,GROPANP
      REAL      RMNC,XNF,TNLAB,RNLAB,RNOUT,SLFW,SLFN,SLFC,SLFT,PLAS
      REAL      TABEX,PCO2
      REAL      DLAYR(NL)
      REAL      KG2PPM(NL)
      REAL      LIFAC
      REAL      LL(NL)
      REAL      NH4(NL)
      REAL      NO3(NL)
      REAL      PAR
      REAL      PARSR
      REAL      SLPF
      REAL      PORMIN
      REAL      RLV(NL)
      REAL      RUE
      REAL      SAT(NL)
      REAL      SATFAC
      REAL      SHF(NL)
      REAL      SUMEX
      REAL      SUMRL
      REAL      SW(NL)
      REAL      SWEXF
      REAL      TRNU
      REAL      TSS(NL)

c--------------------------------------------------------------------
c        Variables for Millet
C--------------------------------------------------------------------
      REAL TOPSINK,TOPMAIN,MGROLF,MGROSTM,TCARBO
      REAL TGROLF,TGROSTM
      REAL MLAG2,MPLAG,MLAG1,TPLAG,SENF,RESERVE
      REAL G1,G0,STMGF,VANC,VMNC,MPLA,TPLA,MLFWT,TLFWT
      REAL MSTMWT,TSTMWT,TLAG1

!----------------------------------------------------------------------
      TYPE (ResidueType) SENESCE   
!     CHP 3/31/2006
!     Proportion of lignin in STOVER and Roots
      REAL PLIGLF, PLIGRT

C--------------------------------------------------------------------
C              DYNAMIC = RUNINIT OR SEASINIT
C--------------------------------------------------------------------
      IF(DYNAMIC .EQ. RUNINIT .OR. DYNAMIC .EQ. SEASINIT) THEN

        CALL SenLig_Ceres(PLIGLF=PLIGLF, PLIGRT=PLIGRT)

         XSTAGE = 0.0
         AGEFAC = 0.0
         BIOMAS = 0.0
         CARBO = 0.0
         DM = 0.0
         GPP = 0.0
         GRAINN = 0.0
         GROLF = 0.0
         GRORT = 0.0
         GROSTM = 0.0
         LAI = 0.0
         LEAFNO = 0
         LFWT = 0.0
         LWMIN = 0.0
         PAF = 0.0
         PANWT = 0.0
         PDWI = 0.0
         PGC = 0.0
         PGRORT = 0.0
         PLA = 0.0
         PLAN = 0.0
         PLAG = 0.0
         PLAO = 0.0
         PLATO = 0.0
         PTF = 0.0
         RLV = 0.0
         ROOTN = 0.0
         RTWT = 0.0
         SENLA = 0.0
         SLA = 0.0
         SLAN = 0.0
         SLW = 0.0
         STMWT = 0.0
         STOVN = 0.0
         STOVWT = 0.0
         TILN = 1.0
         TRNU = 0.0
         TSIZE  = 0.0
         XN     = 0.0
         MPLA   = 0.0
         TPLA   = 0.0
         MLFWT  = 0.0
         TLFWT  = 0.0
         MSTMWT = 0.0
         TSTMWT = 0.0
         MGROSTM = 0.0
         MSTMWT = 0.0
         MGROLF = 0.0
         MLFWT  = 0.0
         STMWT  = 0.0
         PANWT  = 0.0
         LFWT   = 0.0
         BIOMAS = 0.0
         MLAG1  = 0.0
         MLAG2  = 0.0
!KJB INITIALIZED MPLAG, CRAZY RESULTS AT COLD TEMPERATURE 14C, BUT THIS WAS NOT THE PROBLEM
! THE PROBLEM WAS TOO MUCH CARBO.  MPLAG INITIALIZED IN PHASEI.FOR.  BUT TWICE IS NO PROBLEM
         PCARB = 0.0
         MPLAG = 0.0
! KJB - added NSINK here.  If used with N "off", then it should be zero
         NSINK = 0.0
         SENESCE % ResWt  = 0.0
         SENESCE % ResLig = 0.0
         SENESCE % ResE   = 0.0


C-----------------------------------------------------------------------  
C-----------------------------------------------------------------------
C
C                     DYNAMIC = RATE
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      ELSEIF(DYNAMIC.EQ.RATE) THEN

          !daily senescence
          SENESCE % ResWt  = 0.0
          SENESCE % ResLig = 0.0
          SENESCE % ResE   = 0.0


C--------------------------------------------------------------------
C                   DYNAMIC = INTEGRATE
C--------------------------------------------------------------------
      ELSEIF(DYNAMIC.EQ.INTEGR) THEN

      !----------------------------------------------------------
      !   Compute Nitrogen Stress Factors
      !----------------------------------------------------------

      IF (ISWNIT .NE. 'N') THEN
         CALL ML_NFACT(
     &   AGEFAC, CNSD1, CNSD2, ISTAGE, NDEF3, NFAC,
     &   NSTRES, RANC, RCNP, TANC, TCNP, TMNC, XSTAGE)
      ENDIF

      !-------------------------------------------------------------
      !      Compute Water Stress Factors       
      ! ------------------------------------------------------------
      SWFAC  = 1.0
      TURFAC = 1.0

      IF(ISWWAT.NE.'N') THEN
         IF (EOP .GT. 0.0) THEN
            EP1 = EOP * 0.1

            IF (TRWUP / EP1 .LT. RWUEP1) THEN
               TURFAC = (1./RWUEP1) * TRWUP / EP1
            ENDIF

            IF (EP1 .GE. TRWUP) THEN
               SWFAC = TRWUP / EP1
            ENDIF

         ENDIF
      ENDIF

      TURFAC = REAL(INT(TURFAC*1000))/1000

      !-------------------------------------------------------------
      !      Compute Water Saturation Factors       
      ! ------------------------------------------------------------
      SATFAC = 0.0    
      SUMEX = 0.0
      SUMRL = 0.0

      DO L = 1,NLAYR

         !------------------------------------------------------------
         !PORMIN = Minimum pore space required for supplying oxygen to 
         !         roots for optimum growth and function    
         !TSS(L) = Number of days soil layer L has been saturated 
         !         above PORMIN
         !------------------------------------------------------------
         IF ((SAT(L)-SW(L)) .GE. PORMIN) THEN
            TSS(L) = 0.
         ELSE
            TSS(L) = TSS(L) + 1.
         ENDIF

         !------------------------------------------------------------
         ! Delay of 2 days after soil layer is saturated before root
         ! water uptake is affected
         !------------------------------------------------------------

         IF (TSS(L) .GT. 2.) THEN
            SWEXF = (SAT(L)-SW(L))/PORMIN
            SWEXF = MAX(SWEXF,0.0)
         ELSE
            SWEXF = 1.0
         ENDIF

         SWEXF = MIN(SWEXF,1.0)
         SUMEX  = SUMEX + DLAYR(L)*RLV(L)*(1.0 - SWEXF)
         SUMRL  = SUMRL + DLAYR(L)*RLV(L)
      ENDDO

      IF (SUMRL .GT. 0.0) THEN
         SATFAC = SUMEX/SUMRL
      ELSE
         SATFAC = 0.0
      ENDIF

      SATFAC = AMAX1(SATFAC,0.0)
      SATFAC = AMIN1(SATFAC,1.0)


      !--------------------------------------------------------------
      !          Initialize Some Variables
      !--------------------------------------------------------------
      TOPSINK = 0.0
      TOPMAIN = 0.0
      MGROLF  = 0.0
      MGROSTM = 0.0
      TGROLF  = 0.0
      TGROSTM = 0.0
      GROPAN  = 0.0
      GRORT   = 0.0
      TCARBO  = 0.0

      !-------------------------------------------------------------
      !           Daily Photosynthesis Rate
      !-------------------------------------------------------------
                
      PAR = SRAD*PARSR        !PAR local variable
C
C KJB Equation is much too sensitive to Row Spacing, Boote,Singh, Jones
C     The coefficient 0.05 makes it less sensitive, but also increases LIFAC
C     The 0.05 has considerable effect.  Try 0.06 or 0.04
C     Needed 0.04 to get reasonable yield for 150 cm row spacing in 2 expts.
C     May need to reduce 1.5 to 1.4 or decrease the slope of 0.768.  NO!
C     Note:  model is extremely sensitive to change from 1.5 to 1.4.
C
      LIFAC =1.48 - 0.768 * ((ROWSPC * 0.01)**2 * PLTPOP)**0.04

C      LIFAC =1.5 - 0.768 * ((ROWSPC * 0.01)**2 * PLTPOP)**0.1
      PCO2  = TABEX (CO2Y,CO2X,CO2,10)
      IF (PLTPOP .GT. 0.0) THEN
         PCARB = RUE * PAR/PLTPOP * (1.0 - EXP(-LIFAC * LAI))
      ELSE
         PCARB = 0.0
      ENDIF
      PCARB = PCARB*PCO2      !chp added

      TEMPM = (TMAX + TMIN)*0.5   !Mean air temperature, C

      TT    = 0.25*TMIN+0.75*TMAX 
      PRFT = CURV(PRFTYP,PRFTC(1),PRFTC(2),PRFTC(3),PRFTC(4),TT)
      PRFT  = AMAX1 (PRFT,0.0)
      PRFT = MIN(PRFT,1.0)


C      Saturated soil water content reduces RWU and causes damage
C      through SWFAC. Do not need to also cause reduction through SATFAC
C WDB 10/20/03      CARBO = PCARB*AMIN1 (PRFT,SWFAC,NSTRES,(1-SATFAC))*SLPF
      CARBO = PCARB*AMIN1 (PRFT,SWFAC,NSTRES)*SLPF
      !Reduce CARBO for assimilate pest damage
      CARBO = CARBO - ASMDOT
      CARBO = MAX(CARBO,0.0)


      !---------------------------------------------------------------
      !      Compute temperature effect on leaf expansion
      !---------------------------------------------------------------
C KJB  Deleted this code, replaced with look-up function
C      TEMF  = 1.0
C
C      DO I = 1, 8
C         TMFAC1(I) = 0.931 + 0.114*I-0.0703*I**2+0.0053*I**3
C      END DO
C
C
C      IF (TMIN .LT. 14.0 .OR. TMAX .GT. 32.0) THEN
C         IF (TMAX .LT. TBASE) THEN
C            TEMF = 0.0
C         ENDIF
C         IF (TEMF .NE. 0.0) THEN
C            TEMF = 0.0
C            DO I = 1, 8
C               TTMP = TMIN + TMFAC1(I)*(TMAX-TMIN)
C               IF (TTMP .GT. 14.0 .AND. TTMP .LE. 32.0) THEN
C                   TEMF = TEMF + 1.0/8.0
C               ENDIF
C               IF (TTMP .GE.  8.0 .AND. TTMP .LT. 14.0) THEN
C                   TEMF = TEMF + 0.021*(TTMP-8.0)
C               ENDIF
C               IF (TTMP .GT. 32.0 .AND. TTMP .LT. 42.0) THEN
C                   TEMF = TEMF + 0.0125*(42.0-TTMP)
C               ENDIF
C            END DO
C         ENDIF
C      ENDIF

C  KJB, NOW USING LOOK-UP 4-POINT LOOK-UP, WITH LIN, QDR possible
      DO I = 1, 8
      TMFAC1(I) = 0.931 + 0.114*I-0.0703*I**2+0.0053*I**3
      END DO
      TEMF = 0.0      
      DO I = 1, 8
               TTMP = TMIN + TMFAC1(I)*(TMAX-TMIN)
      RTEMF = CURV(LAITYP,LAITC(1),LAITC(2),LAITC(3),LAITC(4),TTMP)
      RTEMF  = AMAX1 (RTEMF,0.0)
      RTEMF = AMIN1(RTEMF,1.0)
      TEMF = TEMF + RTEMF / 8.
      END DO

C     TEMF SHOULD BE USED AS BEFORE
      !--------------------------------------------------------------
      !       Compute leaf characteristics for different stages
      !--------------------------------------------------------------

      DTT = AMAX1 (DTT,0.0)
      IF (ISTAGE .LE. 3) THEN
         PC    = 1.0
         IF (CUMPH .LT. 5.0) THEN
            PC = 0.66 + 0.068 * CUMPH
         ENDIF
         CUMPH  = CUMPH + DTT / (PHINT * PC)
         XN     = CUMPH
         LEAFNO = XN
      ENDIF

      !--------------------------------------------------------------
      !     ISTAGE = 2
      !--------------------------------------------------------------

C KJB - Note:  I think there is a flaw in logic for all leaf area expansion eqs.
C KJB - as there should be a "delta HU", e.g. DTT in all these equations to represent
C KJB - the increment of that leaf area expansion expressed per day.  True for MS
C KJB - and tillers.  If this was corrected, I think the model would show higher
C KJB - LAI at warm conditions.  Right now LAI is highest if quite cool (20C)
C KJB - in the short term, I am avoiding this change, as the model is mostly balanced.
C KJB - DTT alone is not enough.  Need to create cm2 per DTT. or DTT/24 equals thermal day
C          MPLAG   = DTT/24. * (MLAG2-MLAG1) * AMIN1(TURFAC,TEMF,AGEFAC)
C KJB  - I tried this.  It did not do much.  Do not understand it.
C
      IF (ISTAGE .LE. 2) THEN
          MLAG2   = G1 * (18.0*EXP(0.00787*CUMDTT)-18.4)
          MPLAG   = (MLAG2-MLAG1) * AMIN1(TURFAC,TEMF,AGEFAC)  
          MGROLF  = MPLAG  * 0.004
          MGROSTM = MGROLF * 0.20
          IF (CUMDTT .LE. 125.0) THEN
             IF(SEEDRV .GT. 0.0) THEN
! KJB - COOL, 14-17C, SEEDRV GOES WAY UP, AS HIGH AS 0.634, WHEN SEEDRV =0.006 INITIAL
! THIS CAUSED HAYWIRE CARBO IN NEXT LINE, POSSIBLY BECAUSE TEMPERATURE ON PCARB IS WRONG
! KJB - DECIDE SEEDRV SHOULD NEVER EXCEED TWICE INITIAL SEED WT, WHICH IS 6 MG
! KJB - SO PUT A LIMIT ON SEEDRV OF 0.012 G
! KJB - THIS IS ONLY PART OF THE PROBLEM. PUSHING THE TB AND TOPT FOR PCARB UP, HELPED 
! KJB - CONSIDERABLY.  REMEMBER THAT PCARB'S TT USES 3/4TH OF TMAX AND 1/4 OF TMIN
!                 CARBO = SEEDRV + CARBO
                 CARBO = AMIN1(SEEDRV,0.012) + CARBO
                     IF (MGROLF .LT. CARBO) THEN
                   GRORT = MGROLF
                     ENDIF
!                SEEDRV = CARBO - MGROLF - GRORT
                SEEDRV = CARBO - MGROLF - GRORT
                     IF (SEEDRV .GT. 0.012) THEN 
                          SEEDRV = 0.012
                     ENDIF
                IF (SEEDRV .LT. 0.0) THEN
                   GRORT  = 0.25*CARBO
                   MGROLF = CARBO-GRORT
                   SEEDRV = 0.0
                ENDIF
             ELSE
                GRORT = CARBO - MGROSTM - MGROLF
             END IF
             IF (GRORT .LT. 0.25*CARBO) THEN
                 GRORT   = CARBO*0.25
                 MGROLF  = 0.65*CARBO
                 MGROSTM = CARBO-GRORT-MGROLF
                 MPLAG   = MGROLF/0.0040
             END IF
           ELSEIF (CUMDTT .GT. 125.0) THEN
             CALL ML_TILLSUB (DYNAMIC,TURFAC,AGEFAC,
     &       G0,CUMDTT,TPLAG,TLAG1,TGROLF,TGROSTM,TCARBO,
     &       CARBO,SUMDTT,DTT,TEMF, ISTAGE)
          END IF

          TOPSINK = MGROSTM + MGROLF + TGROLF + TGROSTM
          TCARBO  = TGROLF  + TGROSTM

          IF (TOPSINK .GT. 0.80*CARBO) THEN
             GRORT   = CARBO   * 0.20
             GRF     = CARBO   * 0.80/TOPSINK
             MGROLF  = MGROLF  * GRF
             MGROSTM = MGROSTM * GRF
             MPLAG   = MGROLF  / 0.0040
             TGROLF  = TGROLF  * GRF
             TGROSTM = TGROSTM * GRF
             TPLAG   = TGROLF  / 0.0040
           ELSE
             GRORT   = CARBO - TOPSINK
          ENDIF

      !--------------------------------------------------------------
      !     ISTAGE = 3
      !--------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 3) THEN
          MLAG2  = G1 * (18.0*EXP(0.00787*CUMDTT)-18.4)
          MPLAG  = (MLAG2-MLAG1)* AMIN1(TURFAC,TEMF,AGEFAC)
          MGROLF = MPLAG*0.0045
          GRORT  = CARBO*0.15
          IF (SUMDTT .GT. P3-75.0) THEN              ! P3-75
             MPLAG  = MPLAG*(1.0-((SUMDTT-P3+75.0)/125.0))
             MGROLF = MPLAG*0.0045
          END IF
          MGROSTM = MGROLF*(0.20+1.0*SUMDTT/P3)
          TOPMAIN = MGROSTM + MGROLF
          TCARBO  = CARBO   - MGROLF - MGROSTM - GRORT
C-WALTER  GRF     = 0.45*AMIN1(AGEFAC,TEMF,SWFAC)
C KJB    increasing GRF as function of lack of canopy cover
C        shifts more of LAI to tillers and less to main stem
C
          GRF     = 0.45 * (1.0 + 0.4 * EXP(-LIFAC * LAI))
C          GRF     = 0.55 
     &               *AMIN1(AGEFAC,TEMF,SWFAC)
C     &    *AMIN1(AGEFAC,TEMF,TURFAC)
          IF (TCARBO .LT. GRF*CARBO) THEN
             TCARBO  = GRF*CARBO
             MGROLF  = (CARBO-GRORT-TCARBO)*(MGROLF/TOPMAIN)
             MGROSTM =  CARBO-GRORT-TCARBO-MGROLF
             MPLAG   = MGROLF/0.0045
          END IF
          IF (TCARBO .GT. 0.0) THEN
              CALL ML_TILLSUB (DYNAMIC,TURFAC,AGEFAC,
     &        G0,CUMDTT,TPLAG,TLAG1,TGROLF,TGROSTM,TCARBO,
     &        CARBO,SUMDTT,DTT,TEMF, ISTAGE)
          ENDIF

      !--------------------------------------------------------------
      !     ISTAGE = 4
      !--------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 4) THEN
C-WALTER  MGROSTM = 0.095*DTT*AMIN1(TEMF,AGEFAC,SWFAC)   ! 0.15
C-KJB     MGROSTM = 0.095*DTT*AMIN1(TEMF,AGEFAC,TURFAC)  ! 0.15
           MGROSTM = 0.095*DTT*AMIN1(TEMF,AGEFAC,SWFAC)  ! 0.15
          GRORT   = CARBO*(0.25-0.15*AMIN1(SWFAC,NSTRES))
          TCARBO  = CARBO-MGROSTM-GRORT
C-WALTER  GRF     = 0.50*AMIN1(AGEFAC,TEMF,SWFAC)
C-KJB     GRF     = 0.50*AMIN1(AGEFAC,TEMF,TURFAC)
          GRF     = 0.50*AMIN1(AGEFAC,TEMF,SWFAC)
          IF (TCARBO .LT. CARBO*GRF) THEN
             TCARBO  = CARBO*GRF
             MGROSTM = CARBO-GRORT-TCARBO
          ENDIF
          IF (TCARBO .GT. 0.0) THEN
              CALL ML_TILLSUB (DYNAMIC,TURFAC,AGEFAC,
     &        G0,CUMDTT,TPLAG,TLAG1,TGROLF,TGROSTM,TCARBO,
     &        CARBO,SUMDTT,DTT,TEMF, ISTAGE)
          ENDIF
      !--------------------------------------------------------------
      !     ISTAGE = 5
      !--------------------------------------------------------------
      ELSEIF (ISTAGE .EQ. 5) THEN
C-KJB     SENF   = 0.85-0.5*AMIN1(AGEFAC,TURFAC)
          SENF   = 0.85-0.5*AMIN1(AGEFAC,SWFAC)
          CARBO  = CARBO*(1.-SENF*(SUMDTT/P5))   !0.40
          RGFILL = 1.0
! KJB Replaced with a look up function
!          IF (TEMPM .LT. 22.) THEN
!             RGFILL = (TEMPM-7.0)/15.0
!          ENDIF
!          RGFILL = AMAX1 (RGFILL,0.0)
      RGFILL = CURV(GFLTYP,GFLTC(1),GFLTC(2),GFLTC(3),GFLTC(4),TEMPM)
      RGFILL  = AMAX1 (RGFILL,0.0)
      RGFILL = MIN(RGFILL,1.0)
          GROLF  = 0.0
          IF (SUMDTT .LE. P5) THEN
C-WALTER     GROPAN = PGC*(1.0+0.0040*SUMDTT)*AMIN1(SWFAC,AGEFAC)   !.0036
C-KJB        GROPAN = PGC*(1.0+0.0040*SUMDTT)*AMIN1(TURFAC,AGEFAC) 
!             GROPAN = PGC*(1.0+0.0040*SUMDTT)*AMIN1(SWFAC**0.5,AGEFAC)
C KJB - used SWFAC**0.8 and AGEFAC on GROPAN all the time, not just lag phase
             GROPAN = PGC*(1.0+0.0040*SUMDTT)
             IF (GROPAN .GT. 1.75*PGC) THEN
                GROPAN = 1.75*PGC
             ENDIF
!             GROPAN = GROPAN*RGFILL
             GROPAN = GROPAN*RGFILL*AMIN1(SWFAC**0.8,AGEFAC)
             GRORT  = CARBO*(0.3-0.28*AMIN1(SWFAC,NSTRES))
             GROSTM = CARBO - GRORT - GROPAN
             IF (GROSTM .LT. 0.0) THEN
                IF (RESERVE .LE. 0.0) THEN
                   GROPAN  = CARBO - GRORT
                   GROSTM  = 0.0
                 ELSE
                   RESERVE = RESERVE+GROSTM
                   GROLF   = GROSTM*0.3
                   GROSTM  = GROSTM*0.7

!                  CHP 12/14/2006
!                  Don't let GROSTM result in negative STMWT
                   GROSTM = AMAX1(GROSTM, -STMWT)
                ENDIF
              ELSE
!  KJB          STMGF   = AMIN1((0.5+0.003*SUMDTT),1.0)
                STMGF   = AMIN1((0.6+0.003*SUMDTT),1.0)
                RESERVE = RESERVE+GROSTM*STMGF
              ENDIF
                GROPANP = GROPAN
          ELSE
! KJB PRIOR GROPAN WAS FOR A FIXED PLANT, NOT GOOD, NOW NORMALIZED
! ALSO FASTER, AS THIS IS AFTER P5 COMPLETION
!             GROPAN = 0.72-0.006*(SUMDTT-P5)         ! .0067
             GROPAN = GROPANP * (0.72-0.007*(SUMDTT-P5))/0.72
             GROSTM = CARBO - GROPAN
             IF (GROSTM .LT. 0.0) THEN
                GROSTM = 0.0
                GROPAN = CARBO
             ENDIF
          END IF
          !
          ! Used by N Submodel
          !
          IF (ISWNIT .NE. 'N') THEN
             !
             ! Adjusted from wheat model to millet grain size
             !
             NSINK = GROPAN*TCNP*AGEFAC
             IF (NSINK.NE.0.0) THEN
                RMNC   = RCNP*0.75
                VANC   = STOVN/STOVWT
                NPOOL1 = STOVWT*(VANC-VMNC)
                NPOOL2 = RTWT*(RANC-RMNC)
                NPOOL2 = AMAX1 (NPOOL2,0.0)
                NPOOL1 = AMAX1 (NPOOL1,0.0)
                XNF    = 0.1+0.2*NFAC
                TNLAB  = XNF*NPOOL1
                RNLAB  = XNF*NPOOL2
                NPOOL  = TNLAB+RNLAB
                NSDR   = NPOOL/NSINK
                IF (NSDR .LT. 1.0) THEN
                   NSINK = NSINK*NSDR
                ENDIF

                IF (NSINK .GT. TNLAB) THEN
                   VANC  = VMNC
                   STOVN = STOVN-TNLAB
                   RNOUT = NSINK-TNLAB
                   TNLAB = 0.0
                   ROOTN = ROOTN-RNOUT
                   RANC  = ROOTN/RTWT
                 ELSE
                   STOVN = STOVN-NSINK
                   VANC  = STOVN/STOVWT
                ENDIF
             ENDIF
             GRAINN = GRAINN+NSINK
          ENDIF
      !--------------------------------------------------------------
      !     ISTAGE = 6
      !--------------------------------------------------------------

       ELSEIF (ISTAGE .EQ. 6) THEN
          RETURN
      ENDIF   !End if ISTAGE loop



C--------------------------------------------------------------
C                   APPLY PEST DAMAGE
C--------------------------------------------------------------

      IF(ISWDIS.EQ.'Y') THEN
      ! Leaf Damage
          IF((LFWT+STMWT).GT.0.0)
     $      STOVN=STOVN - STOVN*(WLIDOT/PLTPOP)/(LFWT+STMWT)
          IF (PLTPOP.GT.0.0.AND.LFWT.GT.0.0)
     &      LAIDOT = WLIDOT*(PLA-SENLA)/(LFWT*PLTPOP)  !cm2/plant/day
          IF(PLTPOP.GT.0.0)
     &      LFWT = LFWT - WLIDOT/PLTPOP
          MPLA = MPLA - LAIDOT
          LAI = LAI - LAIDOT*PLTPOP/10000

      ! Stem Damage
          IF(PLTPOP.GT.0.0)
     &       STMWT = STMWT - WSIDOT/PLTPOP
          IF(PLTPOP.GT.0.0.AND.(LFWT+STMWT).GT.0.0)
     &       STOVN=STOVN - STOVN*(WSIDOT/PLTPOP)/(LFWT+STMWT)

      ! Root Weight
          IF(PLTPOP.GT.0.0) THEN
            ROOTN = ROOTN - ROOTN*(WRIDOT/PLTPOP)/RTWT
            RTWT = RTWT - WRIDOT/PLTPOP
          ENDIF

      ! Grain Weight and Number
          IF (PANWT .GT. 0.AND.PLTPOP.GT.0) THEN
            GRAINN = GRAINN - GRAINN*(SWIDOT/PLTPOP)/PANWT
            GPP = GPP - GPP*(SWIDOT/PLTPOP)/PANWT
          ENDIF

          IF(PLTPOP.GT.0.0) THEN 
            PANWT = PANWT - SWIDOT/PLTPOP
          ENDIF

      ! Population
          IF(PPLTD.GT.0) THEN
             PLTPOP = PLTPOP - PLTPOP * PPLTD/100
             LAI = LAI - LAI*(PPLTD/100)
           ENDIF
      ENDIF


C--------------------------------------------------------------------
C      The following code is executed every day regardless of ISTAGE
C--------------------------------------------------------------------
      IF (ISTAGE .NE. 6) THEN
         MPLA   = MPLA   + MPLAG
         TPLA   = TPLA   + TPLAG
         PLA    = MPLA   + TPLA
! KJB - ATTEMPT TO TRANSFER MASS OF N AWAY FROM VEG
! KJB  - DO WE NEED TO PUT IT INTO GRAIN TOO?
! KJB  - REASON:  HARVEST INDEX IS LOW BECAUSE OF POOR ACCOUNTING
! KJB  - IF AS PROTEIN, MAY NEED A MULTIPLIER OF 6.25 TO GET DM
         !         MLFWT  = MLFWT  + MGROLF
         MLFWT  = MLFWT  + MGROLF - 6.25* NSINK * MLFWT /(LFWT+STMWT)
!         TLFWT  = TLFWT  + TGROLF
         TLFWT  = TLFWT  + TGROLF - 6.25* NSINK * TLFWT /(LFWT+STMWT)
!         MSTMWT = MSTMWT + MGROSTM
!         TSTMWT = TSTMWT + TGROSTM
         MSTMWT = MSTMWT + MGROSTM - 6.25*NSINK * MSTMWT /(LFWT+STMWT)
         TSTMWT = TSTMWT + TGROSTM - 6.25*NSINK * TSTMWT /(LFWT+STMWT)
         IF (ISTAGE .NE. 5) THEN
            GROSTM = MGROSTM + TGROSTM
            STMWT  = MSTMWT  + TSTMWT
            GROLF  = MGROLF  + TGROLF
            LFWT   = MLFWT   + TLFWT
         ELSE
! KJB - looks strange, but STMWT and LFWT never defined to this point. Need
! KJB - to substract NSINK during ISTAGE5.  ALSO, GROSTM is never defined
! KJB by itself until ISTAGE5, so that is why GROSTM and GROLF needed again
! KJB - code works to causes DM decline in leaf and stem, under concept that
! KJB - protein DM is lost from veg tissue, but half "C-energy" is respired 
! KJB - Hence, 3.12* NSINK going to PANWT
! KJB - prior code had no loss of DM from leaf or stem
!            STMWT  = STMWT   + GROSTM
            STMWT  = STMWT   + GROSTM - 6.25*NSINK * STMWT /(LFWT+STMWT)
            PANWT  = PANWT   + GROPAN + 3.12*NSINK * STMWT /(LFWT+STMWT)
!            LFWT   = LFWT    + GROLF
            LFWT   = LFWT    + GROLF - 6.25* NSINK * LFWT /(LFWT+STMWT)
         ENDIF
         BIOMAS = (LFWT+STMWT+PANWT)*PLTPOP
         DM     = BIOMAS*10.0
         STOVWT = LFWT+STMWT
         IF (CARBO .EQ. 0.0)  THEN
            CARBO = 0.001
         ENDIF
         PDWI   = PCARB*(1.0-GRORT/CARBO)
         PGRORT = PCARB-PDWI
         PTF    = 1.0-GRORT/CARBO
         RTWT   = RTWT+0.5*GRORT-0.005*RTWT

         IF (ISTAGE .LT. 3) SLAN = 0.0
         IF (ISTAGE .EQ. 3) SLAN = PLA*(0.05*(SUMDTT/P3)**2)
         IF (ISTAGE .EQ. 4) SLAN = PLA*(0.05+0.75*((SUMDTT/(P4+P5))**2))
         IF (ISTAGE .EQ. 5) THEN
            SLAN = PLA*(0.05+0.75*((SUMDTT+P4)/(P4+P5))**2)
            IF (SUMDTT .GT. P5) THEN
               SLAN = PLA*(0.8+0.00049*(SUMDTT-P5))
            ENDIF
         ENDIF

         IF (CARBO .EQ. 0.0) THEN
            CARBO = 0.001
         ENDIF
         SLFW   = 0.95+0.05*SWFAC
         SLFN   = 0.95+0.05*AGEFAC
         SLFC   = 1.0
         IF (LAI .GT. 4.0) THEN
            SLFC = 1.0-0.008*(LAI-4.0)
         ENDIF
         SLFT   = 1.0
         IF (TEMPM .LE. 6.0) SLFT = 1.0-(6.0-TEMPM)/6.0
         IF (TMIN  .LE. 0.0) SLFT = 0.0
         SLFT   = AMAX1 (SLFT,0.0)
         PLAS   = (PLA-SENLA)*(1.0-AMIN1(SLFW,SLFC,SLFT,SLFN))
         SENLA  = SENLA+PLAS
         SENLA  = AMAX1 (SENLA,SLAN)
         SENLA  = AMIN1 (SENLA,PLA)

!        Specific leaf area (cm2/g)
         IF (LFWT .GT. 0.) THEN
           SLA = LAI*10000/LFWT  
         ELSE    
           SLA = 0
         ENDIF

         LAI    = (PLA-SENLA)*PLTPOP*0.0001
         MLAG1  = MLAG2

      ENDIF




 
      !--------------------------------------------------------------
      !       Compute Nitrogen Uptake
      !--------------------------------------------------------------

      IF (ISWNIT .NE. 'N') THEN
      CALL ML_NUPTAK(
     %    RANC, ROOTN,RTWT,TANC,STOVN,STOVWT,TRNU,NLAYR,
     %    RLV,NO3,NH4,PDWI,TCNP,UNO3,UNH4,
     %    XSTAGE,RCNP,PGRORT,PLTPOP,SW,LL,SAT,DLAYR,
     %    SHF,PTF, SENESCE, KG2PPM, PLIGRT)
      ENDIF


C--------------------------------------------------------------------
C     END OF DYNAMIC IF-BLOCK
C--------------------------------------------------------------------
      ENDIF

      RETURN
      END SUBROUTINE ML_GROSUB

C                         DEFINITIONS
C
C  NSINK  : Demand for N associated with grain filling (g/plant/day)
C  NPOOL  : Total plant N available for translocation to grain (g/plant)
C  NPOOL1 : Tops N available for translocation to grain (g/plant)
C  NPOOL2 : Root N available for translocation to grain (g/plant)
C  NSDR   : Plant N supply/demand ratio used to modify grain N content
C  I      : Loop counter
C  PAR    : Daily photosynthetically active radiation, calculated as half
C           the solar radiation - MJ/square metre
C  TT     :
C  PCARB  : Daily amount of carbon fixed - g
C  PRFT   : Photosynthetic reduction factor for low and high temperatures
C  TTMP   :
C  PC     :
C  TI     : Fraction of a phyllochron interval which occurred as a fraction
C           of today's daily thermal time
C  XTN    :
C  A      : Zero to unity factor for relative nitrification rate (unitless)
C  RTR    :
C  TC1    :
C  TC2    :
C  PLATN  :
C  PLAGT  :
C  GRF    :
C  TLG    :
C  FLG    :
C  GGG    :
C  RGFILL : Rate of grain fill - mg/day, NO.  Just relative rate of grain fill
C  WSTR   :
C  FSTR   :
C  GROPAN : Daily growth of the pannicle - g
C  RGNFIL :
C  RMNC   : Root minimum nitrogen concentration (g N/g root dry weight)
C  XNF    :
C  TNLAB  :
C  RNLAB  :
C  RNOUT  :
C  SLFW   :
C  SLFN   :
C  SLFC   :
C  SLFT   :
C  PLAS   : The rate of senescence of leaf area on one plant - sq. cm/day
C=======================================================================
!PAR - photosynthetically active radiation, MJ/m2/d
!PARSR - conversion from solar radiation to PAR
!LIFAC - light interception factor used to reduce photosynthesis under
!        wide rowspacing or low population (0-1.0)
!RUE - radiation use efficiency (from ecotype file), g CH2O/MJ PAR
!PCARB       !Potential dry matter production under optimum water, nitrogen and temperature, g/plant
!ROWSPC - Row spacing, cm
!LAI - leaf area index, m2 leaf/m2 ground
!ASMDOT - daily assimilate damage due to pests, g/plant/day
!CARBO - daily plant growth rate, g/plant/day
!TEMPM - average daily temperature, C
!TT - weighted average daily temperature for computing temperatue effect on photosynthesis
!
