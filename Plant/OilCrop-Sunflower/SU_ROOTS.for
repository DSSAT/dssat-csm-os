C======================================================================
C  SU_ROOTGR, Subroutine
C
C  Computes Daily Root Growth and Distribution
C----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by                           E. Alocilja & B. Baer 9-88
C  3  Modified by                           T. Jou                4-89
C  4. Header revision and minor changes             P.W.W.      2-8-93
C  5. Added switch block, etc.                      P.W.W.      2-8-93
C  6. Simplified the RLNEW calculation and slowed
C     the growth of roots in deeper soils.   J.T.R. & B.D.B.    6-20-94
C  7. Converted to modular routine                  W.D.B.      4-01-01
C  8. Further modular changes                       W.D.B      12-01-01
C----------------------------------------------------------------------
C  Called : MAIZE
C
C  Calls  : None
C----------------------------------------------------------------------

      SUBROUTINE SU_ROOTGR (DYNAMIC,ISWNIT,                       !C
     &        CUMDEP,DEPMAX,DLAYR,DTT,ESW,GRORT,ISTAGE,           !I
     %        LL,DUL,NO3,NH4,NLAYR,PLTPOP,PORMIN,RLWR,SAT,SDEPTH, !I
     %        SHF,STGDOY,SW,SWFAC,YRDOY,                          !I
     %        RTDEP,RLV)            


C----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT  NONE
      SAVE

      INTEGER     DYNAMIC     

      REAL        CUMDEP     
!     REAL        CUMDTT      
      REAL        DEPMAX      
      REAL        DLAYR(NL)   
      REAL        DTT    
      REAL        DUL(NL)          
      REAL        ESW(NL)     
      REAL        GRORT       
      INTEGER     ISTAGE      
      CHARACTER   ISWNIT*1    
      INTEGER     L           
      INTEGER     L1          
      REAL        LL(NL)      
      REAL        NH4(NL)     
      INTEGER     NLAYR       
      REAL        NO3(NL)     
      REAL        PLTPOP 
      REAL        PORMIN     
      REAL        RLDF(NL)    
                              
      REAL        RLNEW      
      REAL        RLV(NL)    
      REAL        RLWR       
      REAL        RNFAC      
                             
      REAL        RNLF       
                             
      REAL        RTDEP 
      REAL        RTEXF     
      REAL        RTSURV
      REAL        SDEPTH     
      REAL        SHF(NL)    
      INTEGER     STGDOY(20) 
      REAL        SW(NL)     
      REAL        SAT(NL)
      REAL        SWDF       
      REAL        SWEXF                       
      REAL        SWFAC      
      REAL        TRLDF      
      INTEGER     YRDOY       

C----------------------------------------------------------------------
C            DYNAMIC = RUNINIT OR SEASINIT
C----------------------------------------------------------------------
      IF(DYNAMIC.EQ.RUNINIT.OR.DYNAMIC.EQ.SEASINIT) THEN
 
          DO L = 1, NL
              RLV(L) = 0.0
              ESW(L) = DUL(L)-LL(L)
              RLDF(L) = 0.0
          END DO
          RTDEP = 0.0
          RNLF  = 0.0
          RNFAC = 0.0
          RLNEW = 0.0

C----------------------------------------------------------------------
C             DYNAMIC = INTEGR
C----------------------------------------------------------------------

      ELSEIF(DYNAMIC.EQ.INTEGR) THEN
          IF(ISTAGE.EQ.7.OR.ISTAGE.EQ.8) RTDEP  = SDEPTH             
          IF(ISTAGE.EQ.9) RTDEP  = RTDEP + 0.15*DTT
          

          IF(YRDOY.EQ.STGDOY(9)) THEN
              CUMDEP = 0.0
              DO L = 1, NLAYR
                  CUMDEP = CUMDEP + DLAYR(L)
                  RLV(L) = 0.20*PLTPOP/DLAYR (L)
                  IF (CUMDEP .GT. RTDEP) GO TO 100            ! Was EXIT
              END DO

  100         CONTINUE                                       ! Sun Fix
              RLV(L) = RLV(L)*(1.0-(CUMDEP-RTDEP)/DLAYR(L))
              L1 = L + 1
              IF (L1 .LT. NLAYR) THEN
                  DO L = L1, NLAYR
                      RLV(L) = 0.0
                  END DO
              ENDIF
          ENDIF


      !----------------------------------------------------------------
      !                   Grow Roots
      !----------------------------------------------------------------

          IF (GRORT.LE.0.0001) GOTO 999

!The small differences between root length/weight ratios used in earlier
!models were insignificant considering the uncertainty of the value
!and the uncertainty of loss of assimilate by exudation and respiration.
!A compromise value of 0.98 was choosen for all crops.

          RLNEW  = GRORT * RLWR * PLTPOP   ! A compromise value -- JTR
          CUMDEP = 0.0
          RNFAC  = 1.0
          L      = 0

          DO WHILE ((CUMDEP .LT. RTDEP) .AND. (L .LT. NLAYR))
              L = L + 1
              CUMDEP = CUMDEP + DLAYR(L)
              IF (SW(L)-LL(L) .LT. 0.25*ESW(L)) THEN
                  SWDF = 4.0*(SW(L)-LL(L))/ESW(L)
                  IF (SWDF .LT. 0.0) THEN
                      SWDF = 0.0
                  ENDIF
              ELSE
                  SWDF = 1.0
              ENDIF

      !Made all crops so that RNFAC is constrained between 0.01 and 1.0;
      !on page 94 of Jones & Kiniry book the minimum is 0.01. - WTB

              IF (ISWNIT .NE. 'N') THEN
                  RNFAC = 1.0 - (1.17*EXP(-0.15*(NO3(L)+NH4(L))))
                  RNFAC = AMAX1 (RNFAC,0.01)
              ENDIF

              RLDF(L) = AMIN1(SWDF,RNFAC)*SHF(L)*DLAYR (L)
          END DO

          L1 = L

!The following changes were made to simplify the code and make the model
!more generic. It also takes into account some newer data provided by
!Julio Dardenelli of Argentina.  For the first time the ceres model
!restricts the rate of downward movement of roots with the soil property
!-- root weighting factor -- to account for greater difficulty in growing
!downward in hard soil. Changes made by JTR 6/16/94.


c** wdb 10/22/03  
          RTEXF = 0.1
          SWEXF = 1.0
          IF (SAT(L)-SW(L) .LT. PORMIN) THEN
            SWEXF = (SAT(L) - SW(L)) / PORMIN
            SWEXF = MIN(SWEXF, 1.0)
          ENDIF

          RTSURV = MIN(1.0,(1.-RTEXF*(1.-SWEXF)))

c** wdb 10/22/03
C          IF (CUMDTT .LT. 275.0) THEN             ! JTR 6/17/94
C              RTDEP = RTDEP + DTT*0.2*SQRT(SHF(L)*AMIN1(SWFAC*2.0,SWDF))
C          ELSE
C              RTDEP = RTDEP + DTT*0.2*SQRT(SHF(L)*AMIN1(SWFAC*2.0,SWDF))
C          ENDIF
          RTDEP = RTDEP + DTT*0.2*AMIN1(SWFAC*2.0,SWDF)

          RTDEP    = AMIN1 (RTDEP,DEPMAX)                            
          RLDF(L1) = RLDF(L1)*(1.0-(CUMDEP-RTDEP)/DLAYR(L1))
          TRLDF    = 0.0

          DO  L = 1, L1
              TRLDF = TRLDF + RLDF(L)
          END DO

          IF (TRLDF .GE. RLNEW*0.00001) THEN
              RNLF = RLNEW/TRLDF
              DO L = 1, L1
                IF (ISTAGE .LT. 4) THEN
                  rLV(L) = RLV(L) + RLDF(L)*RNLF/DLAYR(L)
                ELSE
                  RLV(L) = RLV(L) + RLDF(L)*RNLF/DLAYR(L)-0.005*RLV(L)
                ENDIF
                  !Round off to nearest 1/1000th place
                  RLV(L) = RLV(L) * RTSURV
                  RLV(L) = REAL(INT(RLV(L)*1000.))/1000.  
                  RLV(L) = AMAX1 (RLV(L),0.0)
                  RLV(L) = AMIN1 (RLV(L),10.0)
              END DO
          ENDIF


C----------------------------------------------------------------------
C                     DYNAMIC = OUTPUT
C----------------------------------------------------------------------
      ELSEIF(DYNAMIC.EQ.OUTPUT) THEN

      ! {no procedures to date}

      ENDIF !Dynamic loop

999   CONTINUE

      RETURN
      END SUBROUTINE SU_ROOTGR


! CUMDEP    !Cumulative depth of soil, cm
! CUMDTT    !Cumulative daily thermal time after germination, C
! DEPMAX    !Depth of soil, cm
! DLAYR(L)  Soil thickness in layer L (cm)
! DTT       !Growing degrees occurring today (Base 8C), C      
! ESW(20)   !Extractable water in soil layer L, cm
! GRORT     !Root growth rate, g/plant/day
! ISTAGE    !Crop growth stage (1-9)
!RISWNIT*1  !Switch indicating if soil nitrogen balance is on (Y/N)
! L         !Loop counter
! L1        !Loop counter
! LL(20)    !Volumetric lower limit of soil water in soil layer L, cm3/cm3
! NH4(20)   !Ammonium in soil layer L, ppm
! NLAYR     !Number of soil layers
! NO3(20)   !Nitrate in soil layer L, ppm
! PLTPOP    !Plant population, pl/m2
! RLDF(20)  !A root length density factor for soil layer L used to calculate
!           !new root growth distribution - unitless
! RLNEW     !New root length to be added to the total root system length, cm. root cm2 ground
! RLV(20)   !Root length density, cm root/cm3 soil
! RLWR      !Root length to weight ratio, cm/g
! RNFAC     !Zero to unity factor describing mineral N availability effect on
!           !root growth in Layer L
! RNLF      !Intermediate factor used to calculate distribution of new root
!           !growth in the soil - unitless value between 0 and 1
! RTDEP     !Rooting depth (cm), Initially set at emergence
! SDEPTH    !Sowing depth, cm
! SHF(20)   !Relative root distribution in soil layer L (0-1)
! STGDOY(20)!Year and day of year that a growth stage occurred on
! SW(20)    !Volumetric soil water content of soil layer L, cm3/cm3
! SWDF      !Soil water deficit factor for Layer L used to calculate root
!           !growth and water uptake - unitless value between 0 and 1
! SWFAC     !Soil water stress effect on growth (0-1), 1 is no stress, 0 is full stress
! TRLDF     !An intermediate calculation used to calculate distribution of new root growth in soil
! YRDOY     !Year and day of year  



