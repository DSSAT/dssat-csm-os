c     ****************************************************************
c     This subroutine calculates
c     ::::::::::::::::::::::::::::::::::::::
c     1. Root depth
c     2. Root growth (length density) - RLV
c     ::::::::::::::::::::::::::::::::::::::
c     Matthew Jones, October 2006
c     Gainesville, Florida
c     ::::::::::::::::::::::::::::::::::::::
c     This was taken from the CANEGRO 
c     WATBAL routine, and modified to
c     work with the DSSAT CANEGRO Plant
c     Module structure / variables

!     2023-01-26 chp removed unused variables from argument list: HU16
c     ****************************************************************

      SUBROUTINE ROOTGROWTH(
c      [I] DSSAT Control variable
     - Control,
c      [I] DSSAT Switch variable
     - ISWITCH,
c      [IO] CANEGRO soil properties 
     - Soil, 
c      [IO] CANEGRO water properties
     - WaterBal, 
c      [I] Growth properties
     - Growth,
c      [I] Climate & meteorology variables
     - Climate,
c      [I] Cane crop variables
     - CaneCrop,
c      [O] Max root water movement per cm of root
     - RWUMX
c      [I] Change in heat units, base 16 degrees
!    & HU16 
     & )
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     Use modules from DSSAT:
          USE ModuleDefs
c     Use CPM modules:
          USE CNG_ModuleDefs

      IMPLICIT NONE
      EXTERNAL GET_SPECIES_COEFF, GET_CULTIVAR_COEFF, GETLUN, INFO, 
     &  FIND_INP, D_TT
      SAVE
c     ================================================================
c                            VARIABLES
c     ================================================================
c     Define variables:
c     :::::::::::::::::
c     Parameters:
c     :::::::::::
      TYPE(CONTROLTYPE)  Control
      TYPE(SWITCHTYPE)   ISWITCH
      TYPE(CNG_SoilType) Soil
      TYPE(WaterType)    WaterBal
      TYPE(GrothType)    Growth
      TYPE(ClimateType)  Climate
      TYPE(CaneCropType) CaneCrop
      TYPE(RatoonCarryOverType) RatCarryOver
      REAL               RWUMX

c     Daily change in heat units base 16 degrees
!     REAL, INTENT(IN) :: HU16

      ! HBD (Jan 2023) after MvdL 2011
!      REAL HARVRES
!      REAL ROOTNCONC

c     Local variables (not part of any common blocks)
c     :::::::::::::::::::::::::::::::::::::::::::::::
          REAL     AERFAC
          INTEGER  L
          INTEGER  L1
          REAL     PO
          REAL     RLDF(NL)
          REAL     RLNEW
          REAL     RLVMIN
          REAL     RNFAC
          REAL     RNLF
          REAL     SWDF
          REAL     SWDF3
          REAL     TRLDF

c         Added MJ 2006/10/11
c         Is water balance modelled?
          LOGICAL ISWATBAL

c         Added MJ 2006/10/12
c         Parameters for estimate root distribution
          REAL DL2, DL1

c     Variables in SOILI common block that are used
c     (These are local copies)
c     :::::::::::::::::::::::::
          REAL    BD(NL)
          REAL    DEPMAX
          REAL    DLAYR(NL)
          REAL    DUL(NL)
          REAL    LL(NL)
          INTEGER NLAYR
          REAL    SW(NL)
          REAL    WR(NL)

c     Variables in WATER common block that are used:
c     (These are local copies):
c     :::::::::::::::::::::::::
          REAL    CUMDEP
          REAL    ESW(NL)   
          REAL    RLV(NL)   
          REAL    RTDEP 
          REAL    SWDF1 
          REAL    SWDF2 
          REAL    SWDF9 

c     Variables in the GROTH common block, that
c     are used:
c     :::::::::
c         Length per g DM
          REAL RTCMPG
c         [I] Mass of photosynthate partitioned to roots today
          REAL GRORT

c     Vars used in the CLIMT common block, used here:
          REAL DTT

c       Modification to calculation of delta thermal time, based on ASA 2013 work:
c       ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c       Delta thermal time (°Cd) for stalk elongation
        REAL DTT_RER
c       Function for calculating delta thermal time
        REAL D_TT
c       Base, optimal and final temperatures for thermal time accum. (°C)
c       These are cultivar params.
c       For roots
        REAL TBaseREX, ToptREX, TFinREX
c       Mean daily temperature
        REAL TMEAN
c       Reference root elongation rate per unit thermal time (cm/°Cd)
        REAL RER0


c     Cumulative soil depth
c      REAL CUMDEP

c     Counter:
          INTEGER I

c     Root distribution, for output:
          CHARACTER*78 OUT_WR(NL)


c     Temporary stress array:
          REAL ROOTSWDF(NL)

      LOGICAL CF_ERR, SPC_ERR

!     Unit number for output
!      INTEGER OU   !CHP
      !INTEGER SCLUN

      DATA RatCarryOver%RTDEP /0./


c     ================================================================
c                              CODE
c     ================================================================





c     ================================================================
c     Initialise:
c     ===========
      IF (CONTROL%DYNAMIC .EQ. RUNINIT) THEN
         WaterBal%ICSDUR  =  0
         WaterBal%IDRSW  =  0
         WaterBal%ANAER  =  0.0
         WaterBal%ANAERF  =  0.0
         WaterBal%CANWAT  =  0.0
         WaterBal%CEP  =  0.0
         WaterBal%CES  =  0.0
         WaterBal%CET  =  0.0
         WaterBal%CIRR  =  0.0
         WaterBal%CRAIN  =  0.0
         WaterBal%CSD1  =  0.0
         WaterBal%CSD2  =  0.0
         WaterBal%CUMDEP  =  0.0
         WaterBal%DRAIN  =  0.0
         WaterBal%EO  =  0.0
         WaterBal%EOP  =  0.0
         WaterBal%EOS  =  0.0
         WaterBal%EP  =  0.0
         WaterBal%ES  =  0.0
         WaterBal%ESW  =  0.0
         WaterBal%ET  =  0.0
         WaterBal%PESW  =  0.0
         WaterBal%PETFAC  =  0.0
         WaterBal%PRECIP  =  0.0
         WaterBal%RLV  =  0.0
         WaterBal%RTDEP  =  0.0
         WaterBal%RUNOFF  =  0.0
         WaterBal%RWUEP1  =  0.0
         WaterBal%RWUEP2  =  0.0
         WaterBal%RWUMX  =  0.0
         WaterBal%SI1  =  0.0
         WaterBal%SI2  =  0.0
         WaterBal%SUMES1  =  0.0
         WaterBal%SUMES2  =  0.0
         WaterBal%SWDF1  =  0.0
         WaterBal%SWDF2  =  0.0
         WaterBal%SWDF30  =  0.0
         WaterBal%SWDF9  =  0.0
         WaterBal%T  =  0.0
         WaterBal%TLL  =  0.0
         WaterBal%TPESW  =  0.0
         WaterBal%TRWU  =  0.0
         WaterBal%TRWUP  =  0.0
         WaterBal%TSW  =  0.0
         
         L  =  0 
         L1  =  0 
         I  =  0 
         NLAYR  =  0 
         DO I = 1, NL   
             RLDF(I)  =  0.0
             BD(I)  =  0.0
             DLAYR(I)  =  0.0
             DUL(I)  =  0.0
             ESW(I)     =  0.0
             LL(I)  =  0.0
             RLV(I)     =  0.0
             SW(I)  =  0.0
             WR(I)  =  0.0
             ROOTSWDF(I)  =  0.0
         ENDDO

           AERFAC  =  0.0
           PO  =  0.0
           RLNEW  =  0.0
           RLVMIN  =  0.0
           RNFAC  =  0.0
           RNLF  =  0.0
           SWDF  =  0.0
           SWDF3  =  0.0
           TRLDF  =  0.0
           CUMDEP  =  0.0
           DEPMAX  =  0.0
           RTDEP   =  0.0
           SWDF1   =  0.0
           SWDF2   =  0.0
           SWDF9   =  0.0
           DL2    =  0.0
           DL1  =  0.0
           DTT  =  0.0
           GRORT  =  0.0
           RTCMPG  =  0.0


!      CALL GETLUN('ROOTOUT', OU)
!      OPEN(UNIT=OU, FILE="ROOTOUT.OUT")
!
!      WRITE(OU, '(9(A8, 1X))') 
!     &  'TRNO', 'RUNNO', 'YRDOY',
!     &  'DTT_RER', 'SWDF1', 'RER0', 'SWDF', 'ActSWDF', 'RTDEP'
         
       ENDIF
       
c     Copy values to local variables from composite types:
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::

c     Soil variables:
c     :::::::::::::::
          BD     = Soil%BD
          DEPMAX = Soil%DEPMAX
          DLAYR  = Soil%DLAYR
          DUL    = Soil%DUL
          LL     = Soil%LL
          NLAYR  = Soil%NLAYR
          SW     = Soil%SW
          WR     = Soil%WR

c     Water variables:
c     ::::::::::::::::
          CUMDEP = WaterBal%CUMDEP
          ESW    = WaterBal%ESW
          RLV    = WaterBal%RLV
          RTDEP  = WaterBal%RTDEP
          SWDF1  = WaterBal%SWDF1
          SWDF2  = WaterBal%SWDF2
          SWDF9  = WaterBal%SWDF9

c     Growth variables:
c     :::::::::::::::::
          RTCMPG = Growth%RTCMPG
          GRORT  = Growth%GRORT

c     Climate variables:
c     ::::::::::::::::::
          DTT    = Climate%DTT       




      
      IF (CONTROL%DYNAMIC .EQ. SEASINIT) THEN
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          L      = 0
          L1     = 0
          PO     = 0.
          RLDF   = 0.
          RLNEW  = 0.
          RNFAC  = 0.
          RNLF   = 0.
          SWDF   = 1.
          SWDF3  = 1.
          TRLDF  = 0.
          RTDEP  = 0.

c     Init the 'CANEGRO-global' Root length density variable
      WaterBal%RLV = 0.
          

c     Calculate depmax (is just cumulative depth of soil layers)
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      DEPMAX = 0.
!     MvdL, Feb 2010: replaced NL with NLAYR in following line      
      DO L=1,NLAYR
          DEPMAX=DEPMAX+DLAYR(L)
      ENDDO

c     Init RLV:
      RLV = 0.

c     To be read from species file:
c     ::::::::::::::::::::::::::::::::::::::::
          Growth%RTCMPG = 500.
          CALL GET_SPECIES_COEFF(Growth%RTCMPG, 'RTCMPG', Control, 
     &                           SPC_ERR)

c         Minimum Root length density
          RLVMIN = 0.02
          CALL GET_SPECIES_COEFF(RLVMIN, 'RLVMIN', Control, 
     &                           SPC_ERR)

c         Thermal time parameters for leaf elongation:
c         :::::::::::::::::::::::::::::::::::::::::::::
c         Defaults:
            TBaseREX = 10.0
            TOptREX  = 30.0 
            TFinREX  = 43.0
          CALL GET_CULTIVAR_COEFF(TBaseREX, 'TBASE_REX',  
     &       Control, CF_ERR)
          CALL GET_CULTIVAR_COEFF(TOptREX, 'TOPT_REX',  
     &       Control, CF_ERR)
          CALL GET_CULTIVAR_COEFF(TFinREX, 'TFin_REX',  
     &       Control, CF_ERR)       


c     Reference root elongation rate:
      RER0 = 0.22

c     Switches:
c     :::::::::
          IF (INDEX('YDA',ISWITCH%IDETL) > 0) THEN
!            CALL GETLUN('WORK.OUT',SCLUN)
            IF (ISWITCH%ISWWAT .EQ. 'Y') THEN
                ISWATBAL = .TRUE.
!                WRITE(SCLUN, '(A)') 'Water balance IS modeled.'
            ELSE
                ISWATBAL = .FALSE.
!                WRITE(SCLUN, '(A)') 'Water balance NOT modeled.'
                SWDF1 = 1.
            ENDIF
          ENDIF

c     If rooting weight was not set:
c     ::::::::::::::::::::::::::::::
c     Taken from CANEGRO Initial.for, lines +- 119-130
      DL2 = 0.
      DL1 = 0.
      DO L=1,Soil%NLAYR
          ESW(L) = DUL(L) - LL(L)
          DL2 = DL1 + DLAYR(L)
          IF (WR(L).LE.0.0) THEN
              WR(L)=EXP(-4.0*(DL1+DL2)/400.0)
              WR(1)=1.0
          ENDIF
          DL1=DL2
c         Write root weight to array of strings
          WRITE(OUT_WR(L), '(F10.5)') WR(L)
      ENDDO

c     Output root weight:
          CALL INFO(Soil%NLAYR, 'SC_ROOTGROWTH: WR', OUT_WR) 


c     This is taken from the CANEGRO model, more or less (initial.for)
c     'MAX' is used to reduce the root growth to 0.02 at the beginning 
c     of each season.  In CANEGRO, the roots are initialised with 0.02
c     with no 'MAX' term.
c     If this is a plant crop:
c     ::::::::::::::::::::::::
      IF (CaneCrop%RATOON .LT. 1) THEN
          DO I=1, Soil%NLAYR
              RLV(I) = MAX(RLV(I), RLVMIN)
              CALL FIND_INP(RTDEP, 'PLANT_DEPTH', Control)
              IF (RTDEP .LT. 5.1) THEN
                RTDEP = 5.1
              ENDIF
          ENDDO
      ELSE
c         It is a ratoon; init roots accordingly:
          CUMDEP = 0.
          DO I=1, Soil%NLAYR
              CUMDEP = CUMDEP + Soil%DLAYR(I)
c             Original line from Canegro: 
c                   RLV(i)  = AMAX1((0.2-0.005*CUMDEP),0.0)
c             Modified as follows: min RLV = 0.02; after all, plant
c             crop is initialised with higher RLV in layers lower  
c             than 40 cm otherwise.
              RLV(I) = AMAX1((0.2-0.005*CUMDEP),RLVMIN)
          ENDDO
!           MJ, Jan 2014: fudge to ensure that water stress is not super-sensitive to
!           a dry top layer:
          RLV(1) = 0.05
          RLV(2) = 0.25
          RTDEP = DEPMAX

c         If ratoon has been carried over:
          IF (CaneCrop%CARRY_OVER) THEN
              RTDEP = RatCarryOver%RTDEP
          ENDIF
      ENDIF

c         Maximum root water uptake (to be set in species / cultivar file):
c         Taken from Canegro PRN file.
c         ::::::::::::::::::::::::::::

c             Set default
              RWUMX = 0.07

c             Read from file:
              CALL GET_SPECIES_COEFF(RWUMX, 'RWUMX',
     -                       CONTROL,  SPC_ERR)
c         ::::::::::::::::::::::::::::::::::::::::



      ELSEIF (CONTROL%DYNAMIC .EQ. RATE) THEN
C---------------------------------------------------------------------
C
C       CALL to ROOT GROWTH AND DEPTH ROUTINE
C
C---------------------------------------------------------------------
c      WRITE(SCLUN, '(A, F10.3)') 'GRORT is ', GRORT


c     Mean daily temperature
      TMEAN = (Climate%TEMPMX + Climate%TEMPMN) / 2.0

c     Calculate non-linear thermal time
      DTT_RER = D_TT(TMEAN, TBaseREX, TOptREX, TFinREX) 






C     I have converted GRORT from g/plant to g/m^2 per day. GIB 12/4/89  
      IF (GRORT.GT.0.) THEN
C     GRORT is mass of dm allocated to roots in one day (g/m2), convert this to g 
C     /cm2 and then to cm/cm2. The following routine then allocates this new root 
C     length per cm soil depth for each layer. 
c     RLNEW=GRORT*0.80  from CERES Maize.
c     Root cm per g DM RTCMPG is read in as a variable
      RLNEW=GRORT*0.0001*RTCMPG
      TRLDF=0.
      CUMDEP=0.
      SWDF3=0.0
      RNFAC=1.0

      DO 3000 L=1,NLAYR
           L1=L
           CUMDEP=CUMDEP+DLAYR(L)
           SWDF=1.
c          If water balance is modeled, calc SWDF:
           IF (ISWATBAL) THEN
              IF (SW(L)-LL(L).LT.0.25*ESW(L)) 
     &          SWDF=4.*(SW(L)-LL(L))/ESW(L)
           ENDIF
           IF (SWDF.LT.0.) SWDF=0.
           ROOTSWDF(L) = SWDF
C Roots do not grow in anaerobic soil- but will allow for present because 
c small differences between sat(l) and po give numerical problems. GIB 19/1/95
           PO=1.0-BD(L)/2.65
           AERFAC=1.0  




c           IF(SW(L).GE.(PO-0.05)) AERFAC=0.0

C GIB 13/9/90 N BALANCE NOT CONSIDERED AT THIS STAGE
C           IF (ISWNIT.NE.0) THEN
C              RNFAC=1.0-(1.17*EXP(-0.15*(NO3(L)+NH4(L))))
C              IF(RNFAC.LT.0.1)RNFAC=0.1
C           ENDIF



           RLDF(L)=AMIN1(SWDF,RNFAC,AERFAC)*WR(L)*dlayr(L)
           IF (CUMDEP.LT.RTDEP) GO TO 2900
!           RTDEP=RTDEP+DTT*0.22*AMIN1((SWDF1*2.0),SWDF)
!          MJ: MvdL noticed that 'DTT'is cumulative. UseHU16 instead:
c          ...and revised for ASA2013
c          MJ, Jan 2014: set minimum root depth to 5.1 cm
           RTDEP=MAX(5.1, RTDEP+DTT_RER*RER0*AMIN1((SWDF1*2.0),SWDF))
           
c          :::::::::::::::::::::::::::::::::::::::::::
!      WRITE(OU, '(I8, 1X, 2(I8, 1X), 6(F8.3,1X))') 
!     &  Control%TRTNUM, Control%RUN, Control%YRDOY,
!     &  DTT_RER, SWDF1, RER0, SWDF, AMIN1((SWDF1*2.0),SWDF), RTDEP
c          :::::::::::::::::::::::::::::::::::::::::::
           IF (RTDEP.GT.DEPMAX) RTDEP=DEPMAX
           RLDF(L)=RLDF(L)*(1.-(CUMDEP-RTDEP)/DLAYR(L))
           TRLDF=TRLDF+RLDF(L)
           GO TO 3100
 2900     CONTINUE 



c         Only calc stress factor if water balance modeled          
          IF (ISWATBAL) THEN
              SWDF3=SWDF3+(SW(L)-LL(L))/(DUL(L)-LL(L))*DLAYR(L)
          ELSE
              SWDF3 = SWDF3 + DLAYR(L)
          ENDIF

 3000 TRLDF=TRLDF+RLDF(L)

 3100 SWDF3=SWDF3/CUMDEP


      IF (TRLDF .GE. RLNEW*0.00001) THEN
          RNLF=RLNEW/TRLDF
          DO 3110 L=1,L1
             RLV(L)=RLV(L)+RLDF(L)*RNLF/DLAYR(L)-0.005*RLV(L)
             IF (RLV(L).LT.0) RLV(L)=0.
             IF (RLV(L).GT.5.0) RLV(L)=5.0
c          SNH4(L)=SNH4(L)+RNLOSS(L)*PLANTS*10.0
 3110     CONTINUE
      ENDIF

 3150 CONTINUE

c      WRITE(*, '(I7, 1X, 7(f10.4, 1X))') CONTROL%YRDOY, ROOTSWDF(1:7)

c     END of GRORT > 0.
c     :::::::::::::::::
      ENDIF
c     :::::::::::::::::

c      WRITE(SCLUN, '(A, F10.5)') 'SWDF1 is ', SWDF1

c Added by GAK and JWJ , August 1992.
c     ... and removed by MJ, October 2006
c      swdf1=1.0
c      swdf2=1.0
	swdf9=1.0

c     End of RATE
c     :::::::::::
      ELSEIF (CONTROL%DYNAMIC .EQ. SEASEND) THEN
c         Possibly carry over information:
c         Although probably correct, the intention is not to
c         change the model as such.  This constitutes a model 
c         change.  Bugs can be fixed though.
c          IF (CaneCrop%CARRY_OVER) THEN
              RatCarryOver%RTDEP = WaterBal%RTDEP  
c          ENDIF
c          RatCarryOver%RTDEP = 0.  
c         
      ENDIF

c     =============================================================
c     Copy values from local variables to composite types:
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::

c     Soil variables:
c     :::::::::::::::
          Soil%BD     = BD          
          Soil%DEPMAX = DEPMAX      
          Soil%DLAYR  = DLAYR       
          Soil%DUL    = DUL         
          Soil%LL     = LL          
          Soil%NLAYR  = NLAYR       
          Soil%SW     = SW          
          Soil%WR     = WR          
                                    
c     Water variables:              
c     ::::::::::::::::              
          WaterBal%CUMDEP = CUMDEP  
          WaterBal%ESW    = ESW     
          WaterBal%RLV    = RLV     
          WaterBal%RTDEP  = RTDEP   
c          WaterBal%SWDF1  = SWDF1   
          WaterBal%SWDF2  = SWDF2   
          WaterBal%SWDF9  = SWDF9   



c     :::::::::::
c     =============================================================
c     End of subroutine
c     :::::::::::::::::
      END
c     =============================================================