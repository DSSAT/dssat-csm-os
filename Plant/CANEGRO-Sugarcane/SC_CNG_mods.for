      MODULE CNG_ModuleDefs
c     Contains defintion of derived data types and constants which are 
c     to be used in the intermediate canegro model (and then used in the 
c     DSSAT canegro model

      USE MODULEDEFS  !CHP -- for definition of NL
      SAVE

c     ===============================================================
c     GLOBAL VARIABLES

c     The name of the (current) cultivar
      CHARACTER*20 CULTIVAR, SCSTGNAM(20), GROWTHPHASES(5)

      INTEGER  CANESIM, CANEGRO, MAX_TILLER_COHORTS, MAXCOHORTS
c     Set parameter values:
      PARAMETER (CANEGRO = 1, CANESIM = 2)

      PARAMETER (MAX_TILLER_COHORTS = 100)

      PARAMETER (MAXCOHORTS = 50)

c     Phenological stages:
c      INTEGER SEEDCANE, GERMINATED, EMERGED

c     Sugarcane phenological stage names/numbers
c     Inspired by the Maize stage list:
      DATA SCSTGNAM /
     &  '          ',   !1
     &  'Peak pop. ',   !2
     &  'Phylswitch',   !3
     &  'Stalk emrg',   !4
     &  '          ',   !5
     &  'Flowr init',   !6
     &  'Flowr emrg',   !7
     &  'Plant/ratn',   !8
     &  'Germinate ',   !9
     &  'Emergence ',   !10
     &  'Peak popn ',   !11
     &  'Maturity  ',   !12
     &  '          ',   !13
     &  'Start Sim ',   !14
     &  'End Sim   ',   !15
     &  'Harvest   ',   !16
     &  '          ',   !17
     &  '          ',   !18
     &  '          ',   !19
     &  'Harvest   '/   !20

c     Sugarcane growth phases:
      DATA GROWTHPHASES /
c       1: emergence
     &  'EMERGENCE ',
c       2: tillering
     &  'TILLERING ',
c       3: tillering following stalk emergence
     &  'TILL+STALK',
c       4: Stalk elongation and tiller senescence
     &  'Stlk.Senes',
     &  'FLWR INIT '/


c=======================================================================
c     For now, these modules will simply replace the common blocks,
c     so will contain the same variables with the same names.

c     The climate type:
c     :::::::::::::::::
          TYPE ClimateType
              REAL APAN
              REAL DTT
              REAL PAR
              REAL RAIN
              REAL RHAM
              REAL RHPM
              REAL SOLRAD
              REAL SUNHRS
              REAL SUNMAX
              REAL TDRY2
              REAL TDRY8
              REAL TEMPMN
              REAL TEMPMX
              REAL TWET2
              REAL TWET8
              REAL WINDSP
          END TYPE ClimateType

c     The water balance type (this might be problematic in DSSAT)
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          TYPE WaterType
              REAL ANAER
              REAL ANAERF
              REAL CANWAT
              REAL CEP
              REAL CES
              REAL CET
              REAL CIRR
              REAL CRAIN
              REAL CSD1
              REAL CSD2
              REAL CUMDEP
              REAL CWSI
              REAL DRAIN
              REAL EO
c             Added by MJ, 20070323
              REAL EOS
c             Added by MJ, 20070323
              REAL EOP
              REAL EP
              REAL ES
              REAL ESW(NL)
              REAL ET
              INTEGER ICSDUR
              INTEGER IDRSW
              REAL PESW
              REAL PETFAC
              REAL PRECIP
              REAL RLV(NL)
              REAL RTDEP
              REAL RUNOFF
              REAL RWUEP1, RWUEP2
              REAL RWUMX
              REAL SI1(5)
              REAL SI2(5)
              REAL SUMES1
              REAL SUMES2
              REAL SWDF1
              REAL SWDF2
              REAL SWDF9
              REAL SWDF30
              REAL T
              REAL TLL
              REAL TPESW
              REAL TRWU
c             Potential root water uptake:
              REAL TRWUP
              REAL TSW
          END TYPE WaterType

c     The canopy/crop type:
c     :::::::::::::::::::::
          TYPE CaneCropType
              REAL    AREAMX(70)
              REAL    CANHEIGHT
c             Should the ratoon be carried over?
              LOGICAL CARRY_OVER
              REAL CWSI
c              REAL    DEDLFN(30)
              REAL    DEDLFN(MAX_TILLER_COHORTS)
              REAL    DEDSTK
              REAL    EXTCFN
              REAL    EXTCFST
              REAL    GLFN
c             Growth phase
              INTEGER GROPHASE

c              REAL    LEN(30,70)
              REAL    LEN(MAX_TILLER_COHORTS,70)
              INTEGER LFMAX
c              INTEGER LFN(30)
              INTEGER LFN(MAX_TILLER_COHORTS)
              INTEGER LFNMXEXT
              REAL    LMAX(70)
              INTEGER LOSING
              INTEGER LT
              INTEGER MXLFAREA
              INTEGER MXLFARNO
              INTEGER NTLGRP
c             Different phyllochron intervals
              REAL    PHYLO(2)
c             Number of leaves before Phyllo interval changes
              REAL    PHYLSWTCH
c             Empirical population = f(TT) coefficients
              REAL    popcf(10)
              REAL    poplfn
c              REAL    popmax(30)
              REAL    popmax(MAX_TILLER_COHORTS)
              REAL    POPN
c             Ratoon: 0 = plant crop, ratoon 1, 2, 3, etc
              INTEGER RATOON
c             Stalk height
              REAL    SHGT
c              REAL    TEMPOP(30)
              REAL    TEMPOP(MAX_TILLER_COHORTS)
              REAL    TMEANDEDLF
              REAL    TMEANLF
              REAL    TOTPOP
              REAL    WIDCOR
              REAL    WIDTH(70)
              REAL    WMAX(70)
          END TYPE CaneCropType

          TYPE CNG_SoilType
              REAL    BD(NL)
              REAL    CN2(3)
              REAL    DEPMAX
              REAL    DLAYR(NL)
              REAL    DUL(NL)
              REAL    LL(NL)
              INTEGER NLAYR
              INTEGER NUMSL
              REAL    RWU(NL)
              REAL    SALB
              REAL    SAT(NL)
              REAL    SMX
              REAL    SW(NL)
              REAL    SWCN(NL)
              REAL    SWCN2(NL)
              REAL    SWCON
              REAL    SWCON1
              REAL    SWCON3
              REAL    SWCONL(NL)
              REAL    SWCONX
              REAL    SWEF
              REAL    TDUL
              REAL    U
              REAL    WF(NL)
              REAL    WR(NL)
          END TYPE CNG_SoilType

          TYPE GrothType
              REAL    GRORT
              REAL    LAI
              REAL    TLAI
              REAL    LI
              REAL    GERMDY
              REAL    RTCMPG
          END TYPE GrothType

c         This is the partitioning / photosynthesis
c         composite type.
c         :::::::::::::::::::::::::::::::::::::::::
          TYPE PartType
              REAL     AERDWDT
              REAL     AERLDM
              REAL     AMPLIF
              REAL     APFMX
              REAL     BRXPDM
              REAL     CHUPIBASE
              REAL     DELTAOUT
              REAL     DELTTMAX
              REAL     DMMAS(150)
              REAL     FDWDT
              REAL     FLODGE
              REAL     FTCON
              REAL     NSUCMAS
              REAL     PARCE
              REAL     PCB
!              REAL     PERLST(10)
              REAL     PURITY
              REAL     RESPCF
              REAL     RFA
              REAL     RFB
              REAL     RFC
c             Daily growth respiration rate (t/ha/d)
              REAL RESP_G
c             Daily maintenance respiration rate (t/ha/d)
              REAL RESP_M
              REAL     ROOTF
              REAL     STKDM
              REAL     STKPFMAX
              REAL     STKWM
              REAL     SUCA
              REAL     SUCB
              REAL     SUCMAS
              REAL     SUCNEWMAS
              REAL     SURCON
              REAL     SWDF2AMP
              REAL     TBASEFNS
              REAL     TBASETOT
              REAL     TBDELTT
              REAL     TBFT
              REAL     TDMAS
              REAL     TOPDM
              REAL     TOPRATE
              REAL     TOPTFNS
              REAL     TOPTTOT
              REAL     TOTBASE
          END TYPE PartType

          TYPE OutType
              INTEGER AGE
              REAL    AVTEMP
              REAL    BOOTLI
              REAL    CANHGT
              REAL    CDRAIN
              REAL    CHUPI
c             Cumulative thermal time for stalk population
              REAL    CHUPOP
c             Cumulative thermal time for emergence
              REAL    CHU_EM
              REAL    CLAI
              REAL    CRUNOF
              REAL    CUMLI
              REAL    CUMRAD
              REAL    CUMTMP
              REAL    CUMVPD
              REAL    DWDT
              REAL    EGRASS
              REAL    GLFN
              REAL    GTIRRI
              REAL    GROSSP
              REAL    HUPI
              REAL    HUPOP
              INTEGER IDOY
              INTEGER IDY
              INTEGER IYR
              INTEGER JDAY
              INTEGER MON
              INTEGER NSITE
              REAL    PCLI
              REAL    PER
              REAL    PAR
              REAL    ROOTDM
              REAL    SHGT
              REAL    SOD
              REAL    SRAD
              REAL    STDAYER
              REAL    SUCRPDM
              REAL    TLARV1
              REAL    TLARV2
              REAL    TMOTH
              REAL    TPUPA
              REAL    TRASDM
              REAL    TTOTEGG
              REAL    VPD
          END TYPE OutType
c         :::::::::::::::::::::::::::::::::::::::::


c         MJ, 2007-05-15:
c         The Ratoon carryover type:
c         ::::::::::::::::::::::::::
c         This composite variable stores information that will be
c         used to initialise a ratoon crop in a crop sequence.
c         Problem:
c         ::::::::
c         In reality, the existing root system dies off slowly 
c         while the new root system, grown by new tillers, replaces
c         it.  Short of modelling two cohorts of roots, I'm not 
c         sure how to handle this (CANEGRO does not handle it).
          TYPE RatoonCarryOverType
c             Root depth
              REAL RTDEP
          END TYPE RatoonCarryOverType


c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     MJ, May 2012
c     Define the tiller cohort type.
c     This composite variable represents a set of shoots.
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TYPE SHOOT_COHORT
c        Date of emergence (TT_BASE_EMERGE)
         REAL EMERGE_TTDATE 
c        Age of this shoot cohort in Degree Days (Base TT16 ~ TT_Emerge)
         REAL AGE_TT
         REAL DTT_POP
c        The number of shoots this cohort represents (shoots/m)
         REAL NUMSHOOTS, D_NUMSHOOTS
c        Change in the number of tillers (if this is a primary cohort)
c        (whole numbers)
         REAL D_N_TILLERS
c        Is this a primary shoot or a tiller cohort?
         LOGICAL ISPRIMARY
c        Unique ID for this shoot cohort (in case it's useful!)
         INTEGER UNIQUE_ID
      END TYPE SHOOT_COHORT
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  


c     ==================================================================
      END MODULE CNG_ModuleDefs


c     ------------------------------------------------------------------
c     SOME related subroutines:
c     -------------------------

c     Copy_xxx_Common
c     ::::::::::
c     This subroutine copies values from the climate common block 
c     to the specified climate module:
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE Copy_CLIMT_Common(Climate, CopyTo)
c
c     Instruct compiler to use CANEGRO module defns:
c     ::::::::::::::::::::::::::::::::::::::::::::::
          USE CNG_ModuleDefs
      IMPLICIT NONE

c     The Climate 'object'
c     ::::::::::::::::::::
      TYPE (ClimateType) Climate

c     If CopyTo is true, it will copy from the common block to
c     the module; if CopyTo is false, it will copy from the 
c     module back to the common block.
      LOGICAL CopyTo

c     The climate common block
c     ::::::::::::::::::::::::
      COMMON /CLIMT/ TEMPMN,TEMPMX,RAIN,SOLRAD,DTT,APAN,SUNMAX
     1 ,WINDSP,RHAM,RHPM,SUNHRS,TDRY8,TDRY2,TWET8,TWET2

              REAL APAN
              REAL DTT
              REAL RAIN
              REAL RHAM
              REAL RHPM
              REAL SOLRAD
              REAL SUNHRS
              REAL SUNMAX
              REAL TDRY2
              REAL TDRY8
              REAL TEMPMN
              REAL TEMPMX
              REAL TWET2
              REAL TWET8
              REAL WINDSP

c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     ~~~~~~~  CODE  ~~~~~~~~~~~
c     ::::::::::::::::::::::::::

c     Copy to
c     :::::::
      IF (CopyTo) THEN
          Climate%APAN = APAN
          Climate%DTT  = DTT
          Climate%RAIN = RAIN
          Climate%RHAM = RHAM
          Climate%RHPM = RHPM
          Climate%SOLRAD = SOLRAD
          Climate%SUNHRS = SUNHRS
          Climate%SUNMAX = SUNMAX
          Climate%TDRY2  = TDRY2
          Climate%TDRY8  = TDRY8
          Climate%TEMPMN = TEMPMN
          Climate%TEMPMX = TEMPMX
          Climate%TWET2  = TWET2
          Climate%TWET8  = TWET8
          Climate%WINDSP = WINDSP
      ELSE
          APAN = Climate%APAN
          DTT  = Climate%DTT
          RAIN = Climate%RAIN
          RHAM = Climate%RHAM
          RHPM = Climate%RHPM
          SOLRAD = Climate%SOLRAD
          SUNHRS = Climate%SUNHRS
          SUNMAX = Climate%SUNMAX
          TDRY2  = Climate%TDRY2
          TDRY8  = Climate%TDRY8
          TEMPMN = Climate%TEMPMN
          TEMPMX = Climate%TEMPMX
          TWET2  = Climate%TWET2
          TWET8  = Climate%TWET8
          WINDSP = Climate%WINDSP
      ENDIF


c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     END of subroutine CopyCommon
      END
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     ============================================================



c     Copy_xxx_Common
c     ::::::::::
c     This subroutine copies values from the climate common block 
c     to the specified climate module:
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE Copy_WATER_Common(WaterBal, CopyTo)

c     Instruct compiler to use CANEGRO module defns:
c     ::::::::::::::::::::::::::::::::::::::::::::::
          USE CNG_ModuleDefs
      IMPLICIT NONE

c     The Climate 'object'
c     ::::::::::::::::::::
      TYPE (WaterType) WaterBal

c     If CopyTo is true, it will copy from the common block to
c     the module; if CopyTo is false, it will copy from the 
c     module back to the common block.
      LOGICAL CopyTo
      INTEGER I

c     Water variables:
c     ::::::::::::::::
      COMMON /WATER/ SUMES1,SUMES2,T,TLL,PESW,TPESW,TSW,CUMDEP,ESW(NL),
     1    CSD1,CSD2,SI1(5),SI2(5),ICSDUR,ES,EP,ET,EO,CES,CEP,CET,
     2    RLV(NL),PRECIP,CRAIN,DRAIN,IDRSW,RTDEP,SWDF1,SWDF2,SWDF9,
     3   TRWU,RWUMX,CIRR,RUNOFF,PETFAC,ANAER,CANWAT,ANAERF 
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
              REAL ANAER
              REAL ANAERF
              REAL CANWAT
              REAL CEP
              REAL CES
              REAL CET
              REAL CIRR
              REAL CRAIN
              REAL CSD1
              REAL CSD2
              REAL CUMDEP
              REAL DRAIN
              REAL EO
              REAL EP
              REAL ES
              REAL ESW
              REAL ET
              INTEGER ICSDUR
              INTEGER IDRSW
              REAL PESW
              REAL PETFAC
              REAL PRECIP
              REAL RLV
              REAL RTDEP
              REAL RUNOFF
              REAL RWUMX
              REAL SI1
              REAL SI2
              REAL SUMES1
              REAL SUMES2
              REAL SWDF1
              REAL SWDF2
              REAL SWDF9
!              REAL SWDF30
              REAL T
              REAL TLL
              REAL TPESW
              REAL TRWU
              REAL TSW

c     ~~~~~~~  CODE  ~~~~~~~~~~~
c     ::::::::::::::::::::::::::

c     Copy to
c     :::::::
      IF (CopyTo) THEN
          WaterBal%ANAER   = ANAER
          WaterBal%ANAERF  = ANAERF
          WaterBal%CANWAT  = CANWAT
          WaterBal%CEP     = CEP
          WaterBal%CES     = CES
          WaterBal%CET     = CET
          WaterBal%CIRR    = CIRR
          WaterBal%CRAIN   = CRAIN
          WaterBal%CSD1    = CSD1
          WaterBal%CSD2    = CSD2
          WaterBal%CUMDEP  = CUMDEP
          WaterBal%DRAIN   = DRAIN
          WaterBal%EO      = EO
          WaterBal%EP      = EP
          WaterBal%ES      = ES
          WaterBal%ET      = ET
          WaterBal%ICSDUR  = ICSDUR
          WaterBal%IDRSW   = IDRSW
          WaterBal%PESW    = PESW
          WaterBal%PETFAC  = PETFAC
          WaterBal%PRECIP  = PRECIP
          WaterBal%RTDEP   = RTDEP
          WaterBal%RUNOFF  = RUNOFF
          WaterBal%RWUMX   = RWUMX
          WaterBal%SUMES1  = SUMES1
          WaterBal%SUMES2  = SUMES2
          WaterBal%SWDF1   = SWDF1
          WaterBal%SWDF2   = SWDF2
          WaterBal%SWDF9   = SWDF9
          WaterBal%T       = T
          WaterBal%TLL     = TLL
          WaterBal%TPESW   = TPESW
          WaterBal%TRWU    = TRWU
          WaterBal%TSW     = TSW
c         Copy array values      
          DO I=1,5 
              WaterBal%SI1(I)  = SI1(I)
              WaterBal%SI2(5)  = SI2(5)
          ENDDO
          DO I=1,NL 
              WaterBal%ESW(I) = ESW(I)
              WaterBal%RLV(I) = RLV(I)
          ENDDO
      ELSE
          ANAER    = WaterBal%ANAER
          ANAERF   = WaterBal%ANAERF
          CANWAT   = WaterBal%CANWAT
          CEP      = WaterBal%CEP
          CES      = WaterBal%CES
          CET      = WaterBal%CET
          CIRR     = WaterBal%CIRR
          CRAIN    = WaterBal%CRAIN
          CSD1     = WaterBal%CSD1
          CSD2     = WaterBal%CSD2
          CUMDEP   = WaterBal%CUMDEP
          DRAIN    = WaterBal%DRAIN
          EO       = WaterBal%EO
          EP       = WaterBal%EP
          ES       = WaterBal%ES
          ET       = WaterBal%ET
          ICSDUR   = WaterBal%ICSDUR
          IDRSW    = WaterBal%IDRSW
          PESW     = WaterBal%PESW
          PETFAC   = WaterBal%PETFAC
          PRECIP   = WaterBal%PRECIP
          RTDEP    = WaterBal%RTDEP
          RUNOFF   = WaterBal%RUNOFF
          RWUMX    = WaterBal%RWUMX
          SUMES1   = WaterBal%SUMES1
          SUMES2   = WaterBal%SUMES2
          SWDF1    = WaterBal%SWDF1
          SWDF2    = WaterBal%SWDF2
          SWDF9    = WaterBal%SWDF9
          T        = WaterBal%T
          TLL      = WaterBal%TLL
          TPESW    = WaterBal%TPESW
          TRWU     = WaterBal%TRWU
          TSW      = WaterBal%TSW
c         Copy array values      
          DO I=1,5 
              SI1(I)   = WaterBal%SI1(I)
              SI2(I)   = WaterBal%SI2(I)
          ENDDO
          DO I=1,NL
              ESW(I)  = WaterBal%ESW(I)
              RLV(I)  = WaterBal%RLV(I)
          ENDDO

      ENDIF


c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     END of subroutine CopyCommon
      END
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     ============================================================

c     Copy_xxx_Common
c     ::::::::::
c     This subroutine copies values from the climate common block 
c     to the specified climate module:
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE Copy_CANCRP_Common(CaneCrop, CopyTo)

c     Instruct compiler to use CANEGRO module defns:
c     ::::::::::::::::::::::::::::::::::::::::::::::
          USE CNG_ModuleDefs
      IMPLICIT NONE

c     The CaneCrop 'object'
c     ::::::::::::::::::::
      TYPE (CaneCropType) CaneCrop

c     If CopyTo is true, it will copy from the common block to
c     the module; if CopyTo is false, it will copy from the 
c     module back to the common block.
      LOGICAL CopyTo
      INTEGER I, J

c     CANCRP variables:
c     ::::::::::::::::
      COMMON /CANCRP/ LEN(30,70),LMAX(70),AREAMX(70),WIDTH(70),LFN(30),
     -  LT,POPN, LFMAX,WIDCOR,TOTPOP,TEMPOP(30),WMAX(70),DEDLFN(30),
     -  DEDSTK,popcf(10),phylo(2),poplfn,ntlgrp,losing,popmax(30),
     -  TMEANLF,TMEANDEDLF,MXLFAREA,MXLFARNO,PHYLSWTCH,LFNMXEXT,EXTCFST,
     -  EXTCFN
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
              REAL    AREAMX
              REAL    DEDLFN
              REAL    DEDSTK
              REAL    EXTCFN
              REAL    EXTCFST
              REAL    LEN
              INTEGER LFMAX
              INTEGER LFN
              INTEGER LFNMXEXT
              REAL    LMAX
              INTEGER LOSING
              INTEGER LT
              INTEGER MXLFAREA
              INTEGER MXLFARNO
              INTEGER NTLGRP
              REAL    PHYLO
              REAL    PHYLSWTCH
              REAL    popcf
              REAL    poplfn
              REAL    popmax
              REAL    POPN
              REAL    TEMPOP
              REAL    TMEANDEDLF
              REAL    TMEANLF
              REAL    TOTPOP
              REAL    WIDCOR
              REAL    WIDTH
              REAL    WMAX


c     ~~~~~~~  CODE  ~~~~~~~~~~~
c     ::::::::::::::::::::::::::

c     Copy to
c     :::::::
      IF (CopyTo) THEN
c         Copy scalar values:
c         :::::::::::::::::::
          CaneCrop%DEDSTK     = DEDSTK
          CaneCrop%EXTCFN     = EXTCFN
          CaneCrop%EXTCFST    = EXTCFST
          CaneCrop%LFMAX      = LFMAX
          CaneCrop%LFNMXEXT   = LFNMXEXT
          CaneCrop%LOSING     = LOSING
          CaneCrop%LT         = LT
          CaneCrop%MXLFAREA   = MXLFAREA
          CaneCrop%MXLFARNO   = MXLFARNO
          CaneCrop%NTLGRP     = NTLGRP
          CaneCrop%PHYLSWTCH  = PHYLSWTCH
          CaneCrop%poplfn     = poplfn
          CaneCrop%POPN       = POPN
          CaneCrop%TMEANDEDLF = TMEANDEDLF
          CaneCrop%TMEANLF    = TMEANLF
          CaneCrop%TOTPOP     = TOTPOP
          CaneCrop%WIDCOR     = WIDCOR

c         Copy array values
c         :::::::::::::::::
          CaneCrop%PHYLO(1) = PHYLO(1)
          CaneCrop%PHYLO(2) = PHYLO(2)
          DO I=1,10
              CaneCrop%POPCF(I) = POPCF(I)
          ENDDO
          DO I=1,MAX_TILLER_COHORTS
              CaneCrop%DEDLFN(I) = DEDLFN(I)
              CaneCrop%LFN(I)    = LFN(I)
              CaneCrop%POPMAX(I) = POPMAX(I)
              CaneCrop%TEMPOP(I) = TEMPOP(I)
          ENDDO
          DO I=1,70
              CaneCrop%AREAMX(I) = AREAMX(I)
              CaneCrop%LMAX(I)   = LMAX(I)
              CaneCrop%WMAX(I)   = WMAX(I)
              CaneCrop%WIDTH(I)  = WIDTH(I)
              DO J=1,MAX_TILLER_COHORTS
                  CaneCrop%LEN(J,I) = LEN(J,I)
              ENDDO
          ENDDO

      ELSE
          DEDSTK     = CaneCrop%DEDSTK
          EXTCFN     = CaneCrop%EXTCFN
          EXTCFST    = CaneCrop%EXTCFST
          LFMAX      = CaneCrop%LFMAX
          LFNMXEXT   = CaneCrop%LFNMXEXT
          LOSING     = CaneCrop%LOSING
          LT         = CaneCrop%LT
          MXLFAREA   = CaneCrop%MXLFAREA
          MXLFARNO   = CaneCrop%MXLFARNO
          NTLGRP     = CaneCrop%NTLGRP
          PHYLSWTCH  = CaneCrop%PHYLSWTCH
          poplfn     = CaneCrop%poplfn
          POPN       = CaneCrop%POPN
          TMEANDEDLF = CaneCrop%TMEANDEDLF
          TMEANLF    = CaneCrop%TMEANLF
          TOTPOP     = CaneCrop%TOTPOP
          WIDCOR     = CaneCrop%WIDCOR

c         Copy array values
c         :::::::::::::::::
          PHYLO(1) = CaneCrop%PHYLO(1)
          PHYLO(2) = CaneCrop%PHYLO(2)
          DO I=1,10
              POPCF(I) = CaneCrop%POPCF(I)
          ENDDO
          DO I=1,MAX_TILLER_COHORTS
              DEDLFN(I) = CaneCrop%DEDLFN(I)
              LFN(I)    = CaneCrop%LFN(I)
              POPMAX(I) = CaneCrop%POPMAX(I)
              TEMPOP(I) = CaneCrop%TEMPOP(I)
          ENDDO
          DO I=1,70
              AREAMX(I) = CaneCrop%AREAMX(I)
              LMAX(I)   = CaneCrop%LMAX(I)
              WMAX(I)   = CaneCrop%WMAX(I)
              WIDTH(I)  = CaneCrop%WIDTH(I)
              DO J=1,MAX_TILLER_COHORTS
                  LEN(J,I) = CaneCrop%LEN(J,I)
              ENDDO
          ENDDO
      ENDIF
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     END of subroutine CopyCommon
      END
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

c     ============================================================
c     Copy_xxx_Common
c     ::::::::::
c     This subroutine copies values from the climate common block 
c     to the specified climate module:
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE Copy_SOILI_Common(Soil, CopyTo)
c
c     Instruct compiler to use CANEGRO module defns:
c     ::::::::::::::::::::::::::::::::::::::::::::::
          USE CNG_ModuleDefs
      IMPLICIT NONE

c     The Climate 'object'
c     ::::::::::::::::::::
      TYPE (CNG_SoilType) Soil

c     If CopyTo is true, it will copy from the common block to
c     the module; if CopyTo is false, it will copy from the 
c     module back to the common block.
      LOGICAL CopyTo
      INTEGER I

c     The Soil common block
c     ::::::::::::::::::::::::
      COMMON /SOILI/SALB,SWCON,CN2(3),DLAYR(NL),DUL(NL),LL(NL),SW(NL),U,
     1 SAT(NL),DEPMAX,TDUL,NLAYR,SMX,WF(NL),WR(NL),RWU(NL),SWEF,NUMSL
     2 ,SWCONX,BD(NL),SWCONL(NL),SWCON1,SWCN2(NL),SWCON3,SWCN(NL) 

              REAL    BD
              REAL    CN2
              REAL    DEPMAX
              REAL    DLAYR
              REAL    DUL
              REAL    LL
              INTEGER NLAYR
              INTEGER NUMSL
              REAL    RWU
              REAL    SALB
              REAL    SAT
              REAL    SMX
              REAL    SW
              REAL    SWCN
              REAL    SWCN2
              REAL    SWCON
              REAL    SWCON1
              REAL    SWCON3
              REAL    SWCONL
              REAL    SWCONX
              REAL    SWEF
              REAL    TDUL
              REAL    U
              REAL    WF
              REAL    WR


c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     ~~~~~~~  CODE  ~~~~~~~~~~~
c     ::::::::::::::::::::::::::

c     Copy to
c     :::::::
      IF (CopyTo) THEN
          Soil%DEPMAX = DEPMAX
          Soil%NLAYR  = NLAYR
          Soil%NUMSL  = NUMSL
          Soil%SALB   = SALB
          Soil%SMX    = SMX
          Soil%SWCON  = SWCON
          Soil%SWCON1 = SWCON1
          Soil%SWCON3 = SWCON3
          Soil%SWCONX = SWCONX
          Soil%SWEF   = SWEF
          Soil%TDUL   = TDUL
          Soil%U      = U

          DO I=1,3
              Soil%CN2(I)    = CN2(I)
          ENDDO
          DO I=1,NL
              Soil%BD(I)     = BD(I)
              Soil%DLAYR(I)  = DLAYR(I)
              Soil%DUL(I)    = DUL(I)
              Soil%LL(I)     = LL(I)
              Soil%RWU(I)    = RWU(I)
              Soil%SAT(I)    = SAT(I)
              Soil%SW(I)     = SW(I)
              Soil%SWCN(I)   = SWCN(I)
              Soil%SWCN2(I)  = SWCN2(I)
              Soil%SWCONL(I) = SWCONL(I)
              Soil%WF(I)     = WF(I)
              Soil%WR(I)     = WR(I)
          ENDDO

      ELSE
              DEPMAX  =  Soil%DEPMAX
              NLAYR   =  Soil%NLAYR 
              NUMSL   =  Soil%NUMSL 
              SALB    =  Soil%SALB  
              SMX     =  Soil%SMX 
              SWCON   =  Soil%SWCON 
              SWCON1  =  Soil%SWCON1
              SWCON3  =  Soil%SWCON3
              SWCONX  =  Soil%SWCONX
              SWEF    =  Soil%SWEF  
              TDUL    =  Soil%TDUL  
              U       =  Soil%U 
  
              DO I=1,3  
                    CN2(I) = Soil%CN2(I)
              ENDDO  
              DO I=1,NL  
                  BD(I)    = Soil%BD(I)
                  DLAYR(I) = Soil%DLAYR(I) 
                  DUL(I)   = Soil%DUL(I)
                  LL(I)    = Soil%LL(I)
                  RWU(I)   = Soil%RWU(I)
                  SAT(I)   = Soil%SAT(I) 
                  SW(I)    = Soil%SW(I)
                  SWCN(I)  = Soil%SWCN(I)  
                  SWCN2(I) = Soil%SWCN2(I) 
                  SWCONL(I)= Soil%SWCONL(I)
                  WF(I)    = Soil%WF(I)
                  WR(I)    = Soil%WR(I)
              ENDDO
      ENDIF


c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     END of subroutine CopyCommon
      END
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     ============================================================




c     This subroutine prints out the CaneCropType object:
      SUBROUTINE PrintCaneCrop(CaneCrop)
c     Instruct compiler to use CANEGRO module defns:
c     ::::::::::::::::::::::::::::::::::::::::::::::
          USE CNG_ModuleDefs
      IMPLICIT NONE
      EXTERNAL INFO

c     The CaneCrop 'object'
c     ::::::::::::::::::::
      TYPE (CaneCropType) CaneCrop
      INTEGER I

!     CHP added write to INFO.OUT
      CHARACTER*78 MSG(38)


c     CANCRP variables:
c     ::::::::::::::::

c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
           !chp temp remove variables not used
           !   REAL    AREAMX
           !   REAL    DEDLFN
           !   REAL    DEDSTK
           !   REAL    EXTCFN
           !   REAL    EXTCFST
           !   REAL    LEN
           !   INTEGER LFMAX
           !   INTEGER LFN
           !   INTEGER LFNMXEXT
           !   REAL    LMAX
           !   INTEGER LOSING
           !   INTEGER LT
           !   REAL    MXLFAREA
           !   INTEGER MXLFARNO
           !   INTEGER NTLGRP
           !   REAL    PHYLO
           !   REAL    PHYLSWTCH
           !   REAL    popcf
           !   REAL    poplfn
           !   REAL    popmax
           !   REAL    POPN
           !   REAL    TEMPOP
           !   REAL    TMEANDEDLF
           !   REAL    TMEANLF
           !   REAL    TOTPOP
           !   REAL    WIDCOR
           !   REAL    WIDTH
           !   REAL    WMAX


c     ~~~~~~~  CODE  ~~~~~~~~~~~
c     ::::::::::::::::::::::::::
!     CHP - Move output to INFO.OUT file
      
      WRITE(MSG(1),'(80(1H-))')
      WRITE(MSG(2),'(10H          ,A, 14(1H ),1H|,A)') 
     &            'CANCRP', 'CaneCrop'
      WRITE(MSG(3),'(80(1H-))')
      WRITE(MSG(4),100) 'TOTPOP', CaneCrop%TOTPOP
      WRITE(MSG(5),100) 'POPCF(1)', CaneCrop%POPCF(1)
      WRITE(MSG(6),100) 'POPCF(2)', CaneCrop%POPCF(2)
      WRITE(MSG(7),100) 'TMEANLF',  CaneCrop%TMEANLF
      WRITE(MSG(8),100) 'TMEANDEDLF', CaneCrop%TMEANDEDLF
      DO I=1,MAX_TILLER_COHORTS
          WRITE(MSG(I+8),100) 'TEMPOP(I)', CaneCrop%TEMPOP(I)
      ENDDO
!      WRITE(*,'(80(1H-))')
  100  FORMAT(A10, 2H: , F10.3)

      CALL INFO(38,'PCaneCrop',MSG)

      END
c     END of subroutine printcanecrop
c     :::::::::::::::::::::::::::::::
c     ===================================





















c     This subroutine initialises values for the
c     CaneCrop object.  This will initially take
c     hardwired values; at some stage, this info
c     is to be read from file.
c     Matthew Jones, September 2006
c     :::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE InitCaneCrop(CaneCrop) !, CONTROL, ISWITCH)
c     ===================================================
          USE CNG_ModuleDefs
          USE ModuleDefs
          IMPLICIT NONE

          TYPE (CaneCropType) CaneCrop
!          TYPE (ControlType)  CONTROL !chp not actually used yet
!          TYPE (SWITCHTYPE)   ISWITCH !chp not actually used yet
        !chp  INTEGER I, J, L
        !chp  REAL AL
        !chp  LOGICAL CF_ERR
c         Temp values read from file
        !chp  REAL T_LFNMXEXT, T_MXLFAREA, T_MXLFARNO
c     ===================================================
c     :::::::::::::::::::::::::::::::::::



c         READ VALUES FROM CULTIVAR FILE:
c         ===============================

c         READ FROM CULTIVAR FILE:
c         ------------------------
c         Note: defaults are set for NCo376
c         :::::::::::::::::::::::::::::::::
c         Mature stalk population:
c         ::::::::::::::::::::::::
c             Set default:
c              CaneCrop%POPN       = 133000.

c             Read from file:
c              CALL GET_CULTIVAR_COEFF(CaneCrop%POPN, 'POPTT16', 
c     -                               CONTROL, ISWITCH, CF_ERR)
c         ::::::::::::::::::::::::

c         Population coefficients:
c         ::::::::::::::::::::::::
c         These describe the response of population to thermal time
c         (MJ thinks).  They change from cultivar to cultivar.
c         ::::::::::::::::::::::::
c         Set defaults:
c         :::::::::::::
c              CaneCrop%POPCF(1) =   1.826
c              CaneCrop%POPCF(2) =  -0.00201
c              CaneCrop%POPCF(3) = 866.70001
c              CaneCrop%POPCF(4) =  -0.99024
c              CaneCrop%POPCF(5) =   0.0003282 

c              DO I=6,10
c                  CaneCrop%POPCF(I)  = 0.
c              ENDDO

c         Read from cultivar file:
c         ::::::::::::::::::::::::
c              CALL GET_CULTIVAR_COEFF(CaneCrop%POPCF(1), 'POPCF(1)',
c     -                             CONTROL, ISWITCH, CF_ERR)
c              CALL GET_CULTIVAR_COEFF(CaneCrop%POPCF(2), 'POPCF(2)',
c     -                             CONTROL, ISWITCH, CF_ERR)
c              CALL GET_CULTIVAR_COEFF(CaneCrop%POPCF(3), 'POPCF(3)',
c     -                             CONTROL, ISWITCH, CF_ERR)
c              CALL GET_CULTIVAR_COEFF(CaneCrop%POPCF(4), 'POPCF(4)',
c     -                             CONTROL, ISWITCH, CF_ERR)
c              CALL GET_CULTIVAR_COEFF(CaneCrop%POPCF(5), 'POPCF(5)',
c     -                             CONTROL, ISWITCH, CF_ERR)


c         READ VALUES FROM CULTIVAR FILE:
c         ===============================
c         Extinction coefficients:
c         ::::::::::::::::::::::::
c         Set defaults:
c              CaneCrop%EXTCFN     = 0.840
c              CaneCrop%EXTCFST    = 0.580

c         Read from file:
c              CALL GET_CULTIVAR_COEFF(CaneCrop%EXTCFN, 'EXTCFN', 
c     -                               CONTROL, ISWITCH, CF_ERR)
c              CALL GET_CULTIVAR_COEFF(CaneCrop%EXTCFST, 'EXTCFST', 
c     -                               CONTROL, ISWITCH, CF_ERR)
c         ::::::::::::::::::::::::

c         LFNMXEXT - not sure what this represents...
c         ::::::::::::::::::::::::::::::::::::::::::::
c         Default value:
c              CaneCrop%LFNMXEXT   = 20

c         Read from file:
c             Note: this value is read as a REAL and then converted to integer:
c              CALL GET_CULTIVAR_COEFF(T_LFNMXEXT, 'LFNMXEXT', 
c     -                               CONTROL, ISWITCH, CF_ERR)
c              CaneCrop%LFNMXEXT = INT(T_LFNMXEXT)
c         ::::::::::::::::::::::::::::::::::::::::::::

c         Maximum leaf area, per leaf, cm2:
c         :::::::::::::::::::::::::::::::::
c             Set default:
c              CaneCrop%MXLFAREA   = 360.

c             Read from file:
c              CALL GET_CULTIVAR_COEFF(T_MXLFAREA, 'MXLFAREA',
c     -                             CONTROL, ISWITCH, CF_ERR)
c              CaneCrop%MXLFAREA = T_MXLFAREA
c         :::::::::::::::::::::::::::::::::

c         This is a variety-specific value
c         It seems to affect the width of the leaf
c         ::::::::::::::::::::::::::::::::::::::::
c             Set default
c              CaneCrop%WIDCOR     = 1.

c             Read from file:
c              CALL GET_CULTIVAR_COEFF(CaneCrop%WIDCOR, 'WIDCOR',
c     -                             CONTROL, ISWITCH, CF_ERR)
c         ::::::::::::::::::::::::::::::::::::::::








c         Leaf number at which max. leaf size is reached:
c         :::::::::::::::::::::::::::::::::::::::::::::::
c             Set default:
c              CaneCrop%MXLFARNO   = 14

c             Read from file:
c              CALL GET_CULTIVAR_COEFF(T_MXLFARNO, 'MXLFARNO',
c     -                             CONTROL, ISWITCH, CF_ERR)
c              CaneCrop%MXLFARNO = INT(T_MXLFARNO)
c         :::::::::::::::::::::::::::::::::::::::::::::::

c         Number of leaves at which phyllocron interval switches:
c         :::::::::::::::::::::::::::::::::::::::::::::::::::::::
c             Default value:
c              CaneCrop%PHYLSWTCH  = 18.

c             Read from file:
c              CALL GET_CULTIVAR_COEFF(CaneCrop%PHYLSWTCH, 'PSWITCH', 
c     -                             CONTROL, ISWITCH, CF_ERR)              
c         :::::::::::::::::::::::::::::::::::::::::::::::::::::::


c         Phyllocron intervals:
c         :::::::::::::::::::::
c             Set defaults
c              CaneCrop%PHYLO(1) =  69.
c              CaneCrop%PHYLO(2) = 169.

c             Read from file
c              CALL GET_CULTIVAR_COEFF(CaneCrop%PHYLO(1), 'PI1', 
c     -                                CONTROL,ISWITCH, CF_ERR)
c              CALL GET_CULTIVAR_COEFF(CaneCrop%PHYLO(2), 'PI2', 
c     -                                CONTROL,ISWITCH, CF_ERR)
c         :::::::::::::::::::::

c         END READ VALUES FROM CULTIVAR FILE
c         ==================================

c         Species coeff.
c          CaneCrop%poplfn     = 4.5

c         Not used - BEST is read directly
          CaneCrop%LFMAX      = 11



c         INITIALISE START VALUES FOR STATE VARIABLES:
c         ============================================
c          CaneCrop%DEDSTK     = 0.
c          CaneCrop%LOSING     = 0
c          CaneCrop%LT         = 0.
c          CaneCrop%NTLGRP     = 1
c          CaneCrop%TMEANDEDLF = 0.
c          CaneCrop%TMEANLF    = 0.
c          CaneCrop%TOTPOP     = 0.

c         Copy array values
c         :::::::::::::::::

c          DO I=1,30
c              CaneCrop%DEDLFN(I) = 0.
c              CaneCrop%LFN(I)    = 0
c              CaneCrop%POPMAX(I) = 0.
c              CaneCrop%TEMPOP(I) = 0.
c          ENDDO
c          DO I=1,70
c              CaneCrop%AREAMX(I) = 0.
c              CaneCrop%LMAX(I)   = 0.
c              CaneCrop%WMAX(I)   = 0.
c              CaneCrop%WIDTH(I)  = 0.
c              DO J=1,30
c                  CaneCrop%LEN(J,I) = 0.
c              ENDDO
c          ENDDO


c         Values used here can be considered species / model coeffs
c          DO 5 L=1,70
c             AL=REAL(L)
c            Taken from CANINT.for
C            This was replaced 15/6/92 by GIB AREAMX(L)=-54.6+36.88*AL-0.945*AL**2
c             CaneCrop%AREAMX(L) = -20.8 + 27.2  * AL
c             CaneCrop%LMAX(L)   = 21.8  + 12.2  * AL - 0.376  * AL**2
c             CaneCrop%WMAX(L)   = (7.75 + 2.243 * AL - 0.0345 * AL**2)
c     -                             / 10.0
c             IF(AL.GT.15.0) THEN
c                CaneCrop%AREAMX(L) = -20.8+27.2*15.0
c                CaneCrop%LMAX(L)   = AMAX1 (CaneCrop%LMAX(L),100.0) 
c                CaneCrop%WMAX(L)   = AMAX1 (CaneCrop%WMAX(L),3.5) 
c             ENDIF      
c  5         CONTINUE

c         MJ: Eh?  I'm sure this is not right!
c          DO L=1,70
c	        IF (L.GE.CaneCrop%MXLFARNO) 
c     -                 CaneCrop%AREAMX(L) = CaneCrop%MXLFAREA
c          ENDDO
      END





c     Print out soil properties:
c      SUBROUTINE PrintSoil(SOILPROP)
c          USE ModuleDefs 
c          USE ModuleData
c
c          EXTERNAL GETLUN
c
c          Type(SoilType) SOILPROP
c          TYPE (SwitchType) ISWITCH
!          INTEGER SCLUN
c
c          CALL GET(ISWITCH)
c          IF (INDEX('YDA',ISWITCH%IDETL)< 1) RETURN
c
!          CALL GETLUN('WORK.OUT',SCLUN)
c         Print soil layer properties in CANEGRO-compatible form:
c         DLAYR:
!          WRITE(SCLUN, '(A5, 1X, 20(F4.0, 1X))') 'DLAYR', SOILPROP%DLAYR
c         LL
!          WRITE(SCLUN, '(A5, 1X, 20(F4.3, 1X))') 'LL', SOILPROP%LL
c         DUL
!          WRITE(SCLUN, '(A5, 1X, 20(F4.3, 1X))') 'DUL', SOILPROP%DUL
c         SAT
!          WRITE(SCLUN, '(A5, 1X, 20(F4.3, 1X))') 'SAT', SOILPROP%SAT
c         BD
!          WRITE(SCLUN, '(A5, 1X, 20(F4.2, 1X))') 'BD', SOILPROP%BD
c         SWCN
!          WRITE(SCLUN, '(A5, 1X, 20(F4.2, 1X))') 'SWCN', SOILPROP%SWCN
c
c      END



c     MJ, November 2006
c     ::::::::::::::::::
c     This subroutine looks up variables from the input file 
c     (DSSAT45.INP)
c     ===============================================================
      SUBROUTINE FIND_INP(var, varname, Control)
          USE ModuleDefs
          IMPLICIT NONE
          EXTERNAL ERROR, FIND, GETLUN
          SAVE

          TYPE(CONTROLTYPE) CONTROL
          REAL var
          CHARACTER*(*) varname
c         Planting method
          CHARACTER*1   PLTRAT

          INTEGER LUNIO, ERR, LNUM
          CHARACTER*6     ERRKEY          
          PARAMETER       (ERRKEY='SC_CNG')   
          CHARACTER*30    FILEIO 
          CHARACTER*12    FILEC     
       !chp   CHARACTER*92    FILECC
          CHARACTER*12    FILES
          CHARACTER*12    FILEE 
          CHARACTER*80    PATHCR 
          CHARACTER*80    PATHSR
          CHARACTER*80    PATHER 

          REAL PLTPOP,ROWSPC
          CHARACTER*6     SECTION 
          INTEGER         LINC
          INTEGER         FOUND  

c         Has the file been read?
          LOGICAL HASBEENREAD
      

c     ===============================================================
c     CODE
c     ===============================================================
c     Set variables
      HASBEENREAD = .FALSE.
      FILEIO  = CONTROL % FILEIO

c     Open inp file:
c     NOTE: code copied (with permission) from MZ_GROSUB - thanks 
c           Cheryl!
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF (.NOT.(HASBEENREAD)) THEN
          !-------------------------------------------------------
          !     Read input file name (ie. DSSAT45.INP) and path
          !-------------------------------------------------------
          CALL GETLUN('FILEIO', LUNIO)
          OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)  
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
          READ(LUNIO,50,IOSTAT=ERR) FILES, PATHSR; LNUM = 7
   50     FORMAT(//////,15X,A12,1X,A80)
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

          READ(LUNIO,51,IOSTAT=ERR) FILEE, PATHER; LNUM = LNUM + 1   
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

          READ(LUNIO,51,IOSTAT=ERR) FILEC, PATHCR; LNUM = LNUM + 1 
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
   51     FORMAT(15X,A12,1X,A80)
          !------------------------------------------------------
          !   Read Planting Details Section
          !------------------------------------------------------
          SECTION = '*PLANT'
          CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
          IF (FOUND .EQ. 0) THEN
            CALL ERROR(SECTION, 42, FILEIO, LNUM)
          ELSE
c           Read plant population, rowspacing, and planting method
            READ(LUNIO,61,IOSTAT=ERR) PLTPOP, PLTRAT, ROWSPC   
            LNUM = LNUM + 1
 60         FORMAT(25X,F5.2,13X,F5.2,7X,F5.2)
c           Added by MJ, 2007-05-04
 61         FORMAT(25X,F5.2,5X, A1,7X,F5.2,7X,F5.2)
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
          ENDIF

c         Set variable so that file is not reread for every parameter
c         request
          HASBEENREAD = .TRUE.
C     -----------------------------------------------------------------
C             Read crop cultivar coefficients
C     -----------------------------------------------------------------
c          SECTION = '*CULTI'
c          CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
c          IF (FOUND .EQ. 0) THEN
c              CALL ERROR(SECTION, 42, FILEIO, LNUM)
c          ELSE
c            READ (LUNIO,1800,IOSTAT=ERR) VARNO,VRNAME,ECONO,
c     %                 P1,P2,P5,G2,G3,PHINT  
!CHP 1800        FORMAT (A6,1X,A16,1X,A6,1X,F6.1,F6.3,2(F6.1),2(F6.2))    
c1800        FORMAT (A6,1X,A16,1X,A6,1X,6F6.0)    
c            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
c          ENDIF

        CLOSE(LUNIO)
      ENDIF

c     ===============================================================
c         Search for requested parameter:
c         :::::::::::::::::::::::::::::::
      SELECT CASE (varname)
c         Row spacing:
c         ::::::::::::
          CASE ('ROWSPC')
              var = rowspc/100.

          CASE ('RATOON')
c             If a ratoon is specifically selected, then use ratoon
              IF (PLTRAT .EQ. 'R') THEN
                  var = 1.
              ELSE
c             Any other form of planting will be treated as seed(cane) = plant
                  var = 0.
              ENDIF

          CASE ('PLTPOP')
              var = PLTPOP
      END SELECT

      CONTINUE

      END


c     MJ, May 2007
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     This subroutine chooses which canopy routine should be 
c     used; the decision is based on which cultivar inputs are 
c     available.  In general, it is assumed that the CANEGRO canopy 
c     is more attractive, so this will be chosen if parameters are 
c     available.  The Canesim canopy is more robust, however.  If no 
c     params are available, the Canesim canopy will be used with 
c     default values.
c     ===============================================================
      SUBROUTINE GET_CANOPY_ROUTINE(CANOPY_ROUTINE, Control)
          USE CNG_ModuleDefs
          USE ModuleDefs
          IMPLICIT NONE
          EXTERNAL GET_CULTIVAR_COEFF, INFO, WARNING

          TYPE(ControlType) CONTROL
          INTEGER CANOPY_ROUTINE, TRUECOUNT, I
          LOGICAL CNG_ERR(14) !, CSIM_ERR(14)
          CHARACTER*256 TEXT(1)

c         CANEGRO canopy variables
c         ::::::::::::::::::::::::
          REAL          AREAMX_CF(3)
          REAL          LMAX_CF(3)
          REAL          MAXLFLENGTH  
          REAL          MAXLFWIDTH  
          REAL          MXLFARNO 
          REAL          WIDCOR
          REAL          WMAX_CF(3)
          REAL          MXLFAREA  

!         Unit number for output
!          INTEGER SCLUN   !CHP


c     1.  Look for CANEGRO canopy parameters:
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c         Very important
          CALL GET_CULTIVAR_COEFF(MXLFAREA, 'MXLFAREA', 
     &                            Control, CNG_ERR(1))
          CALL GET_CULTIVAR_COEFF(MXLFARNO, 'MXLFARNO', 
     &                            Control, CNG_ERR(2))
 

c         Important
          CALL GET_CULTIVAR_COEFF(AREAMX_CF(1), 'AREAMX_CF(1)', 
     &                            Control, CNG_ERR(3))
          CALL GET_CULTIVAR_COEFF(AREAMX_CF(2), 'AREAMX_CF(2)', 
     &                            Control, CNG_ERR(4))
          CALL GET_CULTIVAR_COEFF(AREAMX_CF(3), 'AREAMX_CF(3)', 
     &                            Control, CNG_ERR(5))
          

          CALL GET_CULTIVAR_COEFF(LMAX_CF(1), 'LMAX_CF(1)', 
     &                            Control, CNG_ERR(6))
          CALL GET_CULTIVAR_COEFF(LMAX_CF(2), 'LMAX_CF(2)', 
     &                            Control, CNG_ERR(7))
          CALL GET_CULTIVAR_COEFF(LMAX_CF(3), 'LMAX_CF(3)', 
     &                            Control, CNG_ERR(8))


          CALL GET_CULTIVAR_COEFF(WMAX_CF(1), 'WMAX_CF(1)', 
     &                            Control, CNG_ERR(9))
          CALL GET_CULTIVAR_COEFF(WMAX_CF(2), 'WMAX_CF(2)', 
     &                            Control, CNG_ERR(10))
          CALL GET_CULTIVAR_COEFF(WMAX_CF(3), 'WMAX_CF(3)', 
     &                            Control, CNG_ERR(11))

          CALL GET_CULTIVAR_COEFF(MAXLFLENGTH, 'MAXLFLENGTH', 
     &                            Control, CNG_ERR(12))
            

          CALL GET_CULTIVAR_COEFF(MAXLFWIDTH, 'MAXLFWIDTH', 
     &                            Control, CNG_ERR(13))
            

          CALL GET_CULTIVAR_COEFF(WIDCOR, 'WIDCOR', 
     &                            Control, CNG_ERR(14))

c     2. Check CANEGRO params:
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c         Count number of 'True' cultivars
c         Note: if CNG_ERR(i) == true, it means there was an error
c         reading the file.  Hence if (NOT(cng_err(i))) truecount++
          TRUECOUNT = 0
          DO I=1,14
              IF (.NOT.(CNG_ERR(I))) TRUECOUNT = TRUECOUNT + 1
          ENDDO        
          IF (TRUECOUNT == 14) THEN
              TEXT(1) = 'Using CANEGRO canopy.'
              CALL INFO(1, 'GET_CANOPY_ROUTINE', TEXT)
              CANOPY_ROUTINE = CANEGRO
              RETURN
          ENDIF  

c         Issue a warning if it appears that the user might have been
c         wanting to run CANEGRO:
          IF (TRUECOUNT .GT. 12) THEN
              TEXT(1) = 'WARNING: NOT using CANEGRO Canopy; '
     &                //'defaulting to CANESIM. '
     &                //'Check cult. file.'
              CALL WARNING(1, 'GET_CANOPY_ROUTINE', TEXT)
          ENDIF


c     3. Use CANESIM params instead
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c         If the subroutine has not yet returned, it is assumed that
c         insufficient inputs were available for CANEGRO; simpler
c         CANESIM Canopy to be used:
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          TEXT(1) =  'Using CANESIM Canopy.'
          CALL INFO(1, 'GET_CANOPY_ROUTINE', TEXT)
          CANOPY_ROUTINE = CANESIM

          RETURN

c     ===============================================================
      END
c     ===============================================================
c     End of subroutine
c     ===============================================================







c     MJ, May 2007
c     ::::::::::::::::::
c     This subroutine module determines whether or not to carry over
c     values from planting/ratoon to the next ratoon.
c     Criteria for carryover are:
c     1.  The simulation run is part of a DSSAT sequence run
c     2.  The previous run has the same treatment number as this one
c     3.  The previous run has the same crop/cultivar as this run
c     4.  The planting method for this run is defined as a ratoon crop
c     5.  The planting date for this crop is the same day as / day 
c         after date of harvest of the previous crop
c
c     Note: PRECONDITION! CaneCrop%RATOON must be set
c     ===============================================================
      SUBROUTINE CARRYOVER_RATOON(Control, RATOON, CARRY_OVER)
          USE ModuleDefs
          USE CNG_ModuleDefs
          IMPLICIT NONE
          EXTERNAL GET_CULTIVAR_COEFF, INFO, WARNING
          SAVE

c         Parameter variables
          TYPE(CONTROLTYPE) CONTROL
!          TYPE(RatoonCarryOverType) RatCarryOver

c         Should the values be carried over?
c         * SUBROUTINE ARGUMENT / PARAMETER *
          LOGICAL CARRY_OVER

          INTEGER DYNAMIC

c         There are 5 requirements for a ratoon crop to be carried 
c         over.  For each of these tests, a T/F value is assigned
c         to the following variable.  
c         If all 5 are met, previous crop information is carried over.
c         If more than 3 or 4 of them are met, the system will issue 
c         a warning indicating that there was NO carryover, but that
c         perhaps the user intended this.
          LOGICAL REQS(5)

c         File unit numbers:
c                 FileIO FileX
!          INTEGER LUNIO, LUNFX

c         Treatment/rotation numbers for this and previous simulation:
          INTEGER TNUM_PREV, TNUM_THIS, ROTN_PREV, ROTN_THIS

c         Cultivar names for this and previous sim:
          CHARACTER*16 CULT_THIS, CULT_PREV
          LOGICAL CERR
          REAL DUMMY

c         Dates of planting/harvest:
          INTEGER YRDOY_THIS, YRDOY_PREV

c         An error message, to issue in the WARNINGS.OUT file
          CHARACTER*78 ERRMSG(6)

c         Useful counter/loop variables:
          INTEGER COUNT, I, ERRCOUNT



c         Ratoon number (0 = plant crop)
          INTEGER RATOON

c         Initialise values (because apparently the RUNINIT
c         can't be trusted with a sequential run
          DATA CULT_PREV /'         '/
          DATA TNUM_PREV, ROTN_PREV /-1, -1/
          DATA TNUM_THIS, ROTN_THIS /0, 0/


c     ===============================================================
c     CODE:
c     ===============================================================

          DYNAMIC = Control%DYNAMIC
c     -----------------------------------------------------
c
c              DYNAMIC = RUNINIT
c              Beginning of simulation
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF (DYNAMIC.EQ.RUNINIT) THEN
c         Init variables with default values
          REQS = .FALSE.

          CULT_THIS = '         '
c          CULT_PREV = '         '

          CARRY_OVER = .FALSE.
          

     


c     -----------------------------------------------------
c
c              DYNAMIC = SEASINIT
c              Beginning of season
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF (DYNAMIC.EQ.SEASINIT) THEN

          ERRMSG    = ''
          ERRCOUNT  = 2
          ERRMSG(1) = 'Did not carry over values to this ratoon, ' 
     &              //'because: '

c     1. is this a DSSAT sequence?
c     ----------------------------
          IF ((Control%RNMODE .EQ. 'Q') .OR. (Control%RNMODE .EQ. 'F'))
     &    THEN
c              WRITE(*,'(A)') 'This is a sequence.'
              REQS(1) = .TRUE.
          ELSE
              ERRMSG(ERRCOUNT) 
     &              = ' this is NOT a sequential run; '
              ERRCOUNT = ERRCOUNT + 1
              REQS(1) = .FALSE.
c              WRITE(*,'(A)') 'This is NOT a sequence.'
          ENDIF

c          WRITE(*,*) 'CONTROL RNMODE is ', CONTROL%RNMODE
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


c     2.    The previous run has the same treatment number as this one:
c     -----------------------------------------------------------------
c     2.1   Get treatment / rotation numbers for this run

          TNUM_THIS = Control%TRTNUM
c          ROTN_THIS = Control%MULTI
          ROTN_THIS = Control%RUN

          
c         Is this the first run in the simulation? (do not
c         carry over):
          IF (TNUM_PREV .EQ. -1) THEN
              REQS(2) = .FALSE.
c              WRITE(*,'(A)') 'No previous treatment number.'
          ELSE
c         Otherwise, check:
              IF ((TNUM_PREV .EQ. TNUM_THIS) .AND.
     &            (ROTN_THIS .EQ. ROTN_PREV + 1))
     &        THEN
c                  WRITE(*,'(A)') 'Previous treatment number equals 
c     & this one, and rotation number is one more than previous.'
                  REQS(2) = .TRUE.
              ELSE

                  REQS(2) = .FALSE.
              ENDIF
          ENDIF

c         Temp:
c         Perhaps move to WARNINGS.OUT / INFO.OUT
          IF (.NOT.(REQS(2))) THEN
c               WRITE(*,'(A)') 'Assigning err.'
               ERRMSG(ERRCOUNT) =  
     &            ' this treatment number != previous ' 
     &         // 'one, or crop rotation / run number not 1 more '
     &         // 'than previous one; '

          ERRCOUNT = ERRCOUNT + 1
          ENDIF
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


c     3.  The previous run has the same crop/cultivar as this run
c     -----------------------------------------------------------
c     Consider: if the previous crop was not sugarcane, then
c     (a) CULT_PREV would equal CULT_THIS for the last sugarcane run
c     and not for the previous <crop> run
c     (b) ROTN_PREV would equal ROTN_THIS for the previous run, but
c     would fail on point (2) because ROTN_THIS = ROTN_PREV + 2 (at
c     least), not ROTN_PREV + 1
c     So it is only necessary to check the cultivar names:

c         Note: cultivar coefficient subroutine only reads REAL
c         values.  For this reason, it was decided to introduce
c         a global variable called 'CULTIVAR'; this is initialised
c         by GET_CULTIVAR_COEFF... so a dummy call must be made:
c         ::::::::::::::::::::::::::::::::::::::::::::::::::::::
          CALL GET_CULTIVAR_COEFF(DUMMY, 'TTBASEEM', Control, CERR)
c         Now retrieve cultivar name:
          CULT_THIS = CULTIVAR
c          WRITE(*,*) 'CULTNAME is ', CULT_THIS
          IF (CULT_THIS .EQ. CULT_PREV) THEN
              REQS(3) = .TRUE.
          ELSE
              REQS(3) = .FALSE.
          ENDIF

c         Issue a warning?
          IF (.NOT.(REQS(3))) THEN
              ERRMSG(ERRCOUNT) = ' cultivar (' // TRIM(CULT_THIS) 
     &                 // ') not consistent with '
     &                 // 'previous run ( ' // TRIM(CULT_PREV) 
     &                 // ')'
              ERRCOUNT = ERRCOUNT + 1
c              WRITE(*, '(A)') ERRMSG(ERRCOUNT-1)

          ENDIF
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


c     4.  The planting method for this run is defined as a ratoon crop
c     -----------------------------------------------------------------
          IF (RATOON .GT. 0) THEN
              REQS(4) = .TRUE.
          ELSE
              REQS(4) = .FALSE.
          ENDIF

c         Issue a warning?
          IF (.NOT.(REQS(4))) THEN
              ERRMSG(ERRCOUNT) = ' this crop is not a ratoon crop; '
              ERRCOUNT = ERRCOUNT + 1
              !WRITE(*, '(A)') ERRMSG(ERRCOUNT-1)
              CALL INFO(ERRCOUNT,"SC_CNG",ERRMSG)
          ENDIF
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

c     5.  The planting date for this crop is the same day as / day 
c         after date of harvest of the previous crop
c     ---------------------------------------------------------------
c          WRITE(*,*) 'This  YRDOY is ', CONTROL%YRDOY
c          WRITE(*,*) 'Prev. YRDOY is ', YRDOY_PREV
          YRDOY_THIS = CONTROL%YRDOY
          IF ((YRDOY_THIS .EQ. YRDOY_PREV) .OR.
     &        (YRDOY_THIS .EQ. YRDOY_PREV + 1))
     &    THEN
              REQS(5) = .TRUE.
          ELSE
              REQS(5) = .FALSE.
          ENDIF

c         Issue a warning?
          IF (.NOT.(REQS(5))) THEN
              ERRMSG(ERRCOUNT) =  
     &           'plant/ratoon date not the same/next '
     &        // 'day as harvest of previous crop.'
          ERRCOUNT = ERRCOUNT + 1
          ENDIF
c     ---------------------------------------------------------------
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     
c     And now, for something completely different! - 
c     Ba bum ba dum ba bum ba dum ba bum diddly dum
c     Check the REQS array.  If all are true, carry over; if 3/5 are
c     true, issue a warning and indicate why the carryover might 
c     have failed:
c     ---------------------------------------------------------------
c     Count true values:
      COUNT = 0
      DO I = 1, 5
          IF (REQS(I)) COUNT = COUNT + 1 
      ENDDO

c     Issue warning
      IF ((COUNT .GE. 3) .AND. (COUNT .LT. 5)) THEN
          CALL WARNING(ERRCOUNT, 'RATOON CARRYOVER', ERRMSG)
c          WRITE(*, '(6(A78))') ERRMSG
      ELSEIF
c     If the simulation is a sequence and harvest/plant dates 
c     consecutive, then issue warning as well (basically 
c     to indicate that possibly this should be defined as 
c     ratoon crop following the first rotation):
     & (REQS(1) .AND. REQS(5) .AND. (COUNT .LT. 5)) THEN
          CALL WARNING(ERRCOUNT, 'RATOON CARRYOVER', ERRMSG)
c          WRITE(*, '(6(A78))') ERRMSG
      ENDIF

c     What we've all been waiting for, the grand result:
      IF (COUNT .EQ. 5) THEN
          CARRY_OVER = .TRUE.
c         And issue a warning
          ERRMSG(1) = 'Carrying ratoon info over.'
          CALL WARNING(1, 'RATOON CARRYOVER', ERRMSG)
c          WRITE(*,*) ERRMSG(1)
      ENDIF

c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

c     ===============================================================
c         Note: this was not programmed most efficiently; however,
c         I chose to do it this way for explicit clarity. 
c         (MJ, 2007-05-09)
c     ===============================================================


c     -----------------------------------------------------
c
c              DYNAMIC = SEASEND
c              End of season...
c
c     Set things up to handle a possible carryover
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF (DYNAMIC.EQ.SEASEND) THEN

c         Check when this is called:
c          WRITE(*, '(A)') 'END of season! *** '

c         Copy this treatment number to 'previous' treatment number
          TNUM_PREV = TNUM_THIS

c         Copy this rotation number to 'previous' rotation number
          ROTN_PREV = ROTN_THIS

c                  WRITE(*,'(A, 2(I3, 1H, ))') 'This TNUM/ last TNUM: ',  
c     &                           TNUM_THIS, TNUM_PREV
c                  WRITE(*,'(A, 2(I3, 1H, ))') 'This RNUM/ last RNUM: ',  
c     &                           ROTN_THIS, ROTN_PREV
c          WRITE(*, '(A)') '*** END of season! '

c         Copy cultivar from this to previous:
          CULT_PREV = CULT_THIS

c         Copy day of harvest (today, now, when running) to YRDOY_PREV
          YRDOY_PREV = Control%YRDOY

c         In SC_ROOTG - ROOTRGROWTH subroutine.
c         Possibly carry over information:
c          RatCarryOver%RTDEP = WaterBal%RTDEP  
      ENDIF

      END


c     ===============================================================      
c     A function that calculates change in degree days based on 
c     mean daily temperature (TMEAN), a base temperature (TBase), 
c     an optimal temperature (TOpt), 
c     and a final cutoff temperature (TFinal) above which the
c     plant stops responding to temperature
c     DTT |___/\__
c            °C
c     MJ, Dec 2012
      REAL FUNCTION D_TT(TMEAN, TBase, TOpt, TFinal)
c     ===============================================================      
      IMPLICIT NONE
      REAL, INTENT(IN) :: TMEAN, TBase, TOpt, TFinal
      REAL SLOPE, INTERCEPT
c     Note: Tbase < TOpt < TFinal

      IF (TMEAN .LE. TBase) THEN
c       Temperatures less than TBase:
        D_TT = 0.
      ELSE IF (TMEAN .LE. TOpt) THEN
c       Temperatures up to TOpt
        D_TT = MAX(0., TMEAN - TBase)
      ELSE IF (TMEAN .LE. TFinal) THEN
c       Temperatures between TOpt and TFinal
c       Calculate slope of decrease in D_TT
        SLOPE = (TOpt - TBase)/(TOpt-TFinal)
c       and the intercept
        INTERCEPT = -SLOPE*TFinal
c       Now calculate the D_TT:
        D_TT = SLOPE * TMEAN + INTERCEPT
      ELSE
        D_TT = 0.
      ENDIF

      END
c     =============================================================== 



c     ===============================================================      
c     A function that calculates change in degree days based on 
c     mean daily temperature (TMEAN), a base temperature (TBase), 
c     an optimal temperature range start (Topt1), an optimal
c     temperature range end (Topt2),
c     and a final cutoff temperature (TFinal) above which the
c     plant stops responding to temperature
c     DTT |___/--\__
c            °C
c     MJ, Dec 2012
      REAL FUNCTION D_TT4(TMEAN, TBase, TOpt1, Topt2, TFinal)
c     ===============================================================      
      IMPLICIT NONE
      REAL, INTENT(IN) :: TMEAN, TBase, TOpt1, TOpt2, TFinal
      REAL SLOPE, INTERCEPT
c     Note: Tbase < TOpt1 < TOpt2 < TFinal

      IF (TMEAN .LE. TBase) THEN
c       Temperatures less than TBase:
        D_TT4 = 0.
      ELSE IF (TMEAN .LE. TOpt1) THEN
c       Temperatures up to TOpt
        D_TT4 = MAX(0., TMEAN - TBase)
      ELSE IF (TMEAN .LE. TOpt2) THEN
c       Temperatures between TOpt1 and TOpt2
        D_TT4 = TOpt1 - TBase
      ELSE IF (TMEAN .LE. TFinal) THEN
c       Temperatures between TOpt and TFinal
c       Calculate slope of decrease in D_TT
        SLOPE = (TOpt1 - TBase)/(TOpt2-TFinal)
c       and the intercept
        INTERCEPT = -SLOPE*TFinal
c       Now calculate the D_TT:
        D_TT4 = SLOPE * TMEAN + INTERCEPT
      ELSE
        D_TT4 = 0.
      ENDIF

      END
c     =============================================================== 



c     ---------------------------------------------------------------
c                       N_MODULE composite variables
c     ---------------------------------------------------------------
c     A subroutine for calculating Nitrogen demand, stress, etc
c     for the SASRI Canegro Plant Module in the DSSAT CSM.
c     These Fortran composite variables are used for assisting in
c     operation and clarity of the code
c     ---------------------------------------------------------------
c     July 2008
c
c     Matthew Jones,  (matthew.jones@sugar.org.za)
c     Maurits van den Berg (maurits_vandenberg@sugar.org.za)
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     (c) South African Sugarcane Research Institute
c     Mount Edgecombe, 4300
c     ---------------------------------------------------------------

      MODULE N_TYPES
c         NUM_COMPONENTS: Number of plant components
          INTEGER, PARAMETER:: NUM_COMPS = 4

c         TOPS, STALKS, ROOTS, TRASH: index numbers for
c         each of these components
          INTEGER, PARAMETER:: TOPS  = 1, STALKS = 2, 
     &                         ROOTS = 3, DEADLF  = 4

c         Composite variable type to store
c         N-stress response to N-concentration parameters:
          TYPE N_PARAM_TYPE
c             X: N-concentration (kg/kg/ha)
              REAL X_VALUES(4)
c             Y: N-stress factor (0-1) 
c                [0 = maximum stress; 1 = no stress]
              REAL Y_VALUES(4)

c             MIN_CONC: minimum concentration of N for this plant
c             component.  Define along the lines of, if N-conc
c             was any lower, the plant would be dead.  In practice,
c             N-conc might be lower, and might fall if DM increases,
c             but the N translocation process cannot reduce N-conc 
c             below this
              REAL MIN_CONC
              REAL CRIT_CONC
	        REAL OPT_CONC
			REAL INIT_CONC



          END TYPE

      END
c     ===============================================================