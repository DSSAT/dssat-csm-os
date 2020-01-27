	module SAM_ModuleDefs
    !-------------------------------------------------------------------------------!
    !     Contains defintion of derived data types and constants which are          !
    !     to be used in the intermediate SAMUCA model (Following CANEGRO - MJ)      !
    !-------------------------------------------------------------------------------!

      use ModuleDefs    ! Accessing DSSAT Global Env.
      save

    !--- Global Variables

    !--- The name of the (current) cultivar
      CHARACTER*20 CULTIVAR, SCSTGNAM(20), GROWTHPHASES(5)

!     Phenological stages:
!      INTEGER SEEDCANE, GERMINATED, EMERGED
      
!!     Sugarcane growth phases:
!      DATA GROWTHPHASES /
!!       1: emergence
!     &  'EMERGENCE ',
!!       2: tillering
!     &  'TILLERING ',
!!       3: tillering following stalk emergence
!     &  'TILL+STALK',
!!       4: Stalk elongation and tiller senescence
!     &  'Stlk.Senes',
!     &  'FLWR INIT '/

        !--- Climate type:
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
          
          !--- WaterBalance
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
              REAL EOS
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
              REAL TRWUP
              REAL TSW
        END TYPE WaterType
          
        type CaneSamuca
              REAL    AREAMX(70)
              REAL    CANHEIGHT
!c             Should the ratoon be carried over?
              LOGICAL CARRY_OVER
              REAL CWSI
!c              REAL    DEDLFN(30)
              !REAL    DEDLFN(MAX_TILLER_COHORTS)
              REAL    DEDSTK
              REAL    EXTCFN
              REAL    EXTCFST
              REAL    GLFN
!c             Growth phase
              INTEGER GROPHASE

!c              REAL    LEN(30,70)
              !REAL    LEN(MAX_TILLER_COHORTS,70)
              INTEGER LFMAX
!c              INTEGER LFN(30)
!              INTEGER LFN(MAX_TILLER_COHORTS)
              INTEGER LFNMXEXT
              REAL    LMAX(70)
              INTEGER LOSING
              INTEGER LT
              INTEGER MXLFAREA
              INTEGER MXLFARNO
              INTEGER NTLGRP
!c             Different phyllochron intervals
              REAL    PHYLO(2)
!c             Number of leaves before Phyllo interval changes
              REAL    PHYLSWTCH
!c             Empirical population = f(TT) coefficients
              REAL    popcf(10)
              REAL    poplfn
!c              REAL    popmax(30)
!              REAL    popmax(MAX_TILLER_COHORTS)
              REAL    POPN
!            Ratoon: 0 = plant crop, ratoon 1, 2, 3, etc
              INTEGER RATOON
!            Stalk height
              REAL    SHGT
!             REAL    TEMPOP(30)
!              REAL    TEMPOP(MAX_TILLER_COHORTS)
              REAL    TMEANDEDLF
              REAL    TMEANLF
              REAL    TOTPOP
              REAL    WIDCOR
              REAL    WIDTH(70)
              REAL    WMAX(70)
        
        end type CaneSamuca
        
        
!c     The canopy/crop type:
!c     :::::::::::::::::::::
          TYPE CaneCropType
              
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

!        This is the partitioning / photosynthesis
!        composite type.
!        :::::::::::::::::::::::::::::::::::::::::
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
!            Daily growth respiration rate (t/ha/d)
              REAL RESP_G
!            Daily maintenance respiration rate (t/ha/d)
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
              REAL    CHUPOP
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
!        :::::::::::::::::::::::::::::::::::::::::


!        MJ, 2007-05-15:
!        The Ratoon carryover type:
!        ::::::::::::::::::::::::::
!        This composite variable stores information that will be
!        used to initialise a ratoon crop in a crop sequence.
!        Problem:
!        ::::::::
!        In reality, the existing root system dies off slowly 
!        while the new root system, grown by new tillers, replaces
!        it.  Short of modelling two cohorts of roots, I'm not 
!        sure how to handle this (CANEGRO does not handle it).
          TYPE RatoonCarryOverType
!            Root depth
              REAL RTDEP
          END TYPE RatoonCarryOverType


!    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!    MJ, May 2012
!    Define the tiller cohort type.
!    This composite variable represents a set of shoots.
!    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TYPE SHOOT_COHORT
!       Date of emergence (TT_BASE_EMERGE)
         REAL EMERGE_TTDATE 
!       Age of this shoot cohort in Degree Days (Base TT16 ~ TT_Emerge)
         REAL AGE_TT
         REAL DTT_POP
!       The number of shoots this cohort represents (shoots/m)
         REAL NUMSHOOTS, D_NUMSHOOTS
!       Change in the number of tillers (if this is a primary cohort)
!       (whole numbers)
         REAL D_N_TILLERS
!       Is this a primary shoot or a tiller cohort?
         LOGICAL ISPRIMARY
!       Unique ID for this shoot cohort (in case it's useful!)
         INTEGER UNIQUE_ID
      END TYPE SHOOT_COHORT
!    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  


!    ==================================================================
      end module SAM_ModuleDefs

