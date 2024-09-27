!======================================================================
!  SU_GROSUB, Subroutine
!
!  Sunflower growth routine
!----------------------------------------------------------------------
!  Revision history
!
!                 Written
!  02/07/1993 PWW Header revision and minor changes   
!  02/07/1993 PWW Switch block added, etc                     
!  06/21/1994 JTR & BDB Updated PCARB calculation                       
!  03/29/2001 WDB Converted to modular version               
!  12/01/2000 WDB Major restructuring for 2002 release      
!  04/15/2002 WDB Added pest damage and eco and species file 
!  06/11/2002 GH  Modified for Y2K
!  08/12/2003 CHP Added I/O error checking
!  12/16/2004 CHP Changed conversion for grain weight from PLTPOP to
!                   ears to account for barrenness factor.
!  02/04/2005 CHP Changed conversion for podwt to account for
!                   barrenness factor.
!  05/11/2005 CHP Calculate N loss in senesced leaves for N balance.
!  08/11/2005 JIL Minor revisions to eliminate carbon imbalance
!                 Do not allow increase in leaf number during 
!                   expansion of final leaf.
!  10/04/2005 JIL Set MDATE upon failure due to cold stress or water 
!                   stress.
!  03/30/2006 CHP Added composition of senesced matter for SOM modules
!  05/01/2006 CHP Added RS canopy height computation
!  06/06/2006 CHP/JIL Added optional temperature sensitivity parameter
!                 to ecotype file (TSEN)
!  07/13/2006 CHP Added P model
!  10/31/2007 CHP Added simple K model.
!  01/03/2013 CHP Initialization for RLV prevents carryover 
!  11/01/2020 fv Added specific code for sunflower
!----------------------------------------------------------------------
!
!  Called : Sunflower
!
!  Calls  : NFACTO NUPTAK
!----------------------------------------------------------------------

      SUBROUTINE SU_GROSUB (DYNAMIC, ISWITCH, 
     &      ASMDOT, CDAY, CO2, DLAYR, DS, DTT, EOP, FILEIO,   !Input
     &      FracRts, ISTAGE, KG2PPM, LL, NLAYR, NH4, NO3,     !Input
     &      PLTPOP, PPLTD, RLV, RTDEP, RUE, SAT, SeedFrac,    !Input
     &      SHF, SLPF, SPi_AVAIL, SRAD, STGDOY, SUMDTT, SW,   !Input
     &      SWIDOT, TLNO, TMAX, TMIN, TRWUP, TSEN, VegFrac,   !Input
     &      WLIDOT, WRIDOT, WSIDOT, XNTI, XSTAGE,             !Input
     &      YRDOY, YRPLT, SKi_Avail,                          !Input
     &      EARS, GPP, MDATE,HARVFRAC,                        !I/O
     &      AGEFAC, APTNUP, AREALF, CANHT, CANNAA, CANWAA,    !Output
     &      CANWH, CARBO, GNUP, GPSM, GRNWT, GRORT, HI, HIO,  !Output
     &      LEAFNO, NSTRES, PCNGRN, PCNL, PCNRT, PCNST,       !Output
     &      PCNVEG, OILPC, PConc_Root, PConc_Seed,     !Output
     &      PConc_Shel, PConc_Shut, OILWT, PORMIN, PSTRES1,   !Output
     &      PSTRES2, PTF, PUptake, RLWR, ROOTN, RSTAGE, RTWT, !Output
     &      RTWTO, RWUMX, SATFAC, SDWT, SEEDNO, SENESCE,      !Output
     &      SHELPC, SI1, SI3, SKERWT, SLA, STMWTO, STOVER,    !Output
     &      STOVN, STOVWT, SUMP, SWFAC, TOPWT, TURFAC, UNH4,  !Output
     &      UNO3, VSTAGE, WTLF, WTNCAN, WTNLF, WTNSD, WTNST,  !Output
     &      WTNUP, WTNVEG, XGNP, XHLAI, XLAI, XN, YIELD,      !Output
     &      KUptake, KSTRES,                                  !Output
     &      PERWT,EMBWT,PERWTE,EMBWTE,HEADWT,POTGROPER,
     &      POTHEADWT,PPP,PSKER,GRNWTE,KCAN,KEP,CUMDTT,P3P,P9)

      USE ModuleDefs
      USE Interface_SenLig_Ceres
      IMPLICIT  NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE, SU_NFACTO, SU_NUPTAK, 
     &  SU_KUPTAK, P_Ceres, WARNING, TABEX, CURV
      SAVE
!----------------------------------------------------------------------
!                         Variable Declaration
!----------------------------------------------------------------------

      REAL        AGEFAC            
      REAL        APTNUP      
      REAL        AREALF
      REAL        ASMDOT
      REAL        BIOMAS 
      CHARACTER*80 C80
      REAL        CANHT     
!**
      REAL        CANHT_POT !added 26May04 RS     
!**
      REAL        CANNAA      
      REAL        CANWAA 
      REAL        CANWH     
      REAL        CARBO       
      REAL        CARBOT       
      INTEGER     CDAY     
      INTEGER     CMAT
      REAL        CNSD1       
      REAL        CNSD2        
      REAL        CPSD1       
      REAL        CPSD2        
      INTEGER        CUMPH       
      REAL        CO2X(10)    
      REAL        CO2Y(10)    
      REAL        CO2         
      REAL        CSD1        
      REAL        CSD2        
      REAL        CUMDTTEG      
      REAL        CURV
      REAL        DLAYR(NL)   
      REAL        DS(NL)           
!     INTEGER     DOY         
      REAL        DTT         
      REAL        DUMMY           
      INTEGER     DYNAMIC     
      REAL        EARS        
      REAL        EARWT       
      CHARACTER*6     ECONO 
      INTEGER     EMAT        
      REAL        EOP
      REAL        EP1
      INTEGER         ERR 
      CHARACTER*6     ERRKEY          
      PARAMETER       (ERRKEY='SU_GRO')   
      CHARACTER*30    FILEIO 
      CHARACTER*12    FILEC     
      CHARACTER*92    FILECC
      CHARACTER*12    FILES
      CHARACTER*12    FILEE 
      INTEGER         FOUND  
      REAL        FSLFW
      REAL        FSLFN  
      REAL        G2    
      REAL        G3          
      REAL        GNP         
      REAL        GNUP        
      REAL        GPP         
      REAL        GPSM        
      REAL        GRAINN      
      REAL        GRF         
      REAL        GRNWT       
      REAL        GROEAR      
      REAL        GROGRN      
      REAL        GROLF       
      REAL        GRORT       
      REAL        GROSTM      
      REAL        HI
      REAL        HIO
      INTEGER     ICOLD       
      INTEGER     ICSDUR      
      CHARACTER   IDETO*1     
      REAL        IPAR
      INTEGER     ISECT
      INTEGER     ISTAGE    
      CHARACTER*1 ISWNIT
      CHARACTER*1 ISWWAT      
      CHARACTER*1 ISWPOT      
      REAL        KG2PPM(NL)   
      INTEGER     L
      REAL        LAI         
      REAL        LAIDOT
      INTEGER     LEAFNO 
      REAL        LEAFNOE 
      REAL        CumLeafSenes    !today's cumul. leaf senescence
      REAL        CumLeafSenesY   !yesterday's cumul. leaf senescence
      REAL        CumLfNSenes     !cumul. N loss in senesced leaves
      REAL HARVFRAC(2)
      INTEGER     LINC  
      REAL        LFWT        
      REAL        LFWTE
      REAL        LIFAC
      REAL        LL(NL)
      INTEGER     LNUM          
      INTEGER     LUNCRP            
      INTEGER     LUNIO 
      INTEGER     MDATE
      REAL        MAXLAI
      REAL        NDEF3       
      REAL        NFAC        
      REAL        NH4(NL)     
      INTEGER     NLAYR       
      REAL        NO3(NL)     
      INTEGER     NOUTDO      
      REAL        NPOOL       
      REAL        NPOOL1      
      REAL        NPOOL2      
      REAL        NSDR        
      REAL        NSINK       
      REAL        NSTRES      
      INTEGER     NWSD 
      REAL        P1             
      REAL        P2    
!     REAL        P3         
      REAL        P5          
      REAL        PAR         
      REAL        PARSR
      CHARACTER*80    PATHCR 
      CHARACTER*80    PATHSR
      CHARACTER*80    PATHER          
      REAL        PCARB
      REAL        PCNGRN
      REAL        PCNL 
      REAL        PCNRT     
      REAL        PCNSD
      REAL        PCNVEG      
      REAL        PCNST
      REAL        PCO2                
      REAL        PHY1
      REAL        PHY2      
      REAL        PRFTC(4)
      REAL        SLPF
      REAL        PGRORT           
      REAL        PLA    
      REAL        PLAE     
      REAL        PLAG        
      REAL        PLAS        
      REAL        PLTPOP      
      REAL        OILWT
      REAL        PORMIN
      REAL        PPLTD 
      REAL        PRFT        
      REAL        PTF         
      REAL        RANC        
      REAL        RANCE
      REAL        RCNP        
      REAL        RGFILL 
      REAL        RGFIL(4)     
      REAL        RLV(NL)     
      REAL        RLWR
      REAL        RMNC        
      REAL        RNLAB
      REAL        RNOUT
      REAL        ROOTN 
      REAL        RSGR
      REAL        RSGRT
      INTEGER     RSTAGE      
      REAL        RTDEP     
      REAL        RTWT
      REAL        RTWTE
      REAL        RTWTO        
      REAL        RUE         
      REAL        RWUEP1
      REAL        ROWSPC      
      REAL        RWUMX
      REAL        SAT(NL)     
      REAL        SATFAC      
      REAL        SDSZ
      REAL        SDWT
      CHARACTER*6     SECTION 
      REAL        SEEDNO
      REAL        SEEDRV      
      REAL        SEEDRVE
      REAL        SENLA   !, SENLAY    
      REAL        SFAC        
      REAL        SHF(NL)     
      REAL        SHELPC
      REAL        SI1(6)      
      REAL        SI2(6)      
      REAL        SI3(6)      
      REAL        SI4(6)      
      REAL        SI5(6)      
      REAL        SI6(6)      
      REAL        SKERWT      
      REAL        SLAN        
      REAL        SLA
      REAL        SLFC        
      REAL        SLFN        
      REAL        SLFT        
      REAL        SLFW        
      REAL        SRAD        
      INTEGER     STGDOY(20) 
      REAL        Stg2CLS 
      REAL        STMWTO
      REAL        STMWT   
      REAL        STMWTE    
      REAL        STOVER      
      REAL        STOVN       
      REAL        STOVWT      
      REAL        SUMDTT      
      REAL        SUMEX
      REAL        SUMP   
      REAL        SUMRL     
      REAL        FracRts(NL)   
      REAL        SW(NL) 
      REAL        SWEXF     
      REAL        SWFAC       
      REAL        SWIDOT  
      REAL        SWMAX       
      REAL        SWMIN       
      REAL        TABEX       
      REAL        TANC 
      REAL        TANCE
      REAL        TAVGD       
      REAL        TCNP        
      REAL        TEMPM       
      REAL        TFAC  
      REAL        TFACLG  ! fv 11/1/2020      
      REAL        TI         
      REAL        TLNO        
      REAL        TMAX        
      REAL        TMNC        
      REAL        TNLAB
      REAL        TMIN        
      REAL        TOPWT
      REAL        TOTNUP      
      REAL        TRNU    
      REAL        TRWUP
      REAL        TSEN
      REAL        TSS(NL) 
      REAL        TTE   
      REAL        TURFAC      
      REAL        UNH4(NL)    
      REAL        UNO3(NL)    
      REAL        VMNC        
      REAL        VANC  
      CHARACTER*6     VARNO          
      CHARACTER*16    VRNAME  
      REAL        VSTAGE      
      REAL        WLIDOT
      REAL        WRIDOT
      REAL        WSIDOT 
      REAL        WTLF
      REAL        WTNCAN      
      REAL        WTNLF       
      REAL        WTNSD       
      REAL        WTNST       
      REAL        WTNUP       
      REAL        WTNVEG      
      REAL        XANC        
      REAL        XLAI
      REAL        XHLAI      
      REAL        XLFWT       
      REAL        XGNP        
      REAL        XN          
      REAL        XNF      
      REAL        XNTI        
      REAL        XSTAGE           
      REAL        YIELD         
      REAL        PODWT   
      INTEGER     YRDOY !YR,    
      REAL        OILPC,TRG,XHY
!     Added to send messages to WARNING.OUT
      CHARACTER*78 MESSAGE(10)

!     CHP added for P model 9/5/2004
      CHARACTER*1 ISWPHO      
      INTEGER     YRPLT
      REAL        SPi_AVAIL(NL), PUptake(NL)    !, RWU(NL)
!      REAL        SPi_Labile(NL)
      REAL        FSLFP, PStres1
      REAL        PStres2, SeedFrac, VegFrac, SLFP      
      REAL PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed
!      REAL        CPSD1, CPSD2, SI5(6), SI6(6)  
!      real        PCPVEG, N2P !P conc in veg., N:P ratio
      
!     Transfer mass from stem to ear -- need to transfer P also
      REAL Stem2Ear

!     Added 04/19/07 US/CHP - Optional species coefficients for N conc
      REAL CTCNP1, CTCNP2

!     K model
      REAL, DIMENSION(NL) :: KUptake, SKi_Avail
      REAL KSTRES
      
      
!     FV added for SUOIL
      REAL ABIOMS,APLA,EMBWT,EMBWTE,GRFACTOR,HEADWT
      REAL PERWT,PERWTE,PLAMX,POTGROPER
      REAL POTHEADWT,PPP,PSKER,RI1,RM,GRNWTE
      REAL BIOMAS_R,C1,CDEMAND,CDGR,CPOOL1,CPOOL2
      REAL DM,DSLAN1,DSLAN2,DSLANW,ELO,ELOFT,EMBN,ENP,EXCESS
      REAL FACHN,FACLN,FACPOOL,FACSN,FCP,FCP2,FPOOL1,FPOOL2
      REAL FRCARB,FSINK1,FSINK2,GF1,GLFWT,GPLA,GROEMB,GROPER
      REAL GROHEAD,HWMAX,HWMIN,K2,LER,MAXGROLF !,INCPLA
      REAL MAXGROSTM,MAXLA,MINGROLF,NPL1H,NPL1S,NPL1L
      REAL NPL2L,NPL2R,NSINK1,NSINK2,O1,OIL,OILFRAC,OILINC
      REAL P3P,P9,PDWIH,PDWIL,PDWIS,PEPE,PERN,PHY,PO,PR,QD,QN
      REAL RFR,RI,RONL,SDN,SLAMAX,SLAMIN
      REAL SLAN1,SLAN2,SLAN22,SLAX,SLAY,SLFWT,SLOPEPE,SPLA,SUMDT8
      REAL TLNOI,TTMP,WLAN2,WWWW,X,XCUMPH,XHANC,XHCNP,XHEADN
      REAL XHMNC,XI,XLANC,XLAY,XLCNP,XLEAFN,XLMNC,XLN
      REAL XNGLF,XNSLF,XPEPE,XRAT,XSANC,XSCNP,XSMNC,XSTEMN
      REAL XXX,YLNOI,YRAT,YYY,Z,ZLNOI,ZZZ,C2,CPOOL,CUMDTT
      REAL OILPERC,PNP,SENRATE,TMFAC1(10),SGRO(25)
      REAL ALF,ALF1,GRAINN1,GRAINNE
      REAL PS,KCAN,KEP,CPOOL3,pdwi
      
      INTEGER IDURP,I,SENTIME,SENCODE
    
       
      TYPE (ResidueType) SENESCE 
      TYPE (SwitchType)  ISWITCH


      
!----------------------------------------------------------------------
!     CHP 3/31/2006
!     Proportion of lignin in STOVER and Roots
      REAL PLIGLF, PLIGRT

      TTE  = 350.0
      PHY1 =  39.0
      PHY2 =  24.0
      DO I = 1, 8
         TMFAC1(I) = 0.931 + 0.114*I-0.0703*I**2+0.0053*I**3
      END DO

!----------------------------------------------------------------------
!                     DYNAMIC = RUNINIT
!----------------------------------------------------------------------

      IF(DYNAMIC.EQ.RUNINIT.OR.DYNAMIC.EQ.SEASINIT) THEN

        CALL GETLUN('OUTO', NOUTDO)
        ISWNIT = ISWITCH % ISWNIT
        ISWPOT = ISWITCH % ISWPOT
        ISWPHO = ISWITCH % ISWPHO
        ISWWAT = ISWITCH % ISWWAT
        IDETO  = ISWITCH % IDETO

        !-------------------------------------------------------
        !     Read input file name (ie. DSSAT45.INP) and path
        !-------------------------------------------------------
        CALL GETLUN('FILEIO', LUNIO)
        OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)  
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
        REWIND (LUNIO)
        READ(LUNIO,50,IOSTAT=ERR) FILES, PATHSR; LNUM = 7
   50   FORMAT(//////,15X,A12,1X,A80)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
        READ(LUNIO,51,IOSTAT=ERR) FILEE, PATHER; LNUM = LNUM + 1   
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
        READ(LUNIO,51,IOSTAT=ERR) FILEC, PATHCR; LNUM = LNUM + 1 
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
   51   FORMAT(15X,A12,1X,A80)
        !------------------------------------------------------
        !   Read Planting Details Section
        !------------------------------------------------------
        SECTION = '*PLANT'
        CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILEIO, LNUM)
        ELSE
          READ(LUNIO,60,IOSTAT=ERR) PLTPOP,ROWSPC ; LNUM = LNUM + 1
   60     FORMAT(25X,F5.2,13X,F5.2,7X,F5.2)
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
        ENDIF
!     -----------------------------------------------------------------
!       Read crop cultivar coefficients
!     -----------------------------------------------------------------
        SECTION = '*CULTI'
        CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILEIO, LNUM)
        ELSE
          READ (LUNIO,1800,IOSTAT=ERR) VARNO,VRNAME,ECONO,
     %                  P1,P2,P5,G2,G3,O1 

            
1800      FORMAT (A6,1X,A16,1X,A6,1X,F5.1,1X,F5.2,1X,F5.1,1X,F5.0,
     &        1X,F5.2,1X,F5.2)         
   
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
        ENDIF

        CLOSE(LUNIO)

!       ************************************************************
!       ************************************************************
! 
!       READ SPECIES FILE
!       ************************************************************
!       ************************************************************


        FILECC =  TRIM(PATHSR) // FILES
        CALL GETLUN('FILEC', LUNCRP)
        OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)

!        ----------------------------------------------------------------
!               Find and Read TEMPERATURE Section
!        ----------------------------------------------------------------

        SECTION = '*TEMPE'
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(7X,4(1X,F5.2))',IOSTAT=ERR)
     &    PRFTC(1),PRFTC(2),PRFTC(3),PRFTC(4)
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(7X,4(1X,F5.2))',IOSTAT=ERR) RGFIL(1),
     &    RGFIL(2),RGFIL(3),RGFIL(4)
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF
        REWIND(LUNCRP)
        !---------------------------------------------------------------
        !         Find and Read PHOTOSYNTHESIS section
        !---------------------------------------------------------------
        SECTION = '*PHOTO'
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(8X,F6.3)',IOSTAT=ERR) PARSR
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(7X,10(1X,F5.0))',IOSTAT=ERR) CO2X(1),CO2X(2),
     &    CO2X(3), CO2X(4),CO2X(5), CO2X(6),CO2X(7),CO2X(8),
     &    CO2X(9),CO2X(10)
          IF (ERR.NE.0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(7X,10(1X,F5.2))',IOSTAT=ERR) CO2Y(1),CO2Y(2),
     &    CO2Y(3), CO2Y(4),CO2Y(5), CO2Y(6),CO2Y(7),CO2Y(8),
     &    CO2Y(9),CO2Y(10)
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        ENDIF
        REWIND(LUNCRP)
!        ----------------------------------------------------------------
!                Find and Read Stress Response
!        ----------------------------------------------------------------
        SECTION = '*STRES'
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(9X,F6.3)',IOSTAT=ERR) FSLFW
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(9X,F6.3)',IOSTAT=ERR) FSLFN
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          IF (ISWPHO .NE. 'N') THEN
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            READ(C80,'(9X,F6.3)',IOSTAT=ERR) FSLFP
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
          ENDIF
        ENDIF
        REWIND(LUNCRP)
!        ----------------------------------------------------------------
!                Find and Read Seed Growth Parameters
!        ----------------------------------------------------------------
        SECTION = '*SEED '
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILECC, LNUM)
        ELSE

          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(8X,F6.4)',IOSTAT=ERR) SDSZ
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(8X,F6.3)',IOSTAT=ERR) RSGR
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(8X,F6.3)',IOSTAT=ERR) RSGRT
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(8X,F6.3)',IOSTAT=ERR) CARBOT
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        ENDIF    
        REWIND(LUNCRP)
!        ----------------------------------------------------------------
!                Find and Read Emergence Initial Conditions
!        ----------------------------------------------------------------
        SECTION = '*EMERG'
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(9X,F6.3)',IOSTAT=ERR) STMWTE
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(9X,F6.3)',IOSTAT=ERR) RTWTE
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(9X,F6.3)',IOSTAT=ERR) LFWTE
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(9X,F6.3)',IOSTAT=ERR) SEEDRVE
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(9X,F6.3)',IOSTAT=ERR) LEAFNOE
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(9X,F6.3)',IOSTAT=ERR) PLAE
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        ENDIF
        REWIND(LUNCRP)

!        ----------------------------------------------------------------
!                Find and Read Plant Nitrogen Parameters
!        ----------------------------------------------------------------
        SECTION = '*NITRO'
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(9X,F8.4)',IOSTAT=ERR) TMNC
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(9X,F8.3)',IOSTAT=ERR) TANCE
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(9X,F8.4)',IOSTAT=ERR) RCNP
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(9X,F8.3)',IOSTAT=ERR) RANCE
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

! CHP/US 04/19/2007 Coefficients pulled out to species file.
          xstage=real(istage)
 
!         Default values: CTCNP1 = 1.52; CTCNP2 = 0.160
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(9X,F8.3)',IOSTAT=ERR) CTCNP1
!         If error reading either value, use defaults
          IF (ERR /= 0 .OR. ISECT /= 1 .OR. CTCNP1 < 1.E-6) THEN
            CTCNP1 = 1.52
            CTCNP2 = 0.160
          ELSE
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            READ(C80,'(9X,F8.3)',IOSTAT=ERR) CTCNP2
            IF (ERR /= 0 .OR. ISECT /= 1 .OR. CTCNP2 < 1.E-6) THEN 
              CTCNP1 = 1.52
              CTCNP2 = 0.160
            ENDIF
          ENDIF
        ENDIF
        REWIND(LUNCRP)
!        ----------------------------------------------------------------
!                Find and Read Root parameters
!        ----------------------------------------------------------------
        SECTION = '*ROOT '
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(9X,F6.3)',IOSTAT=ERR) PORMIN
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(9X,F6.3)',IOSTAT=ERR) RWUMX
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(9X,F6.3)',IOSTAT=ERR) RLWR
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(9X,F6.3)',IOSTAT=ERR) RWUEP1
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF

        CLOSE (LUNCRP)
!**     Initialize variables

        CALL SenLig_Ceres(PLIGLF=PLIGLF, PLIGRT=PLIGRT)

!       JIL 08/01/2006 parameters for ear growth 
!**     Read in SPE file?


        APTNUP = 0.0
        AREALF = 0.0
        ASMDOT = 0.0  
        BIOMAS = 0.0
        CANHT  = 0                
!**
!       set to 1.5 m, fv 11/1/2020
        CANHT_POT = 1.5              
!**
        CANNAA = 0.0
        CANWAA = 0.0
        CANWH  = 0  
        CARBO  = 0.0
!       CSD1   = 0.0
!       CSD2   = 0.0
        CMAT   = 0
        CUMDTTEG=0.0
        CumLeafSenes = 0.0
        CumLeafSenesY = 0.0
        CumLfNSenes = 0.0
        CUMPH  = 0
        XCUMPH = 0.0
!       DM     = 0.0
        DUMMY  = 0.0
        EARS   = 0.0 
        EARWT  = 0.0
        EMAT   = 0
        EP1    = 0.0
        GRAINN = 0.0
        EMBN = 0.0
        PERN = 0.0
        GRF    = 0.0
        GNP = 0.0
        GNUP   = 0.0
        GRNWT  = 0.0
        GRNWTE = 0.0
        PERWTE = 0.0
        EMBWTE = 0.0
        PERWT = 0.0
        EMBWT = 0.0
        GROEAR = 0.0
        GROGRN = 0.0
        GPSM   = 0.0
        GROLF  = 0.0
        GRORT  = 0.0
        GROSTM = 0.0
        HI     = 0.0
        HIO    = 0.0
        ICOLD  = 0
        ICSDUR = 0
        IPAR   = 0.0
        LAI    = 0.0
        LAIDOT = 0.0
        LEAFNO = 0
        LFWT   = 0.0
        LIFAC  = 0.0
        MAXLAI = 0.0
        NPOOL  = 0.0
        NPOOL1 = 0.0
        NPOOL2 = 0.0
        NSDR   = 0.0
        NSINK  = 0.0
        PCNVEG = 0.0
        PCO2   = 0.0
        PCNGRN = 0.0
        PCNL   = 0.0
        PCNRT  = 0.0
        PCNSD  = 0.0
        PCNST  = 0.0
        PLA = 0.0
        PLAG   = 0.0
        PLAS = 0.0
        PRFT   = 0.0
        PPLTD  = 0.0  
        PTF    = 0.0
        RANC   = 0.0
        RLV = 0.0     !CHP 1/3/2013
        RMNC   = 0.0
        RNLAB  = 0.0
        ROOTN  = 0.0
        RSTAGE = 0
        RTWTO  = 0.0
        RTWT   = 0.0
        SATFAC = 0.0
        SDWT = 0.0
        SEEDNO = 0.0
        SEEDRV = 0.0
        SENLA  = 0.0
        SENESCE % ResWt  = 0.0
        SENESCE % ResLig = 0.0
        SENESCE % ResE   = 0.0
        SHELPC = 0.0
        SLA    = 0.0
        SLAN = 0.0
        SLAN1 = 0.0
        SLAN2 = 0.0
        SKERWT = 0.0
       
        SLFC   = 0.0  
        SLFN   = 0.0
        SLFP   = 0.0
        SLFT   = 0.0
        SLFW   = 0.0
        SI1 = 0.0
        SI2 = 0.0
        SI3 = 0.0
        SI4 = 0.0
        SI5 = 0.0
        SI6 = 0.0
        Stem2Ear = 0.0
        STMWT  = 0.0
        STMWTO = 0.0
        STOVER = 0.0
        STOVN  = 0.0
        STOVWT = 0.
        SUMP   = 0.0
        SUMRL  = 0.0
        SUMEX  = 0.0
        SWEXF  = 0.0
        SWFAC  = 1.0
        SWIDOT = 0.0 
        SWMAX = 0.0
        SWMIN = 0.0
        TANC   = TANCE
        TAVGD  = 0.0
        TCNP   = 0.0
        TEMPM  = 0.0
        TFAC   = 0.0
        TFACLG = 0.0
        TI     = 0.0
        TLNO   = 0.0
        XN = 0.0
        TNLAB  = 0.0
        TOPWT  = 0.0
        TOTNUP = 0.0
        TRNU   = 0.0
        XLEAFN = 0.0
        HEADWT = 0.0
        OIL=0.0
        IDURP = 0
        TSS = 0.0
        XLN = 0.0
 
        TURFAC = 1.0

        UNH4   = 0.0      !CHP 1/3/2013

        UNO3   = 0.0      !CHP 1/3/2013

        VANC   = 0.0
        VMNC   = 0.0 
  
        VSTAGE = 0.0

        WLIDOT = 0.0 
        WRIDOT = 0.0  
        WSIDOT = 0.0  

        WTLF   = 0.0
        WTNCAN = 0.0
        WTNLF  = 0.0
        WTNSD  = 0.0
        WTNST  = 0.0
        WTNUP  = 0.0
        WTNVEG = 0.0
  
        XANC   = 0.0
        XGNP   = 0.0
  
        XHLAI  = 0.0
 
        XLAI   = 0.0

        XLFWT  = 0.0

        XN = 0.0
        XNF    = 0.0
        XNTI   = 0.0
        YIELD  = 0.0
       XNGLF = 0.0
       XSTEMN = 0.0
       XHEADN = 0.0
       XNSLF = 0.0
       XLEAFN = 0.0
       SENTIME = 0
       SENCODE = 0
       OIL = 0.0
       OILWT = 0.0
        
        IF (ISWNIT .NE. 'N') THEN

          CALL SU_NFACTO(DYNAMIC,TANC,TCNP,TMNC,
     %    xlanc,xlcnp,xlmnc,AGEFAC,NDEF3,NFAC,NSTRES)         
        ELSE
          AGEFAC = 1.0
          NSTRES = 1.0
          NDEF3 = 1.0
          NFAC = 1.0
        ENDIF
        
        CALL SU_NUPTAK(
     %    RANC, ROOTN,RTWT,TANC,STOVN,STOVWT,TRNU,NLAYR,
     %    RLV,NO3,NH4,UNO3,UNH4,
     %    RCNP,PGRORT,PLTPOP,SW,LL,SAT,DLAYR,
     %    SHF,PTF, SENESCE, KG2PPM, PLIGRT,
     %    XLCNP,XSCNP,XHCNP,GROPER,GROEMB,
     %    PDWIL,PDWIH,PDWIS,XNGLF,XSTEMN,XHEADN,
     %    GLFWT,STMWT,HEADWT,PNP,ENP,PERWT,EMBWT,
     %    XLEAFN,XLANC,XNSLF)
        
        CALL SU_KUPTAK(
     &  ISWPOT, NLAYR, SKi_Avail, UNH4, UNO3,        !Input
     &  KUPTAKE, KSTRES)                             !Output

        PODWT = SDWT     ! added as no pods exist
        
        CALL P_Ceres (DYNAMIC, ISWPHO,                    !Input
     &  CumLeafSenes, DLAYR, DS, FILECC, MDATE, NLAYR,  !Input
     &        PCNVEG, PLTPOP, PODWT, RLV, RTDEP, RTWTO,       !Input
     &  SDWT, SWIDOT, SeedFrac, SPi_AVAIL, Stem2Ear,    !Input
     &  STMWTO, VegFrac, WLIDOT, WRIDOT, WSIDOT,        !Input
     &  WTLF, YRPLT,                                    !Input
     &  SENESCE,                                        !I/O
     &  PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, !Output
     &  PStres1, PStres2, PUptake, FracRts)             !Output

!-----------------------------------------------------------------------  
!-----------------------------------------------------------------------
!
!                     DYNAMIC = RATE
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!      ELSEIF(DYNAMIC.EQ.RATE) THEN

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!                     DYNAMIC = INTEGR
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      ELSEIF(DYNAMIC.EQ.INTEGR) THEN   
   
        SENESCE % ResWt  = 0.0
        SENESCE % ResLig = 0.0
        SENESCE % ResE   = 0.0
        KUPTAKE = 0.0

!       Keep yesterdays cumulative senescence 
        CumLeafSenesY = CumLeafSenes

!       Need to set these at the beg. of each new growth stage. From phasei
!       Initialize variables at the beginning of a new phenological stage
!       This code was moved from phenol or phasei in Generic CERES v 3.9

        IF (YRDOY == STGDOY(1) .OR. YRDOY == STGDOY(2) .OR. 
     &  YRDOY == STGDOY(5) .OR. YRDOY == STGDOY(6) .OR. 
     &  YRDOY == STGDOY(7) .OR. YRDOY == STGDOY(8)) THEN 
          ICSDUR = 1
        ENDIF


        IF(YRDOY.EQ.STGDOY(2)) THEN

!         Assuming  1 head/plant. we use EARS for number of heads per m2
          EARS   = PLTPOP  
          IDURP = 0
          SUMP = 0
          DO I = 1, 12
            SGRO(I) = 0.0
          END DO
          SLAN1  = 0.0  
        ENDIF
          

 
        IF(YRDOY.EQ.STGDOY(3)) THEN

          SWMAX  = 1.25*STMWT   
          SWMIN=STMWT*.80     
          VANC   = TANC       
          VMNC   = TMNC       
          EMAT   = 0
          ICSDUR = 1
          CANNAA = STOVN*PLTPOP
          CANWAA = BIOMAS
 

          PSKER  = SUMP / IDURP
          PSKER  = AMAX1 (PSKER,0.1)           ! Set PSKER to min of 0.1
          !
          ! Include calculations to correct RUE after anthesis
          ! mantainance respiration
          !
          RM  = (BIOMAS+RTWT*PLTPOP)*0.008*0.729
          IF(PLTPOP .GT. 0.0) THEN
            RI1 = 1.0 + RM/PSKER/PLTPOP      
          ELSE
            RI1 = 0.0
          ENDIF

          !
          ! Pericarp number calculated according to Soriano
          ! FV - 12/12/97
          !
          PPP = 1250.0 + (GPLA-750.0)*750.0/1500.0
          PPP = AMIN1 (PPP,G2)

          ZZZ       = AMIN1 (1.0,HEADWT/POTHEADWT)
          GRFACTOR  = 0.6 + 0.4 * ZZZ
          GRFACTOR  = AMIN1 (GRFACTOR,1.0)
          
          PERWT     = PPP * 0.002            ! Pericarp starts with 2 mg
          IF (PERWT.GT.0.5*HEADWT) THEN
              PERWT=0.5*HEADWT
          ENDIF
          HEADWT=HEADWT-PERWT
          ALF       = 0.22
          ALF1      = (ALF*G3/24.*(P5-170.)-2.*(1.-ALF))/270./(1.-ALF)
          POTGROPER = 24.0*ALF1*PPP/1000.0

          XRAT    = XNGLF / GLFWT
          YRAT    = (0.009-0.0875*XRAT)/0.9125
          IF(PLTPOP .GT. 0.0) THEN
            SLOPEPE = LAI*10E3/PLTPOP/(XNGLF-YRAT*GLFWT) ! SUN, HV
          ELSE
            SLOPEPE = 0.0
          ENDIF
          
          XPEPE   = 0.50 * (XNGLF - YRAT*GLFWT)
          GPP    = 0.0
          MAXLAI = PLAMX*PLTPOP/10000.0
          IF(PLTPOP .GT. 0.0) THEN
            APLA   = LAI*10000.0/PLTPOP
          ELSE
            APLA   = 0.0
          ENDIF
          ABIOMS = BIOMAS
          OILWT = 0.0
          OIL = 0.0

        ENDIF
        IF(YRDOY.EQ.STGDOY(4)) THEN
          OIL    = GRNWT * 0.025
          GPP    = 0.0
          HWMAX  = 2.20 * HEADWT
          HWMIN  = 1.00 * HEADWT
          VANC   = TANC
          VMNC   = TMNC

 
        ENDIF
        
        IF(YRDOY.EQ.STGDOY(9)) THEN
          !Emergence
          STMWT   = STMWTE          
          RTWT    = RTWTE        
          LFWT    = LFWTE   
          GLFWT   = LFWT
          SLFWT   = 0.0
          GPP=0.      
          STOVWT  = LFWT+STMWT     
          SEEDRV  = SEEDRVE
          BIOMAS = STOVWT*PLTPOP
          LEAFNO = 1   
          XLN=1   
          PLA     = PLAE
          GPLA = PLA
          SPLA = 0.0
          SENLA  = 0.0
          LAI    = PLTPOP*PLA*0.0001 
          CUMPH   = 0
          XCUMPH = 0.0
          CUMPH = 0.0
          TLNO     =  (2.0+(P1+30.0+30.0*P2)/14.0)
          NSTRES = 1.0
          AGEFAC = 1.0
          NDEF3  = 1.0
          IF (ISWNIT .NE. 'N') THEN
            GRAINN = 0.000
            TANC = TANCE
            XLANC = .062
            XSANC = .0444
            RANC = RANCE   !Initialize root N (fraction) JIL
            ROOTN = RANCE   * RTWT
            STOVN = STOVWT * TANC
            XLEAFN = XLANC*LFWT
            XNGLF  = XLEAFN
            XSTEMN = XSANC * STMWT
            XHEADN = 0.0
            XNSLF = 0.0
          ENDIF


          WTLF = LFWT * PLTPOP      !Leaf weight, g/m2
          STMWTO = STMWT * PLTPOP   !Stem weight, g/m2
          RTWTO = RTWT * PLTPOP     !Root weight, g/m2
          GROHEAD = 0.0
          HEADWT = 0.0
          ! POD WEIGHT IS SET TO SEED WEIGHT
          PODWT = SDWT
 
          CALL P_Ceres (EMERG, ISWPHO,                      !Input
     &          CumLeafSenes, DLAYR, DS, FILECC, MDATE, NLAYR,  !Input
     &          PCNVEG, PLTPOP, PODWT, RLV, RTDEP, RTWTO,       !Input
     &          SDWT, SWIDOT, SeedFrac, SPi_AVAIL, Stem2Ear,    !Input
     &          STMWTO, VegFrac, WLIDOT, WRIDOT, WSIDOT,        !Input
     &          WTLF, YRPLT,                                    !Input
     &          SENESCE,                                        !I/O
     &          PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, !Output
     &          PStres1, PStres2, PUptake, FracRts)             !Output
        ENDIF
!       CHP 7/29/2008
        IF (MDATE == YRDOY) THEN
          RETURN
        ENDIF

        !-------------------------------------------------------------
        !           Begin Rate and Integration Calculations
        !-------------------------------------------------------------
      
        TEMPM = (TMAX + TMIN)*0.5   !Mean air temperature, C

        !-------------------------------------------------------------
        !           Compute critical nitrogen contents
        !-------------------------------------------------------------
! CHP/US 04/19/2007 Coefficients pulled out to species file.

        ! Calculate critical and minimum N concentrations

        XLCNP  = (02.94 * EXP(-0.326*XSTAGE) + 3.26)/100.0
        XLMNC  = XLCNP - 0.025
        XSCNP  = (03.29 * EXP(-0.516*XSTAGE) + 1.25)/100.0
        XSMNC  = XSCNP - 0.0095
        RCNP   = (03.61 * EXP(-0.521*XSTAGE) + 1.05)/100.0
        RMNC   = RCNP  - 0.0062
        XHCNP  = (21.37 * EXP(-0.600*XSTAGE) + 1.60)/100.0
        XHMNC  = XHCNP - 0.00895
      !
        ! Calculate total N in plant parts
      !
        IF(STOVWT-SLFWT .GT. 0.0) THEN
          TCNP=(XLCNP*GLFWT+XSCNP*STMWT+XHCNP*HEADWT)/(STOVWT-SLFWT)
          TMNC=(XLMNC*GLFWT+XSMNC*STMWT+XHMNC*HEADWT)/(STOVWT-SLFWT)
        ELSE
          TCNP=0.0
          TMNC=0.0
        ENDIF

!        -------------------------------------------------------------
!                Compute Nitrogen Stress Factors 
!        -------------------------------------------------------------
        IF (ISWNIT .NE. 'N' .AND. ISTAGE .LT. 7) THEN
          CALL SU_NFACTO(DYNAMIC,                 !Control
     %    TANC,TCNP,TMNC,xlanc,xlcnp,xlmnc,       !Inputs
     %    AGEFAC,NDEF3,NFAC,NSTRES)               !Outputs
        ELSE
          AGEFAC = 1.0
          NSTRES = 1.0
          NDEF3 = 1.0
          NFAC = 1.0
        ENDIF
!          -------------------------------------------------------------
!                Compute Water Stress Factors       
!           ------------------------------------------------------------
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

!        -------------------------------------------------------------
!              Compute Water Saturation Factors       
!         ------------------------------------------------------------

        SATFAC = 0.0    
        SUMEX = 0.0
        SUMRL = 0.0
      
          DO L = 1,NLAYR

!            ------------------------------------------------------------
!            PORMIN = Minimum pore space required for supplying oxygen to 
!                     roots for optimum growth and function    
!            TSS(L) = Number of days soil layer L has been saturated 
!                     above PORMIN
!            ------------------------------------------------------------
            IF ((SAT(L)-SW(L)) .GE. PORMIN) THEN
              TSS(L) = 0.
            ELSE
              TSS(L) = TSS(L) + 1.
            ENDIF
!            ------------------------------------------------------------
!             Delay of 2 days after soil layer is saturated before root
!             water uptake is affected
!            ------------------------------------------------------------
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

!          -------------------------------------------------------------
!                          Daily Photosynthesis Rate
!           ------------------------------------------------------------
!          Compare with weather module: PAR = SRAD * 2.0  chp
          PAR = SRAD*PARSR    !PAR local variable        
          
         


          QN  = 1.0 - EXP(-0.86*LAI)
          QD  = 2.0*QN/(1.0 + QN)

          IF (LAI .GT. 0.001) THEN
            K2 = -ALOG(1.0-QD)/LAI
          ELSE
            K2 = 3.0
          ENDIF
          K2 = AMIN1 (K2,3.0)

          IF (ISTAGE .GE. 4) THEN
            K2 = 1.0
          ENDIF

         
          RUE  = 1.4 + 1.8*(1.0-EXP(-0.5*LAI))
          RUE  = AMIN1 (RUE,3.0)
          RI  = PAR*(1.0-EXP(-K2*LAI))

          KCAN=K2
C     recalculated KEP taken into account that K(FR)=0.5 K(PAR)
          IF (LAI.GT.0.0) THEN
            KEP=-LOG(0.5*EXP(-KCAN*LAI)+.5*EXP(-0.5*KCAN*LAI))/LAI
          ELSE
            KEP=0.0
          ENDIF  
          !
          ! RUE changes when oil starts to accumulate
          !
          IF (ISTAGE .GT. 3) THEN
            IF (GPP .EQ. 0.0) THEN
              GF1 = 0.8 - SUMDTT/140.0*0.2
              GF1 = AMAX1 (GF1,0.6)
              C1  = 0.8
              C2  = GF1
            ELSE
              GF1 = 0.6 - (SUMDTT-230.0)/140.0*0.2
              GF1 = AMAX1 (GF1,0.4)
              C1  = 0.8
              C2  = GF1
            ENDIF
          ENDIF
          IF (ISTAGE .LT. 4 .AND. PLTPOP .GT. 0.0) THEN
            PCARB = RUE * RI/PLTPOP
          ELSE
            IF(PLTPOP .GT. 0.0) THEN
              IF (RI .GT. 0) THEN
                PCARB = (RI*RI1*C2/C1*RUE-RM)/PLTPOP

              ELSE
                PCARB =-RM/PLTPOP
              ENDIF
            ELSE
              PCARB = 0.0
            ENDIF
          ENDIF
c          write(*,*)istage,lai,pcarb,pco2,ri1,c2,c1,rm
c         PCARB = AMAX1 (PCARB,0.0)

          PCO2  = TABEX (CO2Y,CO2X,CO2,10)
          PCARB = PCARB*PCO2
          TAVGD = 0.25*TMIN+0.75*TMAX

          PRFT = CURV('LIN',PRFTC(1),PRFTC(2),PRFTC(3),PRFTC(4),TAVGD)
          PRFT  = AMAX1 (PRFT,0.0)
          PRFT = MIN(PRFT,1.0)
!************************************************************************
!************************************************************************
          RFR  = 1.2*EXP(-0.5*K2*LAI)
          IF (RFR .GT. 0.5) THEN
            RFR = 1.0
          ELSE
            RFR = 1.0/0.5*RFR
          ENDIF


!** WDB   Water logging reduces root water uptake and creates water stress
!** WDB   through SWFAC. Thus, we do not need to also create a reduction
!** WDB   using SATFAC

!     CHP 9/5/04 Added P stress
          CARBO = PCARB*AMIN1 (PRFT,SWFAC,NSTRES, PStres1,KSTRES)*SLPF

          !Reduce CARBO for assimilate pest damage
          CARBO = CARBO - ASMDOT
!         CARBO = MAX(CARBO,0.0)

             ! Calculate total number of leaves appeared ....  SUN

          IF (LEAFNO .LT. 6) THEN
            PHY = PHY1
              ELSE
            PHY = PHY2
          ENDIF

          XLN      = XLN + DTT/PHY

          IF (XLN .GT. TLNO) THEN
            LEAFNO = INT(TLNO)
            XN=LEAFNO
          ELSE
            LEAFNO = INT(XLN)
            XN=XLN
          ENDIF
      !
      !   Calculate number of expanded leaves  .... SUN
      !
           IF (CUMPH .LT. 6) THEN
             XHY = PHY1
           ELSE
             XHY = PHY2
           ENDIF

          IF (CUMPH .LT. TLNO) THEN
            IF ((CUMDTT-P9) .LT. (TTE+PHY1)) THEN
              CUMPH  = 0
            ELSE
              XCUMPH = XCUMPH + DTT/XHY
              CUMPH  = INT(XCUMPH)
            ENDIF
          ELSE
            CUMPH = INT(TLNO)
          ENDIF


      !   Run the code needed for the particular stage the plant is in
      !
          SLAX   = 1.0  / SQRT (2.778E-5-2.007E-7*LFWT)
          SLAMAX = 1.40 * SLAX
          SLAMIN = 1.00 * SLAX

C         CALCULATE MAXIMUM LEAF AREA GROWTH
          PLAG =   0.00

          IF (TEMPM .LT. 4.0 .OR. TEMPM .GT. 40.0) THEN
            TFACLG = 0.0
          ELSE
            IF (TEMPM .LT. 24.0) THEN
              TFACLG = (TEMPM - 4.0)/20.0
            ELSE
              TFACLG = 1.0 - (TEMPM - 24.0)/16.0
            ENDIF
          ENDIF      

        TLNOI = IFIX (1.54 + 0.61*TLNO)
        ZLNOI = REAL (TLNOI)
        YLNOI = 150 + (ZLNOI - 6.0) *74.0

        DO I = CUMPH + 1, LEAFNO
          Z  = REAL(I)
          X  = Z/TLNO
          XI = (Z-1)/TLNO
          IF (Z .LE. 6.0) THEN
            MAXLA = Z * 25.0
          ELSE
            IF (Z .LE. ZLNOI) THEN
              MAXLA = 150.0 + (Z - 6.0) * 74.0
            ELSE
              MAXLA = YLNOI - 117.0 * (Z - ZLNOI)
            ENDIF
          ENDIF
          IF (MAXLA .LT. 0.0) THEN
            MAXLA = 1.0
          ENDIF

          LER   = MAXLA * TFACLG / 18.0
          PLAG  = PLAG + LER
        ENDDO
        
        IF (ISTAGE. GE. 1 .AND. ISTAGE .LE. 3) THEN
          PLAG = RFR*PLAG 
        ELSE
          PLAG=0.
        ENDIF
         IF (TURFAC .LT. 0.9) THEN
          SLAMAX = SLAMIN
        ENDIF

        MAXGROLF = PLAG / SLAMIN
        MINGROLF = PLAG / SLAMAX
        IF (ISTAGE.GT.2) THEN
          trg = 0.0
          DO I = 1, 8
            TTMP = TMIN + TMFAC1(I)*(TMAX-TMIN)
            ELO  = TTMP
            IF (ELO .LT. 4.0 .OR. ELO .GT. 45.0) THEN
              ELOFT = 0.0
            ELSE
              IF (ELO .LE. 17.0) THEN
                ELOFT = (ELO - 4.0)/13.0
              ELSE
                IF (ELO .LE. 31.0) THEN
                  ELOFT = 1.0
                ELSE
                  ELOFT = (45.0 - ELO)/14.0
                ENDIF
              ENDIF
            ENDIF
            trg = trg + ELOFT/8.0
          ENDDO
        ENDIF
          !-------------------------------------------------------------
          !   ISTAGE = 1 (Emergence to End of Juvenile Stage)
          !-------------------------------------------------------------
        IF (ISTAGE.EQ.1) THEN   
          DSLAN1 = 0.0
          DSLAN2 = 0.0
          GROLF = 0.0
          GROSTM = 0.0
          GROHEAD = 0.0  
          GROPER = 0.0
          GROEMB = 0.0   

      !
        ! More than 0.18 carbo to root
      !
          FRCARB   = 0.57 * CARBO

          IF (MAXGROLF .LE. FRCARB) THEN
            GROLF = MAXGROLF
            PLAG  = MAXGROLF * SLAMIN
          ELSE
            IF (MINGROLF .GT. FRCARB) THEN
              GROLF = FRCARB
              PLAG  = GROLF * SLAMAX
            ELSE
              GROLF = FRCARB
            ENDIF
          ENDIF

          GROLF  = GROLF * AMIN1 (TURFAC,AGEFAC)
          PLAG   = PLAG  * AMIN1 (TURFAC,AGEFAC)
          GROSTM = 0.245 * CARBO
          GRORT  = CARBO - GROLF - GROSTM    
          GROHEAD = 0.0
          GROPER = 0.0
          GROEMB = 0.0   

          !-------------------------------------------------------------
          !  ISTAGE = 2 (End of Juvenile to Tassel Initiation)
          !-------------------------------------------------------------

        ELSEIF (ISTAGE .EQ. 2) THEN
          DSLAN1 = 0.0
          DSLAN2 = 0.0
          GROLF = 0.0
          GROSTM = 0.0
          GROHEAD = 0.0  
          GROPER = 0.0
          GROEMB = 0.0   


          ! More than 0.18 carbo to root
          !
          FRCARB = 0.57 * CARBO
          IF (MAXGROLF .LE. FRCARB) THEN
            GROLF = MAXGROLF
            PLAG  = MAXGROLF * SLAMIN
          ELSE
            IF (MINGROLF .GT. FRCARB) THEN
              GROLF = FRCARB
              PLAG  = GROLF * SLAMAX
            ELSE
              GROLF = FRCARB
            ENDIF
          ENDIF

          GROLF  = GROLF * AMIN1 (TURFAC,AGEFAC)
          PLAG   = PLAG  * AMIN1 (TURFAC,AGEFAC)
          GROSTM = 0.245 * CARBO
          GRORT  = CARBO - GROLF - GROSTM
          GROHEAD = 0.0
          GROPER = 0.0
          GROEMB = 0.0   
!         Stage 3 accumulates leaf senescence from 0
!             Save stage 2 value for true accumulation
          Stg2CLS = CumLeafSenes

!         -------------------------------------------------------------------
!         ISTAGE=3 (Floret Initiation to First Anthesis)
!         -------------------------------------------------------------------

        ELSEIF (ISTAGE .EQ. 3) THEN   

          DSLAN1 = 0.0
          DSLAN2 = 0.0
          GROLF = 0.0
          GROSTM = 0.0
          GROHEAD = 0.0  
          GROPER = 0.0
          GROEMB = 0.0 
          SENRATE = 0.0
          DSLAN1 = 0.0
          DSLANW = 0.0  

          IF (SUMDTT .LT. 150.0) THEN
            !
            ! More than 0.18 carbo to root
            !
            FRCARB = 0.57 * CARBO
            IF (MAXGROLF .LE. FRCARB) THEN
              GROLF = MAXGROLF
              PLAG  = MAXGROLF * SLAMIN
            ELSE
              IF (MINGROLF .GT. FRCARB) THEN
                GROLF = FRCARB
                PLAG  = GROLF * SLAMAX
              ELSE
                GROLF = FRCARB
              ENDIF
            ENDIF

            GROLF  = GROLF * AMIN1 (TURFAC,AGEFAC)
            PLAG   = PLAG  * AMIN1 (TURFAC,AGEFAC)
            GROSTM = 0.245 * CARBO
            GRORT  = CARBO - GROLF - GROSTM 
                   
          ELSE
            MAXGROSTM = 0.605 * CARBO
            IF (SUMDTT .GT. (P3P-180.0) .AND. HEADWT .EQ. 0.0) THEN
              HEADWT    = 0.05  * STMWT
              POTHEADWT = 22.1
              STMWT     = STMWT - HEADWT
              
              IF (ISWNIT .EQ. 'Y') THEN
                   XHEADN = 0.0420 * HEADWT
                   XSTEMN = XSTEMN - XHEADN
              ENDIF
            ENDIF

            IF (HEADWT .GT. 0.0) THEN
              GROHEAD   = 1.71 * trg
            ELSE
              GROHEAD   = 0.0
              MAXGROSTM = trg * MAXGROSTM
            ENDIF
            FRCARB = 0.90 * CARBO - MAXGROSTM - GROHEAD

            IF (FRCARB .GT. 0.295*CARBO) THEN
              FRCARB = 0.295 * CARBO
            ENDIF

            IF (FRCARB .LE. 0.0) THEN
              FRCARB    = 0.0
              MAXGROSTM = 0.90 * CARBO - GROHEAD
              MAXGROSTM = AMAX1 (MAXGROSTM,0.0)
              GROLF     = 0.0
              PLAG      = 0.0
            ELSE
              IF (MAXGROLF .LE. FRCARB) THEN
                GROLF = MAXGROLF
                PLAG  = MAXGROLF * SLAMIN
              ELSE
                IF (MINGROLF .GT. FRCARB) THEN
                  GROLF = FRCARB
                  PLAG  = GROLF * SLAMAX
                ELSE
                  GROLF = FRCARB
                ENDIF
              ENDIF
            ENDIF

            GROSTM  = MAXGROSTM
            PLAG    = PLAG    * AMIN1 (TURFAC,AGEFAC)
            GROLF   = GROLF   * AMIN1 (TURFAC,AGEFAC)
            GROHEAD = GROHEAD * AMIN1 (TURFAC,AGEFAC)
            GROSTM  = GROSTM  * AMIN1 (SWFAC, NSTRES)
            GRORT   = CARBO - GROLF - GROSTM - GROHEAD
          !
            ! Cambios en el calculo de la senescencia
          !
            IF (SENTIME .EQ. 13) THEN
              SENCODE = 1
            ENDIF
            IF (LAI .GT. 1.2 .AND. SENCODE .EQ. 0) THEN
              SENTIME    = SENTIME + 1             
              SGRO(SENTIME) = PLAG
            ENDIF

            IF (SENCODE .EQ. 1) THEN
              WWWW= -0.0182 + 0.4147*SGRO(1)*PLTPOP/10000.0
              DO I = 1, 11
                SGRO(I) = SGRO(I+1)
              END DO
              SGRO(12) = PLAG
              SENRATE  = WWWW
            ELSE
              SENRATE  = 0.0
            ENDIF
            IF (SENRATE .GT. 0.0 .AND. PLTPOP .GT. 0.0) THEN
!             rate of senescence cm2/day/plant
              DSLAN1 = SENRATE*10000.0/PLTPOP    
            ELSE
              DSLAN1 = 0.0
            ENDIF

          ! Calculo de senescencia March/91
          !
            IF (CUMPH .GT. (TLNO-5) .AND. TURFAC .LT. 0.8) THEN
              DSLANW = 0.03 * GPLA
            ELSE
              DSLANW = 0.0
            ENDIF

!           rate of lraf senescence cm2/day/plant
            DSLAN1 = AMAX1 (DSLAN1,DSLANW)    
            SLAN1  = SLAN1 + DSLAN1
            SLAN   = SLAN1
            SLAY   = GPLA  / GLFWT     ! SLA of mean green area
            GPLA   = GPLA  - DSLAN1 
            SPLA   = SPLA  + DSLAN1
            GLFWT  = GLFWT + GROLF - DSLAN1/SLAY
            SLFWT  = SLFWT + DSLAN1/SLAY
            IF (ISWNIT .EQ. 'Y') THEN
              XRAT   = XNGLF / GLFWT
!             residual nitrogen concentration in senesced leaves
              YRAT   = (0.009 - 0.0875 * XRAT)/0.9125  
!             amount of nitrogen retranslocated from the senesced leaves to other organs ( g N/plant)
              SDN    = DSLAN1/SLAY*(XRAT - YRAT)      
              SLFWT  = SLFWT - SDN * 6.25
              XNSLF  = XNSLF + DSLAN1/SLAY*YRAT
              GLFWT  = GLFWT + SDN * 6.25
              XNGLF  = XNGLF - DSLAN1/SLAY*YRAT
!             N content in green leaves at critical level
              XXX    = GLFWT * XLCNP   

!             N is first moved from secesced leaves to green leaves and then from the latter to the stem
              IF (XNGLF .GT. XXX) THEN
!               Leaf N amount in excess of critical (g/plant)
                YYY    = XNGLF  - XXX   
!               set actual to critical amount of N in green leaves
                XNGLF  = XXX            
!               export of protein by senescence
                GLFWT  = GLFWT  - YYY * 6.25  
!               export of protein by senescence
                LFWT   = LFWT   - YYY * 6.25  
!               input of protein by senescence
                STMWT  = STMWT  + YYY * 6.25  
                XSTEMN = XSTEMN + YYY
                XLEAFN = XLEAFN - YYY
              ENDIF  
            ENDIF
          ENDIF  
          IF (SUMDTT .GT. (P3P-130.0)) THEN
            SUMP  = SUMP  + CARBO
            IDURP = IDURP + 1
          ENDIF
        

!       total leaf senescence kg/ha
        CumLeafSenes =SLFWT * PLTPOP * 10.    


!      --------------------------------------------------------------------
!         ISTAGE = 4 (Anthesis to beginning of effective grain filling period)
!      --------------------------------------------------------------------

        ELSEIF (ISTAGE .EQ. 4) THEN
          PLAG = 0.0
          DSLAN1 = 0.0
          DSLAN2 = 0.0
          GROLF = 0.0
          GROSTM = 0.0
          GROHEAD = 0.0  
          GROPER = 0.0
          GROEMB = 0.0   

          IF (CARBO .NE. 0.0) THEN
           GROPER = POTGROPER * trg* AMIN1 (TURFAC,AGEFAC)                 ! SUN, Hall
          ENDIF

          GROHEAD = 1.71 * trg                        ! SUN, Hall

          IF (GROPER .GT. CARBO) THEN
            GROPER  = CARBO
            GROHEAD = 0.0
          ELSE
            IF ((GROPER + GROHEAD) .GT. CARBO) THEN
              GROHEAD = CARBO - GROPER
            ENDIF
          ENDIF   

          EXCESS = CARBO - GROPER - GROHEAD
          EXCESS = AMAX1 (EXCESS,0.0)

          IF (STMWT .LT. SWMAX) THEN
            GROSTM = EXCESS
            GRORT  = 0.0
          ELSE
            GRORT  = EXCESS
            GROSTM = 0.0
          ENDIF 
 
          GROHEAD = GROHEAD * AMIN1 (TURFAC,AGEFAC)
          GROPER  = GROPER  * (0.7 + 0.3*SWFAC)
          GROSTM  = GROSTM  * AMIN1 (SWFAC,NSTRES)
          GRORT   = CARBO - GROHEAD - GROPER - GROSTM
          IF (ISWNIT .EQ. 'Y') THEN
            ! Grain n allowed to vary between .02 and .038.
            ! High temp., low soil water, and high n increase grain n
            !
                  SFAC   = 1.125 - 0.1250*TURFAC                      !
                  TFAC   = 0.690 + 0.0125*TEMPM                       !
! 
 
!           SUN Pericarp N conc.
            PNP = (0.0050 + 0.0100*NFAC)*AMAX1(SFAC,TFAC)            

            OILFRAC = 0.0
            NSINK1 = 0.0
 
            IF (PERWT .GT. 0.0) THEn
              NSINK2 = GROPER*PNP
            ELSE
              NSINK2 = 0.0
            ENDIF
            NSINK2 = AMAX1 (NSINK2,0.0)
            NSINK  =  NSINK2

            IF (NSINK .NE. 0.0) THEN
            
              FSINK2 = NSINK2 / NSINK
              RANC   = AMAX1 (RANC,RMNC)        ! Roots actual N conc.
              XLANC  = XNGLF  / GLFWT           ! Leaf N conc.
              XSANC  = XSTEMN / STMWT           ! Stem N conc.
              XHANC  = XHEADN / HEADWT          ! Head N conc.
              VANC   = STOVN  / STOVWT          ! Stover actual N conc.
              NPL1L  = GLFWT  * (XLANC - XLMNC) ! Total N in leaf pool
              NPL1L  = AMAX1 (NPL1L,0.0)

              IF (NPL1L .GT. XPEPE) THEN
                NPL1L = NPL1L - XPEPE
                NPL2L = XPEPE
              ELSE
                NPL2L = NPL1L
                NPL1L = 0 
              ENDIF   
              NPL1S  = STMWT  * (XSANC - XSMNC) ! Total N in stem pool
              NPL1S  = AMAX1 (NPL1S,0.0)
              NPL1H  = HEADWT * (XHANC - XHMNC) ! Total N in head pool
              NPL1H  = AMAX1 (NPL1H,0.0)
              NPOOL1 = NPL1L  + NPL1S + NPL1H   ! Total N in stover
              NPL2R  = RTWT   * (RANC  - RMNC)
              NPL2R  = AMAX1 (NPL2R,0.0)
              NPOOL2 = NPL2R  + NPL2L           ! Total N in roots pool
              NPOOL  = NPOOL1 + NPOOL2
!             Nitrogen supply/demand ratio
              NSDR   = NPOOL  / NSINK           

              IF (NSDR .LT. 1.0) THEN
                NSINK = NSINK*NSDR
              ENDIF

              XRAT = XNGLF / GLFWT
              YRAT = (0.009 - 0.0875*XRAT)/0.9125

              IF (NSINK .GT. NPOOL1 ) THEN
                XSTEMN = XSTEMN - NPL1S
                XHEADN = XHEADN - NPL1H
                STOVN  = STOVN  - NPOOL1
                RNOUT  = NSINK  - NPOOL1
                ROOTN  = ROOTN  - RNOUT*NPL2R/NPOOL2
                RANC   = ROOTN  / RTWT
                RONL   = RNOUT*NPL2L/NPOOL2 + NPL1L
                XLEAFN = XLEAFN - RONL           
                XNGLF  = XNGLF  - RONL - RONL*YRAT/(XNGLF/GLFWT-YRAT)
                XNSLF  = XLEAFN - XNGLF
              ELSE
                FACLN  = NPL1L  / NPOOL1
                FACSN  = NPL1S  / NPOOL1
                FACHN  = NPL1H  / NPOOL1
                XLEAFN = XLEAFN - FACLN * NSINK
                XNGLF=XNGLF-FACLN*NSINK*(1+YRAT/(XNGLF/GLFWT-YRAT))
                XNSLF  = XLEAFN - XNGLF
                XSTEMN = XSTEMN - FACSN * NSINK
                XHEADN = XHEADN - FACHN * NSINK
                STOVN  = STOVN  - NSINK
                VANC   = STOVN  / STOVWT
              ENDIF

              PERN = PERN + NSINK * FSINK2
         
            ENDIF

          GRAINN = PERN        !
          ! Calculate senescence due to N demand
        
          IF (GPLA .GT. 0.1) THEN
            XLAY = XNGLF/GLFWT
            YRAT = (0.009 - 0.0875 * XLAY)/0.9125
            ZZZ  = APLA - SLOPEPE * (XNGLF - YRAT*GLFWT)
              IF (ZZZ .GT. SLAN2) THEN
                DSLAN2 = ZZZ - SLAN2
              ELSE
                DSLAN2 = 0.0
              ENDIF
          ELSE
              DSLAN2 = 0.0
          ENDIF

        ! Threshold for starting leaf senescence due to water stress
        ! changed from 0.8 to 0.2 - FV, 12/12/97
        !
          IF (TURFAC .LT. 0.2) THEN
            DSLANW = 0.06 * GPLA
          ELSE
            DSLANW = 0.0
          ENDIF
        endif
          DSLAN2 = AMAX1 (DSLAN2,DSLANW)
          SLAN2  = SLAN2  + DSLAN2
          SLAY   = GPLA   / GLFWT
          SLAN   = SLAN1  + SLAN2
          WLAN2  = DSLAN2 / SLAY
          GROLF   = LFWT   - WLAN2 * (XLAY-YRAT)*6.25
          GLFWT  = GLFWT  - WLAN2
          SLFWT  = SLFWT  + WLAN2 * (1.0-6.25*(XLAY-YRAT))
          GPLA   = GPLA   - DSLAN2
 
          SPLA   = SPLA   + DSLAN2
          GROLF  = 0.0
          GROSTM  =  WLAN2 * (XLAY-YRAT)*6.25+GROSTM
          GROEMB = 0.0
          GRNWT  = PERWT
          SUMP   = SUMP   + CARBO
          IDURP  = IDURP  + 1
c      WRITE(*,*)'GROSUB 1655',dslan1,DSLAN2,dslanw,GPLA,SLAN2,plag
      !             5/11/2005 CHP Added cumulative leaf senescence
         CumLeafSenes =SLFWT * PLTPOP * 10. !total leaf senescence kg/ha

          !-------------------------------------------------------------
          !   ISTAGE = 5 Effective Grain Filling Period
          !-------------------------------------------------------------

        ELSEIF (ISTAGE .EQ. 5) THEN
          DSLAN1 = 0.0
          DSLAN2 = 0.0
          GROLF = 0.0
          GROSTM = 0.0
          GROHEAD = 0.0  
          GROPER = 0.0
          GROEMB = 0.0   
          PLAG = 0.0

          IF (SUMDTT .GE. 230.0 .AND. GPP .EQ. 0.0) THEN             
!             Include calculations to correct RUE after anthesis
             ! mantainance respiration
             PS    = RM/(RI1-1.0)
             RM    = (BIOMAS+RTWT*PLTPOP)*0.008*0.729  !mantainance resp
             RI1   = 1.0 + RM/PS !ratio gross net photosyntesis
             PSKER = SUMP / IDURP
         
             !
             ! Grain number calculated according to Soriano
             ! FV - 12/12/97
             !
             GPP = 500.0 + GPLA*750.0/6000.0
             GPP = AMIN1 (GPP,PPP)
             IF (PPP.GT.0.0) THEN
                PERWTE  = (PPP - GPP) * PERWT / PPP
                EMBWTE  = (PPP - GPP) * EMBWT / PPP
                GRNWTE  = PERWTE + EMBWTE
 
                IF (ISWNIT .EQ. 'Y'.and.grainn.gt.0.0) THEN
                   GRAINN1 = GRAINN * (1.0 - GRNWTE / GRNWT)
                   GRAINNE = GRAINN - GRAINN1
                   PERN    = PERN   * GRAINN1 / GRAINN
                   EMBN    = EMBN   * GRAINN1 / GRAINN
                   GRAINN  = GRAINN1
                ENDIF
                GRNWT   = GRNWT  - GRNWTE
                PERWT   = PERWT  - PERWTE
                EMBWT   = EMBWT  - EMBWTE
              ELSE
                PERWTE  = PERWT
                EMBWTE  = EMBWT
                GRNWTE  = PERWTE + EMBWTE
                
                EMBN    = 0.0
                GRNWT   = 0.0
                EMBWT   = 0.0
                GPSM    = 0.0
                IF (ISWNIT .EQ. 'Y') THEN
                  GRAINN1 = 0.0
                  GRAINNE = GRAINN
                ENDIF
             ENDIF
          ENDIF  
          IF (ABS(CARBO) .GT. 0.000001) CMAT = 0
C          WRITE(*,*)'GROSUB 1852',ISTAGE,EMAT,CMAT,CARBO
          IF (ISWNIT .EQ. 'Y') THEN
            IF (GLFWT .GT. 0.1) THEN
              XLAY = XNGLF / GLFWT
              YRAT = (0.009 - 0.0875 * XLAY)/0.9125
              ZZZ  = APLA - SLOPEPE * (XNGLF - YRAT*GLFWT)
                IF (ZZZ .GT. SLAN2 .AND. ZZZ .LE. APLA) THEN
                  DSLAN2 = ZZZ - SLAN2  ! Increment in senesc. leaf
                ELSE
                  DSLAN2 = 0.0
                ENDIF
             ENDIF
             
          ELSE
          
!           Convert thermal time to base of 8.5 C
!          
!           If N is not calculated, calculate leaf senescence according to
!           Sadras and Hall (1988), FCR 18:185-196.  - FV, 12/12/97
!          
            SUMDT8 = 0.75*SUMDTT
            IF (SUMDT8 .GT. 200.0) THEN
            !
            ! Senescence=f(TT) - FV 12/12/97
            !
              SLAN22 = APLA*(1.614-685./SUMDT8+73870./SUMDT8**2)
              DSLAN2 = SLAN22-SLAN2
            ELSE
              DSLAN2 = 0.0
            ENDIF
          ENDIF
      !
        ! Threshold for starting leaf senescence due to water stress
        ! changed from 0.8 to 0.2 - FV, 12/12/97
        !
          IF (TURFAC .LT. 0.2) THEN
            DSLANW = 0.06 * GPLA
          ELSE
            DSLANW = 0.0
          ENDIF

          DSLAN2 = AMAX1 (DSLAN2,DSLANW)
          SLAN2  = SLAN2 + DSLAN2
          SLAN   = SLAN1 + SLAN2

          IF (ABS(CARBO) .GT. 0.0001) THEN        !<--------------!
 
            IF (SUMDTT .LE. 270.0) THEN
              GROPER = POTGROPER * trg *(0.70 + 0.30*SWFAC)
            ELSE
              GROPER = 0.0
            ENDIF
         !
           ! Growth per embryo
          !
            PEPE = trg * G3 * 0.001 * (0.70 + 0.30 * SWFAC) 
            PEPE = PEPE * GRFACTOR
            IF (GPP .GT. 0.0) THEN
              GROEMB = GPP * PEPE
            ELSE
              GROEMB = PPP * PEPE
            ENDIF
c            PEPE =TRG*(0.70 + 0.30 * SWFAC)
            
            IF (HEADWT .LT. HWMAX) THEN
              GROHEAD = 1.71 * trg
            ELSE
              GROHEAD = 0.0
            ENDIF
            CDGR    = GROPER + GROEMB
            if (gpp.gt.0) then
              RGFILL = CDGR/GPP
            ELSE
              rgfill = 0.
            endif    
            IF (PEPE.GT.0.0) THEN   
              EMAT  = 0                                       !
             GRORT = 0.0                                     !
!            ELSE                                                !
!              EMAT = EMAT + 1                                 !
!              IF (FLOAT(EMAT).LE.RSGRT) THEN 
!                GRORT  = 0.0                                !
!              ELSE                                            !
!                SUMDTT = P5                                 !
!                CALL YR_DOY(YRDOY, YR, DOY)
!                WRITE(MESSAGE(1),2700) DOY                  !
!                CALL WARNING(1,ERRKEY, MESSAGE)             !
!                WRITE (     *,2700) DOY                     !
!                IF (IDETO .EQ. 'Y') THEN                    !
!                  WRITE (NOUTDO,2700) DOY                 !
!                ENDIF                                       !
!                EMAT   = 0                                  !
!                GRORT  = 0.0                                !
                 
!              ENDIF                                           !
            ENDIF                                               !
          ENDIF 


          CDEMAND = GROEMB + GROPER + GROHEAD
          
 
          IF (CDGR .GT. 0.1) THEN
            IF (CARBO .GT. CDEMAND ) THEN
              IF (STMWT .LT. SWMAX) THEN
                IF ((SWMAX-STMWT) .GT. (CARBO - CDEMAND)) THEN
                  GROSTM = CARBO - CDEMAND
                ELSE
                  GROSTM = SWMAX - STMWT
                  GRORT  = CARBO - CDEMAND - GROSTM
                ENDIF
              ELSE
                GRORT  = CARBO - CDEMAND
                GROSTM = 0.0
              ENDIF
            ELSE
              GRORT  = 0.0
              GROSTM = 0.0
              IF (CARBO .GT. CDGR) THEN
                GROHEAD = CARBO - CDGR
              ELSE
                GROHEAD = 0.0
                CPOOL   = (STMWT - SWMIN) + (HEADWT - HWMIN)
                FCP     = (STMWT - SWMIN) / (SWMAX - SWMIN)
                IF (FCP .GT. 0.3) THEN
                  FCP2 = 1.0
                ELSE
                  FCP2 = 0.0
                ENDIF
                CPOOL1 = 0.79*FCP2
                FCP    = (HEADWT-HWMIN)/(HWMAX-HWMIN)
                IF (FCP .GT. 0.3) THEN
                  FCP2 = 1.0
                ELSE
                  FCP2 = 0.0
                ENDIF
                CPOOL2 = 0.42*FCP2
                CPOOL  = CPOOL1 + CPOOL2
                IF (CPOOL .GT. ((CDGR-CARBO)/0.44)) THEN
                  FPOOL1 = CPOOL1 / CPOOL
                  FPOOL2 = CPOOL2 / CPOOL
                  STMWT  = STMWT  - (GROEMB+GROPER-CARBO)/0.44*FPOOL1
                  HEADWT = HEADWT - (GROEMB+GROPER-CARBO)/0.44*FPOOL2
                ELSE
                  IF (CDGR .GT. 0.1) THEN
                    FACPOOL = (CPOOL*0.44+CARBO)/CDGR
                  ELSE
                    FACPOOL = 0.0
                  ENDIF
                  FACPOOL=AMAX1(FACPOOL,0.0)
                  GROEMB = GROEMB * FACPOOL
                  GROPER = GROPER * FACPOOL
                  STMWT  = STMWT  - CPOOL1
                  HEADWT = HEADWT - CPOOL2
                ENDIF   
              ENDIF
            ENDIF

c           GROPER = GROPER*(.7+.3*SWDF1)
            GRORT  = CARBO - GROEMB - GROPER - GROHEAD - GROSTM

            IF (ISWNIT .EQ. 'Y') THEN
              ! Grain n allowed to vary between .02 and .038.
              ! High temp., low soil water, and high n increase grain n
                  SFAC   = 1.125 - 0.1250*TURFAC                      !
                  TFAC   = 0.690 + 0.0125*TEMPM                       !
! 
             
!             SUN Emryo N conc.
              ENP = (0.0225 + 0.0200*NFAC)*AMAX1(SFAC,TFAC) 
!             SUN Pericarp N conc.
              PNP = (0.0050 + 0.0100*NFAC)*AMAX1(SFAC,TFAC) 


              NSINK = 0.1
      !
            ! Modified N conc in kernel to 4.25%
      
              IF (EMBWT .GT. 0.0) THEN
                NSINK1 = GROEMB*ENP
              ELSE
                NSINK1 = 0.0
              ENDIF

              IF (PERWT .GT. 0.0) THEN
                NSINK2 = GROPER*PNP
              ELSE
                NSINK2 = 0.0
              ENDIF
              NSINK1 = AMAX1 (NSINK1,0.0)
              NSINK2 = AMAX1 (NSINK2,0.0)
              NSINK  = NSINK1 + NSINK2

              IF (NSINK .NE. 0.0) THEN
                FSINK1 = NSINK1 / NSINK
                FSINK2 = NSINK2 / NSINK
                RANC   = AMAX1 (RANC,RMNC)        ! Roots actual N conc.
                XLANC  = XNGLF  / GLFWT           ! Leaf N conc.
                XSANC  = XSTEMN / STMWT           ! Stem N conc.
                XHANC  = XHEADN / HEADWT          ! Head N conc.
                VANC   = STOVN  / STOVWT          ! Stover actual N conc
                NPL1L  = GLFWT  * (XLANC - XLMNC) ! Total N in leaf pool
                NPL1L  = AMAX1 (NPL1L,0.0)
                IF (NPL1L .GT. XPEPE) THEN
                  NPL1L = NPL1L - XPEPE
                  NPL2L = XPEPE
                ELSE
                  NPL2L = NPL1L
                  NPL1L = 0 
                ENDIF   

                NPL1S  = STMWT  * (XSANC - XSMNC) ! Total N in stem pool
                NPL1S  = AMAX1 (NPL1S,0.0)
                NPL1H  = HEADWT * (XHANC - XHMNC) ! Total N in head pool
                NPL1H  = AMAX1 (NPL1H,0.0)
                NPOOL1 = NPL1L  + NPL1S + NPL1H   ! Total N in stover
                NPL2R  = RTWT   * (RANC  - RMNC)
                NPL2R  = AMAX1 (NPL2R,0.0)
                NPOOL2 = NPL2R  + NPL2L           !Total N in roots pool
                NPOOL  = NPOOL1 + NPOOL2
                IF (NSINK.NE.0.0) THEN
                  NSDR   = NPOOL  / NSINK        ! N supply/demand ratio
                ELSE
                  NSDR= 10.
                ENDIF  
                IF (NSDR .LT. 1.0) THEN
                  NSINK = NSINK*NSDR
                ENDIF
                IF (GLFWT.GT.0.0) THEN
                  XRAT = XNGLF / GLFWT
                ELSE
                  XRAT = .02
                ENDIF    
                YRAT = (0.009 - 0.0875*XRAT)/0.9125

                IF (NSINK .GT. NPOOL1 ) THEN
                  XSTEMN = XSTEMN - NPL1S
                  XHEADN = XHEADN - NPL1H
                  STOVN  = STOVN  - NPOOL1
                  RNOUT  = NSINK  - NPOOL1
                  IF (NPOOL2.GT.0.0) THEN
                    ROOTN  = ROOTN  - RNOUT*NPL2R/NPOOL2
                  ENDIF  
                  RANC   = ROOTN  / RTWT
                  IF (NPOOL2.GT.0.0) THEN
                    RONL   = RNOUT*NPL2L/NPOOL2 + NPL1L
                  ELSE
                    RONL =   NPL1L
                  ENDIF  
                  XLEAFN = XLEAFN - RONL           
                  XNGLF  = XNGLF  - RONL - RONL*YRAT/(XNGLF/GLFWT-YRAT)
                  XNSLF  = XLEAFN - XNGLF
                ELSE
                  FACLN  = NPL1L  / NPOOL1
                  FACSN  = NPL1S  / NPOOL1
                  FACHN  = NPL1H  / NPOOL1
                  XLEAFN = XLEAFN - FACLN * NSINK
                  XNGLF=XNGLF-FACLN*NSINK*(1+YRAT/(XNGLF/GLFWT-YRAT))
                  XNSLF  = XLEAFN - XNGLF
                  XSTEMN = XSTEMN - FACSN * NSINK
                  XHEADN = XHEADN - FACHN * NSINK
                  STOVN  = STOVN  - NSINK
                  VANC   = STOVN  / STOVWT
                ENDIF

                PERN = PERN + NSINK * FSINK2
                EMBN = EMBN + NSINK * FSINK1
              ENDIF

              GRAINN = EMBN + PERN
            ENDIF

 
            IF (GLFWT.GT.0.0) THEN
              SLAY   = GPLA   / GLFWT
              XLAY   = XNGLF  / GLFWT
              YRAT = (0.009 - 0.0875*XLAY)/0.9125
            ENDIF  
            SLAN   = SLAN1  + SLAN2
            IF (SLAY .GT. 0.0) THEN
              WLAN2 = DSLAN2/SLAY
            ELSE
              WLAN2 = 0.0
            ENDIF
            LFWT  = LFWT  - WLAN2 * (XLAY-YRAT)*6.25
            GLFWT = GLFWT - WLAN2
            SLFWT = SLFWT + WLAN2 * (1.0-6.25*(XLAY-YRAT))
            GPLA  = GPLA  - DSLAN2
            SPLA  = SPLA  + DSLAN2
            STMWT = STMWT + WLAN2*(XLAY-YRAT)*6.25 

            IF (EMBWT.GT.0.0.AND.SUMDTT.GT.230.0) THEN
              PR = 1000.0*GROEMB/GPP/G3
              IF (PR .LE. 0.7) THEN
                PO = O1
              ELSE
                PO = EXP(-1.4*(PR-0.8))*O1
              ENDIF
              OILINC  = PO/100.0 * GROEMB *(P5-170.0)/(P5-230.0)
              OIL     = OIL + OILINC
              OILWT=OIL*PLTPOP
              OILPERC = OIL / GRNWT * 100.0
              OILPC=OILPERC
            ELSE
              OILFRAC = 0.0
              OILPERC = 0.0
              OILPC = 0.0
              OILWT = 0.0
            ENDIF

            SUMP  = SUMP  + CARBO
            IDURP = IDURP + 1
          ENDIF

        CumLeafSenes =SLFWT * PLTPOP * 10.  !total leaf senescence kg/ha
!        ----------------------------------------------------------------
!        ISTAGE = 6 
!        (End effective grain filling period to harvest maturity)
!        ----------------------------------------------------------------
        ELSEIF (ISTAGE .EQ. 6) THEN
          GROLF = 0.0
          GROSTM = 0.0
          GROHEAD = 0.0  
          GROPER = 0.0
          GROEMB = 0.0  
          GRORT = 0.0
          GROGRN = 0.0 
          PLAG = 0.0

          rm = .01*(glfwt+stmwt+headwt+grnwt)
          IF (STMWT-SWMIN.GT.0.) THEN
            CPOOL1=STMWT-SWMIN
          ELSE
            CPOOL1=0.  
          ENDIF  
          IF (HEADWT-HWMIN.GT.0.) THEN
            CPOOL2=HEADWT-HWMIN
          ELSE
            CPOOL2=0.  
          ENDIF 
          IF (GLFWT.GT.0.) THEN
            CPOOL3=GLFWT*.7
          ELSE
            CPOOL3=0.  
          ENDIF 
          CPOOL=CPOOL1+CPOOL2+CPOOL3
          IF (RM.GT.CPOOL) THEN
            RM = CPOOL
          ENDIF  
          IF (CPOOL.GT.0.) THEN
            STMWT=STMWT-CPOOL1/CPOOL*RM
            HEADWT=HEADWT-CPOOL2/CPOOL*RM
            IF (GLFWT.GT.0.0) THEN
              SLAY   = GPLA   / GLFWT
            ELSE
              SLAY=0.
            ENDIF           
            GLFWT=GLFWT-CPOOL3/CPOOL*RM
            GPLA = GPLA - CPOOL3/CPOOL*RM * SLAY
            IF (ISWNIT .EQ. 'Y'.AND.GLFWT.GT.0.0) THEN
              XLAY   = XNGLF  / GLFWT
              XNGLF = XNGLF -CPOOL3/CPOOL*RM*XLAY
            ENDIF
          ENDIF  
            UNO3 = 0.0; UNH4 = 0.0; KUPTAKE = 0.0
 
        ENDIF

        CARBO = AMAX1 (CARBO,0.001)
      !

!----------------------------------------------------------------------
!   The following code is executed each day regardless of ISTAGE value
!----------------------------------------------------------------------


          !------------------------------------------------------------
          !               Compute Leaf Senescence Factors
          !------------------------------------------------------------

!     Senescence due to phosphorus stress inactivated for OS
!     5/9/07 CHP, JIL, KJB change from PStres2 to PStres1
c      SLFP   = (1-FSLFP) + FSLFP*PSTRES1 

!     Senescence due to temperature
      SLFT   = 1.0
      IF (TMIN.LE.6.0) THEN
        SLFT  = AMAX1 (0.0, 1.0 - 0.01 * (TMIN-6.0)**2)
      ENDIF
      SLFT  = AMAX1 (SLFT,0.0)

      LFWT = LFWT + GROLF
      GLFWT = GLFWT + GROLF
      STMWT = STMWT + GROSTM
      HEADWT = HEADWT + GROHEAD
      PERWT = PERWT + GROPER
      EMBWT = EMBWT + GROEMB
      GRNWT = PERWT + EMBWT
      PLA = PLA + PLAG
      
      GPLA = GPLA + PLAG
c      PLAS  = (PLA-SENLA)*(1.0-SLFP) 
!     Daily rate of leaf senescence
      
      SENLA = SLAN
      SENLA = AMIN1 (SENLA,PLA)         
      LAI   = GPLA*PLTPOP*0.0001

      PLAMX  = AMAX1 (PLAMX,GPLA)
        ! Calculate potential growth of plant parts
      !
        PDWI   = CARBO - GRORT
        PDWIL  = GROLF
        PDWIS  = GROSTM
        PDWIH  = GROHEAD
        PGRORT = GRORT
        SLFN   = 0.95 + 0.05*AGEFAC
        SLFT   = 1.0
        IF (TMIN .GT. TSEN) THEN
          ICOLD = 0
        ELSE
          ICOLD = ICOLD + 1
        ENDIF
        SLFT   = AMAX1 (SLFT,0.0)     

        IF (((LEAFNO.GT.3).AND.(LAI.LE.0.0).AND.(ISTAGE.LE.4))
     &                      .OR. (ICOLD .GE. CDAY)) THEN
          
c          WRITE (*,2800)
          IF (IDETO .EQ. 'Y') THEN
            WRITE (NOUTDO,2800)
          ENDIF
          ISTAGE = 6
          MDATE = YRDOY
        ENDIF

        IF (ISTAGE .LT. 4) THEN
           RTWT = RTWT + GRORT
        ELSE
           RTWT = RTWT + GRORT - 0.005*RTWT
        ENDIF

   
!     the section below is added similar to that of Ceres-Maize      
           !------------------------------------------------------------
          !CROP GROWTH FAILURE DUE TO SEVERE WATER STRESS  
          !End growth and module run if water stress days 
          !exceeds 10 days before Effective grain Filling Period if 
          !LAI <= 0.0
          !------------------------------------------------------------

          IF (SWFAC .GT. 0.1) THEN
              NWSD = 0                                                  
          ELSE
              NWSD = NWSD + 1
          ENDIF

          IF (LAI .LE. 0.1 .AND. ISTAGE .LT. 4
     &                 .AND. NWSD .GT. 10) THEN
              WRITE(MESSAGE(1),2800)
              CALL WARNING(1,ERRKEY, MESSAGE)
              IF (ISWITCH % IDETL .NE. '0') THEN                     !
                WRITE (*,2800)
              ENDIF                                                  !
              IF (IDETO .EQ. 'Y') THEN
                  WRITE (NOUTDO,2800)
              ENDIF   
              ISTAGE = 6           
              MDATE = YRDOY
          ENDIF


!--------------------------------------------------------------
!  APPLY PEST DAMAGE
!--------------------------------------------------------------

      ! Leaf Damage
      IF((LFWT+STMWT).GT.0.0)
     $      STOVN=STOVN - STOVN*(WLIDOT/PLTPOP)/(LFWT+STMWT)
      IF (PLTPOP.GT.0.0.AND.LFWT.GT.0.0)
     &      LAIDOT = WLIDOT*(PLA-SENLA)/(LFWT*PLTPOP)  !cm2/plant/day
      IF(PLTPOP.GT.0.0)
     &      LFWT = LFWT - WLIDOT/PLTPOP
           
      PLA = PLA - LAIDOT
      LAI = LAI - LAIDOT*PLTPOP/10000

      ! Stem Damage
      IF(PLTPOP.GT.0.0)
     &       STMWT = STMWT - WSIDOT/PLTPOP
      IF(PLTPOP.GT.0.0.AND.(LFWT+STMWT).GT.0.0)
     &       STOVN=STOVN - STOVN*(WSIDOT/PLTPOP)/(LFWT+STMWT)

      ! Root Weight
      IF(PLTPOP.GT.0.0 .AND. RTWT .GT. 0.) THEN
        ROOTN = ROOTN - ROOTN*(WRIDOT/PLTPOP)/RTWT
        RTWT = RTWT - WRIDOT/PLTPOP
        
      ENDIF

      ! Grain Weight and Number
      IF (GRNWT .GT. 0.AND.PLTPOP.GT.0) THEN
        GRAINN = GRAINN - GRAINN*(SWIDOT/PLTPOP)/GRNWT
        GPP = GPP - GPP*(SWIDOT/PLTPOP)/GRNWT
      ENDIF

      IF(PLTPOP.GT.0.0) THEN 
        GRNWT = GRNWT - SWIDOT/PLTPOP
        EARWT = EARWT - SWIDOT/PLTPOP
      ENDIF

      ! Population
      IF(PPLTD.GT.0) THEN
        PLTPOP = PLTPOP - PLTPOP * PPLTD/100
           
        LAI = LAI - LAI*(PPLTD/100)
      ENDIF

      !------------------------------------------------------------
      !               COMPUTE NITROGEN UPTAKE
      !------------------------------------------------------------

      IF (CARBO .LE. 0.0) THEN
        CARBO = 0.001
      ENDIF
      IF (ISWNIT .NE. 'N') THEN 
        IF(CARBO.GT.0.0) THEN  
          PDWI   = PCARB*(1.0-GRORT/CARBO)
          PGRORT = PCARB*GRORT/CARBO
        ENDIF

        CALL SU_NUPTAK(
     %    RANC, ROOTN,RTWT,TANC,STOVN,STOVWT,TRNU,NLAYR,
     %    RLV,NO3,NH4,UNO3,UNH4,
     %    RCNP,PGRORT,PLTPOP,SW,LL,SAT,DLAYR,
     %    SHF,PTF, SENESCE, KG2PPM, PLIGRT,
     %    XLCNP,XSCNP,XHCNP,GROPER,GROEMB,
     %    PDWIL,PDWIH,PDWIS,XNGLF,XSTEMN,XHEADN,
     %    GLFWT,STMWT,HEADWT,PNP,ENP,PERWT,EMBWT,
     %    XLEAFN,XLANC,XNSLF)

        CALL SU_KUPTAK(
     &          ISWPOT, NLAYR, SKi_Avail, UNH4, UNO3,     !Input
     &          KUPTAKE, KSTRES)                          !Output

      ENDIF

      !------------------------------------------------------------
      !                      STATE VARIABLES
          !------------------------------------------------------------

          
      LFWT = MAX(0.0,LFWT)
      PLA = MAX(0.0,PLA)
      LAI = MAX(0.0,LAI)
      STMWT = MAX(0.0,STMWT)
      STOVN = MAX(0.0,STOVN)
      GPP = MAX(0.0,GPP)
      GRNWT = MAX(0.0,GRNWT)
      HEADWT = MAX(0.0,HEADWT)
      PLTPOP = MAX(0.0,PLTPOP)
      RTWT = MAX(0.0,RTWT)

      BIOMAS = (GLFWT + STMWT + HEADWT + GRNWT + GRNWTE)*PLTPOP
      DM     = BIOMAS*10.0
      IF (BIOMAS .GT. 0.0) THEN
        HIO = OILWT/BIOMAS
      ELSE
        HIO = 0.0
      ENDIF

      BIOMAS_R=(GLFWT+STMWT+HEADWT+GRNWT+GRNWTE+RTWT)
     &     *PLTPOP

      STOVWT = GLFWT + STMWT + HEADWT
      IF(RTWT + STOVWT .GT. 0.0) THEN
        PTF    = STOVWT/(RTWT + STOVWT)
      ELSE
        PTF    = 0.0
      ENDIF   
      IF (BIOMAS .GT. 0.0) THEN
        PTF = BIOMAS/BIOMAS_R
      ELSE
        PTF = 0.0
      ENDIF

      VSTAGE = REAL (LEAFNO)        !V-stage
      IF (ISTAGE .GE.1.AND.ISTAGE.LE.6) THEN
        RSTAGE = ISTAGE           !R-stage
      ELSE
        RSTAGE = 0
      ENDIF

      TOPWT = (LFWT+STMWT+HEADWT+GRNWT+GRNWTE)*PLTPOP
      WTLF = LFWT*PLTPOP        !Leaf weight, g/m2
      XLAI = LAI                !Leaf area index, m2/m2
      XHLAI = LAI               !Used in WATBAL


      IF (XLAI >= MAXLAI) THEN    !keeps CANHT at the maximum value
        CANHT = XLAI / 2.0
        IF (CANHT .GT. CANHT_POT) THEN
          CANHT = CANHT_POT
        ENDIF
      ENDIF 

      MAXLAI = AMAX1 (MAXLAI,XLAI)      ! Maximum XLAI season

      IF(WTLF.GT.0) THEN
        SLA = LAI*10000/WTLF  !Specific leaf area, cm2/g
        DUMMY = PLA/LFWT
      ELSE    
        SLA = 0
      ENDIF

      STMWTO = STMWT * PLTPOP   !Stem weight, g/m2
      RTWTO = RTWT * PLTPOP     !Root weight, g/m2
      SDWT = GRNWT*PLTPOP         !Seed weight, g/m2
      GPSM = GPP * PLTPOP         !Seed no/m2, but units don't match?
      SEEDNO=GPSM               !Seed no./m2


      IF (TOPWT .GT. 0. .AND. SDWT .GE. 0.) THEN
        HI = SDWT/TOPWT
      ELSE
        HI = 0.
      ENDIF

!------> OLD NUPTAK CALL

      !------------------------------------------------------------
      ! Compute Plant Nitrogen Variables for Output
      !------------------------------------------------------------

      WTNUP = WTNUP + TRNU*PLTPOP    !Total N uptake (g N/m2)  
      WTNCAN = (STOVN+GRAINN)*PLTPOP !Nitrogen in canopy (g N/m2)
      WTNSD = GRAINN*PLTPOP  !Nitrogen in grain (g N/m2)
    
        WTNLF = XNGLF * PLTPOP 
        !Nitrogen in leaf, g/m2
        WTNST = XSTEMN * PLTPOP 
        !Nitrogen in stem, g/m2
      
!     Nitrogen in vegetative tissue (g/m2)
      WTNVEG=(XNGLF+XSTEMN+XHEADN)*10.   

C       WTN = WTNCAN*10. / BIOMAS * 100.

          !------------------------------------------------------------
      !Plant Nitrogen Concentration Variables
      !------------------------------------------------------------

      IF(GLFWT.GT.0.AND.PLTPOP.GT.0) THEN
        PCNL = WTNLF/(GLFWT*PLTPOP)*100  !Percent N in leaf tissue
      ELSE
        PCNL = 0
      ENDIF

      IF(GRNWT.GT.0.AND.PLTPOP.GT.0) THEN
        PCNSD = WTNSD/(GRNWT*PLTPOP) *100
      ELSE
        PCNSD = 0.0
      ENDIF

      IF (GRNWT .GT. 0.0.AND.PLTPOP.GT.0) THEN
        PCNGRN = WTNSD/(GRNWT*PLTPOP)*100
      ELSE
        PCNGRN = 0.0
      ENDIF

      IF( (GLFWT+STMWT).GT.0.AND.PLTPOP.GT.0) THEN
        PCNVEG = (WTNLF+WTNST)/((GLFWT+STMWT)*PLTPOP)*100  
      ELSE
        PCNVEG = 0.0
      ENDIF

      IF(STMWT.GT.0.0.AND.PLTPOP.GT.0.0) THEN
        PCNST = WTNST/(STMWT*PLTPOP)*100 
      ELSE                            !Concentration of N in stems
        PCNST = 0.0
      ENDIF

      IF(RTWT.GT.0.0) THEN
        PCNRT = ROOTN/RTWT*100  !Concentration of N in roots
      ELSE
        PCNRT = 0.0
      ENDIF

      IF (ISWPHO .NE. 'N') THEN 
        CALL P_Ceres (DYNAMIC, ISWPHO,                    !Input
     &       CumLeafSenes, DLAYR, DS, FILECC, MDATE, NLAYR,  !Input
     &        PCNVEG, PLTPOP, OILWT, RLV, RTDEP, RTWTO,       !Input
     &        SDWT, SWIDOT, SeedFrac, SPi_AVAIL, Stem2Ear,    !Input
     &        STMWTO, VegFrac, WLIDOT, WRIDOT, WSIDOT,        !Input
     &        WTLF, YRPLT,                                    !Input
     &        SENESCE,                                        !I/O
     &        PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, !Output
     &        PStres1, PStres2, PUptake, FracRts)             !Output
      ENDIF

          !------------------------------------------------------------
      ! COMPUTE AVERAGE STRESS FACTORS DURING EACH GROWTH STAGE
          !------------------------------------------------------------
      !CHP 9/5/04 - these aren't currently being used anywhere.
      CSD1 = CSD1 + 1.0 - SWFAC
      CSD2 = CSD2 + 1.0 - TURFAC
      CNSD1 = CNSD1 + 1.0 - NSTRES
      CNSD2 = CNSD2 + 1.0 - AGEFAC
      CPSD1 = CPSD1 + 1.0 - PSTRES1
      CPSD2 = CPSD2 + 1.0 - PSTRES2
      ICSDUR = ICSDUR + 1

      IF (ISWWAT .NE. 'N' .AND. ISTAGE .GT. 0) THEN
        IF(ICSDUR.GT.0) THEN
          SI1(ISTAGE) = CSD1  / ICSDUR
          SI2(ISTAGE) = CSD2  / ICSDUR
          SI3(ISTAGE) = CNSD1 / ICSDUR
          SI4(ISTAGE) = CNSD2 / ICSDUR
          SI5(ISTAGE) = CPSD1 / ICSDUR
          SI6(ISTAGE) = CPSD2 / ICSDUR
        ENDIF
      ENDIF

          !------------------------------------------------------------
      ! Compute Variables Needed for Next Day's Pest Damage
          !------------------------------------------------------------

      AREALF = LAI *10000   !cm2/m2
!     Compute N lost in leaf senescence
!     N in senesced leaves - calculate based on today's N content
      IF (GLFWT .GT. 0.0) THEN
        CumLfNSenes = CumLfNSenes + 
     &    (CumLeafSenes - CumLeafSenesY) * XNGLF/GLFWT
      ELSE
        CumLfNSenes = 0.0
      ENDIF

!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
!                     DYNAMIC = OUTPUT
!
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      
      ELSEIF(DYNAMIC.EQ.OUTPUT) THEN

!          WTNUP = WTNUP + TRNU*PLTPOP  
              !Total N uptake, g/m2  Moved to mz_roots.for
          YIELD = GRNWT*10.0*PLTPOP
          IF (GPP.GT. 0) THEN
            SKERWT = GRNWT/GPP
          ELSE
            SKERWT = 0.0
          ENDIF
          GPSM = GPP*PLTPOP
          APTNUP = STOVN*10.0*PLTPOP
          TOTNUP = APTNUP

          IF (ISWNIT .EQ. 'N') THEN
              XGNP = 0.0
              GNUP = 0.0
          ELSE
              IF (GRNWT .GT. 0.0) THEN
                  XGNP = (GRAINN/GRNWT)*100.0
                  GNUP = GRAINN*EARS*10.0
              ENDIF
          ENDIF
          IF(ISTAGE.EQ.6) TOTNUP      = GNUP + APTNUP  
              !Need to track the logic for this...

          CALL P_Ceres (DYNAMIC, ISWPHO,                    !Input
     &      CumLeafSenes, DLAYR, DS, FILECC, MDATE, NLAYR,  !Input
     &      PCNVEG, PLTPOP, OILWT, RLV, RTDEP, RTWTO,       !Input
     &      SDWT, SWIDOT, SeedFrac, SPi_AVAIL, Stem2Ear,    !Input
     &      STMWTO, VegFrac, WLIDOT, WRIDOT, WSIDOT,        !Input
     &      WTLF, YRPLT,                                    !Input
     &      SENESCE,                                        !I/O
     &      PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, !Output
     &      PStres1, PStres2, PUptake, FracRts)             !Output

!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
!                     DYNAMIC = SEASEND
!
!----------------------------------------------------------------------
!----------------------------------------------------------------------

      ELSEIF(DYNAMIC.EQ.SEASEND) THEN

        STOVER  = BIOMAS*10.0-YIELD
        XANC   = TANC*100.0

        IF (ISWNIT .EQ. 'N') THEN
          XGNP = 0.0
          GNUP = 0.0
        ELSE
          IF (GRNWT .GT. 0.0) THEN
            XGNP = (GRAINN/GRNWT)*100.0
            GNUP = GRAINN*EARS*10.0
          ENDIF
        ENDIF
        
        IF (HARVFRAC(2) .LE. 0.0) THEN
              HARVFRAC(2) = 1.0
        ENDIF
        
        CALL P_Ceres (DYNAMIC, ISWPHO,                    !Input
     &      CumLeafSenes, DLAYR, DS, FILECC, MDATE, NLAYR,  !Input
     &      PCNVEG, PLTPOP, OILWT, RLV, RTDEP, RTWTO,       !Input
     &      SDWT, SWIDOT, SeedFrac, SPi_AVAIL, Stem2Ear,    !Input
     &      STMWTO, VegFrac, WLIDOT, WRIDOT, WSIDOT,        !Input
     &      WTLF, YRPLT,                                    !Input
     &      SENESCE,                                        !I/O
     &      PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, !Output
     &      PStres1, PStres2, PUptake, FracRts)             !Output

!       Senesced leaves do not fall to the ground and so are added to
!         surface litter only at harvest.
        SENESCE % ResWt(0)  = CumLeafSenes
        SENESCE % ResLig(0) = CumLeafSenes * 0.4 * PLIGLF
!       N in senesced leaves
        SENESCE % ResE(0,1) = CumLfNSenes

      ENDIF       !Endif for DYNAMIC LOOP
      RETURN

!----------------------------------------------------------------------
!     Format Strings
!----------------------------------------------------------------------

 2700 FORMAT (2X,'Crop mature on JD',I4,' due to slowed grain filling')
 2800 FORMAT (2X,'Crop failure growth program terminated ')


      END SUBROUTINE SU_GROSUB


!----------------------------------------------------------------------
!                         DEFINITIONS
!----------------------------------------------------------------------

! AGEFAC      !Nitrogen stress factor affecting expansion (0-1)
! ANO3        !Total extractable nitrate N in soil profile (kg N/ha)
! ANH4        !Total extractable ammonium N in soil profile (kg N/ha)
! APTNUP      !Nitrogen in stover (above ground biomass) kg N/ha
! AREALF      !Leaf area index, cm2 leaf/m2 ground
! ASMDOT      !Reduction in photosynthesis due to pests, %/day
! BIOMAS      !Above ground biomass, g/m2
! CANHT       !Canopy height, cm (not used)
! CANNAA      !Stover N at anthesis, g N/M2
! CANWAA      !Canopy weight at anthesis, g/m2
! CANWA       !Canopy width, cm (not used)
! CARBO       !Daily biomass production, g/plant/day
! CARBOT      !Consecutive days where CARBO is less than .001 before plant matures due to stress
! CMAT        !Counts consecutive days that CARBO is less than 0.001    
! CNSD1       !Cumulative nitrogen deficit factor on photosynthesis in 
!              each stage used to compute avg. stress during stage
! CNSD2       !Cumulative nitrogen deficit on growth in each stage used
!              to compute avg. stress during stage
! CUMPH       !Cumulative phyllochron intervals or fully expanded leaves
! CO2X(10)    !CO2 effect on photosynthesis, X axis is CO2 level, ppm
! CO2Y(10)    !CO2 effect on photosynthesis, Y axis is relative effect
! CO2         !Atmospheric CO2, ppm
! CSD1        !Cumulative water stress on photosynthesis during growth stage
! CSD2        !Cumulative water stress on growth during growth stage
! CURV        !Function to interpolate a value from a series of curves
! DLAYR(L)    !Soil thickness in layer L (cm)
! DM          !Total above ground biomass, kg/ha
! DOY         !Day of year
! DTT         !Growing degree days today, C
! DYNAMIC     !Main control variable to tell each module which section of code to run
! EARS        !Ears per m2
! EARWT       !Ear weight, g/ear JIL: should be g/plant
! ECONO       !Ecotype number for the variety
! EMAT        !Flag used in grosub to determine if relative grain fill is below 0.1 for 2 consecutive days
! EOP         !Potential plant transpiration, mm/d
! EP1         !Potential plant transpiration, cm/d
! SEASEND     !Program control variable to execute code to complet model run (value=6)
! FSLFW       !Fraction of leaf area senesced due to 100% water stress, 1/day
! FSLFN       !Fraction of leaf area senesced due to 100% nitrogen stress, 1/day
! G3          !Potential kernel growth rate mg/kernel/day
! GNP         !Nitrogen concentration in new grain growth, gN/g dry matter
! GNUP        !Total grain N uptake, kg N/ha
! GPP         !Grain number per plant, grains/plant
! GPSM        !Grain numbers, grains/m2
! GRAINN      !Grain nitrogen content, g N/plant
! GRF         !Fraction of today's carbon allocated to above ground biomass
! GRNWT       !Grain weight, g/plant
! GROEAR      !Daily growth of the ear - g
! GROGRN      !Daily growth of the grain - g
! GROLF       !Leaf growth rate, g/plant/day
! GRORT       !Root growth rate, g/plant/day
! GROSTM      !Stem growth rate, g/plant/day
! HI          !Ratio of seed weight (SDWT) to weight of above-ground portion of 
!              plant (TOPWT) (g[seed] / g[tops])
! HIO         !Ratio of OIL weight (OILWT) to weight of above-ground portion of 
!              plant (TOPWT) (g[pods] / g[tops])
! I           !Counter
! ICOLD       !Cumulative number of days when TMIN is less than -3 C
! ICSDUR      !Calendar day accumulator for each growth stage   
! IDETO*1     !Screen output switch, (Y/N)
! INTEGR      !Program control variable to execute code to integrate daily rate variables (value=4)
! ISTAGE      !Growth stage (integer)
! ISWNIT*1    !Nitrogen balance switch (Y/N)
! ISWWAT      !Soil water balance on/off switch (Y for yes, N for no)
! KG2PPM(L)   !Factor that convergs mg elemental N/kg soil to kg N/ha for soil layer L
! L           !Index counter
! LAI         !Leaf area index, m2/m2
! LAIDOT      !Leaf area consumed by pests, cm2 leaf area/m2/day
! LEAFNO      !Number of oldest leaf per plant (same as XN)
! LEAFNOE     !Number of oldest leaf at emergence
! LFWT        !Leaf weight, g/plant
! LFWTE       !Leaf weight at emergence, g/plant
! LIFAC       !Effect of row spacing and plant population on light interception
! LL(L)       !Lower limit of plant extractable water for soil layer L, cm3/cm3
! MAXLAI      !Maximum leaf area index, m2/m2
! NDEF3       !Nitrogen stress factor affecting grains per plant (0-1)
! NFAC        !Nitrogen stress factor based on actual and critical nitrogen content in vegetative tisue
! NH4(L)      !Ammonium in soil layer L, mg elemental N/kg soil
! NLAYR       !Number of soil layer
! NO3(L)      !Nitrate in soil layer L (mg elemental N/kg soil)
! NOUTDO      !File number
! NPOOL       !Total plant N available for translocation to grain (g/plant)
! NPOOL1      !Tops N available for translocation to grain (g/plant)
! NPOOL2      !Root N available for translocation to grain (g/plant)
! NSDR        !Plant N supply/demand ratio used to modify grain N content
! NSINK       !Demand for N associated with grain filling (g/plant/day)
! NSTRES      !Nitrogen stress factor affecting growth (0-1)
! NWSD        !No. of consecutive days with severe water stress
! OUTPUT      !Program control variable to output state and rate variables to output file (value=5)
! P5          !GDD from silking to physiological maturity, C
! PAR         !Daily photosynthetically active radiation, calculated as half
! PARSR       !Conversion of solar radiation to PAR, (MJ PAR/m2/d)/(MJ SRAD/m2/d)
! PC          !Used to compute fraction of phyllochron interval occurring today
! PCARB       !Potential dry matter production under optimum water, nitrogen and temperature, g/plant
! PCNGRN      !Percent nitrogen in grain, %
! PCNL        !Percent nitrogen in leaves, %
! PCNRT       !Percent nitrogen in roots, %
! PCNST       !Percent nitrogen in stems, %
! PCNVEG      !Percent nitrogen in vegetative tissue (leaf and stem), kg N/ha
! PCO2        !Effect of CO2 on plant growth rate
! PDWI        !Potential increment in new shoot growth, g/plant
! SLPF        !Relative reduction in growth due to poor soil fertility (0-1.0) that is
!              not related to nitrogen.
! PGRORT      !Potential increment in new root growth, g/plant
! PHY1        !Phyllochron interval up to leaf 6
! PHY2        !Phyllochron interval from leaf 7
! PLA         !Plant leaf area, cm2/plant
! PLAE        !Plant leaf area at emergence, cm2/plant
! PLAG        !Leaf area growth, cm2/plant
! PLAS        !The rate of senescence of leaf area on one plant - sq. cm/day
! PLTPOP      !Plant population, plants/m2
! PORMIN      !Minimum pore space volume required for supplying oxygen to roots for optimum
!             ! growth and function (0-100%) 
! PPLTD       ! Percent of plants destroyed today, %/m2/d
! PRFTC       !Array containing base, optimum and maximum temperature for function reducing photosynthesis due to temperature.
! PRFT        !Photosynthetic reduction factor for low and high temperatures
! PTF         !Ratio of above ground biomass to total biomass
! RANC        !Root actual N concentration, g N/g root
! RATE        !Program control variable to execute code to compute daily rate variables (value=3)
! RANCE       !Root nitrogen concentration at emergence, g N/g root dry weight
! RCNP        !Root critical nitrogen concentration, g N/g root dry weight
! RGFIL       !Array containing base, optimum and maximum temperature to compute RGFILL function
! RGFILL      !Rate of grain fill - mg/day
! RLV(L)      !Root length density for soil layer L, cm root/cm3 soil
! RLWR        !Root length weight ratio
! RMNC        !Root minimum nitrogen concentration (g N/g root dry weight)
! RNLAB
! RNOUT
! ROOTN       !Root nitrogen content, g N/plant
! RSGR        !Relative seed growth rate below which early maturity may occur after RSGRT days
! RSGRT       !Number of consecutive days where minimum relative seed growth rate (RSGR)is not
!              achieved. This results in early maturity due to slow seed growth. days
! RSTAGE      !ISTAGE as a real number for output
! RTWT        !Root weight, g/plant
! RTWTE       !Root weight at emergence, g/plant
! RTWTO       !Root weight, g/m2
! RUE         !Radiation use efficiency, g CH2O/MJ Par        
! ROWSPC      !Row spacing, cm
! RWUEP1      !Factor to modify water stress for cell expansion (in species file), mm/day
! RWUMX       ! Max root water uptake
! SAT(L)      !Saturated water holding capacity for soil layer L, cm3/cm3
! SATFAC      !Watterlogging stress factor (0-1.0)
! SDWT        !Seed weight, g/m2
! SEEDRV      !Carbohydrate reserve in seed, g/plant
! SEEDRVE     !Carbohydrate reserve in seed at emergence, g/plant
! SENLA       !Normal leaf senescence today, cm2/plant
! SFAC        !Drought stress factor for grain nitrogen concentration
! SHF(L)      !Relative root distribution in soil layer L (0-1)
! SHELPC      !Shelling percent
! SI1(6)      !Water stress during a growth stage used for output
! SI2(6)      !Water stress during a growth stage used for output
! SI3(6)      !Nitrogen stress during a growth stage used for output
! SI4(6)      !Nitrogen stress during a growth stage used for output
! SKERWT      !Weight per kernel, g/kernel
! SLA         !Specific leaf area, cm2/g
! SLAN        !Normal leaf senescence since emergence, cm2/plant
! SLFC        !Leaf senescence factor due to competition for light(0-1)
! SLFN        !Leaf senescence factor due to nitrogen stress (0-1)
! SLFP        !Leaf senescence factor due to phosphorus stress (0-1)
! SLFT        !Leaf senescence factor due to temperature (0-1)
! SLFW        !Leaf senescence factor due to water sterss (0-1)
! SNH4(L)     !Ammonium nitrogen in soil layer L, kg N/ha
! SNO3(L)     !Nitrate content in soil layer L, kg N/ha
! SRAD        !Daily solar radiation, MJ/M2/day
! STGDOY(20)  !Year and day of year that a growth stage occurred on
! STMWT       !Stem weight, g/plant
! STMWTE      !Stem weight at emergence, g/plant
! STOVER      !Stover weight (leaf+stem), kg/ha
! STOVN       !Nitrogen content in stover, g N/plant
! STOVWT      !Stover weight (Stem + leaf), g/plant
! SUMDTT      !Sum of GDD for a given stage, C
! SUMEX       !Sum over all layers of water excess factor times depth
!             ! times root length density
! SUMP        !Cumulative plant growth during ISTAGE 4, g/plant
! SUMRL       !Sum of root length density (integral over depth)
! SW(L)       !Soil water content in layer L, cm3 water/cm3 soil
! SWEXF       !Soil water excess factor (0-1.0)
! SWFAC       !Soil water stress effect on growth (0-1), 1 is no stress, 0 is full    
! SWIDOT      !Seed loss due to pests, g seed/m2/day
! SWMAX       !Maximum stem weight (set in phasei) (move to grosub)
! SWMIN       !Minimum value stem weight can reach durign linear grain filling g/plant
! TABEX       !Table lookup function
! TANCE       !Nitrogen content in above ground biomass at emergence, g N/g dry weight
! TANC        !Nitrogen content in above ground biomass, g N/g dry weight
! TAVGD       !Average temperature during daylight hours, C
! TCNP        !Critical nitrogen concentration in tops, g N/g dry weight
! TEMPM       !Mean daily temperature, C
! TFAC        !Temperature stress factor for grain nitrogen concentration
! TFACLG      !Temperature factor for leaf expansion
! TI          !Fraction of a phyllochron interval which occurred as a fraction of today's daily thermal time
! TLNO        !Total number of leaves that the plant produces
! TMAX        !Maximum daily temperture, C
! TMNC        !Plant top minimum N concentration g N/g dry matter
! TNLAB       !Total potential daily plant water uptake, cm/d
! TMIN        !Minimum daily temperature
! TOTNUP      !Total shoot N uptake at maturity, kg N/ha
! TRNU        !Total potential root nitrogen uptake, kg N/ha, converted to g N/pl
! TRWUP
! TSS(L)      !Total days soil layer L has been saturated in excess of PORMIN
! TURFAC      !Soil water stress effect on expansion (0-1), 1 is no stress, 0 is full stress
! UNH4(L)     !Plant uptake of ammonium from layer (kg N/ha/day)
! UNO3(L)     !Plant uptake of nitrate from a layer (kg N/ha/day)
! VMNC        !Plant vegetative minimum nitrogen concentration, g N/g plant
! VANC        !Plant vegetative actual N concentration, g N/g plant
! WLIDOT      !Leaf loss due to pests, g/m2/day
! VSTAGE      !Vegetative growth stage (real number) used for output
! WRIDOT      !Root loss due to pests, g root/m2/day
! WSIDOT      !Stem loss due to pests, g stem/m2/day


! WTNCAN      !Weight of nitrogen in above ground biomass (stem, leaf, grain), kg N/ha
! WTNLF       !Weight of nitrogen in leaf tissue, kg N/ha
! WTNSD       !Weight of nitrogen in seed, g[N] / m2[ground]
! WTNST       !Weight of nitrogen in stem tissue, kg N/ha
! WTNUP       !Total N uptake, g/m2
! WTNVEG      !Weight of nitrogen in vegetative tissue, kg N/ha
! XANC        !Nitrogen concentration in above ground biomass %
! XHLAI       !Healthy leaf area index used to compute transpiration in water balance routine, m2/m2
! XLFWT       !New leaf weight today, g/plant
! XGNP        !Nitrogen content of grain, %
! XN          !Number of oldest expanding leaf
! XNF         !Modified nitrogen factor based on critical N concentration in vegetative biomass
! XNTI        !Number of leaves at tassel initiation
! XSTAGE      !Non-integer growth stage indicator
! YIELD       !Yield in kg/ha at 0% moisture content
! YRDOY       !Year and day of year


