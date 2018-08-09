!======================================================================
!  MZ_GROSUB, Subroutine
!
!  Maize growth routine
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
!----------------------------------------------------------------------
!
!  Called : MAIZE
!
!  Calls  : NFACTO NUPTAK
!----------------------------------------------------------------------

      SUBROUTINE MZ_GROSUB (DYNAMIC, ISWITCH, 
     &      ASMDOT, CDAY, CO2, DLAYR, DS, DTT, EOP, FILEIO,   !Input
     &      FracRts, ISTAGE, KG2PPM, LL, NLAYR, NH4, NO3, P3, !Input
     &      PLTPOP, PPLTD, RLV, RTDEP, RUE, SAT, SeedFrac,    !Input
     &      SHF, SLPF, SPi_AVAIL, SRAD, STGDOY, SUMDTT, SW,   !Input
     &      SWIDOT, TLNO, TMAX, TMIN, TRWUP, TSEN, VegFrac,   !Input
     &      WLIDOT, WRIDOT, WSIDOT, XNTI, XSTAGE,             !Input
     &      YRDOY, YRPLT, SKi_Avail,                          !Input
     &      EARS, GPP, MDATE,HARVFRAC,                        !I/O
     &      AGEFAC, APTNUP, AREALF, CANHT, CANNAA, CANWAA,    !Output
     &      CANWH, CARBO, GNUP, GPSM, GRNWT, GRORT, HI, HIP,  !Output
     &      LEAFNO, NSTRES, PCNGRN, PCNL, PCNRT, PCNST,       !Output
     &      PCNVEG, PHINT, PODNO, PConc_Root, PConc_Seed,     !Output
     &      PConc_Shel, PConc_Shut, PODWT, PORMIN, PSTRES1,   !Output
     &      PSTRES2, PTF, PUptake, RLWR, ROOTN, RSTAGE, RTWT, !Output
     &      RTWTO, RWUMX, SATFAC, SDWT, SEEDNO, SENESCE,      !Output
     &      SHELPC, SI1, SI3, SKERWT, SLA, STMWTO, STOVER,    !Output
     &      STOVN, STOVWT, SUMP, SWFAC, TOPWT, TURFAC, UNH4,  !Output
     &      UNO3, VSTAGE, WTLF, WTNCAN, WTNLF, WTNSD, WTNST,  !Output
     &      WTNUP, WTNVEG, XGNP, XHLAI, XLAI, XN, YIELD,      !Output
     &      KUptake, KSTRES)                                  !Output

      USE ModuleDefs
      USE Interface_SenLig_Ceres
      IMPLICIT  NONE
      SAVE
!----------------------------------------------------------------------
!                         Variable Declaration
!----------------------------------------------------------------------

      REAL        AGEFAC            
      REAL        APTNUP      
      REAL        AREALF
      REAL        ASGDD
      REAL        ASMDOT
      REAL        BIOMAS 
      REAL        BSGDD
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
      REAL        CUMPH       
      REAL        CO2X(10)    
      REAL        CO2Y(10)    
      REAL        CO2         
      REAL        CSD1        
      REAL        CSD2        
      REAL        CUMDTTEG      
      REAL        CURV
      REAL        DLAYR(NL)   
      REAL        DS(NL)   
!      REAL        DM          
      INTEGER     DOY         
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
      PARAMETER       (ERRKEY='MZ_GRO')   
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
      REAL        HIP
      INTEGER     ICOLD       
      INTEGER     ICSDUR      
      CHARACTER   IDETO*1     
      REAL        IPAR
      INTEGER     ISECT
      INTEGER     ISTAGE    
      CHARACTER*1 ISWNIT
      CHARACTER*1 ISWWAT      
      CHARACTER*1 ISWPOT      
!      REAL        K1
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
      REAL        P3         
      REAL        P5          
      REAL        PAR         
      REAL        PARSR
      CHARACTER*80    PATHCR 
      CHARACTER*80    PATHSR
      CHARACTER*80    PATHER 
      REAL        PC          
      REAL        PCARB
      REAL        PCNGRN
      REAL        PCNL 
      REAL        PCNRT     
      REAL        PCNSD
      REAL        PCNVEG      
      REAL        PCNST
      REAL        PCO2        
      REAL        PDWI        
      REAL        PRFTC(4)
      REAL        SLPF
      REAL        PGRORT      
      REAL        PHINT       
      REAL        PLA    
      REAL        PLAE     
      REAL        PLAG        
      REAL        PLAS        
      REAL        PLTPOP      
      REAL        PODNO
      REAL        PODWT
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
      REAL        YIELDB      
      INTEGER     YR, YRDOY    

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
       
      TYPE (ResidueType) SENESCE 
      TYPE (SwitchType)  ISWITCH
  
!----------------------------------------------------------------------
!     CHP 3/31/2006
!     Proportion of lignin in STOVER and Roots
      REAL PLIGLF, PLIGRT

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
            READ(LUNIO,60,IOSTAT=ERR) PLTPOP,ROWSPC ; LNUM = LNUM + 1
 60         FORMAT(25X,F5.2,13X,F5.2,7X,F5.2)
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
          ENDIF
!     -----------------------------------------------------------------
!             Read crop cultivar coefficients
!     -----------------------------------------------------------------
          SECTION = '*CULTI'
          CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
          IF (FOUND .EQ. 0) THEN
              CALL ERROR(SECTION, 42, FILEIO, LNUM)
          ELSE
            READ (LUNIO,1800,IOSTAT=ERR) VARNO,VRNAME,ECONO,
     %                 P1,P2,P5,G2,G3,PHINT  
!CHP 1800        FORMAT (A6,1X,A16,1X,A6,1X,F6.1,F6.3,2(F6.1),2(F6.2))    
1800        FORMAT (A6,1X,A16,1X,A6,1X,6F6.0)    
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
          ENDIF

        CLOSE(LUNIO)

!         ************************************************************
!         ************************************************************
! 
!                 READ SPECIES FILE
!         ************************************************************
!         ************************************************************


      FILECC =  TRIM(PATHSR) // FILES
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)

      !----------------------------------------------------------------
      !       Find and Read TEMPERATURE Section
      !----------------------------------------------------------------

      SECTION = '*TEMPE'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(7X,4(1X,F5.2))',IOSTAT=ERR)
     &       PRFTC(1),PRFTC(2),PRFTC(3),PRFTC(4)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(7X,4(1X,F5.2))',IOSTAT=ERR) RGFIL(1),
     &     RGFIL(2),RGFIL(3),RGFIL(4)
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
     &   CO2X(3), CO2X(4),CO2X(5), CO2X(6),CO2X(7),CO2X(8),
     &   CO2X(9),CO2X(10)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(7X,10(1X,F5.2))',IOSTAT=ERR) CO2Y(1),CO2Y(2),
     &   CO2Y(3), CO2Y(4),CO2Y(5), CO2Y(6),CO2Y(7),CO2Y(8),
     &   CO2Y(9),CO2Y(10)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      ENDIF
      REWIND(LUNCRP)
      !----------------------------------------------------------------
      !        Find and Read Stress Response
      !----------------------------------------------------------------
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
      !----------------------------------------------------------------
      !        Find and Read Seed Growth Parameters
      !----------------------------------------------------------------
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
      !----------------------------------------------------------------
      !        Find and Read Emergence Initial Conditions
      !----------------------------------------------------------------
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

      !----------------------------------------------------------------
      !        Find and Read Plant Nitrogen Parameters
      !----------------------------------------------------------------
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
!          TCNP = EXP (1.52  - 0.160 * XSTAGE) / 100.0  
!          TCNP = EXP (CTCNP1 - CTCNP2 * XSTAGE) / 100.0  
!       Default values: CTCNP1 = 1.52; CTCNP2 = 0.160
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F8.3)',IOSTAT=ERR) CTCNP1
!       If error reading either value, use defaults
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

      !----------------------------------------------------------------
      !        Find and Read Root parameters
      !----------------------------------------------------------------
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

!** Initialize variables

      CALL SenLig_Ceres(PLIGLF=PLIGLF, PLIGRT=PLIGRT)

! JIL 08/01/2006 parameters for ear growth 
!** Read in SPE file?

          ASGDD = 100.0        !Ending of ear growth (gdd)
          BSGDD = 250.0        !Beginning of ear growth (gdd)

          APTNUP = 0.0
          AREALF = 0.0
          ASMDOT = 0.0  
          BIOMAS = 0.0
          CANHT  = 0                
!**
!         set to 1.6 m, need to become user-input; RS 26May04   
          CANHT_POT = 1.6              
!**
          CANNAA = 0.0
          CANWAA = 0.0
          CANWH  = 0  
          CARBO  = 0.0
!          CSD1   = 0.0
!          CSD2   = 0.0
          CMAT   = 0
          CUMDTTEG=0.0
          CumLeafSenes = 0.0
          CumLeafSenesY = 0.0
          CumLfNSenes = 0.0
          CUMPH  = 0.0
!          DM     = 0.0
          DUMMY  = 0.0
          EARS   = 0.0 
          EARWT  = 0.0
          EMAT   = 0
          EP1    = 0.0
          GRAINN = 0.0
          GRF    = 0.0
          GNP = 0.0
          GNUP   = 0.0
          GRNWT  = 0.0
          GROEAR = 0.0
          GROGRN = 0.0
          GPSM   = 0.0
          GROLF  = 0.0
          GRORT  = 0.0
          GROSTM = 0.0
          HI     = 0.0
          HIP    = 0.0
          ICOLD  = 0
          ICSDUR = 0
          IPAR   = 0.0
!         K1     = 0.0
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
          PDWI   = 0.0
          PLA = 0.0
          PLAG   = 0.0
          PLAS = 0.0
          PODNO  = 0.0
          PODWT  = 0.0
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
          SKERWT = 0.0
          SLAN = 0.0
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
          TI     = 0.0
          TLNO   = 0.0
          TNLAB  = 0.0
          TOPWT  = 0.0
          TOTNUP = 0.0
          TRNU   = 0.0
          DO L=1,NLAYR
              TSS(L) = 0.0
          ENDDO
          TURFAC = 1.0
          UNO3   = 0.0      !CHP 1/3/2013
          UNH4   = 0.0      !CHP 1/3/2013
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
          YIELDB = 0.0

          IF (ISWNIT .NE. 'N') THEN
             CALL MZ_NFACTO(DYNAMIC,TANC,TCNP,TMNC,
     %       AGEFAC,NDEF3,NFAC,NSTRES)         
          ELSE
              AGEFAC = 1.0
              NSTRES = 1.0
              NDEF3 = 1.0
              NFAC = 1.0
          ENDIF

          CALL MZ_NUPTAK(
     %      RANC, ROOTN,RTWT,TANC,STOVN,STOVWT,TRNU,NLAYR,
     %      RLV,NO3,NH4,PDWI,TCNP,UNO3,UNH4,
     %      XSTAGE,RCNP,PGRORT,PLTPOP,SW,LL,SAT,DLAYR,
     %      SHF,PTF, SENESCE, KG2PPM, PLIGRT)

          CALL MZ_KUPTAK(
     &       ISWPOT, NLAYR, SKi_Avail, UNH4, UNO3,        !Input
     &       KUPTAKE, KSTRES)                             !Output

          CALL P_Ceres (DYNAMIC, ISWPHO,                    !Input
     &      CumLeafSenes, DLAYR, DS, FILECC, MDATE, NLAYR,  !Input
     &      PCNVEG, PLTPOP, PODWT, RLV, RTDEP, RTWTO,       !Input
     &      SDWT, SWIDOT, SeedFrac, SPi_AVAIL, Stem2Ear,    !Input
     &      STMWTO, VegFrac, WLIDOT, WRIDOT, WSIDOT,        !Input
     &      WTLF, YRPLT,                                    !Input
     &      SENESCE,                                        !I/O
     &      PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, !Output
     &      PStres1, PStres2, PUptake, FracRts)             !Output

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

!         Keep yesterdays cumulative senescence 
          CumLeafSenesY = CumLeafSenes

!      Need to set these at the beg. of each new growth stage. From phasei
!      Initialize variables at the beginning of a new phenological stage
!      This code was moved from phenol or phasei in Generic CERES v 3.9


          IF (YRDOY == STGDOY(1) .OR. YRDOY == STGDOY(2) .OR. 
     &        YRDOY == STGDOY(5) .OR. YRDOY == STGDOY(6) .OR. 
     &        YRDOY == STGDOY(7) .OR. YRDOY == STGDOY(8)) THEN 
            ICSDUR = 1
          ENDIF

          !IF(YRDOY.EQ.STGDOY(1)) THEN
          !    CNSD1  = 0.0
          !    CNSD2  = 0.0
          !    CPSD1  = 0.0
          !    CPSD2  = 0.0
          !    CSD1   = 0.0
          !    CSD2   = 0.0
          !    ICSDUR = 1
          !ENDIF

          IF(YRDOY.EQ.STGDOY(2)) THEN
          !    CNSD1  = 0.0
          !    CNSD2  = 0.0
          !    CPSD1  = 0.0
          !    CPSD2  = 0.0
          !    CSD1   = 0.0
          !    CSD2   = 0.0
          !    ICSDUR = 1
! JIL 05/02/2006 Assuming initially 1 ear/plant (later is calculated)
              EARS   = PLTPOP

          ENDIF


          IF(YRDOY.EQ.STGDOY(3)) THEN
! JIL 03/07/2006 remove ear DW initialization
!              EARWT  = 0.167*STMWT    
!              STMWT  = STMWT - EARWT      

              SWMIN  = STMWT*0.85         
              SUMP   = 0.0
              !CNSD1  = 0.0
              !CNSD2  = 0.0
              !CPSD1  = 0.0
              !CPSD2  = 0.0
              !CSD1   = 0.0
              !CSD2   = 0.0
              ICSDUR = 1
              CANNAA = STOVN*PLTPOP
              CANWAA = BIOMAS
          ENDIF

          IF(YRDOY.EQ.STGDOY(4)) THEN
              SWMAX  = STMWT          
              VANC   = TANC       
              VMNC   = TMNC       
              EMAT   = 0
              !CNSD1  = 0.0
              !CNSD2  = 0.0
              !CPSD1  = 0.0
              !CPSD2  = 0.0
              !CSD1   = 0.0
              !CSD2   = 0.0
              ICSDUR = 1
          ENDIF

          !IF(YRDOY.EQ.STGDOY(5)) THEN
          !    CNSD1  = 0.0
          !    CNSD2  = 0.0
          !    CPSD1  = 0.0
          !    CPSD2  = 0.0
          !    CSD1   = 0.0
          !    CSD2   = 0.0
          !    ICSDUR = 1
          !ENDIF

          !IF(YRDOY.EQ.STGDOY(6)) THEN
          !    CNSD1  = 0.0
          !    CNSD2  = 0.0
          !    CPSD1  = 0.0
          !    CPSD2  = 0.0
          !    CSD1   = 0.0
          !    CSD2   = 0.0
          !    ICSDUR = 1
          !ENDIF

          !IF(YRDOY.EQ.STGDOY(7)) THEN
          !    CNSD1  = 0.0
          !    CNSD2  = 0.0
          !    CPSD1  = 0.0
          !    CPSD2  = 0.0
          !    CSD1   = 0.0
          !    CSD2   = 0.0
          !    ICSDUR = 1
          !ENDIF

          !IF(YRDOY.EQ.STGDOY(8)) THEN
          !    CNSD1  = 0.0
          !    CNSD2  = 0.0
          !    CPSD1  = 0.0
          !    CPSD2  = 0.0
          !    CSD1   = 0.0
          !    CSD2   = 0.0
          !    ICSDUR = 1
          !ENDIF

          IF(YRDOY.EQ.STGDOY(9)) THEN
              !Emergence
              STMWT   = STMWTE          
              RTWT    = RTWTE        
              LFWT    = LFWTE         
              STOVWT  = LFWT+STMWT     
              SEEDRV  = SEEDRVE
              BIOMAS = STOVWT
              LEAFNO = INT(LEAFNOE)       
              PLA     = PLAE
              SENLA  = 0.0
              LAI    = PLTPOP*PLA*0.0001 
              CUMPH   = 0.514
              IF (ISWNIT .NE. 'N') THEN
                  GRAINN = 0.000
                  TANC = TANCE
                  RANC = RANCE   !Initialize root N (fraction) JIL
                  ROOTN = RANC   * RTWT
                  STOVN = STOVWT * TANC
              ENDIF

              WTLF = LFWT * PLTPOP      !Leaf weight, g/m2
              STMWTO = STMWT * PLTPOP   !Stem weight, g/m2
              RTWTO = RTWT * PLTPOP     !Root weight, g/m2
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

!         CHP 7/29/2008
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
!          TCNP = EXP (1.52-0.160*XSTAGE)/100.0  
          TCNP = EXP (CTCNP1 - CTCNP2 * XSTAGE) / 100.0  
! SPE          TMNC = 0.0045                        
!          TMNC = TMNCO    !Where is TMNCO given value?
          IF (XSTAGE .LT. 4.0) THEN
              TMNC =  (1.25-0.200*XSTAGE)/100.0 
          ENDIF

          !-------------------------------------------------------------
          !        Compute Nitrogen Stress Factors 
          !-------------------------------------------------------------
          IF (ISWNIT .NE. 'N' .AND. ISTAGE .LT. 7) THEN
              CALL MZ_NFACTO(DYNAMIC,                 !Control
     %        TANC,TCNP,TMNC,                         !Inputs
     %        AGEFAC,NDEF3,NFAC,NSTRES)               !Outputs
          ELSE
              AGEFAC = 1.0
              NSTRES = 1.0
              NDEF3 = 1.0
              NFAC = 1.0
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

          !-------------------------------------------------------------
          !                Daily Photosynthesis Rate
          ! ------------------------------------------------------------
          !Compare with weather module: PAR = SRAD * 2.0  chp
          PAR = SRAD*PARSR    !PAR local variable

          LIFAC  = 1.5 - 0.768 * ((ROWSPC * 0.01)**2 * PLTPOP)**0.1 
          PCO2  = TABEX (CO2Y,CO2X,CO2,10)

! JIL 08/01/2006 Intercepted PAR (MJ/plant d)
          IPAR = PAR/PLTPOP * (1.0 - EXP(-LIFAC * LAI))
          PCARB = IPAR * RUE * PCO2

!-SPE     PRFT= AMIN1(1.25 - 0.0035*((0.25*TMIN+0.75*TMAX)-25.0)**2,1.0) 
          TAVGD = 0.25*TMIN+0.75*TMAX


!**************************************************************************
!**************************************************************************
!     CHP inserted old code for comparison of methods:
!**************************************************************************
!     DSSAT V3.5 CODE:
!      PRFT  = 1.0 - 0.0025*((0.25*TMIN+0.75*TMAX)-26.0)**2

!**************************************************************************
!     DSSAT V3.5 CODE modified:
      !
      ! CIMMYT 99-- JTR, US
      !
!      PRFT  = AMIN1(1.25 - 0.0035*((0.25*TMIN+0.75*TMAX)-25.0)**2,1.0)

!**************************************************************************
!     DSSAT V4.0 CODE:
          PRFT = CURV('LIN',PRFTC(1),PRFTC(2),PRFTC(3),PRFTC(4),TAVGD)
          PRFT  = AMAX1 (PRFT,0.0)
          PRFT = MIN(PRFT,1.0)

!**************************************************************************
!**************************************************************************


!** WDB   Water logging reduces root water uptake and creates water stress
!** WDB   through SWFAC. Thus, we do not need to also create a reduction
!** WDB   using SATFAC
!** WDB 10/20/03 CARBO = PCARB*AMIN1 (PRFT,SWFAC,NSTRES,(1.0-SATFAC))*SLPF
!          CARBO = PCARB*AMIN1 (PRFT,SWFAC,NSTRES)*SLPF

!     CHP 9/5/04 Added P stress
          CARBO = PCARB*AMIN1 (PRFT,SWFAC,NSTRES, PStres1,KSTRES)*SLPF
          !Reduce CARBO for assimilate pest damage
          CARBO = CARBO - ASMDOT
          CARBO = MAX(CARBO,0.0)

          IF (ISTAGE .EQ. 1 .OR. ISTAGE .EQ. 2 .OR. ISTAGE .EQ. 3) THEN
              !Allow first 5 leaves to expand faster than Phyllochron 
              !interval PHINT by reducing PC
              PC     = 1.0
              IF (CUMPH .LT. 5.0) THEN
                  PC  = 0.66+0.068*CUMPH      
                  !Sets PC to 0.7 for leaf 1 and 1.0 for leaf 5
              ENDIF

              TI     = DTT/(PHINT*PC)
             !CUMPH  = CUMPH + DTT/(PHINT*PC)
              CUMPH  = CUMPH + TI
              XN     = CUMPH + 1.0
              LEAFNO = INT(XN)
          ENDIF


          !-------------------------------------------------------------
          !   ISTAGE = 1 (Emergence to End of Juvenile Stage)
          !-------------------------------------------------------------
          IF (ISTAGE .EQ. 1) THEN
              !!Allow first 5 leaves to expand faster than Phyllochron 
              !!interval PHINT by reducing PC
              !PC     = 1.0
              !IF (CUMPH .LT. 5.0) THEN
              !    PC  = 0.66+0.068*CUMPH      
              !    !Sets PC to 0.7 for leaf 1 and 1.0 for leaf 5
              !ENDIF
              !
              !TI     = DTT/(PHINT*PC)
              !CUMPH  = CUMPH + DTT/(PHINT*PC)
              !XN     = CUMPH + 1.0
              !LEAFNO = INT(XN)

              ! PLAG - Leaf area growth, cm2/plant/day
!             5/9/07 CHP, JIL, KJB include AGEFAC and PStres2 here
!               Take the minimum of TURFAC and SATFAC, not the product
!             PLAG   = 3.0*XN*XN*TI*TURFAC*(1.0-SATFAC)    

!             10/10/2007 CHP, JIL
!             Remove AGEFAC -- too much N stress too early.

!              PLAG   = 3.0*XN*XN*TI*
!     &              AMIN1(TURFAC,(1.0-SATFAC),PStres2,AGEFAC)
              PLAG   = 3.0*XN*XN*TI*
     &              AMIN1(TURFAC,(1.0-SATFAC),PStres2, KSTRES)

              IF (XN .LT. 4.0) THEN           
               ! Allow first 4 leaves to expand 30% faster
                  ! XN - number of oldest expanding leaf
!                PLAG = 4.0*XN*TI*TURFAC*(1.0-SATFAC)     
!                  PLAG = 4.0*XN*TI*
!     &              AMIN1(TURFAC,(1.0-SATFAC),PStres2,AGEFAC) 
                  PLAG = 4.0*XN*TI*
     &              AMIN1(TURFAC,(1.0-SATFAC),PStres2, KSTRES) 
              ENDIF

              PLA    = PLA + PLAG
              ! PLA - Total plant leaf area, cm2/plant
              XLFWT  = (PLA/250.0)**1.25      
              ! XLFWT - new leaf weight today, g/plant
              XLFWT  = AMAX1 (XLFWT,LFWT)
              GROLF  = XLFWT - LFWT           
              ! GROLF - leaf growth rate, g/plant/day
              !LFWT is initialized in phasei at emergence to 0.2 g/plant
              GRORT  = CARBO - GROLF          
              ! GRORT - Daily root growth, g/plant/day

              IF (GRORT .LE. 0.25*CARBO) THEN
                  GRORT  = CARBO*0.25
                  SEEDRV = SEEDRV + CARBO - GROLF - GRORT 
                  !SEEDRV - reserve carbohydrate in seed for use
                  !      by plant in seedling stage g/plant
                  IF (SEEDRV .LE. 0.0) THEN
                      SEEDRV = 0.0
                      GROLF  = CARBO*0.75
                      PLA    = (LFWT+GROLF)**0.8*267.0
                  ENDIF
              ENDIF

              LFWT   = LFWT + GROLF
              IF (GROLF .GT. 0.0) THEN
                SLAN = SUMDTT*PLA/10000.0   
               !SLAN - normal leaf senescence since emergence, cm2/plant
              ENDIF
              LFWT   = LFWT - SLAN/600.0      ! Subtract respiration
!             CHP - 5/11/2005 why use 600 here?  Why not convert with SLA?

!             5/11/2005 CHP Added cumulative leaf senescence
              CumLeafSenes = SLAN / 600. * PLTPOP * 10.
!                kg/ha     =  g/plant * plants/m2 * (kg/ha)/(g/m2)

          !-------------------------------------------------------------
          !         ISTAGE = 2 (End of Juvenile to Tassel Initiation)
          !-------------------------------------------------------------

          ELSEIF (ISTAGE .EQ. 2) THEN

              !!Allow first 5 leaves to expand faster than Phyllochron 
              !!interval PHINT by reducing PC
              !PC     = 1.0
              !IF (CUMPH .LT. 5.0) THEN
              !    PC  = 0.66+0.068*CUMPH      
              !    !Sets PC to 0.7 for leaf 1 and 1.0 for leaf 5
              !ENDIF
!
              !TI     = DTT/(PHINT*PC)
              !CUMPH  = CUMPH + DTT/(PHINT*PC)
              !XN     = CUMPH + 1.0
              !LEAFNO = INT(XN)

!             CHP 9/5/04 Added Pstres2
              PLAG   = 3.5*XN*XN*TI
     &                *AMIN1(AGEFAC,TURFAC,(1.0-SATFAC),PStres2, KSTRES)
!              PLAG   = 3.5*XN*XN*TI*AMIN1(AGEFAC,TURFAC,(1.0-SATFAC))
              PLA    = PLA   + PLAG
              XLFWT  = (PLA/267.0)**1.25
              GROLF  = XLFWT - LFWT

              IF (GROLF .GE. CARBO*0.75) THEN
                  GROLF = CARBO*0.75
                  PLA   = (LFWT+GROLF)**0.8*267.0
              ENDIF

              GRORT  = CARBO - GROLF
              LFWT   = LFWT  + GROLF
              SLAN   = SUMDTT*PLA/10000.0
              LFWT   = LFWT  - SLAN/600.0

!             5/11/2005 CHP Added cumulative leaf senescence
              CumLeafSenes = SLAN / 600. * PLTPOP * 10.
!                kg/ha     =  g/plant * plants/m2 * (kg/ha)/(g/m2)

!             Stage 3 accumulates leaf senescence from 0
!             Save stage 2 value for true accumulation
              Stg2CLS = CumLeafSenes

!          -------------------------------------------------------------------
!             ISTAGE=3 (Tassel Initiation to End of Leaf Growth and Silking)
!          -------------------------------------------------------------------

          ELSEIF (ISTAGE .EQ. 3) THEN

!             JIL 08/11/2005
!             Do not allow increase in leaf number during 
!               expansion of final leaf.
              IF (SUMDTT > P3 - (2. * PHINT)) THEN
                CUMPH = CUMPH - TI
              ENDIF

              XN     = CUMPH + 1.0
              LEAFNO = INT(XN)

! JIL 04/06/2006 Ear grows starting at thermal time BSGDD
              IF (SUMDTT .GE. P3-BSGDD) THEN
                CUMDTTEG = SUMDTT - (P3-BSGDD)
                GROEAR = (0.81/(1.0+EXP(-0.02*(CUMDTTEG-210.0))))*CARBO
!               ! Fraction: 0.0 - 0.56 at silking 

               ELSE
                CUMDTTEG = 0.0
                GROEAR = 0.0
              ENDIF

!     chp 9/5/04 Added PStres2 to PLAG, GROSTM calcs
              IF (XN .LT. 12.0) THEN
!                  PLAG   = 3.5*XN*XN*TI*AMIN1(AGEFAC,TURFAC,(1.-SATFAC))
                  PLAG   = 3.5*XN*XN*TI
     &                *AMIN1(AGEFAC,TURFAC,(1.-SATFAC),PStres2, KSTRES)
                  GROLF  = 0.00116*PLAG*PLA**0.25
                  GROSTM = GROLF*0.0182*(XN-XNTI)**2
!CHP 3/22/05  ELSEIF (XN .LT. TLNO-3.0) THEN
              ELSEIF (XN .LT. TLNO-2.9999) THEN
!                 PLAG   = 3.5*170.0*TI*AMIN1(AGEFAC,TURFAC,(1.-SATFAC))
                  PLAG   = 3.5*170.0*TI
     &                *AMIN1(AGEFAC,TURFAC,(1.-SATFAC),PStres2, KSTRES)
                  GROLF  = 0.00116*PLAG*PLA**0.25
                  GROSTM = GROLF*0.0182*(XN-XNTI)**2
              ELSE
                  PLAG   = 170.0*3.5/((XN+3.0-TLNO)**0.5)*TI*
!     &                     AMIN1(AGEFAC,TURFAC,(1.0-SATFAC))
     &                 AMIN1(AGEFAC,TURFAC,(1.0-SATFAC),PStres2, KSTRES)
                  GROLF  = 0.00116*PLAG*PLA**0.25
!                  GROSTM = 3.000*3.1*TI*AMIN1(AGEFAC,TURFAC,(1.-SATFAC))
                  GROSTM = 3.000*3.1*TI
     &                *AMIN1(AGEFAC,TURFAC,(1.-SATFAC),PStres2, KSTRES)
              ENDIF

! JIL 03/07/2006 Ear dry weight was simulated as part of stem dry weight during this stage. 
!                Substract ear growth rate from stem growth rate
              IF (GROSTM .GT. GROEAR) THEN
                 GROSTM = GROSTM - GROEAR
                ELSE
                 GROEAR = GROSTM * 0.5
                 GROSTM = GROSTM * 0.5
              ENDIF

              GRORT  = CARBO - GROLF - GROSTM - GROEAR

!JIL 08/11/2005 IF (GRORT .LE. 0.08*CARBO .AND. TURFAC .GT. 0.0) THEN
              IF (GRORT .LE. 0.10*CARBO .AND. TURFAC .GT. 0.0) THEN
                  IF (GROLF .GT. 0.0 .OR. GROSTM .GT. 0.0) THEN
                      GRF   = CARBO*0.90/(GROSTM+GROLF+GROEAR)
                      GRORT = CARBO*0.10
                  ELSE
                      GRF   = 1.0
                  ENDIF
                  GROLF  = GROLF*GRF
                  GROSTM = GROSTM*GRF
                  GROEAR = GROEAR*GRF
              ENDIF

              PLA    = (LFWT+GROLF)**0.8*267.0
              LFWT   = LFWT  + GROLF 
              SLAN   = PLA/1000.0
              LFWT   = LFWT  - SLAN/600.0    !g/plant
              STMWT  = STMWT + GROSTM
              EARWT  = EARWT + GROEAR

!             5/11/2005 CHP Added cumulative leaf senescence
!             7/22/2005 CHP Need to add leaf senescence from Stage 2 for
!                           total accumulation.
              CumLeafSenes = SLAN / 600. * PLTPOP * 10. + Stg2CLS
!                kg/ha     =  g/plant * plants/m2 * (kg/ha)/(g/m2)

!      --------------------------------------------------------------------
!         ISTAGE = 4 (Silking to beginning of effective grain filling period)
!      --------------------------------------------------------------------

          ELSEIF (ISTAGE .EQ. 4) THEN
!              GROEAR = 0.22*DTT*AMIN1(AGEFAC,TURFAC,(1.0-SATFAC))
              CUMDTTEG = CUMDTTEG + DTT
              GROEAR = 0.22*DTT
!     chp 9/5/04 Added PStres2 to PLAG, GROSTM calcs
              GROEAR = (0.81/(1.0+EXP(-0.02*(CUMDTTEG-210.0))))*CARBO
     &                *AMIN1(AGEFAC,TURFAC,(1.0-SATFAC),PStres2, KSTRES)

              GRORT = CARBO * 0.08
              IF (CARBO .GT. GROEAR+GRORT) THEN
                 GROSTM = CARBO - GROEAR - GRORT
                ELSE
                 GRORT=(CARBO-GROEAR)*0.5
                 GROSTM=(CARBO-GROEAR)*0.5
              ENDIF

!             chp 5/12/2005   
              GROLF = 0.0

              SLAN   = PLA*(0.05+SUMDTT/200.0*0.05)
              LFWT   = LFWT  - SLAN/600.0
              EARWT  = EARWT + GROEAR
              STMWT  = STMWT + GROSTM
              SUMP   = SUMP  + CARBO

!             5/11/2005 CHP Added cumulative leaf senescence
              CumLeafSenes = SLAN / 600. * PLTPOP * 10. + Stg2CLS
!                kg/ha     =  g/plant * plants/m2 * (kg/ha)/(g/m2)

          !-------------------------------------------------------------
          !   ISTAGE = 5 Effective Grain Filling Period
          !-------------------------------------------------------------

          ELSEIF (ISTAGE .EQ. 5) THEN

              IF (PLTPOP .LE. 0.01) RETURN

              IF (ABS(CARBO) .GT. 0.0001) THEN        !<--------------!
                  CMAT = 0
                  SLAN   = PLA*(0.1+0.60*(SUMDTT/P5)**3)              !
!JIL 08/11/2005 Remove this
!                  LFWT   = LFWT*0.9990                                !


!**************************************************************************
!**************************************************************************
!     CHP inserted old code for comparison of methods:
!**************************************************************************
!     DSSAT V3.5 CODE:
!              RGFILL = 1.0 - 0.0025*(TEMPM-26.0)**2               !

!**************************************************************************
!     DSSAT V3.5 CODE modified:
             !
             ! CIMMYT 99 --JTR,US
             !
!             RGFILL = 1.4-0.003*(TEMPM-27.5)**2

!**************************************************************************
!     DSSAT V4.0 CODE:
!-SPE             RGFILL = 1.4-0.003*(TEMPM-27.5)**2                  !
                  RGFILL = CURV('LIN',RGFIL(1),RGFIL(2),RGFIL(3),     !
     $                     RGFIL(4),TEMPM)                            !
                  RGFILL = AMIN1(1.0,RGFILL)                          !
                  RGFILL = AMAX1(0.0,RGFILL)                          !

!**************************************************************************
!**************************************************************************

                  GROGRN = RGFILL*GPP*G3*0.001*(0.45+0.55*SWFAC)      !

                  IF (RGFILL.GT.RSGR) THEN   
                      EMAT  = 0                                       !
                      GRORT = 0.0                                     !
                  ELSE                                                !
                      EMAT = EMAT + 1                                 !
                      IF (FLOAT(EMAT).LE.RSGRT) THEN 
                          GRORT  = 0.0                                !
                      ELSE                                            !
                          SUMDTT = P5                                 !
                          CALL YR_DOY(YRDOY, YR, DOY)
                          WRITE(MESSAGE(1),2700) DOY                  !
                          CALL WARNING(1,ERRKEY, MESSAGE)             !
                          WRITE (     *,2700) DOY                     !
                          IF (IDETO .EQ. 'Y') THEN                    !
                              WRITE (NOUTDO,2700) DOY                 !
                          ENDIF                                       !
                          EMAT   = 0                                  !
                          GRORT  = 0.0                                !
                      ENDIF                                           !
                  ENDIF                                               !
              ELSE                                                    !
                  CMAT = CMAT + 1
                  IF (CMAT.GE.CARBOT) THEN
                  SUMDTT = P5                                         !
                  CALL YR_DOY(YRDOY, YR, DOY)
                  WRITE(MESSAGE(1),2700) DOY                          !
                  CALL WARNING(1,ERRKEY, MESSAGE)                     !
                  WRITE (     *,2700) DOY                             !
                  IF (IDETO .EQ. 'Y') THEN                            !
                      WRITE (NOUTDO,2700) DOY                         !
                  ENDIF                                               !
                  EMAT   = 0                                          !
                  GRORT  = 0.0                                        !
                  ENDIF
              ENDIF           !<--------------------------------------!

              GROSTM = CARBO - GROGRN

              IF (GROSTM .GE. 0.0) THEN           !<------------------!
                  STMWT  = STMWT + GROSTM*0.50                        !
                  GRORT  = GROSTM*0.50                                !
              ELSE                                                    !
                  STMWT  = STMWT + CARBO - GROGRN                     !
                  IF (STMWT .LE. SWMIN*1.07) THEN                     !
                      STMWT  = STMWT + LFWT*0.0050                    !
                      IF (STMWT .LT. SWMIN) THEN                      !
                          STMWT  = SWMIN                              !
                          GROGRN = CARBO                              !
                      ENDIF                                           !
                  ENDIF                                               !
              ENDIF                               !<------------------!


              IF (ISWNIT .NE. 'N') THEN     !<------------------------!
                                                                      !
           ! Grain N allowed to vary between .01 and .018.            !
           ! High temp., low soil water, and high N increase grain N  !
                                                                      !
                  SFAC   = 1.125 - 0.1250*TURFAC                      !
                  TFAC   = 0.690 + 0.0125*TEMPM                       !
                  GNP    = (0.004+ 0.0130*NFAC)*AMAX1(SFAC,TFAC)      !
                  NSINK  = GROGRN*GNP                                 !
                                                                      !
                  IF (NSINK .NE. 0.0) THEN                            !
                      RMNC   = 0.75*RCNP                              !

                      IF (RANC .LT. RMNC) THEN                        !
                          RANC = RMNC                                 !
                      ENDIF                                           !
                                                      
                      VANC   = STOVN/STOVWT                           !
                      IF (VANC .LT. VMNC) THEN                        !
                          VANC = VMNC                                 !
                      ENDIF                                           !
                                                                      !
                      NPOOL1 = STOVWT*(VANC-VMNC)                     !
                      NPOOL2 = RTWT  *(RANC-RMNC)                     !
                      XNF    = 0.15  + 0.25*NFAC                      !
                      TNLAB  = XNF   * NPOOL1                         !
                      RNLAB  = XNF   * NPOOL2                         !
                      NPOOL  = TNLAB + RNLAB                          !

                      IF (ICSDUR .EQ. 1) THEN                         !
                        GPP  = AMIN1 (GPP*NDEF3,(NPOOL/(0.062*0.0095)))
!                   ! Put a max to GPP based on N supply... JIL
                          ! Corrected GPSM calculations .. PWW 2-2-94
                          !
!                        GPSM = GPP * EARS                            !
                      ENDIF                                           !

                      NSDR   = NPOOL/NSINK                            !
 
                      IF (NSDR .LT. 1.0) THEN                         !
                          NSINK = NSINK*NSDR                          !
                      ENDIF                                           !
                                                                       
                      IF (NSINK .GT. TNLAB) THEN                      !
                          STOVN = STOVN - TNLAB                       !
                          RNOUT = NSINK - TNLAB                       !
                          ROOTN = ROOTN - RNOUT                       !
                          RANC  = ROOTN / RTWT                        !
                      ELSE 
                          STOVN = STOVN - NSINK                       !
                          VANC  = STOVN / STOVWT                      !
                      ENDIF                                           !
                                                                      !
                  ENDIF                                               !
                                                                      !
                  GRAINN = GRAINN + NSINK                             !
              ENDIF                       !<--------------------------!

              GRNWT  = GRNWT  + GROGRN                     ! g/pl
              EARWT  = EARWT  + GROGRN
              STMWT  = AMIN1 (STMWT,SWMAX)

      !----------------------------------------------------------------
      !ISTAGE = 6 
      !(End effective grain filling period to physiological maturity)
      !----------------------------------------------------------------
          ELSEIF (ISTAGE .EQ. 6) THEN
              UNO3 = 0.0; UNH4 = 0.0; KUPTAKE = 0.0
              RETURN
          ENDIF           !corresponding to main ELSEIF ISTAGE Loop


!----------------------------------------------------------------------
!   The following code is executed each day regardless of ISTAGE value
!----------------------------------------------------------------------


          !------------------------------------------------------------
          !               Compute Leaf Senescence Factors
          !------------------------------------------------------------

!         Senescence due to water stress
          SLFW   = (1-FSLFW) + FSLFW*SWFAC  

!         Senescence due to nitrogen stress
          SLFN   = (1-FSLFN) + FSLFN*AGEFAC 

!         Senescence due to phosphorus stress
!         5/9/07 CHP, JIL, KJB change from PStres2 to PStres1
          SLFP   = (1-FSLFP) + FSLFP*PSTRES1 

!         Senescence due to light competition
          SLFC   = 1.00        
          IF (LAI .GT. 4.0) THEN
              SLFC = 1.0 - 0.008*(LAI-4.0)
          ENDIF

!         Senescence due to temperature
          SLFT   = 1.0
          IF (TMIN.LE.6.0) THEN
              SLFT  = AMAX1 (0.0, 1.0 - 0.01 * (TMIN-6.0)**2)
          ENDIF
          SLFT  = AMAX1 (SLFT,0.0)

!          PLAS  = (PLA-SENLA)*(1.0-AMIN1(SLFW,SLFC,SLFT,SLFN)) 
          PLAS  = (PLA-SENLA)*(1.0-AMIN1(SLFW,SLFC,SLFT,SLFN,SLFP)) 
!         Daily rate of leaf senescence
          SENLA = SENLA + PLAS
          SENLA = AMAX1 (SENLA,SLAN)
          SENLA = AMIN1 (SENLA,PLA)         
          LAI   = (PLA-SENLA)*PLTPOP*0.0001

          !------------------------------------------------------------
          !       CROP GROWTH FAILURE DUE TO COLD TEMPERATURES
          !  This is probably a holdover from wheat, and can be deleted
          !------------------------------------------------------------

!         JIL/CHP Added optional TSEN from ecotype file for cold 
!         sensitivity threshold.
!          IF (TMIN .LE. 6.0) THEN 
          IF (TMIN .LE. TSEN) THEN 
              ICOLD = ICOLD + 1
          ELSE
              ICOLD = 0
          ENDIF

          IF (LEAFNO .GT. 4 .AND. LAI .LE. 0.0 .AND. ISTAGE .LE. 4
     &                  .AND. ICOLD .GT. 6) THEN
              WRITE(MESSAGE(1),2800)
              CALL WARNING(1,ERRKEY, MESSAGE)
              WRITE (*,2800)
              IF (IDETO .EQ. 'Y') THEN
                  WRITE (NOUTDO,2800)
              ENDIF
              ISTAGE = 6
              MDATE = YRDOY
          ELSE
!         JIL/CHP Added optional CDAY from ecotype file for cold 
!         sensitivity.
!              IF (ICOLD .GE. 15) THEN
              IF (ICOLD .GE. CDAY) THEN
                  WRITE(MESSAGE(1),2800)
                  CALL WARNING(1,ERRKEY, MESSAGE)
                  WRITE (*,2800)
                  IF (IDETO .EQ. 'Y') THEN
                      WRITE (NOUTDO,2800)
                  ENDIF
                  ISTAGE = 6
                  MDATE = YRDOY
              ENDIF
          ENDIF

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
              WRITE (*,2800)
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
             EARS = EARS - EARS*PPLTD/100
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

              CALL MZ_NUPTAK(
     %        RANC, ROOTN,RTWT,TANC,STOVN,STOVWT,TRNU,NLAYR,
     %        RLV,NO3,NH4,PDWI,TCNP,UNO3,UNH4,
     %        XSTAGE,RCNP,PGRORT,PLTPOP,SW,LL,SAT,DLAYR,
     %        SHF,PTF, SENESCE, KG2PPM, PLIGRT)

              CALL MZ_KUPTAK(
     &          ISWPOT, NLAYR, SKi_Avail, UNH4, UNO3,     !Input
     &          KUPTAKE, KSTRES)                          !Output

          ENDIF

          !------------------------------------------------------------
          !                      STATE VARIABLES
          !------------------------------------------------------------

          RTWT   = RTWT  + 0.5*GRORT-0.005*RTWT
          LFWT = MAX(0.0,LFWT)
          PLA = MAX(0.0,PLA)
          LAI = MAX(0.0,LAI)
          STMWT = MAX(0.0,STMWT)
          STOVN = MAX(0.0,STOVN)
          GPP = MAX(0.0,GPP)
          GRNWT = MAX(0.0,GRNWT)
          EARWT = MAX(0.0,EARWT)
          PLTPOP = MAX(0.0,PLTPOP)
          EARS = MAX(0.0,EARS)

          RTWT = MAX(0.0,RTWT)
          BIOMAS = (LFWT + STMWT + EARWT)*PLTPOP
!          DM     = BIOMAS*10.0
          STOVWT = LFWT  + STMWT
          IF (RTWT + LFWT + STMWT + EARWT .GT. 0.0) THEN
            PTF = (LFWT + STMWT + EARWT)/(RTWT + LFWT + STMWT + EARWT)
          ELSE
            PTF = 0.0
          ENDIF

          VSTAGE = REAL (LEAFNO)        !V-stage
          IF (LEAFNO .GT. 0.0) THEN
              RSTAGE = ISTAGE           !R-stage
          ELSE
              RSTAGE = 0
          ENDIF

          TOPWT = BIOMAS            !Total above ground biomass, g/m2
          WTLF = LFWT*PLTPOP        !Leaf weight, g/m2
          XLAI = LAI                !Leaf area index, m2/m2
          XHLAI = LAI               !Used in WATBAL

!** added 26May04 RS
! An empirical equation was added to calculate canopy height (CANHT). The denominator
! (0.4238*PLTPOP + 0.3424) describes the relationship between potential maximum
! LAI and plant population by fitting a straight line through the data of Jaya
! et al. (http://www.regional.org.au/au/asa/2001/6/b/jaya.htm).
! Potential canopy height (CANHT_POT) was set to 1.6 m but should be provided by
! the user (added to cultivar file?); RS 26May04
          IF (XLAI >= MAXLAI) THEN    !keeps CANHT at the maximum value
              CANHT = XLAI / (0.4238 * PLTPOP + 0.3424) * CANHT_POT
              IF (CANHT .GT. CANHT_POT) THEN
                  CANHT = CANHT_POT
               ENDIF
          ENDIF 
!** end
          MAXLAI = AMAX1 (MAXLAI,XLAI)      ! Maximum XLAI season

          IF(WTLF.GT.0) THEN
              SLA = LAI*10000/WTLF  !Specific leaf area, cm2/g
              DUMMY = PLA/LFWT
          ELSE    
              SLA = 0
          ENDIF

          STMWTO = STMWT * PLTPOP   !Stem weight, g/m2
          RTWTO = RTWT * PLTPOP     !Root weight, g/m2

!     02/06/2005 CHP, GH changed to account for barrenness factor
!     Reduce PODWT by grain weight per plant for barren ears
!OLD          PODWT = EARWT*PLTPOP      !Pod weight, g/m2
!      IF (PLTPOP .GE. EARS) THEN                       !Barrenness
!        PODWT = EARWT*PLTPOP - GRNWT*(PLTPOP-EARS) !Pod weight, g/m2
!      ELSE                                           !Prolificacy
        PODWT = EARWT*EARS                            ! Ear wt, g/m2
!      ENDIF

!          PODWTD = 0.0              !Detached pod weight

!     12/16/2004 CHP, GH changed to account for barrenness factor
!          SDWT = GRNWT*PLTPOP       !Seed weight, g/m2
          SDWT = GRNWT*EARS         !Seed weight, g/m2
          GPSM = GPP * EARS         !Seed no/m2, but units don't match?
          SEEDNO=GPSM               !Seed no./m2

          IF(GPP.GT.0) THEN
!              PODNO = SEEDNO/GPP    !Pod (ear) No/m2 
              PODNO = EARS
          ELSE
              PODNO = 0
          ENDIF
          
          IF (PODWT .GT. 0.1) THEN
              SHELPC = SDWT*100./PODWT
          ELSE
              SHELPC = 0.0
          ENDIF

          SHELPC = MIN(SHELPC,99.99)

          IF (TOPWT .GT. 0. .AND. SDWT .GE. 0.) THEN
              HI = SDWT/TOPWT
          ELSE
              HI = 0.
          ENDIF

          IF (TOPWT .GT. 0. .AND. PODWT .GE. 0.) THEN
              HIP = PODWT/TOPWT
          ELSE
              HIP = 0.
          ENDIF

!------> OLD NUPTAK CALL

          !------------------------------------------------------------
          ! Compute Plant Nitrogen Variables for Output
          !------------------------------------------------------------

          WTNUP = WTNUP + TRNU*PLTPOP    !Total N uptake (g N/m2)  
          WTNCAN = (STOVN+GRAINN)*PLTPOP !Nitrogen in canopy (g N/m2)
          WTNSD = GRAINN*PLTPOP  !Nitrogen in grain (g N/m2)
          IF ((LFWT+STMWT) .GT. 0.0) THEN     
              WTNLF = STOVN * (LFWT/STOVWT) * PLTPOP 
                                  !Nitrogen in leaf, g/m2
              WTNST = STOVN * (STMWT/(LFWT+STMWT)) * PLTPOP 
                                  !Nitrogen in stem, g/m2
          ELSE
              WTNLF = 0.0 
              WTNST = 0.0
          ENDIF
      
          WTNVEG=WTNLF+WTNST     !Nitrogen in vegetative tissue (g/m2)


          !------------------------------------------------------------
          !Plant Nitrogen Concentration Variables
          !------------------------------------------------------------

          IF(LFWT.GT.0.AND.PLTPOP.GT.0) THEN
              PCNL = WTNLF/(LFWT*PLTPOP)*100  !Percent N in leaf tissue
          ELSE
              PCNL = 0
          ENDIF

          IF(SDWT.GT.0.AND.PLTPOP.GT.0) THEN
              PCNSD = WTNSD/(GRNWT*PLTPOP) *100
          ELSE
              PCNSD = 0.0
          ENDIF

          IF (GRNWT .GT. 0.0.AND.PLTPOP.GT.0) THEN
             PCNGRN = WTNSD/(GRNWT*PLTPOP)*100
          ELSE
             PCNGRN = 0.0
          ENDIF

          IF( (WTLF+STMWT).GT.0.AND.PLTPOP.GT.0) THEN
              PCNVEG = (WTNLF+WTNST)/((LFWT+STMWT)*PLTPOP)*100  
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
     &        CumLeafSenes, DLAYR, DS, FILECC, MDATE, NLAYR,  !Input
     &        PCNVEG, PLTPOP, PODWT, RLV, RTDEP, RTWTO,       !Input
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

!         Compute N lost in leaf senescence
!         N in senesced leaves - calculate based on today's N content
          CumLfNSenes = CumLfNSenes + 
     &               (CumLeafSenes - CumLeafSenesY) * STOVN / STOVWT

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
          YIELD = GRNWT*10.0*EARS
          IF (GPP.GT. 0) THEN
            SKERWT = GRNWT/GPP
          ELSE
            SKERWT = 0.0
          ENDIF
          GPSM = GPP*EARS
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
     &      PCNVEG, PLTPOP, PODWT, RLV, RTDEP, RTWTO,       !Input
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
        YIELDB  = YIELD/62.8  
            ! Yield in bu/ac at 0% moisture content
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
        
!       chp 2018-07-19 - default to zero removal of by-product from field
!       This default behavior is already handled in the input module.
!       IF (HARVFRAC(2) .LE. 0.0) THEN
!              HARVFRAC(2) = 1.0
!        ENDIF
        
        CALL P_Ceres (DYNAMIC, ISWPHO,                    !Input
     &      CumLeafSenes, DLAYR, DS, FILECC, MDATE, NLAYR,  !Input
     &      PCNVEG, PLTPOP, PODWT, RLV, RTDEP, RTWTO,       !Input
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


      END SUBROUTINE MZ_GROSUB


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
! HIP         !Ratio of pod weight (PODWT) to weight of above-ground portion of 
!              plant (TOPWT) (g[pods] / g[tops])
! I           !Counter
! ICOLD       !Cumulative number of days when TMIN is less than 6 C
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
! PHINT       !Phyllochron interval. Number of GDD required for new leaf emergence, C.
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
! YIELDB      !Yield, bu/ac at 0% moisture content
! YRDOY       !Year and day of year


