C=======================================================================
C  FOR_OPMOB, Subroutine, S. J. Rymph
C-----------------------------------------------------------------------
C  Generates output file for daily growth variables
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  10/13/2005 SJR  Written
C-----------------------------------------------------------------------
C  Called by: CROPGRO
C  Calls:     None
!=======================================================================
      SUBROUTINE FOR_OPMOB(CONTROL, ISWITCH, 
     &  YRPLT, MDATE, DAS, YRDOY, DTX, DXR57, PGAVL, NAVL, PG, PPMFAC, 
     &  NMOBR, NMOBSR, MAINR, ASMDOT, RSPNO3, RSPNH4, RPRO,       
     &  CNOD, CGRSD, CGRSH, CADVG, CSAVEV, AGRVG, PCH2O, WTLF, WLDOT, 
     &  WLIDOT, WLFDOT, WLDOTN, RTWT, WRDOT, WRDOTN, STMWT, WSDOT,          
     &  WSIDOT, WSFDOT, WSDOTN, 
     &  STRWT, WSRDOT, WSRIDOT, WSRFDOT, WSRDOTN,
     &  SLMDOT, LFSENWT, LTSEN, SLNDOT, SLCADDOT, SLNADDOT, 
     &  SLDOT, SRMDOT, SRNDOT, SRCADDOT, SRNADDOT, SRDOT, 
     &  SSMDOT, STSENWT, STLTSEN, SSNDOT, SSCADDOT, SSNADDOT, 
     &  SSDOT, SSRMDOT, SSRNDOT, SSRCADDOT, SSRNADDOT, SSRDOT,
     &  NDMVEG, NDMNEW, NDMOLD, NDMTOT, NMINEP, NMINEA, SDNPL, TRNU, 
     &  NFIXN, NGRSD, NGRSH, TSNMOB,
     &  WTNLF, WNRLF, LFSNMOB, NMINELF, ANMINELF, 
     &  NGRLF, NLALL, NADLF, NRUSLF, PCNL,
     &  WTNRT, WNRRT, RTSNMOB, NMINERT, ANMINERT, 
     &  NGRRT, NRALL, NADRT, NRUSRT, PCNRT,
     &  WTNST, WNRST, STSNMOB, NMINEST, ANMINEST, 
     &  NGRST, NSALL, NADST, NRUSST, PCNST,
     &  WTNSR, WNRSR, SRSNMOB, NMINESR, ANMINESR, 
     &  NGRSR, NSRALL, NADSR, NRUSSR, PCNSR,
     &  WTNSH, WNRSH, SHNMINE, NRUSSH,    
     &  TSCMOB, WCRLF, WRCLDT, LFSCMOB, CMINELF, CADLF, CRUSLF, RHOL,
     &  WCRRT, WRCRDT, RTSCMOB, CMINERT, CADRT, CRUSRT, RHOR,          
     &  WCRST, WRCSDT, STSCMOB, CMINEST, CADST, CRUSST, RHOS,     
     &  WCRSR, WRCSRDT, SRSCMOB, CMINESR, CADSR, CRUSSR, RHOSR,
     &  WCRSH, WRCSHD, SHCMINE, CRUSSH, CHORECOVER, NLKSPENT, NLKNUSED,
     &  NLKCHK, TNLKCHK, CMOBSR, LAIMOBR, VNMOBR)     

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, YR_DOY, TIMDIF
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1  IDETG, RNMODE
      CHARACTER*2  CROP
      CHARACTER*6, PARAMETER :: ERRKEY = 'OPMOB1'
!      CHARACTER*8  FNAME
      CHARACTER*12 OUTMOB

      CHARACTER*30 FILEIO

      INTEGER DAP, DAS, DOY, DYNAMIC, ERRNUM, FROP !, I
      INTEGER RUN
      INTEGER NOUTMOB 
      INTEGER MDATE, TIMDIF, YEAR, YRDOY, YRPLT
      REAL DTX, DXR57, PPMFAC, NMOBR, NMOBSR
      REAL WTLF, WLDOT, WLDOTN, RTWT, WRDOT, WRDOTN, STMWT, WSDOT, 
     &  WSDOTN, STRWT, WSRDOT, WSRDOTN,
     &  SLMDOT, LFSENWT, LTSEN, SLNDOT, SLCADDOT, SLNADDOT, 
     &  SLDOT, SRMDOT, SRNDOT, SRCADDOT, SRNADDOT, SRDOT, 
     &  SSMDOT, STSENWT, STLTSEN, SSNDOT, SSCADDOT, SSNADDOT, 
     &  SSDOT, SSRMDOT, SSRNDOT, SSRCADDOT, SSRNADDOT, SSRDOT,
     &  NDMVEG, NDMNEW, NDMOLD, NDMTOT, NMINEP, NMINEA, TRNU, TSNMOB,  
     &  WTNLF, WNRLF, LFSNMOB, NMINELF, ANMINELF, NRUSLF, PCNL,
     &  WTNRT, WNRRT, RTSNMOB, NMINERT, ANMINERT, NRUSRT, PCNRT,
     &  WTNST, WNRST, STSNMOB, NMINEST, ANMINEST, NRUSST, PCNST,
     &  WTNSR, WNRSR, SRSNMOB, NMINESR, ANMINESR, NRUSSR, PCNSR,
     &  WTNSH, WNRSH, SHNMINE, NRUSSH,     
     &  TSCMOB, WCRLF, WRCLDT, LFSCMOB, CMINELF, CRUSLF, RHOL,
     &  WCRRT, WRCRDT, RTSCMOB, CMINERT, CRUSRT, RHOR,          
     &  WCRST, WRCSDT, STSCMOB, CMINEST, CRUSST, RHOS,     
     &  WCRSR, WRCSRDT, SRSCMOB, CMINESR, CRUSSR, RHOSR,
     &  WCRSH, WRCSHD, SHCMINE, CRUSSH, WLIDOT, WLFDOT, 
     &  WSIDOT, WSFDOT, WSRIDOT, WSRFDOT, PG, PGAVL, NAVL,
     &  MAINR, ASMDOT, RSPNO3, RSPNH4, RPRO, CNOD, CGRSD, CGRSH,
     &  CADVG, CSAVEV, AGRVG, PCH2O, SDNPL, NFIXN, 
     &  CADLF, CADRT, CADST, CADSR, NADLF, NADRT, NADST, NADSR, 
     &  NGRSD, NGRSH, NGRLF, NGRRT, NGRST, NGRSR, 
     &  NLALL, NRALL, NSALL, NSRALL, CHORECOVER, NLKSPENT, NLKNUSED,
     &  NLKCHK, TNLKCHK, CMOBSR, LAIMOBR, VNMOBR

C-------------------------------------------

      LOGICAL FEXIST, FIRST

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH

!     No output for fallow crop
      CROP    = CONTROL % CROP
      IDETG   = ISWITCH % IDETG
      IF (CROP .EQ. 'FA' .OR. IDETG .EQ. 'N') RETURN

!     Transfer values from constructed data types into local variables.
      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      FROP    = CONTROL % FROP
!      LUNIO   = CONTROL % LUNIO
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE
      YRDOY   = CONTROL % YRDOY
!      YRSIM   = CONTROL % YRSIM

      IDETG = ISWITCH % IDETG

!***********************************************************************
!***********************************************************************
!     Run initialization - run once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!      IF (IDETG .EQ. 'Y') THEN
        OUTMOB  = 'PlantMob.OUT'
        CALL GETLUN('OUTMOB', NOUTMOB)

!      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!       Initialize daily growth output file
        INQUIRE (FILE = OUTMOB, EXIST = FEXIST)
        IF (FEXIST) THEN
        OPEN (UNIT = NOUTMOB, FILE = OUTMOB, STATUS = 'OLD',
     &  IOSTAT = ERRNUM, ACCESS = 'APPEND')
        FIRST = .FALSE.
        ELSE
        OPEN (UNIT = NOUTMOB, FILE = OUTMOB, STATUS = 'NEW',
     &  IOSTAT = ERRNUM)
        WRITE(NOUTMOB,'("*SENESCENCE/MOBILIZATION OUTPUT FILE")')
        FIRST = .TRUE.
        ENDIF

        !Write headers
!        CALL HEADER(SEASINIT, FILEIO, NOUTMOB, RUN)
        CALL HEADER(SEASINIT, NOUTMOB, RUN)
        WRITE (NOUTMOB,210)
210   FORMAT('@YEAR DOY   DAS   DAP   ',
     &  'DTX DXR57  PPMF NMOBR NMBSR    ',
     &  'PG PGAVL MAINR ASMDT RSPN3 RSPN4  RPRO  CNOD ',
     &  'CGRSD CGRSH CADVG CSAVE AGRVG PCH2O   ',
     &  'WTLF  WLDOT  WLDTN   RTWT WRDOT  WRDTN  STMWT  WSDOT  ', 
     &  'WSDTN   STRWT  WSRDT  WSRDN ',
     &  'SLMDT LSNWT LLTSN SLNDT SLCAD SLNAD ', 
     &  'SLDOT SRMDT  SRNDT SRCAD SRNAD  SRDOT ', 
     &  'SSMDT SSNWT SLTSN SSNDT SSCAD SSNAD ',
     &  'SSDOT  SSRMD SSRND SSRCA SSRNA  SSRDT ',
     &  'NDMVG NDMNW NDMOD NDMTT NMINP NMINA  TRNU SDNPL NFIXN TSNMO   ',
     &  'NAVL NGRSD NGRSH  ',
     &  'WTNLF  WNRLF NGRLF   WLIDT WLFDT ',
     &  'LFSNM NMNLF ANMLF NLALL NADLF NRULF  PCNL   ',
     &  'WTNRT WNRRT NGRRT RTSNM NMNRT ANMRT NRALL NADRT NRURT PCNRT ',
     &  'WTNST WNRST NGRST  WSIDT WSFDT ',
     &  'STSNM NMNST ANMST NSALL NADST NRUST PCNST   ',
     &  'WTNSR WNRSR NGRSR  WSRID WSRFD ',
     &  'SRSNM NMNSR ANMSR NSRAL NADSR NRUSR PCNSR ',
     &  'WTNSH WNRSH SHNMN NRUSH ',     
     &  'TSCMO WCRLF   WCLDT LFSCM CMNLF CADLF CRULF  RHOL ',
     &  'WCRRT  WCRDT RTSCM CMNRT CADRT CRURT  RHOR ',          
     &  'WCRST  WCSDT STSCM CMNST CADST CRUST  RHOS ',     
     &  'WCRSR WCSRD SRSCM  CMNSR CADSR  CRUSR RHOSR ',
     &  'WCRSH WCSHD SHCMN CRUSH  CHORC  NLKSP  NLKNU  ',
     &  'LKCHK   TLKCK CMBSR LAIMB VNMOB')     

  


!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN

!-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C   CHECK FOR OUTPUT FREQUENCY
C-----------------------------------------------------------------------
      IF (YRDOY .LT. YRPLT .OR. YRPLT .LT. 0) RETURN
      IF ((MOD(DAS,FROP) .EQ. 0)          !Daily output every FROP days,
     &  .OR. (YRDOY .EQ. YRPLT)           !on planting date, and
     &  .OR. (YRDOY .EQ. MDATE)) THEN     !at harvest maturity 

!       Print 
        DAP = MAX(0,TIMDIF(YRPLT,YRDOY))
        CALL YR_DOY(YRDOY, YEAR, DOY) 

        WRITE (NOUTMOB,310)YEAR, DOY, DAS, DAP,
     &  DTX, DXR57, PPMFAC, NMOBR, NMOBSR,      
     &  PG, PGAVL, MAINR, ASMDOT, RSPNO3, RSPNH4, RPRO, CNOD,
     &  CGRSD, CGRSH, CADVG, CSAVEV, AGRVG, PCH2O,
     &  WTLF, WLDOT, WLDOTN, RTWT, WRDOT, WRDOTN, STMWT, WSDOT,          
     &  WSDOTN, STRWT, WSRDOT, WSRDOTN,
     &  SLMDOT, LFSENWT, LTSEN, SLNDOT, SLCADDOT, SLNADDOT, 
     &  SLDOT, SRMDOT, SRNDOT, SRCADDOT, SRNADDOT, SRDOT, 
     &  SSMDOT, STSENWT, STLTSEN, SSNDOT, SSCADDOT, SSNADDOT, 
     &  SSDOT, SSRMDOT, SSRNDOT, SSRCADDOT, SSRNADDOT, SSRDOT,
     &  NDMVEG, NDMNEW, NDMOLD, NDMTOT, NMINEP, NMINEA, TRNU,
     &  SDNPL, NFIXN, TSNMOB, NAVL, NGRSD, NGRSH,
     &  WTNLF, WNRLF, NGRLF, WLIDOT, WLFDOT, 
     &  LFSNMOB, NMINELF, ANMINELF, NLALL, NADLF, NRUSLF, PCNL,
     &  WTNRT, WNRRT, NGRRT, 
     &  RTSNMOB, NMINERT, ANMINERT, NRALL, NADRT, NRUSRT, PCNRT,
     &  WTNST, WNRST, NGRST, WSIDOT, WSFDOT, 
     &  STSNMOB, NMINEST, ANMINEST, NSALL, NADST, NRUSST, PCNST,
     &  WTNSR, WNRSR, NGRSR, WSRIDOT, WSRFDOT, 
     &  SRSNMOB, NMINESR, ANMINESR, NSRALL, NADSR, NRUSSR, PCNSR,
     &  WTNSH, WNRSH, SHNMINE, NRUSSH,    
     &  TSCMOB, WCRLF, WRCLDT, LFSCMOB, CMINELF, CADLF, CRUSLF, RHOL,
     &  WCRRT, WRCRDT, RTSCMOB, CMINERT, CADRT, CRUSRT, RHOR,          
     &  WCRST, WRCSDT, STSCMOB, CMINEST, CADST, CRUSST, RHOS,     
     &  WCRSR, WRCSRDT, SRSCMOB, CMINESR, CADSR, CRUSSR, RHOSR,
     &  WCRSH, WRCSHD, SHCMINE, CRUSSH, CHORECOVER, NLKSPENT, NLKNUSED,
     &  NLKCHK, TNLKCHK, 
     &  CMOBSR, LAIMOBR, VNMOBR             
310       FORMAT (1X,I4,1X,I3.3,2(1X,I5),2(1X,F5.1),3(1X,F5.3),
     &    1X,F5.2,1X,F5.3,2(1X,F5.3),2(1X,F5.2),8(1X,F5.3),1X,F6.2,
     &    1X,F6.1,1X,F6.3,1X,F6.1,1X,F5.1,1X,F6.3,1X,F6.2,1X,F6.1,
     &    1X,F6.3,1X,F7.2,1X,F6.2,1X,F6.3,8(1X,F5.3),1X,F6.3,
     &    2(1X,F5.3),1X,F6.3,7(1X,F5.3),1X,F6.3,3(1X,F5.3),1X,F6.3,
     &    10(1X,F5.3),1X,F6.3,2(1X,F5.3),2(1X,F6.3),1X,F5.3,1X,F7.3,
     &    1X,F5.2,7(1X,F5.3),1X,F7.3,1X,F5.2,11(1X,F5.3),1X,F6.2,
     &    1X,F5.2,7(1X,F5.3),1X,F7.3,1X,F5.2,1X,F5.3,1X,F6.3,
     &    13(1X,F5.3),1X,F5.2,1X,F7.3,5(1X,F5.3),1X,F5.1,1X,F6.3,
     &    5(1X,F5.3),1X,F5.2,1X,F6.2,5(1X,F5.3),1X,F5.1, 1X,F5.2,   
     &    1X,F5.3,1X,F6.3, 1X,F5.3, 1X,F6.3,5(1X,F5.3), 3(1X,F6.4),  
     &    1X,F6.4,1X,F7.3,3(1X,F5.3))
  
  
      
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal Output 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------


 
 
        !Close daily output files.
        CLOSE (NOUTMOB)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END ! SUBROUTINE FOR_OPMOB
!=======================================================================


!=======================================================================
!       Variable definitions for FOR_OPMOB
!-----------------------------------------------------------------------
!***********************************************************************
!       END SUBROUTINE FOR_OPMOB
!=======================================================================

