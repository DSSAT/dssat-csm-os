C=======================================================================
C  FOR_VEGGR, Subroutine, J. W. Jones, K. J. Boote, G. Hoogenboom
C  Calculates vegetative partitioning as a function of V stage,
C  and limits growth prior to VSINK.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/01/1989     Written.
C  04/24/1994 NBP Changed TAIRHR to TGRO.
C  09/26/1995 KJB Added shift in partitioning from stem to leaf during
C                 and especially after drought stress.
C  01/19/1996 KBJ & JWJ  Idea that we should allow shift in partitioning
C                 from stem to leaf under cold stress and possibly low
C                 light stress in similar manner.  Non-optimum temp
C                 causes more DM to leaf in SPAR experiment.  What
C                 about root:shoot?  Do these later.
C  02/15/1996 KJB Allow excess (=PGLEFT) to influence PG on next day
C  06/18/1998 CHP Modified for modular format
C  05/10/1999 GH  Incorporated in CROPGRO
C  06/21/2001 GH  Modified seasonal initialization
C  07/01/2003 SJR Added storage tissue organ and reduction and mobilization
C                        controls for dormancy and harvest/damage
C-----------------------------------------------------------------------
C  Called by: CROPGRO
C  Calls:     FOR_CANOPY
C             ERROR, FIND, IGNORE
C========================================================================

      SUBROUTINE FOR_VEGGR(
     &    AGRLF, AGRRT, AGRSTM, CADVG, CMINEP, CSAVEV,    !
     &    ECONO, FILECC, FILEGC, FNINL, FNINR, FNINS,     !
     &    LFCDEBT, LFCMINE, LFSCMOB, LFSNMOB, NAVL,       !
     &    NDMNEW, NDMOLD, PAR, PCH2O, PCNL, PCNST,        !Input
     &    PCNRT, PCNSR, PG, PGAVL, ROWSPC, RTCDEBT,       !
     &    RTCMINE, RTSCMOB, RTSNMOB, RTWT, RVSTGE,        !
     &    SHCMINE, SLDOT, SRCDEBT, SRCMINE, SRDOT,        !
     &    SRSCMOB, SRSNMOB, SSDOT, SSRDOT, STCDEBT,       !
     &    STCMINE, STMWT, STSCMOB, STSNMOB, TGRO, TSCMOB, !
     &    TSNMOB, TURFAC, VSTAGE, WCRLF, WCRRT, WCRST,    !
     &    WTLF, WTNLF, WTNRT, WTNSR, WTNST, XLAI,         !
     &    YRDOY, YREMRG, YRSIM,                           !
     &    AGRSTR, FNINSR, STRWT, WCRSR,                   !Input
     &    NLAYR, PROLFI, PRORTI, PROSTI, PROSRI,          !Input
     &    RNH4C, RNO3C, TRNH4U, TRNO3U, UNO3, UNH4,       !

     &    AGRVG, ANMINELF, ANMINERT, ANMINESR, ANMINEST,  !Input/Output
     &    FRLF, FRRT, FRSTM, FRSTR, NMINEA, NFIXN,        !
     &    NRUSLF, NRUSRT, NRUSSR, NRUSST, TRNU,           !

     &    ACMINELF, ACMINERT, ACMINESH, ACMINESR,         !
     &    ACMINEST, CADLF, CADST, CANHT, CANWH, CMINEA,   !
     &    CRUSLF, CRUSRT, CRUSSH, CRUSST, EXCESS, NADLF,  !
     &    NADRT, NADST, NGRLF, NGRRT, NGRST,              !Output
     &    NSTRES, TNLEAK, WLDOTN, WRDOTN, WSDOTN,         !
     &    PNMLF, PNMRT, PNMSH, PNMSR,PNMST,RPRO,          !
     &    CADRT, CADSH, NADSH, CADSR, CRUSSR, NADSR,      !Output
     &    NGRSR, WSRDOTN, CADSRF, CMOBSRN, CMOBSRX,       !Output
     &    FNINSRG, NGRSRG, PROSRG, PROSRT, CHORECOVER,    !Output
     &    NLKSPENT, NLKNUSED, NLKCHK, TNLKCHK,            !Output
     &    DYNAMIC)                                        !Control

! 2023-01-26 chp removed unused variables in argument list:
!     CMINELF, CMINERT, CMINESH, CMINESR, CMINEST, DTX, DXR57, 
!     NR1, NVSTL, NVSTR, NVSTS, NVSTSR, WCRSH, CLAIT, NRUSTOT, 
!     CDEBIT, CMOBSR, PPMFAC, ALPHL, ALPHR, ALPHS, ALPHSR, 
!     NUPNH4, NUPNO3, RFIXN, 
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE, FOR_CANOPY, NLKDIST, TIMDIF
      SAVE

      CHARACTER*6  ERRKEY
      PARAMETER   (ERRKEY = 'VEGGR')

      CHARACTER*6  ECONO, SECTION
      CHARACTER*80 C80
      CHARACTER*92 FILECC, FILEGC

      INTEGER DYNAMIC, TIMDIF
      INTEGER YRDOY, YREMRG, YRSIM, DAS !, NR1
      INTEGER I, LUNCRP, ERR, LNUM, ISECT, FOUND

      REAL AGRLF, AGRRT, AGRSTM, CMINEP, CMOBMX
      REAL FNINL, FNINR, FNINS  !DTX, DXR57, 
      REAL NAVL, NDMNEW, NDMOLD, PAR, PCH2O, PG
      REAL PROLFI, PRORTI, PROSTI, ROWSPC
      REAL RVSTGE, STMWT, TURFAC, WCRLF, WCRRT, WCRST !, WCRSH
      REAL WTLF, XLAI

      REAL AGRVG, CADLF, CADST, CANHT, CANWH, CMINEA
      REAL CRUSLF, CRUSRT, CRUSST, CRUSSH, CUMTUR
      REAL EXCESS, FRLF, FRRT, FRSTM, NADLF, NADRT, NADST
      REAL NGRLF, NGRRT, NGRST, NSTRES, PGAVL
      REAL TNLEAK, VSTAGE, WLDOTN, WRDOTN, WSDOTN

      REAL ATOP, CADSTF, FNINLG, FNINRG, FNINSG
      REAL PROLFG, PRORTG, PROSTG
      REAL NRATIO,NGRVEG,NADRAT,NLEFT
      REAL NGRVGG, NGRLFG,NGRSTG,NGRRTG
      REAL PROLFT,PROSTT,PRORTT
      REAL VGRDEM, SUPPN, PGLEFT, LSTR, CSAVEV
      REAL NLEAK
      REAL NMINEA, NFIXN, TRNU

      REAL TGRO(TS)
      REAL AGRSTR, CADSR, CADSRF, CMOBSRN, ! CLAIT, CMOBSR, 
     &    CMOBSRX, CRUSSR, FNINSR, FNINSRG, FRSTR,   
     &    NADSR, NGRSR, NGRSRG, PROSRG,   !PPMFAC, 
     &    PROSRI, PROSRT, STRWT, WCRSR, WSRDOTN

      REAL NADSH, PNMLF, PNMRT, PNMSH, PNMSR, PNMST,  !NRUSTOT, 
     &    CADRT, CADSH, RPRO

      REAL LFSCMOB, RTSCMOB, SRSCMOB, STSCMOB, TSCMOB, TSNMOB
      REAL ACMINELF, ACMINERT, ACMINESH, ACMINESR, ACMINEST
      REAL CMINER  !CMINELF, CMINERT, CMINESH, CMINESR, CMINEST 
      REAL LFCMINE, RTCMINE, SHCMINE, SRCMINE, STCMINE
      REAL SLDOT, SRDOT, SSDOT, SSRDOT
      REAL LFSNMOB, RTSNMOB, SRSNMOB, STSNMOB
      REAL PROLFF, PRORTF, PROSRF, PROSTF
      REAL WTNLF, WTNRT, WTNSR, WTNST



C-----------------------------------------------------------------------
C      New NDMOLD allocation variables for forage model 
C      Used to weight partitioning in favor of one organ over others
C-----------------------------------------------------------------------
      REAL PCNL, PCNST, PCNRT, PCNSR,  !NVSTL, NVSTR, NVSTS, NVSTSR, 
     &    CWST, MXWST, PWLF, PWST, PWRT, PWSR, RTWT
      REAL PROLFR, PROSTR, PRORTR, PROSRR

C-----------------------------------------------------------------------
C      New NLEAK distribution variables for all models 
C      Used to "put back" and distribute NLEAK to new growth
C-----------------------------------------------------------------------
      INTEGER L, NLAYR
      REAL DNADRAT, NLKCOST, NLKGROW,  !AGRVGI, AGRVGPI, 
     &   NRFRESP, ONDMOLD,  !PNTVG, NUPNH4(NL), NUPNO3(NL), 
     &   RNH4C, RNO3C, TRNH4U,  !RFIXN, RNNU, PNUPNH4,PNUPNO3, 
     &   TRNO3U, UNH4(NL), UNO3(NL)  !, XTVEGM

      REAL NLKCHK, TNLKCHK

      REAL LFCDEBT, LSTSR, RTCDEBT, SRCDEBT, STCDEBT  !CDEBIT, 

!     REAL ALPHL, ALPHR, ALPHS, ALPHSR
C-----------------------------------------------------------------------
C      New minimum mobile CH2O concentration variables 
C      for forage model.  Sets CH2O concentration in 
C      abscised tissue instead of using RHOx
C-----------------------------------------------------------------------
      REAL PCHOLFF, PCHORTF, PCHOSRF, PCHOSTF
      REAL CADVG, CHORECOVER, NLKSPENT, NLKNUSED

      REAL ANMINELF, ANMINERT, ANMINESR, ANMINEST, NRUSLF,
     &  NRUSRT, NRUSSR, NRUSST, NSTFAC

      REAL AAA, BBB, CCC, DDD,ZZZ, XXX
!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!     Read in values from input file, which were previously input
!       in Subroutine IPCROP.
!-----------------------------------------------------------------------
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)

!-----------------------------------------------------------------------
!    Find and Read Plant Composition Section
!-----------------------------------------------------------------------
!     Subroutine FIND finds appropriate SECTION in a file by
!     searching for the specified 6-character string at beginning
!     of each line.
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!    Find and Read Plant Composition Section
!-----------------------------------------------------------------------

      LNUM = 1
      SECTION = '!*PLAN'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(6F6.0)',IOSTAT=ERR)
     &    PROLFI, PROLFG, PROLFF, PROSTI, PROSTG, PROSTF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(3F6.0)',IOSTAT=ERR) PRORTI, PRORTG, PRORTF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(3F6.0)',IOSTAT=ERR) PROSRI, PROSRG, PROSRF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(6X, 4F6.0)',IOSTAT=ERR) PROLFR,PROSTR,PRORTR,PROSRR
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4F6.0)',IOSTAT=ERR) 
     &    PCHOLFF, PCHOSTF, PCHORTF, PCHOSRF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)



      ENDIF
!-----------------------------------------------------------------------
!    Find and Read Carbon and Nitrogen Mining Section
!-----------------------------------------------------------------------
      SECTION = '!*CARB'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(2F6.0)',IOSTAT=ERR) CMOBMX, CADSTF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(3F6.0)',IOSTAT=ERR) CMOBSRN, CMOBSRX, 
     &    CADSRF
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(24X,F6.0)',IOSTAT=ERR) NSTFAC
!        WRITE(3000,'(F10.2)') NSTFAC
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      ENDIF
!-----------------------------------------------------------------------
!    Find and Read Partitioning Section
!-----------------------------------------------------------------------
      SECTION = '!*VEGE'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        DO I=1,4
        ISECT = 2
        DO WHILE (ISECT .NE. 1)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        ENDDO
        ENDDO
        READ(C80,'(24X,F6.0)',IOSTAT=ERR) ATOP
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)


        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(5F6.0)',IOSTAT=ERR) PWLF,PWST,PWRT,PWSR,MXWST
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)


      ENDIF
      
      CLOSE (LUNCRP)

!-----------------------------------------------------------------------
!    Call FOR_CANOPY for input
!-----------------------------------------------------------------------
      CALL FOR_CANOPY(
     &  ECONO, FILECC, FILEGC, PAR, ROWSPC,               !Input
     &  RVSTGE, TGRO, TURFAC, VSTAGE, XLAI,NSTRES,        !Input
     &  CANHT, CANWH,                                     !Output
     &  RUNINIT)                                          !Control

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CADLF  = 0.0  
      CADST  = 0.0  
      CMINEA = 0.0  
      CRUSLF = 0.0  
      CRUSRT = 0.0  
      CRUSST = 0.0  
      CUMTUR = 1.0  
      EXCESS = 1.0  
      FNINLG = 0.0  
      FNINRG = 0.0  
      FNINSG = 0.0  
      NADLF  = 0.0  
      NADRT  = 0.0  
      NADST  = 0.0  
      NGRLF  = 0.0  
      NGRRT  = 0.0  
      NGRST  = 0.0  
      NSTRES = 1.0  
      PGLEFT = 0.0
      SUPPN  = 0.0
      TNLEAK = 0.0  
      VGRDEM = 0.0
      WLDOTN = 0.0  
      WRDOTN = 0.0  
      WSDOTN = 0.0 
      
      CADSR  = 0.0 
      CRUSSR = 0.0
      FNINSRG = 0.0
      NADSR  = 0.0
      NGRSR  = 0.0
      WSRDOTN = 0.0
      TNLKCHK = 0.0
      
!***********************************************************************
!***********************************************************************
!     EMERGENCE CALCULATIONS - Performed once per season upon emergence
!         or transplanting of plants
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. EMERG) THEN
!-----------------------------------------------------------------------
      FNINLG = PROLFG * 0.16   
      FNINRG = PRORTG * 0.16   
      FNINSG = PROSTG * 0.16   
      FNINSRG = PROSRG * 0.16
      CUMTUR = 1.0             

      CALL FOR_CANOPY(
     &  ECONO, FILECC, FILEGC, PAR, ROWSPC,               !Input
     &  RVSTGE, TGRO, TURFAC, VSTAGE, XLAI,NSTRES,        !Input
     &  CANHT, CANWH,                                     !Output
     &  EMERG)                                            !Control



!***********************************************************************
!***********************************************************************
!     DAILY RATE/INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      DAS   = MAX(0,TIMDIF(YRSIM,YRDOY))
!-----------------------------------------------------------------------
C     Partitioning is modified by water stress and nitrogen stress
C-----------------------------------------------------------------------
      SUPPN = NFIXN + TRNU + NMINEA
!    chp added check for YRDOY = YREMRG, but on the next day, it still
!     shows N stress because there is little supply.  Force a lag time?
      NSTFAC = MIN(NSTFAC,1.0) 
      NSTFAC = MAX(NSTFAC,0.1)
!      IF (SUPPN .LT. 0.70 * NDMNEW .AND. NDMNEW .GT. 0.) THEN
      IF (SUPPN .LT. NSTFAC * NDMNEW .AND. NDMNEW .GT. 0. .AND. 
     &    YRDOY .NE. YREMRG) THEN
!        NSTRES = MIN(1.0,SUPPN/(NDMNEW * 0.70))
!        NSTRES = MIN(1.0,SUPPN/(NDMNEW * 0.70))
         NSTRES = MIN(1.0,SUPPN/(NDMNEW * NSTFAC))

      ELSE
        NSTRES = 1.0
      ENDIF
      FRRT  = ATOP * (1.0 - (MIN(TURFAC,NSTRES)))*(1.0-FRRT) + FRRT
C-----------------------------------------------------------------------
C     Cumulative turgor factor that remembers veg drought stress
C     to shift partitioning between leaf and stem toward leaf,
C     especially after drought is released.
C     Sort of 20-day rolling average
C-----------------------------------------------------------------------
      CUMTUR = 0.95*CUMTUR + 0.05*TURFAC 
C-----------------------------------------------------------------------
C     0.6 IS A SCALAR, COULD BE LESS, was once 0.8 and 0.7
C     0.7 appears to be too much for peanut, but not for soybean.
C-----------------------------------------------------------------------
      FRSTR = (FRSTR/(FRLF+FRSTM+FRSTR))*(1-FRRT)

      FRLF  = (1.0 + 0.6*(1.0-CUMTUR))*(1.-FRRT-FRSTR)*FRLF/  
     &    (FRLF + FRSTM)
      FRLF = MIN(FRLF, 0.90*(1. - FRRT-FRSTR))

      FRSTM = 1.0 - FRRT - FRLF - FRSTR
C-----------------------------------------------------------------------
C     To prevent negative partitioning to root and limit leaf plus
C     stem to a maximum of 98 % of the vegetative partitioning
C      NOTE - kept 98% after adding storage organ to scheme
C-----------------------------------------------------------------------
      FRLF  = MIN(FRLF,FRLF*0.98/(MAX(0.001,FRLF+FRSTM+FRSTR)))
      FRSTM = MIN(FRSTM,FRSTM*0.98/(MAX(0.001,FRLF+FRSTM+FRSTR)))
      FRSTR = MIN(FRSTR,FRSTR*0.98/(MAX(0.001,FRLF+FRSTM+FRSTR)))
      FRRT  = 1.0 - FRLF - FRSTM - FRSTR
C-----------------------------------------------------------------------
C     Calculate weighted PHI + GR = 1/E = AGRVG for veg. growth
C-----------------------------------------------------------------------
      AGRVG = AGRLF * FRLF + AGRRT * FRRT + AGRSTM * FRSTM
     &    + AGRSTR * FRSTR
C-----------------------------------------------------------------------
C     Calculate New Growth Rate of Leaves, Stems, and Roots
C-----------------------------------------------------------------------
      VGRDEM = PGAVL / AGRVG
      WLDOTN = FRLF * VGRDEM
      WSDOTN = FRSTM * VGRDEM
      WRDOTN = FRRT * VGRDEM
      WSRDOTN = FRSTR * VGRDEM
C-----------------------------------------------------------------------
C     Compute maximum N required for tissue growth
C-----------------------------------------------------------------------
      NGRLF  = WLDOTN * FNINL
      NGRST  = WSDOTN * FNINS
      NGRRT  = WRDOTN * FNINR
      NGRSR  = WSRDOTN * FNINSR
      NGRVEG = NGRLF + NGRST + NGRRT + NGRSR
C-----------------------------------------------------------------------
C     Compute minimum N required for tissue growth
C-----------------------------------------------------------------------
      NGRLFG = WLDOTN * FNINLG
      NGRSTG = WSDOTN * FNINSG
      NGRRTG = WRDOTN * FNINRG
      NGRSRG = WSRDOTN * FNINSRG
      NGRVGG = NGRLFG + NGRSTG + NGRRTG + NGRSRG

      NRATIO = 1.0
      IF (NAVL .LT. NGRVGG) THEN
C-----------------------------------------------------------------------
C     Compute ratio for reducing leaf growth to prevent N conc of
C       new tissue from being below the minimum for growth
C-----------------------------------------------------------------------
        IF (NGRVGG .GT. 0.0) THEN
        NRATIO = NAVL / NGRVGG
        WLDOTN = WLDOTN * NRATIO
        WSDOTN = WSDOTN * NRATIO
        WRDOTN = WRDOTN * NRATIO
        WSRDOTN = WSRDOTN * NRATIO
        NGRLF  = NGRLFG * NRATIO
        NGRST  = NGRSTG * NRATIO
        NGRRT  = NGRRTG * NRATIO
        NGRSR = NGRSRG * NRATIO
C-----------------------------------------------------------------------
C     Adjust conversion costs to account for composition of tissue at
C       lower N concentration
C-----------------------------------------------------------------------
        AGRVG = AGRLF * FRLF * (1.0 - (PROLFG - PROLFI)/(1.0 -
     &   PROLFI))+ AGRRT * FRRT * (1.0 - (PRORTG - PRORTI)/(1.0 -
     &   PRORTI)) + AGRSTM * FRSTM * (1.0 - (PROSTG - PROSTI)/
     &   (1.0 - PROSTI)) + AGRSTR * FRSTR * (1.0 - (PROSRG - PROSRI)
     &    /(1.0 - PROSRI))

!       IF (AGRVG * (WLDOTN + WSDOTN + WRDOTN + WSRDOTN) .GT. PGAVL) THEN
!                              VGRDEM = PGAVL / AGRVG
!            WLDOTN = FRLF * VGRDEM
!            WSDOTN = FRSTM * VGRDEM
!            WRDOTN = FRRT * VGRDEM
!            WSRDOTN = FRSTR * VGRDEM
!       ENDIF

        ENDIF
      ELSE
C-----------------------------------------------------------------------
C     NAVL IS between lower and maximum N limit in this case,
C       leaf expansion occurs as normal, but N concentration is reduced
C-----------------------------------------------------------------------
        IF (NGRVEG .GT. 0.0 .AND. NAVL .LT. NGRVEG) THEN
        NGRLF = MIN(NAVL * NGRLF / NGRVEG, NGRLF)
        NGRST = MIN(NAVL * NGRST / NGRVEG, NGRST)
        NGRRT = MIN(NAVL * NGRRT / NGRVEG, NGRRT)
        NGRSR = MIN(NAVL * NGRSR / NGRVEG, NGRSR)
        ENDIF
C-----------------------------------------------------------------------
C     Compute protein fraction of new vegetative tissue growth
C-----------------------------------------------------------------------
        IF (WLDOTN .GT. 0.0) THEN
        PROLFT = NGRLF * (100./16.)/WLDOTN
        ELSE
        PROLFT = 0.0
        ENDIF
        IF (WSDOTN .GT. 0.0) THEN
        PROSTT = NGRST * (100./16.)/WSDOTN
        ELSE
        PROSTT = 0.0
        ENDIF
        IF (WRDOTN .GT. 0.0) THEN
        PRORTT = NGRRT * (100./16.)/WRDOTN
        ELSE
        PRORTT = 0.0
        ENDIF
        IF (WSRDOTN .GT. 0.0) THEN
        PROSRT = NGRSR * (100./16.)/WSRDOTN
        ELSE
        PROSRT = 0.0
        ENDIF



C-----------------------------------------------------------------------
C     Recompute respiration costs if expansion occurs at low N-conc.,
C       allow N dilution during growth of leaves, stems, and roots
C-----------------------------------------------------------------------
        AGRVG = AGRLF * FRLF * (1.0 - (PROLFT - PROLFI)/
     &    (1.0-PROLFI)) + AGRRT * FRRT * (1.0 - (PRORTT - PRORTI)/
     &    (1.0 - PRORTI)) + AGRSTM * FRSTM * (1.0 - (PROSTT -
     &    PROSTI)/(1.0 - PROSTI)) + AGRSTR * FRSTR * (1.0 - (PROSRT
     &    - PROSRI)/(1.0-PROSRI))

!            VGRDEM = PGAVL / AGRVG
!            WLDOTN = FRLF * VGRDEM
!            WSDOTN = FRSTM * VGRDEM
!            WRDOTN = FRRT * VGRDEM
!            WSRDOTN = FRSTR * VGRDEM

      ENDIF
C-----------------------------------------------------------------------
C     Compute C and N remaining to add to reserves
C-----------------------------------------------------------------------
      PGLEFT = MAX(0.0,PGAVL - ((WLDOTN + WSDOTN + WRDOTN + WSRDOTN)
     &    * AGRVG))
C-----------------------------------------------------------------------
C     Scales to 1.0 if PGLEFT is small fraction, and to 0.2 if large
C     fraction.  Used 0.04, so minor PGLEFT has no effect.  Used square
C     root.  Creates almost no effect if PGLEFT/PG is small, but goes to
C     0.2 as PGLEFT/PG  approaches 1.0.  0.04 could be parameterized as
C     kickoff point.  Upper cutoff is the value 1.04.  Limit of 1.04 -
C     1.00 forces relationship to stop at 0.04, gives 0.2 of normal PG.
C     value 1.04 -0.04 also can not be greater than 1.0 or we get
C     stimulation of photosynthesis and the sq root works differently.
C-----------------------------------------------------------------------
      IF (PG .GT. 0.0001 .AND. PGLEFT .GT. 0.00001) THEN
        EXCESS =  (1.20 - MIN(1.0, MAX(PGLEFT/PG,0.20)) )**0.5
      ELSE
        EXCESS = 1.00
      ENDIF

      ACMINELF = 0.0
      ACMINERT = 0.0
      ACMINESH = 0.0
      ACMINESR = 0.0
      ACMINEST = 0.0

      CADST = 0.0
      CADLF = 0.0
      CADSR = 0.0
      CADRT = 0.0
      CADSH = 0.0
      CMINEA = 0.0
      CRUSLF = 0.0
      CRUSST = 0.0
      CRUSRT = 0.0
      CRUSSH = 0.0
      CRUSSR = 0.0
C-----------------------------------------------------------------------
C      SJR 10/16/03
C      Set all NAD variables to 0.0
C      NADSH never assigned a value other than 0.0 - not used anywhere
C-----------------------------------------------------------------------

!      NADRAT = 0.0
!      NADST  = 0.0
!      NADLF  = 0.0
!      NADRT  = 0.0
!      NADSR  = 0.0
        NADSH  = 0.0

C-----------------------------------------------------------------------
C      SJR 10/20/03
C      Set all NLEAK fix variables to 0.0
C-----------------------------------------------------------------------
      NRFRESP=0.0
      NLKGROW=0.0
      NLKCOST=0.0

C-----------------------------------------------------------------------
C    Calculate Increase in Remobilizable C due to N shortage and
C      add to Carbon Pool.  Distribute to Leaves and Stems.
C-----------------------------------------------------------------------
C    Want half as much accumulation in stem in veg phase
C-----------------------------------------------------------------------
!      IF (DAS .LT. NR1) THEN
!         LSTR = (1.-0.6*CADSTF)/(0.6*CADSTF)
!      ELSE
C-----------------------------------------------------------------------
C    5/11/04  KJB/SJR Add code to allocate excess CH2O to Stolon as well
C      as to leaf and stem.  For forages chose to ignore different 
C      partitioning for vegetative vs. reproductive stages.
C-----------------------------------------------------------------------
C      How CADSTF and CADSRF work to patition CAD between STOR, Stems and Leaves.
C      CADSRF/(1-CADSRF) X as much CH2O will be allocated to each g of STOR 
C      as to each g of Tops.
C      Of the CH2O added to Tops, CADSTF/(1-CADSTF) as much CH2O will be  
C      allocated to each g of stems as to each g of  leaves.
C-----------------------------------------------------------------------
C    02/02/06 SJR  Adjust CH2O refill partitioning for today's DM lost 
C                        to natural senescence.
C      Complicates the equation considerably with minimal impact on results.
C      Only added for "correctness"
C-----------------------------------------------------------------------
        LSTSR = CADSRF/(1-CADSRF)
        LSTR = (1.-CADSTF)/CADSTF
!      ENDIF
      IF (STMWT-SSDOT+WTLF-SLDOT .GT. 0.0) THEN
C      5/16/05 SJR added WTLF to calculation of LSTSR to be consistent 
C            with the definition - ratio of CH2O to be added to stem 
C            relative to that added to leaf + stem
!         LSTSR = LSTSR*STRWT/(STRWT*LSTSR+STMWT)
!         LSTSR = LSTSR * (STRWT - SSRMDOT) / ((STRWT-SSRMDOT) *
!     &              LSTSR + STMWT - SSMDOT + WTLF - SLMDOT)
!         LSTR = LSTR * (WTLF - SLMDOT) / 
!     &              (STMWT - SSMDOT + (WTLF - SLMDOT) * LSTR)

        LSTSR = LSTSR * (STRWT - SSRDOT ) 
     &    / (LSTSR * (STRWT-SSRDOT) 
     &    + (STMWT - SSDOT ) 
     &    + (WTLF - SLDOT ))

        LSTR = LSTR * (WTLF - SLDOT ) / 
     &    (LSTR * (WTLF - SLDOT )
     &    + (STMWT - SSDOT) )



      ENDIF
      IF (PGLEFT .GE. CMINEP) THEN
        CADSR = (PGLEFT-CMINEP)/PCH2O * LSTSR
        CADLF = (PGLEFT-CMINEP)/PCH2O * LSTR*(1-LSTSR)
        CADST = (PGLEFT-CMINEP) * (1.-LSTR) * (1-LSTSR) / PCH2O
      ELSE
C-----------------------------------------------------------------------
C    Calculate actual C used (CMINEA) , compute how much is taken
C    from LF, ST, RT, and SH, which may be less than orig calc of CMINEP
C
C    8/26/97 KJB  DTX IN PLACE OF 1 TO SLOW IT DOWN A BIT AT ALL TIMES
C    AND TO BE SENSITIVE TO TEMPERATURE PRIOR TO R5 STAGE, BUT
C    STILL WANT THE SPEED-UP CAUSED BY THE "+ DXR57" FEATURE AFTER R5.
C
C-----------------------------------------------------------------------
C      7/2/03 SJR added (PPMFAC) to limit mobilization from roots 
C      while dormant
C-----------------------------------------------------------------------
!        IF (CMINEP .GT. 0) THEN
!          CMINEA = CMINEP - PGLEFT
!          CRUSLF = CMINEA / CMINEP * CMOBMX * (DTX + DXR57) * 
!     &              (WCRLF - WTLF*PCHOLFF) 
!          CRUSST = CMINEA / CMINEP * CMOBMX * (DTX + DXR57) * 
!     &              (WCRST - STMWT * PCHOSTF) 
!          CRUSSH = CMINEA / CMINEP * CMOBMX * (DTX + DXR57) * WCRSH 
!          CRUSRT = CMINEA / CMINEP * CMOBMX * (DTX + DXR57) * PPMFAC *
!     &              (WCRRT - RTWT * PCHORTF)        
!            CRUSSR = CMINEA / CMINEP * CMOBSR * (DTX + DXR57) * 
!     &              (WCRSR - STRWT * PCHOSRF)
!        ENDIF



!      IF (CMINEP .GT. 0) THEN
        CMINEA = CMINEP - PGLEFT
!        CMINEA = MAX(TSCMOB,CMINEP - PGLEFT)
        IF (CMINEA .GT. CMINEP) CMINEA = CMINEP

C-----------------------------------------------------------------------
C      In this case, the remaining TSNMOB will stay inthe WTNxx pools
C-----------------------------------------------------------------------
        IF (CMINEA .LT. TSCMOB) THEN
        LFSCMOB = LFSCMOB * (CMINEA / TSCMOB)
        STSCMOB = STSCMOB * (CMINEA / TSCMOB)
        RTSCMOB = RTSCMOB * (CMINEA / TSCMOB)
        SRSCMOB = SRSCMOB * (CMINEA / TSCMOB)
        ELSE
C-----------------------------------------------------------------------
C      Otherwise all TSNMOB plus some portion of mobilized N will be used
C-----------------------------------------------------------------------
!            IF (CMINEP .GT.TSCMOB) THEN
        CMINER = (CMINEA - TSCMOB) / (CMINEP - TSCMOB)
        ACMINESH = SHCMINE * CMINER
        ACMINELF = (LFCMINE - LFSCMOB) * CMINER
        ACMINEST = (STCMINE - STSCMOB) * CMINER
        ACMINERT = (RTCMINE - RTSCMOB) * CMINER
        ACMINESR = (SRCMINE - SRSCMOB) * CMINER
        ENDIF
        
        CRUSLF = LFSCMOB + ACMINELF
        CRUSST = STSCMOB + ACMINEST
        CRUSRT = RTSCMOB + ACMINERT
        CRUSSR = SRSCMOB + ACMINESR
        CRUSSH = ACMINESH
!      ENDIF

      ENDIF
C-----------------------------------------------------------------------
C      "Original" Forage model modification to code for adding CSAVEV
C      back to leaf, stem and STOR
C-----------------------------------------------------------------------
!        CADSR = CADSR + CSAVEV/PCH2O * LSTSR
!      CADLF = CADLF + CSAVEV/PCH2O * LSTR*(1-LSTSR)
!      CADST = CADST + CSAVEV * (1. - LSTR)*(1-LSTSR)/PCH2O
C-----------------------------------------------------------------------
C      5/31/05 SJR apportion CSAVEV to organs in proportion to their 
C      level of CH2O "deficit" or debt.  When deficit is really 
C      a surplus, dump all CSAVEV to STOR
C-----------------------------------------------------------------------
C      10/06/05 SJR Move calculation of xxDEBT to CH2OREF 
C-----------------------------------------------------------------------
C      10/31/05 SJR Created separate variable CADVG for CH2O reserved for
C            refilling vegetative tissues prior to NR1.  Previously CSAVEV 
C            had been used to carry both Pre-NR1 and Post-NR1 reserves.
C            This change heps delineat the difference in purposes for these
C            two periods.
C            When this was done, the CSAVEV code was returned to the 
C            original code from DSSAT4 for consistenc between the model
C            versions.
C-----------------------------------------------------------------------


      CADLF = CADLF + CADVG/PCH2O * LFCDEBT
      CADST = CADST + CADVG/PCH2O * STCDEBT 
      CADRT = CADRT + CADVG/PCH2O * RTCDEBT
      CADSR = CADSR + CADVG/PCH2O * SRCDEBT

      CADSR = CADSR + CSAVEV/PCH2O * LSTSR
      CADLF = CADLF + CSAVEV/PCH2O * LSTR*(1-LSTSR)
      CADST = CADST + CSAVEV * (1. - LSTR)*(1-LSTSR)/PCH2O

C-----------------------------------------------------------------------
C    Calculate Increase in Remobilizable N Due to a C shortage,
C      add to Nitrogen pool
C-----------------------------------------------------------------------
      NLEFT  = MAX(0.0,NAVL  -  (NGRLF  + NGRST  + NGRRT + NGRSR))
      IF (NLEFT .GT. 0.0) THEN


C-----------------------------------------------------------------------
C      Calculate capacity for refilling old tissue N if PGAVL were not
C      limiting.  This is the original calculation with no modifiers.
C      This may get tid of some N that would otherwise be NLEAK by 
C      using ONDMOLD instead of NDMOLD whish may have been limited 
C      by PGAVL
C      10/03/05 SJR Add code to stop refilling tissue that will be 
C      senesced today.
C-----------------------------------------------------------------------

!       ONDMOLD = (WTLF - SLMDOT - WCRLF) * MAX(0.0,(NVSTL - PCNL /100.))
!     &     + (STMWT - SSMDOT - WCRST) * 
!     &              MAX(0.0,(NVSTS - PCNST/100.))
!     &     + (RTWT  - SRMDOT - WCRRT) * 
!     &              MAX(0.0,(NVSTR - PCNRT/100.))
!     &         + (STRWT - SSRMDOT - WCRSR) * 
!     &              MAX(0.0,(NVSTSR - PCNSR/100.))
C-----------------------------------------------------------------------
C    02/01/06 SJR  Adjust N refill capacity for today's DM and N lost 
C                        to natural senescence.
C      Refill capacity for each organ is the max potential N content - current N content
C      Max potential N content = current organ mass (yesterday's mass 
C      without CH2O minus losses to natural senescence, need to add back 
C      C&N mobilized before senescence) X initial N concentration 
C      Current N content is WTNxx minus N lost in today's natural senescence
C-----------------------------------------------------------------------

       ONDMOLD = MAX(0.0,(WTLF - SLDOT - WCRLF) * PROLFR * 0.16  
     &    - (WTNLF -(SLDOT * PCNL/100 - LFSNMOB))) 
     &    + MAX(0.0,(STMWT - SSDOT - WCRST) * PROSTR * 0.16 
     &    - (WTNST - (SSDOT * PCNST /100 - STSNMOB)))
     &    + MAX(0.0,(RTWT  - SRDOT - WCRRT) * PRORTR * 0.16 
     &    - (WTNRT - (SRDOT * PCNRT / 100 - RTSNMOB)))
     &    + MAX(0.0,(STRWT - SSRDOT - WCRSR) * PROSRR * 0.16
     &    - (WTNSR -(SSRDOT * PCNSR / 100 - SRSNMOB)))

C-----------------------------------------------------------------------
C       10/03/05 SJR Limit refill to N uptake + TSBNMOB to
C      ensure that N mobilized form non-senescing tissues is not used 
C      to refilll old tissue
C-----------------------------------------------------------------------
        IF (ONDMOLD .GT. TRNU + TSNMOB) ONDMOLD = TRNU + TSNMOB
C-----------------------------------------------------------------------
C      If total capacity for refilling N allows, put "NLEAK" into NLEFT
C      Otherwise, allocate NLEAK to NLEFT to the amount allowed by ONDMOLD
C-----------------------------------------------------------------------
        IF (NLEFT .GT. ONDMOLD) THEN
        NLEAK = NLEFT - ONDMOLD
        NLEFT = ONDMOLD
        ELSE
        NLEAK = 0.0
        ENDIF

        NDMOLD = NLEFT
C-----------------------------------------------------------------------
C      SJR 10/20/03 Added code to fix/distribute NLEAK
C      NLEAK caused by 3 scenarios
C        I) Rounding/precision errors.
C       II) Changing FRRT & FRLF in VEGGR.FOR due to H2O or N stress, 
C             changes N demand.
C      III) Low Pg - exhaust PGAVL on N uptake.  Have N available but no
C             CH2O left for growth so N goes to refill old tissue.  
C             Particularly a problem when LFWT is low.  Also causes N% to 
C             exceed initial or maximum concentrations set in species file.
C-----------------------------------------------------------------------
C      SJR 10/16/03 Eliminate NLEAK values due to rounding errors
C            see I) above.
C-----------------------------------------------------------------------
        IF (NLEAK .LT. 0.0001) NLEAK = 0.0
      

C-----------------------------------------------------------------------
C    Original code allocated N back to only leaf, stem and root.
C    Uses FRxx which is the fraction of today's growth going to organ xx.
C    This is not indicative of the original "source" of the mobilized N being returned.
C-----------------------------------------------------------------------


C-----------------------------------------------------------------------
C      Allocate excess N uptake to new tissues
C-----------------------------------------------------------------------
C      Original DSSAT4 scheme - based on proportions of new growth
C      but came from  proportions of existing tissue
C-----------------------------------------------------------------------

!        NDMOLD = (WTLF  - WCRLF) * MAX(0.0,(NVSTL - PCNL /100.))
!     &     + (STMWT - WCRST) * MAX(0.0,(NVSTS - PCNST/100.))
!     &     + (RTWT  - WCRRT) * MAX(0.0,(NVSTR - PCNRT/100.))
!     &         + (STRWT - WCRSR) * MAX(0.0,(NVSTSR - PCNSR/100.))

!
!                    NADRAT = NLEFT / (FRLF*FNINL+FRSTM*FNINS+FRRT*FNINR
!               &                               +FRSTR*FNINSR)
!                    NADLF  = NADRAT * FRLF * FNINL
!                    NADST  = NADRAT * FRSTM * FNINS
!                    NADRT  = NADRAT * FRRT * FNINR

C-----------------------------------------------------------------------
C      sjr 10/16/03 Revised scheme based on existing growth proportions
C      10/03/05 SJR Revised equation to be consistent with earlier change 
C      preventing filling of tissues that will be senesced today.
C-----------------------------------------------------------------------
!      IF (NLEFT .GT. 0.0) THEN
C-----------------------------------------------------------------------
C      Allocate excess N uptake to refill old tissues
C-----------------------------------------------------------------------
C      11/04/05 SJR added MXWST and CWST to increase allocation of N to 
C            stems during drought, to mimic increase in NO3- concentration.
C-----------------------------------------------------------------------
        CWST = PWST + (1 - TURFAC) * (MXWST - PWST)

!        DNADRAT = PWLF * (WTLF - SLMDOT - WCRLF) * 
!     &              MAX(0.0,(NVSTL - PCNL /100.))
!     &    + CWST * (STMWT - SSMDOT - WCRST) * 
!     &              MAX(0.0,(NVSTS - PCNST/100.))
!     &    + PWRT * (RTWT  - SRMDOT - WCRRT) * 
!     &              MAX(0.0,(NVSTR - PCNRT/100.))
!     &        + PWSR * (STRWT - SSRMDOT - WCRSR) * 
!     &              MAX(0.0,(NVSTSR - PCNSR/100.))

C-----------------------------------------------------------------------
C    02/01/06 SJR  Adjust N refill capacity for today's DM and N lost 
C                        to natural senescence
C-----------------------------------------------------------------------

       DNADRAT = PWLF * MAX(0.0,(WTLF - SLDOT - WCRLF)*PROLFR*0.16  
     &    - (WTNLF -(SLDOT * PCNL/100 - LFSNMOB))) 
     &    + PWST * MAX(0.0,(STMWT - SSDOT - WCRST) * PROSTR * 0.16 
     &    - (WTNST -(SSDOT * PCNST/100 - STSNMOB))) 
     &    + PWRT * MAX(0.0,(RTWT  - SRDOT - WCRRT) * PRORTR * 0.16 
     &    - (WTNRT -(SRDOT * PCNRT/100 - RTSNMOB))) 
     &    + PWSR * MAX(0.0,(STRWT - SSRDOT - WCRSR) * PROSRR * 0.16
     &    - (WTNSR -(SSRDOT * PCNSR/100 - SRSNMOB))) 


        IF (DNADRAT .GT. 0.0) THEN
        NADRAT = NLEFT / DNADRAT 
        ELSE
        NADRAT = 0.0
        ENDIF

!                NADLF = NADRAT * PWLF * (WTLF - SLMDOT - WCRLF) * 
!     &              MAX(0.0,(NVSTL - PCNL /100.))
!
!            NADST = NADRAT * CWST * (STMWT - SSMDOT - WCRST) * 
!     &              MAX(0.0,(NVSTS - PCNST/100.))
!
!            NADRT = NADRAT * PWRT * (RTWT  - SRMDOT - WCRRT) * 
!     &              MAX(0.0,(NVSTR - PCNRT/100.))
!
!            NADSR = NADRAT * PWSR * (STRWT - SSRMDOT - WCRSR) * 
!     &              MAX(0.0,(NVSTSR - PCNSR/100.))
        
        NADLF = NADRAT * PWLF * MAX(0.0,(WTLF - SLDOT - WCRLF) * 
     &    PROLFR * 0.16 - (WTNLF -(SLDOT * PCNL/100 - LFSNMOB))) 

        NADST = NADRAT * PWST * MAX(0.0,(STMWT - SSDOT - WCRST) * 
     &    PROSTR * 0.16 - 
     &    (WTNST -(SSDOT * PCNST/100 - STSNMOB))) 

        NADRT = NADRAT * PWRT * MAX(0.0,(RTWT  - SRDOT - WCRRT) * 
     &    PRORTR * 0.16 -
     &    (WTNRT -(SRDOT * PCNRT/100 - RTSNMOB))) 

        NADSR = NADRAT * PWSR * MAX(0.0,(STRWT - SSRDOT - WCRSR) *      
     &    PROSRR * 0.16 - 
     &    (WTNSR -(SSRDOT * PCNSR/100 - SRSNMOB))) 



      ELSE
        NADRAT = 0.0
        NADLF = 0.0
        NADST = 0.0
        NADRT = 0.0
        NADSR = 0.0
        NLEAK = 0.0
      ENDIF




C-----------------------------------------------------------------------
C      SJR 10/20/03 Added code to fix/distribute NLEAK
C      Fix for II) and III) above.  
C      Basic strategy is to estimate how much growth could have been 
C      achieved if CH2O that wAs used for N mobilization, fixation, or uptake
C      was used for growth instead.  Then lower NLEAK and the appropriate
C      source by the amount of N used in and to fuel this growth.
C
C      Considered using PGAVL in this as well but did not because, to date,
C      NLEAK that would be addressed by this fix only occurs when PGAVL is 
C      exhausted.  If there were more PGAVL, more growth would have occurred
C      and there wouldn't be any NLEAK.
C
C      Strategy:
C      1) Figure out where it came from (mined, fixed or uptake)
C      2) Use appropriate respiration coefficient to calculate how much CH2O 
C        was used to liberate that amount of N
C      3) Calculate how much new growth could be generated if that amount 
C        of CH2O was used for growth and liberation of just enough
C        N for that new growth, no excess.
C      4) Recalculate N source (NMINEA, NFIXN, or TRNU).  Recalculate NLEAK
C      5) In case of TRNU, distrbute N to NO3 and NH4 in each soil layer
C      6) Recalculate growth rate and N concentration
C-----------------------------------------------------------------------

      CHORECOVER = 0.0
      NLKSPENT = 0.0
      NLKNUSED = 0.0
      NLKCHK = NLEAK
      TNLKCHK = TNLKCHK + NLEAK

      IF (NLEAK .GT. 0.0) THEN
        CALL NLKDIST(
     &    AGRLF, AGRRT, AGRSTM, AGRSTR, AGRVG, FNINL,     !Input
     &    FNINR, FNINS, FNINSR, FRLF, FRRT, FRSTM, FRSTR, !Input
     &    L, NLAYR, PROLFI, PRORTI, PROSRI, PROSTI,       !Input
     &    RNH4C, RNO3C, RPRO, TSNMOB,                     !Input
     &    ANMINELF, ANMINERT, ANMINEST, ANMINESR, NGRLF,  !Input/Output
     &    NGRRT, NGRST, NGRSR, NLEAK, NMINEA, NRUSLF,     !Input/Output
     &    NRUSRT, NRUSSR, NRUSST, TRNH4U, TRNO3U, TRNU,   !Input/Output
     &    UNO3, UNH4, WLDOTN, WRDOTN, WSDOTN, WSRDOTN,    !Input/Output
     &    CHORECOVER, NLKSPENT, NLKNUSED)                 !Output


        IF (NLEAK .LT. 0.0001) NLEAK = 0.0

        TNLEAK = TNLEAK + NLEAK

      ENDIF

      AAA=FRLF * FNINL/(FRLF*FNINL+FRSTM*FNINS+FRRT*FNINR
     &    +FRSTR*FNINSR)
      BBB=FRSTM * FNINS/(FRLF*FNINL+FRSTM*FNINS+FRRT*FNINR
     &    +FRSTR*FNINSR)
      CCC=FRRT * FNINR/(FRLF*FNINL+FRSTM*FNINS+FRRT*FNINR
     &    +FRSTR*FNINSR)
      DDD=FRSTR * FNINSR/(FRLF*FNINL+FRSTM*FNINS+FRRT*FNINR
     &    +FRSTR*FNINSR)
      ZZZ=AAA+BBB+CCC+DDD

      XXX=PNMLF+PNMST+PNMRT+PNMSR+PNMSH



C-----------------------------------------------------------------------
C     Subroutine FOR_CANOPY calculates height and width of the FOR_CANOPY as a
C     function of VSTAGE, air temperature, drought stress (TURFAC),
C     daylenght and radiation (PAR).
C-----------------------------------------------------------------------
      CALL FOR_CANOPY(
     &  ECONO, FILECC, FILEGC, PAR, ROWSPC,             !Input
     &  RVSTGE, TGRO, TURFAC, VSTAGE, XLAI,NSTRES,      !Input
     &  CANHT, CANWH,                                   !Output
     &  INTEGR)                                         !Control

!***********************************************************************
!     CALL FOR_DEMAND (
!    &  NRATIO)                                          !input 
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
C-----------------------------------------------------------------------
      END ! SUBROUTINE FOR_VEGGR

C-----------------------------------------------------------------------
C     Subroutine NLKDIST for distributing NLEAK
C      For each potential source of NLEAK:
C      2) Use appropriate respiration coefficient (NRSPCST)to calculate 
C        how much CH2O was used to liberate that amount of N
C      3) Calculate how much new growth could be generated if that amount 
C        of CH2O was used for growth and liberation of just enough
C        N for that new growth, no excess.
C-----------------------------------------------------------------------
      SUBROUTINE NLKDIST(
     &    AGRLF, AGRRT, AGRSTM, AGRSTR, AGRVG, FNINL,     !Input
     &    FNINR, FNINS, FNINSR, FRLF, FRRT, FRSTM, FRSTR, !Input
     &    L, NLAYR, PROLFI, PRORTI, PROSRI, PROSTI,       !Input
     &    RNH4C, RNO3C, RPRO, TSNMOB,                     !Input
     &    ANMINELF, ANMINERT, ANMINEST, ANMINESR, NGRLF,  !Input/Output
     &    NGRRT, NGRST, NGRSR, NLEAK, NMINEA, NRUSLF,     !Input/Output
     &    NRUSRT, NRUSSR, NRUSST, TRNH4U, TRNO3U, TRNU,   !Input/Output
     &    UNO3, UNH4, WLDOTN, WRDOTN, WSDOTN, WSRDOTN,    !Input/Output
     &    CHORECOVER, NLKSPENT, NLKNUSED)                 !Output
 
      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.
      IMPLICIT NONE

      REAL AGRLF, AGRRT, AGRSTM, AGRSTR, FNINL, FNINR,      
     &  FNINS, FNINSR, FRLF, FRRT, FRSTM, FRSTR,             
     &  NLEAK, PROLFI, PRORTI, PROSRI, PROSTI,  
     &  RNO3C, RNH4C, RPRO, TSNMOB,      
     &  ANMINELF, ANMINERT, ANMINEST, ANMINESR, NGRLF,      
     &  NGRRT, NGRST, NGRSR,NMINEA, NRUSLF, NRUSRT, NRUSSR, 
     &  NRUSST, TRNH4U, TRNO3U, TRNU, WLDOTN,      
     &  WRDOTN, WSDOTN, WSRDOTN,                                    
     &    CHORECOVER, NLKSPENT, NLKNUSED
     
      REAL AGRVG, AGRVGI, AGRVGPI, ANMINETOT,  
     &  NLEAK2, NLKNG1, 
     &  NLKNG2, NLKNG3, NLKRTRN1, NLKRTRN2, NLKRTRN3, 
     &  OCH2OCOST, PNUPNH4, PNUPNO3, PNTVG, RNNU, TRNURTRN 
        
      INTEGER L, NLAYR
      REAL UNH4(NL), UNO3(NL)

      NLEAK2 = 0.0
      NLKNG1 = 0.0
      NLKNG2 = 0.0
      NLKNG3 = 0.0
      NLKRTRN1 = 0.0
      NLKRTRN2 = 0.0
      NLKRTRN3 = 0.0
      CHORECOVER = 0.0
      NLKSPENT = 0.0
      NLKNUSED = 0.0
      TRNURTRN = 0.0

C      Calculate CH2O cost of new growth at initial N concentrations
        PNTVG=(FRLF*PROLFI+FRSTM*PROSTI+FRRT*PRORTI+FRSTR*PROSRI)*0.16
        AGRVGI=FRLF*AGRLF+FRSTM*AGRSTM+FRRT*AGRRT+FRSTR*AGRSTR
        AGRVGPI=FRLF*PROLFI+FRSTM*PROSTI+FRRT*PRORTI+
     &    FRSTR*PROSRI


C      Calculate CH2O cost for today's TRNU
        IF (TRNU .GT. 0.0)THEN
        PNUPNO3 = TRNO3U / TRNU
        PNUPNH4 = TRNH4U / TRNU
        RNNU= PNUPNO3 * RNO3C + PNUPNH4 * RNH4C
        ENDIF

C      Allocate NLEAK to a source.  Allocate first to N mobilization 
C      (NLKRTRN1), next to N uptake (NLKRTRN2), and lastly, to N from 
C      natural senescence (NLKRTRN3).
      NLKRTRN1 = MIN(NLEAK,NMINEA-TSNMOB)
      NLEAK2 = NLEAK - NLKRTRN1
      NLKRTRN2 = MIN(NLEAK2,TRNU)
      NLKRTRN3 = NLEAK2 - NLKRTRN2
      
C      For NLKRTRN1 and NLKRTRN2, "return" part of the excess N in 
C      exchange for "recovering" the CH2O that was spent acquiring it.  
C      Then use this CH2O and remaining N for new growth at "initial" N 
C      concentrations.  N from natural senescence cannot be returned as 
C      the tissue will be lost, so N is just added to the N pool.  
C      Need to formally add to the N pool as it will be subtracted out 
C      later as NRUSxx.

C      Calculate amount of new growth (NLKNGx) from each source of NLEAK.
C      Based on :
C      N in new growth = NLKNGx * PNTVG (amount of new growth * proportion N 
C      in new growth (assume to be "initial" N concentration.
C      N to be returned to provide the CH2O required for new growth
C      NLKNGx * CH2O cost/g new growth * 1/CH2O recivered/g N returned.
C      NLKRTRNx = the sum of these two (N required for new growth + 
C      N returned for CH2O). Set NLKRTRNx = sum of the two equations and 
C      solve algebraically for NLKNGx to find how much new growth can be 
C      generated from each component of NLEAK.  Note that no N is returned to 
C      natural senescence so "new growth" = N/0.16.

      NLKNG1 = NLKRTRN1 * 1/(PNTVG + AGRVGI / (RPRO * 0.16))
      IF (NLKRTRN2 .GT. 0.0) NLKNG2 = NLKRTRN2 * 
     &    1 / (PNTVG + AGRVGI / (RNNU * 0.16))
      NLKNG3 = NLKRTRN3 / 0.16
C      Calculate total CH2O cost of growth before adding in NLEAK growth
      OCH2OCOST = AGRVG * (WLDOTN + WSDOTN +WRDOTN + WSRDOTN)

C      Add new growth to WxDOTN
      WLDOTN = WLDOTN + (NLKNG1 + NLKNG2 + NLKNG3) * FRLF
      WSDOTN = WSDOTN + (NLKNG1 + NLKNG2 + NLKNG3) * FRSTM
      WRDOTN = WRDOTN + (NLKNG1 + NLKNG2 + NLKNG3) * FRRT
      WSRDOTN = WSRDOTN + (NLKNG1 + NLKNG2 + NLKNG3) * FRSTR      

C      Add N in new growth to NGRxx
      NGRLF = NGRLF + (NLKNG1 + NLKNG2) * FRLF *FNINL + NLKRTRN3 * FRLF            
      NGRST = NGRST + (NLKNG1 + NLKNG2) * FRSTM *FNINS + 
     &    NLKRTRN3 * FRSTM            
      NGRRT = NGRRT + (NLKNG1 + NLKNG2) * FRRT *FNINR + NLKRTRN3 * FRRT            
      NGRSR = NGRSR + (NLKNG1 + NLKNG2) * FRSTR *FNINSR + 
     &    NLKRTRN3 * FRSTR            

C      Adjust NRUSxx for N "returned" for CH2O
C      Adjust ANMINExx for N "returned" for CH2O
      ANMINETOT = ANMINELF + ANMINEST + ANMINERT + ANMINESR
      IF (ANMINETOT .GT. 0.0) THEN
      ANMINELF = ANMINELF - NLKNG1 * (AGRVGI / (RPRO * 0.16)) * 
     &    ANMINELF/ANMINETOT
      ANMINEST = ANMINEST - NLKNG1 * (AGRVGI / (RPRO * 0.16)) *  
     &    ANMINEST/ANMINETOT
      ANMINERT = ANMINERT - NLKNG1 * (AGRVGI / (RPRO * 0.16)) *  
     &    ANMINERT/ANMINETOT
      ANMINESR = ANMINESR - NLKNG1 * (AGRVGI / (RPRO * 0.16)) * 
     &    ANMINESR/ANMINETOT


      ANMINETOT = ANMINELF + ANMINEST + ANMINERT + ANMINESR

      NRUSLF = NRUSLF - NLKNG1 * (AGRVGI / (RPRO * 0.16)) * 
     &    ANMINELF/ANMINETOT
      NRUSST = NRUSST - NLKNG1 * (AGRVGI / (RPRO * 0.16)) * 
     &    ANMINEST/ANMINETOT
      NRUSRT = NRUSRT - NLKNG1 * (AGRVGI / (RPRO * 0.16)) * 
     &    ANMINERT/ANMINETOT
      NRUSSR = NRUSSR - NLKNG1 * (AGRVGI / (RPRO * 0.16)) * 
     &    ANMINESR/ANMINETOT
      ENDIF

C      Calculate how much NLEAK was used as N and how much was 
C      returned for CH2O

      CHORECOVER = NLKNG1 * AGRVGI + NLKNG2 * AGRVGI
      AGRVG = (OCH2OCOST + CHORECOVER) / 
     &    (WLDOTN + WSDOTN + WRDOTN + WSRDOTN)
      IF (TRNU .GT. 0.0) THEN
      NLKSPENT =  NLKNG1 * AGRVGI / (RPRO * 0.16) + 
     &    NLKNG2 * AGRVGI / (RNNU * 0.16)
      ELSE
      NLKSPENT =  NLKNG1 * AGRVGI / (RPRO * 0.16) 
      ENDIF

      NLKNUSED = NLKNG1 * PNTVG + NLKNG2 * PNTVG + NLKRTRN3
      NLEAK = NLEAK - (NLKSPENT + NLKNUSED)

C      Return NLKSPENT to its sources
      NMINEA = NMINEA - NLKNG1 * (AGRVGI / (RPRO * 0.16))  
      IF (TRNU .GT. 0.0) THEN
      TRNURTRN = NLKNG2 * AGRVGI / (RNNU * 0.16)


C      Allocate N "returned for respiration" (CH2O) to layers as NO3&NH4

        DO L = 1,NLAYR
        IF (TRNO3U .GT. 0.0) THEN
        UNO3(L) = UNO3(L) - TRNURTRN * 10 * PNUPNO3 * 
     &    (UNO3(L)/(TRNO3U * 10))
        ENDIF

        IF (TRNH4U .GT. 0.0) THEN
        UNH4(L) = UNH4(L) - TRNURTRN * 10 * PNUPNH4 * 
     &    (UNH4(L)/(TRNH4U * 10))
        ENDIF
        
        ENDDO

      TRNO3U = TRNO3U - PNUPNO3 * TRNURTRN * 10
      TRNH4U = TRNH4U - PNUPNH4 * TRNURTRN * 10
      TRNU=TRNU - TRNURTRN

      ENDIF

      RETURN
C-----------------------------------------------------------------------
      END ! SUBROUTINE NLKDIST

C-----------------------------------------------------------------------
! AGRLF   Mass of CH2O required for new leaf growth (g[CH2O] / g[leaf])
! AGRRT   Mass of CH2O required for new root growth (g[CH2O] / g[root])
! AGRSTM  Mass of CH2O required for new stem growth (g[CH2O] / g[stem])
! AGRSTR  Mass of CH2O required for new storage organ growth 
!              (g[CH2O] / g[storage])
! AGRVG   Mass of CH2O required for vegetative tissue growth including 
!           stoichiometry and respiration (g[CH2O] / g[tissue])
! AGRVGI      CH2O cost for new growth from NLEAK, excludes  cost for 
!            liberating and reducing N to CP (g[CH2O] / g[tissue])
! AGRVGPI Amount of protein in new growth - used to calculate CH2O cost 
! ANMINELF Actual leaf N mobilized in excess of that from natural senescence
!                  (g[N] / m2 / d)
! ANMINERT Actual root N mobilized in excess of that from natural senescence
!                  (g[N] / m2 / d)
! ANMINESH Actual shell N mobilized 
!                  (g[N] / m2 / d)                  
! ANMINESR Actual STOR N mobilized in excess of that from natural senescence
!                  (g[N] / m2 / d)
! ANMINEST Actual stem N mobilized in excess of that from natural senescence
!                  (g[N] / m2 / d)
! ANMINETOT Total N mobilized in excess of that from natural senescence
!                  (g[N] / m2 / d)
! ATOP    Maximum fraction change in partitioning from top growth to roots 
!           if severe water or nitrogen stresses occur. 
! CADLF   Mass of CH2O added to leaf reserves after growth
!           (g[CH2O] / m2 / d)
! CADRT      Mass of CH2O added back to roots for CH2O cost of excess mobilized N 
!            (never really mobilized) (g[CH2O] / m2 / d)
! CADSH      Mass of CH2O added back to shells for CH2O cost of excess mobilized N 
!            (never really mobilized) (g[CH2O] / m2 / d)
! CADSR      Mass of CH2O added to storage organs (g[CH2O] / m2 / d)
! CADST   Mass of CH2O added to stems (g[CH2O] / m2 / d)
! CADSRF      Proportion of CH2O reserves that are added to storage organ
!              (fraction)
! CADSTF  Proportion of CH2O reserves that are added to stems (fraction)
! CADVG        CH2O to be reserved today for CH2O refill of vegetative 
!                  organs g [CH2O] m-2 d-1
! CANHT   Canopy height (m)
! CANWH   Canopy width normal to row (m)
! CH2OCST      CH2O cost for new growth generated from "recovering" NLEAK
!            (g[CH2O] / m2 / d)
! CHORECOVER CH2O "recovered" by returning part of N in NLEAK
! CLAIT   LAI threshold triggering incresed mobilization from storage
!              organ tissue - represents dramatic loss due to harvest 
!              or damage
! CMINEA  Actual carbon mined from vegetative tissue (g[CH2O] / m2 / d)
! CMINEP  Potential CH2O mobilization from storage (g[CH2O] / m2 / d)
! CMOBMX  Maximum C pool mobilization rate (g[CH2O] / m2 / d)
! CMOBSR    Stage-dependent potential C mining rate from storage organ 
!             expressed as a fraction of the maximum rate (CMOBSRX)
! CMOBSRN   "Normal" (minimum) fraction of C which can be mobilized from
!                  storage organ in a day 
! CMOBSRX   Maximum fraction of C which can be mobilized from
!                  storage organ in a day 
! CRUSLF  C mobilized from leaf tissue in a day (g[CH2O] / m2 / d)
! CRUSRT  C mobilized from root tissue in a day (g[CH2O] / m2 / d)
! CRUSSH  C mobilized from shell tissue in a day (g[CH2O] / m2 / d)
! CRUSSR  C mobilized from storage organ tissue in a day (g[CH2O] / m2 / d)
! CRUSST  C mobilized from stem tissue in a day (g[CH2O] / m2 / d)
! CSAVEV  Fraction of PG for VEG that is stored as CH2O 
! CUMTUR  Cumulative turgor factor - 20 day water stress average 
! CWST      "Current" weighting factor for partitioning NAD to stems
! DAS     Days after start of simulation (days)
! DNADRAT      Denominator of calculation for NADRAT.  Separated out to 
!            prevent divide by 0.0 errors.
! DTX     Thermal time that occurs in a real day based on vegetative 
!           development temperature function (thermal days / day)
! DXR57   Relative time between first seed (NR5) and physiological maturity 
!           (NR7) 
! DYNAMIC Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, INTEGR, 
!           OUTPUT, or SEASEND 
! ECONO   Ecotype code - used to match ECOTYP in .ECO file 
! ERR     Error code for file operation 
! ERRKEY  Subroutine name for error file 
! EXCESS  Factor based on excess PG used to affect tomorrow's PG 
!           calculation 
! FILECC  Path plus filename for species file (*.spe) 
! FILEGC  Pathname plus filename for ECO file 
! FNINL   Maximum fraction of N for growing leaf tissue (g[N] / g[leaf])
! FNINLG  Minimum fraction of N for growing leaf tissue (g[N] / g[leaf])
! FNINR   Maximum fraction of N for growing root tissue (g[N] / g[root])
! FNINRG  Minimum fraction of N for growing root tissue (g[N] / g[root])
! FNINS   Maximum fraction of N for growing stem tissue (g[N] / g[stem])
! FNINSG  Minimum fraction of N for growing stem tissue (g[N] / g[stem])
! FNINSR  Maximum fraction of N for growing storage organ tissue 
!              (g[N] / g[storage])
! FNINSRG Minimum fraction of N for growing storage organ tissue 
!            (g[N] / g[storage])
! FOUND   Indicator that good data was read from file by subroutine FIND (0 
!           - End-of-file encountered, 1 - NAME was found) 
! FRLF    Fraction of vegetative tissue growth that goes to leaves on a day
!           (g[leaf] / g[veg])
! FRRT    Fraction of vegetative tissue growth that goes to roots on a day
!           (g[root] / g[veg])
! FRSTM   Fraction of vegetative tissue growth that goes to stems on a day
!           (g[stem] / g[veg])
! FRSTR   Fraction of vegetative tissue growth that goes to storage organs 
!           on a day (g[storage] / g[veg])
! ISECT   Data record code (0 - End of file encountered, 1 - Found a good 
!           line to read, 2 - End of Section in file encountered, denoted 
!           by * in column 1
! L            A counting variable
! LFCDEBT        Proportion of total plant CH2O "deficit" attributed to leaves
! LKNRET      "Generic" variable for amount of NLEAK potentially attributed 
!            to an N source (NMINEA, NFIXN, TRNU) (g[N] / m2 / d)
! LNUM    Current line number of input file 
! LSTR    Ratio of excess C to be added to leaves in a day relative to the 
!           amount to be stored in stems 
! LSTSR   Ratio of excess C to be added to stolon in a day relative to the 
!           amount to be stored in stems and leaves 
! LUNCRP  Logical unit number for FILEC (*.spe file) 
! MXWST   Maximum weighting factor for allocating NAD to stems.  This would
!            be the weighting factor if TURFAC=0.0 (maximum water stress).  
!            Emphasizes allocation of N to stems during drought.
! NADLF   N added to leaf N reserves (g[N] / m2 / d)
! NADRAT  Total nitrogen added to vegetative N reserves (g[N] / m2 / d)
! NADRT   N added to root N reserves (g[N] / m2 / d)
! NADSH   N added back to shell N reserves - excess mobilized N 
!            (never really mobilized)(g[N] / m2 / d)
! NADSR   N added to storage organ N reserves (g[N] / m2 / d)
! NADST   N added to stem N reserves (g[N] / m2 / d)
! NAVL    Total mass of nitrogen available for growth (g[N] / m2 / d)
! NDMNEW  Total N demand for new growth (g[N] / m2 / d)
! NDMOLD  N demand for old tissue (g[N] / m2 / d)
! NFIXN   Amount of N fixed during the day (g[N] / m2 / d)
! NGRLF   Maximum N demand for leaf growth (g[leaf N] / m2[ground] / d)
! NGRLFG  Minimum N requirement for leaf growth
!           (g[leaf N] / m2[ground] / d)
! NGRRT   Maximum N demand for root growth (g[root N] / m2[ground] / d)
! NGRRTG  Minimum N requirement for root growth
!           (g[leaf N] / m2[ground] / d)
! NGRSR   Maximum N demand for storage organ growth 
!              (g[storage N] / m2[ground] / d)
! NGRSRG  Minimum N requirement for storage organ growth
!           (g[storage N] / m2[ground] / d)
! NGRST   Maximum N demand for stem growth (g[stem N] / m2[ground] / d)
! NGRSTG  Minimum N requirement for stem growth
!           (g[stem N] / m2[ground] / d)
! NGRVEG  Maximum N demand for vegetative tissue growth
!           (g[veg N] / m2[ground] / d)
! NGRVGG  Minimum N requirement for vegetative tissue growth
!           (g[veg N] / m2[ground] / d)
! NL         Maximum number of soil layers = 20 
! NLAYR      Actual number of soil layers 
! NLEAK   Nitrogen leak (g[N] / m2 / d)
! NLEAK2  Amount of NLEAK attributable to TRNU + natural senescence (g[N] / m2 / d)
! NLEFT   Nitrogen left after vegetative demands are met (g[N] / m2 / d)
! NLKNG1  New growth from NLEAK1 g [DM] m-2 d-1
! NLKNG2  New growth from NLEAK2 g [DM] m-2 d-1
! NLKNG3  New growth from NLEAK3 g [DM] m-2 d-1
! NLKRTRN1 Amount of NLEAK attributable to N mobilization (g[N] / m2 / d)
! NLKRTRN2 Amount of NLEAK attributable to TRNU (g[N] / m2 / d)
! NLKRTRN3 Amount of NLEAK attributable to natural senescence (g[N] / m2 / d)
! NLKNUSED Amount of NLEAK incorporated into new growth g [N] m-2 d-1
! NLKSPENT Amount of N from NLEAK retrned to recover CHORECOVER g [N] m-2 d-1
! NMINEA  Actual Nitrogen mined from existing tissue (g[N] / m2 / d)
! NR1     Day when 50% of plants have at least one flower (days)
! NRATIO  Factor to reduce tissue growth based on low available nitrogen 
! NRFRESP N "recovered for respiration".  NLEAK "returned to source in 
!            exchange for the CH2O used to acquire it (g[N] / m2 / d)
! NRSPCST      "Generic" variable name for N respiration cost or CH2O cost
!            to acquire a gram of N from a given source. Assumes values of 
!            RPRO, RFIXN, or RNNU depending on N source. (g[CH2O] / g[protein]) 
! NRUSLF  N actually mobilized from leaves in a day (g[N]/m2-d)
! NRUSRT  N actually mobilized from roots in a day (g[N]/m2-d)
! NRUSSH  N actually mobilized from shells in a day (g[N]/m2-d)
! NRUSSR  N actually mobilized from storageorgan in a day (g[N]/m2-d)
! NRUSST  N actually mobilized from stems in a day (g[N]/m2-d)
! NSOURCE "Generic" variable name for N source that NLEAK is being "returned"
!            to.  Assumes the values of NMINEA, NFIXN, or TRNU depending on 
!            source of N (g[N] / m2 / d)
! NSTRES  Nitrogen stress factor (1=no stress, 0=max stress) 
! NUPNO3(L)      Proportion of TRNU from a soil layer that is nitrate
! NUPNH4(L)      Proportion of TRNU from a soil layer that is ammonium
! ONDMDOLD Total N demand for old tissue (g[N] / m2 / d) - not limited bt PGAVL.
! PAR     Daily photosynthetically active radiation or photon flux density
!           (moles[quanta]/m2-d)
! PCH2O   Respiration loss due to storage/mobilization of CH2O
!           (g[CH2O] / g[CH2O])
! PG      Daily gross photosynthesis (g[CH2O] / m2 / d)
! PGAVL   Total available CH2O available for growth & respiration
!           (g[CH2O] / m2)
! PGLEFT  Excess PG after today's tissue growth (g[CH2O] / m2)
! PNMLF      Proportion of actually mobilized N mobilized from leaves in a day
! PNMST      Proportion of actually mobilized N mobilized from stems in a day
! PNMRT      Proportion of actually mobilized N mobilized from roots in a day
! PNMSR      Proportion of actually mobilized N mobilized from storage organ in a day
! PNMSH      Proportion of actually mobilized N mobilized from shells in a day
! PNTVG      Percent N in vegetative tissues at initial or max protein concentration.
! PNUPNH4      Proportion of TRNU that is NH4
! PNUPNO3      Proportion of TRNU that is NO3
! PPMFAC  Reduction in mobilization from storage organ due to photoperiod 
!              induced dormancy
! PROLFG  Normal growth protein composition in leaves during growth
!           (g[protein] / g[leaf tissue])
! PROLFI  Maximum protein composition in leaves during growth with 
!           luxurious supply of N (g[protein] / g[leaf tissue])
! PROLFT  Protein fraction of new leaf growth (g[protein] / g[leaf tissue])
! PRORTG  Normal growth protein composition in roots during growth
!           (g[protein] / g[root])
! PRORTI  Maximum protein composition in roots during growth with luxurious 
!           supply of N (g[protein] / g[root])
! PRORTT  Protein fraction of new root growth (g[protein] / g[root])
! PROSRG  Normal growth protein composition in storage organ during growth
!           (g[protein] / g[storage])
! PROSRI  Maximum protein composition in storage organ during growth with  
!           luxurious supply of N (g[protein] / g[storage])
! PROSRT  Protein fraction of new storage organ growth (g[protein] / g[storage])
! PROSTG  Normal growth protein composition in stems during growth
!           (g[protein] / g[stem])
! PROSTI  Maximum protein composition in stems during growth with luxurious 
!           supply of N (g[protein] / g[stem])
! PROSTT  Protein fraction of new stem growth (g[protein] / g[stem])
! PWLF    Weighting factors for partitioning N when refilling  old leaf tissues
! PWRT    Weighting factors for partitioning N when refilling  old root tissues
! PWSR    Weighting factors for partitioning N when refilling  old STOR tissues
! PWST    Weighting factors for partitioning N when refilling  old stem tissues
! RFIXN      CH2O required for biological N fixation (g[CH2O] / g[protein])
! RNH4C   CH2O required for protein synthesis when source of N is 
!             ammonium uptake (g[CH2O] / g[protein])
! RNNGR      (g[N] / m2 / d) that must be "returned" to "release" enough CH2O 
!            to produce 1 gram of new growth 
! RNNU      Actual CH2O required for protein synthesis when source of N is 
!             ammonium + nitrate uptake (g[CH2O] / g[protein])
! RNO3C      CH2O required for protein synthesis when source of N is 
!             nitrate uptake (g[CH2O] / g[protein])
! ROWSPC  Row spacing (m)
! RPRO    Respiration required for re-synthesizing protein from mobilized N
!           (g[CH2O] / g[protein])
! RTCDEBT        Proportion of total plant CH2O "deficit" attributed to roots
! RVSTGE  Rate of VSTAGE change (nodes/day)
! SECTION Section name in input file 
! SRCDEBT        Proportion of total plant CH2O "deficit" attributed to STOR
! STCDEBT        Proportion of total plant CH2O "deficit" attributed to stems
! STRWT   Dry mass of storage organ tissue, including C and N 
!              (g[storage] / m2[ground)
! STMWT   Dry mass of stem tissue, including C and N (g[stem] / m2[ground)
! SUPPN   Total supply of N (g[N] / m2 / d)
! TGRO(I) Hourly air temperature (°C)
! TIMDIF  Integer function which calculates the number of days between two 
!           Julian dates (da)
! TNLEAK  Total nitrogen leak (g[N] / m2 / d)
! TRNH4U      Total N uptake in ammonium form in a day (g[N] / m2 / d)
! TRNO3U      Total N uptake in nitrate form in a day (g[N] / m2 / d)
! TRNU    Total N uptake in a day (g[N] / m2 / d)
! TRNURTRN Amount of NLKRTRN2 returned to soil to recover CH2O for NLKNG2 g [N] m-2 d-1
! TS      Number of intermediate time steps (=24) 
! TURFAC  Water stress factor for expansion (0 - 1) 
! UNO3    Uptake of NO3 from soil (interim value) (kg N/ha)
! UNH4    Uptake of NH4 from soil (interim value) (kg N/ha)
! VGRDEM  Vegetative growth demand (g[vegetative tissue] / m2-d)
! VSTAGE  Number of nodes on main stem of plant 
! WCRLF   Mass of CH2O reserves in leaves (g[leaf CH2O] / m2[ground])
! WCRRT   Mass of CH2O reserves in roots (g[root CH2O] / m2[ground])
! WCRSH   Mass of CH2O reserves in shells (g[shell CH2O] / m2[ground])
! WCRSR   Mass of CH2O reserves in storage organ (g[storage CH2O] / m2[ground])
! WCRST   Mass of CH2O reserves in stems (g[stem CH2O] / m2[ground])
! WLDOTN  Dry weight growth rate of new leaf tissue including N but not C 
!           reserves (g[leaf] / m2[ground]-d)
! WRDOTN  Dry weight growth rate of new root tissue including N but not C 
!           reserves (g[root] / m2[ground]-d)
! WSRDOTN Dry weight growth rate of new storage organ tissue including N  
!           but not C reserves (g[stem] / m2[ground]-d)
! WSDOTN  Dry weight growth rate of new stem tissue including N but not C 
!           reserves (g[stem] / m2[ground]-d)
! WTLF    Dry mass of leaf tissue including C and N (g[leaf] / m2[ground])
! XLAI    Leaf area (one side) per unit of ground area
!           (m2[leaf] / m2[ground])
! XTVEGM      "Extra" new growth generated from "returning" NLEAK to its sources
!            in exchange for the CH2O used originally to acquire that amount 
!            of N (g[vegetative tissue] / m2-d)
! YRDOY   Current day of simulation (YYDDD)
! YRSIM   Start of simulation date (YYDDD)
!-----------------------------------------------------------------------
!     END SUBROUTINE FOR_VEGGR
!-----------------------------------------------------------------------


