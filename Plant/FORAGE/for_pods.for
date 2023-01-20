C=======================================================================
C  FOR_PODS, Subroutine, K. J. Boote, J. W. Jones, G. Hoogenboom
C-----------------------------------------------------------------------
C  Computes seed and shell growth; initiation of flowers, shells, seed.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/01/1989     Written.
C  04/24/1994 NBP Changed TAIRHR to TGRO.
C  08/22/1995 GH  Added seed protein/oil calculations
C  01/19/1996 KJB Adjustments to code
C  04/02/1996 KJB Adjustments to nitrogen
C  01/10/1997 GH  Reorganize and restructure code
C  01/22/1998 GH  Extracted seed composition code
C  09/28/1998 CHP Modified for modular structure
C  05/11/1999 GH  Incorporated in CROPGRO
C  01/13/2000 NBP Changed minimum for ADDSHL and PGLEFT from 1E-30 to 1E-6
C  06/21/2001 GH  Updated initialization variables
C  06/26/2001 GH  Set NR2TIM
C-----------------------------------------------------------------------
C  Called from:  PLANT
C  Calls:        FOR_PODCOMP
C                ERROR, FIND, IGNORE
C=======================================================================

      SUBROUTINE FOR_PODS(DYNAMIC, !RUN, 
     &    AGRSD1, AGRSH1, DLAYR, DRPP, DUL, FILECC,       !Input
     &    FILEGC,FILEIO, FNINL, FNINSD, FNINSH, GDMSD,    !Input
     &    GRRAT1, ISWWAT, LL, NAVL, NDSET, NLAYR, NRUSSH, !Input
     &    NSTRES, PGAVL, PHTHRS, PHTIM, PNTIM, PUNCSD,    !Input
     &    PUNCTR, RNITP, SDDES, SDGR, SHELWT, SW, SWFAC,  !Input
     &    TDUMX, TGRO, TURADD, XFRT, YRDOY, YRNR1, YRNR2, !Input
     7    YRSIM,                                          !Input
     &    AGRSD3, LAGSD, LNGPEG, NGRSD, NGRSH, PCTMAT,    !Output
     &    PODNO, POTCAR, POTLIP, SDNO, SDVAR, SEEDNO,     !Output
     &    SHELN, SHVAR, WSDDTN, WSHDTN, WTABRT, WTSD,     !Output
     &    WTSHE, WTSHMT, FLWN)                            !Output

!     2023-01-20 CHP Remove unused variables in argument list:
!     RUN
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE, FOR_PODCOMP, WARNING, 
     &  TIMDIF, CURV, TABEX
      SAVE

      CHARACTER*1   ISWWAT
      CHARACTER*2   CROP
      CHARACTER*3   TYPPDT
      CHARACTER*6   ERRKEY
      PARAMETER (ERRKEY = 'PODS  ')
      CHARACTER*6   ECOTYP, ECONO
      CHARACTER*6   SECTION
      CHARACTER*30 FILEIO
      CHARACTER*78 MESSAGE(10)
      CHARACTER*80  C80
      CHARACTER*92  FILECC, FILEGC
      CHARACTER*255 C255

      INTEGER LUNECO, LUNCRP, LUNIO, ERR, LNUM, FOUND, ISECT, II
      INTEGER NPP,NAGE,I,NLAYR
      INTEGER DYNAMIC, TIMDIF, NR1TIM, NR2TIM
      INTEGER YRDOY, YRNR1, YRNR2, YRSIM, DAS
      INTEGER NDSET  !, RUN
      INTEGER TRIGGR

      REAL AGRSD1, FNINSD, GDMSD, NAVL, WSDDTN, WSHDTN, NGRSD, NGRSH
      REAL TEMPOD, PCTMAT, WTSHM, NSTRES, PGAVL, XFRT, LAGSD, THRESH
      REAL WTSHMT, TURADD, SDGR, DSWBAR, SWBAR, SWADD1, SWADD2
      REAL SHVAR, LNGSH, GRRAT1, LNGPEG, AGRSH1, FNINSH, NLEFT, SHLAG
      REAL PROSHI, SHELWT, NRUSSH, WTABRT, TDUMX, SWFAC, SETMAX, DRPP
      REAL SDPDVR, RFLWAB, PMAX, SDVAR, PODUR, MNESPM, RNITP
      REAL PROLFF, FNINL, SEEDNO, PODNO, XMPAGE
      REAL WTPSD, SFDUR, PROSHF

      REAL NAVPOD, ADDSHL, FLWADD
      REAL PGLEFT, PODMAT, AFLW, FLWRDY, PODADD, SHMINE, ACCAGE, PGAVLR
      REAL REDPUN, WTSHMY, PAGE, REDSHL, SDMAX, RSD, PGNPOD, SHMAXG
      REAL SUPDAY, SDMAXX, SHRAT, WTABR, START, PNAGE, FLWFRC, FLADD
      REAL ACTSW, POTSW, DSW, FLAYR, CURV, TABEX
      REAL CUMSIG, CNSTRES, AGRSD3, ANINSD
      REAL RNITPD, POTLIP, POTCAR
      REAL CRSD, NRSD, NREQ

!     Puncture variables, not yet functional
      REAL PUNCSD, PUNCTR, RPRPUN     

      REAL FNPDT(4)
      REAL XSWBAR(10), YSWBAR(10), XSWFAC(10), YSWFAC(10)
      REAL DLAYR(NL), SW(NL), LL(NL), DUL(NL)
      REAL PHTHRS(20)
      REAL TGRO(TS)
      REAL WTSD(NCOHORTS), WTSHE(NCOHORTS), SDNO(NCOHORTS)
      REAL SHELN(NCOHORTS), SDDES(NCOHORTS), SUPDE(NCOHORTS)
      REAL AVTEM(NCOHORTS), FLWN(NCOHORTS)
      REAL PHTIM(365), PNTIM(365)

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!    Find and Read Simulation Control Section from FILEIO
!               Previously read in IPIBS
!-----------------------------------------------------------------------
      CALL GETLUN('FILEIO', LUNIO)
      OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)

!-----------------------------------------------------------------------
!    Find and Read Cultivars Section from FILEIO
!-----------------------------------------------------------------------
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILEIO, LNUM)
      ELSE
        READ(LUNIO,'(3X,A2)') CROP
      ENDIF

!-----------------------------------------------------------------------
!    Find and Read Cultivars Section from FILEIO
!-----------------------------------------------------------------------
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILEIO, LNUM)
      ELSE
        READ(LUNIO,'(24X,A6,66X,5F6.0)')
     &    ECONO, WTPSD, SFDUR, SDPDVR, PODUR, THRESH
      ENDIF

      CLOSE (LUNIO)

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
      SECTION = '!*PLAN'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(12X,F6.0)',IOSTAT=ERR) PROLFF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(18X,F6.0,6X,F6.0)',IOSTAT=ERR) PROSHI, PROSHF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF
!-----------------------------------------------------------------------
!    Find and Read Seed and Shell Growth Section
!-----------------------------------------------------------------------
      SECTION = '!*SEED'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ENDIF
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(F6.0,6X,2F6.0)',IOSTAT=ERR) SETMAX, RFLWAB, XMPAGE
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(F6.0,6X,F6.0)',IOSTAT=ERR) DSWBAR, SHLAG
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4F6.0,3X,A3)',IOSTAT=ERR)(FNPDT(II),II=1,4),TYPPDT
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        DO I=1,4
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        ENDDO
        READ(C80,'(4F6.0)',IOSTAT=ERR)(XSWFAC(II),II=1,4)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4F6.0)',IOSTAT=ERR)(YSWFAC(II),II=1,4)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(5F6.0)',IOSTAT=ERR)(XSWBAR(II),II=1,5)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(5F6.0)',IOSTAT=ERR)(YSWBAR(II),II=1,5)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF
      CLOSE (LUNCRP)

!-----------------------------------------------------------------------
C    Read Ecotype Parameter File
C-----------------------------------------------------------------------
      CALL GETLUN('FILEE', LUNECO)
      OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERR)

      ISECT = 2
      DO I=1,200
        CALL IGNORE(LUNECO, LNUM, ISECT, C255)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,0)
        IF ((ISECT .EQ. 1) .AND. (C255(1:1) .NE. ' ') .AND.
     &    (C255(1:1) .NE. '*')) THEN
        READ (C255,'(A6,66X,F6.0,30X,F6.0)',IOSTAT=ERR)
     &    ECOTYP, LNGSH       !, THRESH
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)
        IF (ECOTYP .EQ. ECONO) THEN
        EXIT
        ENDIF
        ELSE IF (ISECT .EQ. 0) THEN
        IF (ECONO .EQ. 'DFAULT') CALL ERROR(ERRKEY,3,FILEGC,LNUM)
        ECONO = 'DFAULT'
        REWIND(LUNECO)
        ENDIF
      ENDDO

      CLOSE (LUNECO)

!-----------------------------------------------------------------------

      CALL FOR_PODCOMP(
     &  AGRSD1, FILECC, FNINSD, GDMSD, NAVL, PGAVLR,    !Input
     &  POTCAR, POTLIP,                                 !Input/Output
     &  AGRSD3, ANINSD, CUMSIG, RSD,                    !Output
     &  RUNINIT)                                        !Control

!-----------------------------------------------------------------------
C     Set minimum days for phenological events under optimum conditions
C     (temperature and short photoperiod)
C-----------------------------------------------------------------------
C     Number of days from end pod set to physiological maturity
        MNESPM = PHTHRS(10) - PHTHRS(9)

C-----------------------------------------------------------------------
C     Number of days between start of peg (full flower) and shell
C     formation
C     Only used in peanut to define slow growth period.
        LNGPEG  = PHTHRS(7) - PHTHRS(6)

C-----------------------------------------------------------------------
C     Number of days between start of shell and seed formation of a pod
        LAGSD  = PHTHRS(8) - PHTHRS(6)

C-----------------------------------------------------------------------
C     Compute reproductive rates from cultivar and ecotype coefficients
C-----------------------------------------------------------------------
       SDVAR = WTPSD / SFDUR
       SHVAR = WTPSD * SDPDVR * ((100.-THRESH)/THRESH)/
     &    ((LNGSH-.85*LNGPEG)*((1.-PROSHI)/(1.-PROSHF)))

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      FNINSH = 0.0   
      NAVPOD = 0.0
      NGRSD  = 0.0   
      NGRSH  = 0.0   
      NR2TIM = 0
      PCTMAT = 0.0   
      PGNPOD = 0.0
      PODNO  = 0.0   
      WSDDTN = 0.0   
      WSHDTN = 0.0   
      WTABRT = 0.0   
      WTSHM  = 0.0   
      WTSHMT = 0.0   
      WTSD   = 0.0
      WTSHE  = 0.0

      RPRPUN = 1.0 
      PGAVLR = 0.0
      SDNO   = 0.0
      AGRSD3 = AGRSD1
      SHELN  = 0.0
      FLWN   = 0.0

!***********************************************************************
!***********************************************************************
!     EMERGENCE CALCULATIONS - Performed once per season upon emergence
!         or transplanting of plants
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. EMERG) THEN
C-----------------------------------------------------------------------
        ACCAGE  = 0.0
        AFLW    = 0.0
        CNSTRES = 1.0
        FNINSH  = PROSHI * 0.16         
        FLWRDY  = 0.0
        PCTMAT  = 0.0
        PODADD  = 0.0
        SHMINE  = 0.0
        TEMPOD  = 0.0
        TRIGGR  = 0
        WTSHM   = 0.0

        DO NPP = 1, NCOHORTS
        SHELN(NPP) = 0.0
        WTSHE(NPP) = 0.0
        WTSD(NPP) = 0.0
        SDNO(NPP) = 0.0
        FLWN(NPP) = 0.0
        SUPDE(NPP) = 0.0
        AVTEM(NPP) = 0.0
        ENDDO
      
        CALL FOR_PODCOMP(
     &    AGRSD1, FILECC, FNINSD, GDMSD, NAVL, PGAVLR,  !Input
     &    POTCAR, POTLIP,                               !Input/Output
     &    AGRSD3, ANINSD, CUMSIG, RSD,                  !Output
     &    EMERG)                                        !Control

!***********************************************************************
!***********************************************************************
!     DAILY RATE/INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
C     Daily Initialization.
C-----------------------------------------------------------------------
      PODMAT = 0.0
      WSDDTN = 0.0
      WSHDTN = 0.0
      NGRSD  = 0.0
      NGRSH  = 0.0
      NLEFT  = 0.0
      PGLEFT = 0.0

      DAS   = MAX(0,TIMDIF(YRSIM,YRDOY))

!***********************************************************************
!     Seed growth section
!-----------------------------------------------------------------------
      IF (YRDOY .GE. YRNR1 .AND. YRNR1 .GT. 0) THEN
        NR1TIM = MAX(TIMDIF(YRNR1,YRDOY),0)
!-----------------------------------------------------------------------
        PGAVLR = PGAVL * XFRT

C-----------------------------------------------------------------------
C     Nitrogen stress; 8-Day moving average.
C     Slow response to change in N stress.
C-----------------------------------------------------------------------
        CNSTRES = 0.875 * CNSTRES + 0.125 * NSTRES
C-----------------------------------------------------------------------
C     Calculate insect feeding and puncture damage
C-----------------------------------------------------------------------
        REDPUN = 1.0
        IF (PUNCSD .GT. 0.0) THEN
        REDPUN = REDPUN - (PUNCTR/PUNCSD) * RPRPUN
        REDPUN = MAX(0.0,REDPUN)
        ELSE
        REDPUN = 1.0
        ENDIF
        IF (YRDOY .GT. YRNR2 .AND. YRNR2 .GT. 0) THEN
        NR2TIM = MAX(TIMDIF(YRNR2,YRDOY),0)
C-----------------------------------------------------------------------
C     Remember yesterdays mature shell weight, WTSHMY
C-----------------------------------------------------------------------
        WTSHMY = WTSHM
        WTSHM = 0.0
        DO 800 NPP = 1, NR2TIM
        IF (NPP .GT. NCOHORTS) THEN
        WRITE(MESSAGE(1),851)
        WRITE(MESSAGE(2),852)
        WRITE(MESSAGE(3),853)
        CALL WARNING(3,ERRKEY, MESSAGE)
!        WRITE (*,854) MESSAGE(1), MESSAGE(2), MESSAGE(3)
        STOP
 851          FORMAT(' You have reached the maximum number of',
     &    ' cohorts which can be produced.')
 852          FORMAT(' There is probably an error in your inputs.')
 853          FORMAT(' Please end this run and fix the problem.')
 854          FORMAT(/,1X,A78,/,1X,A78,/,1X,A78,/)
        ENDIF
C-----------------------------------------------------------------------
C     Compute physiological age of cohort
C-----------------------------------------------------------------------
        PAGE = PHTIM(NR2TIM + 1) - PHTIM(NPP)
C-----------------------------------------------------------------------
C     Prevent seeds from growing until they are older than LAGSD p-t-d
C-----------------------------------------------------------------------
!           IF (PAGE .LT. LAGSD) GO TO 800
        IF (PAGE .GE. LAGSD) THEN
C-----------------------------------------------------------------------
C     Prevent cohort from exceeding threshing limit, considering
C     damaged seed (SDDES) that were damaged without shell loss ***wdb??
C-----------------------------------------------------------------------
        IF (SDDES(NPP) .GT. 0.0) THEN
        REDSHL = WTSHE(NPP)*SDDES(NPP)/(SDDES(NPP)+SDNO(NPP))
        ELSE
        REDSHL = 0.
        ENDIF
        SDMAX = (WTSHE(NPP)-REDSHL)*THRESH/(100.-THRESH)-WTSD(NPP)
        SDMAX = MAX(0.0,SDMAX)
C-----------------------------------------------------------------------
C     Compute shell wt of cohorts that are full
C-----------------------------------------------------------------------
        IF (SDMAX .LE. 0.0) WTSHM = WTSHM + WTSHE(NPP)
        ENDIF
800     ENDDO
C-----------------------------------------------------------------------
C     Compute cohorts of shell wt. that reach THRESH today
C-----------------------------------------------------------------------
        WTSHMT = WTSHM - WTSHMY

C-----------------------------------------------------------------------
C     Modification of seed composition
!-----------------------------------------------------------------------
!       This section of code provides an alternative to calling PODCOMP
!       routine.  Currently, both are done.
!-----------------------------------------------------------------------
        RSD = 1.0
        IF (GDMSD .GT. 0.0001) THEN
        CRSD = MIN(PGAVLR / (GDMSD*AGRSD1), 1.0)
        NREQ = FNINSD * MIN(PGAVLR/AGRSD1, GDMSD)
        IF (NREQ .GT. 0.0) THEN
        NRSD = NAVL / NREQ
        ELSE
        NRSD = 1.0
        ENDIF
        RSD = MIN(CRSD, NRSD, 1.0)
        ENDIF

        AGRSD3 = AGRSD1
        ANINSD = FNINSD

!-----------------------------------------------------------------------
!     Detailed seed composition calculations
!-----------------------------------------------------------------------
        CALL for_PODCOMP(
     &  AGRSD1, FILECC, FNINSD, GDMSD, NAVL, PGAVLR,  !Input
     &  POTCAR, POTLIP,                               !Input/Output
     &  AGRSD3, ANINSD, CUMSIG, RSD,                  !Output
     &  INTEGR)                                       !Control

C-----------------------------------------------------------------------
C     Grow seed cohorts
C-----------------------------------------------------------------------
        DO 1300 NPP = 1, NR2TIM
        PAGE = PHTIM(NR2TIM + 1) - PHTIM(NPP)
!           IF (PAGE .LT. LAGSD) GO TO 1300
        IF (PAGE .GE. LAGSD) THEN
        IF (SDDES(NPP) .GT. 0.0) THEN
        REDSHL = WTSHE(NPP)*SDDES(NPP)/(SDDES(NPP)+SDNO(NPP))
        ELSE
        REDSHL = 0.
        ENDIF
        SDMAX = (WTSHE(NPP)-REDSHL)*THRESH/(100.-THRESH)-WTSD(NPP)
        SDMAX = MAX(0.0,SDMAX) * (1. + TURADD)
        WTSD(NPP) = WTSD(NPP)+RSD*MIN(SDGR*SDNO(NPP)*REDPUN,SDMAX)
C-----------------------------------------------------------------------
C     New Seed Tissue Growth, for updating crop seed mass
C         in GROW, N Required
C-----------------------------------------------------------------------
        WSDDTN = WSDDTN + RSD * MIN(SDGR*SDNO(NPP)*REDPUN,SDMAX)
        NGRSD = NGRSD+ANINSD*RSD*MIN(SDGR*SDNO(NPP)*REDPUN,SDMAX)
        ENDIF
 1300     ENDDO
C-----------------------------------------------------------------------
        ENDIF           !End of YRDOY>YRNR2 Seed growth section

C***********************************************************************
C     Shell section
C-----------------------------------------------------------------------
        PGLEFT = MAX(0.0,(PGAVLR - WSDDTN*AGRSD3))
        NLEFT  = MAX(0.0,(NAVL - NGRSD))
        PGNPOD = PGLEFT
        NAVPOD = NLEFT
C-----------------------------------------------------------------------
C     Calculate function for modifying pod setting with temperature
C-----------------------------------------------------------------------
        TEMPOD = 0.
        DO I = 1,24
        TEMPOD = TEMPOD +
     &  CURV(TYPPDT,FNPDT(1),FNPDT(2),FNPDT(3),FNPDT(4),TGRO(I))
        ENDDO
        TEMPOD = TEMPOD/24.
C-----------------------------------------------------------------------
C     Avg soil water (SWBAR) over DSWBAR depth to affect flower,
C         pod addition
C-----------------------------------------------------------------------
        IF (ISWWAT .EQ. 'Y') THEN
        ACTSW = 0.0
        POTSW = 0.0
        DSW = 0.0
        DO I = 1,NLAYR
        DSW = DSW + DLAYR(I)
        FLAYR = 1.0
        IF (DSW .GT. DSWBAR) THEN
        FLAYR = (DSWBAR-(DSW-DLAYR(I)))/DLAYR(I)
        ENDIF
        ACTSW = ACTSW + (SW(I) - LL(I)) * DLAYR(I) * FLAYR
        POTSW = POTSW + (DUL(I) - LL(I)) * DLAYR(I) * FLAYR
        IF ( FLAYR .LT. 1.0 ) GO TO 401
        ENDDO
401     SWBAR = ACTSW / POTSW
        SWBAR = MIN (SWBAR,1.0)
        SWBAR = MAX (SWBAR,0.0)
        ELSE
        SWBAR = 1.0
        ENDIF
C-----------------------------------------------------------------------
C     Soil water factor (SWADD1), and Water stress factor (SWADD2)
C-----------------------------------------------------------------------
        SWADD1 = TABEX (YSWBAR,XSWBAR,SWBAR,5)
        SWADD2 = TABEX (YSWFAC,XSWFAC,SWFAC,4)
C-----------------------------------------------------------------------
        SHMAXG = SHVAR
C-----------------------------------------------------------------------
C     This section calculates shell growth after first pod (NR2)
C-----------------------------------------------------------------------
        IF (YRDOY .GT. YRNR2 .AND. YRNR2 .GT. 0) THEN
        DO 2100 NPP = 1, NR2TIM
        NAGE = NR2TIM + 1 - NPP
        PAGE = PHTIM(NR2TIM + 1) - PHTIM(NPP)
        ADDSHL = 0.0
        SUPDAY = 1.0
        IF (PAGE .LE. LNGSH) THEN
        IF (SHELN(NPP) .GE. 0.001 .AND. GRRAT1 .GE. 0.001) THEN
        IF (PAGE .GE. LNGPEG) THEN
        ADDSHL = MIN(PGLEFT/AGRSH1,GRRAT1 * SHELN(NPP),
     &    NLEFT/(FNINSH*CNSTRES**0.5))
        SUPDAY = MIN((PGLEFT/AGRSH1)/(GRRAT1*SHELN(NPP)),
     &    (NLEFT/(FNINSH*CNSTRES**0.5))/(GRRAT1 * SHELN(NPP)),
     &    SWADD1)
        IF (SUPDAY .GE. 1.0) SUPDAY = 1.0
        ELSE
        IF (SHLAG .LT. 0.001) SHLAG = 0.001
        ADDSHL = MIN(PGLEFT/AGRSH1 ,GRRAT1*SHELN(NPP)*SHLAG,
     &    NLEFT/(FNINSH*CNSTRES**0.5))
        SUPDAY = MIN(
     &    (PGLEFT/AGRSH1)/(GRRAT1*SHELN(NPP)*SHLAG),
     &    (NLEFT/(FNINSH*CNSTRES**0.5))/
     &    (GRRAT1*SHELN(NPP)*SHLAG), SWADD1)
        IF (SUPDAY .GE. 1.0) SUPDAY = 1.0
        ENDIF
C-----------------------------------------------------------------------
C     Compute running avg ratio supply to demand for shell grwoth
C-----------------------------------------------------------------------
        ENDIF
        IF (NAGE .LE. 1) THEN
        SUPDE(NPP) = SUPDAY
        AVTEM(NPP) = TEMPOD
        ELSE
        SUPDE(NPP) = (SUPDE(NPP) * (NAGE-1) + SUPDAY)/NAGE
        AVTEM(NPP) = (AVTEM(NPP) * (NAGE-1) + TEMPOD)/NAGE
        ENDIF
C-----------------------------------------------------------------------
C     Compute overall growth of all shells, total N required
C     and the remaining C (PGLEFT) and N (LEFT)
C-----------------------------------------------------------------------
        WSHDTN = WSHDTN + ADDSHL
        NGRSH = NGRSH + ADDSHL * PROSHI * 0.16 * CNSTRES**0.5
        IF (PGLEFT .LT. 1.0E-6) PGLEFT=0.0          !NBP
        IF (ADDSHL .LT. 1.0E-6) ADDSHL=0.0          !NBP
        PGLEFT = MAX(0.0,(PGLEFT - ADDSHL * AGRSH1))
        NLEFT  = MAX(0.0,(NLEFT - ADDSHL * (FNINSH*CNSTRES**0.5)))
        ENDIF
C-----------------------------------------------------------------------
C     Grow shells if greater than 1 day old
C-----------------------------------------------------------------------
        SHMINE = 0.0
        IF (SDDES(NPP) .GT. 0.0) THEN
        REDSHL = WTSHE(NPP)*SDDES(NPP)/(SDDES(NPP)+SDNO(NPP))
        ELSE
        REDSHL = 0.
        ENDIF
        SDMAXX = (WTSHE(NPP)-REDSHL) * THRESH/(100. - THRESH)
        IF (SHELWT-WTSHM .GT. 0.0 .AND. SDMAXX .GE. WTSD(NPP)) THEN
        SHMINE = NRUSSH/0.16 * WTSHE(NPP)/(SHELWT - WTSHM)
        ENDIF
        WTSHE(NPP) = WTSHE(NPP) + ADDSHL - MAX(SHMINE,0.0)
 2100     ENDDO
C-----------------------------------------------------------------------
C     Set seeds based on ratio of supply to demand for shells,
C     average temperature and night length effect
C     between (LAGSD) and (LAGSD+TDUMX) p-t-d age
C-----------------------------------------------------------------------
        WTABRT = 0.0
        DO 2200 NPP = 1, NR2TIM
        PAGE = PHTIM(NR2TIM + 1) - PHTIM(NPP)
        IF (PAGE .GE. LAGSD .AND. PAGE .LT. LAGSD + TDUMX
     &    .AND. SDNO(NPP) .LE. 0.0) THEN
C-----------------------------------------------------------------------
C     Physiol age to set seeds
C-----------------------------------------------------------------------
        IF (SUPDE(NPP) .GE. SETMAX) THEN
        SHRAT = 1.0
        ELSE
        SHRAT = SUPDE(NPP)/SETMAX
        ENDIF
        SDNO(NPP) = MIN(SHRAT, AVTEM(NPP)*(DRPP**1.0)) *
     &    SHELN(NPP)* SDPDVR + SDNO(NPP)
C-----------------------------------------------------------------------
C     Abort shells that do not form seed; abort (1-SHRAT) fraction
C-----------------------------------------------------------------------
        WTABR = 0.0
        START = SHELN(NPP)
        SHELN(NPP) = SHELN(NPP)*MIN(SHRAT, AVTEM(NPP)*(DRPP**1.0))
        IF (START .GT. 0.) THEN
        WTABR = (START-SHELN(NPP))*WTSHE(NPP)/START
        ENDIF
        WTSHE(NPP) = WTSHE(NPP) - WTABR
        WTABRT = WTABRT + WTABR
        ENDIF
 2200     ENDDO
C-----------------------------------------------------------------------
        ENDIF         !End of DAS>NR2 Shell growth section
C***********************************************************************
C     Add new pods and flowers
C     RFLWAB is relative rate of flower abortion per day because
C     daylength is not optimum.  The flowers that survive "NR2"
C     PHOTOTHERMAL days is equal to FLWRDY which can limit pod addition
C-----------------------------------------------------------------------
        AFLW = RFLWAB * (1.0 - TDUMX) * (1.0 - SWADD2)
        FLWRDY = 0.
        DO 2500 NPP = 1, NR1TIM
        IF (FLWN(NPP) .GT. 0.0001) THEN
        PNAGE = PNTIM(NR1TIM + 1) - PNTIM(NPP)
        FLWN(NPP) = FLWN(NPP) * (1.0 - AFLW)
        IF (PNAGE .GE. PHTHRS(6)) THEN
C-----------------------------------------------------------------------
C     Allow flowers in each cohort to make pods over 2-3 days
C-----------------------------------------------------------------------
        FLWFRC = 0.
        IF (TDUMX .GT. 0.0001) FLWFRC = (PNAGE-PHTHRS(6))/TDUMX
        FLWFRC = MIN(FLWFRC,1.0)
        FLWFRC = MAX(FLWFRC,0.0)
        FLWRDY = FLWFRC*FLWN(NPP) + FLWRDY
        FLWN(NPP) = (1.0-FLWFRC)*FLWN(NPP)
        ENDIF
        ENDIF
 2500   ENDDO
 
        PMAX = PGAVLR/(SDVAR*AGRSD1*SDPDVR)*(1./PODUR)

        IF (YRDOY .GE. YRNR2 .AND. YRNR2 .GT. 0) THEN
        FLADD = FLWRDY * TEMPOD *(DRPP**1.3)* MIN(SWADD1,SWADD2) *XFRT
        IF (DAS .GT. NDSET .AND. MNESPM .GT. 0.) THEN
        ACCAGE = ACCAGE + TEMPOD * DRPP * SWFAC / MNESPM
        ACCAGE = MIN(1.0,ACCAGE)
        ENDIF
C-----------------------------------------------------------------------
C    Reduce pod addition from END POD SET to physiological maturity
C    DRPP**1.3 makes smaller and more sentitive to long days
C    Scale pod addition to RNITP, leaf N for photo purporses
C-----------------------------------------------------------------------
        RNITPD = (RNITP*0.01-PROLFF*0.16)/(FNINL-PROLFF*0.16)
        RNITPD = MIN(1.1,RNITPD)
        RNITPD = MAX(0.1,RNITPD)
        PODADD = PMAX * TEMPOD * (DRPP**1.3) *
     &   MIN(SWADD1,SWADD2,RNITPD) * MAX((1.0 - ACCAGE),0.0)
C    &   * MAX((1.0 - ACCAGE),0.0) * (1.0 + TURADD)
        SHELN(NR2TIM + 1) = MIN(PODADD, PGNPOD/(SHMAXG*AGRSH1),
     &  FLADD, NAVPOD/(SHMAXG*(FNINSH*CNSTRES**0.5)))
C-----------------------------------------------------------------------
C    KJB ADDED 1/27/96.  2 CONDITIONS: NDSET AND TRIGGER (CUMSIG >.98)
C    MUST BE MET TO STOP POD ADDITION.  THUS, IF WE ARE THRU THE WINDOW
C    AND FULL LOAD IS SET, THEN NO LATE PODS AND FLOWERS CAN BE ADDED
C    PRESENTLY NDSET WAS PLACED TO R7 IN SOYBEAN.  MOVE IT EARLIER
C    SO WE CAN PREVENT FUNNY LATE BUMPS (WMS-88, 84RF) OCCURRING AFTER
C    FULL LOAD IS APPARENTLY SET, BUT DROUGHT IS RELEASED.
C-----------------------------------------------------------------------
        IF (TRIGGR .EQ. 0 .AND. CUMSIG .LT. 0.98) THEN
        TRIGGR = 1
        ENDIF

        IF (DAS .GE. NDSET .AND. TRIGGR .EQ. 1) THEN
        SHELN(NR2TIM + 1) = 0.0
        ENDIF
C-----------------------------------------------------------------------
        ENDIF         !End of DAS>NR2 Pod and flower growth section
C-----------------------------------------------------------------------
        FLWADD = 2. * PMAX * TEMPOD * (DRPP**1.3) *
     &     MIN(SWADD1,SWADD2,CNSTRES**0.5)
C    &     MIN(SWADD1,SWADD2,CNSTRES**0.5) * (1.0 + TURADD)
        FLWN(NR1TIM + 1) = MIN(FLWADD,PGNPOD/(SHMAXG*0.1*AGRSH1),
     &     NAVPOD/(SHMAXG*0.1*(FNINSH*CNSTRES**0.5)))
        IF (DAS .GE. NDSET .AND. TRIGGR .EQ. 1) THEN
        FLWN(NR1TIM + 1) = 0.
        ENDIF
C-----------------------------------------------------------------------
C     Calculate number of pods, including those with and without seeds
C-----------------------------------------------------------------------
        SEEDNO = 0.0
        PODNO = 0.0

C-----------------------------------------------------------------------
        IF (YRDOY .GE. YRNR2 .AND. YRNR2 .GT. 0) THEN
        DO 2900 NPP = 1, NR2TIM + 1
C-----------------------------------------------------------------------
        PAGE = PHTIM(NR2TIM + 1) - PHTIM(NPP)
C-----------------------------------------------------------------------
        IF (PAGE .GT. LNGPEG) THEN
        PODNO = PODNO + SHELN(NPP)
        SEEDNO = SEEDNO + SDNO(NPP)
        IF (PAGE .GE. LAGSD) THEN
        IF (SDDES(NPP) .GT. 0.0) THEN
        REDSHL = WTSHE(NPP) * SDDES(NPP) / 
     &    (SDDES(NPP) + SDNO(NPP))
        ELSE
        REDSHL = 0.
        ENDIF
        SDMAXX = (WTSHE(NPP)-REDSHL) * THRESH/(100. - THRESH)
        IF ((WTSD(NPP) .GE. 0.95 * SDMAXX) .AND.
     &    (WTSHE(NPP) .GT. 0.001) .OR.
     &    (PAGE .GT. XMPAGE)) THEN
        PODMAT = PODMAT + SHELN(NPP)
        ENDIF
        ENDIF
        ENDIF
 2900     ENDDO
        ENDIF

C-----------------------------------------------------------------------
        IF (PODMAT .GT. 0. .AND. PODNO .GT. 0.) THEN
        PCTMAT = PODMAT*100./PODNO
        ENDIF

C-----------------------------------------------------------------------
      ENDIF             !End of section for YRDOY>YRNR1
C-----------------------------------------------------------------------
C    Leave PODS with : NGRSD  : New N Used for New Seed Growth
C                      NGRSH  : New N Used for New Shell Growth
C                      WSDDTN : New Seed Growth, g tissue/m2 d
C                      WSHDTN : New Shell Growth, g tissue/m2 d
C                      New pods, seeds, flowers, mature pods, % mature pods
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END !SUBROUTINE FOR_PODS
!=======================================================================

C=======================================================================
C  FOR_PODCOMP, Subroutine, K. J. Boote, E. Piper
C-----------------------------------------------------------------------
C  Computes modification of seed composition
C-----------------------------------------------------------------------
C  Ratio of supply to demand for seed growth (RSD). Carbohydrates
C  still a limiting factor, N is limiting to seed growth only if
C  protein falls below minimum N (PROMIN), mobilized protein can be
C  used for seed growth if C supply is less than demand, CRSD<1.0,
C  upto a maximum seed protein.
C
C  The AGRSD1 is set by INCOMP, and stays constant and is used with
C  "questions", DEMAND, and setting pod no, etc.  AGRSD3 is used
C  when  composition changes below, and used to compute PGLEFT and
C  one time later in PLANT for the day's reprod. growth.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/22/98 GH  Restructured PODS routine
C  01/22/98 GH  Reorganize and restructure code
C  07/13/98 GH  Modified for modular structure
C  05/11/99 GH  Incorporated in CROPGRO routine
C-----------------------------------------------------------------------
!  Called from:  PODS
!  Calls:        ERROR, FIND, IGNORE
C=======================================================================

      SUBROUTINE FOR_PODCOMP(
     &  AGRSD1, FILECC, FNINSD, GDMSD, NAVL, PGAVLR,    !Input
     &  POTCAR, POTLIP,                                 !Input/Output
     &  AGRSD3, ANINSD, CUMSIG, RSD,                    !Output
     &  DYNAMIC)                                        !Control

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE

      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = 'PODCOM')

      CHARACTER*6 SECTION
      CHARACTER*80 C80
      CHARACTER*92 FILECC

      INTEGER LUNCRP, ERR, LNUM, FOUND, ISECT, I
      INTEGER DYNAMIC

      REAL AGRSD1, AGRSD3, ANINSD, CRSD, CRSD2, CUMSIG, DMSDC 
      REAL DMSDN, DTCAR, DTLIP, FNINSD, GDMSD, NAVL, NREQ, NRSD 
      REAL PGAVLR, PLIGSD, PMINSD, PNINSD, POASD, POTCAR, POTLIP
      REAL PROMAX, PROMIN, RATIOC, RATION, RCH2O, RLIG, RLIP
      REAL RMIN, ROA, RSD, THETA, TOTAL, XRSD

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!***********************************************************************
!     Read in values from input file, which were previously input
!       in Subroutine IPCROP.
!-----------------------------------------------------------------------
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)
!-----------------------------------------------------------------------
!    Find and Read Respiration Section
!-----------------------------------------------------------------------
!     Subroutine FIND finds appropriate SECTION in a file by
!     searching for the specified 6-character string at beginning
!     of each line.
!-----------------------------------------------------------------------
      LNUM = 1
      SECTION = '!*RESP'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        DO I = 1, 3
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        IF (ISECT .EQ. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDDO
        READ(C80,'(5F6.0)',IOSTAT=ERR) RCH2O, RLIP, RLIG, ROA, RMIN
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

!-----------------------------------------------------------------------
!    Find and Read Plant Composition Section
!-----------------------------------------------------------------------
      SECTION = '!*PLAN'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        DO I = 1, 3
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        IF (ISECT .EQ. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDDO
        READ(C80,'(18X,3F6.0)',IOSTAT=ERR) PROMIN, PROMAX, THETA
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        DO I = 1, 3
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        IF (ISECT .EQ. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDDO
        READ(C80,'(24X,F6.0)',IOSTAT=ERR) PLIGSD
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(24X,F6.0)',IOSTAT=ERR) POASD
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(24X,F6.0)',IOSTAT=ERR) PMINSD
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

      CLOSE (LUNCRP)

!***********************************************************************
!***********************************************************************
!     EMERGENCE CALCULATIONS - Performed once per season upon emergence
!         or transplanting of plants
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. EMERG) THEN
!-----------------------------------------------------------------------
C     Initialize plant variables at emergence
C-----------------------------------------------------------------------
        CUMSIG = 1.0      
        RATION = 1.0      
        RATIOC = 1.0      

!***********************************************************************
!***********************************************************************
!     DAILY INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
C     Daily initialize, each should change before seed cohorts section
C-----------------------------------------------------------------------
      PNINSD = FNINSD
      ANINSD = FNINSD
      DTLIP = 0.0
      DTCAR = 0.0
      AGRSD3 = AGRSD1
      RSD = 1.0
      CRSD = 1.0
      NRSD = 1.0
      XRSD = 1.0

      IF (PGAVLR .LE. 0.00001) THEN
        RSD = 0.0
      ELSEIF (GDMSD .LE. 0.0001) THEN
        RSD = 1.0
      ELSE
        CRSD2 = PGAVLR/(GDMSD*AGRSD1)
        CRSD  = MIN(CRSD2,1.0)
        NREQ = FNINSD*(MIN(PGAVLR/AGRSD1,GDMSD))
        NRSD = NAVL/NREQ
C-----------------------------------------------------------------------
C     Full seed load defined for either of all N or all C
C     being used for seed growth.
C-----------------------------------------------------------------------
        CUMSIG = 0.8 * CUMSIG + 0.2 * CRSD
C-----------------------------------------------------------------------
C     5-day moving average.  Does it work ?
C
C     Computing the possible seed N conc given the "total" NAVL
C     relative to PG available or seed growth demand
C-----------------------------------------------------------------------
        PNINSD = NAVL/(MIN(PGAVLR/AGRSD1,GDMSD))
C-----------------------------------------------------------------------
C     Set ANINSD equal to FNINSD to allow nitrogen to go to vegetative
C     parts when carbon is not limiting.  Note:  CRSD and NRSD are above
C     1 and upto 30 during the time seed setting occurs.  This is why we
C     can not let ANINSD = PNINSD or let RSD = CRSD during this phase.

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
!     CASE 4: NRSD <= 1.0 - N supply limiting seed growth
C-----------------------------------------------------------------------
        IF (NRSD .LE. 1.0) THEN
C-----------------------------------------------------------------------
C   NOTE THAT WHEN NRSD .LT. 1.0, THEN PNINSD .LT. FNINSD.  IN THIS CASE,
C   THE N SUPPLY IS INSUFFICIENT TO GROW SEED AT FNINSD.  N IS LIMITED
C   RELATIVE TO C SUPPLY AND RELATIVE TO SEED DEMAND.  ALLOW SEED N CONC
C   TO DECLINE AND RECOMPUTE RSD & AGRSD3.  THERE IS NO CASE OF EXCESS C
C   SUPPLY HERE BECAUSE N STRESS BY THE DEFINED CURVE LIMITS SINGLE SEED
C   GROWTH RATE.  N STRESS INCREASINGLY LIMITS SEED GROWTH RATE AS PNINSD
C   DECLINES.  AS A RESULT OF "LIMITING" SEED GROWTH RATE, THE SEED N CONC
C   IS  HELD UP A BIT MORE BECAUSE LESS C IS USED.  SO ANINSD WILL BE
C   A BIT HIGHER THAN PNINSD HERE.  AGRSD3 AND RSD SHOULD BE RECOMPUTED.
C
C   MORE NOTES:  NOTE THAT NRSD AND PNINSD ALREADY CONSIDER CARBON SUPPLY
C   (PGAVLR) IN THEIR EQUATIONS.  THUS, IF C SUPPLY IS HIGH, THEN PNINSD
C   WILL BE LOW AND IT WILL BE CONSIDERED HERE.  IF C SUPPLY IS LOW, THE
C   PNINSD WILL BE HIGH (ABOVE FNINSD) AND NRSD > 1, AND IT WILL BE
C   CONSIDERED IN THE NEXT LOOP.
C
C        COMPUTE XRSD WITH RECT HYP EQ, WITH INIT SLOPE = 1/PROMIN
C        MAX = 1.0 AT FNINSD, AND THE Y IS 0 TO 1.0 AND X IS
C        INCOMING PNINSD FROM ZERO TO FNINSD.
C        THIS "RSD" DESCRIBES N LIMIT ON SEED GROWTH RATE
C        THEN COMPUTE "NEW" "ANINSD" BASED ON "N-LIMITED' SEED
C        GROWTH RATE.  USE THAT AS THE ANINSD, THEN CHECKS FOR
C        CARBOHYDRATE REQUIRED BASED ON NAVL AND PGAVLR
C
C-----------------------------------------------------------------------
!          SCALAR = (FNINSD/PROMIN + 1.0 - ((FNINSD/PROMIN + 1.0)**2
!     &      - 4*THETA*FNINSD/PROMIN*1.0)**0.5)/(2*THETA)
!          XRSD = ((PNINSD/PROMIN + 1.0 - ((PNINSD/PROMIN + 1.0)**2
!     &      - 4*THETA*PNINSD/PROMIN*1.0)**0.5)/(2*THETA))/SCALAR

        XRSD = ((PNINSD/PROMIN + 1.0 - ((PNINSD/PROMIN + 1.0)**2
     &    - 4*THETA*PNINSD/PROMIN*1.0)**0.5)/(2*THETA)) /
     &    ((FNINSD/PROMIN + 1.0 - ((FNINSD/PROMIN + 1.0)**2
     &    - 4*THETA*FNINSD/PROMIN*1.0)**0.5)/(2*THETA))

        ANINSD = NAVL/(XRSD*MIN(PGAVLR/AGRSD1,GDMSD))
C-----------------------------------------------------------------------
C  CHECK BECAUSE N CONC CAN GO ABOVE FNINSD IF PNINSD NEAR FNINSD
C-----------------------------------------------------------------------
        ANINSD = MIN(ANINSD,FNINSD)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
!     CASE 3: CRSD2 <= 1.0 - C supply is limiting seed growth.
C-----------------------------------------------------------------------
        ELSE
C      HERE NRSD .GT. 1.0 AND PNINSD .GT. FININSD, SO ALLOW INCREASING
C      SEED N CONC.  CONSIDER TWO OPTIONS, C DEFICIENT OR THAT C
C      SUPPLY IS EXCESS.  A CRSD <= 1.0 IS THE SAME AS CRSD2 <= 1.0
C      FIRST OPTION IS IF C SUPPLY IS DEFICIENT, BUT N SUPPLY/DEMAND
C      RATIO GREATER THAN 1.
        IF (CRSD2 .LE. 1.0) THEN
C-----------------------------------------------------------------------
C      DOES POSSIBLE N CONCENTRATION EXCEED MAX?
C-----------------------------------------------------------------------
!           IF (PNINSD.GT.PROMAX) THEN
!             ANINSD = PROMAX
!           ELSE
C-----------------------------------------------------------------------
C     UNDER THIS CONDITION, CRSD < 1.0 AND NRSD > 1.0 AND PNINSD <PROMAX
C     SO HERE WE LET SEED N CONC INCREASE, AND BE EQUAL TO PNINSD.
C-----------------------------------------------------------------------
!               ANINSD = PNINSD
!            ENDIF
        ANINSD = MIN(PNINSD, PROMAX)

C-----------------------------------------------------------------------
C      IT IS NOT POSSIBLE FOR PNINSD < FNINSD AND YET NRSD > 1.0
C      IT IS NOT POSSIBLE FOR PNINSD > FNINSD AND YET NRSD < 1.0 EITHER
C      SO NRSD CAN ONLY BE < 1.0 IF NAVL < NREQ, AND THAT IS ONLY
C      IF PNINSD < FNINSD.  THIS WAS COVERED BEFORE

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
!     CASE 2: Full seed load and neither N or C supply is limiting 
!         seed growth
C-----------------------------------------------------------------------
        ELSE IF (CRSD2 .GT. 1.0 .AND. CUMSIG .LT. 0.98) THEN
C-----------------------------------------------------------------------
C      UNDER THIS CONDITION, N IS EXCESS RELATIVE TO C SUPPLY OR SD
C      DEMAND (NRSD > 1) AND C SUPPLY EXCEEDS SEED DEMAND (CRSD2 > 1).
C      ALSO, RESTRICTED THIS ONE TO OCCUR ONLY AFTER A FULL SEED LOAD
C      OCCURS.  THE "CUMSIG" MUST BE WORKED TO DROPPED BELOW 1.0 ONLY
C      WHEN SEVERAL DAYS OF "FULL SEED" LOAD OCCURS.  RESULTS FROM
C      SEVERAL DAYS OF CRSD OR CRSD2 < 1, WE NEED THIS TO AVOID
C      PROBLEMS DURING EARLY PODSET WHEN CRSD IS VERY LARGE AND NRSD
C      ALSO LARGE.
C-----------------------------------------------------------------------
C      0.20 FRACTION OF EXCESS ASSIMILATE IS ALLOWED TO "PUSH" SINGLE
C      SEED GROWTH RATE (I.E., VIA INCREASED RSD).  THIS WILL DECREASE
C      AMOUNT OF STEM AND LEAF GROWTH DURING SEED FILL.
C      LIMIT THE ACTUAL N CONC TO PROMAX OR THE NAVL/(GDMSD*RSD).  THIS
C      ONE WILL NEED TO BE CHECKED CAREFULLY TO PREVENT REALLY HIGH OR
C      LOW N.  WE INTEND FOR IT TO ALLOW N CONC LOWER THAN FNINSD, BUT
C      WILL DO AN XRSD LIMIT ON SEED GROWTH RATE IF ANINSD .LT. FNINSD
C
C      BECAUSE WE USE ONLY 20% OF EXCESS ASSIMILATE, I DON'T THINK WE
C      NEED TO RE-COMPUTE RSD AFTER COMPUTING COMPOSITIONS AND COSTS.
C
C      GIVE UP ON "BOOSTING" SEED GROWTH RATE, TRY TO INFLUENCE
C      COMPOSITION AS A RATIO OF N SUPPLY TO C SUPPLY, NORMALIZED
C      TO FNINSD.  RECIPROCAL OF RATION IS RATIOC.  ASSUME SLOPE OF
C      ONE-THIRD OF POSSIBLE CHANGE FROM FNINSD TO PROMAX OR PROMIN.
C       9/25/95 KJB
C-----------------------------------------------------------------------
        ANINSD = FNINSD
        IF (NAVL .GT. 0.0) THEN
        RATION = (NAVL/(PGAVLR/AGRSD1))/FNINSD
        RATIOC = 1.0 / RATION
        IF (RATION .GE. 1.0) THEN
        ANINSD = MIN(FNINSD * (1.0 + (RATION - 1.0)/3.), PROMAX)
        ELSE
        ANINSD = MAX(FNINSD * (1.0-(RATIOC-1.0)/3.), PROMIN)
        ENDIF
        ENDIF

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
!     CASE 1: Do not have full seed load and carbon supply is not 
!         limiiting sedd growth.  Do not change composition.
C-----------------------------------------------------------------------

        ELSEIF (CRSD2 .GT. 1.0 .AND. CUMSIG .GE. 0.98) THEN
C-----------------------------------------------------------------------
C  EVEN IF CRSD2 > 1 AND NRSD > 1, WE HAVE NOT REACHED A SEED LOAD
C  SO HOLD CONCENTRATIONS AND COSTS UNCHANGED.
!-----------------------------------------------------------------------
        ANINSD = FNINSD

        ENDIF
        ENDIF

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
!  For cases 2, 3 and 4, adjust lipids and carbohydrate concentrations
C-----------------------------------------------------------------------
        IF (NRSD .LE. 1.0 .OR. CRSD2 .LE. 1.0 .OR. 
     &    CUMSIG .LT. 0.98) THEN
C-----------------------------------------------------------------------
C     ADJUSTING SO LIPID AND CARBOHYDRATE TAKE UP DIFFERENCE
C     MOST DATA SUGGEST THAT LIPID CHANGES 0.33 PER 1 PERCENT CHANGE
C     IN PROTEIN.
C-----------------------------------------------------------------------
        DTLIP = 0.33*(FNINSD - ANINSD)*6.25
        DTCAR = (FNINSD - ANINSD)*6.25  - DTLIP
        POTLIP = POTLIP + DTLIP
        POTCAR = POTCAR + DTCAR
!     POTPRO = ANINSD*6.25
        TOTAL  = POTLIP + ANINSD*6.25 + POTCAR + PMINSD + 
     &    POASD + PLIGSD
!             TOTAL not used - chp
        AGRSD3 = PMINSD*RMIN + PLIGSD*RLIG + POASD*ROA +
     &    POTLIP*RLIP + POTCAR*RCH2O
C-----------------------------------------------------------------------
C     THIS IS CORRECT, ABOVE BASED ON N LIMIT, NEXT ON C LIMIT
C     CONSIDERING ANY SHIFT IN PROTEIN CONC.
C-----------------------------------------------------------------------
        DMSDN = NAVL / ANINSD
        DMSDC = PGAVLR / AGRSD3
        RSD = MIN(MIN(DMSDN,DMSDC)/GDMSD,1.0)
        RSD = MAX(0.0,RSD)
        ENDIF
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END !SUBROUTINE FOR_PODCOMP
C=======================================================================

!***********************************************************************
!       Variable Definitions for PODS and FOR_PODCOMP
!***********************************************************************
! ACCAGE    Fraction of physiological time from pod set to physiological 
!             maturity which has occurred 
! ACTSW     Extractable soil water content in the fruiting zone (cm3/cm3)
! ADDSHL    Today's growth demand for shells of age NPP (g[shell] / m2 / d)
! AFLW      Relative rate of flower abortion per day 
! AGRSD1    CH2O requirement for seed growth, excluding cost for protein 
!             content (g[CH2O] / g[seed])
! AGRSD3    CH2O requirement for seed growth, with reduced N content
!             (g[CH2O] / g[seed])
! AGRSH1    CH2O required for shell growth, excluding cost for protein 
!             content (g[CH2O] / g[shell])
! ANINSD    Actual fraction of N for growing seed tissue (g[N] / g[seed])
! AVTEM(J)  Running average value of TEMPOD (factor for modifying pod 
!             setting based on temperature) 
! CNSTRES   Nitrogen stress factor - 8 day moving average 
! CROP      Crop identification code 
! CRSD      Ratio of carbon supply to demand 
! CRSD2     Ratio of carbon supply to demand, not limited to maximum of 1.0
!             
! CUMSIG    5 day running average of CRSD used to determine a full seed 
!             load 
! CURV      Function subroutine 
! DAS       Days after start of simulation (days)
! DLAYR(L)  Soil Depth in layer L (cm)
! DMSDC     Carbon limited seed growth demand (g[seed] / m2 / d)
! DMSDN     Nitrogen limited seed growth demand (g[seed] / m2 / d)
! DRPP      Photoperiod days which occur in a real day
!             (photoperiod days / day)
! DSW       Accumulated soil depth (cm)
! DSWBAR    Average depth of soil over which soil water content affects 
!             flower, pod addition (cm)
! DTCAR     Additional potential carbohydrate composition (fraction)
! DTLIP     Additional potential lipid composition (fraction)
! DUL(L)    Volumetric soil water content at Drained Upper Limit in soil 
!             layer L (cm3 [H2O] /cm3 [soil])
! DYNAMIC   Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!             INTEGR, OUTPUT, or SEASEND 
! ECONO     Ecotype code - used to match ECOTYP in .ECO file 
! ECOTYP    Ecotype code for this simulation 
! ERRKEY    Subroutine name for error file 
! FILECC    Path plus filename for species file (*.spe) 
! FILEGC    Pathname plus filename for ECO file 
! FILEIO    Filename for INP file (e.g., IBSNAT35.INP) 
! FLADD     Number of flowers that are developed and available to form pods
!             (#)
! FLAYR     Fraction of layer within nodule zone 
! FLWADD    Actual addition of flowers today (flower /da)
! FLWFRC    Fraction of flowers available to be converted to pods on given 
!             day (fraction)
! FLWN(J)   Number of flowers added for cohort J (# / m2)
! FLWRDY    Flowers that survive NR2 photothermal days which can limit pod 
!             addition (# / m2)
! FNINL     Maximum fraction of N for growing leaf tissue (g[N] / g[leaf])
! FNINSD    Maximum fraction of N for growing seed tissue based on 
!             temperature (g[N] / g[seed])
! FNINSH    Maximum fraction of N for growing shell tissue
!             (g[N] / g[shell])
! FNPDT(I)  Critical values of temperature for function to reduce pod 
!             addition and seed setting rates under non-optimal temperatures
!             (°C)
! GDMSD     Seed growth demand based on temperature and photoperiod
!             (g[seed] / m2 / d)
! GRRAT1    Maximum growth per individual shell (g / shell / d)
! ISWWAT    Water simulation control switch (Y or N) 
! LAGSD     Time required between shell growth and seed growth, per cohort
!             (Photo-thermal days)
! LL(L)     Volumetric soil water content in soil layer L at lower limit
!             ( cm3/cm3)
! LNGPEG    Time between start of peg and rapid shell formation (for 
!             peanuts only).  Defines slow growth period. (Photo-thermal days)
! LNGSH     Time required for shell growth (Photo-thermal days)
! LUNCRP    Logical unit number for FILEC (*.spe file) 
! LUNECO    Logical unit number for FILEE (*.eco file) 
! LUNIO     Logical unit number for FILEIO 
! MNESPM    Minimum time from end of pod set to physiological maturity.
!             (thermal days)
! NAGE      Age of cohort (days)
! NAVL      Total mass of nitrogen available for growth (g[N] / m2 / d)
! NAVPOD    N available for pod growth (g[N] / m2 / d)
! NDSET     Day when last pod can form (days)
! NGRSD     Rate of N accumulation in new seeds (g[N] / m2 / d)
! NGRSH     Rate of N accumulation in new shells (g[N] / m2 / d)
! NL        Maximum number of soil layers = 20 
! NLAYR     Number of soil layers 
! NLEFT     Nitrogen left after vegetative demands are met (g[N] / m2 / d)
! NPP       Cohort number used as index in loops 
! NR1TIM    Days since NR1 (first flower) (d)
! NR2TIM    Days since NR2 (first peg) (d)
! NREQ      N demand for today's seed growth (g[N] / m2)
! NRSD      N ratio of supply to demand for seed growth (fraction)
! NRUSSH    N actually mobilized from shells in a day (g[N]/m2-d)
! NSTRES    Nitrogen stress factor (1=no stress, 0=max stress) 
! PAGE      Photothermal age of each cohort (Photo-thermal days)
! PCTMAT    Fraction of pods that are mature (seed are 90% of final size) 
! PGAVL     Total available CH2O available for growth & respiration
!             (g[CH2O] / m2)
! PGAVLR    CH2O available for reproductive growth (g[CH2O] / m2)
! PGLEFT    Excess PG after today's tissue growth (g[CH2O] / m2)
! PGNPOD    Carbohydrate availability for pod growth, after seed growth
!             (mg[CH2O] / m2 / s)
! PHTHRS(I) Threshold time that must accumulate in phase I for the next 
!             stage to occur  (thermal or photothermal days)
! PHTIM(I)  Cumulative photothermal time ages of seeds and shells 
! PLIGSD    Proportion of seed tissue that is lignin (fraction)
! PMAX      Maximum number of pods set today based on available 
!             carbohydrate (pods / m2 / d)
! PMINSD    Proportion of seed tissue that is mineral (fraction)
! PNAGE     Photothermal time elapsed since flower formation (p-t-d)
! PNINSD    Potential fraction of N for growing seed tissue based on C and 
!             N supply (g[N] / g[seed])
! PNTIM(I)  Photothermal days from first flower when flowers in age group I 
!             formed (p-t-d)
! POASD     Proportion of seed tissue that is organic acid (fraction)
! PODADD    Number of pods added today (pods / m2 / d)
! PODMAT    Number of mature pods (#/m2)
! PODNO     Total number of pods (#/m2)
! PODUR     Time required for crop to add full pod load under optimal 
!             conditions, used to compute rate of pod and flower addition
!             (p-t-d)
! POTCAR    Potential carbohydrate composition of seed based on temperature
!             (fraction)
! POTLIP    Potential lipid composition of seed based on temperature
!             (fraction)
! POTSW     Average potential extractable soil water in fruiting depth of 
!             soil (cm3/cm3)
! PROLFF    Minimum leaf protein composition after N mining
!             ( g[protein] / g[leaf])
! PROMAX    Maximum N concentration of seed (g[N] / g[seed])
! PROMIN    Minimum N concentration of seed (g[N] / g[seed])
! PROSHF    Minimum shell protein composition after N mining
!             (g[protein] / g[shell])
! PROSHI    Maximum protein composition in shells during growth with 
!             luxurious supply of N ( g[protein] / g[shell tissue])
! PUNCSD    Cumulative puncture damage to seed (not yet implemented) 
! PUNCTR    Cumulative puncture damage (not yet implemented) 
! RATIOC    Ratio of C supply to N supply 
! RATION    Ratio of N supply to C supply 
! RCH2O     Respiration required for synthesizing CH2O structure
!             (g[CH2O] / g[tissue])
! REDPUN    Reduces growth of seed in an age group due to pest-caused 
!             punctures in seed (0 to 1) (not yet implemented) 
! REDSHL    Reduces growth of shell in an age group due to pest-caused 
!             punctures in seed (0 to 1) 
! RFLWAB    Relative rate of flower abortion per day because daylength not 
!             optimum 
! RLIG      Respiration required for synthesizing lignin structure
!             (g[CH2O] / g[lignin])
! RLIP      Respiration required for synthesizing lipid structure
!             (g[CH2O] / g[lipid])
! RMIN      Respiration required for synthesizing mineral structure
!             (g[CH2O] / g[mineral])
! RNITP     True nitrogen concentration in leaf tissue for photosynthesis 
!             reduction. (%)
! RNITPD    Fraction of N in pods, scaled from leaf nitrogen (fraction)
! ROA       Respiration required for synthesizing organic acids
!             (g[CH2O] / g[product])
! RPRPUN    Puncture damage reduction variable (not yet implemented) (0-1) 
! RSD       Ratio of supply to demand for seed growth 
! SAT(L)    Volumetric soil water content in layer L at saturation
!             (cm3 [water] / cm3 [soil])
! SDDES(J)  Number of seeds destroyed today in cohort J when shells are 
!             not destroyed (#/m2/day)
! SDGR      Potential growth rate per seed (g / seed / d)
! SDMAX     A maximum amount of remaining growth for each cohort (g/m2)
! SDMAXX     
! SDNO(J)   Number of seeds for cohort J (#/m2)
! SDPDVR    Seed per pod for cultivar (# / pod)
! SDVAR     Maximum cultivar-dependent seed growth rate, per seed
!             (g / seed / d)
! SEEDNO    Total number of seeds (#/m2)
! SETMAX    Fraction of pod and seed number to set in a given cohort if the 
!             supply to demand ratio for that cohort is less than 1.0 
! SFDUR     Seed filling duration for a cohort of seed (p-t-d)
! SHELN(J)  Number of shells for cohort J (#/m2)
! SHELWT    Total mass of all shells (g / m2)
! SHLAG     Shell (peg) growth rate during its initial slow growth phase 
!             after beginning pegging (R2) as a fraction of shell growth 
!             rate (SHVAR) during its rapid growth phase. 
! SHMAXG    Maximum growth rate per shell (g[tissue] / shell / d)
! SHMINE    Protein mass mined from shell tissue for seed growth
!             (g[shell] / m2)
! SHRAT     Supply : demand ratio for pod abortion 
! SHVAR     Shell growth rate during its rapid growth phase, per shell
!             (g / shell / d)
! START     Initial shell number (non-aborted number) when shell number is 
!             being reduced because of low supply-demand ratio (# / m2)
! SUPDAY    Today's ratio of minimum of carbohydrate supply to C demand or 
!             N supply to N demand.  Computed for each cohort to determine 
!             fraction of shells and seed to add in that cohort.  Computed 
!             in order of age, so could =1 for first shells and 0 for 
!             youngest 
! SUPDE(J)  Cumulative average supply-demand ratio for shells formed on day 
!             J 
! SW(L)     Volumetric soil water content in layer L
!             (cm3 [water] / cm3 [soil])
! SWADD1     Soil water factor 
! SWADD2    Water stress factor 
! SWBAR     Average ratio of extractable soil water to potential 
!             extractable soil water over DSWBAR depth to affect flower, 
!             pod addition 
! SWFAC     Effect of soil-water stress on photosynthesis, 1.0=no stress, 
!             0.0=max stress 
! TABEX     Function subroutine - Lookup utility 
! TDUMX     Photo-thermal time that occurs in a real day based on early 
!             reproductive development temperature function
!             (photo-thermal days / day)
! TEMPOD    Factor for modifying pod setting based on temperature 
! TGRO(I)   Hourly air temperature (°C)
! THETA     Curvature of rectangular hyperbola to limit seed growth rate to 
!             hold minimum seed N concentration. 
! THRESH    The maximum ratio mass of seed to mass of seed plus shell at 
!             maturity.  Causes seed to stop growing as their dry weights 
!             increase until shells are filled in a cohort. 
! TOTAL     Check for total composition equal to one. 
! TRIGGR    Switch to determine if full pod load has been set for at least 
!             5 days 
! TS        Number of intermediate time steps (=24) 
! TURADD    Water stress factor (TURFAC) effect on reproductive growth and 
!             pod addition.  Stress is defined to INCREASE growth and 
!             addition. 
! TYPPDT    Type of function for temperature effects on pod and seed 
!             addition rates 
! WSDDTN    New seed growth today (g[seed] / m2 / d)
! WSHDTN    New shell growth today (g[shell] / m2 / d)
! WTABR     Weight of shells aborted on a day by cohort (g[shell] / m2 / d)
! WTABRT    Weight of shells aborted on a day (g[shell] / m2 / d)
! WTPSD     Maximum weight per seed under non-limiting substrate (g / seed)
! WTSD(J)   Seed mass  for cohort J (g/m2)
! WTSHE(J)  Shell mass  for cohort J (g/m2)
! WTSHM     Mature shell weight (g/m2)
! WTSHMT     Cohorts that reach THRESH today (g/m2)
! WTSHMY    Yesterday's mature shell weight (g/m2)
! XFRT      Current day's partitioning to reproductive growth (0-1)
!             (g[fruit] / g[plant])
! XMPAGE    Maximum physiological age of a fruit beyond which shell 
!             thickening and seed growth end. (p-t-d)
! XRSD      Decrease in seed growth due to low N supply (fraction)
! XSWBAR(I) Array of available soil water content in the pegging zone, at 
!             which rate of pod addition (pegging) is affected in a given 
!             proportion (YSWBAR). (cm3/cm3)
! XSWFAC(I) Array of values describing the ratio of soil-root water supply 
!             to transpirative demand, at which the rate of pod addition is 
!             reduced by a given proportion (YSWFAC). 
! YRDOY     Current day of simulation (YYDDD)
! YRNR1     Day when 50% of plants have at least one flower (YYDDD)
! YRNR2     Day when 50% of plants have one peg (peanuts only) (YYDDD)
! YRSIM     Start of simulation date (YYDDD)
! YSWBAR(I) Array describing the relative (0 to 1) rate of pod addition 
!             (pegging) which occurs at a given fraction available soil 
!             water content in the pegging zone 
! YSWFAC(I) Array describing the relative (0 to 1) rate of pod addition 
!             (pegging) which occurs at a given ratio of soil-root water 
!             supply to transpirative demand (XSWFAC). 
!***********************************************************************
!      END SUBROUTINE FOR_PODCOMP
!=======================================================================
