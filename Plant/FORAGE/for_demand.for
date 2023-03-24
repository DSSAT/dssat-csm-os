C=======================================================================
C  FOR_DEMAND, Subroutine, J.W. Jones and G. Hoogenboom.
C-----------------------------------------------------------------------
C  Calculates potential FOR_DEMAND for C and N based upon new growth and
C  existing N deficiency in old tissue.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/01/1990 JWJ Written.
C  02/01/1993 GH  Revised.
C  04/24/1994 NBP Changed TAIRHR to TGRO.
C  08/22/1995 GH  Added seed composition routine from KJB & ELPiper
C  04/02/1996 JWJ Modified partitioning during early growth
C  01/10/1997 GH  Added TURFAC effect on seed growth and pod addition
C  09/15/1998 CHP Modified for modular format
C  05/10/1999 GH  Incorporated in CROPGRO
C  06/24/2003 SJR Modified initialization of XLEAF for grass transplant age
C-----------------------------------------------------------------------
C  Called by:  CROPGRO
C  Calls:      FOR_SDCOMP, FOR_IPDMND
C=======================================================================

      SUBROUTINE FOR_DEMAND(DYNAMIC,
     &  AGRLF, AGRRT, AGRSH2, AGRSTM, CROP, DRPP, DXR57,  !Input
     &  FILECC, FILEGC, FILEIO, FNINSH, FRACDN, LAGSD,    !Input
     &  LNGPEG, NDLEAF, NMINEP, NSTRES,                   !Input
     &  PAR, PGAVL, PHZACC, PUNCSD,                       !Input
     &  PUNCTR, PLTPOP, RPROAV, RTWT,                     !Input
     &  SDDES, SDNO, SDVAR, SHELN, SHVAR, SLDOT, SRDOT,   !Input
     &  SSDOT, SSRDOT, STMWT,                             !Input
     &  TAVG, TDUMX, TGRO, TURFAC,                        !Input 
     &  VSTAGE, WCRLF, WCRRT, WCRST,                      !Input
     &  WTLF, WTNLF, WTNRT, WTNSR, WTNST, WTSD,           !Input
     &  WTSHE, YRDOY,                                     !Input
     &  NVEG0, NR1, NR2, NR7, YRSIM,                      !Input

     &  AGRSD1, AGRSD2, AGRVG, AGRVG2, CDMREP, F, FNINL,  !Output
     &  FNINR, FNINS, FNINSD, FRLF, FRRT, FRSTM, GDMSD,   !Output
     &  GRRAT1, NDMNEW, NDMOLD, NDMREP, NDMSDR, NDMTOT,   !Output
     &  NDMVEG, PHTIM, PNTIM, POTCAR,                     !Output
     &  POTLIP, SDGR, TURADD, XFRT,                       !Output
     &  PPTFAC, STRWT,                                    !Input?
     &  WCRSR,                                            !Input?
     &  AGRSTR, FNINSR, FRSTR,                            !Output
     &  FRSTRF, FRSTRM, FRSTRMX, LRMOB,                   !Output
     &  NMOBSRN, NMOBSRX, NRMOB, NVSTL, NVSTR, NVSTS,     !Output
     &  NVSTSR, TYPLMOB, TYPNMOB, XSTR, YSTOR)            !Output
     
! 2023-01-18 CHP removed unused variables from argument list:
!  LFSCMOB, LFSNMOB, PCNL, PCNRT, PCNST, RTSCMOB, RTSNMOB, SRSCMOB, 
!  SRSNMOB, STSCMOB, STSNMOB, SWFAC, TDUMX2, WNRLF, WNRRT, WNRSH, 
!  WNRST, XPOD, NR5, NMOBR, NMOBSR, PPMFAC, PCNSR, WLIDOT, WNRSR, XLAI, 
!  

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL FOR_IPDMND, FOR_SDCOMP, FIND, ERROR, IGNORE, GETLUN, 
     &  TABEX, TIMDIF, CURV
      SAVE
      CHARACTER*1 PLME
      CHARACTER*2 CROP
      CHARACTER*3 TYPSDT
      CHARACTER*30 FILEIO
      CHARACTER*92 FILECC, FILEGC
      
!     CHARACTER*80  C80
      CHARACTER*6   SECTION
      CHARACTER*255 C255
!     CHARACTER*6   ERRKEY

      INTEGER DYNAMIC, TIMDIF
      INTEGER NPP, I, NAGE, DAS, YRSIM
      INTEGER YRDOY, NDLEAF, NR1, NR2, NR7, NVEG0  !NR5, 
      
      INTEGER LUNCRP, ERR, LNUM, FOUND, ISECT  !LUNIO, LUNECO, 

      REAL FRLFM, FRSTMM, YY, XX, TMPFAC
      REAL REDPUN,TMPFCS,PAGE,REDSHL,SDMAX,CDMSH,GDMSH,ADDSHL
      REAL TEMXFR,CAVTOT,GDMSDO,CNOLD
      REAL NVSTL,NVSTS,NVSTR,FRNLFT
      REAL TABEX,CURV
      REAL POTLIP, POTCAR

      REAL TPHFAC,PARSLA,FFVEG
      REAL GROYES,GAINNW,GAINWT
      REAL SLAVAR, SLAREF, FINREF
      REAL SLAMAX, SLAMIN, THRESH
      REAL AGRSH2, AGRRT, AGRSTM, FRLFF, FRSTMF
      REAL CARMIN, LIPOPT, LIPTB, SLOSUM

      REAL AGRLF, AGRSD1, AGRSD2, AGRVG, AGRVG2,
     &  CDMREP, CDMSD, CDMSDR, CDMTOT,
     &  CDMVEG, DRPP, DUMFAC, DXR57, F,
     &  FNINL, FNINR, FNINS, FNINSD, FNINSH,
     &  FRACDN, FRLF, FRLFMX,
     &  FRRT, FRSTM, FVEG,
     &  GDMSD, GDMSDR,
     &  GROMAX, GRRAT1, LAGSD, LNGPEG, LNGSH,
     &  NDMNEW, NDMOLD, NDMREP,
     &  NDMSD, NDMSDR, NDMSH, NDMTOT, NDMVEG,
     &  NMINEP, NMOBMX, NRCVR, NSTRES, !NMOBR, NRATIO, 
     &  NVSMOB,
     &  PAR,   !PCNL, PCNRT, PCNST,
     &  PGAVL, PLIGSD, PLTPOP, PMINSD, POASD,
     &  PROLFF, PROLFI,
     &  PRORTF, PRORTI, PROSTF, PROSTI, RCH2O,
     &  RLIG, RLIP, RMIN, RNO3C,
     &  ROA, RPRO, RPROAV, RTWT, SDGR,
     &  SDLIP, SDPRO, SDVAR, SHLAG, SHVAR,
     &  SIZELF, SIZREF, SLAMN, SLAMX, SLAPAR,
     &  SRMAX, STMWT, TAVG, TDUMX,  !SWFAC, 
     &  SIZRAT, !TDUMX2,
     &  TURADD, TURFAC, TURSLA, TURXFR,
     &  VSSINK, VSTAGE, WCRLF, WCRRT, WCRST, !WNRLF,
     &  WTLF, XFRMAX,  !WNRRT, WNRSH, WNRST, 
     &  XFRT, XFRUIT   !, XPOD

      REAL FNSDT(4)
      REAL XVGROW(6), YVGROW(6), YVREF(6)
      REAL XSLATM(10), YSLATM(10), XTRFAC(10), YTRFAC(10),
     &    XXFTEM(10), YXFTEM(10)
      REAL XLEAF(8), YLEAF(8), YSTEM(8)
      REAL TGRO(TS)
      REAL SDDES(NCOHORTS), SDNO(NCOHORTS), SHELN(NCOHORTS) 
      REAL WTSD(NCOHORTS), WTSHE(NCOHORTS)
      REAL PHTIM(365), PNTIM(365)

      REAL TURFSL
      REAL NFSL                                       !Diego added
      REAL NSLA                                       !Diego added
      REAL CUMNSF                                     !Diego added
!     REAL NHGT                                       !Diego added
      
!CHP - puncture variables, not functional
      REAL PUNCSD, PUNCTR, RPRPUN     
!-----------------------------------------------------------------------
C Variables for adjusting initial XLEAF for grass transplants
!-----------------------------------------------------------------------
      REAL SDAGPL
!-----------------------------------------------------------------------
C Variables for adding storage organ and dormancy functions
!-----------------------------------------------------------------------

      REAL AGRSTR, FNINSR, FRSTR,   !WLIDOT, 
     &    FRSTRF, FRSTRM, FRSTRMX, NMOBSRN, NMOBSRX,   !NMOBSR, 
     &    NVSTSR, PPTFAC, PROSRF, PROSRI, !PCNSR, PPMFAC, 
     &    STRWT, TFRLF, TFRSTM, TFRRT, WCRSR, !TFRSTR, WNRSR, 
     &    XSTR, YSTOR(8)  !XLAI, 

      CHARACTER*3 TYPLMOB, TYPNMOB 
      REAL LRMOB(4), NRMOB(4)
      REAL PHZACC(20), SDLEST
      REAL XLFEST(8), YLFEST(8), YSREST(8), YSTEST(8)

!-----------------------------------------------------------------------
C Variables for apportioning NDMVEG and NDMOLD
!-----------------------------------------------------------------------
      REAL CDMOLD, CHOPRO, FROLDA, KCOLD
      REAL SLDOT, SRDOT, SSDOT, SSRDOT
!     REAL LFSCMOB, LFSNMOB, RTSCMOB, RTSNMOB, SRSCMOB, SRSNMOB, 
!    &   STSCMOB, STSNMOB
      REAL WTNLF, WTNRT, WTNSR, WTNST
      REAL PROLFR, PROSTR, PRORTR, PROSRR
!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
      CALL FOR_IPDMND(
     &  FILECC, FILEGC, FILEIO,                           !Input
     &  CARMIN, FINREF, FNSDT, FRLFF, FRLFMX,             !Output
     &  FRSTMF, LIPOPT, LIPTB, LNGSH, NMOBMX,             !Output
     &  NRCVR, NVSMOB, PLIGSD, PMINSD, POASD,             !Output
     &  PROLFF, PROLFI, PRORTF, PRORTI, PROSTF, PROSTI,   !Output
     &  RCH2O, RLIG, RLIP, RMIN, RNO3C, ROA,              !Output
     &  RPRO, SDLIP, SDPRO, SHLAG, SLAMAX, SLAMIN,        !Output
     &  SLAPAR, SLAREF, SLAVAR, SLOSUM, SIZELF, SIZREF,   !Output
     &  SRMAX, THRESH, TURSLA, TYPSDT, VSSINK, XFRMAX,    !Output
     &  XFRUIT, XLEAF, XLFEST, XSLATM, XTRFAC, XVGROW,    !Output
     &  XXFTEM, YLEAF, YLFEST, YSLATM, YSTEM, YSTEST,     !Output
     &  YTRFAC, YVREF, YXFTEM, SDLEST,                    !Output
     &  FRSTRF, FRSTRMX, LRMOB, NMOBSRN,                  !Output
     &  NMOBSRX, NRMOB, PLME, PROLFR, PRORTR, PROSRF,     !Output
     &  PROSRI, PROSRR, PROSTR,                           !Output
     &  SDAGPL, TYPLMOB, TYPNMOB, YSREST, YSTOR, KCOLD )  !Output



!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CDMSDR = 0.0
      GDMSDR = 0.0
      FNINSD = 0.0
      NDMNEW = 0.0
      NDMREP = 0.0
      NDMSD  = 0.0
      NDMSH  = 0.0
      NDMSDR = 0.0
      NDMVEG = 0.0
!      NMOBR  = 0.0
      SDGR   = 0.0
      FNINL  = 0.0
      FNINS  = 0.0
      FNINR  = 0.0
      NMINEP = 0.0

      RPRPUN = 1.0 
      TMPFAC = 1.0
      CUMNSF = 1.0
      
      FNINSR=0.0

C-----------------------------------------------------------------------
C     SET VARIETY SPECIFIC LEAF PARAMETERS
C-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
        DUMFAC = SLAVAR / SLAREF
        F      = DUMFAC * FINREF
        FVEG   = DUMFAC * SLAMAX
        SLAMN  = DUMFAC * SLAMIN
        SLAMX  = DUMFAC * SLAMAX
        GROMAX = 0.0
        SIZRAT = SIZELF / SIZREF

        DO I = 1,6
        YVGROW(I) = SIZRAT * YVREF(I)
        ENDDO

C-----------------------------------------------------------------------
C     INITIALIZE PARTITIONING PARAMETERS - seedlings
C-----------------------------------------------------------------------
        FRLF = TABEX(YLEAF,XLEAF,0.0,8)
        FRSTM = TABEX(YSTEM,XLEAF,0.0,8)
        FRSTR = TABEX(YSTOR,XLEAF,0.0,8)
        FRRT = 1.0 - FRLF - FRSTM - FRSTR


      ENDIF

!***********************************************************************
!***********************************************************************
!     EMERGENCE CALCULATIONS - Performed once per season upon emergence
!         or transplanting of plants
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. EMERG) THEN
!-----------------------------------------------------------------------
        XFRT   = XFRUIT
        ADDSHL = 0.0
        TURXFR = 0.0
        GDMSD  = 0.0
        CDMSD  = 0.0
        NDMSD  = 0.0
        GDMSDR = 0.0
        CDMSDR = 0.0
        NDMSDR = 0.0
        CDMREP = 0.0
        NAGE   = 0
        DO NPP = 1,365
        PHTIM(NPP) = 0.
        PNTIM(NPP) = 0.
        END DO
        FNINSD = SDPRO * 0.16   
        FNINL  = PROLFI * 0.16  
        FNINS  = PROSTI * 0.16  
        FNINR  = PRORTI * 0.16  
        FNINSR = PROSRI * 0.16
        NVSTL = FNINL
        NVSTS = FNINS
        NVSTR = FNINR
        NVSTSR = FNINSR
C-----------------------------------------------------------------------
C     INITIALIZE PARTITIONING PARAMETERS for transplants adjusted for 
C      SDAGE and growth temperature )
C-----------------------------------------------------------------------

        IF (PLME .EQ. 'T') THEN
      
        IF (PHZACC(4) .GE. SDLEST) THEN 
        FRLF = TABEX(YLFEST,XLFEST,VSTAGE,8)
        FRSTM = TABEX(YSTEST,XLFEST,VSTAGE,8)
        FRSTR = TABEX(YSREST,XLFEST,VSTAGE,8)
        ELSE
        FRLF = TABEX(YLEAF,XLEAF,VSTAGE,8)
        FRSTM = TABEX(YSTEM,XLEAF,VSTAGE,8)
        FRSTR = TABEX(YSTOR,XLEAF,VSTAGE,8)
        ENDIF

        FRRT = 1.0 - FRLF - FRSTM - FRSTR
        ENDIF

C***********************************************************************
C***********************************************************************
C     DAILY RATE/INTEGRATION
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
C-----------------------------------------------------------------------
      DAS = MAX(0,TIMDIF(YRSIM,YRDOY))
C-----------------------------------------------------------------------
C     Compute max N mining, NMINEP, based on stage-dependent mining
C     rate, NMOBR
C-----------------------------------------------------------------------
C     Assume that a Maximum Fraction (NMOBMX) of N can be Mobilized per Day
C     NVSMOB is the relative N mobil rate in veg stage, rel to reprod. stage
C-----------------------------------------------------------------------
C     9/27/95 ACCELERATE N MOBILIZATION AFTER R5, FUNCTION OF (1-SWFAC)
C     ALLOWS ACCELERATING BY 50% IF MAX DEFICIT.
C     2/6/96 SOMETIMES SEEDS FILL, XPOD IS LOW, THEN N MOBILIZATION SLOWS
C     I DON'T REALLY WANT THAT, LATE IN CYCLE.  KJB
C     NOW, DXR57 HITS CLOSE TO 1 AT MATURITY AND PREVENTS THAT
C-----------------------------------------------------------------------

!      NMOBR  = NVSMOB * NMOBMX * TDUMX
!      IF (DAS .GT. NR5) THEN
!        NMOBR = NMOBMX * TDUMX2 * (1.0 + 0.5*(1.0 - SWFAC))
!     &  * (1.0 + 0.3*(1.0 - NSTRES)) * (NVSMOB + (1. - NVSMOB)
!     &  * MAX(XPOD,DXR57**2.))
!      ENDIF

C-----------------------------------------------------------------------
C      DSSAT4 equation
C-----------------------------------------------------------------------
!      NMINEP = NMOBR * (WNRLF + WNRST + WNRRT + WNRSH)

C-----------------------------------------------------------------------
C      Add N from storage to N available for potential mobilization
C-----------------------------------------------------------------------
!        NMINEP = NMOBR * (WNRLF + WNRST + WNRSH) +
!     &              NMOBR * PPMFAC * WNRRT +
!     &              NMOBSR * WNRSR

C-----------------------------------------------------------------------
      IF (DAS .GE. NR1) THEN
C-----------------------------------------------------------------------
C     Accumulate physiological age of flower (PNTIM) and pod (PHTIM) cohorts
C-----------------------------------------------------------------------
        IF (DAS .EQ. NR1) THEN
        PNTIM(1) = 0.0
        ELSE
        PNTIM(DAS - NR1 + 1) = PNTIM(DAS - NR1) + TDUMX
        ENDIF

        IF (DAS .LE. NR2) THEN
        PHTIM(1) = 0.0
        ELSE
        PHTIM(DAS - NR2 + 1) = PHTIM(DAS - NR2) + TDUMX
        ENDIF

C-----------------------------------------------------------------------
C     Calculate function for modifying seed growth rate with temperature
C-----------------------------------------------------------------------
        TMPFAC = 0.
        TMPFCS = 0.
        DO I = 1,24
        TMPFAC =CURV(TYPSDT,FNSDT(1),FNSDT(2),FNSDT(3),FNSDT(4),
     &    TGRO(I))
        TMPFCS = TMPFCS + TMPFAC
        ENDDO
        TMPFAC = TMPFCS / 24.
C-----------------------------------------------------------------------
C       Calculate reduction in seed growth due to insect punctures
C-----------------------------------------------------------------------
        IF (PUNCSD .GT. 0.001) THEN
        REDPUN = 1.0 - (PUNCTR/PUNCSD) * RPRPUN
        REDPUN = MAX(0.0,REDPUN)
        ELSE
        REDPUN = 1.0
        ENDIF
C-----------------------------------------------------------------------
C       Water stress factor (TURADD) effect on reproductive growth and
C       pod addition.  Stress is defined to INCREASE growth and addition.
C-----------------------------------------------------------------------
        TURADD = TABEX (YTRFAC,XTRFAC,TURFAC,4)
C-----------------------------------------------------------------------
C     Calculate maximum growth per seed based on temp and seed punctures
C-----------------------------------------------------------------------
        SDGR = SDVAR * TMPFAC * REDPUN * (1.-(1.-DRPP)*SRMAX) *
     &   (1. + TURADD)
C-----------------------------------------------------------------------
C     Initialize Seed Growth Demands and CH2O and N required for seed
C       growth
C-----------------------------------------------------------------------
        GDMSD  = 0.0
        CDMSD  = 0.0
        NDMSD  = 0.0
        GDMSDR = 0.0
        CDMSDR = 0.0
        NDMSDR = 0.0
C-----------------------------------------------------------------------
        IF (DAS .GT. NR2) THEN
        DO NPP = 1, DAS - NR2
C-----------------------------------------------------------------------
C     Calculate physiol age of seed cohort.  Do not allow seed to grow
C     until shells are greater than LAGSD physiol age.
C-----------------------------------------------------------------------
        PAGE = PHTIM(DAS - NR2 + 1) - PHTIM(NPP)
        IF (PAGE .GE. LAGSD) THEN
C-----------------------------------------------------------------------
C     Allow cohort growth until threshing limit (seed wt./pod wt) occurs
C     taking into account damage by pests to seed and shells
C-----------------------------------------------------------------------
        REDSHL = 0
        IF (SDDES(NPP).GT.0) THEN
        REDSHL = WTSHE(NPP)*SDDES(NPP)/(SDDES(NPP)+SDNO(NPP))
        ENDIF
        SDMAX = (WTSHE(NPP)-REDSHL)*THRESH/(100.-THRESH)-WTSD(NPP)
        SDMAX = MAX(0.0,SDMAX)
C-----------------------------------------------------------------------
C     Compute Seed Growth Demand, GDMSD, and N required for seed, NDMSD
C-----------------------------------------------------------------------
        GDMSD  = GDMSD  + MIN(SDGR*SDNO(NPP)*REDPUN, SDMAX)
        ENDIF
        ENDDO
C-----------------------------------------------------------------------
C     Call seed composition routine
C-----------------------------------------------------------------------
        CALL FOR_SDCOMP(
     &  CARMIN, LIPOPT, LIPTB, PLIGSD,                !Input
     &  PMINSD, POASD, RCH2O, RLIG, RLIP,             !Input
     &  RMIN, RNO3C, ROA, SDLIP, SLOSUM,              !Input
     &  SDPRO, TAVG,                                  !Input
     &  AGRSD1, AGRSD2, FNINSD, POTCAR, POTLIP)       !Output

        NDMSD  = FNINSD * GDMSD
C-----------------------------------------------------------------------
C     Calculate Amount of Mobilized N Which Can be Used for Seed Growth,
C     NDMSDR, potential seed growth from this source of N, GDMSDR,
C     and CH2O required for this seed growth from mobilized N, CDMSDR
C-----------------------------------------------------------------------
        IF (NDMSD .GT. NMINEP) THEN
        NDMSDR = NMINEP
        ELSE
        NDMSDR = NDMSD
        ENDIF
        GDMSDR = NDMSDR/FNINSD
        CDMSDR = GDMSDR * (AGRSD1 + FNINSD*6.25 * RPRO)
C-----------------------------------------------------------------------
C    Compute Total CH2O Demand to Grow GDMSD g Tissue
C-----------------------------------------------------------------------
        CDMSD = (MAX(0.0,(GDMSD - GDMSDR))) * AGRSD2 + CDMSDR
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
C     Compute max growth per shell, depending on temp, daylength
C-----------------------------------------------------------------------
      GRRAT1 = SHVAR * TMPFAC * (1.- (1.-DRPP) * SRMAX)
     & * (1.0 + TURADD)
C-----------------------------------------------------------------------
C     Initialize Shell Growth Demand, N (NDMSH) and C (CDMSH) needed for growth
C-----------------------------------------------------------------------
      GDMSH = 0.0
      NDMSH = 0.0
      CDMSH = 0.0
C-----------------------------------------------------------------------
C     Compute growth demand for shells, GDMSH, allowing slow growth
C     until LNGPEG age, then potential growth until LNGSH
C-----------------------------------------------------------------------
      IF (DAS .GT. NR2) THEN
        DO NPP = 1,DAS - NR2
        NAGE = DAS - NR2 + 1 - NPP  !NAGE not used - chp
        PAGE = PHTIM(DAS - NR2 + 1) - PHTIM(NPP)
        IF (PAGE .LE. LNGSH .AND. SHELN(NPP) .GE. 0.001 .AND.
     &   GRRAT1 .GE. 0.001) THEN
        IF (PAGE .GE. LNGPEG) THEN
        !Shells between LNGPEG and LNGSH
        ADDSHL = GRRAT1 * SHELN(NPP)
        ELSE
        !Shells < LNGPEG
        ADDSHL = GRRAT1 * SHELN(NPP) * SHLAG
        ENDIF
        ENDIF
        GDMSH  = GDMSH + ADDSHL
        ENDDO
C-----------------------------------------------------------------------
C     Compute CH2O required for the potential shell growth
C-----------------------------------------------------------------------
        CDMSH = GDMSH * AGRSH2
      ENDIF
C-----------------------------------------------------------------------
C     Compute TEMXFR, the temp effect on partitioning to pods
C     High temp would increase fraction growth to vegetative tissue
C-----------------------------------------------------------------------
      TEMXFR = 0.
      DO I = 1,24
        TEMXFR = TEMXFR + TABEX(YXFTEM,XXFTEM,TGRO(I),6)
      ENDDO
      TEMXFR = TEMXFR/24.
C-----------------------------------------------------------------------
C     Partitioning to pods is increased under drought stress conditions
C        depending on XFRMAX, an input parameter
C-----------------------------------------------------------------------
      TURXFR = XFRMAX * (1. - TURFAC)
      TURXFR = MIN(TURXFR,1.0)
      TURXFR = MAX(TURXFR,0.0)
C-----------------------------------------------------------------------
C     Night length and temperature are multiplicative
C     but turgor effect adds to the partitioning
C-----------------------------------------------------------------------
      XFRT = XFRUIT * TEMXFR + XFRUIT * TURXFR
c     XFRT = XFRUIT * RNIT * TEMXFR   !NEED TO FIX FOR DAYLENGTH EFFECT
      XFRT = MIN(XFRT,1.0)
      XFRT = MAX(XFRT,0.0)
C-----------------------------------------------------------------------
C    Total Potential Available CH2O for Reprod Growth (CAVTOT)
C    and total CH2O needed for potential reproductive growth (CDMREP)
C-----------------------------------------------------------------------
      CAVTOT = PGAVL * XFRT
      CDMREP = CDMSH + CDMSD
C-----------------------------------------------------------------------
C    Adjust C-Demand for New Growth if C-Available is Less than C Demand
C    Also adjust tissue growth demand for seeds and shells
C-----------------------------------------------------------------------
      GDMSDO = GDMSD
      IF (CDMREP .GT. CAVTOT) THEN
        IF (CDMSD .GT. CAVTOT) THEN
        CDMSH = 0.0
        GDMSH = 0.0
        CDMSD = CAVTOT
        IF (CDMSDR .GT. CAVTOT) THEN
        CDMSDR = CAVTOT
        ENDIF
        GDMSD = (MAX(0.0,(CDMSD-CDMSDR)))/AGRSD2 +
     &    CDMSDR/(AGRSD1+FNINSD*6.25*RPRO)
        NDMSDR = GDMSDR * FNINSD
        ELSE
        CDMSH = CAVTOT - CDMSD
        GDMSH = CDMSH/AGRSH2
        ENDIF
        CDMREP = CDMSD + CDMSH
      ENDIF
C-----------------------------------------------------------------------
C     Compute N demand for seed, shell, and total reproductive growth
C-----------------------------------------------------------------------
      NDMSD  = GDMSD * FNINSD
      NDMSH  = GDMSH * FNINSH
      NDMREP = NDMSD + NDMSH

C-----------------------------------------------------------------------
C     Vegetative partitioning factors and demand for C and N for new
C     growth before VSSINK, assume leaf expansion is fixed, compute
C     SLA based on function of light, temp, etc, then compute
C     FRLF (leaf partitioning), then FRRT, FRSTM
C-----------------------------------------------------------------------
C     Check to See if New Vegetative Tissue Can Be Grown, Using PGAVL
C-----------------------------------------------------------------------
      CDMVEG = MAX(0.0,(1.-XFRT)*PGAVL)
      NDMVEG = 0.0
      CDMVEG = (PGAVL * XFRT - CDMREP) + CDMVEG

!-----------------------------------------------------------------------
!       This is from documentation:  check no longer needed?? chp
!-----------------------------------------------------------------------
!      CDMVEG = MAX(0.0,(1.-XFRT)*PGAVL)
!      IF (PGAVL * XFRT .GT. CDMREP) THEN
!        IF(N .LE. NDLEAF) CDMVEG = (PGAVL * XFRT - CDMREP) + CDMVEG
!      ENDIF
!-----------------------------------------------------------------------

C-----------------------------------------------------------------------
      IF (DAS .EQ. NR1) THEN
!-----------------------------------------------------------------------
C     Fraction of growth going to leaves, roots and storage decreases
C     linearly between R1 and NDLEAF.
C-----------------------------------------------------------------------
        FRLFM  = TABEX (YLFEST, XLFEST, VSTAGE, 8)
        FRSTMM = TABEX (YSTEST, XLFEST, VSTAGE, 8)
        FRSTRM = TABEX (YSREST, XLFEST, VSTAGE, 8)
        

        YY = FRLFM - FRLFF 
        XX = FRSTMM - FRSTMF
        XSTR = FRSTRM - FRSTRF

      ENDIF
!-----------------------------------------------------------------------
      IF (DAS .LT. NR1) THEN
C-----------------------------------------------------------------------
C     Calculate Pattern of Vegetative Partitioning, a function of V-STAGE
C-----------------------------------------------------------------------
        IF (PHZACC(4) .GE. SDLEST) THEN 
        FRLF  = TABEX(YLFEST,XLFEST,VSTAGE,8)
        FRSTM = TABEX(YSTEST,XLFEST,VSTAGE,8)
        FRSTR = TABEX(YSREST,XLFEST,VSTAGE,8)
        ELSE
        FRLF  = TABEX(YLEAF,XLEAF,VSTAGE,8)
        FRSTM = TABEX(YSTEM,XLEAF,VSTAGE,8)
        FRSTR = TABEX(YSTOR,XLEAF,VSTAGE,8)
        ENDIF

      ELSE
C-----------------------------------------------------------------------
C     Partitioning between vegetative tissues depends on development
C     as expressed by FRACDN, the relative development between R1 and NDLEAF
C-----------------------------------------------------------------------
        FRLF = FRLFM - YY * FRACDN
        FRSTM = FRSTMM - XX * FRACDN
        FRSTR = FRSTRM - XSTR * FRACDN

        IF ( DAS .GE. NDLEAF) THEN
        FRLF = FRLFF
        FRSTM = FRSTMF
        FRSTR = FRSTRF
        ENDIF
      ENDIF


      FRRT = 1. - FRLF - FRSTM -FRSTR

!      Location to insert code to increase partitioning to leaf
!      in spring and summer but not fall.  Per Dr. Boote.

      IF (PPTFAC .GT. 0.0) THEN
        FRSTR = (FRSTRMX - FRSTR) * PPTFAC + FRSTR
       TFRLF = FRLF/(FRLF + FRSTM + FRRT) * (1-FRSTR)
       TFRSTM = FRSTM/(FRLF + FRSTM + FRRT) * (1-FRSTR)
       TFRRT = FRRT/(FRLF + FRSTM + FRRT) * (1-FRSTR)
       FRLF=TFRLF
       FRSTM=TFRSTM
       FRRT=1.0 - (FRLF + FRSTM + FRSTR)
      ENDIF

!-----------------------------------------------------------------------
C     Compute F, specific leaf area for new leaf weight
C-----------------------------------------------------------------------
      TPHFAC = 0.
      DO I = 1,24
        TPHFAC = TPHFAC + TABEX (YSLATM,XSLATM,TGRO(I),5)
      ENDDO
      TPHFAC = TPHFAC/24.
C-----------------------------------------------------------------------
      PARSLA = (SLAMN+(SLAMX-SLAMN)*EXP(SLAPAR*PAR))/SLAMX
      TURFSL = MAX(0.1, (1.0 - (1.0 - TURFAC)*TURSLA))
      
      !Nitrogen effect added by Diego
!-----------------------------------------------------------------------
!    Find and Read Leaf Growth Section
!-----------------------------------------------------------------------
      !SECTION = '!*LEAF'
      !CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      !IF (FOUND .EQ. 0) THEN
      !  CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      !ELSE
      !  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
      !  READ(C80,'(4F6.0)',IOSTAT=ERR) FINREF, SLAREF, SIZREF, VSSINK
      !  IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      !  
      !  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
      !  READ(C80,'(24X,F6.3)',IOSTAT=ERR) NSLA
      !  IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      !  ENDIF
        !SECTION = '!*LEAF'
        !CALL IGNORE(C80)
        !CALL IGNORE(C80)
        !READ(C80,'(24X,F6.3)') NSLA
        !write(700,'(I7,F10.3)') YRDOY,NSLA
C---------------------------------------------------------------- added by Diego      
      CALL GETLUN('FILEC', LUNCRP)      
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
        SECTION = '!*LEAF'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
      CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(24X,F6.0)') NSLA
!        WRITE(1000,'(I7,F10.3)') YRDOY,NSLA
        CLOSE (LUNCRP)
C-----------------------------------------------------------------
      if (NSLA .GT. 1.2) then     !To limit NSLA to 1.2
          NSLA=1.2 
          endif
      NFSL   = MAX(0.1, (1.0 - (1.0 - NSTRES)*NSLA))       
      CUMNSF = 0.75*CUMNSF + 0.25*NFSL  
C-----------------------------------------------------------------------
C     Compute overall effect of TMP, PAR, water stress on SLA (F), first
C     for veg stages, then transition to rep stage from R1 to end leaf
C     effect of PAR on SLA, COX PEANUT SCI. 5:27, 1978
C-----------------------------------------------------------------------
      !* NFSL !NFSL added by Diego
      FFVEG = FVEG * TPHFAC * PARSLA * TURFSL * CUMNSF   
!      WRITE(2500,'(F10.3)') CUMNSF
      F = FFVEG
      IF (XFRT*FRACDN .GE. 0.05) F = FFVEG * (1.0 - XFRT * FRACDN)
C-----------------------------------------------------------------------
C     For determinate plants (XFRUIT=1.) leaf expansion stops at NDLEAF
C-----------------------------------------------------------------------
      IF (XFRUIT .GT. 0.9999 .AND. DAS .GE. NDLEAF) F = 0.0

C-----------------------------------------------------------------------
C     During early vegetative growth, leaf area expansion depends on
C     VSTAGE (Prior to VSSINK).  This sets FRLF, partitioning of d.m.
C     to leaves.  FRRT and FRSTM are then computed by left over C.  When
C     an upper limit of d.m. goes to leaves, leaf area expansion is
C     restricted so that F is maintained as computed and minimal amounts
C     of C is partitioned to FRSTM and FRRT  (JWJ 4/1/96)
C-----------------------------------------------------------------------
      IF (VSTAGE .LT. VSSINK) THEN
        GROYES = GROMAX
        GROMAX = TABEX(YVGROW,XVGROW,VSTAGE,6) * SIZELF/SIZREF
        GAINNW = (GROMAX - GROYES) * PLTPOP
C-----------------------------------------------------------------------
C     CALCULATE MINIMUM WEIGHT NEEDED TO ADD GAINNW LEAF AREA/M2,
C     AND AMOUNT OF LEAF WEIGHT WHICH CAN BE GROWN WITH PG AVAILABLE
C-----------------------------------------------------------------------
        IF (F .GT. 0.0) THEN
        GAINWT = GAINNW/F
        ELSE
        GAINWT = 0.0
        ENDIF
C-----------------------------------------------------------------------
C     Compute fraction of C partitioned to leaves, based on F, VSSINK
C     Limit leaf partitioning to FRLFMX (i.e., FRLFMX = 0.7)
C-----------------------------------------------------------------------
        FRLF = (AGRLF*GAINWT)/(CDMVEG + 0.0001)
        IF (FRLF .GT. FRLFMX) THEN
        GAINWT = (CDMVEG/AGRLF) * FRLFMX
        GAINNW = GAINWT * F
        FRLF = FRLFMX
        ENDIF
C-----------------------------------------------------------------------
C     Recompute FRSTM, FRSTR, and FRRT based on FRLF
C-----------------------------------------------------------------------
        FRSTM = (1. - FRLF) * FRSTM / (FRSTM + FRRT + FRSTR)
        FRSTR = (1. - FRLF) * FRSTR / (FRSTM + FRRT + FRSTR)
        FRRT  = 1. - FRLF - FRSTM - FRSTR
C-----------------------------------------------------------------------
      ENDIF
C-----------------------------------------------------------------------
C     Compute CH2O cost per g of tissue, excluding cost for protein (AGRVG)
C     and total CH2O cost per g of veg tissue (AGRVG2)
C-----------------------------------------------------------------------
      AGRVG = AGRLF * FRLF + AGRRT * FRRT + AGRSTM * FRSTM 
     &    + AGRSTR * FRSTR
      AGRVG2 = AGRVG + (FRLF*PROLFI+FRRT*PRORTI+FRSTM*PROSTI+
     &    FRSTR*PROSRI)*RPROAV
C-----------------------------------------------------------------------
C    Compute N Demand for New Tissue, including reproductive and vegetative
C-----------------------------------------------------------------------
      NDMVEG = (CDMVEG/AGRVG2) * (FRLF*FNINL+FRSTM*FNINS+
     &   FRRT*FNINR+FRSTR*FNINSR)
      NDMNEW = NDMREP + NDMVEG
C-----------------------------------------------------------------------
C    Check to See if Any C is Left After Reproductive Growth for
C    Reducing N to Re-Fill Old Tissue, if N Can Be Taken up by Roots
C-----------------------------------------------------------------------
      CNOLD = MAX(0.0,PGAVL-CDMREP)
      NDMOLD = 0.0
C-----------------------------------------------------------------------
C    Nitrogen Demand for Old Tissue
C-----------------------------------------------------------------------
      IF (DAS .GT. NVEG0 .AND. DAS .LT. NR7 .AND.
     &    CNOLD .GT. 0.0) THEN
        NVSTL = PROLFR*0.16
        NVSTS = PROSTR*0.16
        NVSTR = PRORTR*0.16
        NVSTSR = PROSRR*0.16

        IF (DXR57 .GT.0.0) THEN
        FRNLFT = (NRCVR + (1. - NRCVR) * (1. - DXR57**2))
        NVSTL = PROLFF*0.16 + ((PROLFR-PROLFF)*0.16) * FRNLFT
        NVSTS = PROSTF*0.16 + ((PROSTR-PROSTF)*0.16) * FRNLFT
        NVSTR = PRORTF*0.16 + ((PRORTR-PRORTF)*0.16) * FRNLFT
        NVSTSR = PROSRF*0.16 + ((PROSRR-PROSRF)*0.16) * FRNLFT
        ENDIF
!        NDMOLD = (WTLF  - WCRLF) * MAX(0.0,(NVSTL - PCNL /100.))
!     &     + (STMWT - WCRST) * MAX(0.0,(NVSTS - PCNST/100.))
!     &     + (RTWT  - WCRRT) * MAX(0.0,(NVSTR - PCNRT/100.))
!     &         + (STRWT - WCRSR) * MAX(0.0,(NVSTSR - PCNSR/100.))

C-----------------------------------------------------------------------
C    02/01/06 SJR  Adjust N refill capacity for today's DM and N lost 
C                        to natural senescence
C-----------------------------------------------------------------------

        NDMOLD = MAX(0.0,(WTLF - SLDOT - WCRLF) * PROLFR * 0.16  
     &    - WTNLF) 
     &    + MAX(0.0,(STMWT - SSDOT - WCRST) * PROSTR * 0.16
     &    - WTNST)
     &    + MAX(0.0,(RTWT  - SRDOT - WCRRT) * PRORTR * 0.16 
     &    - WTNRT)
     &    + MAX(0.0,(STRWT - SSRDOT - WCRSR) * PROSRR * 0.16
     &    - WTNSR)


        IF (NDMOLD .GT. (CNOLD/RNO3C*0.16)) THEN
        NDMOLD = CNOLD/RNO3C*0.16
        ENDIF
      ENDIF

C-----------------------------------------------------------------------
C      KJB/SJR New code to minimize/fix situation of overspending PGAVL on
C      N demand.  Original code uses all PGAVL for CDMREP and CDMVEG 
C      then goes ahead and calculates NDMOLD using CDMVEG again.
C      Get to N uptake and can potentially take up more N than have CHO
C      to reduce.  Contributes to NLEAK.
C-----------------------------------------------------------------------
      IF (NDMOLD .GT. 0.0) THEN

        CHOPRO = NDMOLD * 6.25 * RNO3C
      
        FROLDA = 1-EXP(-KCOLD * (CDMVEG / CHOPRO))
      
        CDMOLD = CHOPRO * FROLDA

        NDMOLD = FROLDA * NDMOLD

        CDMVEG = CDMVEG - CDMOLD

        NDMVEG = (CDMVEG/AGRVG2) * (FRLF*FNINL+FRSTM*FNINS+
     &    FRRT*FNINR+FRSTR*FNINSR)

        NDMNEW = NDMREP + NDMVEG

      ENDIF
C-----------------------------------------------------------------------
C      KJB/SJR End new code.  Now CDMTOT=PGAVL, not more.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C    Total N Demand
C-----------------------------------------------------------------------
      NDMTOT = NDMREP + NDMVEG + NDMOLD

C-----------------------------------------------------------------------
C    Compute Total Demand for C, and Max. C that Could be Mined
!     CDMTOT not used - chp
C-----------------------------------------------------------------------
      CDMTOT = CDMREP + CDMVEG + NDMOLD*RNO3C/0.16 
      GDMSD = GDMSDO
C-----------------------------------------------------------------------
C    At this point, PGAVL will be used entirely, assuming that N can be
C    made available in the ratio described.
C     Growth Demands : GDMSD, GDMSH
C     N-Demands      : NDMREP, NDMVEG, NDMOLD, NDMTOT, NDMNEW
C     C-Demands      : CDMREP, CDMVEG, CDMTOT, CNOLD

C***********************************************************************
C***********************************************************************
C     END OF DYNAMIC IF CONSTRUCT
C***********************************************************************
      ENDIF
C-----------------------------------------------------------------------

      RETURN
      END ! SUBROUTINE FOR_DEMAND
!-----------------------------------------------------------------------
C=======================================================================
C  FOR_IPDMND, Subroutine, C.H. Porter
C-----------------------------------------------------------------------
C  Reads input data for FOR_DEMAND subroutine
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  07/04/1998 CHP Written.
C-----------------------------------------------------------------------
C  Called by:  FOR_DEMAND
C  Calls:      FIND, ERROR, IGNORE
C=======================================================================
      SUBROUTINE FOR_IPDMND(
     &  FILECC, FILEGC, FILEIO,                           !Input
     &  CARMIN, FINREF, FNSDT, FRLFF, FRLFMX,             !Output
     &  FRSTMF, LIPOPT, LIPTB, LNGSH, NMOBMX,             !Output
     &  NRCVR, NVSMOB, PLIGSD, PMINSD, POASD,             !Output
     &  PROLFF, PROLFI, PRORTF, PRORTI, PROSTF, PROSTI,   !Output
     &  RCH2O, RLIG, RLIP, RMIN, RNO3C, ROA,              !Output
     &  RPRO, SDLIP, SDPRO, SHLAG, SLAMAX, SLAMIN,        !Output
     &  SLAPAR, SLAREF, SLAVAR, SLOSUM, SIZELF, SIZREF,   !Output
     &  SRMAX, THRESH, TURSLA, TYPSDT, VSSINK, XFRMAX,    !Output
     &  XFRUIT, XLEAF, XLFEST, XSLATM, XTRFAC, XVGROW,    !Output
     &  XXFTEM, YLEAF, YLFEST, YSLATM, YSTEM, YSTEST,     !Output
     &  YTRFAC, YVREF, YXFTEM, SDLEST,                    !Output
C     &  YTRFAC, YVREF, YXFTEM,                           !Output
     &  FRSTRF, FRSTRMX, LRMOB, NMOBSRN,                  !Output
     &  NMOBSRX, NRMOB, PLME, PROLFR, PRORTR, PROSRF,     !Output
     &  PROSRI, PROSRR, PROSTR,                           !Output
     &  SDAGPL, TYPLMOB, TYPNMOB, YSREST, YSTOR, KCOLD )  !Output
        
!-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE
!-----------------------------------------------------------------------
      CHARACTER*1 PLME
      CHARACTER*3   TYPSDT
      CHARACTER*6   ERRKEY
      PARAMETER (ERRKEY = 'DEMAND')
      CHARACTER*6   SECTION
      CHARACTER*6   ECOTYP, ECONO
      CHARACTER*30  FILEIO
      CHARACTER*80  C80
      CHARACTER*92  FILECC, FILEGC
      CHARACTER*255 C255

      INTEGER LUNCRP, LUNIO, LUNECO, ERR, LNUM, FOUND, ISECT
      INTEGER I, II

      REAL CARMIN, FINREF, FRLFF, FRLFMX, FRSTMF,
     &  LIPOPT, LIPTB, NMOBMX, NRCVR, NVSMOB,
     &  PLIGSD, PMINSD, POASD, PROLFF,
     &  PROLFI, PRORTF, PRORTI, PROSTF, PROSTI,
     &  RCH2O, RLIG, RLIP, RMIN, RNO3C, ROA,
     &  RPRO, SHLAG, SLAMAX, SLAMIN, SLAPAR,
     &  SLAREF, SLAVAR, SLOSUM, SIZELF, SIZREF,
     &  SRMAX, TURSLA, VSSINK, XFRMAX, XFRUIT
        REAL LNGSH, THRESH, SDPRO, SDLIP

        REAL FNSDT(4)
        REAL XVGROW(6), YVREF(6)
        REAL XSLATM(10), YSLATM(10), XTRFAC(10), YTRFAC(10),
     &    XXFTEM(10), YXFTEM(10)
        REAL XLEAF(8), YLEAF(8), YSTEM(8)

      REAL  FRSTRF, FRSTRMX, NMOBSRN, NMOBSRX, PROSRF, PROSRI,
     &  SDAGPL  !, CLAIT
      REAL YSTOR(8)   
      REAL KCOLD
      REAL PROLFR, PROSTR, PRORTR, PROSRR

      CHARACTER*3 TYPLMOB, TYPNMOB 

      REAL LRMOB(4), SDLEST
      REAL NRMOB(4)  !, VEGNPCT, VEGNPMX, VNMOBR, VNSTAT
      REAL XLFEST(8), YLFEST(8), YSTEST(8), YSREST(8)

!-----------------------------------------------------------------------
      CALL GETLUN('FILEIO', LUNIO)
      OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)


!-----------------------------------------------------------------------
!    Read Planting Details Section
!-----------------------------------------------------------------------
      LNUM = 1
        SECTION = '*PLANT'
        CALL FIND(LUNIO, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILEIO, LNUM)
        ELSE
        READ(LUNIO,'(35X,A1,30X,F6.0)')
     &    PLME, SDAGPL
        ENDIF

!-----------------------------------------------------------------------
C    Find and Read Field Section from FILEIO - previously read in IPIBS
!       Look for the second section header beginning with '*CULTI'
C-----------------------------------------------------------------------

      SECTION = '*CULTI'
!      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
!      IF (FOUND .EQ. 0) THEN
!        CALL ERROR(ERRKEY, 1, FILEIO, LNUM)
!      ENDIF
      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILEIO, LNUM)
      ELSE
        READ(LUNIO,'(24X,A6,48X,3F6.0)') ECONO, SLAVAR, SIZELF, XFRUIT
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
!    Find and Read Respiration Section
!-----------------------------------------------------------------------
      LNUM = 1
      SECTION = '!*RESP'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(F6.0,6X,F6.0)',IOSTAT=ERR) RNO3C, RPRO
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(5F6.0)',IOSTAT=ERR)RCH2O,RLIP,RLIG,ROA,RMIN
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
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(F6.0,6X,2F6.0,6X,F6.0)',IOSTAT=ERR)
     &    PROLFI, PROLFF, PROSTI, PROSTF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(F6.0,6X,F6.0)',IOSTAT=ERR) PRORTI, PRORTF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(24X,F6.0)',IOSTAT=ERR) PLIGSD
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(24X,F6.0)',IOSTAT=ERR) POASD
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(24X,F6.0)',IOSTAT=ERR) PMINSD
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(F6.0,6X,F6.0)',IOSTAT=ERR) PROSRI,PROSRF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(5F6.0)',IOSTAT=ERR) KCOLD,PROLFR,PROSTR,PRORTR,PROSRR
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      ENDIF

!-----------------------------------------------------------------------
!    Find and Read Seed Composition Section
!-----------------------------------------------------------------------
      SECTION = '!*SEED'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4F6.0)',IOSTAT=ERR) LIPTB, LIPOPT, SLOSUM, CARMIN
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        SLOSUM = SLOSUM / 100.0
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
        READ(C80,'(18X,3F6.0)',IOSTAT=ERR) NMOBMX, NVSMOB,NRCVR
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(18X,2F6.0)',IOSTAT=ERR) NMOBSRN, NMOBSRX
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4(1X,F5.2),3X,A3)',IOSTAT=ERR)
     &    (LRMOB(II),II=1,4), TYPLMOB
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4(1X,F5.2),3X,A3)',IOSTAT=ERR)
     &    (NRMOB(II),II=1,4), TYPNMOB
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)


      ENDIF

!-----------------------------------------------------------------------
!    Find and Read Vegetative Partitioning Section
!-----------------------------------------------------------------------
      SECTION = '!*VEGE'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(8F6.0)',IOSTAT=ERR)(XLEAF(II),II=1,8)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(8F6.0)',IOSTAT=ERR)(YLEAF(II),II=1,8)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(8F6.0)',IOSTAT=ERR)(YSTEM(II),II=1,8)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(12X,2F6.0)',IOSTAT=ERR) FRSTMF, FRLFF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(F6.0)',IOSTAT=ERR) FRLFMX
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(8F6.0)',IOSTAT=ERR)(YSTOR(II),II=1,8)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(2F6.0)',IOSTAT=ERR) FRSTRF,FRSTRMX
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(8F6.0)',IOSTAT=ERR)(XLFEST(II),II=1,8)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(8F6.0)',IOSTAT=ERR)(YLFEST(II),II=1,8)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(8F6.0)',IOSTAT=ERR)(YSTEST(II),II=1,8)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(8F6.0)',IOSTAT=ERR)(YSREST(II),II=1,8)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(F6.1)',IOSTAT=ERR) SDLEST
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      ENDIF

!-----------------------------------------------------------------------
!    Find and Read Leaf Growth Section
!-----------------------------------------------------------------------
      SECTION = '!*LEAF'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4F6.0)',IOSTAT=ERR) FINREF, SLAREF, SIZREF, VSSINK
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4F6.0)',IOSTAT=ERR) SLAMAX, SLAMIN, SLAPAR, TURSLA
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(6F6.0)',IOSTAT=ERR)(XVGROW(II),II=1,6)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(6F6.0)',IOSTAT=ERR)(YVREF(II),II=1,6)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(5F6.0)',IOSTAT=ERR)(XSLATM(II),II = 1,5)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(5F6.0)',IOSTAT=ERR)(YSLATM(II),II = 1,5)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

!-----------------------------------------------------------------------
!    Find and Read Seed and Shell Growth Section
!-----------------------------------------------------------------------
      SECTION = '!*SEED'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(6X,F6.0)',IOSTAT=ERR) SRMAX
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(6X,2F6.0)',IOSTAT=ERR) XFRMAX, SHLAG
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4(1X,F5.2),3X,A3)',IOSTAT=ERR)
     &    (FNSDT(II),II=1,4), TYPSDT
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(6F6.0)',IOSTAT=ERR)(XXFTEM(II),II = 1,6)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(6F6.0)',IOSTAT=ERR)(YXFTEM(II),II = 1,6)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        DO I=1,5
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        ENDDO
        READ(C80,'(4F6.0)',IOSTAT=ERR)(XTRFAC(II),II = 1,4)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4F6.0)',IOSTAT=ERR)(YTRFAC(II),II = 1,4)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF
!-----------------------------------------------------------------------
      CLOSE(LUNCRP)

C-----------------------------------------------------------------------
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
        READ (C255,'(A6,66X,F6.0,30X,3F6.0)',IOSTAT=ERR)
     &    ECOTYP, LNGSH, THRESH, SDPRO, SDLIP
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
      RETURN
!-----------------------------------------------------------------------
      END  ! SUBROUTINE FOR_IPDMND

!=======================================================================
!       Variable definitions for FOR_DEMAND and FOR_IPDMND
!-----------------------------------------------------------------------
! ADDSHL    Today's growth demand for shells of age NPP (g[shell] / m2 / d)
! AGRLF     Mass of CH2O required for new leaf growth (g[CH2O] / g[leaf])
! AGRRT     Mass of CH2O required for new root growth (g[CH2O] / g[root])
! AGRSD1    CH2O requirement for seed growth, excluding cost for protein 
!             content (g[CH2O] / g[seed])
! AGRSD2    CH2O requirement for seed growth, including cost for protein 
!             content (g[CH2O] / g[seed])
! AGRSH2    CH2O requirement for shell growth, including cost for protein 
!             content (g[CH2O] / g[shell])
! AGRSTM    Mass of CH2O required for new stem growth (g[CH2O] / g[stem])
! AGRVG     Mass of CH2O required for vegetative tissue growth including 
!             stoichiometry and respiration (g[CH2O] / g[tissue])
! AGRVG2    Total mass of CH2O required for vegetative tissue growth
!             (g[CH2O] / g[tissue])
! AGRSTR    Mass of CH2O required for new storage organ growth 
!                  (g[CH2O] / g[storage])
! CARMIN    Minimum carbohydrate fraction 
! CAVTOT    Total potential available CH2O for reproductive growth
!             (g[CH2O] / m2)
! CDMREP    Total CH2O needed for potential reproductive growth
!             (g[CH2O] / m2 / d)
! CDMSD     Total CH2O demand to grow seed demand (GDMSD)
!             (g[CH2O] / m2 / d)
! CDMSDR    CH2O required for seed growth from mobilized N
!             (g[CH2O] / m2 / d)
! CDMSH     Total CH2O demand to grow shell demand (GDMSH)
!             (g[CH2O] / m2 / d)
! CDMOLD    Total CH2O demand for refilling old tissue N
!             (g[CH2O] / m2 / d)
! CDMTOT    Total CH2O demand (g[CH2O] / m2 / d)
! CDMVEG    Carbon demand for vegetative growth (g[CH2O] / m2 / d)
! CHOPRO        CHO required for uptake and reduction of N to fully 
!             refill old tissue N (g[CH2O] / m2 / d)
! CNOLD     Available CH2O after reproductive growth (g[CH2O] / m2 / d)
! CROP      Crop identification code 
! DAS       Days after start of simulation (days)
! DRPP      Photoperiod days which occur in a real day
!             (photoperiod days / day)
! DXR57     Relative time between first seed (NR5) and physiological 
!             maturity (NR7) 
! ECONO     Ecotype code - used to match ECOTYP in .ECO file 
! ECOTYP    Ecotype code for this simulation 
! F         Specific leaf area of new leaf tissue growth, including N
!             (cm2[leaf] / g[leaf])
! FFVEG     Specific leaf area of new leaf tissue growth (interim value)
!             (cm2[leaf] / g[leaf])
! FILECC    Path plus filename for species file (*.spe) 
! FILEGC    Pathname plus filename for ECO file 
! FILEIO    Filename for INP file (e.g., IBSNAT35.INP) 
! FINREF    Specific leaf area (SLA) of leaves of standard crop cultivar 
!             when plants emerge (cm2[leaf] / g[leaf])
! FNINL     Maximum fraction of N for growing leaf tissue (g[N] / g[leaf])
! FNINR     Maximum fraction of N for growing root tissue (g[N] / g[root])
! FNINS     Maximum fraction of N for growing stem tissue (g[N] / g[stem])
! FNINSD    Maximum fraction of N for growing seed tissue based on 
!             temperature (g[N] / g[seed])
! FNINSH    Maximum fraction of N for growing shell tissue
!             (g[N] / g[shell])
! FNINSR    Maximum fraction of N for growing storage tissue 
!                  (g[N] / g[storage])
! FNSDT(I)  Temperature values which describe function for modifying seed 
!             growth rate with temperature (°C)
! FRACDN    Relative time between flowering (NR1) and last leaf appearance 
!             (NDLEAF) 
! FRLF      Fraction of vegetative tissue growth that goes to leaves on a 
!             day (g[leaf] / g[veg])
! FRLFF     Fraction of daily increase in vegetative weight which goes to 
!             leaves after the day on which the maximum number of V-stages 
!             occurs (NDVSTG). (g[leaf] / g[veg])
! FRLFM     Fraction of growth going to leaves, decreases linearly between 
!             R1 and NDLEAF (g[leaf] / g[veg])
! FRLFMX    Maximum leaf partitioning (g[leaf] / g[veg])
! FRNLFT    A quadratic function of the progress from NR5 to NR7 (DXR57), 
!             used to compute the change in maximum tissue N content 
!             between its maximum value and a fractional value (NRCVR) 
!             between the minimum and maximum tissue  N concentrations 
! FROLDA    Fraction of max potential NDMOLD allowed to be met given
!           today's level of CDMVEG.  Prevents refilling old tissue
!           without allowing any new growth due to low PG.
! FRRT      Fraction of vegetative tissue growth that goes to roots on a 
!             day (g[root] / g[veg])
! FRSTM     Fraction of vegetative tissue growth that goes to stems on a 
!             day (g[stem] / g[veg])
! FRSTMF    Fraction of daily dry weight increase in vegetative plant parts 
!             which goes to stems after the day on which the maximum number 
!             of V-stages occurs (NDVSTG). (g[stem] / g[veg])
! FRSTMM    Fraction of growth going to stems, decreases linearly between 
!             R1 and NDLEAF (g[stem] / g[veg])
! FRSTR     Fraction of vegetative tissue growth that goes to storage organ 
!             on a day (g[storage] / g[veg])
! FRSTRF    Fraction of daily dry weight increase in vegetative plant parts 
!             which goes to storage organ after the day on which the maximum 
!             number of V-stages occurs (NDVSTG). (g[storage] / g[veg])
! FRSTRM    Fraction of growth going to storage organ decreases linearly 
!             between  R1 and NDLEAF (g[stem] / g[veg])
! FRSTRMX   Maximum storage organ partitioning (g[storage] / g[veg])
! FVEG      Specific leaf area prior to computing effects of temperature, 
!             PAR, water stress (cm2[leaf] / g[leaf])
! GAINNW    Leaf area added (prior to VSSINK) (cm2[leaf] / m2[ground])
! GAINWT    Leaf weight added (prior to VSSINK and after NDLEAF)
!             (g[leaf] / m2[ground])
! GDMSD     Seed growth demand based on temperature and photoperiod
!             (g[seed] / m2 / d)
! GDMSDO    Seed growth demand (temporary value) (g[seed] / m2 / d)
! GDMSDR    Potential seed growth from NDMSDR (amount of Mobilized N which 
!             can be used for seed growth) (g[seed] / m2 / d)
! GDMSH     Growth demand for shells (g[shell] / m2 / d)
! GROMAX    Maximum leaf area which can be added per plant between 
!             emergence and day of simulation as a function of V-stage on 
!             day of simulation (cm2[leaf] / plant)
! GROYES    Maximum leaf area which could have been added per plant between 
!             emergence and yesterday as a function of V-stage
!             (cm2[leaf] / plant)
! GRRAT1    Maximum growth per individual shell (g / shell / d)
! KCOLD     Curvature factor (K value) for exponential function limiting
!           NDMOLD when PG is low
! LAGSD     Time required between shell growth and seed growth, per cohort
!             (Photo-thermal days)
! LIPOPT    Temperature above which lipid composition is at a maximum (°C)
! LIPTB     Temperature below which lipid composition is zero (°C)
! LNGPEG    Time between start of peg and rapid shell formation (for 
!             peanuts only).  Defines slow growth period. (Photo-thermal days)
! LNGSH     Time required for shell growth (Photo-thermal days)
! LRMOB(4)  CURV response of mobilization to current LAI
! LUNCRP    Logical unit number for FILEC (*.spe file) 
! LUNECO    Logical unit number for FILEE (*.eco file) 
! LUNIO     Logical unit number for FILEIO 
! NAGE      Age of cohort (days)
! NDLEAF    Day when leaf expansion ceased (days)
! NDMNEW    Total N demand for new growth (g[N] / m2 / d)
! NDMOLD    N demand for old tissue (g[N] / m2 / d)
! NDMREP    Total N needed for potential reproductive growth
!             (g[N] / m2 / d)
! NDMSD     Total N demand to grow seed demand (GDMSD) (g[N] / m2 / d)
! NDMSDR    Amount of Mobilized N which can be used for seed growth
!             (g[N] / m2 / d)
! NDMSH     Total N demand to grow shell demand (GDMSH) (g[N] / m2 / d)
! NDMTOT    Total N demand (g[N] / m2 / d)
! NDMVEG    N required for vegetative growth if all PGAVL is used as 
!             computed (g[N] / m2 / d)
! NMINEP    Potential N mobilization from storage (g[N] / m2 / d)
! NMOBMX    Maximum fraction of N which can be mobilized in a day 
! NMOBR     Stage-dependent potential N mining rate expressed as a 
!             fraction of the maximum rate (NMOBMX)
! NMOBSR    Stage-dependent potential N mining rate from storage organ 
!             expressed as a fraction of the maximum rate (NMOBSRX)
! NMOBSRN   Minimum/ "normal" fraction of N which can be mobilized from
!                  storage organ in a day 
! NMOBSRX   Maximum fraction of N which can be mobilized from
!                  storage organ in a day 
! NPP       Cohort number used as index in loops 
! NR1       Day when 50% of plants have at least one flower (days)
! NR2       Day when 50% of plants have one peg (peanuts only) (days)
! NR5       Day when 50% of plants have pods with beginning seeds (days)
! NR7       Day when 50% of plants first have yellowing or maturing pods
!             (days)
! NRCVR     Fractional value between minimum and maximum tissue N values 
!             (0-1) 
! NRMOB(4)  CURV response of mobilization to current vegetative N status 
!              of plant relative to maximum N concentration
! NSTRES    Nitrogen stress factor (1=no stress, 0=max stress) 
! NVEG0     Day of emergence (days)
! NVSMOB    Relative rate of N mining during vegetative stage to that in 
!             reproductive stage 
! NVSTL     N content in leaves (fraction)
! NVSTR     N content in roots (fraction)
! NVSTS     N content in stems (fraction)
! NVSTSR    N content in storage tissue (fraction)
! PAGE      Photothermal age of each cohort (Photo-thermal days)
! PAR       Daily photosynthetically active radiation or photon flux 
!             density (moles[quanta]/m2-d)
! PARSLA    Effect of PAR on specific leaf area 
! PCNL      Percentage of N in leaf tissue (100 g[N] / g[leaf])
! PCNRT     Percent N in root tissue (100 g[N] / g[root])
! PCNSR     Percent N in storage organ tissue (100 g[N] / g[storage])
! PCNST     Percent N in stem tissue (100 g[N] / g[stem])
! PGAVL     Total available CH2O available for growth & respiration
!             (g[CH2O] / m2)
! PHTIM(I)  Cumulative photothermal time ages of seeds and shells 
! PLIGSD    Proportion of seed tissue that is lignin (fraction)
! PLTPOP    Plant population (# plants / m2)
! PMINSD    Proportion of seed tissue that is mineral (fraction)
! PNTIM(I)  Photothermal days from first flower when flowers in age group I 
!             formed (p-t-d)
! POASD     Proportion of seed tissue that is organic acid (fraction)
! POTCAR    Potential carbohydrate composition of seed based on temperature
!             (fraction)
! POTLIP    Potential lipid composition of seed based on temperature
!             (fraction)
! PPMFAC    Reduction in mobilization from storage organ due to photoperiod 
!                  induced dormancy
! PPTFAC    Reduction in partitioning to shoot due to photoperiod 
!                  induced dormancy
! PROLFF    Minimum leaf protein composition after N mining
!             ( g[protein] / g[leaf])
! PROLFI    Maximum protein composition in leaves during growth with 
!             luxurious supply of N (g[protein] / g[leaf tissue])
! PRORTF    Minimum root protein composition after N mining
!             ( g[protein] / g[root])
! PRORTI    Maximum protein composition in roots during growth with 
!             luxurious supply of N (g[protein] / g[root])
! PROSRF    Minimum storage organ protein composition after N mining
!             (g[protein] / g[storage])
! PROSRI    Maximum protein composition in storage organ during growth  
!             with luxurious supply of N (g[protein] / g[storage])
! PROSTF    Minimum stem protein composition after N mining
!             (g[protein] / g[stem])
! PROSTI    Maximum protein composition in stems during growth with 
!             luxurious supply of N (g[protein] / g[stem])
! PUNCSD    Cumulative puncture damage to seed (not yet implemented) 
! PUNCTR    Cumulative puncture damage (not yet implemented) 
! RCH2O     Respiration required for synthesizing CH2O structure
!             (g[CH2O] / g[tissue])
! REDPUN    Reduces growth of seed in an age group due to pest-caused 
!             punctures in seed (0 to 1) 
! REDPUN    Reduces growth of seed in an age group due to pest-caused 
!             punctures in seed (0 to 1) (not yet implemented) 
! REDSHL    Reduces growth of shell in an age group due to pest-caused 
!             punctures in seed (0 to 1) 
! RLIG      Respiration required for synthesizing lignin structure
!             (g[CH2O] / g[lignin])
! RLIP      Respiration required for synthesizing lipid structure
!             (g[CH2O] / g[lipid])
! RMIN      Respiration required for synthesizing mineral structure
!             (g[CH2O] / g[mineral])
! RNO3C     Respiration required for reducing NO3 to protein
!             (g[CH2O] / g[protein])
! ROA       Respiration required for synthesizing organic acids
!             (g[CH2O] / g[product])
! RPRO      Respiration required for re-synthesizing protein from mobilized 
!             N (g[CH2O] / g[protein])
! RPROAV    Respiration required for protein synthesis, average based on 
!             sources of N (g[CH2O] / g[protein])
! RPRPUN    Puncture damage reduction variable (not yet implemented) (0-1) 
! RTWT      Dry mass of root tissue, including C and N
!             (g[root] / m2[ground])
! SDAGPL    Initial planting material age (days)
! SDDES(J)  Number of seeds destroyed today in cohort J when shells are 
!             not destroyed (#/m2/day)
! SDGR      Potential growth rate per seed (g / seed / d)
! SDLIP     Maximum lipid composition in seed (fraction)
! SDMAX     A maximum amount of remaining growth for each cohort (g/m2)
! SDNO(J)   Number of seeds for cohort J (#/m2)
! SDPRO     Seed protein fraction at 25oC (g[protein] / g[seed])
! SDVAR     Maximum cultivar-dependent seed growth rate, per seed
!             (g / seed / d)
! SHELN(J)  Number of shells for cohort J (#/m2)
! SHLAG     Shell (peg) growth rate during its initial slow growth phase 
!             after beginning pegging (R2) as a fraction of shell growth 
!             rate (SHVAR) during its rapid growth phase. 
! SHVAR     Shell growth rate during its rapid growth phase, per shell
!             (g / shell / d)
! SIZELF    The size of a normal upper node leaf (nodes 8 - 10) used to 
!             adjust leaf area expansion during sink-limited phase of 
!             vegetative growth, i.e., prior to VSSINK nodes on the main stem
!             (cm2/leaf)
! SIZRAT    Ratio of upper node normal leaf size for given variety to that 
!             for standard cultivar, used to adjust table of maximum leaf 
!             area vs. V-stage 
! SIZREF    The size of a normal upper node  leaf (nodes 8 - 10) of 
!             standard cultivar. (cm2 / leaf)
! SLAMAX    The maximum specific leaf area (SLA) for new leaves when grown 
!             under low (nearly zero) radiation but optimum water and 
!             temperature for the standard cultivar. (cm2 / g)
! SLAMIN    The minimum specific leaf area (SLA) for new leaves when grown 
!             under infinitely high radiation, optimum water and 
!             temperature for the standard cultivar. (cm2 / g)
! SLAMN     Minimum specific leaf area for new leaves when grown under high 
!             radiation and optimum water and temperature conditions (cm2 / g)
! SLAMX     Maximum specific leaf area for new leaves when grown under low 
!             radiation, but optimum water and temperature conditions
!             (cm2 / g)
! SLAPAR    Coefficient in exponential equation to reduce SLA as PAR 
!             increases (leaf curvature) 
! SLAREF    Specific leaf area (SLA) for new leaves during peak vegetative 
!             growth for the standard cultivar. (cm2/g)
! SLAVAR    Specific leaf area (SLA) for new leaves during peak vegetative 
!             growth for cultivar I, modified by environmental factor (cm2/g)
! SLOSUM    Slope of temperature vs. SUMTEM line (1/oC)
! SRMAX     Maximum fraction change in seed growth rate for long daylengths
!             
! STMWT     Dry mass of stem tissue, including C and N
!             (g[stem] / m2[ground)
! STRWT     Dry mass of storage organ tissue, including C and N
!             (g[storage] / m2[ground)
! SUMTEM    Factor which affects protein composition based on average 
!             temperature. 
! SWFAC     Effect of soil-water stress on photosynthesis, 1.0=no stress, 
!             0.0=max stress 
! TAVG      Average daily temperature (°C)
! TDUMX     Photo-thermal time that occurs in a real day based on early 
!             reproductive development temperature function
!             (photo-thermal days / day)
! TDUMX2    Photo-thermal time that occurs in a real day based on late 
!             reproductive development temperature function
!             (photo-thermal days / day)
! TEMXFR    Temperature effect on partitioning to pods, high temp. 
!             increases fraction of growth to vegetative tissue (0-1) 
! TGRO(I)   Hourly air temperature (°C)
! THRESH    The maximum ratio mass of seed to mass of seed plus shell at 
!             maturity.  Causes seed to stop growing as their dry weights 
!             increase until shells are filled in a cohort. 
! TMPFAC    Modifies maximum growth rate for seed and shells depending on 
!             temperature 
! TMPFCS    Interim value of TMPFAC 
! TPHFAC    Reduction in specific leaf area due to daytime temperature 
!             being less than optimal (0-1) 
! TURADD    Water stress factor (TURFAC) effect on reproductive growth and 
!             pod addition.  Stress is defined to INCREASE growth and 
!             addition. 
! TURFAC    Water stress factor for expansion (0 - 1) 
! TURFSL    Factor which applies water stress to specific leaf area of new 
!             leaf tissue growth 
! TURSLA    Water stress effects on leaf area expansion 
! TURXFR    Turgor water stress factor used to modify partitioning to 
!             reproductive growth 
! TYPLMOB        Shape of CURV response for mobilization to LAI
! TYPNMOB        Shape of CURV response for mobilization to current vegetative N 
!              status of plant relative to maximum N concentration
! TYPSDT    Curve type for temperature factor calculations 
! VNMOBR        Effect of N status on N & C mobilization. (0= no effect, 
!              mobilization at minimum rate, 1.0 = increase mobilization
!              to maximum rate      
! VSSINK    Vegetative stage beyond which sink-limited leaf area expansion 
!             can no longer limit photosynthesis or leaf area growth. 
! VSTAGE    Number of nodes on main stem of plant 
! WCRLF     Mass of CH2O reserves in leaves (g[leaf CH2O] / m2[ground])
! WCRRT     Mass of CH2O reserves in roots (g[root CH2O] / m2[ground])
! WCRSR     Mass of CH2O reserves in storage organ 
!                  (g[storage CH2O] / m2[ground])
! WCRST     Mass of CH2O reserves in stems (g[stem CH2O] / m2[ground])
! WNRLF     N available for mobilization from leaves above lower limit of 
!             mining (g[N] / m2)
! WNRRT     N available for mobilization from roots above lower limit of 
!             mining (g[N] / m2)
! WNRSH     N available for mobilization from shells above lower limit of 
!             mining (g[N] / m2)
! WNRSR     N available for mobilization from storage organ above lower  
!             mining (g[N] / m2)
! WNRST     N available for mobilization from stems above lower limit of 
!             limit of mining (g[N] / m2)
! WTLF      Dry mass of leaf tissue including C and N
!             (g[leaf] / m2[ground])
! WTSD(J)   Seed mass  for cohort J (g/m2)
! WTSHE(J)  Shell mass  for cohort J (g/m2)
! XFRMAX    Maximum increase in partitioning to fruits induced under water 
!             stress, assuming no problem in pod setting 
! XFRT      Current day's partitioning to reproductive growth (0-1)
!             (g[fruit] / g[plant])
! XFRUIT    Maximum fraction of daily available gross photosynthate (PG) 
!             which is allowed to go to seeds plus shells, varies from 0 to 
!             1.0. 
! XLAI        Leaf area (one side) per unit of ground area
!           (m2[leaf] / m2[ground])      
! XLEAF(I)  V-stage at which partitioning to leaves is YLEAF(I).
!             (leaf nodes)
! XPOD      Growth partitioning to pods which slows node appearance
!             (fraction)
! XSLATM(I) Temperature values for function that reduces specific leaf area 
!             (SLA) (°C)
! XSTR        Difference between partitioning fraction to storage organ at 
!             beginning bloom (R1) and at the day on which the maximum  
!             number of V-stages occurs (NDLEAF)       
! XTRFAC(I) Values of TURFAC for function which reduces reproductive growth 
!             based on water stress 
! XVGROW(I) V-stage at which maximum leaf area growth per plant since 
!             emergence is YVGROW(I). (# leaf nodes)
! XX        Difference between partitioning fraction to stems at beginning 
!             bloom (R1) and at the day on which the maximum number of 
!             V-stages occurs (NDLEAF) 
! XXFTEM(I) Array of temperature values in table lookup describing effect 
!             of temperature on partitioning to pods (YXFTEM = 0 TO 1). (°C)
! YLEAF(I)  Partitioning fraction to leaves at V-stage XLEAF(I)
!             ( g[leaf] / g[veg. plant])
! YRDOY     Current day of simulation (YYDDD)
! YRSIM     Start of simulation date (YYDDD)
! YSLATM(I) Array which describes the effect of temperature on specific 
!             leaf area 
! YSTEM(I)  Partitioning factor for stem growth at V-stage XSTEM(I)
!             (g[stem] / g[veg. plant])
! YSTOR(I)  Partitioning factor for storage organ growth at V-stage XSTOR(I)
!             (g[storage] / g[veg. plant])
! YTRFAC(I) Factor which affects reproductive growth based on water stress 
! YVGROW(I) Maximum leaf area grown per plant at V-stage XVGROW(I)
!             (cm2 / plant)
! YVREF(I)  Maximum leaf area grown per plant at V-stage XVGROW(I), for 
!             reference cultivar. (cm2 / plant)
! YXFTEM(I) Array describing the relative partitioning to pods (0 to 1 
!             effect on XFRUIT) as temperature increases. 
! YY        Used to linearly interpolate the difference in partitioning to 
!             leaves between stages R1 and NDLEAF 
!-----------------------------------------------------------------------
!       END SUBROUTINE FOR_DEMAND
!=======================================================================
