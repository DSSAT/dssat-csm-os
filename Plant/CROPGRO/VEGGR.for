C=======================================================================
C  VEGGR, Subroutine, J. W. Jones, K. J. Boote, G. Hoogenboom
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
C  08/12/2003 CHP Added I/O error checking
!  07/13/2006 CHP Added P model
!  06/11/2007 CHP PStres2 affects growth
C-----------------------------------------------------------------------
C  Called by: PLANT
C  Calls:     CANOPY
C             ERROR, FIND, IGNORE
C========================================================================

      SUBROUTINE VEGGR (DYNAMIC, 
     &    AGRLF, AGRRT, AGRSTM, CMINEP, CSAVEV, DTX,      !Input
     &    DXR57, ECONO, FILECC, FILEGC, FNINL, FNINR,     !Input
     &    FNINS, KCAN, NAVL, NDMNEW, NDMOLD,              !Input
     &    NFIXN, NMINEA, NR1, PAR, PCH2O, PG, PGAVL,      !Input
     &    PStres2, ROWSPC, RVSTGE, STMWT, TGRO,           !Input
     &    TRNU, TURFAC, VSTAGE, WCRLF, WCRRT, WCRSH,      !Input
     &    WCRST, WTLF, XLAI, YRDOY, YREMRG,               !Input
     &    AGRVG, FRLF, FRRT, FRSTM,                       !I/O
     &    CADLF, CADST, CANHT, CANWH, CMINEA, CRUSLF,     !Output
     &    CRUSRT, CRUSSH, CRUSST, EXCESS, NADLF, NADRT,   !Output
     &    NADST, NGRLF, NGRRT, NGRST, NSTRES,             !Output
     &    TNLEAK, WLDOTN, WRDOTN, WSDOTN)                 !Output

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE, CANOPY
      SAVE

      CHARACTER*6  ERRKEY
      PARAMETER   (ERRKEY = 'VEGGR')

      CHARACTER*6  ECONO, SECTION
      CHARACTER*80 C80
      CHARACTER*92 FILECC, FILEGC

      INTEGER DYNAMIC
      INTEGER YRDOY, YREMRG, NR1, DAS
      INTEGER I, LUNCRP, ERR, LINC, LNUM, ISECT, FOUND

      REAL AGRLF, AGRRT, AGRSTM, CMINEP, CMOBMX
      REAL DTX, DXR57, FNINL, FNINR, FNINS, KCAN
      REAL NAVL, NDMNEW, NDMOLD, PAR, PCH2O, PG
      REAL PROLFI, PRORTI, PROSTI, ROWSPC
      REAL RVSTGE, STMWT, TURFAC, WCRLF, WCRRT, WCRSH, WCRST
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
      
!     FO - Cotton-Nitrogen
      REAL NSTFAC, PNSTRES, XNSTRES

!     P module
      REAL PStres2

      TYPE (ControlType) CONTROL
      CALL GET(CONTROL)
      DAS = CONTROL % DAS

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
      LNUM = 0
!-----------------------------------------------------------------------
!    Find and Read Plant Composition Section
!-----------------------------------------------------------------------
!     Subroutine FIND finds appropriate SECTION in a file by
!     searching for the specified 6-character string at beginning
!     of each line.
!-----------------------------------------------------------------------
      SECTION = '!*PLAN'
      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(2F6.0,6X,2F6.0)',IOSTAT=ERR)
     &          PROLFI, PROLFG, PROSTI, PROSTG
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(2F6.0)',IOSTAT=ERR) PRORTI, PRORTG
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF
!-----------------------------------------------------------------------
!    Find and Read Plant Composition Section
!-----------------------------------------------------------------------
      SECTION = '!*CARB'
      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(2F6.0)',IOSTAT=ERR) CMOBMX, CADSTF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(6X,F6.0)',IOSTAT=ERR) NSTFAC
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF
!-----------------------------------------------------------------------
!    Find and Read Partitioning Section
!-----------------------------------------------------------------------
      SECTION = '!*VEGE'
      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
          DO I=1,4
            ISECT = 2
            DO WHILE (ISECT .NE. 1)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            ENDDO
          ENDDO
        READ(C80,'(24X,F6.0)',IOSTAT=ERR) ATOP
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF
      
      CLOSE (LUNCRP)

!-----------------------------------------------------------------------
!    Call CANOPY for input
!-----------------------------------------------------------------------
      CALL CANOPY(RUNINIT,
     &    ECONO, FILECC, FILEGC, KCAN, PAR, ROWSPC,       !Input
     &    RVSTGE, TGRO, TURFAC, VSTAGE, XLAI, NSTRES,     !Input
     &    CANHT, CANWH)                                   !Output

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
!     FO/KJB - Running average      
      PNSTRES= 1.0
      XNSTRES= 1.0
      
      CALL CANOPY(SEASINIT,
     &    ECONO, FILECC, FILEGC, KCAN, PAR, ROWSPC,       !Input
     &    RVSTGE, TGRO, TURFAC, VSTAGE, XLAI, NSTRES,     !Input
     &    CANHT, CANWH)                                   !Output

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
      CUMTUR = 1.0             

      CALL CANOPY(EMERG,
     &    ECONO, FILECC, FILEGC, KCAN, PAR, ROWSPC,       !Input
     &    RVSTGE, TGRO, TURFAC, VSTAGE, XLAI, NSTRES,     !Input
     &    CANHT, CANWH)                                   !Output

!***********************************************************************
!***********************************************************************
!     DAILY RATE/INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
C     Partitioning is modified by water stress and nitrogen stress
C-----------------------------------------------------------------------
      SUPPN = NFIXN + TRNU + NMINEA
! KJB - Replacing a hardwire 0.70 for N-Stress      
      NSTFAC = MIN(NSTFAC,1.0) 
      NSTFAC = MAX(NSTFAC,0.1)
!    chp added check for YRDOY = YREMRG, but on the next day, it still
!     shows N stress because there is little supply.  Force a lag time?
!      IF (SUPPN .LT. 0.70 * NDMNEW .AND. NDMNEW .GT. 0.) THEN
!     FO/KJB - Running average
      PNSTRES = XNSTRES

      IF (SUPPN .LT. NSTFAC * NDMNEW .AND. NDMNEW .GT. 0. .AND. 
     &        YRDOY .NE. YREMRG) THEN
        XNSTRES = MIN(1.0,SUPPN/(NDMNEW * NSTFAC))
      ELSE
        XNSTRES = 1.0
      ENDIF
      
!     FO/KJB - Running average
      NSTRES = XNSTRES * 0.5 + PNSTRES * 0.5
      
!      FRRT  = ATOP * (1.0 - (MIN(TURFAC,NSTRES)))*(1.0-FRRT) + FRRT
      FRRT  = ATOP * (1.0 - (MIN(TURFAC, NSTRES, PStres2))) * 
     &                    (1.0 - FRRT) + FRRT
C-----------------------------------------------------------------------
C     Cumulative turgor factor that remembers veg drought stress
C     to shift partitioning between leaf and stem toward leaf,
C     especially after drought is released.
C     Sort of 20-day rolling average
C-----------------------------------------------------------------------
      CUMTUR = 0.95*CUMTUR + 0.05*TURFAC
      IF (CUMTUR < 1.E-7) CUMTUR = 0.0    !prevent underflow
C-----------------------------------------------------------------------
C     0.6 IS A SCALAR, COULD BE LESS, was once 0.8 and 0.7
C     0.7 appears to be too much for peanut, but not for soybean.
C-----------------------------------------------------------------------
      FRLF  = (1.0 + 0.6*(1.0-CUMTUR))*(1.-FRRT)*FRLF/(FRLF + FRSTM)
      FRLF = MIN(FRLF, 0.90*(1. - FRRT))
      FRSTM = 1.0 - FRRT - FRLF
C-----------------------------------------------------------------------
C     To prevent negative partitioning to root and limit leaf plus
C     stem to a maximum of 98 % of the vegetative partitioning
C-----------------------------------------------------------------------
      FRLF  = MIN(FRLF,FRLF*0.98/(MAX(0.001,FRLF+FRSTM)))
      FRSTM = MIN(FRSTM,FRSTM*0.98/(MAX(0.001,FRLF+FRSTM)))
      FRRT  = 1.0 - FRLF - FRSTM
C-----------------------------------------------------------------------
C     Calculate weighted PHI + GR = 1/E = AGRVG for veg. growth
C-----------------------------------------------------------------------
      AGRVG = AGRLF * FRLF + AGRRT * FRRT + AGRSTM * FRSTM
C-----------------------------------------------------------------------
C     Calculate New Growth Rate of Leaves, Stems, and Roots
C-----------------------------------------------------------------------
      VGRDEM = PGAVL / AGRVG
      WLDOTN = FRLF * VGRDEM
      WSDOTN = FRSTM * VGRDEM
      WRDOTN = FRRT * VGRDEM
C-----------------------------------------------------------------------
C     Compute maximum N required for tissue growth
C-----------------------------------------------------------------------
      NGRLF  = WLDOTN * FNINL
      NGRST  = WSDOTN * FNINS
      NGRRT  = WRDOTN * FNINR
      NGRVEG = NGRLF + NGRST + NGRRT
C-----------------------------------------------------------------------
C     Compute minimum N required for tissue growth
C-----------------------------------------------------------------------
      NGRLFG = WLDOTN * FNINLG
      NGRSTG = WSDOTN * FNINSG
      NGRRTG = WRDOTN * FNINRG
      NGRVGG = NGRLFG + NGRSTG + NGRRTG

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
            NGRLF  = NGRLFG * NRATIO
            NGRST  = NGRSTG * NRATIO
            NGRRT  = NGRRTG * NRATIO

C-----------------------------------------------------------------------
C     Adjust conversion costs to account for composition of tissue at
C       lower N concentration
C-----------------------------------------------------------------------
            AGRVG = AGRLF * FRLF * (1.0 - (PROLFG - PROLFI)/(1.0 -
     &       PROLFI))+ AGRRT * FRRT * (1.0 - (PRORTG - PRORTI)/(1.0 -
     &       PRORTI)) + AGRSTM * FRSTM * (1.0 - (PROSTG - PROSTI)/
     &       (1.0 - PROSTI))
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
C-----------------------------------------------------------------------
C     Recompute respiration costs if expansion occurs at low N-conc.,
C       allow N dilution during growth of leaves, stems, and roots
C-----------------------------------------------------------------------
         AGRVG = AGRLF * FRLF * (1.0 - (PROLFT - PROLFI)/
     &        (1.0-PROLFI)) + AGRRT * FRRT * (1.0 - (PRORTT - PRORTI)/
     &        (1.0 - PRORTI)) + AGRSTM * FRSTM * (1.0 - (PROSTT -
     &        PROSTI)/(1.0 - PROSTI))
      ENDIF
C-----------------------------------------------------------------------
C     Compute C and N remaining to add to reserves
C-----------------------------------------------------------------------
      PGLEFT = MAX(0.0,PGAVL - ((WLDOTN + WSDOTN + WRDOTN) * AGRVG))
      IF (PGLEFT < 1.E-5) PGLEFT = 0.0
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

      CADST = 0.0
      CADLF = 0.0
      CMINEA = 0.0
      CRUSLF = 0.0
      CRUSST = 0.0
      CRUSRT = 0.0
      CRUSSH = 0.0
C-----------------------------------------------------------------------
C    Calculate Increase in Remobilizable C due to N shortage and
C      add to Carbon Pool.  Distribute to Leaves and Stems.
C-----------------------------------------------------------------------
C    Want half as much accumulation in stem in veg phae
C-----------------------------------------------------------------------
      IF (DAS .LT. NR1) THEN
         LSTR = (1.-0.6*CADSTF)/(0.6*CADSTF)
      ELSE
         LSTR = (1.-CADSTF)/CADSTF
      ENDIF
      IF (STMWT+WTLF .GT. 0.0) THEN
         LSTR = LSTR * WTLF/(STMWT+WTLF*LSTR)
      ENDIF
      IF (PGLEFT .GE. CMINEP) THEN
        CADLF = (PGLEFT-CMINEP)/PCH2O * LSTR
        CADST = (PGLEFT-CMINEP) * (1. - LSTR) / PCH2O
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
        IF (CMINEP .GT. 0) THEN
          CMINEA = CMINEP - PGLEFT
          CRUSLF = CMINEA / CMINEP * CMOBMX * WCRLF * (DTX + DXR57)
          CRUSST = CMINEA / CMINEP * CMOBMX * WCRST * (DTX + DXR57)
          CRUSRT = CMINEA / CMINEP * CMOBMX * WCRRT * (DTX + DXR57)
          CRUSSH = CMINEA / CMINEP * CMOBMX * WCRSH * (DTX + DXR57)
        ENDIF
      ENDIF
      CADLF = CADLF + CSAVEV/PCH2O * LSTR
      CADST = CADST + CSAVEV * (1. - LSTR)/PCH2O

C-----------------------------------------------------------------------
C    Calculate Increase in Remobilizable N Due to a C shortage,
C      add to Nitrogen pool
C-----------------------------------------------------------------------
      NLEFT  = MAX(0.0,NAVL  -  (NGRLF  + NGRST  + NGRRT))
      IF (NLEFT .GT. 0.0) THEN
         IF (NLEFT .GT. NDMOLD) THEN
            NLEAK  = NLEFT  - NDMOLD
            TNLEAK = TNLEAK + NLEAK
            NLEFT  = NLEFT  - NLEAK
         ELSE
            NLEAK = 0.0
         ENDIF
         NADRAT = NLEFT / (FRLF*FNINL+FRSTM*FNINS+FRRT*FNINR)
         NADLF  = NADRAT * FRLF * FNINL
         NADST  = NADRAT * FRSTM * FNINS
         NADRT  = NADRAT * FRRT * FNINR
      ELSE
         NADRAT = 0.0
         NADST  = 0.0
         NADLF  = 0.0
         NADRT  = 0.0
      ENDIF

C-----------------------------------------------------------------------
C     Subroutine CANOPY calculates height and width of the canopy as a
C     function of VSTAGE, air temperature, drought stress (TURFAC),
C     daylenght and radiation (PAR).
C-----------------------------------------------------------------------
      CALL CANOPY(INTEGR,
     &    ECONO, FILECC, FILEGC, KCAN, PAR, ROWSPC,       !Input
     &    RVSTGE, TGRO, TURFAC, VSTAGE, XLAI, NSTRES,     !Input
     &    CANHT, CANWH)                                   !Output

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
C-----------------------------------------------------------------------
      END ! SUBROUTINE VEGGR
C-----------------------------------------------------------------------
! AGRLF   Mass of CH2O required for new leaf growth (g[CH2O] / g[leaf])
! AGRRT   Mass of CH2O required for new root growth (g[CH2O] / g[root])
! AGRSTM  Mass of CH2O required for new stem growth (g[CH2O] / g[stem])
! AGRVG   Mass of CH2O required for vegetative tissue growth including 
!           stoichiometry and respiration (g[CH2O] / g[tissue])
! ATOP    Maximum fraction change in partitioning from top growth to roots 
!           if severe water or nitrogen stresses occur. 
! CADLF   Mass of CH2O added to leaf reserves after growth
!           (g[CH2O] / m2 / d)
! CADST   Mass of CH2O added to stems (g[CH2O] / m2 / d)
! CADSTF  Proportion of CH2O reserves that are added to stems (fraction)
! CANHT   Canopy height (m)
! CANWH   Canopy width normal to row (m)
! CMINEA  Actual carbon mined from vegetative tissue (g[CH2O] / m2 / d)
! CMINEP  Potential CH2O mobilization from storage (g[CH2O] / m2 / d)
! CMOBMX  Maximum C pool mobilization rate (g[CH2O] / m2 / d)
! CRUSLF  C mobilized from leaf tissue in a day (g[CH2O] / m2 / d)
! CRUSRT  C mobilized from root tissue in a day (g[CH2O] / m2 / d)
! CRUSSH  C mobilized from shell tissue in a day (g[CH2O] / m2 / d)
! CRUSST  C mobilized from stem tissue in a day (g[CH2O] / m2 / d)
! CSAVEV  Fraction of PG for VEG that is stored as CH2O 
! CUMTUR  Cumulative turgor factor - 20 day water stress average 
! DAS     Days after start of simulation (days)
! DTX     Thermal time that occurs in a real day based on vegetative 
!           development temperature function (thermal days / day)
! DXR57   Relative time between first seed (NR5) and physiological maturity 
!           (NR7) 
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
! FOUND   Indicator that good data was read from file by subroutine FIND (0 
!           - End-of-file encountered, 1 - NAME was found) 
! FRLF    Fraction of vegetative tissue growth that goes to leaves on a day
!           (g[leaf] / g[veg])
! FRRT    Fraction of vegetative tissue growth that goes to roots on a day
!           (g[root] / g[veg])
! FRSTM   Fraction of vegetative tissue growth that goes to stems on a day
!           (g[stem] / g[veg])
! ISECT   Data record code (0 - End of file encountered, 1 - Found a good 
!           line to read, 2 - End of Section in file encountered, denoted 
!           by * in column 1
! LNUM    Current line number of input file 
! LSTR    Ratio of excess C to be added to leaves in a day relative to the 
!           amount to be stored in stems 
! LUNCRP  Logical unit number for FILEC (*.spe file) 
! NADLF   N added to leaf N reserves (g[N] / m2 / d)
! NADRAT  Total nitrogen added to vegetative N reserves (g[N] / m2 / d)
! NADRT   N added to root N reserves (g[N] / m2 / d)
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
! NGRST   Maximum N demand for stem growth (g[stem N] / m2[ground] / d)
! NGRSTG  Minimum N requirement for stem growth
!           (g[leaf N] / m2[ground] / d)
! NGRVEG  Maximum N demand for vegetative tissue growth
!           (g[leaf N] / m2[ground] / d)
! NGRVGG  Minimum N requirement for vegetative tissue growth
!           (g[leaf N] / m2[ground] / d)
! NLEAK   Nitrogen leak (g[N] / m2 / d)
! NLEFT   Nitrogen left after vegetative demands are met (g[N] / m2 / d)
! NMINEA  Actual Nitrogen mined from existing tissue (g[N] / m2 / d)
! NR1     Day when 50% of plants have at least one flower (days)
! NRATIO  Factor to reduce tissue growth based on low available nitrogen 
! NSTRES  Nitrogen stress factor (1=no stress, 0=max stress) 
! PAR     Daily photosynthetically active radiation or photon flux density
!           (moles[quanta]/m2-d)
! PCH2O   Respiration loss due to storage/mobilization of CH2O
!           (g[CH2O] / g[CH2O])
! PG      Daily gross photosynthesis (g[CH2O] / m2 / d)
! PGAVL   Total available CH2O available for growth & respiration
!           (g[CH2O] / m2)
! PGLEFT  Excess PG after today's tissue growth (g[CH2O] / m2)
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
! PROSTG  Normal growth protein composition in stems during growth
!           (g[protein] / g[stem])
! PROSTI  Maximum protein composition in stems during growth with luxurious 
!           supply of N (g[protein] / g[stem])
! PROSTT  Protein fraction of new root growth (g[protein] / g[stem])
! ROWSPC  Row spacing (m)
! RVSTGE  Rate of VSTAGE change (nodes/day)
! SECTION Section name in input file 
! STMWT   Dry mass of stem tissue, including C and N (g[stem] / m2[ground)
! SUPPN   Total supply of N (g[N] / m2 / d)
! TGRO(I) Hourly air temperature (°C)
! TIMDIF  Integer function which calculates the number of days between two 
!           Julian dates (da)
! TNLEAK  Total nitrogen leak (g[N] / m2 / d)
! TRNU    Total N uptake in a day (g[N] / m2 / d)
! TS      Number of intermediate time steps (=24) 
! TURFAC  Water stress factor for expansion (0 - 1) 
! VGRDEM  Vegetative growth demand (g[vegetative tissue] / m2-d)
! VSTAGE  Number of nodes on main stem of plant 
! WCRLF   Mass of CH2O reserves in leaves (g[leaf CH2O] / m2[ground])
! WCRRT   Mass of CH2O reserves in roots (g[root CH2O] / m2[ground])
! WCRSH   Mass of CH2O reserves in shells (g[shell CH2O] / m2[ground])
! WCRST   Mass of CH2O reserves in stems (g[stem CH2O] / m2[ground])
! WLDOTN  Dry weight growth rate of new leaf tissue including N but not C 
!           reserves (g[leaf] / m2[ground]-d)
! WRDOTN  Dry weight growth rate of new root tissue including N but not C 
!           reserves (g[root] / m2[ground]-d)
! WSDOTN  Dry weight growth rate of new stem tissue including N but not C 
!           reserves (g[stem] / m2[ground]-d)
! WTLF    Dry mass of leaf tissue including C and N (g[leaf] / m2[ground])
! XLAI    Leaf area (one side) per unit of ground area
!           (m2[leaf] / m2[ground])
! YRDOY   Current day of simulation (YYDDD)
! YRSIM   Start of simulation date (YYDDD)
!-----------------------------------------------------------------------
!     END SUBROUTINE VEGGR
!-----------------------------------------------------------------------

