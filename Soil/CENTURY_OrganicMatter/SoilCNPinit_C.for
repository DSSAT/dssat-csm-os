!***********************************************************************
!  SoilCNPinit_C, Subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: Do soil N initialization.
!
!  REVISION HISTORY
!  02/08/1993 PWW Header revision and minor changes.
!  02/20/1996 GH  Written.
!  06/21/1999 CHP Modular format.
!  01/01/2000 CHP/AJG Integrated the CENTURY-based and CERES-based SOM
!                modules with CHP's modular structure.
!  08/12/2003 CHP Added I/O error checking
!                 No re-initialization done for sequenced runs.
!  11/11/2002 AJG Added DLAYR to the PARTIT_C parameter string.
!  12/17/2003 AJG Renamed all the CH_ and CHEM_ variables to P_ variables.
!  01/15/2004 AJG Made new SOM P pool structure with SOM23
!  08/30/2004 AJG Corrected some layout irregularities.
!  02/22/2005 CHP Removed inorganic N processes.
!  04/13/2005 CHP Changed name to SOILCNP_C (was SOILNI_C)
!  04/24/2005 AJG Set the initial value of CES1, CES2, CES3.
!  04/30/2008 CHP Changed units for SCN, SCP, RCN, RCP to %
!
!  Called: CENTURY
!  Calls : ERROR, FIND, OPSOMLIT_C, PARTIT_C, SOMFIX_C, 
!          SOMLITPRINT_C, TSOMLIT_C
!***********************************************************************

      SUBROUTINE SoilCNPinit_C (CONTROL, ISWITCH, 
     &  N_ELEMS, SOILPROP,                                !Input
     &  ACCCO2, ACCMNR, ADDMETABEFLAG, AMINRL, CEDAM,     !Output
     &  CES1, CES1M, CES1T, CES1X, CES2, CES21I, CES21M,  !Output
     &  CES21S, CES21T, CES21X, CES23LM, CES23LX, CES23M, !Output
     &  CES23T, CES23X, CES2LI, CES2LM, CES2LS, CES2LX,   !Output
     &  CES3, CES3M, CES3T, CES3X, CESTR, CO2MET, CO2S1,  !Output
     &  CO2S2, CO2S3, CO2STR, CULMETQ, CULS1Q, CULS2Q,    !Output
     &  CULS3Q, CULSTRQ, DECMET, DECS1, DECS2, DECS3,     !Output
     &  DECSTR, DISTURBNUM, DISTURBEND, DISTURBDEPTH,     !Output
!    &  DLTLIGC, DLTMETABC, DLTMETABE, DLTSTRUCC,         !Output
!    &  DLTSTRUCE, DSNC, FRDAE, FRMETFLAG, FRMETI,        !Output
     &  DSNC, FRDAE, FRMETFLAG, FRMETI,                   !Output
     &  FRMETS,HARVRES, LIGC, LIGSTR, METABC, METABE,     !Output
     &  MULCH, RESDAX, S1S3, S2S3, SOM1C, SOM1E, SOM2C,   !Output
     &  SOM2E, SOM23C, SOM23E, SOM3C, SOM3E, SSOMC,       !Output
     &  STRUCC, STRUCE, TLITE, TXS1)                      !Output

!     ------------------------------------------------------------------
      USE ModuleDefs 
      USE Interface_IpSoil

      IMPLICIT NONE
      EXTERNAL ERROR, FIND, INCYD, PARTIT_C, SOMFIX_C, 
     &  SOMINIT_C, WARNING
      SAVE
!     ------------------------------------------------------------------
      LOGICAL ADDMETABEFLAG, FRMETFLAG

      CHARACTER*1  RNMODE 
      CHARACTER*2  PREV_CROP
      CHARACTER*6  ERRKEY, SECTION
      CHARACTER*30 FILEIO
      PARAMETER (ERRKEY = 'SOILNI')
      CHARACTER*78 MSG(10)

      INTEGER ERRNUM, FOUND, IEL, INCYD, L, LAYER, LNUM, MULTI,
     &  LUNIO, N_ELEMS, NLAYR, RUN, YRSIM
      INTEGER DISTURBNUM, DISTURBEND(NAPPL*3)
      INTEGER, PARAMETER :: SRFC = 0, SOIL = 1

      REAL DISTURBDEPTH(NAPPL*3)

      REAL CO2S1I, CO2S1S,
     &  CO2S2, CO2S3, CULMETQ, CULS1Q, CULS2Q, CULS3Q,
     &  CULSTRQ, DEPMAX, DEPTH, DSNC, FACTOR, FRMETI,
     &  FRMETS, HOLD
      REAL ICNOD, ICREN, ICREP, ICRES, ICRID, ICRIP
      REAL ICRT, ICRTN, ICRTP
      REAL ICREN_file, ICREP_file
      REAL PRLIG, RCN, RCP, SCN, SCP, RESDAX, RESDEPTH, AM,EXTFAC,WATFAC
      REAL RESSOL, RESSOLN, RESSOLP, RESSRF, RESSRFN, RESSRFP
      REAL S1S3I, S1S3S, S2S3I, S2S3S, !SOILCN, SOILCP,  
     &  TXS1I, TXS1S, WSUM

      REAL, DIMENSION(0:1) :: ACCCO2, CO2MET, DECMET, DECS1, DECSTR, 
     &    LIGSTR

      REAL, DIMENSION(0:NL) :: CO2S1, !DLTLIGC, DLTMETABC, DLTSTRUCC, 
     &    FRLRES, LIGC, METABC, RESC, SOM1C, SSOMC, STRUCC

      REAL, DIMENSION(1:NL) :: BD, DLAYR, DS, KG2PPM, S1S3, S2S3, 
     &    SOM2C, SOM23C, SOM3C, TXS1, WRN, NH4I, NO3I, OC

      REAL, DIMENSION(NELEM) :: CEDAM, CESTR, FRDAE, TLITE

      REAL, DIMENSION(0:0,NELEM) :: CES21I   
      REAL, DIMENSION(1:1,NELEM) :: CES21T, CES23T, CES3M, CES3T, CES3X

      REAL, DIMENSION(0:1,NELEM) :: CES1M, CES1T, CES1X, CES21M, 
     &    CES21S, CES21X, CES23LM, CES23LX, 
     &    CES23M, CES23X, CES2LI, CES2LM, CES2LS, CES2LX

      REAL, DIMENSION(0:NL,NELEM) :: ACCMNR, CES1, 
     &  IMMOB, METABE, RESCE, RESE, SOM1E, STRUCE 

      REAL, DIMENSION(1:NL,NELEM) :: AMINRL, CES2, CES23, CES3,  SOM2E, 
     &    SOM23E, SOM3E

      REAL DECS2(1), DECS3(1), CO2STR(0:1,2) 

!-----------------------------------------------------------------------
!     Constructed variables are defined in ModuleDefs.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (ResidueType) HARVRES
      TYPE (SoilType)    SOILPROP
      TYPE (MulchType)   MULCH

!     Transfer values from constructed data types into local variables.
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
      MULTI   = CONTROL % MULTI
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN
      YRSIM   = CONTROL % YRSIM

      BD     = SOILPROP % BD  
      DLAYR  = SOILPROP % DLAYR  
      DS     = SOILPROP % DS  
      KG2PPM = SOILPROP % KG2PPM  
      NLAYR  = SOILPROP % NLAYR 
      OC     = SOILPROP % OC 
      DEPMAX = SOILPROP % DS(NLAYR)    

!***********************************************************************
!     Done only for non-sequenced runs.
      IF (RUN == 1 .OR. INDEX('QF',RNMODE) <= 0) THEN
!       --------------------------------------------------------------
!       Read initial root and shoot residue from FILEIO
!       Open the FILEIO input file.
        OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT = ERRNUM)

!       If the file can't be found, call an error.
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, 0)

!       Find and Read INITIAL CONDITIONS Section
        SECTION = '*INITI'

!       Find the line number from where to start reading.
        CALL FIND (LUNIO, SECTION, LNUM, FOUND)

!       If the Initial Conditions section can't be found, call an
!       error, or else read the input data.
        IF (FOUND == 0) CALL ERROR (SECTION, 42, FILEIO, LNUM)

!       Read the weight of root residues and nodules from the
!       previous crop. 
        READ (LUNIO, '(3X,A2,11X, 2F6.0, 18X, 5F6.0)', IOSTAT = ERRNUM) 
     &    PREV_CROP, 
     &    ICRT, ICNOD, ICRES, ICREN, ICREP, ICRIP, ICRID
        LNUM = LNUM + 1
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)

        IF (ICRT < 0.) ICRT = 0.
        IF (ICNOD < 0.) ICNOD = 0.
        IF (ICRIP < 0.01) ICRIP = 0.
        IF (ICRID < 0.01) ICRID = 0.
        IF (ICRID < 0.01) ICRIP = 0.

!       Read initial inorganic N for initialization of organic matter
!       constituents.  
        DO L = 1, NLAYR
          LNUM = LNUM + 1
          READ(LUNIO, 100, IOSTAT=ERRNUM) NH4I(L),NO3I(L)
100       FORMAT (14X, 2 (1X, F5.1))
          IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)
        ENDDO
        CLOSE (LUNIO)

        IF (N_ELEMS > 0) THEN
          DO L=1,NLAYR
            AMINRL(L,N) = (NO3I(L) + NH4I(L)) / KG2PPM(L)
!           Don't yet know initial values of Pi
            AMINRL(L,P) = 0.0 !SPi_Labile(L)
          ENDDO 
        ENDIF  

!       ----------------------------------------------------------------
!       Read the fixed (site- and crop-independent) parameters for the 
!       CENTURY-based SOM/litter module.
!     ----------------------------------------------------------------
        CALL SOMFIX_C (CONTROL, 
     &    CEDAM, CES1, CES1M, CES1T, CES1X, CES2,         !Output
     &    CES21I, CES21M, CES21S, CES21T, CES21X, CES23,  !Output
     &    CES23LM, CES23LX,                               !Output
     &    CES23M, CES23T, CES23X, CES2LI,                 !Output
     &    CES2LM, CES2LS, CES2LX, CES3, CES3M, CES3T,     !Output
     &    CES3X, CESTR, CO2MET, CO2S1, CO2S1I, CO2S1S,    !Output
     &    CO2S2, CO2S3, CO2STR, CULMETQ, CULS1Q,          !Output
     &    CULS2Q, CULS3Q, CULSTRQ, DECMET, DECS1,         !Output
     &    DECS2, DECS3, DECSTR, FRDAE, FRMETI, FRMETS,    !Output
     &    LIGSTR, RESDAX, S1S3I, S1S3S, S2S3I, S2S3S,     !Output
     &    TXS1I, TXS1S)                                   !Output

!       ---------------------------------------------------------------
!       Initialize Fresh organic matter (residues)
!       ---------------------------------------------------------------
!       Find and Read Residue Lignin and Root C:N ratio and
!       integration depth.
!       ----------------------------------------------------------------
!       Get residue characteristics from RESCH???.SDA. 
        CALL IPSOIL (CONTROL, CROP=PREV_CROP,             !Input
     &    AM=AM, DSNC=DSNC, PRLIG = PRLIG, RCN = RCN,     !Output
     &    RCP = RCP, SCN = SCN, SCP = SCP, WATFAC=WATFAC, !Output
     &    EXTFAC=EXTFAC)                                  !Output

!       Lignin concentration of root residue.
        FRLRES = PRLIG

!       ----------------------------------------------------------------
!       Initialize fresh organic matter to zero. 
!       Not needed for the SOM pools, which get a
!       value in SOMINIT.
        DO L = 0, NLAYR   !With SRFC layer.
          METABC(L) = 0.
          STRUCC(L) = 0. 
          LIGC(L) = 0.
          DO IEL = 1, N_ELEMS
            METABE(L,IEL) = 0.
            STRUCE(L,IEL) = 0. 
          END DO 
        END DO

        TLITE = 0.

!       ****************************************************************
!       Distribute the initial shoot residue over soil and surface 
!       ----------------------------------------------------------------
        IF (ICRES > 1.E-3) THEN

!         Set shoot N content
          IF (N_ELEMS > 0) THEN
            ICREN_file = ICREN
            IF (ICREN > 1.E-6) THEN
!             Check C:N ratios, limit to within 20% of expected values 
!             from RESCH???.SDA
              ICREN = MAX(ICREN, 0.8 * SCN)
              ICREN = MIN(ICREN, 1.2 * SCN)
            ELSE
!             If no value specified, use ratio from RESCH???.SDA
              ICREN = SCN
            ENDIF
            IF (ABS(ICREN - ICREN_file) > 1.E-3 .AND. MULTI < 2) THEN
              MSG(1)="N content of residue was changed from " //
     &          "value in initial conditions"
              MSG(2)="  section of experiment file to maintain " //
     &          "allowable C:N ratio."
              WRITE(MSG(3),'(A,A)')
     &          "  - Previous crop code                 :    ",PREV_CROP
              WRITE(MSG(4),'(A,A,F6.1)') "  - Recommended N content ",
     &          "for surface residue (%):", SCN
              WRITE(MSG(5),'(A,15X,A,F6.3,A)')
     &          "  - Value read from file",":",ICREN_file,"%"
              WRITE(MSG(6),'(A,15X,A,F6.3,A)')
     &          "  - Value changed to    ",":",ICREN,"%"
              WRITE(MSG(7),'(A)')
     &          "To change allowable N content, modify SCN parameter "
              WRITE(MSG(8),'(A,A,A)')
     &          "  in RESCH", ModelVerTxt, ".SDA file."
              CALL WARNING(8, ERRKEY, MSG) 
            ENDIF
          ENDIF

!         Set shoot P content
          IF (N_ELEMS > 1) THEN
            ICREP_file = ICREP
            IF (ICREP > 1.E-6) THEN
!             Check C:P ratios, limit to within 20% of expected values 
!             from RESCH???.SDA
              ICREP = MAX(ICREP, 0.8 * SCP)
              ICREP = MIN(ICREP, 1.2 * SCP)
            ELSE
!             If no value specified, use ratio from RESCH???.SDA
              ICREP = SCP
            ENDIF
            IF (ABS(ICREP - ICREP_file) > 1.E-3 .AND. MULTI < 2) THEN
              MSG(1)="P content of residue was changed from " //
     &          "value in initial conditions"
              MSG(2)="  section of experiment file to maintain " //
     &          "allowable C:P ratio."
              WRITE(MSG(3),'(A,A)')
     &          "  - Previous crop code                 :    ",PREV_CROP
              WRITE(MSG(4),'(A,A,F6.1)') "  - Recommended P content ",
     &          "for surface residue (%):", SCP
              WRITE(MSG(5),'(A,15X,A,F6.3,A)')
     &          "  - Value read from file",":",ICREP_file,"%"
              WRITE(MSG(6),'(A,15X,A,F6.3,A)')
     &          "  - Value changed to    ",":",ICREP,"%"
              WRITE(MSG(7),'(A)')
     &          "To change allowable P content, modify SCP parameter "
              WRITE(MSG(8),'(A,A,A)')
     &          "  in RESCH", ModelVerTxt, ".SDA file."

              CALL WARNING(8, ERRKEY, MSG) 
            ENDIF
          ENDIF

          RESSOL = ICRES * ICRIP / 100.
          RESSOLN = ICREN / 100. * RESSOL
          RESSOLP = ICREP / 100. * RESSOL

          RESSRF = ICRES - RESSOL
          RESSRFN = ICREN / 100. * RESSRF   !kg[N]/ha
          RESSRFP = ICREP / 100. * RESSRF   !kg[P]/ha

        ELSE
          RESSRF = 0.0
          RESSOL = 0.0
          RESSRFN = 0.0
          RESSRFP = 0.0
        ENDIF   !End of IF block on ICRES > 1.E-3.

!       ---------------
!       Surface residue
!       ---------------
        IF (RESSRF > 1.E-3) THEN
!         Surface coverage for initial residue
          MULCH % MULCH_AM  = AM
          MULCH % MUL_EXTFAC= EXTFAC
          MULCH % MUL_WATFAC= WATFAC
          MULCH % MULCHMASS = RESSRF
          MULCH % MULCHN    = RESSRFN
          MULCH % MULCHP    = RESSRFP

          RESC(SRFC)   = RESSRF * 0.40
          RESE(SRFC,N) = RESSRFN
          RESE(SRFC,P) = RESSRFP
          IF (RESSRFN > 1.E-6) RESCE(SRFC,N) = RESC(SRFC) / RESSRFN
          IF (RESSRFP > 1.E-6) RESCE(SRFC,P) = RESC(SRFC) / RESSRFP

!         If L is transferred via the subroutine's parameter
!         string, it goes wrong, because L is the DO loop counter.
!         Therefore, L is first copied to LAYER.
          LAYER = 0

!         Distribute the newly added residues over the surface 
!         structural and metabolic residue pools.
          CALL PARTIT_C (
     &      AMINRL, CEDAM, CESTR, DLAYR, FRDAE,           !Input
     &      FRLRES, FRMETI, FRMETS, LAYER, N_ELEMS,       !Input
     &      RESC, RESCE, RESDAX,                          !Input
     &      LIGC, METABC, METABE, STRUCC,                 !Input/Output
     &      STRUCE, IMMOB, RESE,                          !Input/Output
     &      ADDMETABEFLAG, FRMETFLAG)                     !Output
        ENDIF   !End of IF block on RESSRF > 1.E-3.

!       ------------
!       Soil residue
!       ------------
!       Incorporate residue evenly throughout RESDEPTH
        IF (RESSOL .GT. 0.001) THEN 

!         Set the initial shoot residue incorporation depth.
          RESDEPTH = MAX(ICRID, DS(1))

!         Due to soil disturbance, the decomposition rate will speed up
!         for 30 days. Set the end date for the decomposition enhancement,
!         and the depth of the disturbed soil layers for which the
!         increased decomposition rate holds.
          DISTURBNUM = DISTURBNUM + 1
          DISTURBEND(DISTURBNUM) = INCYD (YRSIM, 30)
          DISTURBDEPTH(DISTURBNUM) = RESDEPTH

          DEPTH = 0.0
          DO L = 1, NLAYR
            HOLD  = DEPTH               !Depth to top of layer
            DEPTH = DEPTH + DLAYR(L)    !Depth to bottom of layer
            IF (RESDEPTH <= DEPTH) THEN
!             FACTOR = fraction of the residue added to this layer
              FACTOR = (RESDEPTH - HOLD) / RESDEPTH
            ELSE
              FACTOR = DLAYR(L) / RESDEPTH
            ENDIF   !End of IF block on RESDEPTH =< DEPTH.

            RESC(L) = RESSOL * FACTOR * 0.40
            RESE(L,N) = RESSOLN * FACTOR
            RESE(L,P) = RESSOLP * FACTOR
            IF (N_ELEMS > 0) RESCE(L,N) = RESSOL * 0.4 / RESSOLN
            IF (N_ELEMS > 1) RESCE(L,P) = RESSOL * 0.4 / RESSOLP

            IF (RESC(L) > 0.0) THEN
!             If L is transferred via the subroutine's parameter
!             string, it goes wrong, because L is the DO loop counter.
!             Therefore, L is first copied to LAYER.
              LAYER = L

!             Distribute the newly added residues over the soil 
!             structural and metabolic residue pools.
              CALL PARTIT_C (
     &          AMINRL, CEDAM, CESTR, DLAYR, FRDAE,       !Input
     &          FRLRES, FRMETI, FRMETS, LAYER, N_ELEMS,   !Input
     &          RESC, RESCE, RESDAX,                      !Input
     &          LIGC, METABC, METABE, STRUCC,             !Input/Output
     &          STRUCE, IMMOB, RESE,                      !Input/Output
     &          ADDMETABEFLAG, FRMETFLAG)                 !Output
            ENDIF

!           If there are no more soil layers over which to distribute
!           the residue, jump out of the DO loop. 
            IF (DEPTH >= RESDEPTH) EXIT
          ENDDO   !End of loop on soil layers.
        ENDIF   !End of IF block on RESSOL and RESDEPTH

!       ****************************************************************
!       Add the initial or carry-over root and nodule residues to the 
!       fresh-organic-matter pool.
!       ----------------------------------------------------------------
        IF (ICRT + ICNOD > 1.E-4) THEN
C         Calculate N and P contributions.  Assumes C is 40% of dry matter.
          IF (N_ELEMS > 0) ICRTN = ICRT * RCN / 100.
          IF (N_ELEMS > 1) ICRTP = ICRT * RCP / 100.

!         Root distribution for each layer (WRN) and through the soil
!         profile (WSUM).
          WSUM  = 0.0
          DEPTH = 0.0
          DO L = 1, NLAYR
            IF (L > 1) THEN
              DEPTH  = DS(L-1) + DLAYR(L) / 2.0
            ELSE
              DEPTH = DLAYR(L) / 2.0
            ENDIF
!03/19/2007 LAH/CHP exponential distribution should be applied to concentration, not
!         mass.  Need to take into account layer thickness and BD.
!WAS        WRN(L) = EXP (-3.0 * DEPTH / DEPMAX)
            WRN(L) = EXP (-3.0 * DEPTH / DEPMAX) * BD(L) * DLAYR(L)
            WSUM   = WSUM + WRN(L)
          END DO

          DO L = 1, NLAYR
            IF (WSUM > 0.0) THEN
              FACTOR = WRN(L) / WSUM
            ELSE
              FACTOR = 0.0
            ENDIF

            RESC(L) = (ICRT + ICNOD) * 0.40 * FACTOR
!           CHP 8/1/2008 subtract fresh organic matter from OC to get SOMC
            SSOMC(L) = OC(L) / KG2PPM(L) * 1.E4 - RESC(L) !kg/ha
            SSOMC(L) = MAX(0.0, SSOMC(L))
            IF (RESC(L) > 0.0) THEN
              RESE(L,N) = (ICRTN + 0.3  / 6.25 * ICNOD) * FACTOR
              RESE(L,P) = (ICRTP + 0.03 / 6.25 * ICNOD) * FACTOR
              IF (N_ELEMS > 0) RESCE(L,N) = RESC(L) / RESE(L,N)
              IF (N_ELEMS > 1) RESCE(L,P) = RESC(L) / RESE(L,P)

!             If L is transferred via the subroutine's parameter
!             string, it goes wrong, because L is the DO loop counter.
!             Therefore, L is first copied to LAYER.
              LAYER = L

!             Distribute the newly added residues over the soil 
!             structural and metabolic residue pools.
              CALL PARTIT_C (
     &          AMINRL, CEDAM, CESTR, DLAYR, FRDAE,       !Input
     &          FRLRES, FRMETI, FRMETS, LAYER, N_ELEMS,   !Input
     &          RESC, RESCE, RESDAX,                      !Input
     &          LIGC, METABC, METABE, STRUCC,             !Input/Output
     &          STRUCE, IMMOB, RESE,                      !Input/Output
     &          ADDMETABEFLAG, FRMETFLAG)                 !Output
            ENDIF
          ENDDO
        ELSE
          DO L = 1, NLAYR
            SSOMC(L) = OC(L) / KG2PPM(L) * 1.E4
            SSOMC(L) = MAX(0.0, SSOMC(L))
          ENDDO
        ENDIF

!       ---------------------------------------------------------------
!       Initialize SOM
!       ---------------------------------------------------------------
!       Set the initial value of CES1, CES2, CES3 the same for every
!       layer instead of using CES1(SOIL,N) etc. Take a backward DO loop, so
!       that it doesn't overwrite CES1(SOIL,N) by CES1(1,N).
        DO L = NLAYR, 0, -1
          DO IEL = 1, N_ELEMS
            IF (L == SRFC) THEN
              CES1(SRFC,IEL) = CES1(SRFC,IEL)
            ELSE
              CES1(L,IEL) = CES1(SOIL,IEL)
              CES2(L,IEL) = CES2(SOIL,IEL)
              CES3(L,IEL) = CES3(SOIL,IEL)
              IF (IEL == P) CES23(L,P) = CES23(SOIL,P)
            ENDIF   !End of IF block on L==SRFC.
          ENDDO   !End of DO loop on IEL.
        ENDDO   !End of DO loop on L.

!       Calculate the initial SOM pools.
!       CHP 8/1/2008 Include RESSOL in total organic C measurements.
        CALL SOMINIT_C (CONTROL, ISWITCH,
     &  CES1, CES1M, CES1X, CES2, CES21M, CES21X, CES23M, !Input
     &  CES23X, CES3, CES3M, CES3X, CO2S1I, CO2S1S,       !Input
     &  N_ELEMS, S1S3I, S1S3S, S2S3I, S2S3S, SOILPROP,    !Input
     &  SSOMC, TXS1I, TXS1S,                              !Input
     &  ACCCO2, ACCMNR, CO2S1, S1S3, S2S3, SOM1C,         !Output
     &  SOM1E, SOM2C, SOM2E, SOM23C, SOM23E, SOM3C,       !Output
     &  SOM3E, TXS1)                                      !Output

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!     Sequence runs: Carry-over root residue C and C/E ratio.
      ELSEIF (INDEX ('QF',RNMODE) > 0) THEN
        DO L = 0, NLAYR
          RESC(L) = RESC(L) + 0.4 * HARVRES % RESWT(L)

          DO IEL = 1, N_ELEMS
!chp 7/1/2005 RESE(L,IEL) = RESE(L,IEL) + HARVRES % RESE(L,IEL)
            RESE(L,IEL) = HARVRES % RESE(L,IEL)
            IF (RESE(L,IEL) > 0.) THEN
              RESCE(L,IEL) = RESC(L) / RESE(L,IEL)
            ELSE
              SELECT CASE(IEL)
              CASE(N); RESCE(L,N) = 10.0
              CASE(P); RESCE(L,P) = 100.
              END SELECT
            ENDIF
          ENDDO

!         Lignin concentration of root residue.
          IF (HARVRES % ResWT(L) > 0) THEN
            FRLRES(L) = HARVRES % ResLig(L) / HARVRES % ResWT(L)
          ELSE
            FRLRES(L) = 0.0
          ENDIF
        ENDDO
!!      ENDIF   !End of IF block on RUN == 1

!-----------------------------------------------------------------------

        DO L = 0, NLAYR
!           Since L is the DO loop counter, it goes wrong if L is transferred
!           to a subroutine's parameter string, so L is first copied to LAYER.
          LAYER = L

          IF (RESC(L) > 0.001) THEN
            CALL PARTIT_C (
     &        AMINRL, CEDAM, CESTR, DLAYR, FRDAE,         !Input
     &        FRLRES, FRMETI, FRMETS, LAYER, N_ELEMS,     !Input
     &        RESC, RESCE, RESDAX,                        !Input
     &        LIGC, METABC, METABE, STRUCC,               !Input/Output
     &        STRUCE, IMMOB, RESE,                        !Input/Output
     &        ADDMETABEFLAG, FRMETFLAG)                   !Output
          ENDIF   !End of IF block on RESC(L) > 0.001.
        ENDDO   !End of soil layer loop.
      ENDIF   !End of IF block on RUN == 1

!***********************************************************************
      RETURN
      END Subroutine SoilCNPinit_C
