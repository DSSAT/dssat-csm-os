C***********************************************************************
C  SoilOrg_init, Subroutine
C
C  Purpose: Do soil N initialization.
C  This was modified from SOILNI when NTRANS was split into organic and 
C  inorganic sections.  SOILNI was split into SOILNI_inorganic and 
C  SOILNI_organic.
C-----------------------------------------------------------------------
C  REVISION HISTORY 
C  02/08/1993 PWW Header revision and minor changes.
C  02/20/1996 GH  Written.
C  02/26/1998 WTB Fixed HUMC/HUMN calculations.
C  06/09/1999 AJG Completely revised the soil N and SOM module, and made
C               a new SOM module based on the CENTURY model.
C               Also changed variable names:
C         OLD         NEW       
C         -------     -------   
C         HUM         HUMC            
C         NHUM        HUMN
C         RNKG        ICRTN, TRTRESN
C         TFY         TFNITY          
C         WRESR       ICRT, TRTRES
C  06/21/1999 CHP Modular format
C  03/16/2000 GH  Incorporated in CROPGRO
C  06/11/2002 GH  Modified for Y2K
C  08/12/2003 CHP Added I/O error checking
C                 No re-initialization done for sequenced runs
C  02/25/2005 CHP Split SOILNI into SOILNI_inorg and SOILNI_organic
!  03/01/2005 CHP Changed variable names to match Century:
!                 HUMC to SSOMC     HUMN to SSOME
!                 THUMC to TSOMC    THUMN to TSOME
!                 TFON to TLITE     TLITC to TLITC
!  09/11/2006 CHP Use total organic N instead of TOTN to initialize
!  04/30/2008 CHP Changed units for SCN, SCP, RCN, RCP to %
C-----------------------------------------------------------------------
C  Called : SOIL_ORG
C  Calls  : ERROR, FIND
C=======================================================================

      SUBROUTINE SoilOrg_init(CONTROL, 
     &    HARVRES, PREV_CROP, SOILPROP,                   !Input
     &    CNRAT, FOM, FON, FOP, FPOOL, SSOMC,             !Output
     &    SSOME, MULCH)                                   !Output
      
!-----------------------------------------------------------------------
      USE ModuleDefs
      USE Interface_IpSoil

      IMPLICIT  NONE
      EXTERNAL ERROR, FIND, WARNING, MULCHLAYER
      SAVE

      CHARACTER*1 RNMODE 
      CHARACTER*2 CROP, PREV_CROP
      CHARACTER*6 ERRKEY, SECTION
      CHARACTER*17 SOILLAYERTYPE(NL)
      CHARACTER*30 FILEIO
      CHARACTER*78 MSG(8)
      PARAMETER (ERRKEY = 'SOLORG')

      INTEGER ERRNUM, FOUND, L, LNUM
      INTEGER LUNIO, N_ELEMS, NLAYR, MULTI
      INTEGER RUN
      INTEGER, PARAMETER :: SRFC = 0

      REAL AM, DEPMAX, DEPTH, EXTFAC, FACTOR, HOLD
      REAL ICNOD, ICRT, ICRTN, ICRTP
      REAL ICRES, ICREN, ICREP, ICRIP, ICRID
      REAL ICREN_file, ICREP_file
      REAL PRCEL, PRCHO, PRLIG, RCN, RCP, SCN, SCP, WSUM
      REAL RESSOL, RESSRF, RESSOLN, RESSRFN, RESSOLP, RESSRFP
      REAL RESDEPTH, WATFAC

      REAL, DIMENSION(NL) :: BD, CNRAT, DLAYR, DS, FOM, FON, FOP
      REAL FPOOL(NL,3), KG2PPM(NL), OC(NL), ORGP(NL), pH(NL)
      REAL SSOMC(0:NL), SSOME(0:NL,NELEM), WRN(NL)
      REAL TotOrgN(NL)

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     Constructed variables are defined in ModuleDefs.
      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP
      Type (ResidueType) HARVRES
      Type (MulchType)   MULCH

!     Transfer values from constructed data types into local variables.
      CROP    = CONTROL % CROP
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
      MULTI   = CONTROL % MULTI
      N_ELEMS = CONTROL % N_ELEMS
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN

      BD     = SOILPROP % BD     
      DLAYR  = SOILPROP % DLAYR  
      DS     = SOILPROP % DS     
      KG2PPM = SOILPROP % KG2PPM
      NLAYR  = SOILPROP % NLAYR  
      DEPMAX = SOILPROP % DS(NLAYR)    
      OC     = SOILPROP % OC     
      pH     = SOILPROP % pH     
      ORGP   = SOILPROP % ORGP    
      TotOrgN= SOILPROP % TotOrgN    !kg/ha
      SOILLAYERTYPE = SOILPROP % SOILLAYERTYPE    

!***********************************************************************
!     --------------------------------------------------------------
!     Non-sequenced runs:
      IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN
!       --------------------------------------------------------------
!       Initialize
        FOM = 0.0
        FON = 0.0
        FOP = 0.0
        FPOOL = 0.0
        SSOMC = 0.0
        SSOME = 0.0

!       Open the FILEIO input file.
        OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT = ERRNUM)
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, 0)
!       --------------------------------------------------------------
!       Find and Read INITIAL CONDITIONS Section.
!       Read root residue from previous crop and initial soil N values
!       --------------------------------------------------------------
        SECTION = '*INITI'
        CALL FIND (LUNIO, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) CALL ERROR (SECTION, 42, FILEIO, LNUM)

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

        CLOSE (LUNIO)

!       --------------------------------------------------------------
!       Residue from previous crop
        CALL IPSOIL (CONTROL, CROP=PREV_CROP,             !Input
     &    PRCEL=PRCEL, PRCHO=PRCHO, PRLIG=PRLIG,          !Output 
     &    RCN=RCN, RCP=RCP, SCN=SCN, SCP=SCP,             !Output
     &    AM=AM, EXTFAC=EXTFAC, WATFAC=WATFAC)            !Output 

!       ****************************************************************
!       Distribute the initial shoot residue over soil and surface 
!       ----------------------------------------------------------------
        IF (ICRES > 1.E-3) THEN

!         Set shoot N content
          IF (N_ELEMS > 0) THEN
            ICREN_file = ICREN
!     chp 5/13/08 temporarily remove -- this happens for a majority of files!
!            IF (ICREN > 1.E-6) THEN
!!             Check C:N ratios, limit to within 20% of expected values 
!!             from RESCH???.SDA
!              ICREN = MAX(ICREN, 0.8 * SCN)
!              ICREN = MIN(ICREN, 1.2 * SCN)
!            ELSE
!!             If no value specified, use ratio from RESCH???.SDA
!              ICREN = 40. / SCN
!            ENDIF
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
!     chp 5/13/08 temporarily remove -- this happens for a majority of files!
!            IF (ICREP > 1.E-6) THEN
!!             Check C:P ratios, limit to within 20% of expected values 
!!             from RESCH???.SDA
!              ICREP = MAX(ICREP, 0.8 * SCP)
!              ICREP = MIN(ICREP, 1.2 * SCP)
!            ELSE
!!             If no value specified, use ratio from RESCH???.SDA
!              ICREP = 40. / SCP
!            ENDIF
            IF (ABS(ICREP - ICREP_file) > 1.E-3 .AND. MULTI < 2) THEN
              MSG(1)="P content of residue was changed from " //
     &          "value in initial conditions"
              MSG(2)="  section of experiment file to maintain " //
     &          "allowable C:P ratio."
              WRITE(MSG(3),'(A,A)')
     &          "  - Previous crop code                 :    ",PREV_CROP
              WRITE(MSG(4),'(A,A,F6.1)') "  - Recommended P content ",
     &          "for surface residue:", SCP
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
!       Store surface residues in MULCH variable
        MULCH % MULCHMASS = RESSRF
        MULCH % MULCHN    = RESSRFN
        MULCH % MULCHP    = RESSRFP
        MULCH % MULCH_AM  = AM
        MULCH % MUL_EXTFAC= EXTFAC
        MULCH % MUL_WATFAC= WATFAC

!       Incorporate residue evenly throughout RESDEPTH
        IF (RESSOL .GT. 0.001) THEN 

!         Set the initial shoot residue incorporation depth.
          RESDEPTH = MAX(ICRID, DS(1))

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

!           Initialize FOM variables.
            FOM(L) = FOM(L) + RESSOL  * FACTOR
            IF (N_ELEMS > 0) FON(L) = FON(L) + RESSOLN * FACTOR  !kg/ha
            IF (N_ELEMS > 1) FOP(L) = FOP(L) + RESSOLP * FACTOR  !kg/ha

!           If there are no more soil layers over which to distribute
!           the residue, jump out of the DO loop. 
            IF (DEPTH >= RESDEPTH) EXIT
          ENDDO   !End of loop on soil layers.
        ENDIF

!       ****************************************************************
!       Add the initial or carry-over root and nodule residues to the 
!       fresh-organic-matter pool.
!       ----------------------------------------------------------------
        IF (ICRT + ICNOD > 1.E-4) THEN
C         Calculate N and P contributions.  
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

            FOM(L) = FOM(L) + (ICRT + ICNOD) * FACTOR
            IF (N_ELEMS > 0) 
     &        FON(L) = FON(L) + (ICRTN + 0.30 / 6.25 * ICNOD) * FACTOR
!           N:P = 10.
            IF (N_ELEMS > 1) 
     &        FOP(L) = FOP(L) + (ICRTP + 0.03 / 6.25 * ICNOD) * FACTOR
          ENDDO
        ENDIF

!       ****************************************************************
!       Calculate stable organic C, N, P
!       ----------------------------------------------------------------
        DO L = 1, NLAYR
!         Calculate the stable organic matter pool SSOMC in kg[C] / ha.
          SSOMC(L) = OC(L) * 1000. * BD(L) * DLAYR(L)

!         N initialization
          IF (N_ELEMS > 0) THEN
!           If the soil N concentration is known, calculate the humus N
!           from the total soil N (= organic + inorganic).
            IF (TotOrgN(L) .GT. 0.001) THEN
!             SSOME(L,N) = TOTN(L)* DLAYR(L) * BD(L) * 1.E03 
              SSOME(L,N) = TotOrgN(L)   !kg/ha
!             Humus C:N ratio.
              CNRAT(L) = SSOMC(L) / SSOME(L,N)
          
!             Limit C:N ratio to a minimum of 10.
              IF (CNRAT(L) .LT. 10.0) THEN
                IF (MULTI < 2) THEN
                  WRITE(MSG(1),105) 
                  WRITE(MSG(2),110) L, CNRAT(L) 
                  WRITE(MSG(3),115) 
                  WRITE(MSG(4),120) 
                  CALL WARNING(4, ERRKEY, MSG)
                ENDIF
          
  105 FORMAT('The initialized C:N ratio of the soil organic ')
  110 FORMAT('matter (humus) in layer ',I3,' is unlikely: ', F8.1)
  115 FORMAT('A C:N ratio of 10.0 will be used to calculate initial ',
     &                'soil organic N.')
  120 FORMAT('Please check your SOIL.SOL file.' )

                CNRAT(L) = 10.0
                SSOME(L,N) = SSOMC(L) / CNRAT(L)
              ENDIF

            ELSE
!             If soil N is not known, assume a humus C/N ratio of 10.
              CNRAT(L) = 10.0
              SSOME(L,N) = SSOMC(L) / CNRAT(L)
            ENDIF   !End of IF block on TotOrgN.
            SSOME(L,N) = AMAX1 (SSOME(L,N), 0.01)
          ENDIF

!         P initialization
          IF (N_ELEMS > 1) THEN
!           Calculate stable organic P
            IF (ORGP(L) > -1.E-5) THEN
!             Organic P read directly from soils file:
              ORGP(L) = MAX(0.0, ORGP(L))

            ELSE
!             Organic P estimated from soil type and other soil parameters
              SELECT CASE(SOILLAYERTYPE(L))
              CASE('CALCAREOUS       ')
                ORGP(L) = (200. * EXP(-1.8 * ((pH(L) - 3.) / 6.)**2.)) 
     &                        * (1. - EXP(-0.55 * OC(L)))
              CASE('SLIGHTLYWEATHERED')
                ORGP(L) = (900. * EXP(-1.5 * ((pH(L) - 10.) / 12.)**2.))
     &                        * (1. - EXP(-0.1 * OC(L)))
              CASE('HIGHLYWEATHERED  ')
                ORGP(L) = (200. * EXP(-1.85 * ((pH(L) - 3.) / 6.)**2.)) 
     &                        * (1. - EXP(-0.35 * OC(L)))
!             ANDISOLS
!             CASE('ANDISOL          ')
!###AJG  * ***to be finished***
              CASE DEFAULT
                ORGP(L) = (520. * EXP(-1.5 * ((pH(L) - 7.) / 8.)**2.)) 
     &                        * (1. - EXP(-0.135 * OC(L)))
              END SELECT
            ENDIF
            SSOME(L,P) = ORGP(L) / KG2PPM(L)
          ELSE
            SSOME(L,P) = 0.0
          ENDIF
        END DO   !End of soil layer loop.

C-----------------------------------------------------------------------
C     Crop rotations (sequence mode)
      ELSE
!       Get harvest residue characteristics from previous crop
        CALL IPSOIL (CONTROL, CROP=PREV_CROP,             !Input
     &    PRCEL=PRCEL, PRCHO=PRCHO, PRLIG=PRLIG,          !Output 
     &    AM=AM, EXTFAC=EXTFAC, WATFAC=WATFAC)            !Output 

!       Add the shoot residue from the previous season to the soil.
!       Carry-over root residues (TRTRES) have already been dealt with
!       in the INIT section of SOILNI.
        RESSRF = HARVRES % RESWT(SRFC)
        RESSRFN = 0.0
        RESSRFP = 0.0
        IF (RESSRF .GT. 0.0) THEN
          IF (N_ELEMS > 0) RESSRFN = HARVRES % RESE(SRFC,N)
          IF (N_ELEMS > 1) RESSRFP = HARVRES % RESE(SRFC,P)
        ENDIF

        DO L = 1, NLAYR
          !Initialize with harvest residues from previous crop
          FOM(L) = FOM(L) + HARVRES % RESWT(L)
          IF (N_ELEMS > 0) FON(L) = FON(L) + HARVRES % RESE(L,N)
          IF (N_ELEMS > 1) FOP(L) = FOP(L) + HARVRES % RESE(L,P)
        ENDDO

!       Store surface residues in MULCH variable
        IF (RESSRF > 1.E-6) THEN
          Mulch % Mulch_AM = (Mulch % Mulch_AM * Mulch % MulchMass 
     &     + AM * RESSRF) / (Mulch%MulchMass + RESSRF)
          Mulch % Mul_EXTFAC = (Mulch % Mul_EXTFAC * Mulch % MulchMass 
     &     + EXTFAC * RESSRF) / (Mulch%MulchMass + RESSRF)
          Mulch % Mul_WATFAC = (Mulch % Mul_WATFAC * Mulch % MulchMass 
     &     + WATFAC * RESSRF) / (Mulch%MulchMass + RESSRF)
        ENDIF
        MULCH % MULCHMASS = MULCH % MULCHMASS + RESSRF
        MULCH % MULCHN    = MULCH % MULCHN + RESSRFN
        MULCH % MULCHP    = MULCH % MULCHP + RESSRFP

      ENDIF  

!-----------------------------------------------------------------------
!     For sequenced and non-sequenced runs:
!-----------------------------------------------------------------------
      DO L = 1, NLAYR 
        FPOOL(L,1) = FOM(L) * PRCHO 
        FPOOL(L,2) = FOM(L) * PRCEL 
        FPOOL(L,3) = FOM(L) * PRLIG 
      ENDDO

      CALL MULCHLAYER (MULCH) 

      IF (INDEX('QF',RNMODE) .GT. 0) THEN
!       For sequenced runs, remember crop type for next harvest residue
        PREV_CROP = CROP
      ENDIF

      RETURN
      END SUBROUTINE SoilOrg_init

!=======================================================================
! SOILNI Variables
!
! BD(L)      Bulk density, soil layer L (g [soil] / cm3 [soil])
! CNRAT(L)   C/N ratio of humus or humus pool in soil layer L
!              (kg [C] / kg [N])
! DEPMAX     Maximum depth of reported soil layers (cm)
! DEPTH      Depth to the bottom of a layer from the surface (cm)
! DLAYR(L)   Soil thickness in layer L (cm)
! DMOD       Factor to adjust the mineralization rate for certain atypical 
!              soils (range 0-1) 
! DOY        Current day of simulation (d)
! DS(L)      Cumulative depth in soil layer L (cm)
! ERRKEY     Subroutine name for error file 
! ERRNUM     Error number for input 
! FACTOR     Weighting factor for residue contribution to soil layers.  
!              Decreases exponentially with depth (fraction)
! FILEIO     Filename for input file (e.g., IBSNAT35.INP) 
! FOM(L)     Fresh organic residue in soil layer L (kg [dry matter] / ha)
! FON(L)     Nitrogen in fresh organic matter in soil layer L (kg [N] / ha)
! FOUND      Indicator that good data was read from file by subroutine FIND 
!              (0 - End-of-file encountered, 1 - NAME was found) 
! FPOOL(L,J) FOM pool in soil layer L: J=1:carbohydrate, 2:cellulose, 
!              3:lignin (kg [residue pool] / ha)
! SSOMC(L)    Carbon in stable organic matter (humus) (kg [C] / ha)
! SSOME(L,N) Nitrogen in stable organic matter (humus) (kg [N] / ha)
! ICNOD      Initial mass of nodule residues in soil (left-over from 
!              previous crop) (kg [residue] /ha)
! ICRT       Initial mass of root residues in soil (left-over from previous 
!              crop) (kg [residue] /ha)
! ICRTN      Initial N content of root residues (kg [N] /ha)
! LNUM       Current line number of input file 
! LUNIO      Logical unit number for FILEIO 
! NH4(L)     Ammonium N in soil layer L (µg[N] / g[soil])
! NITCAPY(L) Previous day's nitrification potential in soil layer L, 
!              indicating whether there may be a lag phase for 
!              nitrification to proceed (range: 0-1) 
! NL         Maximum number of soil layers = 20 
! NLAYR      Actual number of soil layers 
! NO3(L)     Nitrate in soil layer L (µg[N] / g[soil])
! OC(L)      Organic carbon content of layer (%)
! PRCEL      Cellulose fraction of the residue (fraction)
! PRCHO      Carbohydrate fraction of the residue (fraction)
! PRLIG      Lignin fraction of the residue (fraction)
! RCN        N content of initial root residue (%)
! SECTION    Section name in input file 
! SNH4(L)    Total extractable ammonium N in soil layer L (kg [N] / ha)
! SNO3(L)    Total extractable nitrate N in soil layer L (kg [N] / ha)
! ST(L)      Soil temperature in soil layer L (°C)
! SW(L)      Volumetric soil water content in layer L
!              (cm3 [water] / cm3 [soil])
! TFNITY(L)  Yesterday’s soil temperature factor for nitrification (range 
!              0-1) 
! TotOrgN(L) Total organic N in layer L (kg/ha)
! UREA(L)    Amount of urea in soil layer L (kg [N] / ha)
! WRN(L)     Exponential root distribution factor for soil layer L for 
!              distribution of initial root residues 
! WSUM       Sum of WRN (root distribution) through soil profile 
!=======================================================================
