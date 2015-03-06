C***********************************************************************
C  SoilNoBal, Subroutine
C 
C  Purpose: Provides output N balance for organic soil processes 
C     (file SoilNoBal.OUT).  
C
C  REVISION   HISTORY
C  01/01/1995 WTB Written.
C  01/01/2000 CHP/AJG Integrated the CENTURY-based and CERES-based SOM
C                 modules with CHP's modular structure.
C  03/16/2000 GH  Checked the new modular CROPGRO.
C  06/19/2001 GH  Modified output
C  03/07/2001 CHP Split plant and soil N balances.
C  09/11/2002 CHP Added daily balance when IDETL = 'D'
!  02/01/2005 CHP Added one-line output seasonal N balance file SNBalSum.OUT
!  03/01/2005 CHP Changed variable names to match Century:
!                 HUMC to SSOMC     HUMN to SSOME
!                 THUMC to TSOMC    THUMN to TSOME
!                 TFON to TLITE     TFOM to TLITC
!  04/14/2005 CHP Removed inorganic components
!  08/25/2005 CHP Added consistency in names and methods between 
!                 SoilNoBal, SoilNoBal_C, and SoilPoBal.
C***********************************************************************

      SUBROUTINE SoilNoBal (CONTROL, ISWITCH, 
     &    HARVRES, IMM, MNR, MULCH, NLAYR, 
     &    OMADATA, SENESCE, TLITE, TSOME) 

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT NONE
      SAVE
!     ------------------------------------------------------------------
      LOGICAL FEXIST

      CHARACTER*1  IDETN, IDETL, ISWNIT
      CHARACTER*13, PARAMETER :: SNBAL = 'SoilNoBal.OUT'

      INTEGER DAS, DOY1, DOY2, DYNAMIC, L, LUNSNC
      INTEGER NLAYR, RUN, YR1, YR2, YRDOY, YRSIM
      INTEGER, PARAMETER :: SRFC = 0  !, SOIL = 1

      REAL AddToday, CUMBAL, CUMIMMOB, CUMMINER, CumResN, CumResNY
      REAL CumSenN, CumSenNY, DAYBAL
      REAL HarvResN, HResNSoil, HResNSurf, MULCHN
      REAL ResNToday, RESLEFTNI, SenNToday, SenNYest, SubToday
      REAL TALLN, TALLNI, TFONI, TIMMOB, TMINER
      REAL TORGN, TORGNI, TORGNY, TSOMNI, TotLastSenes

      REAL TLITE(NELEM), TSOME(NELEM)
      REAL, DIMENSION(0:NL,NELEM) :: IMM, MNR

      TYPE (ControlType)   CONTROL
      TYPE (SwitchType)    ISWITCH
      TYPE (ResidueType)   HARVRES
      TYPE (ResidueType)   SENESCE
      TYPE (MulchType)     MULCH
      TYPE (OrgMatAppType) OMAData

!     ------------------------------------------------------------------
!     Return if detail not requested.
      IDETL   = ISWITCH % IDETL
      IDETN   = ISWITCH % IDETN
      ISWNIT  = ISWITCH % ISWNIT
      IF (IDETL  == 'N' .OR. 
     &    IDETL  == '0' .OR.    !zero
     &    IDETN  == 'N' .OR. 
     &    ISWNIT == 'N') RETURN

      DYNAMIC = CONTROL % DYNAMIC
      DAS     = CONTROL % DAS
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

      CumResN = OMAData % CumResE(N)
      CumSenN = SENESCE % CumResE(N)
      MULCHN  = MULCH   % MULCHN

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization phase
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
!     ----------------------------------------------------------------
!     Initialize output file
      CALL GETLUN(SNBAL, LUNSNC)
      INQUIRE (FILE = SNBAL, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = LUNSNC, FILE = SNBAL, STATUS = 'OLD',
     &    POSITION = 'APPEND')
        WRITE(LUNSNC,'(/,"!",79("="))') 
      ELSE
        OPEN (UNIT = LUNSNC, FILE = SNBAL, STATUS = 'NEW')
        WRITE(LUNSNC,'("*SOIL ORGANIC N BALANCE")')
      ENDIF

      CALL HEADER (SEASINIT, LUNSNC, RUN)

      IF (RUN > 1) THEN
!       Harvest residue plus left-over senesced material from previous
!       crop.
        HResNSurf = HARVRES % ResE(SRFC,N) !surface res N kg[N]/ha
        HResNSoil = 0.0                   !soil res N kg[N]/ha
        DO L = 1, NLAYR
          HResNSoil = HResNSoil + HARVRES % ResE(L,N)
        ENDDO
        HarvResN = HResNSurf + HResNSoil
      ELSE
        HResNSurf = 0.0
        HResNSoil = 0.0
        HarvResN  = 0.0
      ENDIF

!     Initial value for surface litter.
      RESLEFTNI = MULCHN - HResNSurf

!     Initial value for soil litter. Because 
!     HARVRESN has been added to FON (and thus is part of TLITE),
!     remove it here, so as to present them separately.
      TFONI = MAX(0.0, TLITE(N) - HResNSoil)

      TSOMNI = TSOME(N)

!     Initial value for SOM N, summed across all soil layers.
      TORGNI = TSOMNI + TFONI + RESLEFTNI

!     If detailed printout requested, print daily soil N balance
      IF (INDEX('AD',IDETL) > 0) THEN
!       Cumulative values (yesterday)
        CUMRESNY = 0.0
        CUMSENNY = 0.0
        CUMBAL   = 0.0
        SenNYest = 0.0
        TORGNY = TORGNI + HarvResN

        WRITE(LUNSNC,10)
   10     FORMAT('!',17X,'----- STATE VARS ------   ---- ADDED TODAY',
     &    ' ---- REMOVED     DAILY     CUMUL',/,
     &    '@YEAR DOY   DAS      HUMN     FON    RESN',
     &    '   ARESN   ASENN   IMMOB   MINER',
     &    '      DBAL      CBAL')
      ENDIF

      CUMMINER = 0.0
      CUMIMMOB = 0.0

!***********************************************************************
!***********************************************************************
!     Daily output
!***********************************************************************
      ELSEIF (DYNAMIC == OUTPUT) THEN
!     ------------------------------------------------------------------
      TMINER = MNR(0,N)
      TIMMOB = IMM(0,N)
!     Combine mineralization in surface and top soil layer and take the profile sum.
!     *** This time the profile sum includes the SRFC value also! ***
      DO L = 1, NLAYR
!       Mineralization.
        TMINER = TMINER + MNR(L,N)
!       Immobilization.
        TIMMOB = TIMMOB + IMM(L,N)
      ENDDO

!     Accumulate immobilization and mineralization over time.
      CUMMINER = CUMMINER + TMINER
      CUMIMMOB = CUMIMMOB + TIMMOB

      IF (INDEX('AD',IDETL) > 0) THEN
!       Compute daily rates from cumulative values
        ResNToday  = CumResN - CumResNY
        SenNToday  = CumSenN - CumSenNY

!       Total surface and soil organic N
        TORGN = TSOME(N) + TLITE(N) + MULCHN 

!       Additions to and removals from the field.
!       Use yesterday's senescence because addition to soil occurs
!         on the next day.
        AddToday   = ResNToday + SenNYest + TIMMOB
        SubToday   = TMINER

        DAYBAL = TORGN - TORGNY - AddToday + SubToday
        CUMBAL = CUMBAL + DAYBAL

        CALL YR_DOY(YRDOY, YR2, DOY2)
!       Write daily output to SoilNoBal.OUT.
        WRITE (LUNSNC,50) YR2, DOY2, DAS, TSOME(N), TLITE(N), 
     &    MULCHN, ResNToday, SenNYest, TIMMOB, 
     &      TMINER, DAYBAL, CUMBAL
   50     FORMAT(I5, I4.3, I6, 1X, F9.2, 6F8.3, 2F10.3)

!       Save today's cumulative values for use tomorrow
        CumResNY = CumResN
        CumSenNY = CumSenN
        TORGNY = TORGN
        SenNYest = SenNToday
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal output 
!***********************************************************************
      ELSEIF (DYNAMIC == SEASEND) THEN
!     ------------------------------------------------------------------
!     Sum all the surface and soil organic N pools.
      TORGN = TSOME(N) + TLITE(N) + MULCHN

!     Add the N from organic residues applied during the growth period 
!     and sum this with the initial TALLNI to make the balance fit with 
!     the final TALLN. Senesced material is included in the final litter 
!     pools, and thus can be considered as input on the initial soil-N
!       content. SEEDNI is not needed, because for the plant the NBAL
!       only deals with N uptake from the soil.
      TALLNI = TORGNI + CumResN + CumSenN + CUMIMMOB + HarvResN

!     N balance will be off by the amount of N uptake on the last day 
!       of season because this amount has been added to the N uptake by
!       plant, but has not been subtracted from soil.  This is because
!       soil processes are computed before plant processes.
!     Need to subtract this amount from balance.
      TotLastSenes = 0.0
      DO L = 0, NL
        TotLastSenes = TotLastSenes + SENESCE % ResE(L,N)
      ENDDO
      TALLNI = TALLNI - TotLastSenes

      TALLN = TORGN + CUMMINER

!       Write end-of-season output.
        CALL YR_DOY(YRSIM, YR1, DOY1)
        CALL YR_DOY(YRDOY, YR2, DOY2)
        WRITE (LUNSNC,100) YR1, DOY1, YR2, DOY2

        WRITE (LUNSNC, 200) TSOMNI, TSOME(N), TFONI, TLITE(N), 
     &    RESLEFTNI, MULCHN

        WRITE (LUNSNC,600) HARVRESN, CumResN, CUMSENN, CUMIMMOB,CUMMINER

        WRITE(LUNSNC,800) TALLNI, TALLN
        WRITE(LUNSNC,900) TALLN - TALLNI

        CLOSE (UNIT = LUNSNC)

100     FORMAT (//,'!',T42,'INITIAL Year/Doy', T64, 'FINAL Year/Doy', 
     &   /,'!',T49, I5,'/',I3.3, T69,I5,'/',I3.3,
     &   /,'!','SOIL N BALANCE',T49,'-----------kg N/ha-----------')

200     FORMAT (/,'!', 3X, 'Soil Humus N',       T48, F10.2, T68, F10.2,
     &   /,'!', 3X,'Soil Litter N',              T48, F10.2, T68, F10.2,
     &   /,'!', 3X,'N in Surface Residue',       T48, F10.2, T68, F10.2)

600     FORMAT (
     &   /,'!', 3X,'Added to / Removed from Soil:'
     &   /,'!', 3X,'N in Harvest Residues from Previous Crop',
     &                                           T48, F10.2,
     &   /,'!',3X,'N from Organic Applications', T48, F10.2,
     &   /,'!',3X,'N from senesced plant matter',T48, F10.2, 
     &   /,'!',3X,'N immobilized',               T48, F10.2,
     &   /,'!',3X,'N mineralized',                           T68, F10.2)

800     FORMAT (/,'!',3X, 'Total N balance',     T48, F10.2, T68, F10.2)
900     FORMAT ('!',3X,'Balance',                            T68, F10.3)

!***********************************************************************
!***********************************************************************
!     End of DYNAMIC IF construct
!***********************************************************************
      END IF

      RETURN
      END SUBROUTINE SoilNoBal


