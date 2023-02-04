!***********************************************************************
!  SoilNoBal_C, Subroutine for CENTURY-based SOM/residue module.
!
!  Purpose: Provides output N balance for Century organic matter
!     routines (file SoilNoBal.OUT)
!
!  REVISION   HISTORY
!  04/13/2005 CHP Removed inorganic N components from SoilNBal_C
!  08/25/2005 CHP Added consistency in names and methods between 
!                 SoilNoBal, SoilNoBal_C, and SoilPoBal.
!
!  Called: CENTURY
!  Calls : GETLUN, HEADER, YR_DOY
!***********************************************************************

      SUBROUTINE SoilNoBal_C (CONTROL, ISWITCH,
     &  HARVRES, IMM, LITE, MNR, NLAYR, OMAData,          !Input
     &  SENESCE, SOM1E, TLITE, TSOME, TSOM1E,             !Input
     &  TSOM2E, TSOM3E)                                   !Input

!***********************************************************************

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      USE Interface_SoilNBalSum
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, YR_DOY, INCDAT
      SAVE
!     ------------------------------------------------------------------
      LOGICAL FEXIST

      CHARACTER*1  IDETN, IDETL, ISWNIT
      CHARACTER*13, PARAMETER :: SNBAL = 'SoilNoBal.OUT'

      INTEGER DAS, DOY1, DOY2, DYNAMIC, INCDAT, L, LUNSNC
      INTEGER NLAYR, RUN, YR1, YR2, YRDOY, YRSIM
      INTEGER, PARAMETER :: SRFC = 0  !, SOIL = 1

      REAL AddToday, Balance, CUMBAL, CUMIMMOB, CUMMINER
      REAL CumResN, CumResNY, CumSenN, CumSenNY, DAYBAL
      REAL HarvResN, HResNSoil, HResNSurf
      REAL ResNToday, SenNToday, SenNYest, SubToday
      REAL TALLN, TALLNI, TIMMOB, TMINER
      REAL TORGN, TORGNY, TORGNI, TotLastSenes

      REAL, DIMENSION(NELEM) :: TLITE, TLITEI, TSOME, TSOMEI, 
     &  TSOM1E, TSOM1EI, TSOM2E, TSOM2EI, TSOM3E, TSOM3EI

      REAL, DIMENSION(0:NL,NELEM) ::  LITEI, SOM1EI
      REAL, DIMENSION(0:NL,NELEM) :: IMM, LITE, MNR, SOM1E

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType)   CONTROL
      TYPE (SwitchType)    ISWITCH
      TYPE (ResidueType)   HARVRES
      TYPE (ResidueType)   SENESCE
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
      WRITE(LUNSNC,'("*SOIL ORGANIC N BALANCE - CENTURY ROUTINES")')
      ENDIF

      CALL HEADER (SEASINIT, LUNSNC, RUN)

!     Initial value for SOM N, summed across all soil layers (not the
!     SRFC layer.
      TSOM1EI(N) = TSOM1E(N)
      TSOM2EI(N) = TSOM2E(N)
      TSOM3EI(N) = TSOM3E(N)
      TSOMEI(N)  = TSOME(N)

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

!     Initial value for surface litter and surface SOM1.
!     Subtract residue leftover from previous crop from surface 
!     litter because this was added in previously.
      LITEI(SRFC,N) = LITE(SRFC,N) - HResNSurf
      SOM1EI(SRFC,N) = SOM1E(SRFC,N)

!     Initial value for soil litter (not surface litter).
!     Subtract residue leftover from previous crop from soil 
!       litter because this was added in previously.
      TLITEI(N) = TLITE(N) - HResNSoil

!     Sum all the soil organic N pools.
      TORGNI = TSOMEI(N) + TLITEI(N) + LITEI(SRFC,N) + SOM1EI(SRFC,N)

!     If detailed printout requested, print daily soil N balance
      IF (INDEX('AD',IDETL) > 0) THEN
!       Cumulative values (yesterday)
        CUMRESNY = 0.0
        CUMSENNY = 0.0
        CUMBAL   = 0.0
        SenNYest = 0.0
        TORGNY = TORGNI + HarvResN
        
        WRITE(LUNSNC,10)
   10   FORMAT('!',
     &    16X,'--------  STATE VARIABLES TODAY  -------   -',
     &    '------- ADDED TODAY --------   -REMOV TODAY-   BALANCE',
     &    '   BALANCE',/,
     &    '@YEAR DOY   DAS  SRFLIT SRFSOM1   TLITN    TSOMN  SRFS',
     &    'OIL  RESIDN  SENESN   IMMOB  TOTADD   MINER  TOTSUB   ',
     &    ' DAYBAL    CUMBAL')
      ENDIF

      CUMMINER = 0.0
      CUMIMMOB = 0.0

      IF (INDEX('AD',IDETL) > 0) THEN
        CALL YR_DOY(INCDAT(YRDOY,-1), YR2, DOY2)
!       Write daily output to SoilNoBal.OUT.
        WRITE (LUNSNC,50) YR2, DOY2, 0, 
     &    LITE(SRFC,N), SOM1E(SRFC,N), TLITE(N), TSOME(N), TORGN, 
     &    0.0, 0.0, 
     &    0.0, 0.0, 0.0, 0.0, 0.0, 0.0
      ENDIF

      CALL SoilNBalSum (CONTROL, 
     &    CUMRESN=CUMRESN, HARVRESN=HARVRESN, 
     &    LITE=LITEI, CumSenN=CumSenN, SOM1E=SOM1E, TLITN=TLITEI(N), 
     &    TSOM1N=TSOM1E(N), TSOM2N=TSOM2E(N), TSOM3N=TSOM3E(N))

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
        TORGN = TSOME(N) + TLITE(N) + LITE(SRFC,N) + SOM1E(SRFC,N)

!       Additions to and removals from the field.
        AddToday   = ResNToday + SenNYest + TIMMOB
        SubToday   = TMINER

        DAYBAL = TORGN - TORGNY - AddToday + SubToday
        CUMBAL = CUMBAL + DAYBAL

        CALL YR_DOY(YRDOY, YR2, DOY2)
!       Write daily output to SoilNoBal.OUT.
        WRITE (LUNSNC,50) YR2, DOY2, DAS, 
     &    LITE(SRFC,N), SOM1E(SRFC,N), TLITE(N), TSOME(N), TORGN, 
     &    ResNToday, SenNToday, 
     &    TIMMOB, AddToday, TMINER, SubToday, DAYBAL, CUMBAL
   50   FORMAT(1X, I4, 1X, I3, 1X, I5, 3F8.2, 2F9.2, 6F8.2, 2F10.3)

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
      TORGN = TSOME(N) + TLITE(N) + LITE(SRFC,N) + SOM1E(SRFC,N)

!     Add the N from organic residues applied during the growth period 
!     and sum this with the initial TALLNI to make the balance fit with 
!     the final TALLN. Senesced material is included in the final litter 
!     pools, and thus can be considered as input on the initial soil-N
!     content. SEEDNI is not needed, because for the plant the NBAL
!     only deals with N uptake from the soil.
      TALLNI = TORGNI + CumResN + CumSenN + CUMIMMOB + HarvResN

!     N balance will be off by the amount of N uptake on the last day 
!     of season because this amount has been added to the N uptake by
!     plant, but has not been subtracted from soil.  This is because
!     soil processes are computed before plant processes.
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

      WRITE (LUNSNC,200) TSOM1EI(N), TSOM1E(N),
     &    TSOM2EI(N), TSOM2E(N), TSOM3EI(N), TSOM3E(N), TLITEI(N),
     &    TLITE(N), LITEI(SRFC,N), LITE(SRFC,N), SOM1EI(SRFC,N),
     &    SOM1E(SRFC,N)

      WRITE (LUNSNC,300) TORGNI, TORGN, 
     &  HarvResN, CumResN, CumSenN, CUMIMMOB, 
     &  CUMMINER, TALLNI, TALLN

      Balance = TALLN - TALLNI
      WRITE(LUNSNC,400) Balance

        CLOSE (UNIT = LUNSNC)

100   FORMAT (//,'!',T42,'INITIAL Year/Doy', T64, 'FINAL Year/Doy', 
     &    /,'!'T49, I5,'/',I3.3, T69,I5,'/',I3.3,
     &    /,'! SOIL N BALANCE',T49,'-----------kg N/ha-----------')

200   FORMAT ('!  SOIL & SURFACE ORGANIC MATTER N',
     &  /,'!   Active (= microbial) SOM N',      T48, F10.2, T68 ,F10.2,
     &  /,'!   Intermediate SOM N',              T48, F10.2, T68, F10.2,
     &  /,'!   Passive SOM N',                   T48, F10.2, T68, F10.2,
     &  /,'!   Soil Litter N',                   T48, F10.2, T68, F10.2,
     &  /,'!   N in Litter on Top of the Soil',  T48, F10.2, T68, F10.2,
     &  /,'!   N in Microbes on Top of the Soil',T48, F10.2, T68, F10.2,
     &  /,'!',T48,'  --------',T68,'  --------')

300   FORMAT('!   Total Organic N in Soil and Surface Layers',
     &                                             T48,F10.2,T68, F10.2,
     &    //,'!','  ADDITIONS AND REMOVALS:'
     &  /,'!   N in Harvest Residues from Previous Crop',T48, F10.2,
     &  /,'!   N from Organic Applications',     T48, F10.2,
     &  /,'!   N in returned senesced material', T48, F10.2,
     &  /,'!   N immobilized',                   T48, F10.2,
     &  /,'!   N mineralized',                               T68, F10.2,
     &  /,'!',                      T48,'  --------',T68,'  --------',
     &  /,'!     TOTAL N BALANCE',               T48, F10.2, T68, F10.2)

400   FORMAT('!   Balance',                                 T68, F10.2)

      CALL SoilNBalSum (CONTROL, 
     &    Balance=Balance, CUMRESN=CUMRESN, HARVRESN=HARVRESN, 
     &    LITE=LITE, CumSenN=CumSenN, SOM1E=SOM1E, TLITN=TLITE(N), 
     &    TSOM1N=TSOM1E(N), TSOM2N=TSOM2E(N), TSOM3N=TSOM3E(N), 
     &    CUMMINER=CUMMINER, CUMIMMOB=CUMIMMOB)

!***********************************************************************
!***********************************************************************
!     End of DYNAMIC IF construct
!***********************************************************************
      END IF

      RETURN
      END SUBROUTINE SoilNoBal_C
