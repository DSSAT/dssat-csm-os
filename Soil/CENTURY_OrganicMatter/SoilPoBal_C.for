!***********************************************************************
!  SoilPoBal_C, Subroutine for CENTURY-based SOM/residue module.
! 
!  Purpose: Provides output P balance for Century organic matter
!     routines (file SoilPoBal.OUT)
!
!  REVISION   HISTORY
!  09/19/2003 AJG Brought into modular format and linked to the 
!                 CENTURY-based SOM/residue module.
!  03/23/2004 CHP Renamed SoilPBal_C and took out plant routines.
!  04/13/2005 CHP Removed inorganic N components from SoilNBal_C
!  08/25/2005 CHP Added consistency in names and methods between 
!                 SoilNoBal, SoilNoBal_C, and SoilPoBal.
!
!  Called: CENTURY
!  Calls : GETLUN, HEADER, YR_DOY
!***********************************************************************

      SUBROUTINE SoilPoBal_C (CONTROL, ISWITCH,
     &  HARVRES, IMM, LITE, MNR, NLAYR, OMAData,          !Input
     &  SENESCE, SOM1E, TLITE, TSOME, TSOM1E, TSOM23E)    !Input

!***********************************************************************

!     ------------------------------------------------------------------
      USE ModuleDefs
      USE Interface_SoilPBalSum
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, INCDAT, YR_DOY
      SAVE
!     ------------------------------------------------------------------
      LOGICAL FEXIST

      CHARACTER*1  IDETP, IDETL, ISWPHO
      CHARACTER*13, PARAMETER :: SPBAL = 'SoilPoBal.OUT'

      INTEGER DAS, DOY1, DOY2, DYNAMIC, INCDAT, L, LUNSPC
      INTEGER NLAYR, RUN, YR1, YR2, YRDOY, YRSIM
      INTEGER, PARAMETER :: SRFC = 0  !, SOIL = 1

      REAL AddToday, Balance, CUMBAL, CUMIMMOB, CUMMINER
      REAL CumResP, CumResPY, CumSenP, CumSenPY
      REAL DAYBAL, HarvResP, HResPSurf, HResPSoil
      REAL ResPToday, SenPToday, SenPYest, SubToday
      REAL TALLP, TALLPI, TIMMOB, TMINER, TORGP, TORGPI, TORGPY
      REAL TotLastSenes

      REAL, DIMENSION(NELEM) :: TLITE, TLITEI, TSOME, TSOMEI,  
     &  TSOM1E, TSOM1EI, TSOM23E, TSOM23EI

      REAL, DIMENSION(0:0,NELEM) ::  LITEI, SOM1EI
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
      IDETP   = ISWITCH % IDETP
      ISWPHO  = ISWITCH % ISWPHO
      IF (IDETL  == 'N' .OR. 
     &    IDETL  == '0' .OR.    !zero
     &    IDETP  == 'N' .OR. 
     &    ISWPHO == 'N') RETURN

      DYNAMIC = CONTROL % DYNAMIC
      DAS     = CONTROL % DAS
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

      CumResP = OMADATA % CumResE(P)
      CumSenP = SENESCE % CumResE(P)

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization phase
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
!     ----------------------------------------------------------------
!     Initialize output file
      CALL GETLUN(SPBAL, LUNSPC)
      INQUIRE (FILE = SPBAL, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = LUNSPC, FILE = SPBAL, STATUS = 'OLD',
     &    POSITION = 'APPEND')
        WRITE(LUNSPC,'(/,"!",79("="))') 
      ELSE
        OPEN (UNIT = LUNSPC, FILE = SPBAL, STATUS = 'NEW')
        WRITE(LUNSPC,'("*SOIL ORGANIC P BALANCE - CENTURY ROUTINES")')
      ENDIF

      CALL HEADER (SEASINIT, LUNSPC, RUN)

!     Initial value for SOM N, summed across all soil layers (not the
!     SRFC layer.
      TSOM1EI(P)  = TSOM1E(P)
      TSOM23EI(P) = TSOM23E(P)
      TSOMEI(P)   = TSOME(P)

      IF (RUN > 1) THEN
!       Harvest residue plus left-over senesced material from previous
!       crop.
        HResPSurf = HARVRES % ResE(SRFC,P)   !surface res P kg[P]/ha
        HResPSoil = 0.0                   !soil res P kg[P]/ha
        DO L = 1, NLAYR
          HResPSoil = HResPSoil + HARVRES % ResE(L,P)
        ENDDO
        HarvResP = HResPSurf + HResPSoil
      ELSE
        HResPSurf = 0.0
        HResPSoil = 0.0
        HarvResP  = 0.0
      ENDIF

!     Initial value for surface litter and surface SOM1.
!     Subtract residue leftover from previous crop from surface 
!     litter because this was added in previously.
      LITEI(SRFC,P) = LITE(SRFC,P) - HResPSurf
      SOM1EI(SRFC,P) = SOM1E(SRFC,P)

!     Initial value for soil litter (not surface litter).
!     Subtract residue leftover from previous crop from soil 
!       litter because this was added in previously.
      TLITEI(P) = TLITE(P) - HResPSoil

!     Sum all the soil organic P pools.
      TORGPI = TSOMEI(P) + TLITEI(P) + LITEI(SRFC,P) + SOM1EI(SRFC,P)

!     If detailed printout requested, print daily soil P balance
      IF (INDEX('AD',IDETL) > 0) THEN
!       Cumulative values (yesterday)
        CumResPY = 0.0
        CumSenPY = 0.0
        CUMBAL   = 0.0
        SenPYest = 0.0
        TORGPY   = TORGPI + HarvResP

        WRITE(LUNSPC,10)
   10   FORMAT(
     &    17X,'--------  STATE VARIABLES TODAY  -------   -',
     &    '------- ADDED TODAY --------   -REMOV TODAY-   BALANCE',
     &    '   BALANCE',/,
     &    '@YEAR DOY   DAS  SRFLIT SRFSOM1   TLITP    TSOMP  SRFSOIL',
     &    '    ResP  SENESP   IMMOB   TOTAL   MINER   TOTAL     DAILY',
     &    '    CUMUL.')
      ENDIF

      CUMMINER = 0.0
      CUMIMMOB = 0.0

      CALL SoilPBalSum (CONTROL,  
     &    LITE=LITE, SOM1E=SOM1E, TLITP=TLITE(P), 
     &    TSOM1P=TSOM1E(P), TSOM23P=TSOM23E(P))

      ENDIF !DYNAMIC

!***********************************************************************
!***********************************************************************
!     Daily output
!***********************************************************************
      IF (DYNAMIC == OUTPUT .OR. DYNAMIC .EQ. SEASINIT) THEN
!     ------------------------------------------------------------------
      TMINER = MNR(0,P)
      TIMMOB = IMM(0,P)
!     Combine mineralization in surface and top soil layer and take the profile sum.
!     *** This time the profile sum includes the SRFC value also! ***
      DO L = 1, NLAYR
!       Mineralization.
        TMINER = TMINER + MNR(L,P)
!       Immobilization.
        TIMMOB = TIMMOB + IMM(L,P)
      ENDDO

!     Accumulate immobilization and mineralization over time.
      CUMMINER = CUMMINER + TMINER
      CUMIMMOB = CUMIMMOB + TIMMOB

      IF (INDEX('AD',IDETL) > 0) THEN
!       Compute daily rates from cumulative values
        ResPToday = CumResP - CumResPY
        SenPToday = CumSenP - CumSenPY

!       Total soil organic E and combined with the mineral E.
        TORGP = TSOME(P) + TLITE(P) + LITE(SRFC,P) + SOM1E(SRFC,P)

!       Additions to and removals from the field.
        AddToday = ResPToday + SenPYest + TIMMOB
        SubToday = TMINER

        DAYBAL = TORGP - TORGPY - AddToday + SubToday
        CUMBAL = CUMBAL + DAYBAL
!        IF (ABS(DAYBAL) > 1.E-5) THEN
!          PRINT *, YRDOY, DAYBAL
!        ENDIF

        IF (DYNAMIC .EQ. SEASINIT) THEN
          CALL YR_DOY(INCDAT(YRDOY,-1), YR2, DOY2)
        ELSE
          CALL YR_DOY(YRDOY, YR2, DOY2)
        ENDIF

!       Write daily output to SoilPoBal.OUT.
        WRITE (LUNSPC,50) YR2, DOY2, DAS, 
     &    LITE(SRFC,P), SOM1E(SRFC,P), TLITE(P), TSOME(P), TORGP, 
     &    RESPTODAY, SenPYest, 
     &    TIMMOB, AddToday, TMINER, SubToday, DAYBAL, CUMBAL
   50   FORMAT(1X, I4, 1X, I3, 1X, I5, 3F8.3, 2F9.3, 6F8.3, 2F10.3)

!       Save today's cumulative values for use tomorrow
        CumResPY = CumResP
        CumSenPY = CumSenP
        TORGPY   = TORGP
        SenPYest = SenPToday
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal output 
!***********************************************************************
      ELSEIF (DYNAMIC == SEASEND) THEN
!     ------------------------------------------------------------------
!       Sum all the surface and soil organic P pools.
      TORGP = TSOME(P) + TLITE(P) + LITE(SRFC,P) + SOM1E(SRFC,P)

!     Add the P from organic residues applied during the growth period
!     and sum this with the initial TALLPI to make the balance fit with
!     the final TALLP. Senesced material is included in the final litter
!     pools, and thus can be considered as input on the initial soil-P 
!     content. 
      TALLPI = TORGPI + CumResP + CumSenP + CUMIMMOB + HarvResP

!       P balance will be off by the amount of P uptake on the last day 
!         of season because this amount has been added to the P uptake by
!       plant, but has not been subtracted from soil.  This is because
!       soil processes are computed before plant processes.
!     Need to subtract this amount from balance.
      TotLastSenes = 0.0
      DO L = 0, NL
        TotLastSenes = TotLastSenes + SENESCE % ResE(L,P)
      ENDDO
        TALLPI = TALLPI - TotLastSenes

!     Sum the initial value of all abiotic P pools (soil, air,
!     fertilizer), SOM and cumulative mineralization.
      TALLP = TORGP + CUMMINER

!       Write end-of-season output.
        CALL YR_DOY(YRSIM, YR1, DOY1)
        CALL YR_DOY(YRDOY, YR2, DOY2)
        WRITE (LUNSPC,100) YR1, DOY1, YR2, DOY2

      WRITE (LUNSPC,200) TSOM1EI(P), TSOM1E(P),
     &  TSOM23EI(P), TSOM23E(P), TLITEI(P), TLITE(P), 
     &  LITEI(SRFC,P), LITE(SRFC,P), SOM1EI(SRFC,P), SOM1E(SRFC,P)

      WRITE (LUNSPC,300) TORGPI, TORGP, 
     &  HarvResP, CumResP, CumSenP, CUMIMMOB,
     &  CUMMINER, TALLPI, TALLP

      Balance = TALLP - TALLPI
      WRITE(LUNSPC,400) Balance

        CLOSE (UNIT = LUNSPC)

100   FORMAT (//,T42,'INITIAL Year/Doy', T64, 'FINAL Year/Doy', 
     &    /,T49, I5,'/',I3.3, T69,I5,'/',I3.3,
     &  /,'SOIL ORGANIC P BALANCE',T49,'-----------kg P/ha-----------')

200   FORMAT ('  SOIL & SURFACE ORGANIC MATTER P',
     &  /, 3X,'Active (= microbial) SOM P      ',T48, F10.3, T68, F10.3,
     &  /, 3X,'Intermediate & passive SOM P    ',T48, F10.3, T68, F10.3,
     &  /, 3X,'Soil Litter P                   ',T48, F10.3, T68, F10.3,
     &  /, 3X,'P in Litter on Top of the Soil  ',T48, F10.3, T68, F10.3,
     &  /, 3X,'P in Microbes on Top of the Soil',T48, F10.3, T68, F10.3,
     &  /, 3X, T48,'  --------',T68,'  --------')

300   FORMAT (3X, 'Total Organic P in Soil and Surface Layers',
     &                                           T48, F10.2, T68, F10.2,
     &    //,'  ADDITIONS AND REMOVALS:'
     &  /, 3X, 'P in Harvest Residues from Previous Crop',
     &  /, 3X, '  (sequential runs only)',       T48, F10.3,
     &  /, 3X, 'P from Organic Applications',    T48, F10.3,
     &  /, 3X, 'P in returned senesced material',T48, F10.3,
     &  /, 3X, 'P immobilized',                  T48, F10.3,
     &  /, 3X, 'P mineralized',                              T68, F10.3,
     &  /, 3X, T48,'  --------',T68,'  --------',
     &  /, '  TOTAL P BALANCE',                  T48, F10.3, T68, F10.3)

400   FORMAT(3X,'Balance',                                   T68, F10.3)

      CALL SoilPBalSum (CONTROL,  
     &    Balance=Balance, CUMIMMOB=CUMIMMOB, CUMMINER=CUMMINER, 
     &    CUMRESP=CUMRESP, CumSenP=CumSenP, HARVRESP=HARVRESP, 
     &    LITE=LITE, SOM1E=SOM1E, TLITP=TLITE(P), 
     &    TSOM1P=TSOM1E(P), TSOM23P=TSOM23E(P))

!***********************************************************************
!***********************************************************************
!     End of DYNAMIC IF construct
!***********************************************************************
      END IF

      RETURN
      END SUBROUTINE SoilPoBal_C
