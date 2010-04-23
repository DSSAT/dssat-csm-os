!***********************************************************************
!  SoilPoBal, Subroutine
! 
!  Purpose: Seasonal organic soil P balance for Ceres-based organic
!     matter module.
!
!  REVISION   HISTORY
!  04/18/2005 CHP Written.
!  08/25/2005 CHP Added consistency in names and methods between 
!                 SoilNoBal, SoilNoBal_C, and SoilPoBal.
!
!  Called: CENTURY SoilOrg
!  Calls:  GETLUN, HEADER, YR_DOY
!***********************************************************************

      SUBROUTINE SoilPoBal (CONTROL, ISWITCH, 
     &    HARVRES, IMM, MNR, MULCH, NLAYR,                !Input
     &    OMADATA, SENESCE, TLITE, TSOME)                 !Input

!***********************************************************************

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT NONE
      SAVE
!     ------------------------------------------------------------------
      LOGICAL FEXIST

      CHARACTER*1  IDETP, IDETL, ISWPHO
      CHARACTER*13, PARAMETER :: SPBAL = 'SoilPoBal.OUT'

      INTEGER DAS, DOY1, DOY2, DYNAMIC, L, LUNSPC
      INTEGER NLAYR, RUN, YR1, YR2, YRDOY, YRSIM
      INTEGER, PARAMETER :: SRFC = 0  !, SOIL = 1

      REAL ADDTODAY, CUMBAL, CUMMINER, CUMIMMOB
      REAL CumResP, CumResPY, CumSenP, CumSenPY, DAYBAL 
      REAL HARVRESP, HResPSoil, HResPSurf, MulchP
      REAL RESLEFTPI, ResPToday, SenPToday, SenPYest, SUBTODAY  
      REAL TALLP, TALLPI, TFOPI, TMINER, TIMMOB
      REAL TORGP, TORGPI, TORGPY, TSOMPI, TotLastSenes

      REAL TLITE(NELEM), TSOME(NELEM)
      REAL, DIMENSION(0:NL, NELEM) :: IMM, MNR

!     ------------------------------------------------------------------
      TYPE (ControlType)   CONTROL
      TYPE (SwitchType)    ISWITCH
      TYPE (ResidueType)   HARVRES
      TYPE (ResidueType)   SENESCE
      TYPE (MulchType)     MULCH
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

!     Transfer values from constructed data types into local variables.
      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

      CumResP = OMADATA % CumResE(P)
      CumSenP = SENESCE % CumResE(P)
      MulchP = MULCH % MulchP

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization phase
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
!     ------------------------------------------------------------------
!     Initialize output file
      CALL GETLUN(SPBAL, LUNSPC)
      INQUIRE (FILE = SPBAL, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = LUNSPC, FILE = SPBAL, STATUS = 'OLD',
     &    POSITION = 'APPEND')
        WRITE(LUNSPC,'(/,"!",79("="))') 
      ELSE
        OPEN (UNIT = LUNSPC, FILE = SPBAL, STATUS = 'NEW')
        WRITE(LUNSPC,'("*SOIL ORGANIC P BALANCE")')
      ENDIF

      CALL HEADER (SEASINIT, LUNSPC, RUN)

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

!     Initial value for surface litter.
      RESLEFTPI = MulchP - HResPSurf

!     Initial value for soil litter. Because 
!     HARVRESP has been added to FON (and thus is part of TLITE),
!     remove it here, so as to present them separately.
      TFOPI = MAX(0.0, TLITE(P) - HResPSoil)

      TSOMPI = TSOME(P)

!     Sum all the surface and soil organic P pools.
      TORGPI = TSOMPI + TFOPI + RESLEFTPI

!     If detailed printout requested, print daily soil P balance
      IF (INDEX('AD',IDETL) > 0) THEN
!       Cumulative values (yesterday)
        CumResPY = 0.0
        CumSenPY = 0.0
        CUMBAL   = 0.0
        SenPYest = 0.0
        TORGPY   = TORGPI + HARVRESP

        WRITE(LUNSPC,10)
   10     FORMAT('!',17X,'----- STATE VARS -----   ------- ADDED --',
     &    '-----   -REM-     DAILY     CUMUL',/,
     &    '@YEAR DOY  DAS      HUMP     FOP    RESP   ARESP   ASENP',
     &    '  AIMMOB   MOBIL      DBAL      CBAL')
      ENDIF

      CUMMINER = 0.0
      CUMIMMOB = 0.0

!***********************************************************************
!***********************************************************************
!     Daily output
!***********************************************************************
      ELSEIF (DYNAMIC == OUTPUT) THEN
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

      IF (INDEX('AD',IDETL) > 0 .AND. DAS /= 0) THEN
!       Compute daily rates from cumulative values
        ResPToday = CumResP - CumResPY
        SenPToday = CumSenP - CumSenPY

!       Total surface and soil organic P
        TORGP = TSOME(P) + TLITE(P) + MulchP 

!       Additions to and removals from the field.
        AddToday = ResPToday + SenPYest + TIMMOB
        SubToday = TMINER

        DAYBAL = TORGP - TORGPY - AddToday + SubToday
        CUMBAL = CUMBAL + DAYBAL
!        IF (ABS(DAYBAL) > 1.E-5) THEN
!          PRINT *, YRDOY, DAYBAL
!        ENDIF

        CALL YR_DOY(YRDOY, YR2, DOY2)
!       Write daily output to SoilPoBal.OUT.
        WRITE (LUNSPC,50) YR2, DOY2, DAS, TSOME(P), TLITE(P), 
     &      MulchP, ResPToday, SenPToday, TIMMOB, 
     &      TMINER, DAYBAL, CUMBAL
   50   FORMAT(I5, I4.3, I5, 1X, F9.2, 3F8.3, F8.4, 2F8.3, 2F10.3)

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
        TORGP = TSOME(P) + TLITE(P) + MulchP

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

        WRITE (LUNSPC, 200) TSOMPI, TSOME(P), TFOPI, TLITE(P), 
     &    RESLEFTPI, MulchP

        WRITE (LUNSPC,600) HARVRESP, CumResP, CumSenP, CUMIMMOB,CUMMINER

        WRITE(LUNSPC,800) TALLPI, TALLP
        WRITE(LUNSPC,900) TALLP - TALLPI

        CLOSE (UNIT = LUNSPC)

100     FORMAT (//,'!',T42,'INITIAL Year/Doy', T64, 'FINAL Year/Doy', 
     &   /,'!',T49, I5,'/',I3.3, T69,I5,'/',I3.3,
     &   /,'!','SOIL P BALANCE',T49,'-----------kg P/ha-----------')

200     FORMAT (/,'!', 3X, 'Soil Humus P',       T48, F10.2, T68, F10.2,
     &   /,'!', 3X,'Soil Litter P',              T48, F10.2, T68, F10.2,
     &   /,'!', 3X,'P in Surface Residue',       T48, F10.2, T68, F10.2)

600     FORMAT (
     &   /,'!', 3X,'Added to / Removed from Soil:'
     &   /,'!', 3X,'P in Harvest Residues from Previous Crop',
     &                                           T48, F10.2,
     &   /,'!',3X,'P from Organic Applications', T48, F10.2,
     &   /,'!',3X,'P from senesced plant matter',T48, F10.2, 
     &   /,'!',3X,'P immobilized',               T48, F10.2,
     &   /,'!',3X,'P mineralized',                           T68, F10.2)

800     FORMAT (/,'!',3X, 'Total P balance',     T48, F10.2, T68, F10.2)
900     FORMAT ('!',3X,'Balance',                            T68, F10.3)

!***********************************************************************
!***********************************************************************
!     End of DYNAMIC IF construct
!***********************************************************************
      END IF

      RETURN
      END SUBROUTINE SoilPoBal


