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

      SUBROUTINE SoilNoPoBal (CONTROL, ISWITCH, 
     &    FON, HARVRES, IMM, LITE, MNR, MULCH, N_ELEMS,   !Input
     &    NLAYR, OMADATA, SENESCE, TLITE, TSOME)          !Input

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

      CHARACTER*1   IDETL, IDETN, IDETP, ISWNIT, ISWPHO
      CHARACTER*13, PARAMETER :: SNBAL = 'SoilNoBal.OUT'
      CHARACTER*13, PARAMETER :: SPBAL = 'SoilPoBal.OUT'

      INTEGER DAS, DOY1, DOY2, DYNAMIC, IEL, INCDAT, L, LUNSPC, LUNSNC
      INTEGER N_ELEMS, NLAYR, RUN, YR1, YR2, YRDOY, YRSIM
      INTEGER, PARAMETER :: SRFC = 0  !, SOIL = 1

      REAL, DIMENSION(NELEM) :: ADDTODAY, CUMBAL, CUMIMMOB, CUMMINER 
      REAL, DIMENSION(NELEM) :: CumResE, CumResEY, CumSenE, CumSenEY 
      REAL, DIMENSION(NELEM) :: DAYBAL, HarvResE, HResESoil, HResESurf 
      REAL, DIMENSION(NELEM) :: MULCHE, ResEToday, RESLEFTEI 
      REAL, DIMENSION(NELEM) :: SenEToday, SenEYest, SubToday 
      REAL, DIMENSION(NELEM) :: TALLE, TALLEI, TLITE, TLITEI
      REAL, DIMENSION(NELEM) :: TIMMOB, TMINER, TORGE, TORGEI, TORGEY 
      REAL, DIMENSION(NELEM) :: TotLastSenes, TSOME, TSOMEI
      REAL, DIMENSION(NL) :: FON
      REAL, DIMENSION(0:NL, NELEM) :: IMM, MNR, LITE, SOM1E, LITEi

      REAL BalanceN
!     ------------------------------------------------------------------
      TYPE (ControlType)   CONTROL
      TYPE (SwitchType)    ISWITCH
      TYPE (ResidueType)   HARVRES
      TYPE (ResidueType)   SENESCE
      TYPE (MulchType)     MULCH
      TYPE (OrgMatAppType) OMAData

      LOGICAL PRINTN, PRINTP
!     ------------------------------------------------------------------
!     Return if detail not requested.
      IDETL   = ISWITCH % IDETL
      IF (IDETL  == 'N' .OR. IDETL  == '0' .OR. N_ELEMS < 1) RETURN

      IDETN   = ISWITCH % IDETN
      IDETP   = ISWITCH % IDETP
      ISWNIT  = ISWITCH % ISWNIT
      ISWPHO  = ISWITCH % ISWPHO

      PRINTN = .FALSE.
      PRINTP = .FALSE.
      IF (IDETN .NE. 'N' .AND. ISWNIT .NE. 'N') PRINTN = .TRUE.
      IF (IDETP .NE. 'N' .AND. ISWPHO .NE. 'N') PRINTP = .TRUE.
      IF (N_ELEMS < 2) PRINTP = .FALSE.
      IF (.NOT. PRINTN .AND. .NOT. PRINTP) RETURN

!     Transfer values from constructed data types into local variables.
      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

      CumResE = OMADATA % CumResE
      CumSenE = SENESCE % CumResE
      MULCHE(P) = MULCH % MULCHP
      MULCHE(N) = MULCH % MULCHN

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization phase
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
!     ------------------------------------------------------------------
!     Initialize N output file
      IF (PRINTN) THEN
        CALL GETLUN(SNBAL, LUNSNC)
        INQUIRE (FILE = SNBAL, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = LUNSNC, FILE = SNBAL, STATUS = 'OLD',
     &      POSITION = 'APPEND')
          WRITE(LUNSNC,'(/,"!",79("="))') 
        ELSE
          OPEN (UNIT = LUNSNC, FILE = SNBAL, STATUS = 'NEW')
          WRITE(LUNSNC,'("*SOIL ORGANIC N BALANCE")')
        ENDIF

        CALL HEADER (SEASINIT, LUNSNC, RUN)
      ENDIF

!     Initialize P output file
      IF (PRINTP) THEN
        CALL GETLUN(SPBAL, LUNSPC)
        INQUIRE (FILE = SPBAL, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = LUNSPC, FILE = SPBAL, STATUS = 'OLD',
     &      POSITION = 'APPEND')
          WRITE(LUNSPC,'(/,"!",79("="))') 
        ELSE
          OPEN (UNIT = LUNSPC, FILE = SPBAL, STATUS = 'NEW')
          WRITE(LUNSPC,'("*SOIL ORGANIC P BALANCE")')
        ENDIF

        CALL HEADER (SEASINIT, LUNSPC, RUN)
      ENDIF

      CALL YR_DOY(INCDAT(YRDOY,-1), YR2, DOY2)
      DO IEL = 1, N_ELEMS
        IF (INDEX('FQ',CONTROL % RNMODE) > 0 .AND. RUN > 1) THEN
!       Harvest residue plus left-over senesced material from previous
!       crop.
          HResESurf(IEL) = HARVRES % ResE(SRFC,IEL)   !surface kg[E]/ha
          HResESoil(IEL) = 0.0                        !soil    kg[E]/ha
!         For N balance, subtract out harvest redidue from initial value
          LITEi(SRFC,IEL) = LITE(0,IEL) - HARVRES % ResE(SRFC,IEL)
          DO L = 1, NLAYR
            HResESoil(IEL) = HResESoil(IEL) + HARVRES % ResE(L,IEL)
!           For N balance, subtract out harvest redidue from initial value
            LITEi(L,IEL) = LITE(L,IEL) - HARVRES % ResE(L,IEL)
          ENDDO
          HarvResE(IEL) = HResESurf(IEL) + HResESoil(IEL)
        ELSE
          HResESurf(IEL) = 0.0
          HResESoil(IEL) = 0.0
          HarvResE(IEL)  = 0.0
        ENDIF

!       Initial value for surface litter.
        RESLEFTEI(IEL) = MULCHE(IEL) - HResESurf(IEL)

!       Initial value for soil litter. Because 
!       HarvResE(IEL) has been added to FON (and thus is part of TLITE),
!       remove it here, so as to present them separately.
        TLITEI(IEL) = MAX(0.0, TLITE(IEL) - HResESoil(IEL))

        TSOMEI(IEL) = TSOME(IEL)

!       Sum all the surface and soil organic IEL pools.
        TORGEI(IEL) = TSOMEI(IEL) + TLITEI(IEL) + RESLEFTEI(IEL)

!       If detailed printout requested, print daily soil IEL balance
        IF (INDEX('AD',IDETL) > 0) THEN
!         Cumulative values (yesterday)
          CumResEY(IEL) = 0.0
          CumSenEY(IEL) = 0.0
          DAYBAL(IEL)   = 0.0
          CUMBAL(IEL)   = 0.0
          SenEYest(IEL) = 0.0
          TORGEY(IEL)   = TORGEI(IEL)
          ADDTODAY(IEL) = HarvResE(IEL)
          SubToday(IEL) = 0.0
          IF (IEL == N .AND. PRINTN) THEN
            WRITE(LUNSNC,10)
!           Write initial conditions output to SoilNoBal.OUT.
            WRITE (LUNSNC,50) YR2, DOY2, DAS, TSOME(N), TLITE(N), 
     &      MULCHE(N), 0.0, 0.0, 0.0, 
     &      0.0, 0.0, 0.0
          ENDIF
          IF (IEL == P .AND. PRINTP) THEN
            WRITE(LUNSPC,15)
!           Write initial conditions output to SoilPoBal.OUT.
            WRITE (LUNSPC,55) YR2, DOY2, DAS, TSOME(P), TLITE(P), 
     &      MULCHE(P), 0.0, 0.0, 0.0, 
     &      0.0, 0.0, 0.0
          ENDIF
        ENDIF

        TORGEY(IEL)   = TORGEI(IEL) + HarvResE(IEL)
      ENDDO   !End of IEL loop

      CUMMINER = 0.0
      CUMIMMOB = 0.0

   10 FORMAT('!',17X,'----- STATE VARS ------   ---- ADDED TODAY',
     &    ' ---- REMOVED     DAILY     CUMUL',/,
     &    '@YEAR DOY   DAS      HUMN     FON    RESN',
     &    '   ARESN   ASENN AIMMOBN RMINERP',
     &    '      DBAL      CBAL')

   15 FORMAT('!',17X,'----- STATE VARS -----   ------- ADDED --',
     &      '-----   -REM-     DAILY     CUMUL',/,
     &      '@YEAR DOY  DAS      HUMP     FOP    RESP   ARESP   ASENP',
     &      ' AIMMOBP RMINERP      DBAL      CBAL')

      SOM1E = 0.0
      LITE(0,N) = MULCH % MULCHN
      DO L = 1, NLAYR
        LITE(L,N) = FON(L)
      ENDDO

      CALL SoilNBalSum (CONTROL, 
     &    CUMRESN=CUMRESE(N), HARVRESN=HARVRESE(N), 
     &    LITE=LITEI, CumSenN=CumSenE(N), SOM1E=SOM1E, TLITN=TLITEI(N), 
     &    TSOM1N=0.0, TSOM2N=TSOME(N), TSOM3N=0.0)
      ENDIF !DYNAMIC

!***********************************************************************
!***********************************************************************
!     Daily output
!***********************************************************************
      IF (DYNAMIC == OUTPUT) THEN
!     ------------------------------------------------------------------
      DO IEL = 1, N_ELEMS
        TMINER(IEL) = MNR(0,IEL)
        TIMMOB(IEL) = IMM(0,IEL)
!       Combine mineralization in surface and top soil layer and take the profile sum.
!       *** This time the profile sum includes the SRFC value also! ***
        DO L = 1, NLAYR
!         Mineralization.
          TMINER(IEL) = TMINER(IEL) + MNR(L,IEL)
!         Immobilization.
          TIMMOB(IEL) = TIMMOB(IEL) + IMM(L,IEL)
        ENDDO

!       Accumulate immobilization and mineralization over time.
        CUMMINER(IEL) = CUMMINER(IEL) + TMINER(IEL)
        CUMIMMOB(IEL) = CUMIMMOB(IEL) + TIMMOB(IEL)

        IF (INDEX('AD',IDETL) > 0) THEN
!         Compute daily rates from cumulative values
          ResEToday(IEL) = CumResE(IEL) - CumResEY(IEL)
          SenEToday(IEL) = CumSenE(IEL) - CumSenEY(IEL)

!         Total surface and soil organic IEL
          TORGE(IEL) = TSOME(IEL) + TLITE(IEL) + MULCHE(IEL) 

!         Additions to and removals from the field.
          ADDTODAY(IEL) = ResEToday(IEL) + SenEYest(IEL) + TIMMOB(IEL)
          SubToday(IEL) = TMINER(IEL)

          DAYBAL(IEL) = TORGE(IEL) - TORGEY(IEL) - ADDTODAY(IEL) + 
     &      SubToday(IEL)
          CUMBAL(IEL) = CUMBAL(IEL) + DAYBAL(IEL)

          CALL YR_DOY(YRDOY, YR2, DOY2)
          IF (IEL == N .AND. PRINTN) THEN
!           Write daily output to SoilNoBal.OUT.
            WRITE (LUNSNC,50) YR2, DOY2, DAS, TSOME(N), TLITE(N), 
     &        MULCHE(N), ResEToday(N), SenEToday(N), TIMMOB(N), 
     &        TMINER(N), DAYBAL(N), CUMBAL(N)
   50       FORMAT(I5, I4.3, I6, 1X, F9.2, 6F8.3, 2F10.3)
          ENDIF

          IF (IEL == P .AND. PRINTP) THEN
!           Write daily output to SoilPoBal.OUT.
            WRITE (LUNSPC,55) YR2, DOY2, DAS, TSOME(P), TLITE(P), 
     &        MULCHE(P), ResEToday(P), SenEToday(P), TIMMOB(P), 
     &        TMINER(P), DAYBAL(P), CUMBAL(P)
   55       FORMAT(I5, I4.3, I5, 1X, F9.2, 3F8.3, F8.4, 2F8.3, 2F10.3)
          ENDIF

!         Save today's cumulative values for use tomorrow
          CumResEY(IEL) = CumResE(IEL)
          CumSenEY(IEL) = CumSenE(IEL)
          TORGEY(IEL)   = TORGE(IEL)
          SenEYest(IEL) = SenEToday(IEL)
        ENDIF
      ENDDO   !End of IEL loop

!***********************************************************************
!***********************************************************************
!     Seasonal output 
!***********************************************************************
      ELSEIF (DYNAMIC == SEASEND) THEN
!     ------------------------------------------------------------------
      DO IEL = 1, N_ELEMS
!       Sum all the surface and soil organic IEL pools.
        TORGE(IEL) = TSOME(IEL) + TLITE(IEL) + MULCHE(IEL)

!       Add the IEL from organic residues applied during the growth period
!       and sum this with the initial TALLEI to make the balance fit with
!       the final TALLE. Senesced material is included in the final litter
!       pools, and thus can be considered as input on the initial soil-IEL 
!       content. 
        TALLEI(IEL) = TORGEI(IEL) + CumResE(IEL) + CumSenE(IEL) + 
     &        CUMIMMOB(IEL) + HarvResE(IEL)

!       IEL balance will be off by the amount of IEL uptake on the last day 
!         of season because this amount has been added to the IEL uptake by
!       plant, but has not been subtracted from soil.  This is because
!       soil processes are computed before plant processes.
!       Need to subtract this amount from balance.
        TotLastSenes(IEL) = 0.0
        DO L = 0, NL
          TotLastSenes(IEL) = TotLastSenes(IEL) + SENESCE % ResE(L,IEL)
        ENDDO
        TALLEI(IEL) = TALLEI(IEL) - TotLastSenes(IEL)

!       Sum the initial value of all abiotic IEL pools (soil, air,
!       fertilizer), SOM and cumulative mineralization.
        TALLE(IEL) = TORGE(IEL) + CUMMINER(IEL)

!       Write end-of-season output.
        CALL YR_DOY(YRSIM, YR1, DOY1)
        CALL YR_DOY(YRDOY, YR2, DOY2)

        IF (IEL == N .AND. PRINTN) THEN
          WRITE (LUNSNC,100) YR1, DOY1, YR2, DOY2

          WRITE (LUNSNC, 110) TSOMEI(N), TSOME(N), TLITEI(N), TLITE(N), 
     &      RESLEFTEI(N), MULCHE(N)

          WRITE (LUNSNC,120) HarvResE(N), CumResE(N), CumSenE(N), 
     &      CUMIMMOB(N),CUMMINER(N)

          WRITE(LUNSNC,130) TALLEI(N), TALLE(N)
          BalanceN = TALLE(N) - TALLEI(N)
          WRITE(LUNSNC,140) BalanceN

          CLOSE (UNIT = LUNSNC)

        ENDIF

        IF (IEL == P .AND. PRINTP) THEN
          WRITE (LUNSPC,200) YR1, DOY1, YR2, DOY2

          WRITE (LUNSPC, 210) TSOMEI(P), TSOME(P), TLITEI(P), TLITE(P), 
     &      RESLEFTEI(P), MULCHE(P)

          WRITE (LUNSPC,220) HarvResE(P), CumResE(P), CumSenE(P), 
     &      CUMIMMOB(P),CUMMINER(P)

          WRITE(LUNSPC,230) TALLEI(P), TALLE(P)
          WRITE(LUNSPC,240) TALLE(P) - TALLEI(P)

          CLOSE (UNIT = LUNSPC)
        ENDIF
      ENDDO   !End of IEL loop

      SOM1E = 0.0
      LITE(0,N) = MULCH % MULCHN
      DO L = 1, NLAYR
        LITE(L,N) = FON(L)
      ENDDO

      CALL SoilNBalSum (CONTROL, 
     &    CUMRESN=CUMRESE(N), HARVRESN=HARVRESE(N), 
     &    LITE=LITE, CumSenN=CumSenE(N), SOM1E=SOM1E, TLITN=TLITE(N),
     &    TSOM1N=0.0, TSOM2N=TSOME(N), TSOM3N=0.0, Balance=BalanceN,
     &    CUMMINER=CUMMINER(N), CUMIMMOB=CUMIMMOB(N))

!***********************************************************************
!***********************************************************************
!     End of DYNAMIC IF construct
!***********************************************************************
      END IF

      RETURN
100   FORMAT (//,'!',T42,'INITIAL Year/Doy', T64, 'FINAL Year/Doy', 
     & /,'!',T49, I5,'/',I3.3, T69,I5,'/',I3.3,
     & /,'!','SOIL N BALANCE',T49,'-----------kg N/ha-----------')

200   FORMAT (//,'!',T42,'INITIAL Year/Doy', T64, 'FINAL Year/Doy', 
     & /,'!',T49, I5,'/',I3.3, T69,I5,'/',I3.3,
     & /,'!','SOIL P BALANCE',T49,'-----------kg P/ha-----------')


110   FORMAT (/,'!', 3X, 'Soil Humus N',       T48, F10.2, T68, F10.2,
     & /,'!', 3X,'Soil Litter N',              T48, F10.2, T68, F10.2,
     & /,'!', 3X,'N in Surface Residue',       T48, F10.2, T68, F10.2)

210   FORMAT (/,'!', 3X, 'Soil Humus P',       T48, F10.2, T68, F10.2,
     & /,'!', 3X,'Soil Litter P',              T48, F10.2, T68, F10.2,
     & /,'!', 3X,'P in Surface Residue',       T48, F10.2, T68, F10.2)

120   FORMAT (
     & /,'!',3X,'Added to / Removed from Soil:'
     & /,'!',3X,'N in Harvest Residues from Previous Crop',
     &                                         T48, F10.2,
     & /,'!',3X,'N from Organic Applications', T48, F10.2,
     & /,'!',3X,'N from senesced plant matter',T48, F10.2, 
     & /,'!',3X,'N immobilized',               T48, F10.2,
     & /,'!',3X,'N mineralized',                           T68, F10.2)

220   FORMAT (
     & /,'!', 3X,'Added to / Removed from Soil:'
     & /,'!', 3X,'P in Harvest Residues from Previous Crop',
     &                                         T48, F10.2,
     & /,'!',3X,'P from Organic Applications', T48, F10.2,
     & /,'!',3X,'P from senesced plant matter',T48, F10.2, 
     & /,'!',3X,'P immobilized',               T48, F10.2,
     & /,'!',3X,'P mineralized',                           T68, F10.2)

130   FORMAT (/,'!',3X, 'Total N balance',     T48, F10.2, T68, F10.2)
230   FORMAT (/,'!',3X, 'Total P balance',     T48, F10.2, T68, F10.2)

140   FORMAT ('!',3X,'Balance',                            T68, F10.3)
240   FORMAT ('!',3X,'Balance',                            T68, F10.3)


      END SUBROUTINE SoilNoPoBal

