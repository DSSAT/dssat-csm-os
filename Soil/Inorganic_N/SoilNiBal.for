!=======================================================================
C  SoilNiBal, Subroutine
C 
C  Purpose: Provides seasonal inorganic soil N balance.  
C     Based on SoilNBal.for.
C
C  REVISION   HISTORY
C  03/04/2005 CHP wrote based on SoilNBal
!=======================================================================

      SUBROUTINE SoilNiBal (CONTROL, ISWITCH, 
     &    ALGFIX, CIMMOBN, CMINERN, CUMFNRO, FERTDATA, NBUND, CLeach,  
     &    CNTILEDR, TNH4, TNO3, TOTAML, TOTFLOODN, TUREA, WTNUP,
     &    N2O_data) 

!     ------------------------------------------------------------------
      USE GHG_mod
      USE FertType_mod
      USE Interface_SoilNBalSum
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, INCDAT, YR_DOY
      SAVE
!     ------------------------------------------------------------------
      TYPE (ControlType), INTENT(IN) :: CONTROL
      TYPE (SwitchType),  INTENT(IN) :: ISWITCH
      TYPE (FertType),    INTENT(IN) :: FertData
      TYPE (N2O_type), INTENT(IN), OPTIONAL :: N2O_DATA

      INTEGER, INTENT(IN) :: NBUND
      REAL, INTENT(IN) :: ALGFIX, CIMMOBN, CMINERN, CUMFNRO, CLeach,   
     &  CNTILEDR, TNH4, TNO3, TOTAML, TOTFLOODN, TUREA, WTNUP
      LOGICAL FEXIST

      CHARACTER*1  IDETN, IDETL, ISWNIT
      CHARACTER*13, PARAMETER :: SNiBAL = 'SoilNiBal.OUT'

      INTEGER DAS, DYNAMIC, INCDAT, YRDOY, I
      INTEGER YRSIM, RUN, LUNSNC
      INTEGER YR, DOY, YRI, DOYI

      REAL ALGFIXI, AMTFER, TALLN, TALLNI, TNH4I, TNO3I, TUREAI
      REAL TOTFLOODNI
      REAL STATEN, BALANCE

      REAL LCHTODAY, NTILEDRTODAY, IMMOBTODAY, MINERTODAY !HJ
      REAL WTNUPTODAY, AMLTODAY, FNROTODAY, AMTFERTODAY
      REAL N2Otoday, N2today, NOtoday
      REAL CLeachY, TNTILEDRY, WTNUPY, CIMMOBY, CMINERY 
      REAL TOTAMLY, CUMFNROY, AMTFERY
      REAL TOTSTATE, TOTADD, TOTSUB, DAYBAL, TOTSTATY, CUMBAL
      REAL NGasLoss, TNGSOIL, TNGSOILI, TotNEmitted,
     &    CN2O_emitted, CN2_emitted, CNO_emitted 

      REAL N2OY, N2Y, NOY
      REAL UnreleasedN  !Slow release fertilizer

!     ------------------------------------------------------------------
!     Return if detail not requested.
      IDETL   = ISWITCH % IDETL
      IDETN   = ISWITCH % IDETN
      ISWNIT  = ISWITCH % ISWNIT
      IF (IDETL  == 'N' .OR. 
     &    IDETL  == '0' .OR.    !zero
     &    IDETN  == 'N' .OR. 
     &    ISWNIT == 'N') RETURN

!     Transfer values from constructed data types into local variables.
      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

      AMTFER = FERTDATA % AMTFER(N)

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization phase
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
!     ------------------------------------------------------------------
!     Initialize output file
      CALL GETLUN(SNiBAL, LUNSNC)
      INQUIRE (FILE = SNiBAL, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = LUNSNC, FILE = SNiBAL, STATUS = 'OLD',
     &    POSITION = 'APPEND')
        WRITE(LUNSNC,'(/,"!",79("="))') 
      ELSE
        OPEN (UNIT = LUNSNC, FILE = SNiBAL, STATUS = 'NEW')
        WRITE(LUNSNC,'("*SOIL INORGANIC N BALANCE")')
      ENDIF

      CALL HEADER(SEASINIT, LUNSNC, RUN)

!     Initial value for extractable N summed across all soil layers.
      TNO3I  = TNO3
      TNH4I  = TNH4
      TUREAI = TUREA

      IF (PRESENT(N2O_DATA)) THEN
        TNGSOIL  = N2O_DATA % TNGSoil
        TNGSOILI = TNGSoil
      ELSE
        TNGSOIL  = 0.0
        TNGSOILI = 0.0
      ENDIF

!     Sum the initial value of all abiotic N pools (soil, air)
      TALLNI = TNO3I + TNH4I + TUREA + TNGSOILI

      TOTFLOODNI = TOTFLOODN
      ALGFIXI = ALGFIX

!     If detailed printout requested, print daily soil N balance
      IF (INDEX('AD',IDETL) > 0) THEN
!       Cumulative values (yesterday)
!       Save today's cumulative values for use tomorrow
        AMTFERY  = 0.0
        CMINERY  = 0.0
        CIMMOBY  = 0.0
        CLeachY  = 0.0
        TNTILEDRY = 0.0
        WTNUPY   = 0.0
        CUMFNROY = 0.0
        TOTAMLY  = 0.0
        N2OY     = 0.0
        N2Y      = 0.0
        NOY      = 0.0

        CUMBAL   = 0.0
        DAYBAL = 0.0
        CALL YR_DOY(INCDAT(YRDOY,-1), YR, DOY)
!       HJ added RLTD for N loss to tile        
        WRITE(LUNSNC,10)
   10     FORMAT('!',15X,
     &   '|------------------- STATE VARIABLES -------------------|',
     &   '---- ADDED ---|----------------------------- REMOVED ',
     &   'TODAY --------------------------|    DAILY     CUMUL',/,
     &   '@YEAR DOY  DAS     NO3     NH4   TUREA   TNGAS  FLOODN  ALG',
     &   'FIX  UNFERT   AFERT  AMINER  RIMMOB    RLCH    RLTD    RNUP',
     &   '    RNRO    RAML   N2OED    N2ED    NOED      DBAL      CBAL')
        WRITE (LUNSNC,50) YR, DOY, 0, 
     &    TNO3, TNH4, TUREA, TNGSoil, TOTFLOODN, ALGFIX, 0.0,
     &    0.0, 0.0, 
     &    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
      ENDIF

      TOTSTATY = TNO3I + TNH4I + TUREAI + ALGFIXI + TOTFLOODNI 
     &            + TNGSoil
!     FLOODNY  = TOTFLOODNI

      CALL SoilNBalSum (CONTROL, N_inorganic=TOTSTATY)

!***********************************************************************
!***********************************************************************
!     Daily output
!***********************************************************************
      ELSEIF (DYNAMIC == OUTPUT) THEN
!     ------------------------------------------------------------------
!     Daily output only when detail switch is on
      IF (INDEX('AD',IDETL) .LE. 0) RETURN

!     Compute daily rates from cumulative values
!     Additions:
      AMTFERTODAY = AMTFER  - AMTFERY
      MINERTODAY  = CMINERN - CMINERY

!     Subtractions:
      IMMOBTODAY = CIMMOBN - CIMMOBY
      LCHTODAY   = CLeach - CLeachY
      NTILEDRTODAY = CNTILEDR - TNTILEDRY             !HJ added
      WTNUPTODAY = (WTNUP - WTNUPY) * 10.  
      FNROTODAY  = CUMFNRO - CUMFNROY
      AMLTODAY   = TOTAML - TOTAMLY

      IF (PRESENT(N2O_data)) THEN
        TNGSOIL  = N2O_DATA % TNGSoil
        N2Otoday = N2O_data % CN2O_emitted - N2OY
        N2today  = N2O_data % CN2_emitted  - N2Y 
        NOtoday  = N2O_data % CNO_emitted  - NOY
      ELSE
        TNGSOIL  = 0.0
        N2Otoday = 0.0
        N2today  = 0.0
        NOtoday  = 0.0
      ENDIF

!     FLOODNTODAY = TOTFLOODN - FLOODNY

!     For slow release fertilizers, keep track of unreleased N
      UnreleasedN = 0.0
      IF (NActiveSR .GT. 0) THEN
        DO I = 1, NSlowRelN
          IF (SlowRelN(I) % ACTIVE) THEN
            UnreleasedN = UnreleasedN + 
     &         SlowRelN(I) % N0 - SlowRelN(I) % CumRelToday
          ENDIF
        ENDDO
      ENDIF

      TOTSTATE = TNO3 + TNH4 + TUREA + ALGFIX + TOTFLOODN 
     &         + TNGSoil + UnreleasedN
      TOTADD   = AMTFERTODAY + MINERTODAY
      TOTSUB   = IMMOBTODAY + LCHTODAY + WTNUPTODAY + FNROTODAY 
     &         + AMLTODAY + N2Otoday + N2today + NOtoday
     &         + NTILEDRTODAY !HJ added

      DAYBAL = TOTSTATE - TOTSTATY - TOTADD + TOTSUB
      CUMBAL   = CUMBAL + DAYBAL

      CALL YR_DOY(YRDOY, YR, DOY)
!     Write daily output to SoilNiBal.OUT.
      WRITE (LUNSNC,50) YR, DOY, DAS, 
     &  TNO3, TNH4, TUREA, TNGSoil, TOTFLOODN, ALGFIX, 
     &  UnreleasedN,
     &  AMTFERTODAY, MINERTODAY, 
     &  IMMOBTODAY, LCHTODAY, NTILEDRTODAY, WTNUPTODAY, !HJ
     &  FNROTODAY, AMLTODAY, N2Otoday, N2today, NOtoday,
     &  DAYBAL, CUMBAL
   50 FORMAT(I5, I4.3, I5, 7F8.3, F8.1, 10F8.4, 2F10.4)

!     Save today's cumulative values for use tomorrow
      AMTFERY  = AMTFER
      CMINERY  = CMINERN
      CIMMOBY  = CIMMOBN
      CLeachY  = CLeach
      TNTILEDRY = CNTILEDR            !HJ added
      WTNUPY   = WTNUP
      CUMFNROY = CUMFNRO
      TOTAMLY  = TOTAML

      IF (PRESENT(N2O_data)) THEN
        N2OY = N2O_data % CN2O_emitted
        N2Y  = N2O_data % CN2_emitted 
        NOY  = N2O_data % CNO_emitted 
      ELSE
        N2OY  = 0.0
        N2Y   = 0.0
        NOY   = 0.0
      ENDIF

      TOTSTATY = TOTSTATE

!***********************************************************************
!***********************************************************************
!     Seasonal Output
!***********************************************************************
      ELSEIF (DYNAMIC == SEASEND) THEN
!     ------------------------------------------------------------------
!     N balance will be off by the amount of N uptake on the last day 
!       of season because this amount has been added to the N uptake by
!       plant, but has not been subtracted from soil.  This is because
!       soil processes are computed before plant processes.
!     May want to subtract this amount from balance?

        CALL YR_DOY(YRSIM, YRI, DOYI)
        CALL YR_DOY(YRDOY, YR, DOY)
!       Add the fertilizer N to the initial N pool. Also add the N from
!       organic residues applied during the growth period and sum this
!       with the initial TALLNI to make the balance fit with the final
!       TALLN. SEEDNI is not needed, because for the plant the NBAL
!       only deals with N uptake from the soil.
        TALLNI  = TALLNI + AMTFER + CMINERN !Initial + add

!       For slow release fertilizers, keep track of unreleased N
        UnreleasedN = 0.0
        IF (NActiveSR .GT. 0) THEN
          DO I = 1, NSlowRelN
            IF (SlowRelN(I) % ACTIVE) THEN
              UnreleasedN = UnreleasedN + 
     &           SlowRelN(I) % N0 - SlowRelN(I) % CumRelToday
            ENDIF
          ENDDO
        ENDIF

        IF (PRESENT(N2O_data)) THEN
          TNGSOIL  = N2O_DATA % TNGSoil
        ELSE
          TNGSOIL  = 0.0
        ENDIF

!       Sum state N at end of season
        STATEN = TNO3 + TNH4 + TUREA + TNGSoil + UnreleasedN

        IF (PRESENT(N2O_data)) THEN
          CN2O_emitted = N2O_data % CN2O_emitted
          CN2_emitted  = N2O_data % CN2_emitted
          CNO_emitted  = N2O_data % CNO_emitted 
        ELSE
          CN2O_emitted = 0.0
          CN2_emitted  = 0.0
          CNO_emitted  = 0.0 
        ENDIF
        TotNEmitted = CN2O_emitted + CN2_emitted + CNO_emitted

!       Sum the initial value of all abiotic N pools (soil, air,
!       fertilizer), SOM and N uptake (WTNUP multiplied by 10 to
!       convert from g/m2 to kg/ha). Deduct the N in senesced material
!       from the N removed by the plant, because senesced material has
!       been returned to the soil.
        TALLN = STATEN +                          !State end of day
!    &          CLeach + CNOX + WTNUP * 10. +       !Losses
!    &          TOTAML + CIMMOBN           !Losses
!               HJ adeed CNTILEDR
     &          CIMMOBN + CLeach + CNTILEDR + WTNUP * 10. + TOTAML +
     &          TotNEmitted     

!       Write output to NBAL.OUT.
        WRITE (LUNSNC,100) YRI, DOYI, YR, DOY

        WRITE (LUNSNC, 200) TNO3I, TNO3, TNH4I, TNH4, TUREAI, TUREA
        WRITE (LUNSNC, 210) TNGSOILI, TNGSoil,
     &        UnreleasedN

        IF (NBUND .GT. 0) THEN
          WRITE(LUNSNC,300) TOTFLOODNI, TOTFLOODN, ALGFIXI, ALGFIX
          TALLN = TALLN + TOTFLOODN + ALGFIX 
          STATEN = STATEN + TOTFLOODN + ALGFIX
        ENDIF
!       HJ adeed CNTILEDR
        WRITE (LUNSNC,600) AMTFER, CMINERN, 
     &    CIMMOBN, CLeach, CNTILEDR, WTNUP * 10., TOTAML, 
     &    CN2O_emitted, CN2_emitted, CNO_emitted 

        IF (NBUND .GT. 0) THEN
          TALLN = TALLN + CUMFNRO     !Factor in runoff over bund
          WRITE(LUNSNC,700) CUMFNRO
        ENDIF

        WRITE(LUNSNC,800) TALLNI, TALLN
        BALANCE = TALLN - TALLNI
        WRITE(LUNSNC,900) BALANCE

        CLOSE (UNIT = LUNSNC)

100   FORMAT (//,'!SOIL INORGANIC N BALANCE',T51,'Initial',T73,'Final',
     &  /,'!',T50,'Year/Doy', T70, 'Year/Doy', 
     &  /,'!',T49, I5,'/',I3.3, T69,I5,'/',I3.3,
     &  /,'!',T49,'(kg[N]/ha)          (kg[N]/ha)',
     &  /,'! Soil N state variables:')

200     FORMAT (
     &       '!', 3X, 'Soil NO3',             T48, F10.2, T68, F10.2,
     &     /,'!', 3X, 'Soil NH4',             T48, F10.2, T68, F10.2,
     &     /,'!', 3X, 'Soil Urea',            T48, F10.2, T68, F10.2)

210     FORMAT (
     &       '!', 3X, 'Soil N gases',         T48, F10.2, T68, F10.2,
     &     /,'!', 3x, 'Unreleased fert N',                T68, F10.2)

300     FORMAT (
     &       '!', 3X, 'Flood water N',        T48, F10.2, T68, F10.2,
     &     /,'!', 3X, 'Algae N',              T48, F10.2, T68, F10.2)

600     FORMAT (
     &     /,'! N additions:'
     &     /,'!', 3X, 'Fertilizer N',         T48, F10.2,
     &     /,'!', 3X, 'Mineralized N',        T48, F10.2,
	 
!          HJ added N loss to tile drainage
     &    //,'! N subtractions:',
     &     /,'!', 3X, 'N immobilized',                    T68, F10.2,
     &     /,'!', 3X, 'N leached',                        T68, F10.2,
     &     /,'!', 3X, 'N loss to tile drainage',          T68, F10.2,
     &     /,'!', 3X, 'N Uptake From Soil',               T68, F10.2,
     &     /,'!', 3X, 'NH3 loss',                         T68, F10.2,
     &     /,'!', 3X, 'N2O loss',                         T68, F10.2,
     &     /,'!', 3X, 'N2 loss',                          T68, F10.2,
     &     /,'!', 3X, 'NO loss',                          T68, F10.2)

700     FORMAT ('!',3X,'N in runoff over bund',           T68, F10.2)

800     FORMAT (/,'!',3X, 'Total N balance',  T48, F10.2, T68, F10.2)
900     FORMAT (  '!',3X,'Balance',                       T68, F10.3)

      NGasLoss = TOTAML + TotNEmitted

!     HJ added CNTILEDR
      CALL SoilNBalSum (CONTROL, 
     &    AMTFER, Balance, 
     &    CLeach=CLeach, CNTILEDR=CNTILEDR, N_inorganic=StateN, 
     &    WTNUP=WTNUP*10., NGasLoss=NGasLoss, CUMFNRO=CUMFNRO)

!***********************************************************************
!***********************************************************************
!     End of DYNAMIC IF construct
!***********************************************************************
      END IF

      RETURN
      END SUBROUTINE SoilNiBal

!=======================================================================

