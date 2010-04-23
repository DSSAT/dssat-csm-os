!=======================================================================
C  SoilNiBal, Subroutine
C 
C  Purpose: Provides seasonal inorganic soil N balance.  
C     Based on SoilNBal.FOR.
C
C  REVISION   HISTORY
C  03/04/2005 CHP wrote based on SoilNBal
!=======================================================================

      SUBROUTINE SoilNiBal (CONTROL, ISWITCH, 
     &    ALGFIX, CIMMOBN, CMINERN, CUMFNRO, FERTDATA, NBUND, TLCH,  
     &    TNH4, TNO3, TNOX, TOTAML, TOTFLOODN, TUREA, WTNUP) 

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      USE Interface_SoilNBalSum
      IMPLICIT NONE
      SAVE
!     ------------------------------------------------------------------
      LOGICAL FEXIST

      CHARACTER*1  IDETN, IDETL, ISWNIT
      CHARACTER*13, PARAMETER :: SNiBAL = 'SoilNiBal.OUT'

      INTEGER DAS, DYNAMIC, INCDAT, YRDOY
      INTEGER YRSIM, RUN, LUNSNC, NBUND
      INTEGER YR, DOY, YRI, DOYI

      REAL ALGFIX, ALGFIXI, AMTFER, TALLN, TALLNI, TLCH, TNH4, TNH4I,
     &  TNO3, TNO3I,  TNOX, TUREA, TUREAI, WTNUP
      REAL TOTAML, CUMFNRO, TOTFLOODN, TOTFLOODNI
      REAL STATEN, BALANCE

      REAL LCHTODAY, NOXTODAY, IMMOBTODAY, MINERTODAY
      REAL WTNUPTODAY, AMLTODAY, FNROTODAY, AMTFERTODAY
      REAL TLCHY, TNOXY, WTNUPY, CIMMOBY, CMINERY
      REAL TOTAMLY, CUMFNROY, AMTFERY
      REAL TOTSTATE, TOTADD, TOTSUB, DAYBAL, TOTSTATY, CUMBAL
      REAL CIMMOBN, CMINERN

!     ------------------------------------------------------------------
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (FertType)    FertData

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

!     Sum the initial value of all abiotic N pools (soil, air)
      TALLNI = TNO3I + TNH4I

      TOTFLOODNI = TOTFLOODN
      ALGFIXI = ALGFIX

!     If detailed printout requested, print daily soil N balance
      IF (INDEX('AD',IDETL) > 0) THEN
!       Cumulative values (yesterday)
!       Save today's cumulative values for use tomorrow
        TLCHY    = 0.0
        TNOXY    = 0.0
        WTNUPY   = 0.0
        TOTAMLY  = 0.0
        CUMFNROY = 0.0
        AMTFERY  = 0.0
        CMINERY = 0.0
        CIMMOBY = 0.0
        TOTSTATY = TNO3I + TNH4I + TUREAI + ALGFIXI + TOTFLOODNI
        !FLOODNY  = TOTFLOODNI

        CUMBAL   = 0.0
        DAYBAL = 0.0
        CALL YR_DOY(INCDAT(YRDOY,-1), YR, DOY)
        
        WRITE(LUNSNC,10)
   10     FORMAT('!',15X,'----------- STATE VARIABLES -----------',
     &    '  --- ADDED ----   --------------- REMOVED TODAY -----',
     &    '----------     DAILY     CUMUL',/,
     &    '@YEAR DOY  DAS      NO3     NH4   TUREA  FLOODN  ALGFIX',
     &    '   AFERT  AMINER    RLCH    RNOX    RNUP    RAML    RNRO',
     &    '  RIMMOB      DBAL      CBAL')
        WRITE (LUNSNC,50) YR, DOY, 0, 
     &    TNO3, TNH4, TUREA, TOTFLOODN, ALGFIX, 
     &    0.0, 0.0, 
     &    0.0, 0.0, 0.0, 
     &    0.0, 0.0, 0.0, 0.0, 0.0
      ENDIF

      CALL SoilNBalSum (CONTROL, TNH4=TNH4, TNO3=TNO3)

!***********************************************************************
!***********************************************************************
!     Daily output
!***********************************************************************
      ELSEIF (DYNAMIC == OUTPUT) THEN
!     ------------------------------------------------------------------
      IF (INDEX('AD',IDETL) > 0) THEN
        !Compute daily rates from cumulative values

!       Additions:
        AMTFERTODAY = AMTFER  - AMTFERY
        MINERTODAY  = CMINERN - CMINERY

!       Subtractions:
        LCHTODAY   = TLCH - TLCHY
        NOXTODAY   = TNOX - TNOXY
        WTNUPTODAY = (WTNUP - WTNUPY) * 10.
        AMLTODAY   = TOTAML - TOTAMLY
        FNROTODAY  = CUMFNRO - CUMFNROY
        IMMOBTODAY = CIMMOBN - CIMMOBY

        !FLOODNTODAY = TOTFLOODN - FLOODNY

        TOTSTATE = TNO3 + TNH4 + TUREA + ALGFIX + TOTFLOODN
        TOTADD   = AMTFERTODAY + MINERTODAY
        TOTSUB   = LCHTODAY  + NOXTODAY  + WTNUPTODAY + AMLTODAY + 
     &                FNROTODAY + IMMOBTODAY
        DAYBAL = TOTSTATE - TOTSTATY - TOTADD + TOTSUB
        CUMBAL   = CUMBAL + DAYBAL

        CALL YR_DOY(YRDOY, YR, DOY)
!       Write daily output to SoilNiBal.OUT.
        WRITE (LUNSNC,50) YR, DOY, DAS, 
     &    TNO3, TNH4, TUREA, TOTFLOODN, ALGFIX, 
     &    AMTFERTODAY, MINERTODAY, 
     &    LCHTODAY, NOXTODAY, WTNUPTODAY, 
     &    AMLTODAY, FNROTODAY, IMMOBTODAY, DAYBAL, CUMBAL
   50     FORMAT(I5, I4.3, I5, 1X, 13F8.3, 2F10.3)

!       Save today's cumulative values for use tomorrow
        AMTFERY  = AMTFER
        CUMFNROY = CUMFNRO
        !FLOODNY  = TOTFLOODN
        CIMMOBY  = CIMMOBN
        CMINERY  = CMINERN
        TLCHY    = TLCH
        TNOXY    = TNOX
        TOTAMLY  = TOTAML
        WTNUPY   = WTNUP

        TOTSTATY = TOTSTATE
      ENDIF

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

!       Sum state N at end of season
        STATEN = TNO3 + TNH4 + TUREA          !State end of day

!       Sum the initial value of all abiotic N pools (soil, air,
!       fertilizer), SOM and N uptake (WTNUP multiplied by 10 to
!       convert from g/m2 to kg/ha). Deduct the N in senesced material
!       from the N removed by the plant, because senesced material has
!       been returned to the soil.
        TALLN = STATEN +                          !State end of day
     &          TLCH + TNOX + WTNUP * 10. +       !Losses
     &          TOTAML + CIMMOBN           !Losses

!       Write output to NBAL.OUT.
        WRITE (LUNSNC,100) YRI, DOYI, YR, DOY

        WRITE (LUNSNC, 200) TNO3I, TNO3, TNH4I, TNH4, TUREAI, TUREA

        IF (NBUND .GT. 0) THEN
          WRITE(LUNSNC,300) TOTFLOODNI, TOTFLOODN, ALGFIXI, ALGFIX
          TALLN = TALLN + TOTFLOODN + ALGFIX
        ENDIF

        WRITE (LUNSNC,600) AMTFER, CMINERN, TLCH, TNOX, 
     &        WTNUP * 10., TOTAML, CIMMOBN

        IF (NBUND .GT. 0) THEN
          TALLN = TALLN + CUMFNRO     !Factor in runoff over bund
          WRITE(LUNSNC,700) CUMFNRO
        ENDIF

        WRITE(LUNSNC,800) TALLNI, TALLN
        BALANCE = TALLN - TALLNI
        WRITE(LUNSNC,900) BALANCE

        CLOSE (UNIT = LUNSNC)

100     FORMAT (//,'!',T42,'INITIAL Year/Doy', T64, 'FINAL Year/Doy', 
     &   /,'!',T49, I5,'/',I3.3, T69,I5,'/',I3.3,
     &    /,'!','SOIL INORGANIC N BALANCE',T49,
     &        '-----------kg N/ha-----------')

200     FORMAT (
     &     /,'!', 3X, 'Soil NO3',             T48, F10.2, T68, F10.2,
     &     /,'!', 3X, 'Soil NH4',             T48, F10.2, T68, F10.2,
     &     /,'!', 3X, 'Soil Urea',            T48, F10.2, T68, F10.2)

300     FORMAT (
     &       '!', 3X, 'Flood water N',        T48, F10.2, T68, F10.2,
     &     /,'!', 3X, 'Algae N',              T48, F10.2, T68, F10.2)

600     FORMAT (
     &     /,'!', 3X, 'Added to / Removed from Soil:'
     &     /,'!', 3X, 'Fertilizer N',         T48, F10.2,
     &     /,'!', 3X, 'Mineralized N',        T48, F10.2,

     &     /,'!', 3X, 'Leached NO3',                      T68, F10.2,
     &     /,'!', 3X, 'N Denitrified',                    T68, F10.2,
     &     /,'!', 3X, 'N Uptake From Soil',               T68, F10.2,
     &     /,'!', 3X, 'Ammonia volatilization',           T68, F10.2,
     &     /,'!', 3X, 'N immobilized',                    T68, F10.2)

700     FORMAT ('!',3X,'N in runoff over bund',           T68, F10.2)

800     FORMAT (/,'!',3X, 'Total N balance',     T48, F10.2, T68, F10.2)
900     FORMAT ('!',3X,'Balance',                            T68, F10.3)

      CALL SoilNBalSum (CONTROL, 
     &    AMTFER, Balance, 
     &    TLCH=TLCH, TNH4=TNH4, TNO3=TNO3, 
     &    TNOX=TNOX, TOTAML=TOTAML, WTNUP=WTNUP*10.)

!***********************************************************************
!***********************************************************************
!     End of DYNAMIC IF construct
!***********************************************************************
      END IF

      RETURN
      END SUBROUTINE SoilNiBal

!=======================================================================

