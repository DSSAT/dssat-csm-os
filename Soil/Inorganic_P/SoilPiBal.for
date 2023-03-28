!=======================================================================
!  SoilPiBal, Seasonal inorganic Soil P Balance

!-----------------------------------------------------------------------
!  REVISION   HISTORY
!  09/19/2003 AJG Brought into modular format and linked to the 
!                 CENTURY-based SOM/residue module.
!  03/23/2004 CHP Renamed SoilPBal and took out plant routines.
!  04/18/2005 CHP Renamed SoilPiBal and removed organic P components.
!  06/17/2006 AJG Renamed all root-zone and no-root-zone variables with
!                 Rts and noRts.
!
!=======================================================================
      SUBROUTINE SoilPiBal (CONTROL, ISWITCH,
     &    FertData, IMM, MNR, NLAYR, PUptake, 
     &    SPiActProf, SPiLabProf, SPiStaProf)

!     ------------------------------------------------------------------
      USE ModuleDefs
      USE Interface_SoilPBalSum

      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, INCDAT, YR_DOY
      SAVE
!     ------------------------------------------------------------------
      LOGICAL FEXIST

      CHARACTER*1  IDETP, IDETL, ISWPHO
      CHARACTER*13, PARAMETER :: SPBAL = 'SoilPiBal.OUT'

      INTEGER DAS, DOY, DOY1, DOY2, DYNAMIC, INCDAT, L, LUNSPC, 
     &  NLAYR, RUN, YR, YR1, YR2, YRDOY, YRSIM

      REAL SPiActProf,   SPiLabProf,   SPiStaProf
      REAL SPiActProf_I, SPiLabProf_I, SPiStaProf_I

      REAL Balance, DAYBAL, TALLP, TALLPI   !, BandedSpi
      REAL CUMBAL, CumPUptake, AddToday, SubToday
      REAL  AMTFER, AMTFERTODAY, AMTFERY,
     &  TMINER, TIMMOB,  
     &  TSOILP, TSOILPY, TSOILP_I, WTPUPTODAY, WPUPYest
      Real CUMIMMOB, CUMMINER

      REAL, DIMENSION(NL) :: PUptake
!      REAL, DIMENSION(NL) :: AddSPiLabRts, AddSPiActRts
      REAL, DIMENSION(0:NL,NELEM) :: IMM, MNR

!     ------------------------------------------------------------------
      TYPE (ControlType)   CONTROL
      TYPE (SwitchType)    ISWITCH
      TYPE (FertType)    FertData

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

      AMTFER  = FertData % AMTFER(P)

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
        WRITE(LUNSPC,'("*INORGANIC SOIL P BALANCE")')
      ENDIF

      CALL HEADER (SEASINIT, LUNSPC, RUN)

!     Initial value for extractable P summed across all soil layers.
      SPiActProf_I = SPiActProf
      SPiLabProf_I = SPiLabProf
      SPiStaProf_I = SPiStaProf

!     Sum the initial value of all abiotic P pools (soil, air)
      TSOILP_I = SPiActProf + SPiLabProf + SPiStaProf

      TMINER = 0.0
      TIMMOB = 0.0
      CumPUptake = 0.
      CUMMINER = 0.0
      CUMIMMOB = 0.0
      CUMBAL = 0.0

!     If detailed printout requested, print daily soil P balance
      IF (INDEX('AD',IDETL) > 0) THEN
!       Cumulative values (yesterday)
!       Save today's cumulative values for use tomorrow
        AMTFERY = 0.0
        WPUPYest = 0.0
        TSOILPY = TSOILP_I

!       Compute daily rates from cumulative values
        AMTFERTODAY = 0.
        WTPUPTODAY = 0.

        WRITE(LUNSPC,10)
   10   FORMAT(
     &  '!---- DATE ----    --------- STATE ----------',
     &  '     -- ADDITIONS --  -- SUBTRACTIONS --     DAILY     CUMUL',
     &  /,'@YEAR DOY   DAS     PSTAD     PACTD     PLABD',
     &  '      PAPD      PMND      PUPD      PIMD      DBAL      CBAL')

        CUMBAL = 0.0
        DAYBAL = 0.0
        CALL YR_DOY(INCDAT(YRDOY,-1), YR, DOY)
        
!       Write daily output to SOILPIBAL.OUT.
        WRITE (LUNSPC,50) YR, DOY, 0, 
     &    SPiStaProf, SPiActProf, SPiLabProf,  !0.0,
     &    AMTFERTODAY, TMINER, WTPUPTODAY, 
     &    TIMMOB, DAYBAL, CUMBAL
   50   FORMAT(1X, I4, 1X, I3.3, 1X, I5, 7F10.3, 2F10.3)
      ENDIF

      CALL SoilPBalSum(CONTROL, SPiActProf=SPiActProf, 
     &  SPiLabProf=SPiLabProf, SPiStaProf=SPiStaProf)

!***********************************************************************
!***********************************************************************
!     Daily output
!***********************************************************************
      ELSEIF (DYNAMIC == OUTPUT) THEN
!-----------------------------------------------------------------------
      WTPUPTODAY = 0.
      DO L = 1, NLAYR
        WTPUPTODAY = WTPUPTODAY + PUptake(L)
      ENDDO
      CumPUptake = CumPUptake + WTPUPTODAY

!     Combine mineralization in surface and top soil layer and take the profile sum.
!     *** This time the profile sum includes the SRFC value also! ***
      TMINER = MNR(0,P)
      TIMMOB = IMM(0,P)
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
        AMTFERTODAY = AMTFER - AMTFERY

!       Additions to and removals from the field.
!       Use yesterday's P uptake because removal from soil occurs
!         on the next day.
        AddToday = AMTFERTODAY + TMINER
        SubToday = WPUPYest + TIMMOB

!!       Report the amount of fertilizer which has been banded prior
!!       to root growth.
!        BandedSpi = SUM(AddSPiActRts) + SUM(AddSPiLabRts)

!       What has been added to the field is already part of TSOILP,
!       but is additional to the initial P, so that TADDE has to be 
!       added to TSRFCSOILEI. Similarly, TSUBE has to be subtracted.
        TSOILP = SPiActProf + SPiLabProf + SPiStaProf 
        DAYBAL = TSOILP - TSOILPY  - AddToday + SubToday
        CUMBAL = CUMBAL + DAYBAL

        CALL YR_DOY(YRDOY, YR, DOY)

!       Write daily output to SOILPIBAL.OUT.
        WRITE (LUNSPC,50) YR, DOY, DAS, 
     &    SPiStaProf, SPiActProf, SPiLabProf, 
     &    AMTFERTODAY, TMINER, 
     &    WTPUPTODAY, TIMMOB, DAYBAL, CUMBAL
     &    
!       Save today's cumulative values for use tomorrow
        AMTFERY  = AMTFER
        TSOILPY = TSOILP
        WPUPYest = WTPUPTODAY
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal Output
!***********************************************************************
      ELSEIF (DYNAMIC == SEASEND) THEN
!-----------------------------------------------------------------------
!     Add the fertilizer P to the initial P pool. Also add the P from
!       organic residues applied during the growth period and sum this
!     with the initial TALLPI to make the balance fit with the final
!     TALLP. Senesced material is included in the final litter pools,
!     and thus can be considered as input on the initial soil-P 
!     content. SEEDPI is not needed, because for the plant the PBAL
!     only deals with P uptake from the soil.
      TALLPI  = TSOILP_I + AMTFER + CUMMINER 

!     Last day's uptake was not added to soil (1-day lag), so subtract here
      CumPUptake = CumPUptake - WTPUPTODAY    

!     Sum the initial value of all abiotic P pools (soil, air,
!     fertilizer), SOM and P uptake (WTPUP multiplied by 10 to
!     convert from g/m2 to kg/ha).
!     Removed TFERT; AJG.
      TALLP = SPiActProf + SPiLabProf + SPiStaProf +  
     &            CumPUptake + CUMIMMOB

!       Write end-of-season output.
        CALL YR_DOY(YRSIM, YR1, DOY1)
        CALL YR_DOY(YRDOY, YR2, DOY2)
        WRITE (LUNSPC,100) YR1, DOY1, YR2, DOY2

      WRITE (LUNSPC,300)  
     &  SPiStaProf_I, SPiStaProf, 
     &  SPiActProf_I, SPiActProf, 
     &  SPiLabProf_I, SPiLabProf, 
     &  (SPiStaProf_I + SPiActProf_I + SPiLabProf_I), 
     &  (SPiStaProf   + SPiActProf   + SPiLabProf), 
     &  AMTFER, CUMMINER, CUMIMMOB, CumPUptake, 
     &  TALLPI, TALLP

      Balance = TALLP - TALLPI
      WRITE(LUNSPC,400) Balance

        CLOSE (UNIT = LUNSPC)

100     FORMAT (//,'!',T42,'INITIAL Year/Doy', T64, 'FINAL Year/Doy', 
     &   /,'!',T49, I5,'/',I3.3, T69,I5,'/',I3.3,
     &   /,'!','SOIL P BALANCE',T49,'-----------kg P/ha-----------')

300   FORMAT (
     & //,'!','  SOIL INORGANIC P',
     &  /,'!', 3X, 'Stable soil P',             T48, F10.3, T68, F10.3,
     &  /,'!', 3X, 'Active soil P',             T48, F10.3, T68, F10.3,
     &  /,'!', 3X, 'Labile soil P',             T48, F10.3, T68, F10.3,
     &  /,'!', 3X,                    T48,'  --------',T68,'  --------',
     &  /,'!', 3X, 'Total Inorganic P in Soil', T48, F10.3, T68, F10.3,
     & //,'!','  ADDITIONS AND REMOVALS:'
     &  /,'!', 3X, 'Fertilizer P',              T48, F10.3,
     &  /,'!', 3X, 'Mineralized P',             T48, F10.3,
     &  /,'!', 3X, 'Immobilized P',                         T68, F10.3,
     &  /,'!', 3X, 'P Uptake From Soil',                    T68, F10.3,
     &  /,'!', 3X,                    T48,'  --------',T68,'  --------',
     &  /,'!','  TOTAL soil inorganic P BALANCE',
     &                                          T48, F10.3, T68, F10.3)

400   FORMAT ('!','      Difference:',                      T68, F10.3)

      CALL SoilPBalSum(CONTROL, 
     &    AMTFER, Balance, CumPUptake=CumPUptake, SPiStaProf=SPiStaProf,
     &    SPiActProf=SPiActProf, SPiLabProf=SPiLabProf)

!***********************************************************************
!***********************************************************************
!     End of DYNAMIC IF construct
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------

      RETURN
      END Subroutine SoilPiBal

!=======================================================================


