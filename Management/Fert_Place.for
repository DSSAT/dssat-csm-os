!=======================================================================
!  Fert_Place, Subroutine
!  Determines fertilizer placement (previously FPLACE and FPLACE_C)
!-----------------------------------------------------------------------
!  Revision history
!-----------------------------------------------------------------------
!  08/08/1992 WTB Modified for version 3 input/output (matches LeGRO)  
!  02/18/1993 PWW Header revision and minor changes   
!  05/20/1999 CHP Modular format
!  09/16/1999 CHP Return changes to SNH4, SNO3, UREA as DLTSNH4, 
!                 DLTSNO3, DLTUREA  (integration performed in NTRANS)
!  01/01/2000 CHP/AJG Integrated the CENTURY-based and CERES-based SOM
!                 modules with CHP's modular format.
!  03/01/2000 AJG Added link between fertilizer incorporation and
!                 surface-residue incorporation. Renamed:
!                 IFTYPE to FERTYP, M to FERTYPE, FERCOD to FERMET.
!  03/16/2000 GH  Incorporated in CROPGRO
!  07/01/2000 GH  Renamed FERDEP to FERDEPTH, DFERT to FERDEP, 
!                 CD to CUMDEP
!  04/16/2002  GH Adjusted for crop rotations
!  08/17/2002  GH Modified for Y2K  
!  08/12/2003 CHP Added I/O error checking
!  09/29/2003 AJG Reorganized for incorporating the P module. Combined
!                 with subroutine CHEM_APP. Split off FERTILIZERTYPE
!                 as a separate subroutine.
!  12/17/2003 AJG Renamed all the CH_ and CHEM_ variables to P_ variables.
!  05/18/2004 AJG Renamed variables for P module
!                   NAPNIT to NAPFER(NELEM)
!                   AMTNIT to AMTFER(NELEM)
!  10/28/2004 CHP Fixed problem with multiple applications on same day.
!  01/18/2005 CHP Generic fertilizer routine
C  03/17/2005 CHP Moved fertilizer distribution routine to separate 
C                 subroutine to accomodate multiple applications on a
C                 single day with different fertilizer types and depths.
!  10/31/2007 CHP Added simple K model.
!  10/14/2008 CHP Added "F" option, treat same as "A" option
!-----------------------------------------------------------------------
!  Called : MgmtOps 
!  Calls  : Function IDLAYR
C=======================================================================

      SUBROUTINE Fert_Place (CONTROL, ISWITCH, 
     &  DLAYR, FLOOD, NLAYR, NSTRES, YRPLT,               !Input
     &  FERTDATA)                                         !Output

!     ------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData

      IMPLICIT  NONE
      SAVE
!     ------------------------------------------------------------------

      CHARACTER*1  IFERI, RNMODE
      CHARACTER*2  FERTYPEN
      CHARACTER*5  FERMET(NAPPL), FERTYPE_CDE(NAPPL)
      CHARACTER*6  SECTION
      CHARACTER*7  AppType
      CHARACTER*30 FILEIO 
      CHARACTER*78 MSG(10)
      CHARACTER*90 CHAR
      CHARACTER*6, PARAMETER :: ERRKEY = 'FPLACE'

      INTEGER DAP, DYNAMIC, ERRNUM, FERTDAY, FERTYPE,
     &  FOUND, FTYPEN, I, IDATE,    
     &  LINC, LNUM, LUNIO, MULTI, NAPFER(NELEM), 
     &  NFERT, NLAYR, TIMDIF
      INTEGER YR, YRDIF, YRDNIT, YRDOY, YRPLT, YRSIM
      INTEGER METFER

      INTEGER FDAY(NAPPL), FERTYP(NAPPL)

      REAL DSOILN , FERDEPTH,  !, FERMIXPERC,
     &  FERNIT, FERPHOS, FERPOT, NSTRES, SOILNC, SOILNX

      REAL FERMIXPERC

      REAL AMTFER(NELEM), ANFER(NAPPL), APFER(NAPPL), AKFER(NAPPL), 
     &  DLAYR(NL), ADDSNH4(NL), ADDSPi(NL), ADDSKi(NL),
     &  ADDSNO3(NL), ADDUREA(NL), FERDEP(NAPPL)

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (FertType)    FertData

      LOGICAL UNINCO, HASN, HASP, HASK

      REAL ADDFUREA, ADDFNO3, ADDFNH4
      REAL FLOOD, ADDOXU, ADDOXH4, ADDOXN3

!-----------------------------------------------------------------------

      IFERI = ISWITCH % IFERI

!       ===================================================
!       º    Fertilizer types as given in appendix 4,     º
!       º    Technical Report 1,IBSNAT (1986).            º
!       º                                                 º
!       º      1   = Ammonium Nitrate                     º
!       º      2   = Ammonium Sulphate                    º
!       º      3   = Ammonium Nitrate Sulphate            º
!       º      4   = Anhydrous Ammonia                    º
!       º      5   = Urea                                 º
!       º      51  = Urea Super Granule                   º
!       º      6   = Diammonium Phosphate                 º
!       º      7   = Monoammonium Phosphate               º
!       º      8   = Calcium Nitrate                      º
!       º      9   = Aqua Ammonia                         º
!       º     10   = Urea Ammonium Nitrate                º
!       º     11   = Calcium Ammonium Nitrate             º
!       º     12   = Ammonium poly-phosphate              º
!       º     13   = Single super phosphate               º
!       º     14   = Triple super phosphate               º
!       º     15   = Liquid phosphoric acid               º
!       º     16   = Potassium chloride                   º
!       º     17   = Potassium Nitrate                    º
!       º     18   = Potassium sulfate                    º
!       º     19   = Urea super granules                  º
!       º     20   = Dolomitic limestone                  º
!       º     21   = Rock phosphate                       º
!       º     22   = Calcitic limestone                   º
!       º     24   = Rhizobium                            º
!       º     26   = Calcium hydroxide                    º
!       º  19-22   = Reserved for control release fert.   º
!       ===================================================

      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
C-----------------------------------------------------------------------
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
      MULTI   = CONTROL % MULTI
      RNMODE  = CONTROL % RNMODE
      YRDIF   = CONTROL % YRDIF
      YRSIM   = CONTROL % YRSIM

!     Read FPLACE data from FILEIO.
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT = ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, 0)
      LNUM = 0

!     ------------------------------------------------------------------
!     Find AUTOMATIC MANAGEMENT Section
!     ------------------------------------------------------------------
      SECTION = '!AUTOM'
      CALL FIND (LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC

      IF (FOUND == 0) THEN
        CALL ERROR (SECTION, 42, FILEIO, LNUM)
      ELSE
        READ (LUNIO,'(//,14X,3F6.0,4X,A2)',IOSTAT=ERRNUM) 
     &        DSOILN,SOILNC,SOILNX, FERTYPEN
        LNUM = LNUM + 3
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)
        READ(FERTYPEN,'(I2)',IOSTAT=ERRNUM) FTYPEN
        IF (ERRNUM .NE. 0) FTYPEN = 1
      ENDIF

!###AJG  Needs an automatic P fertilizer option in fileX ???

!     ------------------------------------------------------------------
!     Find FERTILIZER Section
!     ------------------------------------------------------------------
      SECTION = '*FERTI'
      CALL FIND (LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC

      IF (FOUND == 0) THEN
        CALL ERROR (SECTION, 42, FILEIO, LNUM)
      ELSE
        NFERT = 0

        DO I = 1, NAPPL
          READ (LUNIO, '(3X,I7,A90)', ERR = 90, END = 90) FDAY(I), CHAR
          LNUM = LNUM + 1

          READ(CHAR,'(1X,A5,1X,A5,4F6.0)',IOSTAT=ERRNUM) FERTYPE_CDE(I),
     &      FERMET(I), FERDEP(I), ANFER(I), APFER(I), AKFER(I)
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY, ERRNUM, FILEIO, LNUM)

          READ(FERTYPE_CDE(I),'(2X,I3)') FERTYP(I)
!         The number of fertilizer applications to be done in this run.
          NFERT = NFERT + 1
        ENDDO
   90   CONTINUE
      ENDIF

      CLOSE (LUNIO)

!     Check for invalid fertilizer option.
!     CHP 10/14/2008 Added "F" option
      IF (INDEX('AFRDN',IFERI) .EQ. 0) THEN
        WRITE(MSG(1),300) IFERI
        WRITE(MSG(2),310) 
        CALL WARNING(2, ERRKEY, MSG)
      ENDIF

  300 FORMAT(
     &    'Warning: The fertilizer option, "',A1,'" is not currently ')
  310 FORMAT('supported.  No fertilizer applications were added.')

!     Initialize to zero -- for all elements modeled.
      AMTFER = 0.
      NAPFER = 0

!-----------------------------------------------------------------------
!     Adjust for multi year runs
!-----------------------------------------------------------------------
      IF (MULTI > 1 .AND. NFERT > 0 .AND. IFERI .NE. 'D') THEN
!       Open the FILEIO input file.
        OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT = ERRNUM)
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, 0)

        SECTION = '*FERTI'
        CALL FIND (LUNIO, SECTION, LNUM, FOUND)

!       If the fertilizer section can't be found, call an error, or else
!       read the input data.
        IF (FOUND == 0) THEN
          CALL ERROR (SECTION, 42, FILEIO, LNUM)
        ELSE
          DO I = 1, NFERT
            READ (LUNIO, '(3X, I7)', IOSTAT = ERRNUM, ERR = 5010,
     &         END = 5010) FDAY(I)
          ENDDO
5010      CONTINUE
        ENDIF   !End of IF block on FOUND.

!       Close input file.
        CLOSE (LUNIO)

!       Adjust dates for seasonal runs.
        DO I = 1, NFERT
          CALL YR_DOY (FDAY(I), YR, IDATE)
          FDAY(I) = (YR + MULTI - 1) * 1000 + IDATE
        ENDDO
      ENDIF

!-----------------------------------------------------------------------
!     Adjust for crop rotations
!-----------------------------------------------------------------------
      IF (RNMODE == 'Q') THEN
        IF (NFERT > 0 .AND. FDAY(1) < YRSIM .AND. 
     &          IFERI .NE. 'D') THEN
          DO I = 1, NFERT
            CALL YR_DOY (FDAY(I), YR, IDATE)
            FDAY(I) = (YR + YRDIF) * 1000 + IDATE
          ENDDO
        ENDIF   !End of IF block on NFERT, FDAY and IFERI
      ENDIF   !End of IF block on RNMODE

!     ------------------------------------------------------------------
      YRDNIT = 0
      UNINCO = .FALSE.

      ADDFUREA = 0.0
      ADDFNH4  = 0.0
      ADDFNO3  = 0.0

      ADDOXU   = 0.0
      ADDOXH4  = 0.0
      ADDOXN3  = 0.0

      ADDSNH4  = 0.0
      ADDSNO3  = 0.0
      ADDUREA  = 0.0

      ADDSPi   = 0.0
      ADDSKi   = 0.0

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS 
!***********************************************************************
      ELSEIF (DYNAMIC == RATE) THEN
!-----------------------------------------------------------------------
!     Initialize daily
      ADDFUREA = 0.0
      ADDFNH4  = 0.0
      ADDFNO3  = 0.0
      ADDOXU   = 0.0
      ADDOXH4  = 0.0
      ADDOXN3  = 0.0
      ADDSNH4  = 0.0
      ADDSNO3  = 0.0
      ADDUREA  = 0.0
      ADDSPi   = 0.0
      ADDSKi   = 0.0

      FERTDAY  = 0
      FERMIXPERC = 0.0
      FERDEPTH = 0.0

      FERNIT = 0.
      FERPHOS = 0.
      FERPOT = 0.0

!     ------------------------------------------------------------------
!     Fertilize on specified dates (YYDDD format)
!     ------------------------------------------------------------------
      IF (NFERT > 0 .AND. IFERI == 'R') THEN
        DO I = 1, NFERT
          IF (YRDOY == FDAY(I)) THEN

            FERDEPTH = FERDEP(I)

C           Convert character codes for fertilizer method into integer
            READ (FERMET(I)(4:5),'(I2)') METFER
            FERTYPE  = FERTYP(I)

!           Go to FERTSECTION A of FERTILIZERTYPE to determine whether
!           the fertilizer contains N and/or P.
            CALL FERTILIZERTYPE (ISWITCH,
     &        ANFER(I), APFER(I), AKFER(I), FERTYPE, FERTYPE_CDE(I),
     &        HASN, HASP, HASK)                       !Output

            IF (HASN) THEN
!             Set the amount of N to be applied and sum total amount of
!             N fertilizer
              FERNIT    = FERNIT + ANFER(I)
              AMTFER(N) = AMTFER(N) + ANFER(I)
              NAPFER(N) = NAPFER(N) + 1
            ENDIF   !End of IF block on HASN.

            IF (HASP) THEN
!             Set the amount of P to be applied and sum total amount of
!             P fertilizer
              FERPHOS   = FERPHOS + APFER(I)
              AMTFER(P) = AMTFER(P) + APFER(I)
              NAPFER(P) = NAPFER(P) + 1
            ENDIF   !End of IF block on HASP.

            IF (HASK) THEN
!             Set the amount of K to be applied and sum total amount of
!             K fertilizer
              FERPOT   = FERPOT + AKFER(I)
              AMTFER(Kel) = AMTFER(Kel) + AKFER(I)
              NAPFER(Kel) = NAPFER(Kel) + 1
            ENDIF   !End of IF block on HASP.

            IF (FERNIT > 1.E-3 .OR. FERPHOS > 1.E-3 .OR. FERPOT > 1.E-3)
     &            THEN
              CALL FertApply(
     &        DLAYR, FERDEPTH, ANFER(I), APFER(I), AKFER(I),!Input
     &        FERTYPE, FLOOD, METFER, NLAYR, YRDOY,       !Input
     &        ADDFUREA, ADDFNH4, ADDFNO3, ADDOXU, ADDOXH4,!I/O
     &        ADDOXN3, ADDSNH4, ADDSNO3, ADDUREA, ADDSPi, ADDSKi,!I/O
     &        AppType, FERMIXPERC, FERTDAY, UNINCO)       !Output
            ENDIF

          ELSEIF (FDAY(I) > YRDOY) THEN
            EXIT                        
          ENDIF
        END DO

!     ------------------------------------------------------------------
!     Fertilize on specified days (DDD format)
!     ------------------------------------------------------------------
      ELSEIF (NFERT > 0 .AND. IFERI == 'D') THEN
        DAP = MAX (0, TIMDIF(YRPLT, YRDOY))
        DO I = 1, NFERT
          IF ((FDAY(I) .NE. 0 .AND. DAP == FDAY(I)) .OR.
     &        (FDAY(I) == 0 .AND. YRDOY == YRPLT)) THEN

            FERDEPTH = FERDEP(I)
C           Convert character codes for fertilizer method into integer
            READ (FERMET(I)(4:5),'(I2)') METFER
            FERTYPE  = FERTYP(I)
 
!           Go to FERTSECTION A of FERTILIZERTYPE to determine whether
!           the fertilizer contains N and/or P.
            CALL FERTILIZERTYPE (ISWITCH,
     &        ANFER(I), APFER(I), AKFER(I), FERTYPE, FERTYPE_CDE(I),
     &        HASN, HASP, HASK)                       !Output

            IF (HASN) THEN
!             Set the amount of N to be applied and sum total amount of
!             N fertilizer
              FERNIT    = FERNIT + ANFER(I)
              AMTFER(N) = AMTFER(N) + ANFER(I)
              NAPFER(N) = NAPFER(N) + 1
            ENDIF   !End of IF block on HASN.

            IF (HASP) THEN
!             Set the amount of P to be applied and sum total amount of
!             P fertilizer
              FERPHOS   = FERPHOS + APFER(I)
              AMTFER(P) = AMTFER(P) + APFER(I)
              NAPFER(P) = NAPFER(P) + 1
            ENDIF   !End of IF block on HASP.

            IF (HASK) THEN
!             Set the amount of K to be applied and sum total amount of
!             K fertilizer
              FERPOT   = FERPOT + AKFER(I)
              AMTFER(Kel) = AMTFER(Kel) + AKFER(I)
              NAPFER(Kel) = NAPFER(Kel) + 1
            ENDIF   !End of IF block on HASP.

            IF (FERNIT > 1.E-3 .OR. FERPHOS > 1.E-3) THEN
              CALL FertApply(
     &        DLAYR, FERDEPTH, ANFER(I), APFER(I), AKFER(I),!Input
     &        FERTYPE, FLOOD, METFER, NLAYR, YRDOY,       !Input
     &        ADDFUREA, ADDFNH4, ADDFNO3, ADDOXU, ADDOXH4,!I/O
     &        ADDOXN3, ADDSNH4, ADDSNO3, ADDUREA, ADDSPi, ADDSKi,!I/O
     &        AppType, FERMIXPERC, FERTDAY, UNINCO)       !Output
            ENDIF

          ELSEIF (FDAY(I) > DAP) THEN
            EXIT                         
          ENDIF
        END DO

!     ------------------------------------------------------------------
!     Automatic N-fertilization routine
!     ------------------------------------------------------------------
      ELSEIF (IFERI == 'A' .OR. IFERI == 'F') THEN
        IF ((1. - NSTRES)  * 100. > SOILNC
  !  Add conditions for P stress also -- something like: 
  !   &      .OR.  (1. - PSTRES1) * 100. > SOILPC)) 
     &      .AND. YRDOY > (YRDNIT + 1)) THEN
     &    
!         Go to FERTSECTION A of FERTILIZERTYPE to determine whether
!         the fertilizer contains N and/or P.
          FERTYPE  = FTYPEN
          FERDEPTH = DSOILN
          METFER = 1
          YRDNIT   = YRDOY
          CALL FERTILIZERTYPE (ISWITCH,
     &        SOILNX, 0.0, 0.0, FERTYPE, FERTYPE_CDE(I),
     &        HASN, HASP, HASK)                       !Output

          IF (HASN) THEN
!           Set the amount of N to be applied and sum total amount of
!           N fertilizer
            FERNIT    = SOILNX
            AMTFER(N) = AMTFER(N) + SOILNX
            NAPFER(N) = NAPFER(N) + 1
          ENDIF   !End of IF block on HASN.

         ! IF (HASP) THEN
!        !   Set the amount of P to be applied and sum total amount of
!        !   P fertilizer
         !   FERPHOS   = SOILPX
         !   AMTFER(P) = AMTFER(P) + SOILPX
         !   NAPFER(P) = NAPFER(P) + 1
         ! ENDIF   !End of IF block on HASP.

          IF (FERNIT > 1.E-3 .OR. FERPHOS > 1.E-3) THEN
            CALL FertApply(
     &        DLAYR, FERDEPTH, SOILNX, 0.0, 0.0,!Input
     &        FERTYPE, FLOOD, METFER, NLAYR, YRDOY,       !Input
     &        ADDFUREA, ADDFNH4, ADDFNO3, ADDOXU, ADDOXH4,!I/O
     &        ADDOXN3, ADDSNH4, ADDSNO3, ADDUREA, ADDSPi, ADDSKi,!I/O
     &        AppType, FERMIXPERC, FERTDAY, UNINCO)       !Output
          ENDIF
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
C-----------------------------------------------------------------------
      FertData % ADDFUREA= ADDFUREA
      FertData % ADDFNH4 = ADDFNH4
      FertData % ADDFNO3 = ADDFNO3

      FertData % ADDOXU  = ADDOXU
      FertData % ADDOXH4 = ADDOXH4
      FertData % ADDOXN3 = ADDOXN3

      FertData % ADDSNH4 = ADDSNH4
      FertData % ADDSNO3 = ADDSNO3
      FertData % ADDUREA = ADDUREA

      FertData % ADDSPi  = ADDSPi
      FertData % ADDSKi  = ADDSKi

      FertData % AMTFER  = AMTFER
      FertData % AppType = AppType
      FertData % FERTDAY = FERTDAY
      FertData % FERDEPTH= FERDEPTH
      FertData % FERTYPE = FERTYPE
      FertData % NAPFER  = NAPFER
      FertData % UNINCO  = UNINCO
      FertData % FERMIXPERC = FERMIXPERC

!     Transfer data to ModuleData
      CALL PUT('MGMT','FERNIT',AMTFER(N))

      RETURN
      END SUBROUTINE Fert_Place


C=======================================================================
C  FertApply, Subroutine
C
C  Distributes N fertilizer constituents to soil layers
C-----------------------------------------------------------------------
C  Revision history
C
C  03/17/2005 CHP pulled N fertilizer distribution from FPLACE 
C=======================================================================
      SUBROUTINE FertApply(
     &    DLAYR, FERDEPTH, FERNIT, FERPHOS, FERPOT,       !Input
     &    FERTYPE, FLOOD, METFER, NLAYR, YRDOY,           !Input
     &    ADDFUREA, ADDFNH4, ADDFNO3, ADDOXU, ADDOXH4,    !I/O
     &    ADDOXN3, ADDSNH4, ADDSNO3, ADDUREA, ADDSPi, ADDSKi,!I/O
     &    AppType, FERMIXPERC, FERTDAY, UNINCO)           !Output

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE FloodModule
      IMPLICIT NONE
      SAVE

!     ------------------------------------------------------------------
      CHARACTER*78 MSG(10)
      CHARACTER*6, PARAMETER :: ERRKEY = 'FPLACE'
      CHARACTER*7  AppType

      INTEGER FERTDAY, FERTYPE, I, K, IDLAYR, KMAX, L, NLAYR
      INTEGER YRDOY

      REAL CUMDEP, FERDEPTH, FERNIT, FERPHOS, FERPOT
      REAL FMIXEFF, FERMIXPERC

      REAL  
     &  DLAYR(NL), ADDSNH4(NL), ADDSPi(NL), ADDSKi(NL),
     &  ADDSNO3(NL), ADDUREA(NL), PROF(NL)

      LOGICAL UNINCO
      INTEGER KD, METFER
      REAL FME(10)    !Fertilizer mixing efficiency
      REAL ADDFUREA, ADDFNO3, ADDFNH4
      REAL FLOOD, ADDOXU, ADDOXH4, ADDOXN3

!-----------------------------------------------------------------------
!Fertilizer methods -- METFER
! @CDE METFER FME DISTR   DESCRIPTION                                            
! AP001     1   0 Surface Broadcast, not incorporated                                          
! AP002     2 100 Layers  Broadcast, incorporated                                
! AP003     3   0 Surface Banded on surface                                      
! AP004     4 100 Layers  Banded beneath surface                                 
! AP005     5             Applied in irrigation water                            
! AP006     6             Foliar spray                                           
! AP007     7 100 Deep    Bottom of hole                                         
! AP008     8             On the seed                                            
! AP009     9 100 Deep    Injected                                               
! AP011    11   0 Surface Broadcast on flooded/saturated soil, none in soil      
! AP012    12  15 Layers  Broadcast on flooded/saturated soil, 15% in soil       
! AP013    13  30 Layers  Broadcast on flooded/saturated soil, 30% in soil           
! AP014    14  45 Layers  Broadcast on flooded/saturated soil, 45% in soil       
! AP015    15  60 Layers  Broadcast on flooded/saturated soil, 60% in soil       
! AP016    16  75 Layers  Broadcast on flooded/saturated soil, 75% in soil       
! AP017    17  90 Layers  Broadcast on flooded/saturated soil, 90% in soil       
! AP018    18  92 Layers  Band on saturated soil,2cm flood, 92% in soil          
! AP019    19  95 Deep    Deeply placed urea super granules/pellets, 95% in soil 
! AP020    20 100 Deep    Deeply placed urea super granules/pellets, 100% in soil

C           MIXING EFFICIENCY BASED ON BURESH ET AL.
!                1     2     3     4     5     6     7     8     9   10   
!       METFER  11    12    13    14    15    16    17    18    19   20
      DATA FME/0.0, 0.15, 0.30, 0.45, 0.60, 0.75, 0.90, 0.92, 0.95, 1.0/

C     Uses mixing efficiency (FME) input to determine where fertilizer goes.
C     If mixing efficiency is zero all fertilizer is in floodwater 
!     (or top soil layer).
C     If mixing efficiency is 1.0 all fertilizer retained in soil.
C     Function IDLAYR is used to identify layers for deep placement.
C     Routine assumes uniform incorporation within a layer for deep
C     point placed sources.
C
C     For all BI treatments, the routine will distribute the soil fraction
C     of the fertilizer over the layers encompassed by the incorporation
C     depth.
C
C     Need to make provision for USG as a source

!       Set fertilizer mixing efficiency based on method of fert.
      SELECT CASE (METFER)
        CASE (1, 3);  FMIXEFF = FME(1)           !0% incorporation
        CASE (2, 4:9);FMIXEFF = FME(10)          !100% incorporated
        CASE (11:20); FMIXEFF = FME(METFER - 10) !Range from 0 to 1
        CASE DEFAULT; FMIXEFF = FME(10)          !Default 100% inc.
      END SELECT

      IF (FMIXEFF < 0.98) THEN
        UNINCO = .TRUE.
      ENDIF

      KMAX = 1
      PROF = 0.
      FERTDAY = YRDOY

      SELECT CASE (METFER)
        CASE (1,3,5,11)
!         Surface placement
          KMAX = 1
          PROF(1) = 1.0

        CASE (2,4,12:18)  
!       These treatments are incorporated
!       Remainder of fertilizer (FME) distributed over layers
           KMAX = IDLAYR (NLAYR,DLAYR,FERDEPTH)
           CUMDEP   = 0.0
          IF (KMAX == 1) THEN
!           Incorporation shallow - only surface layer
            PROF(1) = 1.0
          ELSE
            CUMDEP = DLAYR(1)
            PROF(1)   = DLAYR(1) / FERDEPTH

            DO L = 2, KMAX
              CUMDEP = CUMDEP + DLAYR(L)
              IF (FERDEPTH <= CUMDEP) THEN
                 PROF(L) = (FERDEPTH - (CUMDEP - DLAYR(L))) / FERDEPTH
              ELSE
                PROF(L) = DLAYR(L) / FERDEPTH
              ENDIF
            END DO
          ENDIF

        CASE (7,8,9,19,20)
!       This is deep placement
!       All fertilizer placed in layer KD with (PROF = 1.0)
          KD = IDLAYR (NLAYR,DLAYR,FERDEPTH)
          IF (KD == 1) THEN
            WRITE (MSG(1),1000) FERTYPE,  FERDEPTH
            WRITE (MSG(2),1001)
            WRITE (MSG(3),1002)
 1000       FORMAT('Fertilizer type ',I3,'; Depth ', F5.2)
 1001       FORMAT('Deep placement of fertilizer could not ',
     &          'be accomodated.')
 1002       FORMAT('Model is forcing deep placement into ',
     &          'second layer.')
            CALL WARNING(3, ERRKEY, MSG)
            KD = 2
          ENDIF
          PROF(KD) = 1.0
          KMAX     = KD

        CASE DEFAULT
          MSG(1) = "Application method not currently active in CSM."
          MSG(2) = "No fertilizer added."
          WRITE(MSG(3),'(A,I3)') "Method: ", METFER
          CALL WARNING(3,ERRKEY,MSG)
      END SELECT

      IF (KMAX > 1 .AND. FMIXEFF < 0.95) THEN
        DO I = 1, KMAX
          SELECT CASE (I)
            CASE (1)
              IF (PROF(I)*1.2 < 1.0) THEN
                PROF(I) = PROF(I)*1.2
              ENDIF

            CASE (2)
              PROF(I)   = 1.0 - PROF(I-1)

            CASE (3)
              PROF(I)   = PROF(I-1) - PROF(I)
              PROF(I)   = AMAX1 (PROF(I),0.0)
              PROF(I-1) = PROF(I-1) - PROF(I)

            CASE DEFAULT
              PROF(I)   = 0.0
          END SELECT
        END DO
      ENDIF

!       ===================================================
!       º    Fertilizer types as given in appendix 4,     º
!       º    Technical Report 1,IBSNAT (1986).            º
!       º                                                 º
!       º      1   = Ammonium Nitrate                     º
!       º      2   = Ammonium Sulphate                    º
!       º      3   = Ammonium Nitrate Sulphate            º
!       º      4   = Anhydrous Ammonia                    º
!       º      5   = Urea                                 º
!       º      51  = Urea Super Granule                   º
!       º      6   = Diammonium Phosphate                 º
!       º      7   = Monoammonium Phosphate               º
!       º      8   = Calcium Nitrate                      º
!       º      9   = Aqua Ammonia                         º
!       º     10   = Urea Ammonium Nitrate                º
!       º     11   = Calcium Ammonium Nitrate             º
!       º     12   = Ammonium poly-phosphate              º
!       º     13   = Single super phosphate               º
!       º     14   = Triple super phosphate               º
!       º     15   = Liquid phosphoric acid               º
!       º     16   = Potassium chloride                   º
!       º     17   = Potassium Nitrate                    º
!       º     18   = Potassium sulfate                    º
!       º     19   = Urea super granules                  º
!       º     20   = Dolomitic limestone                  º
!       º     21   = Rock phosphate                       º
!       º     22   = Calcitic limestone                   º
!       º     24   = Rhizobium                            º
!       º     26   = Calcium hydroxide                    º
!       º  19-22   = Reserved for control release fert.   º
!       ===================================================

!       N sources:
        SELECT CASE (FERTYPE)
!       Nitrate only
        CASE (8,17)
          ADDFNO3 = ADDFNO3 + FERNIT * (1.0 - FMIXEFF)
          DO K = 1, KMAX
             ADDSNO3(K) = ADDSNO3(K) + FERNIT * FMIXEFF * PROF(K)
          END DO

!       Ammonium only
        CASE (2,4,6,7,9,12)
          ADDFNH4 = ADDFNH4 + FERNIT * (1.0-FMIXEFF)
          DO K = 1, KMAX
             ADDSNH4(K) = ADDSNH4(K) + FERNIT * FMIXEFF * PROF(K)
          END DO

!       Ammonium nitrate
        CASE (1,3,11)
          ADDFNH4 = ADDFNH4 + FERNIT * 0.5 * (1.0 - FMIXEFF)
          ADDFNO3 = ADDFNO3 + FERNIT * 0.5 * (1.0 - FMIXEFF)
          DO K = 1, KMAX
             ADDSNH4(K) = ADDSNH4(K) + 0.5 * FERNIT * FMIXEFF * PROF(K)
             ADDSNO3(K) = ADDSNO3(K) + 0.5 * FERNIT * FMIXEFF * PROF(K)
          END DO

!       Urea only
        CASE (5,19,51)
          ADDFUREA = ADDFUREA + FERNIT * (1.0 - FMIXEFF)
          DO K = 1, KMAX
             ADDUREA(K) = ADDUREA(K) + FERNIT * FMIXEFF * PROF(K)
          END DO

!       Urea Ammonium Nitrate
        CASE (10)
          ADDFNO3  = ADDFNO3  + FERNIT * 0.25 * (1.0 - FMIXEFF)
          ADDFNH4  = ADDFNH4  + FERNIT * 0.25 * (1.0 - FMIXEFF)
          ADDFUREA = ADDFUREA + FERNIT * 0.50 * (1.0 - FMIXEFF)
          DO K = 1, KMAX
             ADDSNO3(K) = ADDSNO3(K) + FERNIT *0.25 * FMIXEFF * PROF(K)
             ADDSNH4(K) = ADDSNH4(K) + FERNIT *0.25 * FMIXEFF * PROF(K)
             ADDUREA(K) = ADDUREA(K) + FERNIT *0.50 * FMIXEFF * PROF(K)
          END DO

        END SELECT

!     -----------------------------------------------------
!       Phosphorus sources:
        SELECT CASE(FERTYPE)
        CASE (6,7,12:15,20:22)
          ADDSPi(1) = ADDSPi(1) + FERPHOS *(1.0-FMIXEFF)
          DO K = 1, KMAX
            ADDSPi(K) = ADDSPi(K) + FERPHOS * FMIXEFF * PROF(K)
          END DO
        END SELECT

!     -----------------------------------------------------
!       Potassium sources:
        SELECT CASE(FERTYPE)
        CASE (16:18)
          ADDSKi(1) = ADDSKi(1) + FERPOT *(1.0-FMIXEFF)
          DO K = 1, KMAX
            ADDSKi(K) = ADDSKi(K) + FERPOT * FMIXEFF * PROF(K)
          END DO
        END SELECT

!     -----------------------------------------------------
        IF (ABS(FLOOD) < 1.E-4) THEN
          ADDOXU  = ADDOXU  + ADDFUREA
          ADDOXH4 = ADDOXH4 + ADDFNH4
          ADDOXN3 = ADDOXN3 + ADDFNO3

          ADDUREA(1) = ADDUREA(1) + ADDFUREA
          ADDSNH4(1) = ADDSNH4(1) + ADDFNH4
          ADDSNO3(1) = ADDSNO3(1) + ADDFNO3
          ADDFUREA= 0.0
          ADDFNH4 = 0.0
          ADDFNO3 = 0.0
        ENDIF

!       Set the percentage of the surface residues that will be
!       incorporated with the fertilizer incorporation. Set to zero
!       if superficially applied or with irrigation water, and set
!       to 100 if incorporated or deeply placed.
        SELECT CASE (METFER)
          CASE (2,4,19,20); FERMIXPERC = 100. 
          CASE DEFAULT;     FERMIXPERC = 0.
        END SELECT

!       Set the percentage of fertilizer that is applied to the root zone
!       This is used in the soil inorganic phosphorus routine to compute
!       P available for uptake by roots.
        SELECT CASE (METFER)
          CASE (3,4,18); AppType = 'BANDED '
          CASE (7,8,9) ; AppType = 'HILL   '
          CASE DEFAULT ; AppType = 'UNIFORM'
        END SELECT

      RETURN
      END SUBROUTINE FertApply
C=======================================================================

C=======================================================================
C  IDLAYR, Function
C
C  Determines layer where fertilizer is placed
C-----------------------------------------------------------------------
C  Revision history
C
C  02/08/93 PWW Written
C  02/08/93 PWW Header revision and minor changes 
C-----------------------------------------------------------------------
C  INPUT  : NLAYR FLAYR FDEPTH
C  LOCAL  : L
C OUTPUT : IDLAYR
C-----------------------------------------------------------------------
C  Called : FPLACE_C, FERTILIZER
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  NLAYR  : Number of layers in soil
C  DLAYR(): Thickness increment of soil layer L - cm
C  L      : Loop counter
C  FDEPTH : Fertilizer depth (cm)
C  DEPTH  : Depth to the bottom of a layer from the surface (cm)
C=======================================================================

      INTEGER FUNCTION IDLAYR (NLAYR,DLAYR,FDEPTH)

      IMPLICIT  NONE

      INTEGER   NLAYR,L
      DIMENSION DLAYR (NLAYR)
      REAL      FDEPTH,DLAYR,DEPTH

      DEPTH  = 0.0
      IDLAYR = 1
      DO L = 1, NLAYR
         DEPTH  = DEPTH + DLAYR (L)
         IF (FDEPTH <= DEPTH) THEN
            IDLAYR = L
            GO TO 10
         ENDIF
      END DO

      IDLAYR = NLAYR

   10 CONTINUE
      RETURN
      END FUNCTION IDLAYR

!=======================================================================
!  FERTILIZERTYPE, Subroutine for fertilizer placement module.
!  Determines fertilizer type
!-----------------------------------------------------------------------
!  Revision history
!  09/26/2003 AJG Separated this code from FPLACE_C into a new 
!                 subroutine, restructured it and added P.
!  12/17/2003 AJG Renamed all the CH_ and CHEM_ variables to P_ variables.
!  05/06/2004 AJG Removed a few errors and the line in which FERTYPE 17
!                 was set back to 12.
!  01/20/2005 CHP Moved to generic fertilizer placement module, removed
!                 "_C" from name and removed fertilizer distribution.
!
!-----------------------------------------------------------------------
!  Called : FPLACE_C
!  Calls  : DISTRIB_SOILPi_C
!=======================================================================

      SUBROUTINE FERTILIZERTYPE (ISWITCH,
     &    ANFER, APFER, AKFER, FERTYPE, FERTYPE_CDE,      !Input
     &    HASN, HASP, HASK)                               !Output

!     ------------------------------------------------------------------
      USE MODULEDEFS
      IMPLICIT  NONE

      LOGICAL HASN, HASP, HASK, T, F
      INTEGER FERTYPE
      REAL ANFER, APFER, AKFER
      CHARACTER*5  FERTYPE_CDE
      CHARACTER*6, PARAMETER :: ERRKEY = 'FPLACE'
      CHARACTER*35 FERTYPE_TEXT
      CHARACTER*78 MSG(5)
      TYPE (SwitchType) ISWITCH

      PARAMETER (T=.TRUE., F=.FALSE.)
!     ------------------------------------------------------------------
      SELECT CASE(FERTYPE)
       CASE(1);  HASN = T; HASP = F; HASK = F !Ammonium Nitrate
       CASE(2);  HASN = T; HASP = F; HASK = F !Ammonium Sulphate
       CASE(3);  HASN = T; HASP = F; HASK = F !Ammonium Nitrate Sulphate
       CASE(4);  HASN = T; HASP = F; HASK = F !Anhydrous Ammonia
       CASE(5);  HASN = T; HASP = F; HASK = F !Urea
       CASE(51); HASN = T; HASP = F; HASK = F !Urea Super Granule
       CASE(6);  HASN = T; HASP = T; HASK = F !Diammonium Phosphate
       CASE(7);  HASN = T; HASP = T; HASK = F !Monoammonium Phosphate
       CASE(8);  HASN = T; HASP = F; HASK = F !Calcium Nitrate
       CASE(9);  HASN = T; HASP = F; HASK = F !Aqua Ammonia
       CASE(10); HASN = T; HASP = F; HASK = F !Urea Ammonium Nitrate
       CASE(11); HASN = T; HASP = F; HASK = F !Calcium Ammonium Nitrate
       CASE(12); HASN = T; HASP = T; HASK = F !Ammonium poly-phosphate
       CASE(13); HASN = F; HASP = T; HASK = F !Single super phosphate
       CASE(14); HASN = F; HASP = T; HASK = F !Triple super phosphate
       CASE(15); HASN = F; HASP = T; HASK = F !Liquid phosphoric acid
       CASE(16); HASN = F; HASP = F; HASK = T !Potassium chloride
       CASE(17); HASN = T; HASP = F; HASK = T !Potassium Nitrate
       CASE(18); HASN = F; HASP = F; HASK = T !Potassium sulfate
       CASE(19); HASN = T; HASP = F; HASK = F !Urea super granules
       CASE(20); HASN = F; HASP = F; HASK = F !Dolomitic limestone
       CASE(21); HASN = F; HASP = T; HASK = F !Rock phosphate
       CASE(22); HASN = F; HASP = F; HASK = F !Calcitic limestone
       CASE(24); HASN = F; HASP = F; HASK = F !Rhizobium         
       CASE(26); HASN = F; HASP = F; HASK = F !Calcium hydroxide 
       CASE DEFAULT; HASN = F; HASP = F; HASK = F 
      END SELECT

      IF (.NOT. HASN .AND. ANFER > 1.E-6 .AND. ISWITCH%ISWNIT == 'Y') 
     &          THEN
        CALL READ_DETAIL(5, 35, FERTYPE_CDE, '*Ferti', FERTYPE_TEXT)
        MSG(1) = "Invalid fertilizer data in experiment file."
        WRITE(MSG(2),'(A,A,A,A)') 
     &    "Fertilizer type: ",FERTYPE_CDE, " ",FERTYPE_TEXT
        WRITE(MSG(3),'(A,F8.1,A)')"Amount N specified: ",ANFER," kg/ha"
        MSG(4)="Check DETAIL.CDE file for valid fertilizer types."
        MSG(5)="No N applied on this date."
        CALL WARNING(5, ERRKEY, MSG)
      ENDIF

      IF (.NOT. HASP .AND. APFER > 1.E-6 .AND. ISWITCH % ISWPHO == 'Y') 
     &          THEN
        CALL READ_DETAIL(5, 35, FERTYPE_CDE, '*Ferti', FERTYPE_TEXT)
        MSG(1) = "Invalid fertilizer data in experiment file."
        WRITE(MSG(2),'(A,A,A,A)') 
     &    "Fertilizer type: ",FERTYPE_CDE, " ",FERTYPE_TEXT
        WRITE(MSG(3),'(A,F8.1,A)')"Amount P specified: ",APFER," kg/ha"
        MSG(4)="Check DETAIL.CDE file for valid fertilizer types."
        MSG(5)="No P applied on this date."
        CALL WARNING(5, ERRKEY, MSG)
      ENDIF

      IF (.NOT. HASK .AND. AKFER > 1.E-6 .AND. ISWITCH % ISWPOT == 'Y') 
     &          THEN
        CALL READ_DETAIL(5, 35, FERTYPE_CDE, '*Ferti', FERTYPE_TEXT)
        MSG(1) = "Invalid fertilizer data in experiment file."
        WRITE(MSG(2),'(A,A,A,A)') 
     &    "Fertilizer type: ",FERTYPE_CDE, " ",FERTYPE_TEXT
        WRITE(MSG(3),'(A,F8.1,A)')"Amount K specified: ",AKFER," kg/ha"
        MSG(4)="Check DETAIL.CDE file for valid fertilizer types."
        MSG(5)="No K applied on this date."
        CALL WARNING(5, ERRKEY, MSG)
      ENDIF

      RETURN
      END SUBROUTINE FERTILIZERTYPE

!=======================================================================
! FPLACE and IDLAYR Variables - updated 08/18/2003
!-----------------------------------------------------------------------
! AMTFER     Cumulative amount of N in fertilizer applications
!             (kg [N] / ha)
! ANFER(I)   Amount of nitrogen in fertilizer applied in Ith application
!             (kg [N] / ha)
! CHAR       Contains the contents of last record read 
! CONTROL    Composite variable containing variables related to control 
!              and/or timing of simulation.  The structure of the variable 
!              (ControlType) is defined in ModuleDefs.for. 
! CUMDEP     Cumulative depth of soil profile (cm)
! DAILY      Logical variable to determine whether daily or hourly flood 
!              chemistry or oxidation layer calculations are done. 
! DAP        Number of days after planting (d)
! DEPTH      Depth to the bottom of a layer from the surface (cm)
! DLAYR(L)   Thickness of soil layer L (cm)
! ADDFNH4    Rate of change of ammonium in floodwater (kg [N] / ha / d)
! ADDFNO3    Rate of change of nitrate in floodwater (kg [N] / ha / d)
! ADDFUREA   Rate of change of urea in floodwater (kg [N] / ha / d)
! ADDOXH4    Rate of change of ammonium in oxidation layer
!             (kg [N] / ha / d)
! ADDOXN3    Rate of change of nitrate in oxidation layer (kg [N] / ha / d)
! ADDOXU     Rate of change of urea in oxidation layer (kg [N] / ha / d)
! ADDSNH4(L) Rate of change of ammonium in soil layer L (kg [N] / ha / d)
! ADDSNO3(L) Rate of change of nitrate in soil layer L (kg [N] / ha / d)
! ADDUREA(L) Rate of change of urea content in soil layer L
!             (kg [N] / ha / d)
! DOY        Current day of simulation (d)
! DSOILN     Fertilizer depth for automatic fertilizer option (cm)
! ERRKEY     Subroutine name for error file 
! ERRNUM     Error number for input 
! FDAY(I)    Julian date for Ith fertilizer application (YYYYDDD)
! FDEPTH     Fertilizer depth (cm)
! FERDEP(I)  Fertilizer depth for application I (cm)
! FERDEPTH   Fertilizer depth on current day of simulation (cm)
! FERMET(I)  Fertilizer method for Ith application 
! FERNIT     Amount of nitrogen in fertilizer applied on current day of 
!              simulation (kg [N] / ha)
! FERTYP(I)  Type of fertilizer used for Ith application 
! FERTYPE    Fertilizer type for current application 
! FILEIO     Filename for input file (e.g., IBSNAT35.INP) 
! FLOOD      Current depth of flooding (mm)
! FME        Fertilizer mixing efficiency (fraction)
! FOUND      Indicator that good data was read from file by subroutine FIND 
!              (0 - End-of-file encountered, 1 - NAME was found) 
! FTYPEN     Fertilizer types 
! I          Loop counter 
! IDATE      Day of irrigation or fertilizer application (d)
! IDLAYR     Soil layer where fertilizer is placed 
! IFERI      Fertilizer switch (A,F= automatic, R= on specified dates 
!              (YYYYDDD format), D = on specified days after planting (DDD) 
!              format. 
! K          Loop counter/dummy variable 
! KD         Soil layer in which fertilizer is placed 
! KMAX       Maximum soil depth for fertilizer application (cm)
! LFD10      Date, 10 days after last fertilization.  Used to determine 
!              whether hourly flood chemistry computations will be done 
!              (see DAILY variable). (YYYYDDD)
! LINC       Line number of input file 
! LNUM       Current line number of input file 
! LUNIO      Logical unit number for FILEIO 
! MSG        Text array containing information to be written to WARNING.OUT 
!              file. 
! METFER     Numerical code for fertilizer method 
! MULTI      Current simulation year (=1 for first or single simulation, 
!              =NYRS for last seasonal simulation) 
! NAPFER     Current number of fertilizer applications applied. 
! NAPPL      Maximum number of applications (for fertilizer, irrigation, 
!              etc.) 
! NFERT      Total number of observed fertilizer applications 
! NL         Maximum number of soil layers = 20 
! NLAYR      Actual number of soil layers 
! NSTRES     Nitrogen stress factor (1=no stress, 0=max stress) 
! OXLAYR     Composite variable which contains data about oxidation layer.  
!              See ModuleDefs.for for structure. 
! PROF(L)    Proportion of soil destined fertilizer for layer L (fraction)
! RNMODE    Simulation run mode (I=Interactive, A=All treatments, 
!             B=Batch mode, E=Sensitivity, D=Debug, N=Seasonal, Q=Sequence)
! SECTION    Section name in input file 
! SOILNC     Critical threshold of soil nitrogen content to trigger 
!              automatic fertilization.  Measured as a percentage of N 
!              supply to N demand (%)
! SOILNX     Amount of N to be applied for automatic fertilization
!             (kg [N] / ha)
! UNINCO     Logical variable indicating whether fertilizer is fully 
!              incorporated; if true, ignore oxidation layer 
! YEAR       Year of current date of simulation 
! YRDIF      Increment in years which must be added to operations dates for 
!              seasonal or sequenced simulations (yr)
! YRDNIT     Date of last automatic fertilizer application (YYYYDDD)
! YRDOY      Current day of simulation (YYYYDDD)
! YRPLT      Planting date (YYYYDDD)
! YRSIM      Start of simulation date (YYYYDDD)
!======================================================================
