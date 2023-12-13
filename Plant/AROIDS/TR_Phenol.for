C=======================================================================
C  TR_PHENOL, Subroutine
C
C  Determines phenological stage
C-----------------------------------------------------------------------
C  Revision history
C
C                 Written
C  08-07-1993 PWW Header revision and minor changes
C  08/29/2002 CHP/MUS Converted to modular format for inclusion in CSM.
C  02/19/2003 CHP Converted dates to YRDOY format
!  06/15/2022 CHP Added CropStatus
C=======================================================================

      SUBROUTINE TR_PHENOL (CONTROL, ISWITCH, 
     &    AGEFAC, DAYL, LEAFNO, NSTRES, SDEPTH, SOILPROP, !Input
     &    SRAD, SW, SWFAC, TGROCOM, TILNO, TMAX, TMIN,    !Input
     &    TURFAC, YRPLT,                                  !Input
     &    CUMDTT, EMAT, PLANTS, RTDEP, YRSOW,             !I/O
     &    CropStatus,                                     !Output
     &    CDTT_TP, DTT, FIELD, ISTAGE, ITRANS, LTRANS,    !Output
     &    MDATE, NDAT, NEW_PHASE, P1, P1T,P3, P4,         !Output
     &    SDTT_TP,SEEDNI, SI3, STGDOY, STNAME, SUMDTT,    !Output
     &    TAGE, TBASE, TF_GRO, WSTRES, XSTAGE, XST_TP)    !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT  NONE
      EXTERNAL YR_DOY, TR_StnameFill, TR_IPPHEN, TR_PhaseI, TaroInit, 
     &  GETLUN, TR_TRNSPL_PHENOL, WARNING, TR_Stress
      SAVE
      
      CHARACTER ISWWAT*1,ISWNIT*1,IDETO*1,PLME*1
      CHARACTER*10 STNAME(20)
      CHARACTER*78 MSG(10)

      INTEGER   L,I,L0,YRDOY,STGDOY(20),NOUTDO
      INTEGER DOY, DYNAMIC, EMAT, ICSDUR, IDUR1, ISIM, ISTAGE
      INTEGER ISM, ITDATE, ITRANS   !, IMXDAT
      INTEGER LEAFNO, MDATE, NDAS, NDAT, NLAYR
      INTEGER YR, YRPLT, YRSIM, YRSOW, CropStatus

      REAL AGEFAC, ATEMP, CDTT_TP, CNSD1, CNSD2, CSD1, CSD2
      REAL CUMDEP, CUMDTT, CUMTMP, DTT
      REAL NSTRES
      REAL P1, P1T, P3, P4, P5, P8, P9
      REAL PLANTS, RTDEP
      REAL SDAGE, SDEPTH, SDTT_TP, SEEDNI, SRAD
      REAL SUMDTT, SWFAC
      REAL TAGE, TBASE, TEMPM, TGROCOM
      REAL TILNO, TMAX, TMIN
      REAL TURFAC, WSTRES, XSTAGE, XST_TP

      REAL SI1(6), SI2(6), SI3(6), SI4(6)
      REAL DLAYR(NL), LL(NL), SW(NL)

      LOGICAL FIELD, LTRANS, PRESOW, TF_GRO, NEW_PHASE  !, PI_TF
      REAL HARVMAT
      REAL SWSD,TOPT,TNSOIL,TDSOIL
      REAL TMSOIL,ACOEF,DAYL,TH
       
!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH

!     The variable "SOILPROP" is of type "SoilType".
      TYPE (SoilType) SOILPROP

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

      ISWWAT = ISWITCH % ISWWAT
      ISWNIT = ISWITCH % ISWNIT
      IDETO  = ISWITCH % IDETO

      LL    = SOILPROP % LL
      DLAYR = SOILPROP % DLAYR
      NLAYR = SOILPROP % NLAYR
      
      CALL YR_DOY (YRDOY,YR,DOY)

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CALL YR_DOY (YRSIM,YR,ISIM)
      CALL TR_StnameFill(STNAME)

      CALL TR_IPPHEN (CONTROL,                      !Input
     &    ATEMP, P1, P3, P4, P5, PLME, SDAGE)       !Output

      CALL TR_PhaseI(CNSD1, CNSD2, CSD1, CSD2, 
     &    CUMTMP, ICSDUR, IDUR1, NEW_PHASE)

      XSTAGE     = 0.1      
      STGDOY     = 9999999   
      STGDOY(14) = YRSIM    
      MDATE      = -99      
      NDAT       = 0        
      HARVMAT    = 0

      SEEDNI     = 0.0      

      CUMDTT   = 0.0        
      SUMDTT   = 0.0        
      DTT      = 0.0        
      WSTRES = 1.0

      ! Initialze stress indices - FROM INPLNT
      DO I = 1, 6
         SI1(I) = 0.0
         SI2(I) = 0.0
         SI3(I) = 0.0
         SI4(I) = 0.0
      END DO

      P1T   = P1      
      TBASE = 10.0    
      TAGE  = SDAGE   

      LTRANS = .FALSE.
      PRESOW = .TRUE. 

      CALL TaroInit(
     &    PLME, TAGE, YRDOY, YRPLT, YRSIM, YRSOW,         !Input
     &    FIELD, ITRANS, PRESOW, TF_GRO)                  !Output

      IF (IDETO .EQ. 'Y') THEN
        CALL GETLUN('OUTO', NOUTDO)
      ENDIF
     
      IF (ITRANS .EQ. 3) THEN
        ISTAGE = 1
      ELSE
        ISTAGE = 7
      ENDIF

      NEW_PHASE = .FALSE.

!***********************************************************************
!***********************************************************************
!     Daily rate / integration calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      NEW_PHASE = .FALSE.

      CNSD1  = CNSD1  + 1.0 - NSTRES
      CNSD2  = CNSD2  + 1.0 - AGEFAC
      CSD1   = CSD1   + 1.0 - SWFAC
      CSD2   = CSD2   + 1.0 - TURFAC
      ICSDUR = ICSDUR + 1
      TEMPM = (TMAX + TMIN)*0.5

!-----------------------------------------------------------------------
!     Transplant date
!      IF (DOY .EQ. ITDATE) THEN
      IF (YRDOY .EQ. YRPLT) THEN
        FIELD = .TRUE.
        SELECT CASE (ITRANS)
          !CASE (1);    STGDOY(07) = YRDOY 
          CASE (4)
            STGDOY(10) = YRDOY
            ISTAGE     = 8        !GERMINATION
            CALL TR_PhaseI(CNSD1, CNSD2, CSD1, CSD2, 
     &        CUMTMP, ICSDUR, IDUR1, NEW_PHASE)

            !FROM PHASEI
            ISTAGE = 9            !EMERGENCE
            P9 = 100.0           
            SUMDTT = SUMDTT - P8
            !END PHASEI STUFF

          CASE (2,3)  ! No difference in taro
             CALL TR_TRNSPL_PHENOL ( 
     &        ATEMP, ITRANS, P1T, SDEPTH, TAGE, TBASE,       !Input
     &        CDTT_TP, CUMDTT, ISTAGE, P1, P8, P9, SDTT_TP,  !Output
     &        SUMDTT, XSTAGE, XST_TP)                        !Output

             STGDOY(11) = YRDOY
             CALL YR_DOY(YRPLT, YR, ITDATE)
             IF (ITDATE .GT. 200) THEN        
               TAGE = TAGE + (ITDATE - 200)   
             ENDIF                            
             LTRANS     = .TRUE. 
             TF_GRO     = .TRUE.
             NDAT       = 0
             IF (SUMDTT .GT. P1) THEN
               CALL TR_PhaseI(CNSD1, CNSD2, CSD1, CSD2, 
     &           CUMTMP, ICSDUR, IDUR1, NEW_PHASE)

               !FROM PHASEI
               ISTAGE = 2         !PAN INIT
               !END PHASEI STUFF

             ENDIF
         END SELECT
      ENDIF

!-----------------------------------------------------------------------
      IF (TF_GRO) THEN
         TOPT = 30.0         ! FROM SPECIES FILE                   
         IF (TMAX .LT. TBASE) THEN
           DTT = 0.0
         ELSEIF (TMIN .GT. TOPT) THEN
           DTT = TOPT - TBASE
           !
           ! Now, modify TMIN, TMAX based on soil conditions
           !
         ELSEIF (LEAFNO .LE. 4) THEN
           !
           ACOEF  = 0.01061 * SRAD + 0.5902
           TDSOIL = ACOEF * TMAX + (1.0 - ACOEF) * TMIN
           TNSOIL = 0.36354 * TMAX + 0.63646 * TMIN
           IF (TDSOIL .LT. TBASE) THEN
             DTT = 0.0
           ELSE
             IF (TNSOIL .LT. TBASE) THEN
                 TNSOIL = TBASE
             ENDIF
             IF (TDSOIL .GT. TOPT) THEN
                 TDSOIL = TOPT
             ENDIF

             TMSOIL = TDSOIL*(DAYL/24.)+TNSOIL*((24.-DAYL)/24.)
             IF (TMSOIL .LT. TBASE) THEN
                 DTT = (TBASE+TDSOIL)/2.0 - TBASE
             ELSE
                 DTT = (TNSOIL+TDSOIL)/2.0 - TBASE
             ENDIF
               !
               ! Statement added ... GoL and LAH, CIMMYT, 1999
               !
             DTT = AMIN1 (DTT,TOPT-TBASE)
           ENDIF
        !Now, compute DTT for when Tmax or Tmin out of range
         ELSEIF (TMIN .LT. TBASE .OR. TMAX .GT. TOPT) THEN
           DTT = 0.0
           DO I = 1, 24
             TH = (TMAX+TMIN)/2. + (TMAX-TMIN)/2. * SIN(3.14/12.*I)
             IF (TH .LT. TBASE) THEN
                TH = TBASE
             ENDIF
             IF (TH .GT. TOPT) THEN
                TH = TOPT
             ENDIF
             DTT = DTT + (TH-TBASE)/24.0
           END DO
         ELSE
           DTT = (TMAX+TMIN)/2.0 - TBASE
         ENDIF

        SUMDTT = SUMDTT + DTT
         CUMDTT = CUMDTT + DTT
         IF (ICSDUR .EQ. 0) THEN
             ICSDUR = 1
         ENDIF
      ENDIF

!-----------------------------------------------------------------------
!     Determine if new stage is reached today
      SELECT CASE (ISTAGE)
!-----------------------------------------------------------------------
        CASE (7)  !SOWING
          ! Determine sowing date
          STGDOY(ISTAGE) = YRDOY
          IF (ITRANS .EQ. 4) THEN  ! Bud or eye planting
             TF_GRO = .TRUE.
          ENDIF

          CALL TR_PhaseI(CNSD1, CNSD2, CSD1, CSD2, 
     &        CUMTMP, ICSDUR, IDUR1, NEW_PHASE)

          !FROM PHASEI
          ISTAGE = 8      !GERMINATION
          !RTDEP = AMIN1 (SDEPTH, 10.0)
          SUMDTT = 0.0
          CUMDTT = 0.0
          NDAS   = 0
          !END OF PHASEI STUFF

          L0 = 1
          IF (ISWWAT .EQ. 'Y') THEN
            CUMDEP = 0.0
            DO L = 1, NLAYR
              CUMDEP = CUMDEP + DLAYR(L)
              IF (SDEPTH .LT. CUMDEP) EXIT
            END DO
            L0 = L
          ENDIF
          RETURN

!-----------------------------------------------------------------------
        CASE (8)  !GERMINATION
          ! Determine germination date
          SWSD = 1.0                                 ! Default (PW)
          IF (ISWWAT .NE. 'N') THEN
             IF (ISWWAT .EQ. 'Y' .OR. SW(L0) .LE. LL(L0)) THEN
                SWSD = (SW(L0)-LL(L0))*0.65+(SW(L0+1)-LL(L0+1))*0.35
             ENDIF
          ENDIF

          NDAS = NDAS + 1
          
          IF (NDAS .LT. 40) THEN
             !IF (ITRANS .EQ. 2) THEN
             !   SWSD = 1.0
             !ENDIF
             IF (SWSD .LT. 0.02) THEN
                 SUMDTT = 0.               
           ! Heat unit for germination accumulated only when soil 
           !   is moist  - US Feb04
                RETURN
             ENDIF
             IF (TEMPM .LT. 15.0 .OR. TEMPM .GT. 42.0) THEN
                RETURN
             ENDIF
             IF (SUMDTT .LT. P8) THEN
                RETURN
             ENDIF
             STGDOY(ISTAGE) = YRDOY
           ELSE
             !FAILURE TO GERMINATE
             ISTAGE  = 5      !END MN FIL
             PLANTS  = 0.0
             HARVMAT = SUMDTT
          ENDIF
          CALL TR_PhaseI(CNSD1, CNSD2, CSD1, CSD2, 
     &        CUMTMP, ICSDUR, IDUR1, NEW_PHASE)

          SELECT CASE (ISTAGE)
          CASE (5)        !FAILURE TO GERMINATE
            ISTAGE = 6    !MATURITY

          CASE (8)        !GERMINATION
            !FROM PHASEI
            ISTAGE = 9    !EMERGENCE
            SUMDTT = SUMDTT - P8
            !END OF PHASEI STUFF
          END SELECT
          RETURN

!-----------------------------------------------------------------------
        CASE (9)          !EMERGENCE
          ! Determine seedling emergence date
          NDAS = NDAS + 1
          RTDEP = RTDEP + 0.075*DTT    !ROOTGR 
          IF (SUMDTT .LT. P9) THEN
             RETURN
          ENDIF
          STGDOY(ISTAGE) = YRDOY

           CALL TR_PhaseI(CNSD1, CNSD2, CSD1, CSD2, 
     &        CUMTMP, ICSDUR, IDUR1, NEW_PHASE)

          !FROM PHASEI
          ISTAGE = 1          !END JUV. PH.
          SUMDTT = SUMDTT - P9
          !LEAFNO =1

          IF (P9 .GT. 150.0) THEN
            IF (PRESOW) THEN
              MSG(1)=' Planting material may be of poor quality.'
              CALL WARNING(1, "TRPHEN", MSG)
              IF (IDETO .EQ. 'Y') THEN
                WRITE (NOUTDO,'(A78)') MSG(1)
              ENDIF
            ENDIF
          ENDIF
          RETURN

!-----------------------------------------------------------------------
        CASE (1)      !END JUV
          ! Determine end of juvenile stage
          NDAS = NDAS + 1
          IF (LTRANS .AND. TF_GRO) THEN
             NDAT = NDAT + 1
          ENDIF
          XSTAGE = SUMDTT/P1
          IDUR1  = IDUR1 + 1
          IF (SUMDTT .LT. P1) THEN
             RETURN
          ENDIF
          STGDOY(ISTAGE) = YRDOY

          CALL TR_Stress (ISTAGE, ISWWAT, ISWNIT,
     &      CNSD1, CNSD2, CSD1, CSD2, ICSDUR,  
     &      SI1, SI2, SI3, SI4)

          !FROM PHASEI
          ISTAGE = 2
	      SUMDTT = 0.0   
          !END OF PHASEI STUFF

!-----------------------------------------------------------------------
        CASE (2) 
          ! Determine end of maximum leaf growth
          NDAS = NDAS + 1
		IF (LTRANS .AND. TF_GRO) THEN
             NDAT = NDAT + 1
          ENDIF
          XSTAGE = 1.0 + 5.0*SUMDTT/(P3+P4)
          IF (ITRANS .EQ. 2 .AND. YRDOY .EQ. YRPLT) THEN
             P3 = P3 + (P1-P1T)
             MSG(1) = ' Transplanting was done too late.'
             CALL WARNING(1, "TRPHEN", MSG)
             IF (IDETO .EQ. 'Y') THEN
                WRITE (NOUTDO,'(A78)') MSG(1)
             ENDIF
          ENDIF

          IF (SUMDTT .LT. P3) THEN
	       !  MAXGRO = 0   Needed if taro and tanier combined!
             RETURN
          ENDIF
          STGDOY(ISTAGE) = YRDOY
	    ISTAGE = 4
        CASE (4)      
          ! Declining/level vegetative growth phase in taro
          NDAS = NDAS + 1
		IF (LTRANS .AND. TF_GRO) THEN
             NDAT = NDAT + 1
          ENDIF
          XSTAGE = 1.0 + 5.0*SUMDTT/(P3+P4)
		IF (SUMDTT .LT. P3+P4) THEN
	       RETURN
          ENDIF        
          CALL TR_Stress (ISTAGE, ISWWAT, ISWNIT,
     &        CNSD1, CNSD2, CSD1, CSD2, ICSDUR,  
     &        SI1, SI2, SI3, SI4)

          !FROM PHASEI
          ISTAGE = 5      !BEGIN CORM GROWTH
          SUMDTT = SUMDTT - P3 - P4  
          EMAT   = 0
	    ISM    = 0
          !END OF PHASEI STUFF


!-----------------------------------------------------------------------
        CASE (5)      
          ! Determine end of corm growth
          XSTAGE = 6.0 + 4.0*SUMDTT/P5
	    NDAS   = NDAS + 1            
          IF (LTRANS .AND. TF_GRO) THEN
             NDAT = NDAT + 1
          ENDIF
          CUMTMP = CUMTMP + TEMPM
          IF (SUMDTT .LT. 0.85*P5) THEN
             RETURN
          ENDIF

          IF (ISM .LE. 0) THEN
             STGDOY (ISTAGE) = YRDOY
             HARVMAT = P5 + HARVMAT     ! NEW VALUE
             ISM = 1
             RETURN
          ENDIF
          !
          ! Determine end of TILLER and CORMEL growth
          !
          IF (DTT .LT. 0.001 .OR. TILNO .LE. 1.0) THEN
             HARVMAT = SUMDTT
          ENDIF
          IF (LTRANS .AND. TF_GRO) THEN
             NDAT = NDAT + 1
          ENDIF
          IF (SUMDTT .LT. HARVMAT) THEN
             IF (TGROCOM .GT. 0.05) THEN
                RETURN
             ENDIF
             EMAT = EMAT + 1
             IF (EMAT .LT. 2) THEN
                RETURN
             ENDIF
          ENDIF
          STGDOY(12) = YRDOY
          CALL TR_Stress (ISTAGE, ISWWAT, ISWNIT,
     &      CNSD1, CNSD2, CSD1, CSD2, ICSDUR,  
     &      SI1, SI2, SI3, SI4)

!          !FROM PHASEI
           ISTAGE = 6      !MATURITY
!          !END OF PHASEI STUFF

!-----------------------------------------------------------------------
        CASE (6)      !MATURITY
          ! Determine physiological maturity
          STGDOY (ISTAGE) = YRDOY
          NDAS = NDAS + 1
		MDATE  = YRDOY
          CropStatus = 1

          CALL TR_Stress (ISTAGE, ISWWAT, ISWNIT,
     &      CNSD1, CNSD2, CSD1, CSD2, ICSDUR,  
     &      SI1, SI2, SI3, SI4)

          !FROM PHASEI
          ISTAGE = 20      !HARVEST
          CUMDTT = 0.0
          DTT    = 0.0
          !END OF PHASEI STUFF
          STGDOY(ISTAGE) = YRDOY

      END SELECT

!-----------------------------------------------------------------------

      CALL TR_PhaseI(CNSD1, CNSD2, CSD1, CSD2, 
     &        CUMTMP, ICSDUR, IDUR1, NEW_PHASE)

      IF (ITRANS .EQ. 2 .OR. ITRANS .EQ. 3) THEN
	   IF (ISTAGE .EQ. 6) THEN
            TF_GRO = .FALSE.
	   ENDIF
      ENDIF

C-----------------------------------------------------------------------
C        Define dates for water balance calculations
C
C        YRSIM = Start of Simulation Date
C        YRPLT = Planting Date
C        EDATE = Emergence Date
C        MDATE = Maturity Date
C        YRDOY = Year - Day of Year (Dynamic Variable)
C-----------------------------------------------------------------------

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

      END SUBROUTINE TR_PHENOL
C=======================================================================


C=======================================================================
C  TR_Stress, Subroutine
C
C  Initialization at the beginning of every stage
C-----------------------------------------------------------------------
C  Revision history
C
C  05-07-2002 CHP Written
C-----------------------------------------------------------------------
C  Called : TR_PHENOL
C=======================================================================

      SUBROUTINE TR_Stress (ISTAGE, ISWWAT, ISWNIT,
     &    CNSD1, CNSD2, CSD1, CSD2, ICSDUR,  
     &    SI1, SI2, SI3, SI4)

      IMPLICIT NONE
      CHARACTER*1 ISWNIT, ISWWAT
      INTEGER ICSDUR, ISTAGE
      REAL CNSD1, CNSD2, CSD1, CSD2
      REAL SI1(6), SI2(6), SI3(6), SI4(6)

!-----------------------------------------------------------------------
!     Initialize at the beginning of each phase

        IF (ISWWAT .EQ. 'Y') THEN
           SI1(ISTAGE) = CSD1/ICSDUR
           SI2(ISTAGE) = CSD2/ICSDUR
        ELSE
           SI1(ISTAGE) = 0.0
           SI2(ISTAGE) = 0.0
        ENDIF

        IF (ISWNIT .EQ. 'Y') THEN
           SI3(ISTAGE) = CNSD1/ICSDUR
           SI4(ISTAGE) = CNSD2/ICSDUR
        ELSE
           SI3(ISTAGE) = 0.0
           SI4(ISTAGE) = 0.0
        ENDIF

      RETURN
      END SUBROUTINE TR_Stress
C=======================================================================


C=======================================================================
C  TR_PhaseI, Subroutine
C
C  Initialization at the beginning of every stage
C-----------------------------------------------------------------------
C  Revision history
C
C  05-07-2002 CHP Written
C-----------------------------------------------------------------------
C  Called : TR_PHENOL
C=======================================================================

      SUBROUTINE TR_PhaseI (CNSD1, CNSD2, CSD1, CSD2, 
     &    CUMTMP, ICSDUR, IDUR1, NEW_PHASE)

      IMPLICIT NONE
      INTEGER ICSDUR, IDUR1
      REAL CNSD1, CNSD2, CSD1, CSD2, CUMTMP
      LOGICAL NEW_PHASE

!-----------------------------------------------------------------------
!     Initialize at the beginning of each phase

      CNSD1   = 0.0
      CNSD2   = 0.0
      CSD1    = 0.0
      CSD2    = 0.0
      CUMTMP  = 0.0
      ICSDUR  = 0
      IDUR1   = 0
      NEW_PHASE = .TRUE.

      RETURN
      END SUBROUTINE TR_PhaseI
C=======================================================================


C=======================================================================
C  TaroInit, Subroutine
C
C  Seasonal initialization
C-----------------------------------------------------------------------
C  Revision history
C
C  05-07-2002 CHP Written
C  02/19/2003 CHP Converted dates to YRDOY format
C-----------------------------------------------------------------------
C  Called : TR_PHENOL
C=======================================================================
      SUBROUTINE TaroInit(
     &    PLME, TAGE, YRDOY, YRPLT, YRSIM, YRSOW,         !Input
     &    FIELD, ITRANS, PRESOW, TF_GRO)                  !Output

      IMPLICIT NONE
      EXTERNAL INCDAT

      CHARACTER*1 PLME
      INTEGER ITRANS, YRPLT
      INTEGER YRDOY, YRSIM, YRSOW, INCDAT
      REAL TAGE 

      LOGICAL FIELD, TF_GRO, PRESOW
      

      SELECT CASE(PLME)
      CASE ('S')    
        ITRANS = 1                        !FROM IRRVAL
        TF_GRO = .TRUE.                   !FROM IPFLOD
        FIELD  = .TRUE.                   !FROM IPFLOD

      CASE ('N')
        ITRANS = 2                        !FROM IRRVAL
        IF (YRDOY .NE. YRSIM) THEN      !CHP
          ! Allow N stress during nursery development
          PRESOW  = .FALSE.               !FROM INGROW
        ENDIF

!       No planting date specified in FILEX - set here
!       This needs to go in AUTPLT (?) chp
        IF (YRPLT .LE. 0) THEN
           IF (YRSOW .GT. 0) THEN
              YRPLT = INCDAT(YRSOW, IFIX(TAGE))
           ENDIF
        ELSE
           YRSOW  = INCDAT(YRPLT, -IFIX(TAGE))
        ENDIF

        TF_GRO = .TRUE.                   !FROM IPFLOD
        FIELD  = .FALSE.                  !FROM INGROW

      CASE ('T')
        ITRANS = 3                        !FROM IRRVAL
        FIELD  = .TRUE.                   !FROM IPFLOD

!       No planting date specified in FILEX - set here
!       This needs to go in AUTPLT (?) chp
        IF (YRPLT .LE. 0) THEN
           IF (YRSOW .GT. 0) THEN
              YRPLT = INCDAT(YRSOW, IFIX(TAGE))
           ENDIF
        ENDIF

        YRSOW   = YRPLT
        TF_GRO = .FALSE.                  !FROM IPFLOD

      CASE ('P')
        ITRANS = 4                        !FROM IRRVAL
        TF_GRO = .TRUE.                   !FROM IPFLOD
!       No planting date specified in FILEX - set here
!       This needs to go in AUTPLT (?) chp
        IF (YRPLT .LE. 0) THEN
           FIELD = .TRUE.                 !FROM IPFLOD
           IF (YRSOW .GT. 0) THEN
              YRPLT = INCDAT(YRSOW, IFIX(TAGE))
           ENDIF
        ELSE
           FIELD = .FALSE.                !FROM IPFLOD
           YRSOW  = INCDAT(YRPLT, -IFIX(TAGE))
        ENDIF

      CASE DEFAULT
        ITRANS = 1                        !For taro and tanier
        FIELD  = .TRUE.                  

!       No planting date specified in FILEX - set here
!       This needs to go in AUTPLT (?) chp
        IF (YRPLT .LE. 0) THEN
           IF (YRSOW .GT. 0) THEN
              YRPLT = INCDAT(YRSOW, IFIX(TAGE))
           ENDIF
        ENDIF

        YRSOW   = YRPLT
        TF_GRO = .FALSE.                  !FROM IPFLOD

      END SELECT

      RETURN
      END SUBROUTINE TaroInit

C=======================================================================

C=======================================================================
C  TR_StnameFill, Subroutine
C
C  Initialization at the beginning of every stage
C-----------------------------------------------------------------------
C  Revision history
C
C  05-07-2002 CHP Written
C-----------------------------------------------------------------------
C  Called : TR_PHENOL
C=======================================================================

      SUBROUTINE TR_StnameFill(STNAME)

      IMPLICIT NONE
      CHARACTER*10 STNAME(20)

      STNAME(1)  = 'Establish '   ! End Juveni
      STNAME(2)  = 'Rapid Veg '   ! Pan Init
      STNAME(3)  = 'End Max Lf'   ! Heading
      STNAME(4)  = 'Begin corm'   ! Beg Gr Fil
      STNAME(5)  = 'End corm  '   ! End Mn Fil
      STNAME(6)  = 'Maturity  '   ! Maturity
      STNAME(7)  = 'Planting  '   ! Sowing
      STNAME(8)  = 'Root Form '   ! Germinate
      STNAME(9)  = '1st Lf Emg'   ! Emergence
      STNAME(10) = '          '   ! Prgerm Sow
      STNAME(11) = '          '   ! Transplant
      STNAME(12) = '          '   ! End Ti Fil
      STNAME(13) = '          '
      STNAME(14) = 'Start Sim '   ! Start Sim
      STNAME(15) = '          '
      STNAME(16) = '          '
      STNAME(17) = '          '
      STNAME(18) = '          '
      STNAME(19) = '          '
      STNAME(20) = 'Harvest   '   ! Harvest

      RETURN
      END SUBROUTINE TR_StnameFill

C=======================================================================
C  TR_IPPHEN, Subroutine
C
C  Reads FILEIO for RICE routine
C  05/07/2002 CHP Written
C  08/12/2003 CHP Added I/O error checking
C=======================================================================

      SUBROUTINE TR_IPPHEN (CONTROL,                !Input
     &    ATEMP, P1, P3, P4, P5, PLME, SDAGE)       !Output

      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT     NONE
      EXTERNAL FIND, ERROR

      CHARACTER*1  PLME
      CHARACTER*6  ERRKEY, SECTION
      PARAMETER (ERRKEY = 'IPRICE')
      CHARACTER*30 FILEIO
      INTEGER LINC, LNUM, LUNIO, ERR, FOUND
      REAL ATEMP, G2, G4, G3, P1, P3, P4, P5, PHINT, PCINT, PCGRD
	REAL SDAGE

C     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL
      FILEIO = CONTROL % FILEIO
      LUNIO  = CONTROL % LUNIO

!-----------------------------------------------------------------------
!       Read data from FILEIO for use in PLANT module
!-----------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
      LNUM = 0
C-----------------------------------------------------------------------
C    Read Planting Details Section
C-----------------------------------------------------------------------
      SECTION = '*PLANT'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO, LNUM)
      READ (LUNIO,'(35X,A1,30X,2(1X,F5.0))', IOSTAT=ERR) 
     &                PLME, SDAGE, ATEMP ; LNUM = LNUM + 1
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

      IF (PLME .EQ. 'T' .AND. ATEMP .LT. 0.0) THEN
         CALL ERROR (ERRKEY,10,FILEIO,LNUM)
      ENDIF

C-----------------------------------------------------------------------
C     Read crop genetic information
C-----------------------------------------------------------------------
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO, LNUM)
      READ (LUNIO,100, IOSTAT=ERR) P1,P3,P4,P5,
     &                 G2,G3,G4,PHINT,PCINT,PCGRD; LNUM = LNUM + 1
  100 FORMAT  (30X,4(F6.0),3(F6.2),3(F6.1))
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

      CLOSE (LUNIO)
      RETURN
      END SUBROUTINE TR_IPPHEN
C=======================================================================

