C=======================================================================
C  MZ_OPNIT, Subroutine, G. Hoogenboom, J.W. Jones
C-----------------------------------------------------------------------
C  Generates output file for daily plant nitrogen variables
C     This routine is used for maize, sorghum and millet.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1990     Written
C  09/21/1998 CHP Split off from OPDAY.for file
C  05/11/1999 GH  Incorporated in CROPGRO
C  12/18/2001 WDB Revised for modular CERES
C  08/20/2002 GH  Modified for Y2K
C-----------------------------------------------------------------------
C  Called by: MAIZE, SG_CERES, ML_CERES
C  Calls:     None
!=======================================================================
      SUBROUTINE MZ_OPNIT(CONTROL, ISWITCH, 
     &    YRPLT, MDATE, NLAYR, SENESCE,
     &    WTNCAN,WTNSD,WTNVEG,PCNGRN,PCNVEG,
     &    WTNUP,WTNLF,WTNST,PCNL,PCNST,PCNRT)

!-----------------------------------------------------------------------
      USE ModuleDefs     
      USE CsvOutput   ! VSH
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, TIMDIF, YR_DOY
      SAVE

      INTEGER DAS, DYNAMIC, MDATE, NOUTDN, YRSIM

      CHARACTER*1  IDETG, IDETN, RNMODE
      CHARACTER*6, PARAMETER :: ERRKEY = 'MZ_OPN'
      CHARACTER*12 OUTPN

      INTEGER DAP, DOY, ERRNUM, FROP, L, NLAYR, RUN
      INTEGER TIMDIF, YEAR, YRDOY, YRPLT

      REAL PCNL
      REAL WTNCAN,WTNSD,WTNVEG,PCNGRN,PCNVEG
      REAL WTNUP,WTNLF,WTNST,PCNST,PCNRT     
      REAL CUMSENSURFN, CUMSENSOILN   !cumul. senes. N soil and surface

      LOGICAL FEXIST, FIRST

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH
      TYPE (ResidueType) SENESCE

      IDETG   = ISWITCH % IDETG
      IDETN   = ISWITCH % IDETN
      IF (IDETG == 'N' .OR. IDETN == 'N') RETURN

      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FROP    = CONTROL % FROP
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

      FMOPT   = ISWITCH % FMOPT   ! VSH
!-----------------------------------------------------------------------
      IF(DYNAMIC.EQ.RUNINIT) THEN

          OUTPN = 'PlantN.OUT'
          CALL GETLUN('OUTPN', NOUTDN)

      ENDIF

!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
      IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN    ! VSH
!     Initialize daily plant nitrogen output file
        INQUIRE (FILE = OUTPN, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTDN, FILE = OUTPN, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
          FIRST = .FALSE.  
        ELSE
          OPEN (UNIT = NOUTDN, FILE = OUTPN, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTDN,'("*PLANT N OUTPUT FILE")')
          FIRST = .TRUE.  
        ENDIF

        CALL HEADER(SEASINIT, NOUTDN, RUN)
        WRITE (NOUTDN,230)
  230   FORMAT('@YEAR DOY   DAS   DAP',
     &        '   CNAD   GNAD   VNAD   GN%D   VN%D   NUPC',
     &        '   LNAD   SNAD   LN%D   SN%D   RN%D   SNN0C   SNN1C')
       END IF   ! VSH
       
        !cumul. senes. N soil and surface
        CUMSENSURFN = 0.0
        CUMSENSOILN = 0.0   

      ENDIF

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      IF (DYNAMIC .EQ. OUTPUT) THEN

!         Calculate cumulative N senesenced
          CUMSENSURFN = CUMSENSURFN + SENESCE % ResE(0,1) 
          DO L = 1, NLAYR
            CUMSENSOILN = CUMSENSOILN + SENESCE % ResE(L,1)
          ENDDO

      !-----------------------------------------------------------------
      !   CHECK FOR OUTPUT FREQUENCY
      !-----------------------------------------------------------------
        IF (YRDOY .GE. YRPLT .AND. YRPLT .GT. 0)
     &          THEN

          DAP = MAX(0, TIMDIF(YRPLT, YRDOY))
!         DAS = MAX(0, TIMDIF(YRSIM, YRDOY))

          IF ((MOD(DAS,FROP) .EQ. 0)     !Daily output every FROP days,
     &      .OR. (YRDOY .EQ. YRPLT)         !on planting date, and
     &      .OR. (YRDOY .EQ. MDATE)) THEN   !at harvest maturity 

            CALL YR_DOY(YRDOY, YEAR, DOY)
            
            IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN ! VSH
!           Print 
            WRITE (NOUTDN,300) YEAR, DOY, DAS, DAP,
     &       (WTNCAN*10.0), (WTNSD*10.0), (WTNVEG*10.0),
     &        PCNGRN, PCNVEG, (WTNUP*10.0),
     &       (WTNLF*10.0), (WTNST*10.0), PCNL, PCNST, PCNRT,
     &        CUMSENSURFN, CUMSENSOILN
 300        FORMAT (1X,I4,1X,I3.3,2(1X,I5),3(1X,F6.1),2(1X,F6.2),
     &        4(1X,F6.1),2(1X,F6.2), 2F8.2)
            END IF    ! VSH
     
 !    VSH CSV output corresponding to PlantN.OUT
      IF (FMOPT == 'C') THEN    
         CALL CsvOutPlNMzCer(EXPNAME,CONTROL%RUN,CONTROL%TRTNUM, 
     &CONTROL%ROTNUM,CONTROL%REPNO, YEAR, DOY, DAS, DAP, 
     &WTNCAN, WTNSD, WTNVEG, PCNGRN, PCNVEG, WTNUP, WTNLF,  
     &WTNST, PCNL, PCNST, PCNRT, CUMSENSURFN, CUMSENSOILN,  
     &vCsvlinePlNMZCER, vpCsvlinePlNMZCER, vlngthPlNMZCER)
    
         CALL LinklstPlNMZCER(vCsvlinePlNMZCER)
      END IF 
     
          ENDIF
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     SEASEND
!***********************************************************************
      IF ((DYNAMIC .EQ. SEASEND)
     & .AND. (FMOPT == 'A' .OR. FMOPT == ' ')) THEN    ! VSH
!-----------------------------------------------------------------------
        CLOSE (NOUTDN)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE MZ_OPNIT
!=======================================================================


