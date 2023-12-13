C=======================================================================
C  TR_ROOTGR, Subroutine
C
C  Determines root growth and depth
C-----------------------------------------------------------------------
C  Revision history
C
C  08/02/1993 PWW Header revision and minor changes
C  08/29/2002 CHP/MUS Converted to modular format for inclusion in CSM.
C  02/19/2003 CHP Converted dates to YRDOY format
C=======================================================================

      SUBROUTINE TR_ROOTGR (CONTROL, 
     &    DTT, FLOOD, GRORT, ISWNIT, ISWWAT,              !Input
     &    ITRANS, NH4, NO3, PLANTS, RLWR, SOILPROP,       !Input
     &    SUMDTT, SW, SWFAC, YRDOY, YRPLT,                !Input
     &    FLOODN, RTWT,                                   !I/O
     &    RLV, RTDEP, SDEPTH)                             !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
      USE FloodModule    ! parameters, hourly weather data.

      IMPLICIT  NONE
      EXTERNAL TR_IPROOTGR
      SAVE

      CHARACTER*1 ISWNIT, ISWWAT

      INTEGER DYNAMIC, ITRANS
      INTEGER L, L1, NLAYR, YRDOY, YRPLT

      REAL CUMDEP, DEPMAX, DTT, FLDH4C, FLDN3C, FLOOD, GRORT, PLANTS
      REAL RLVF, RLNEW, RLWR, RNFAC, RNLF, RTDEP, RTWT
      REAL SD2, SDEPTH, SUMDTT, SWDF, SWFAC, TotInorgN, TRLDF     

      REAL, DIMENSION(NL) :: DLAYR, DS, DUL, ESW, LL, NH4, NO3
      REAL, DIMENSION(NL) :: RLDF, RLV, SHF, SW

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "SOILPROP" is of type "SoilType".
      TYPE (SoilType) SOILPROP
      TYPE (FloodNType) FLOODN

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      DUL     = SOILPROP % DUL
      LL      = SOILPROP % LL
      SHF     = SOILPROP % WR

      FLDH4C = FLOODN % FLDH4C
      FLDN3C = FLOODN % FLDN3C
 
!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      DS     = SOILPROP % DS
      DLAYR  = SOILPROP % DLAYR
      NLAYR  = SOILPROP % NLAYR
      DEPMAX = DS(NLAYR)
      RLV    = 0.0
      RTDEP  = 0.0        !from ingrow

      CALL TR_IPROOTGR (CONTROL, SDEPTH)

!***********************************************************************
!***********************************************************************
!     Daily rate / integration calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
!      CALL YR_DOY(YRDOY, YEAR, DOY)

!     transplant day
      IF (YRDOY .EQ. YRPLT .AND.(ITRANS .EQ. 2 .OR. ITRANS .EQ. 3)) THEN
        IF (ITRANS .EQ. 3) THEN
           ! Estimate RLV from root weight
           IF (ISWWAT .EQ. 'Y') THEN
              RTDEP  = 5.0
              SD2    = SUMDTT*SUMDTT*0.001
              RTWT = (1.290 - 0.008 * SUMDTT + 0.1095 * SD2) / PLANTS      
              RLV(1) = AMIN1 ((RTWT*PLANTS),0.5)
              RLV(2) = RLV(1)*0.9
           ENDIF

        ! ITRANS 2 assumes nonlimting growth, use 0.8 to reduce growth
        ELSEIF (ITRANS .EQ. 2) THEN
          RTDEP = 0.5*RTDEP
        ENDIF
        RTDEP  = AMAX1 (RTDEP,SDEPTH)

        ! Estimate bias root distribution to surface
        RLV(1) = RLV(1) + 0.75*RLV(2)
        RLV(2) = 0.25*RLV(2)
        RETURN
      ENDIF

      IF (GRORT .LE. 0.0) THEN
        RETURN
      ENDIF

C     RLNEW  = GRORT*PLANTS*1.05
      RLNEW  = GRORT * PLANTS * RLWR
      TRLDF  = 0.0
      CUMDEP = 0.0
      RNFAC  = 1.0

      DO L = 1, NLAYR
         L1 = L
         CUMDEP = CUMDEP + DLAYR(L)
         SWDF   = 1.0
         ESW(L)   = DUL(L) - LL(L)
         IF (SW(L)-LL(L) .LT. 0.25*ESW(L)) THEN
            SWDF = 4.0*(SW(L)-LL(L)) / ESW(L)
         ENDIF
         SWDF = AMAX1 (SWDF,0.0)

         IF (ISWNIT .EQ. 'Y') THEN
            TotInorgN = NO3(L) + NH4(L)
            IF (L .EQ. 1 .AND. FLOOD .GT. 0.0) THEN
               TotInorgN = TotInorgN + FLDH4C + FLDN3C
            ENDIF
            IF (TotInorgN .GT. 50.0) THEN
               RNFAC = 1.0
             ELSE
               RNFAC = 1.0-(1.17*EXP(-0.25*TotInorgN))
            ENDIF
            RNFAC = AMAX1 (RNFAC,0.01)
         ENDIF

         RLDF(L) = AMIN1(SWDF,RNFAC)*SHF(L)*DLAYR(L)

         IF (CUMDEP .GE. RTDEP) THEN
            RTDEP   = RTDEP + DTT*0.18*AMIN1((SWFAC*1.5),SWDF)
            RTDEP   = AMIN1 (RTDEP,DEPMAX)
            RLDF(L) = RLDF(L)*(1.0-(CUMDEP-RTDEP)/DLAYR(L))
            TRLDF   = TRLDF + RLDF(L)
            EXIT
         ENDIF
         TRLDF = TRLDF + RLDF(L)
      END DO

      IF (TRLDF .LT. RLNEW*0.00001 .OR. TRLDF .LE. 0.0) THEN
         RETURN
      ENDIF

      RNLF   = RLNEW/TRLDF
      CUMDEP = 0.0

      DO L = 1, L1
         CUMDEP = CUMDEP + DLAYR(L)
         RLV(L) = RLV(L) + RLDF(L)*RNLF/DLAYR(L)-0.005*RLV(L)
         IF (CUMDEP .GE. 115.0) THEN
            RLVF   = 0.377 - 0.0015*CUMDEP
            RLV(L) = AMIN1 (RLV(L),RLVF)
         ENDIF
         RLV(L) = AMAX1 (RLV(L),0.0)
      END DO

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE TR_ROOTGR
C=======================================================================

C=======================================================================
C  TR_IPROOTGR, Subroutine
C
C  Reads FILEIO for TARO routine
C  05/07/2002 CHP Written
C  08/12/2003 CHP Added I/O error checking
C=======================================================================

      SUBROUTINE TR_IPROOTGR (CONTROL, SDEPTH)

      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT     NONE
      EXTERNAL ERROR, FIND

      CHARACTER*6  ERRKEY, SECTION
      PARAMETER (ERRKEY = 'IPRICE')
      CHARACTER*30 FILEIO

      INTEGER LNUM, LUNIO, ERR, FOUND

      REAL SDEPTH

C     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL
      FILEIO = CONTROL % FILEIO
      LUNIO  = CONTROL % LUNIO

!-----------------------------------------------------------------------
!       Read data from FILEIO for use in PLANT module
!-----------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)

C-----------------------------------------------------------------------
C    Read Planting Details Section
C-----------------------------------------------------------------------
      SECTION = '*PLANT'
      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO, LNUM)

      READ (LUNIO,'(55X,F5.1)') SDEPTH ; LNUM = LNUM + 1
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
      IF (SDEPTH .LE. 0.0) THEN
         SDEPTH = 2.0
      ENDIF

      CLOSE (LUNIO)
      RETURN
      END SUBROUTINE TR_IPROOTGR
C=======================================================================
