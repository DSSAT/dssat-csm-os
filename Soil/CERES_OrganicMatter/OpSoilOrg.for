!=======================================================================
!  OpSoilOrg, Subroutine, C.H.Porter from Soil Nitrogen and Carbon 
!  portions of OPDAY
!
!  Generates output for daily soil Nitrogen and Carbon data
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  11/16/2001 CHP Written
!  06/07/2002 GH  Modified for crop rotations
!  08/20/2002 GH  Modified for Y2K
!  01/14/2005 CHP Split into organic and inorganic daily output routines.
!  02/25/2005 CHP changed HUMC to SSOMC to match Century variable name
!  03/01/2005 CHP Changed variable names to match Century:
!                 HUMC to SSOMC     HUMN to SSOME
!                 THUMC to TSOMC    THUMN to TSOME
!                 TFON to TLITE     TFOM to TLITC
!  04/13/2005 CHP changed subroutine name to OPSoilOrg. Was OpSoilC and
!                 OPSOILC_C.
!-----------------------------------------------------------------------
!  Called from:   CENTURY, SoilOrg
!  Calls: GETLUN, HEADER, YR_DOY, SUMVALS
!=======================================================================

      SUBROUTINE OpSoilOrg(CONTROL, ISWITCH,
     &  DSNC, NLAYR, OMADATA, SOILPROP, SomLitC, SomLitE, !Input
     &  SSOMC, SSOME, TLITC, TLITE, TSOMC, TSOME)         !Input

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE CsvOutput 
      USE Linklist
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, YR_DOY, SUMVALS, INCDAT, INTERPOLATE
      SAVE
!-----------------------------------------------------------------------

      CHARACTER*1  IDETC, RNMODE  !, IDETN, ISWNIT, ISWWAT
      CHARACTER*11, PARAMETER :: OUTSC = 'SoilOrg.OUT'

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, INCDAT, L, REPNO
      INTEGER N_ELEMS, NLAYR, NOUTDC, RUN, YEAR, YRDOY

      REAL CUMRES, DSNC, INTERPOLATE
      REAL SCDD, SNDD, SPDD, TLITC, TLITE(NELEM)
      REAL SOCD, SOND, SOPD
      REAL TSOMC, TSOME(NELEM)
      REAL SSOMC(0:NL), SSOME(0:NL,NELEM), HUMC(NL), HUMN(NL), HUMP(NL)
      REAL CumResE(NELEM)
      REAL SomLitC(0:NL), SomLitE(0:NL,NELEM)

      LOGICAL FEXIST, DOPRINT

!     Arrays which contain data for printing in SUMMARY.OUT file
      INTEGER, PARAMETER :: SUMNUM = 7
      CHARACTER*5, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!-----------------------------------------------------------------------
      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH
      TYPE (OrgMatAppType) OMAData
      TYPE (SoilType) SOILPROP

!     Transfer values from constructed data types into local variables.
      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FROP    = CONTROL % FROP
      N_ELEMS = CONTROL % N_ELEMS
      RNMODE  = CONTROL % RNMODE
      REPNO   = CONTROL % REPNO
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

      IDETC  = ISWITCH % IDETC
!      ISWWAT = ISWITCH % ISWWAT
      FMOPT  = ISWITCH % FMOPT   ! VSH

      CUMRES  = OMADATA % CumResWt
      CUMRESE = OMADATA % CumResE

!      IF (ISWWAT .EQ. 'N') THEN
!        RETURN
!      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
!-----------------------------------------------------------------------
!     Variable heading for SoilOrg.OUT
!-----------------------------------------------------------------------
!      IF (IDETC == 'Y') THEN
      IF ((IDETC == 'Y').AND.(FMOPT == 'A'.OR.FMOPT == ' ')) THEN
        CALL GETLUN(OUTSC, NOUTDC)
        INQUIRE (FILE = OUTSC, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTDC, FILE = OUTSC, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = NOUTDC, FILE = OUTSC, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTDC,'("*Soil Organic Matter daily output file")')
        ENDIF

        IF (RNMODE .NE. 'Q' .OR. RUN == 1) THEN
          !For first run of a sequenced run, use replicate
          ! number instead of run number in header.
          IF (RNMODE == 'Q') THEN
            CALL HEADER(SEASINIT, NOUTDC, REPNO)
          ELSE
            CALL HEADER(SEASINIT, NOUTDC, RUN)
          ENDIF

          IF (N_ELEMS > 1) THEN
            WRITE (NOUTDC,200)
          ELSE
            WRITE (NOUTDC,210)
          ENDIF
        ENDIF
      ENDIF

  200 FORMAT('@YEAR DOY   DAS',
     &  '    OMAC    SCDD    SOCD    SC0D    SCTD   SOMCT    LCTD',
     &  '    ONAC    SNDD    SOND    SN0D    SNTD   SOMNT    LNTD',
     &  '    OPAC    SPDD    SOPD    SP0D    SPTD   SOMPT    LPTD')
  210 FORMAT('@YEAR DOY   DAS',
     &  '    OMAC    SCDD    SOCD    SC0D    SCTD   SOMCT    LCTD',
     &  '    ONAC    SNDD    SOND    SN0D    SNTD   SOMNT    LNTD')

!  200       FORMAT('@YEAR DOY   DAS  SOCD  OMAC  OPAC    NOAD',
!     &      '   TFOM   HUM1   HUM2   HUM3   HUM4   HUM5',
!     &      '   TFON   NHU1   NHU2   NHU3   NHU4   NHU5',
!     &      '   TFOP   PHU1   PHU2   PHU3   PHU4   PHU5',
!     &      '   SNDD   SPDD   SCDD')
!          ELSE
!            WRITE (NOUTDC,210)
!  210       FORMAT('@YEAR DOY   DAS  SOCD  OMAC    NOAD',
!     &      '   TFOM   HUM1   HUM2   HUM3   HUM4   HUM5',
!     &      '   TFON   NHU1   NHU2   NHU3   NHU4   NHU5',
!     &      '   SNDD   SCDD')
      ENDIF !DYNAMIC

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      DOPRINT = .FALSE.

      IF (IDETC == 'Y') THEN
        SELECT CASE (DYNAMIC)
        CASE (SEASINIT)
          IF (INDEX('FQ',RNMODE) < 1 .OR. RUN == 1) DOPRINT = .TRUE.
          CALL YR_DOY(INCDAT(YRDOY,-1), YEAR, DOY)
        CASE (OUTPUT)
          IF (MOD(DAS, FROP) == 0) THEN
            DOPRINT = .TRUE.
            CALL YR_DOY(YRDOY, YEAR, DOY)
          ENDIF
        CASE (SEASEND)
          IF (MOD(DAS, FROP) /= 0) THEN
            DOPRINT = .TRUE.
            CALL YR_DOY(YRDOY, YEAR, DOY)
          ENDIF
        END SELECT

        DO L = 1, NLAYR
          HUMC(L) = SSOMC(L)
          HUMN(L) = SSOME(L,N)  
          HUMP(L) = SSOME(L,P)  
        ENDDO
      ENDIF

      IF (DOPRINT) THEN

        SCDD = INTERPOLATE (HUMC, DSNC, SOILPROP%DS)
        SNDD = INTERPOLATE (HUMN, DSNC, SOILPROP%DS)
        SPDD = INTERPOLATE (HUMP, DSNC, SOILPROP%DS)

        SOCD = (SomLitC(0) + TSOMC + TLITC)   !/1000.
        SOND = SomLitE(0,N) + TSOME(N) + TLITE(N)
        SOPD = SomLitE(0,P) + TSOME(P) + TLITE(P)

        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
        IF (N_ELEMS > 1) THEN
          WRITE(NOUTDC,400) YEAR, DOY, DAS, 
     &      NINT(CumRes), NINT(SCDD), NINT(SOCD), NINT(SomLitC(0)),  
  !   &  '        OMAC         SCDD        SOCD            SC0D',
     &        NINT(TSOMC+TLITC), NINT(TSOMC), TLITC,
  !   &  '            SCTD            SOMCT    LCTD',
     &      CumResE(N), SNDD, SOND, SomLitE(0,N), 
  !   &  '    ONAC    SNDD    SOND    SN0D',
     &        TSOME(N)+TLITE(N), TSOME(N), TLITE(N),
  !   &  '    SNTD                  SOMNT    LNTD',
     &      CumResE(P), SPDD, SOPD, SomLitE(0,P),  
  !   &  '    OPAC    SPDD    SOPD    SP0D')
     &        TSOME(P)+TLITE(P), TSOME(P), TLITE(P)
  !   &  '           SPTD         SOMPT    LPTD')


        ELSE
          WRITE(NOUTDC,400) YEAR, DOY, DAS,  
     &      NINT(CumRes), NINT(SCDD), NINT(SOCD), NINT(SomLitC(0)),  
     &        NINT(TSOMC+TLITC), NINT(TSOMC), TLITC,
     &      CumResE(N), SNDD, SOND, SomLitE(0,N), 
     &        TSOME(N)+TLITE(N), TSOME(N), TLITE(N)
        ENDIF
        END IF   ! VSH 
        
      IF (FMOPT == 'C') THEN 
        IF (N_ELEMS > 1) Then
         CALL CsvOutSoilOrg1(EXPNAME, CONTROL%RUN, CONTROL%TRTNUM, 
     &CONTROL%ROTNUM, CONTROL%REPNO, YEAR, DOY, DAS, 
     &CumRes, SCDD, SOCD, SomLitC, TSOMC, TLITC, CumResE, 
     &SNDD, SOND, SomLitE, TSOME, TLITE, SPDD, SOPD,  
     &vCsvlineSoilOrg, vpCsvlineSoilOrg, vlngthSoilOrg)
     
         CALL LinklstSoilOrg(vCsvlineSoilOrg)
        ELSE
         CALL CsvOutSoilOrg2(EXPNAME, CONTROL%RUN, CONTROL%TRTNUM, 
     &CONTROL%ROTNUM, CONTROL%REPNO, YEAR, DOY, DAS, 
     &CumRes, SCDD, SOCD, SomLitC, TSOMC, TLITC, CumResE, 
     &SNDD, SOND, SomLitE, TSOME, TLITE,   
     &vCsvlineSoilOrg, vpCsvlineSoilOrg, vlngthSoilOrg)
     
         CALL LinklstSoilOrg(vCsvlineSoilOrg)
        END IF
      END IF  ! 'C'    
                     
      ENDIF  ! DOPRINT

  400 FORMAT(1X,I4,1X,I3.3,1X,I5,
     &    6I8, F8.1,
     &    2(F8.2, 2F8.1, F8.2, 2F8.1, F8.2))
      
!***********************************************************************
!***********************************************************************
!     SEASEND
!***********************************************************************
      IF (DYNAMIC == SEASEND) THEN
!-----------------------------------------------------------------------
!       Store Summary.out labels and values in arrays to send to
!       OPSUM routines for printing.  Integers are temporarily 
!       saved aS real numbers for placement in real array.
        LABEL(1)  = 'RECM '; VALUE(1) = CUMRES
!        LABEL(2)  = 'ONAM'; VALUE(2) = TSOME(N)
!        LABEL(3)  = 'OCAM'; VALUE(3) = TSOMC/1000.
!       12/01/2005 Wageningen CHP Report total C surface
!             and soil -- this is important for C sequestration
!             studies.
!        LABEL(3)  = 'OCAM'; VALUE(2) = SOCD

!       12/12/2005 CHP Add SOCD and ONTAM variables
        LABEL(2)  = 'OCTAM'; VALUE(2) = SomLitC(0) + TSOMC + TLITC
        LABEL(3)  = 'OCAM '; VALUE(3) = TSOMC + TLITC 
        LABEL(4)  = 'ONTAM'; VALUE(4) = SomLitE(0,N) +TSOME(N) +TLITE(N)
        LABEL(5)  = 'ONAM '; VALUE(5) = TSOME(N) + TLITE(N) 
        LABEL(6)  = 'OPTAM'; VALUE(6) = SomLitE(0,P) +TSOME(P) +TLITE(P)
        LABEL(7)  = 'OPAM '; VALUE(7) = TSOME(P) + TLITE(P) 

!       Send labels and values to OPSUM
        CALL SUMVALS (SUMNUM, LABEL, VALUE) 

!       Close daily output files.
!        IF (IDETC == 'Y') CLOSE(NOUTDC)
        IF ((IDETC == 'Y')
     &  .AND.(FMOPT == 'A'.OR.FMOPT == ' ')) CLOSE(NOUTDC)   ! VSH

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OpSoilOrg
!-------------------------------------------------------------------

