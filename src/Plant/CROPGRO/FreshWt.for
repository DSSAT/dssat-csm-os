!=======================================================================
!  FreshWt, Subroutine, C.H.Porter, K.J.Boote, J.I.Lizaso, G. Hoogenboom
!-----------------------------------------------------------------------
!  Computes fresh pod weigt
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  05/09/2007     Written. KJB, CHP, JIL, RR
!  02/27/2008     Added pod quality for snap bean. JIL
!  10/02/2020     Add fresh weight for bell pepper
!-----------------------------------------------------------------------
!  Called from:  PODS
!=======================================================================

      SUBROUTINE FreshWt(DYNAMIC, ISWFWT, NR2TIM, PHTIM, SDNO, SHELN, 
     &    WTSD, WTSHE, YRPLT)

!-----------------------------------------------------------------------
      USE ModuleDefs 
      USE ModuleData

      IMPLICIT NONE
      SAVE

      CHARACTER*1   ISWFWT
      CHARACTER*2   CROP
      CHARACTER*7   ERRKEY
      PARAMETER (ERRKEY = 'FreshWt')
      CHARACTER*11, PARAMETER :: FWFile = "FreshWt.OUT"
      CHARACTER*16 CROPD
      CHARACTER*78 MSG(3)

      INTEGER DAP, DAS, DOY, DYNAMIC, ERRNUM, I
      INTEGER NOUTPF, NPP, NR2TIM, TIMDIF
      INTEGER YEAR, YRDOY, YRPLT

      REAL AvgDMC, AvgDPW, AvgFPW, PodDiam, PodLen
      REAL PAGE, PodAge, PODNO, SEEDNO, SHELPC
      REAL TDPW, TFPW, TDSW
      REAL CLASS(7)

      REAL, DIMENSION(NCOHORTS) :: DMC, DryPodWt, FreshPodWt, PHTIM
      REAL, DIMENSION(NCOHORTS) :: SDNO, SHELN, WTSD, WTSHE, XPAGE

      LOGICAL FEXIST

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      CALL GET(CONTROL)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CALL GET(ISWITCH)
      
!     Switch for fresh weight calculations
      IF (INDEX('Y',ISWFWT) < 1 .OR. 
     &    INDEX('N,0',ISWITCH%IDETL) > 0) RETURN

      CROP   = CONTROL%CROP

!     Currently only works for tomato, green bean, bell pepper, and strawberry. 
!     Add other crops later. 
!     Send a message if not available crop
      IF (INDEX('TM,GB,PR',CROP) < 0) THEN
        CALL GET_CROPD(CROP, CROPD)
        WRITE(MSG(1),'(A)') 
     &  "Fresh weight calculations not currently available for "
        WRITE(MSG(2),'(A2,1X,A16)') CROP, CROPD
        CALL INFO(2,ERRKEY,MSG)
      ENDIF

!::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      CALL GETLUN(FWFile, NOUTPF)
      INQUIRE (FILE= FWFile, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN(UNIT = NOUTPF, FILE = FWFile, STATUS = 'OLD',
     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = NOUTPF, FILE = FWFile, STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
        WRITE(NOUTPF,'("*Fresh Weight Output File")')
      ENDIF

      !Write headers
      CALL HEADER(SEASINIT, NOUTPF, CONTROL%RUN)

!     Change header to PWAD1 (was PWAD) because GBuild requires 
!     unique headers (PlantGro also lists PWAD).  Should have same
!     value, but slightly off. Why?

!     Need to look at how GBuild handles P#AD and SH%D here, too.

        SELECT CASE (CROP)
          CASE ('TM')       ! Tomato
            WRITE (NOUTPF,230)
          CASE ('GB')       ! Snap bean
            WRITE (NOUTPF,231)
            CASE ('PR')       ! Bell pepper
         WRITE (NOUTPF,230)
        END SELECT

  230 FORMAT('@YEAR DOY   DAS   DAP',
     &    '   FPWAD   PDMCD   AFPWD',
     &    '   ADPWD   PAGED')

  231 FORMAT('@YEAR DOY   DAS   DAP',
     &    '   FPWAD   PDMCD   AFPWD',
     &    '   ADPWD   PAGED',
     &    ' FCULD FSZ1D FSZ2D FSZ3D FSZ4D FSZ5D FSZ6D')
  
      AvgDMC = 0.0
      AvgDPW = 0.0
      AvgFPW = 0.0
      PodAge = 0.0
      PODNO  = 0.0
      SEEDNO = 0.0
      SHELPC = 0.0
      TDPW   = 0.0
      TFPW   = 0.0

!***********************************************************************
!***********************************************************************
!     DAILY RATE/INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      IF (INDEX('Y',ISWFWT) < 1 .OR. 
     &    INDEX('N,0',ISWITCH%IDETL) > 0) RETURN

!     Calculate number of pods, including those with and without seeds
      SEEDNO = 0.0
      PODNO  = 0.0
      TFPW   = 0.0
      TDPW   = 0.0
      TDSW   = 0.0
      DO I = 1, 7
        CLASS(I) = 0.0
      ENDDO
!-----------------------------------------------------------------------
      DO NPP = 1, NR2TIM + 1
        PAGE = PHTIM(NR2TIM + 1) - PHTIM(NPP)
        XPAGE(NPP) = PAGE

!       Dry matter concentration (fraction)
!       DMC(NPP) = (5. + 7.2 * EXP(-7.5 * PAGE / 40.)) / 100.
        SELECT CASE (CROP)
          CASE ('TM')       ! Tomato
            DMC(NPP) = (5. + 7.2 * EXP(-7.5 * PAGE / 40.)) / 100.
          CASE ('GB')       ! Snap bean
!           DMC(NPP) = 0.0465 + 0.0116 * EXP(0.161 * PAGE)
            DMC(NPP) = 0.023 + 0.0277 * EXP(0.116 * PAGE)
          CASE ('PR')       ! Snap bean
! GB        DMC(NPP) = 0.023 + 0.0277 * EXP(0.116 * PAGE)
            DMC(NPP) = (5. + 7.2 * EXP(-7.5 * PAGE / 40.)) / 100.
        END SELECT

!       Fresh weight (g/pod)
        IF (SHELN(NPP) > 1.E-6) THEN
          FreshPodWt(NPP) = (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP) /
     &                          SHELN(NPP)  !g/pod
          DryPodWt(NPP) = (WTSD(NPP) + WTSHE(NPP))/SHELN(NPP) !g/pod
        ELSE
          FreshPodWt(NPP) = 0.0
        ENDIF

!       Snap bean quality
        IF (CROP .EQ. 'GB') THEN
!         PodDiam = mm/pod; PodLen = cm/pod
          PodDiam = 8.991 *(1.0-EXP(-0.438*(FreshPodWt(NPP)+0.5))) 
          PodLen  = 14.24 *(1.0-EXP(-0.634*(FreshPodWt(NPP)+0.46)))

          IF (PodDiam .LT. 4.7625) THEN
!           Culls
            CLASS(7) = CLASS(7) + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP) 
          ELSEIF (PodDiam .LT. 5.7547) THEN
!           Sieve size 1
            CLASS(1) = CLASS(1) + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP) 
          ELSEIF (PodDiam .LT. 7.3422) THEN
!           Sieve size 2
            CLASS(2) = CLASS(2) + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP)
          ELSEIF (PodDiam .LT. 8.3344) THEN
!           Sieve size 3
            CLASS(3) = CLASS(3) + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP)
          ELSEIF (PodDiam .LT. 9.5250) THEN
!           Sieve size 4
            CLASS(4) = CLASS(4) + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP)
          ELSEIF (PodDiam .LT. 10.7156) THEN
!           Sieve size 5
            CLASS(5) = CLASS(5) + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP)
          ELSE
!           Sieve size 6
            CLASS(6) = CLASS(6) + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP)
          ENDIF
        ENDIF

        TFPW = TFPW + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP)
        TDPW = TDPW + WTSD(NPP) + WTSHE(NPP)
        TDSW = TDSW + WTSD(NPP)

        PODNO = PODNO + SHELN(NPP)
        SEEDNO = SEEDNO + SDNO(NPP)
      ENDDO

      PodAge = XPAGE(1)
      IF (PODNO > 1.E-6) THEN
        AvgFPW = TFPW / PODNO
        AvgDPW = TDPW / PODNO
      ELSE
        AvgFPW = 0.0
        AvgDPW = 0.0
      ENDIF
      IF (TFPW > 1.E-6) THEN
        AvgDMC = TDPW / TFPW
      ELSE
        AvgDMC = 0.0
      ENDIF
      IF (TDPW > 1.E-6) THEN
        ShelPC = TDSW / TDPW * 100.
      ELSE
        ShelPC = 0.0
      ENDIF

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
      IF (INDEX('Y',ISWFWT) < 1 .OR. 
     &    INDEX('N,0',ISWITCH%IDETL) > 0) RETURN

      YRDOY = CONTROL % YRDOY
      IF (YRDOY .LT. YRPLT .OR. YRPLT .LT. 0) RETURN

!     DAS = MAX(0,TIMDIF(YRSIM,YRDOY))
      DAS = CONTROL % DAS

!     Daily output every FROP days
      IF (MOD(DAS,CONTROL%FROP) == 0) THEN  

        CALL YR_DOY(YRDOY, YEAR, DOY) 
        DAP = MAX(0,TIMDIF(YRPLT,YRDOY))
        IF (DAP > DAS) DAP = 0

        SELECT CASE (CROP)
          CASE ('TM')       ! Tomato
            WRITE(NOUTPF, 1000) YEAR, DOY, DAS, DAP, 
     &      NINT(TFPW * 10.), AvgDMC, AvgFPW, AvgDPW, 
     &      PodAge
      CASE ('PR')           ! Bell pepper
            WRITE(NOUTPF, 1000) YEAR, DOY, DAS, DAP, 
     &      NINT(TFPW * 10.), AvgDMC, AvgFPW, AvgDPW, 
     &      PodAge
          CASE ('GB')       ! Snap bean
            WRITE(NOUTPF, 2000) YEAR, DOY, DAS, DAP, 
     &      NINT(TFPW * 10.), AvgDMC, AvgFPW, AvgDPW, 
     &      PodAge,NINT(CLASS(7)*10.),NINT(CLASS(1)*10.),
     &      NINT(CLASS(2)*10.),NINT(CLASS(3)*10.),NINT(CLASS(4)*10.),
     &      NINT(CLASS(5)*10.),NINT(CLASS(6)*10.)
        END SELECT

 1000   FORMAT(1X,I4,1X,I3.3,2(1X,I5),
     &    I8,F8.3,F8.1,F8.2,F8.1)
 2000   FORMAT(1X,I4,1X,I3.3,2(1X,I5),
     &    I8,F8.3,F8.1,F8.2,F8.1,
     &    7(1X,I5))

      ENDIF

!***********************************************************************
!***********************************************************************
!     SEASONAL SUMMARY
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------

      CLOSE (NOUTPF)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE FreshWt
!=======================================================================

