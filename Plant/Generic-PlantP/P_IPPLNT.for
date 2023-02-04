!=======================================================================
!    Subroutine P_IPPLNT
!    Reads species file for plant phosphorus data.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  04/12/2006 CHP Removed "!" from section name in species file
!-----------------------------------------------------------------------

      SUBROUTINE P_IPPLNT (FILECC,
     & N2Pmax, N2Pmin,
     & PCShutMin, PCLeafMin, PCStemMin, PCRootMin, PCShelMin, PCSeedMin,
     & PCShutOpt, PCLeafOpt, PCStemOpt, PCRootOpt, PCShelOpt, PCSeedOpt,
     & FracPMobil, FracPUptake, SRATPHOTO, SRATPART, UseShoots)

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT  NONE
      EXTERNAL GETLUN, FIND, WARNING, ERROR, IGNORE
      SAVE
!     ------------------------------------------------------------------

      CHARACTER*6 SECTION
      CHARACTER*6, PARAMETER :: ERRKEY = 'PPLANT'
      CHARACTER*78 MSG(10)
      CHARACTER*92 FILECC, TEXT

      INTEGER ERR, FOUND, ISECT, I, LNUM
      INTEGER LUNCRP

      REAL FracPMobil, FracPUptake
      REAL SRATPHOTO, SRATPART

      REAL, DIMENSION(3) :: PCShutOpt, PCRootOpt, PCShelOpt, PCSeedOpt
      REAL, DIMENSION(3) :: PCLeafOpt, PCStemOpt
      REAL, DIMENSION(3) :: PCShutMin, PCRootMin, PCShelMin, PCSeedMin
      REAL, DIMENSION(3) :: PCLeafMin, PCStemMin
      REAL, DIMENSION(3) :: N2Pmin, N2Pmax

      LOGICAL UseShoots

!     ----------------------------------------------------------------
!     Check validity of data
      I = 0
      UseShoots = .TRUE.

!     Read Species file for P parameters
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)

!     ------------------------------------------------------------------
!     Find and Read Phosphorus Section
      SECTION = '*PHOSP'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        MSG(1) = 'Phosphorus input section not found in species file.'
        MSG(2) = FILECC
        MSG(3) = 
     &   'Can not simulate phosphorus for this crop. Program will stop.'
        CALL WARNING(3,ERRKEY,MSG)
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
!       Shoot optimum P concentrations 
!       If leaf and stem concentrations are used, these should be -99.
        CALL IGNORE(LUNCRP,LNUM,ISECT,TEXT)
        READ(TEXT,'(4F8.0)',IOSTAT=ERR) 
     &    PCShutOpt(1), PCShutOpt(2), PCShutOpt(3)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!       If shoot data is present, use that.  Otherwise use leaf and stem data.
        IF (PCShutOpt(1) < 0. .OR. PCShutOpt(2) < 0. .OR. 
     &      PCShutOpt(3) < 0.) THEN
          UseShoots = .FALSE.
        ENDIF

!       Leaf optimum P concentrations 
!       If shoot concentrations are used, these should be -99.
        CALL IGNORE(LUNCRP,LNUM,ISECT,TEXT)
        READ(TEXT,'(4F8.0)',IOSTAT=ERR) 
     &    PCLeafOpt(1), PCLeafOpt(2), PCLeafOpt(3)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!       Stem optimum P concentrations 
!       If shoot concentrations are used, these should be -99.
        CALL IGNORE(LUNCRP,LNUM,ISECT,TEXT)
        READ(TEXT,'(4F8.0)',IOSTAT=ERR) 
     &    PCStemOpt(1), PCStemOpt(2), PCStemOpt(3)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!       If shoot data was missing, must have leaf and stem data
        IF (.NOT. UseShoots) THEN
          IF (PCLeafOpt(1) < 0. .OR. PCLeafOpt(2) < 0. .OR. 
     &        PCLeafOpt(3) < 0. .OR. PCStemOpt(1) < 0. .OR. 
     &        PCStemOpt(2) < 0. .OR. PCStemOpt(3) < 0.) THEN
            WRITE(MSG(I+1),*) 
     &      'Optimum P data missing for Leaf, Stem or Shoot.'
            I = I + 1
          ENDIF
        ENDIF

!       Root optimum P concentrations 
        CALL IGNORE(LUNCRP,LNUM,ISECT,TEXT)
        READ(TEXT,'(4F8.0)',IOSTAT=ERR) 
     &    PCRootOpt(1), PCRootOpt(2), PCRootOpt(3)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        IF (PCRootOpt(1) < 0. .OR. PCRootOpt(2) < 0. .OR.
     &      PCRootOpt(3) < 0.) THEN
          WRITE(MSG(I+1),*) 'Optimum P data missing for root.'
          I = I + 1
        ENDIF

!       Shell optimum P concentrations 
        CALL IGNORE(LUNCRP,LNUM,ISECT,TEXT)
        READ(TEXT,'(4F8.0)',IOSTAT=ERR) 
     &    PCShelOpt(1), PCShelOpt(2), PCShelOpt(3)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

   !     IF (PCShelOpt(1) < 0. .OR. PCShelOpt(2) < 0. .OR.
   !  &      PCShelOpt(3) < 0.) THEN
   !       WRITE(MSG(I+1),*) 'Optimum P data missing for shell.'
   !       I = I + 1
   !     ENDIF

!       Seed optimum P concentrations 
        CALL IGNORE(LUNCRP,LNUM,ISECT,TEXT)
        READ(TEXT,'(4F8.0)',IOSTAT=ERR) 
     &    PCSeedOpt(1), PCSeedOpt(2), PCSeedOpt(3)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

   !     IF (PCSeedOpt(1) < 0. .OR. PCSeedOpt(2) < 0. .OR.
   !  &      PCSeedOpt(3) < 0.) THEN
   !       WRITE(MSG(I+1),*) 'Optimum P data missing for seed.'
   !       I = I + 1
   !     ENDIF

!     ------------------------------------------------------------------
!       Shoot minimum P concentrations 
!       If leaf and stem concentrations are used, these should be -99.
        CALL IGNORE(LUNCRP,LNUM,ISECT,TEXT)
        READ(TEXT,'(4F8.0)',IOSTAT=ERR) 
     &    PCShutMin(1), PCShutMin(2), PCShutMin(3)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!       If shoot data is present, use that.  Otherwise use leaf and stem data.
        IF (UseShoots .AND. (PCShutMin(1) < 0. .OR. 
     &      PCShutMin(2) < 0. .OR. PCShutMin(3) < 0.)) THEN
          WRITE(MSG(I+1),*) 
     &      'Minimum P data missing for Shoots.'
          I = I + 1
        ENDIF

!       Leaf minimum P concentrations 
!       If shoot concentrations are used, these should be -99.
        CALL IGNORE(LUNCRP,LNUM,ISECT,TEXT)
        READ(TEXT,'(4F8.0)',IOSTAT=ERR) 
     &    PCLeafMin(1), PCLeafMin(2), PCLeafMin(3)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!       Stem minimum P concentrations 
!       If shoot concentrations are used, these should be -99.
        CALL IGNORE(LUNCRP,LNUM,ISECT,TEXT)
        READ(TEXT,'(4F8.0)',IOSTAT=ERR) 
     &    PCStemMin(1), PCStemMin(2), PCStemMin(3)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!       If shoot data was missing, must have leaf and stem data
        IF ((.NOT. UseShoots) .AND.
     &       (PCLeafMin(1) < 0. .OR. PCLeafMin(2) < 0. .OR. 
     &        PCLeafMin(3) < 0. .OR. PCStemMin(1) < 0. .OR. 
     &        PCStemMin(2) < 0. .OR. PCStemMin(3) < 0.)) THEN
          WRITE(MSG(I+1),*) 
     &      'Minimum P data missing for Leaf or Stem.'
          I = I + 1
        ENDIF

!       Root minimum P concentrations 
        CALL IGNORE(LUNCRP,LNUM,ISECT,TEXT)
        READ(TEXT,'(4F8.0)',IOSTAT=ERR) 
     &    PCRootMin(1), PCRootMin(2), PCRootMin(3)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        IF (PCRootMin(1) < 0. .OR. PCRootMin(2) < 0. .OR.
     &      PCRootMin(3) < 0.) THEN
          WRITE(MSG(I+1),*) 'Minimum P data missing for root.'
          I = I + 1
        ENDIF

!       Shell minimum P concentrations 
        CALL IGNORE(LUNCRP,LNUM,ISECT,TEXT)
        READ(TEXT,'(4F8.0)',IOSTAT=ERR) 
     &    PCShelMin(1), PCShelMin(2), PCShelMin(3)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

   !     IF (PCShelMin(1) < 0. .OR. PCShelMin(2) < 0. .OR.
   !  &      PCShelMin(3) < 0.) THEN
   !       WRITE(MSG(I+1),*) 'Minimum P data missing for shell.'
   !       I = I + 1
   !     ENDIF

!       Seed minimum P concentrations 
        CALL IGNORE(LUNCRP,LNUM,ISECT,TEXT)
        READ(TEXT,'(4F8.0)',IOSTAT=ERR) 
     &    PCSeedMin(1), PCSeedMin(2), PCSeedMin(3)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

   !     IF (PCSeedMin(1) < 0. .OR. PCSeedMin(2) < 0. .OR.
   !  &      PCSeedMin(3) < 0.) THEN
   !       WRITE(MSG(I+1),*) 'Minimum P data missing for seed.'
   !       I = I + 1
   !     ENDIF

!       Maximum N:P ratios for vegetative tissue
        CALL IGNORE(LUNCRP,LNUM,ISECT,TEXT)
        READ(TEXT,'(4F8.0)',IOSTAT=ERR) 
     &    N2Pmax(1), N2Pmax(2), N2Pmax(3)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!       Minimum N:P ratios for vegetative tissue
        CALL IGNORE(LUNCRP,LNUM,ISECT,TEXT)
        READ(TEXT,'(4F8.0)',IOSTAT=ERR) 
     &    N2Pmin(1), N2Pmin(2), N2Pmin(3)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!     ----------------------------------------------------------------
        CALL IGNORE(LUNCRP,LNUM,ISECT,TEXT)
        READ(TEXT,'(2F8.0)',IOSTAT=ERR) SRATPHOTO, SRATPART
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,TEXT)
        READ(TEXT,'(F8.0)',IOSTAT=ERR) FracPMobil
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,TEXT)
        READ(TEXT,'(F8.0)',IOSTAT=ERR) FracPUptake
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

      CLOSE (LUNCRP)

!     ----------------------------------------------------------------
      IF (I > 0) THEN
        WRITE(MSG(I+1),'(A,A64)') 'Species file: ', FILECC(1:64)
        I = I + 1
        IF (LEN(FILECC) > 64) THEN
          WRITE(MSG(I+1),'(14X,A64)') FILECC(65:92)
          I = I + 1
        ENDIF
        CALL WARNING(I, ERRKEY, MSG)
        CALL ERROR(ERRKEY,10,FILECC,LNUM)
      ENDIF
  
      RETURN
      END SUBROUTINE P_IPPLNT

!=======================================================================


