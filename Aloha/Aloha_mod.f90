!=======================================================================
!  MODULE Aloha_mod
!  03/28/2017 CHP Written
!=======================================================================

      MODULE Aloha_mod
!     Contains data definitions for Aloha Pineaple model
      USE ModuleDefs

!     Data construct for control variables
      TYPE AlohaCul_type
        CHARACTER*12 CULfile
        CHARACTER*80 CULpath
        REAL P1, P2, P3, P4, P5, P6
        REAL G2, G3, PHINT
      END TYPE AlohaCul_type

      TYPE AlohaSpe_type
        CHARACTER*12 SPEfile
        CHARACTER*80 SPEpath
        REAL CONV, TBAS, FDMC
        REAL LIFAC
        REAL RWEP, PORM, RWMX, RLWR
        REAL CMFC
        REAL, DIMENSION(10) :: CO2X, CO2Y
      End Type AlohaSpe_type

      TYPE AlohaSow_type
        REAL PLTPOP, SDEPTH, LAT, ROWSPC
      End Type AlohaSow_type

      Type (AlohaCul_type) Cultivar
      Type (AlohaSpe_type) Species
      Type (AlohaSow_type) Planting


      CONTAINS

!=======================================================================
!  Aloha_IPCROP, Subroutine
!  Read crop parameters from PIALO980.SPE
!-----------------------------------------------------------------------
!  Revision history
!
!  06/15/1994 PWW Original written
!  03/29/2017 CGO Revised for v4.6
!=======================================================================
      SUBROUTINE Aloha_IPCROP ()

      IMPLICIT    NONE
      SAVE

! For now, hard wire all parameters until they are collected from the code.
! Then write read routines (or use Willingthon Pavan's)

! ----------------------------------------------------------------------
! Cultivar parameters
! ----------------------------------------------------------------------
    Cultivar % P1 = 60.0     !ADP from first leaf to end of 0 stem growth
    Cultivar % P2 = 500.     !Growing degree days from forcing to sepals closed on youngest flower
    Cultivar % P3 = 500.     !GDD from SCY to early flowering
    Cultivar % P4 = 2200.    !Cumulative growing degree days from EF to maturity
    Cultivar % P5 = 400.     !GDD from fruit harvest to physiological maturity
    Cultivar % P6 = 60.      !NDAP from root initiation to first leaf emerged
    Cultivar % G2 = 200.     !Potential eye number
    Cultivar % G3 = 14.      !Potential eye growth rate
    Cultivar % PHINT = 95.   !Phylochron interval

! ----------------------------------------------------------------------
! Species parameters
! ----------------------------------------------------------------------

!Growth
    Species % CONV = 1.8     ! CONV    Rams dry matter/mj PAR
    Species % TBAS = 16.     ! TBAS    Base temperature during leaf emergence
    Species % FDMC = 0.12    ! FDMC    Fruit dry matter content (0 to 1.0)

!Photosynthesis
    Species % LIFAC = 0.52   ! LIFAC   Light interception coefficient

!Roots
    Species % RWEP = 1.50    ! RWEP    
    Species % PORM = 0.02    ! PORM    Minimum pore space
    Species % RWMX = 0.03    ! RWMX    Max root water uptake
    Species % RLWR = 0.98    ! RLWR    Root length weight ratio

!Management factor - WHAT IS THIS???
    Species % CMFC = 1.0     ! CMFC    Management condition factor

!CO2 effect
    Species % CO2X(1) =   0. ; Species % CO2Y(1) = 0.00
    Species % CO2X(2) = 220. ; Species % CO2Y(2) = 0.81
    Species % CO2X(3) = 330. ; Species % CO2Y(3) = 1.00
    Species % CO2X(4) = 440. ; Species % CO2Y(4) = 1.03
    Species % CO2X(5) = 550. ; Species % CO2Y(5) = 1.06
    Species % CO2X(6) = 660. ; Species % CO2Y(6) = 1.10
    Species % CO2X(7) = 770. ; Species % CO2Y(7) = 1.13
    Species % CO2X(8) = 880. ; Species % CO2Y(8) = 1.16
    Species % CO2X(9) = 990. ; Species % CO2Y(9) = 1.18
    Species %  CO2X(10)=9999.; Species % CO2Y(10)= 1.25

      RETURN
      END SUBROUTINE Aloha_IPCROP

!=======================================================================
!  Aloha_IpPlant, Subroutine
!  Read crop parameters from PIALO980.SPE
!-----------------------------------------------------------------------
!  Revision history
!
!  06/15/1994 PWW Original written
!  03/29/2017 CHP Revised for v4.6
!=======================================================================
      SUBROUTINE Aloha_IpPlant (CONTROL)

      IMPLICIT NONE
      SAVE

      CHARACTER*6, PARAMETER :: ERRKEY = 'IPPLNT'
      CHARACTER*6 SECTION
      CHARACTER*12 FILEIO
      INTEGER LUNIO, ERR, LNUM, LINC, FOUND
      TYPE (ControlType) CONTROL

      FILEIO = CONTROL % FILEIO
      

!     -----------------------------------------------------------------
!     Read input file name (ie. DSSAT45.INP) and path
      CALL GETLUN('FILEIO', LUNIO)
      OPEN (LUNIO, FILE = CONTROL % FILEIO, STATUS = 'OLD', IOSTAT=ERR)  
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
      REWIND (LUNIO)
      READ(LUNIO,50,IOSTAT=ERR) Species % SPEfile, Species % SPEpath; LNUM = 7  !Species file, path
   50 FORMAT(//////,15X,A12,1X,A80)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

!      READ(LUNIO,51,IOSTAT=ERR) FILEE, PATHER; LNUM = LNUM + 1  !Ecotype file name 
!      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

      READ(LUNIO,51,IOSTAT=ERR) Cultivar % CULfile, Cultivar % CULpath; LNUM = LNUM + 2 
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)        !Cultivar file, path
   51 FORMAT(/,15X,A12,1X,A80)

!     -----------------------------------------------------------------
!     Read Planting Details Section
      SECTION = '*PLANT'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
!        READ (LUNIO,70) PLTPOP, SDEPTH, SDWTPL, NFORCING, PLANTSIZE, NDOF, PMTYPE
!   70   FORMAT (24X,F6.0,24X,2F6.0,24X,I6,F6.0,2I6)! From maize model:
      ENDIF

   ! from optempy2k:
   !      WRITE (LUNIO,70,IOSTAT=ERRNUM) YRPLT,IEMRG,PLANTS,PLTPOP,PLME,
   !  &          PLDS,ROWSPC,AZIR,SDEPTH,SDWTPL,SDAGE,ATEMP,PLPH,SPRLAP,
   !  &          NFORC,PLTFOR,NDOF,PMTYPE
   !70 FORMAT (3X,I7,1X,I7,2(1X,F5.1),2(5X,A1),2(1X,F5.0),1X,F5.1,
   !  &        2(1X,F5.0),3(1X,F5.1),I6,F6.1,2I6)

! New variables: NFORCING,PLANTSIZE,NDOF,PMTYPE

     !yrplt = 90191
     !plants = 6.2
     !pltpop = 6.2
     !plme = 'S'
     !plds = 'R'
     !rowspc = 53.
     !azir = 0.
     !sdepth = 5.0
     !sdwtpl = 2976.
     !sprlap = 0.0
     !nforcing = 2
     !plantsize = -99.
     !ndof = 243
     !pmtype = 0
!     -----------------------------------------------------------------
!!     Read crop cultivar coefficients
!      SECTION = '*CULTI'
!      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
!      IF (FOUND .EQ. 0) THEN
!          CALL ERROR(SECTION, 42, FILEIO, LNUM)
!      ELSE
!        READ (LUNIO,1800,IOSTAT=ERR) VARNO,VRNAME,ECONO,     &
!                  P1,P2,P5,G2,G3,PHINT  
!1800    FORMAT (A6,1X,A16,1X,A6,1X,6F6.0)    
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
!      ENDIF

      CLOSE(LUNIO)

      RETURN
      END SUBROUTINE Aloha_IpPlant

!=======================================================================

!=======================================================================

      END MODULE Aloha_mod
!======================================================================

