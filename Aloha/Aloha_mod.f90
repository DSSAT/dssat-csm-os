!=======================================================================
!  MODULE Aloha_mod
!  03/28/2017 CHP Written
!=======================================================================

      MODULE Aloha_mod
!     Contains data definitions for Aloha Pineaple model
      USE ModuleDefs

!     Data construct for control variables
      TYPE AlohaCul_type
        CHARACTER*6 VARNO,ECONO
        CHARACTER*12 CULfile
        CHARACTER*16 VRNAME
        CHARACTER*80 CULpath
        REAL P1, P2, P3, P4, P5, P6
        REAL G2, G3, PHINT
      END TYPE AlohaCul_type

      TYPE AlohaSpe_type
        CHARACTER*12 SPEfile
        CHARACTER*80 SPEpath
        REAL CONV, FDMC
        REAL TBASV, TOPTV, TTOPV
        REAL TBASR, TOPTR, TTOPR
        REAL LIFAC
        REAL RWEP, PORM, RWMX, RLWR
        REAL CMFC
        REAL, DIMENSION(10) :: CO2X, CO2Y
      End Type AlohaSpe_type

      TYPE AlohaSow_type
        REAL PLTPOP, SDEPTH, SDWTPL, PLANTSIZE
        INTEGER NFORCING, NDOF, PMTYPE
      End Type AlohaSow_type

      Type (AlohaCul_type) Cultivar
      Type (AlohaSpe_type) Species
      Type (AlohaSow_type) Planting

      CHARACTER*10 STNAME(20)

      DATA STNAME/    &
        'Zero Stem ', &   ! 1
        'Forcing   ', &   ! 2
        'SCY       ', &   ! 3
        'Early Flwr', &   ! 4
        'Fruit Harv', &   ! 5
        'Maturity  ', &   ! 6
        'Planting  ', &   ! 7
        'Root Init.', &   ! 8
        'Leaf Emerg', &   ! 9
        '          ', &   !10
        '          ', &   !11
        '          ', &   !12
        '          ', &   !13
        'Start Sim ', &   !14
        'End Sim   ', &   !15
        '          ', &   !16
        '          ', &   !17
        '          ', &   !18
        '          ', &   !19
        'Harvest   '/     !20



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
! Species parameters
! ----------------------------------------------------------------------

!Growth
    Species % CONV  = 1.8    ! CONV    Rams dry matter/mj PAR
    Species % FDMC  = 0.12   ! FDMC    Fruit dry matter content (0 to 1.0)

    Species % TBASV = 16.    ! TBASV   Base temperature during leaf emergence
    Species % TOPTV = 35.    ! TOPTV   Upper limit of optimum temperature - veg phase
    Species % TTOPV = 45.    ! TTOPV   Maximum temperature for development - veg phase

    Species % TBASR = 18.    ! TBASV   Base temperature during reproductive phase
    Species % TOPTR = 33.    ! TOPTV   Upper limit of optimum temperature - rep phase
    Species % TTOPR = 45.    ! TTOPV   Maximum temperature for development - rep phase


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
        READ (LUNIO,70) PLANTING % PLTPOP, PLANTING % SDEPTH, PLANTING % SDWTPL, PLANTING % NFORCING, & 
                        PLANTING % PLANTSIZE, PLANTING % NDOF, PLANTING % PMTYPE
   70   FORMAT (24X,F6.0,24X,2F6.0,24X,I6,F6.0,2I6)
      ENDIF

!PLANTING DETAILS   
!        1         2         3         4         5         6         7         8         9        10        11        12
!23456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
!  1989166     -99   2.6   2.6     S     R   53.    0.   5.0  595.  -99. -99.0 -99.0   0.0     2 551.2   459     0
!    yrplt   iemrg      pltpop  plme  plds       azir sdepth      sdage  atemp  plph    nforcing        ndof  
!                 plants                  rowspc           sdwtpl                   sprlap   plantsize       pmtype 

!     -----------------------------------------------------------------
 !     Read crop cultivar coefficients
       SECTION = '*CULTI'
       CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
       IF (FOUND .EQ. 0) THEN
           CALL ERROR(SECTION, 42, FILEIO, LNUM)
       ELSE
         READ (LUNIO,1800,IOSTAT=ERR) & 
            Cultivar % VARNO, Cultivar % VRNAME, Cultivar % ECONO,     &
            Cultivar % P1, Cultivar % P2, Cultivar % P3, Cultivar % P4, Cultivar % P5, & 
            Cultivar % P6, Cultivar % G2, Cultivar % G3, Cultivar % PHINT  
 1800    FORMAT (A6,1X,A16,1X,A6,1X,15F6.0)    
!B0066 SC-F153          IB0001   60.0  629.  381. 2640.  400.  60.0  200.  14.0  40.0
         IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
       ENDIF
      CLOSE(LUNIO)

      RETURN
      END SUBROUTINE Aloha_IpPlant

!=======================================================================

!=======================================================================

      END MODULE Aloha_mod
!======================================================================

