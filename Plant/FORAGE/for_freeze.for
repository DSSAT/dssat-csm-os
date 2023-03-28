C=======================================================================
C  FREEZE, Subroutine, J. W. Jones, K. J. Boote, G. Hoogenboom
C-----------------------------------------------------------------------
C  Calculates freeze damage.  The plant will loose all its leaves if
C  the temperature goes below FREEZ1 and stops growth entirely if the
C  temperature goes below FREEZ2.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1989     Written.
C  12/31/1996 GH  Deleted phenology statements.
C  09/15/1998 CHP Modified for modular format.
C  05/10/1999 GH  Incorporaed in CROPGRO
!  06/15/2022 CHP Added CropStatus
C-----------------------------------------------------------------------
C  Called by  : CROPGRO
C  Calls      : None
C========================================================================
      SUBROUTINE FOR_FREEZE(
     &    FREEZ1, FREEZ2, FRZDC, NRUSLF, NRUSST,      !Input
     &    SLDOT, SSDOT, STMWT, TMIN, WTLF, YRDOY,     !Input
     &    YRPLT,                                      !Input
     &    MDATE,                                      !Input/Output
     &    CropStatus, FRZDL, PSRLYRD, PSRSRFD,        !Output
     &    WLFDOT, WSFDOT, WSRFDOT)                    !Output 

!      variables previously in argument list, but not used:
!      FILEIO, RUN
!      IDETO, NOUTDO, 
!      NRUSSR, PSRSRFL, PSRLYR1,
!      SRFTEMP, SSRDOT, ST, STRWT,
!      SRLYRD, SRSRFD, , VSTAGE

      USE ModuleDefs     !Definitions of constructed variable types, 


C-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL WARNING, TIMDIF
      SAVE
C-----------------------------------------------------------------------
!     CHARACTER*1  IDETO
!     CHARACTER*30 FILEIO
      CHARACTER*78 MESSAGE(10)
      INTEGER MDATE, YRDOY, DAP, YRPLT, TIMDIF, CropStatus    !NOUTDO, 
      REAL  WLFDOT, WTLF, SLDOT, NRUSLF, TMIN, FREEZ1, FREEZ2
      REAL  FRZDC, FRZDL, NRUSST, PSRSRFD,  !NRUSSR, PSRSRFL, PSRLYR1, 
     &  PSRLYRD, SSDOT, STMWT,   !SRLYRD, SRFTEMP, SRSRFD, SSRDOT, 
     &  WSFDOT, WSRFDOT       !STRWT, VSTAGE, 
!     REAL, DIMENSION(NL) :: ST
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C      Set storage organ freeze damage variables to 0.0
C      part of abandoned strategy to allow partial to total kill
C      of the storage organ.  Plant would live as long as there
C      was storage organ left.
C-----------------------------------------------------------------------

      WSRFDOT = 0.0

      PSRSRFD = 0.0
      PSRLYRD = 0.0
C-----------------------------------------------------------------------
C      Calculate proportion of above-ground plant mass that will
C      be lost to frost.  Function of number of degrees below threshold 
C      temperature multiplied by the proportion lost per degree below
C      threshold.  For total kill (like DSSAT35) set FRZDC=1.0
C-----------------------------------------------------------------------

            FRZDL = (FREEZ1 - TMIN) * FRZDC
            FRZDL = MIN(FRZDL,1.0)
            FRZDL = MAX(FRZDL,0.0)

            WLFDOT = (WTLF - SLDOT - NRUSLF/0.16) * FRZDL
            WLFDOT = MIN ((WTLF - SLDOT - NRUSLF/0.16), WLFDOT)
C-----------------------------------------------------------------------
C      SJR 5/12/04 Moved VSTAGE adjustment to GROW
C       to be compatible with adjustment for senescence
C-----------------------------------------------------------------------
!            VSTAGE = ((WTLF-WLFDOT)/WTLF) * VSTAGE

            WSFDOT = (STMWT - SSDOT - NRUSST/0.16) * FRZDL
            WSFDOT = MIN ((STMWT - SSDOT - NRUSST/0.16), WSFDOT)


            IF (TMIN .LE. FREEZ2) THEN
                    IF (MDATE .LT. 0) THEN
                        MDATE = YRDOY
                        CropStatus = 32 !cold stress
                        ENDIF

                  WLFDOT = WTLF - SLDOT - NRUSLF/0.16
C-----------------------------------------------------------------------
C      SJR 5/12/04 Moved VSTAGE adjustment to GROW
C       to be compatible with adjustment for senescence
C-----------------------------------------------------------------------
!                  VSTAGE = 0.0
                  WSFDOT = STMWT - SSDOT - NRUSST/0.16


            ENDIF
      

      DAP   = MAX(0,TIMDIF(YRPLT,YRDOY))

        
      

      WRITE(MESSAGE(1),100) DAP
      WRITE(MESSAGE(2),110) YRDOY
      CALL WARNING(1, 'FREEZE', MESSAGE)
  100 FORMAT(' Freeze occurred at ',I4,' days after planting.')
  110 FORMAT('   (DAY : ',I7,' )')
!      WRITE (*,'(/,10X,A78,/,10X,A78,/)') MESSAGE(1), MESSAGE(2)
!      IF (IDETO .EQ. 'Y')  THEN
!        WRITE (NOUTDO,'(/,5X,A78,/,5X,A78,/)') MESSAGE(1), MESSAGE(2)
!      ENDIF
C-----------------------------------------------------------------------
      RETURN
      END ! SUBROUTINE FREEZE

C-----------------------------------------------------------------------
!     FREEZE VARIABLES: 
C-----------------------------------------------------------------------
! DAP    Number of days after planting (d)
! FREEZ1   Temperature below which plant loses all leaves, but development 
!            continues (°C)
! FREEZ2 Temperature below which plant growth stops completely. (°C)
! FRZDC        Freezing death coefficient  - percentage tissue/population death per day per degree below FREEZ2
! FRZDL  Todays death loss of storage tissue/plant population due to freezing (proportion of STRWT and PLNTPOP)
! IDETO  Switch for printing OVERVIEW.OUT file 
! NOUTDO Logical unit for OVERVIEW.OUT file 
! NRUSLF N actually mobilized from leaves in a day (g[N]/m2-d)
! NRUSSR N actually mobilized from storage organ in a day (g[N]/m2-d)
! NRUSST N actually mobilized from stems in a day (g[N]/m2-d)
! PSRLYRD Proportion of total storage organ tissue loss from below ground tissue
! PSRLYR1 Proportion of storage organ tissue in soil layer 1 (below soil surface)
! PSRSRFD Proportion of total storage organ tissue loss from above ground tissue
! PSRSRFL Proportion of storage organ tissue on/above soil surface
! SLDOT  Defoliation due to daily leaf senescence (g/m2/day)
! SRFTEMP Temperature of soil surface (degrees C)
! SRLYRD      Freeze damage to below ground storage organ tissue
! SRSRFD  Freeze damage to above ground storage organ tissue
! SSDOT  Stem loss due to daily senescence (g/m2/day)
! SSRDOT  Storage organ loss due to daily senescence (g/m2/day)
! ST(I)      Temperature in soil layer (I) (degrees C)
! STMWT   Dry mass of stem tissue, including C and N (g[stem] / m2[ground)
! TMIN   Minimum daily temperature (°C)
! VSTAGE    Number of nodes on main stem of plant 
! WLFDOT Leaf weight losses due to freezing (g[leaf]/m2-d)
! WSFDOT Stem weight losses due to freezing (g[stem]/m2-d)
! WSRFDOT Storage organ weight losses due to freezing (g[storage]/m2-d)
! WTLF   Dry mass of leaf tissue including C and N (g[leaf] / m2[ground])
! YRDOY  Current day of simulation (YYDDD)
! MDATE  Harvest maturity (YYDDD)
! YRPLT  Planting date (YYDDD)
!-----------------------------------------------------------------------
!     END FREEZE SUBROUTINE
!-----------------------------------------------------------------------
