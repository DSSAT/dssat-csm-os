C=======================================================================
C  TR_CALSHK, Subroutine
C
C  Determines rice shock period
C-----------------------------------------------------------------------
C  Revision history
C
C  04/01/1996 MUS Written
C  08/29/2002 CHP/MUS Converted to modular format for inclusion in CSM.
C  02/19/2003 CHP Converted dates to YRDOY format
C=======================================================================
      SUBROUTINE TR_CALCSHK (DYNAMIC, ISTAGE, ISWWAT, LTRANS,  !Input
     &    YRDOY, YRSOW, TROOT, TMAX, TSHOCK)                   !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT  NONE
      EXTERNAL INCDAT, TIMDIF
      SAVE

      REAL      TMAX, TSHOCK, DSHOCK, TROOT
      INTEGER   DYNAMIC, ISTAGE
      INTEGER   INCDAT, TIMDIF, LYRDOY, YRDOY, YRSOW
      LOGICAL   LTRANS
      CHARACTER ISWWAT*1

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      !
      ! Any further initializtion of shock should go here. 

      TSHOCK = 1.0

!***********************************************************************
!***********************************************************************
!     Daily rate / integration calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      IF (ISWWAT .NE. 'Y') THEN
        TSHOCK = 1.0
        RETURN
      ENDIF


      IF (ISTAGE .NE. 5) THEN
         !
         ! Allow 7 day sliding scale effect on partitioning
         ! for transplanting
         !
         TSHOCK  = 1.0

         IF (LTRANS) THEN
	      DSHOCK = 7.0
            LYRDOY  = INCDAT(YRSOW, INT(DSHOCK))
            IF (YRDOY .GE. YRSOW .AND. YRDOY .LE. LYRDOY) THEN
               TSHOCK = 1.0 - TIMDIF(YRDOY, LYRDOY) / DSHOCK
               IF (TMAX .GE. 30.0) THEN
                  TSHOCK  = TSHOCK*0.05*(45.0-TMAX)
               ENDIF
               TSHOCK = AMAX1 (TSHOCK,0.0)
            ENDIF
         ENDIF
         !
         ! Shock causes greater part. to root
         !
         TROOT = AMAX1(1.0,1.5-TSHOCK)
      END IF



!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE TR_CALCSHK

