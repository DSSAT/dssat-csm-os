!----------------------------------------------------------------------*
! SUBROUTINE NNOSTRESS                                                 *
! Authors: Bas Bouman                                                  *
! Date   : Jan-2001, Version: 1.0                                      *
!          Version august, 503                                        *
! Purpose: This subroutine sets nitrogen stress factors on crop growth *
!          to unity, and gets leaf N content as function of DVS or     *
!          from observed values in the experiment data file.           *
!          A programming 'trick' using the function INTGR2 is used to  *
!          get DVS-interpolated values for leaf N content before and   *
!          after the first and last day of observation, respectively.  *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning                                     units  class *
! ----   ---- -------                                     -----  ----- *
! DELT    R4  Time step of integration (d)                          I  *
! IUNITD  I4  Unit that can be used for input files (-)             I  *
! IUNITL  I4  Unit number for log file messages (-)                 I  *
! ITASK   I4  Task that subroutine should perform (-)               I  *
! FILEI1  C*  Name of file with crop input data (-)                 I  *
! FILEIT  C*  Name of file with experimental data (-)               I  *
! CROPSTA I4  Crop stage (-)                                        I  *
! DVS     R4  Development stage (-)                                 I  *
! NFLV    R4  Nitrogen fraction in the leaves (g N m-2 leaf)        O  *
! NSLLV   R4  N stress factor on leaf death (-)                     O  *
! RNSTRS  R4  N stress factor on relative leaf growth (-)           O  *
!                                                                      *
!----------------------------------------------------------------------*
      SUBROUTINE NNOSTRESS2(DELT, IUNITD, IUNITL, ITASK, FILEI1, FILEIT, &
                           CROPSTA, DVS, WLVG, LAI, SLA, NFLV, NSLLV, RNSTRS)

      USE public_module		!VARIABLES
        IMPLICIT NONE
      
!-----Formal parameters
!     INPUT
      INTEGER IUNITD, IUNITL, ITASK, ILNMAX  !,  I
      CHARACTER (*) FILEI1, FILEIT
      REAL  DVS, LAI, WLVG, NMAXL, SLA, DELT

      INTEGER CROPSTA

!     OUTPUTS
      REAL NSLLV, NFLV, RNSTRS, NFLV1

!     Local parameters
      REAL       NFLVI
      INTEGER    ILNFLV

      INTEGER IMX
      PARAMETER (IMX=40)
      REAL NFLVTB(IMX), NMAXLT(IMX)

      REAL LINT2, INTGR2

      SAVE         !&#@TAOLI

!-----Initialization
      IF (ITASK.EQ.1) THEN

!        Reading input parameters (from crop file)
         CALL RDINIT(IUNITD, IUNITL, FILEI1)
         CALL RDSREA('NFLVI', NFLVI)
         CALL RDAREA('NMAXLT ',NMAXLT ,IMX,ILNMAX)
         CALL RDAREA('NFLVTB',NFLVTB,IMX,ILNFLV)
         CLOSE (IUNITD)

         NFLV     = NFLVI
         NSLLV    = 1.
         RNSTRS   = 1.
		!initialize soil nitrogen condition for optimal and uniform in soil profile for root growth
		!added by Dr. TaoLi, Feb 23 2010
		pv%pph = 7.0;				pv%pnh4 = 80.0 !kg N/ha
		pv%pno3 = 150.0; 			pv%purea = 0.0
!-----Rate calculations
      ELSE IF (ITASK.EQ.2) THEN

         IF (CROPSTA .LT. 4) THEN
            NFLV   = NFLVI
         ELSE
!          Read NFLV as function of development state
!            NFLV1 = LINT2('NFLVTB',NFLVTB,ILNFLV,DVS)
            NMAXL  = LINT2('NMAXLT',NMAXLT,ILNMAX,DVS)
            NFLV1 = NMAXL/(10.*SLA)
!          This is a programming 'trick' to enable forcing of observed
!          NFLV as function of day-of-observation. Below the first observation
!          day, NFLV1 is used;l between first and last observation day, interpolated
!          observed values are used; after last observation day, NFLV1 is used again
!          Forcing is determined by the variable NFLV_FRC in the experiment data file.
           NFLV   = INTGR2(0., NFLV1, DELT, FILEIT, 'NFLV')
       END IF

!-----State updates/integration
      ELSE IF (ITASK.EQ.3) THEN
!     No calculations

!-----Terminal output statements
      ELSE IF (ITASK.EQ.4) THEN
!     No calculations

!     END ITASKS
      END IF

      RETURN
      END
