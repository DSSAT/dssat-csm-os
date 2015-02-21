!----------------------------------------------------------------------*
!  SUBROUTINE SUBCD2                                                    *
!  Purpose: This subroutine calculates number of days below a certain  *
!           average temperature (TAV), which is used to terminate the  *
!           simulation after a maximum number of cold days the crop    *
!           can survive.                                               *
! Adapted from version 1, March 2006, Bouman                           *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! COLDMIN I4 lower T threshold for growth                           I  *
! CROPSTA I4  Crop stage (-)                                        I  *
! TAV     R4  Average daily temperature (oC)                        I  *
! TIME    R4  Time of simulation (d)                                T  *
! NCOLD   R4  Number of cold days (-)                               O  *
!                                                                      *
!  FILE usage : none                                                   *
!----------------------------------------------------------------------*
      SUBROUTINE SUBCD2 (COLDMIN, CROPSTA,TAV,TIME,NCOLD)

      IMPLICIT NONE
!-----Formal parameters
      REAL    COLDMIN, TAV, TIME, NCOLD
      INTEGER CROPSTA
      SAVE         ! TAOLI

      IF (CROPSTA .EQ. 3) NCOLD = 0.
      IF (TAV.LT.COLDMIN) THEN
         NCOLD = NCOLD+1.
      ELSE
         NCOLD = 0.
      END IF
      RETURN
      END
 
