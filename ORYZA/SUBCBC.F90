!----------------------------------------------------------------------*
!  SUBROUTINE SUBCBC                                                   *
!  Purpose: This subroutine checks the Crop Carbon Balance             *
!           and stops the simulation if the difference between         *
!           CKCIN and CKCFL exceeds 0.1 %                              *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! CKCIN   R4  Accumulated C in the crop (kg C ha-1)                 I  *
! CKCFL   R4  Sum of integrated C fluxes (kg C ha-1)                I  *
! TIME    R4  Time of simulation (d)                                T  *
! CBCHK   R4  Carbon balance check, relative value to the sums of      *
!             CKIN and CKCFL (-)                                    O  *
! TERMNL  R4  Flag to indicate if simulation is to stop (-)         O  *
!                                                                      *
!  FILE usage : none                                                   *
!----------------------------------------------------------------------*
      SUBROUTINE SUBCBC (CKCIN,CKCFL,TIME,CBCHK,TERMNL)

      IMPLICIT NONE
!-----Formal parameters
      REAL    CKCIN,CKCFL,TIME,CBCHK
      LOGICAL TERMNL
      SAVE         ! TAOLI
 
      CBCHK = 2.0*(CKCIN-CKCFL)/(CKCIN+CKCFL+1.E-10)
 
      IF (ABS(CBCHK).GT.0.001) THEN
!Old:     WRITE (6,10) CBCHK,CKCIN,CKCFL,TIME
!10       FORMAT (/,'* * *Error in Carbon Balance, please check* * *',/,
!     &           ' CBCHK=',F8.3,', CKCIN=',F8.2,', CKCFL=',F8.2,
!     &           ' at TIME=',F6.1)
         WRITE (*,'(A,/,A,F8.3,2(A,F8.2),A,F6.1)') &
           '* * * Error in Carbon Balance, please check * * *', &
           ' CBCHK=',CBCHK,', CKCIN=',CKCIN,', CKCFL=',CKCFL,' at TIME=',TIME
         TERMNL = .TRUE.
      END IF
 
      RETURN
      END
