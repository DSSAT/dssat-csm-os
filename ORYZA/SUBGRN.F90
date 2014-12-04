!----------------------------------------------------------------------*
!  SUBROUTINE SUBGRN                                                   *
!  Adapted: Bouman, July 1999                                          *
!  Purpose: This subroutine calculates spikelet formation rate and     *
!           spikelet fertility as affected by low and high temperature *
!           and the grain growth rate.  Spikelet sterility component   *
!           is according to Horie et al., 1992.                        *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! GCR     R4  Gross growth rate of the crop (kg DM/ha/d)            I  *
! CROPSTA R4  Crop stage (-)                                        I  *
! LRSTRS  R4  Leaf rolling stress factor (-)                        I  *
! DVS     R4  Development stage of the crop (-)                     I  *
! SF1     R4  Spikelet fertility factor due to low temperatures (-) I  *
! SF2     R4  Spikelet fertility factor due to high temperatures (-)I  *
! SPGF    R4  Spikelet growth factor (no kg-1)                      I  *
! TAV     R4  Average daily temperature (oC)                        I  *
! TMAX    R4  Daily maximum temperature (oC)                        I  *
! NSP     R4  Number of spikelets (no)                              I  *
! CTSTER  R4  The critical temperature for 50% sterility (oC)       I  *
! COLDREP R4  The critical temperature for cold causing sterility (oC) *
! GNSP    R4  Rate of increase in spikelet number (no ha-1 d-1)     O  *
! GNGR    R4  Rate of increase in grain number (no ha-1 d-1)        O  *
! SPFERT  R4  Spikelet fertility (-)                                O  *
! GRAINS  L*  Fortran logical function whether grains are formed    O  *
!                                                                      *
!  FILE usage : none                                                   *
!----------------------------------------------------------------------*
     SUBROUTINE SUBGRN(GCR,CROPSTA,LRSTRS, &
                        DVS,SF2,SF1,SPGF,TAV,TMAX,NSP,CTSTER,COLDREP,GNSP,GNGR, &
                        SPFERT,GRAINS)

      IMPLICIT NONE
!-----Formal parameters
      REAL    GCR, DVS, SF2, SF1, SPGF, TAV, TMAX, NSP, GNSP, GNGR
      REAL    SPFERT, LRSTRS, CTSTER, COLDREP            
      INTEGER CROPSTA
      LOGICAL GRAINS
!-----Local parameters
      REAL    DVSPI, DVSF, CTT, COLDTT, TFERT, NTFERT, TINCR
      SAVE

!     Initialization
      IF (CROPSTA .LE. 1) THEN
         GRAINS = .FALSE.
         COLDTT = 0.
         TFERT  = 0.
         NTFERT = 0.
         SF1    = 1.
         SF2    = 1.
         SPFERT = 1.
      END IF

!-----Temperature increase due to leaf rolling (BAS): 1.6 degree
!     per unit leaf rolling (Turner et al., 1986; p 269)
      LRSTRS = max(0.0, LRSTRS)   !TAOLI, 22Feb 2012
      TINCR = 5.*(1.-LRSTRS)*1.6

!-----Spikelet formation between PI and Flowering
      DVSPI = 0.65
      DVSF  = 1.
      IF ((DVS.GE.DVSPI).AND.(DVS.LE.DVSF)) THEN
         GNSP = max(0.0, GCR)*SPGF      !TAOLI, 22Feb 2012
      ELSE
         GNSP = 0.
      END IF
 
!-----Grain formation from spikelets (GNGR)
!-----Calculate GNGR reduction factors
      IF ((DVS.GE.0.75).AND.(DVS.LE.1.2)) THEN
         CTT    = MAX(0.,COLDREP -(TAV-TINCR))
         COLDTT = COLDTT+CTT
      END IF
      IF ((DVS.GE.0.96).AND.(DVS.LE.1.2)) THEN
         TFERT  = TFERT +(TMAX+TINCR)            
         NTFERT = NTFERT+1.
      END IF

!-----Apply GNGR reduction factors when DVS is 1.2
      IF ((DVS.GE.1.2).AND.(.NOT.GRAINS)) THEN
         GRAINS = .TRUE.
         SF1    = 1.-(4.6+0.054*COLDTT**1.56)/100.
         SF1    = MIN(1.,MAX(0.,SF1))
         TFERT  = TFERT/(NTFERT)
         SF2    = 1./(1.+EXP(0.853*(TFERT-CTSTER)))
         SF2    = MIN(1.,MAX(0.,SF2))
         SPFERT = MIN(SF1,SF2)
         GNGR   = NSP*SPFERT
      ELSE
         GNGR   = 0.
      END IF
      RETURN
      END
