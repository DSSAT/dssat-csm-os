!----------------------------------------------------------------------*
!  SUBROUTINE SUWCMS2                                                  *
!                                                                      *
!  Purpose: SUWCMS2 calculates volumetric soil water content from      *
!           soil water suction, and vice versa. Various options are    *
!           offered. See SWIT8 in input file or SAWAH manual.          *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! I       I4  Compartment index (-)                                 I  *
! SWIT4   I4  Switch to set request MS(WCL) or WCL(MS) (-)          I  *
! WCST    R4  Array of water content saturation / layer (cm3 cm-3)  I  *
! WCL     R4  Array of actual water content / layer (cm3 cm-3)     I/O *
! MS      R4  Soil water suction (cm)                              I/O *
!                                                                      *
!  SUBROUTINES called:  SUERR                                          *
!                                                                      *
!  FUNCTION called:     none                                           *
!                                                                      *
!  FILE usage:          none                                           *
!                                                                      *
!***********************************************************************
  
      SUBROUTINE SUWCMS2(I,SWIT4,WCST,WCL,MS,
     &          VGA, VGN, VGR, WCAD, WCSTRP)
 
      USE MODULEDEFS
      IMPLICIT NONE
!-----Formal parameters
      INTEGER I, SWIT4
      REAL    WCST, WCL, MS
 
!-----Local variables
      REAL    HLP1, HLP2, HLP3, HLP4, TINY, VGM, WREL

!-----Common blocks
      REAL  VGA(NL), VGN(NL), VGR(NL)
      REAL WCAD(NL), WCSTRP(NL)


!-----Variables retain their values between subsequent calls
!     of this subroutine
      SAVE
 
      DATA TINY/0.001/
 
      IF (SWIT4.EQ.1) THEN ! request MS(WCL)
!--------Suction calculated from water content
         ! SUERR checks whether value of variable X is within          *
         !       pre-specified domain 
          
         IF (WCL.LT.WCAD(I).OR.WCL.GT.WCST) 
     &      CALL SUERR(3,WCL,WCAD(I),WCST)
         ! SUERR(message #,variable to be checked,XMIN,XMAX)
         ! WCSTRP  R4  Array saturated volumetric water content ripened      I  *
         ! soil per soil compartment (cm3 cm-3)     
         IF (WCL.GT.WCSTRP(I)) THEN
!           It is assumed that MS remains zero during shrinkage
            MS = 0.
         ELSE
!-----------Van Genuchten option
            HLP1 = AMAX1(WCAD(I),WCL)
            WREL = (WCL-VGR(I))/(WCSTRP(I)-VGR(I))
            VGM  = 1.-1./VGN(I)
            HLP2 = 1./VGA(I)
            HLP3 = -1./VGM
            HLP4 = 1./VGN(I)
            MS   = HLP2*(WREL**HLP3-1.)**HLP4
         END IF
      ELSE IF (SWIT4.EQ.2) THEN !request WCL(MS). This is not our case
!--------Water content calculated from suction
         IF (MS.LT.-TINY.OR.MS.GT.1.E8) CALL SUERR(4,MS,0.,1.E8)
!--------Van Genuchten option
         VGM  = 1.-1./VGN(I)
         HLP1 = (MS*VGA(I))**VGN(I)
         WREL = (1.+HLP1)**(-VGM)
         WCL  = WREL*(WCSTRP(I)-VGR(I))+VGR(I)
      END IF
 
      RETURN
      END
! C=====================================================================
!  SUWCMS2 VARIABLE DEFINITIONS: (updated Dec 2009)
!-----------------------------------------------------------------------
! HLP    Intermediate variable
! MS     The soil-water tension (no layer index) in (mbar= 1.0197g/cm2=1.097cm of water)
! SWIT4  Switcher. one means to calculate suction from water content,MS(WCL)
!                  two means calculate water content from suction WCL(MS) 
! VGA    van Genuchten alpha parameter in cm-1 
! VGL    van Genuchten lambda parameter
! VGN    van Genuchten n parameter
! VGR    van Genuchten residual water content
! WCSTRP  Array saturated volumetric water content ripened soil per soil compartment (cm3 cm-3)  
! WCL     Soil water content (m3/m3) corresponding to MS, Note: there is no layer index
! WREL   Relative water content
!-----------------------------------------------------------------------
!     END SUBROUTINE SUWCMS2
!=======================================================================


!----------------------------------------------------------------------*
! SUBROUTINE SUERR                                                     *
!                                                                      *
! Purpose: SUERR checks whether value of variable X is within          *
!          pre-specified domain                                        *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! IMNR    I4  Message number                                        I  *
! X       R4  Value of variable to be checked (variable)            I  *
! XMIN    R4  Minimum allowable value of X (variable)               I  *
! XMAX    R4  Maximum allowable value of X (variable)               I  *
!                                                                      *
! WARNINGS:                                                            *
!                                                                      *
!   X < XMIN * 0.99 and XMIN .NE. -99 then expert message is produced  *
!   X > XMAX * 1.01 and XMAX .NE. -99 then expert message is produced  *
!                                                                      *
! SUBROUTINES called : none                                            *
!                                                                      *
! FUNCTIONS called   : none                                            *
!                                                                      *
! FILE usage         : none                                            *
!----------------------------------------------------------------------*
 
      SUBROUTINE SUERR(IMNR,X,XMIN,XMAX)
      IMPLICIT NONE

!-----Formal paramers
      INTEGER IMNR
      REAL    X, XMIN, XMAX
!-----Local variables
      CHARACTER (38) ERRM(5)
 
!-----Variables retain their values between subsequent calls
!     of this subroutine
      SAVE
 
      DATA ERRM/ 
     &      'MATRIC SUCTION OUT OF RANGE IN SUMSKM2', 
     &       'WATER CNT OUT OF RANGE IN SUSLIN      ', 
     &      'WATER CNT OUT OF RANGE IN SUWCMS2     ', 
     &      'MATRIC SUCTION OUT OF RANGE IN SUWCMS2', 
     &      'ONE OR MORE TRWL(I) OUT OF RANGE      '/
 
      IF ((X.LT.XMIN*0.99).AND.(XMIN.NE.-99.) .OR. 
     &     (X.GT.XMAX*1.01).AND.(XMAX.NE.-99.)) THEN
 
         WRITE (*,'(/,/,A,/,A,/,10X,I2,3(E10.3),/,A)') 
     &       ' ***fatal error in variable or parameter value ***', 
     &       '    message number, value, minimum and maximum: ',   
     &        IMNR,X,XMIN,XMAX, 
     &        ERRM(IMNR)
       !  CALL FATALERR ('SUERRM',' ')
         ! SUBROUTINE FATALERR (STRING1, STRING2). Writes the error messages STRING1 and STRING2 to the screen and holds the simulation until <RETURN> is pressed. Then the execution of the model is terminated.
      END IF

      END
