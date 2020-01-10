!----------------------------------------------------------------------*
! Codes is from of ORYZA2000\SourceCode\S_soil\subsoil.for             * 
! Reference: http://www.treemail.nl/download/treebook7/start.htm
!----------------------------------------------------------------------*
! SUBROUTINE SUBSL2                                                    *
!                                                                      *
! Author   : C. Rappoldt                                               *
!            M. Wopereis (revision March 1993)                         *
! Date     : January 1986, revised June 1990                           *
!            Slightly changed to work with VG parameters, March 1993   * 
! Purpose  :                                                           *
! Chapter 15 in documentation WOFOST Version 4.1 (1988) This routine   *
! calculates the rate of capillary flow or percolation between         *
! groundwater table and root zone. The stationary flow is found by     * 
! integration of dZL = K.d(MH)/(K + FLW), where Z= height above        *
! groundwater, MH= matric head, K= conductivity and FLW= chosen flow.  *
! In an iteration loop the correct flow is found.  The integration     *
! goes at most over four intervals: [0,45],[45,170], [170,330] and     *
! [330,MH-rootzone] (last one on logarithmic scale).                   *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! PF      R4  pF value soil compartment (-)                         I  *
! D       R4  Distance to grounwater table (cm)                     I  *
! I       I4  Compartment index (-)                                 I  *
! WCST    R4  Array of water content saturation / layer (cm3 cm-3)  I  *
! FLOW    R4  Capillary rise calculated by subroutine SUBSL2 (mm       *
!             d-1)                                                  O  *
!                                                                      *
! SUBROUTINES called: SUMSKM2                                          *
!                                                                      *
!----------------------------------------------------------------------*
      SUBROUTINE SUBSL2(PF,D,I,WCST, FLOW,
     &              VGA, VGL, VGN, VGR, KST, WCAD, WCSTRP)
 
      USE ModuleDefs
      IMPLICIT NONE
      SAVE

!-----Formal parameters
      INTEGER I
      REAL    PF, D, WCST, FLOW
      REAL VGA(NL), VGL(NL), VGN(NL), VGR(NL)
      REAL KST(NL), WCAD(NL), WCSTRP(NL)

!-----Local variables
      INTEGER I1,I2,I3,IINT,IMAX
      REAL    D1, DF, ELOG10, FL, FLW, FU, KMS, LOGST4, MH, PF1, Z 
      REAL    START(4),PFSTAN(9),PGAU(3) ,WGAU(3)
      REAL    DEL(4)  ,PFGAU(12),HULP(12),CONDUC(12)
      REAL K0
 
      DATA ELOG10/2.302585/
      DATA PGAU  /.1127016654,.5,.8872983346/
      DATA WGAU  /.2777778,.4444444,.2777778/
      DATA START /0.,45.,170.,330./
      DATA LOGST4/2.518514/
      DATA PFSTAN/0.705143,1.352183,1.601282,1.771497,2.031409,2.192880,
     &              2.274233,2.397940,2.494110/
 
!-----Calculation of matric head and check on small pF
      PF1  = PF
      D1   = D
      MH   = EXP(ELOG10*PF1)
      IF (PF1.LE.0.) GOTO 90
      IINT  = 0
 
!-----Number and width of integration intervals
      DO I1 = 1,4
         IF (I1.LE.3) DEL(I1) = MIN(START(I1+1),MH)-START(I1)
         IF (I1.EQ.4) DEL(I1) = PF1-LOGST4
         IF (DEL(I1).LE.0.) GOTO 20
         IINT = IINT+1
      END DO
 
!-----Preparation of three-point Gaussian integration
20    DO I1 = 1,IINT
         DO I2 = 1,3
            I3 = 3*(I1-1)+I2
            IF (I1.EQ.IINT) GOTO 30
!--         The three points in the full-width intervals are standard
                           PFGAU(I3) = PFSTAN(I3)
            GOTO 40
30          CONTINUE
!--         The three points in the last interval are calculated
            IF (IINT.LE.3) PFGAU(I3) = LOG10(START(IINT)+PGAU(I2)* 
     &                                 DEL(IINT))
            IF (IINT.EQ.4) PFGAU(I3) = LOGST4+PGAU(I2)*DEL(IINT)
40          CONTINUE
!         Variables needed in the loop below
!       NEXT LINE CHANGED
!         CONDUC(I3) = EXP (ELOG10*AFGEN (CONTAB,ILCON,PFGAU(I3)))
!       START CHANGES
          CALL SUMSKM2(I,EXP(ELOG10*PFGAU(I3)),WCST,KMS, 
     &          VGA, VGL, VGN, VGR, KST, WCAD, WCSTRP)
          ! SUMSKM2 calculates the hydraulic conductivity at given suction for layer I on the basis of chosen option
          ! SUMSKM2(Layer index,suction,SAT,Hydraulic conductivity)
          CONDUC(I3) = KMS
         ! write(*,*)"I3=",I3,",KMS=", KMS
!       END CHANGES
            HULP(I3) = DEL(I1)*WGAU(I2)*CONDUC(I3)
            IF (I3.GT.9) HULP(I3)= HULP(I3)*ELOG10*EXP(ELOG10*PFGAU(I3))
         END DO
      END DO
 
!-----Setting upper and lower limit
      FU =4000. !!!!!JZW replaced  1.27  ! first Trial data of flow rate upper limit
! NEXT LINE CHANGED
!      FL = -1.*EXP (ELOG10*AFGEN (CONTAB, ILCON, PF1))
! START CHANGES
      !write(*,*)"---2 ", EXP(ELOG10*PF1), ", ",PF1
      CALL SUMSKM2(I,EXP(ELOG10*PF1),WCST,KMS, 
     &          VGA, VGL, VGN, VGR, KST, WCAD, WCSTRP)
      FL = -1.*KMS ! first Trial data of flow rate lower limit
! END CHANGES
      IF (MH.LE.D1) FU = 0.
      IF (MH.GE.D1) FL = 0.
      IF (MH.EQ.D1) GOTO 80
 
!-----Iteration loop
      IMAX   = 3*IINT
      DO I1  = 1,15
         FLW = (FU+FL)/2.
         DF  = (FU-FL)/2.
         IF ((DF.LT.0.01).AND.((DF/ABS(FLW)).LT.0.1)) GOTO 80
         Z   = 0.
         DO I2 = 1,IMAX
         !   write(*,*)I2, ", HULP=", HULP(I2)
            Z  = Z+HULP(I2)/(CONDUC(I2)+FLW)
         END DO
        ! write(*,*)I1,",Z=",Z,",D1=",D1,",FL=",FL,",FU=",FU,",FLW=",FLW
        ! write(*,*)I1,",Z=",Z,",D1=",D1,",FLW=",FLW
         IF (Z.GE.D1) FL = FLW
         IF (Z.LE.D1) FU = FLW
      END DO
 
!-----Output IN MM/D
80    FLOW = 10*(FU+FL)/2.
      !  Write(*,*) "FLOW=", FLOW
      RETURN
 
!----In case of small matric head
 
!    NEXT LINE CHANGED
!    K0   = EXP ( ELOG10 * AFGEN (CONTAB,ILCON,-1.) )
!    START CHANGES
90    K0   = KST(I)
!    END CHANGES
!    Flow in mm/d
      FLOW = 10*K0*(MH/D-1.)
 
      RETURN
      END SUBROUTINE SUBSL2
      
C=====================================================================
!  SUBSL2 VARIABLE DEFINITIONS: (updated Dec 2009)
!-----------------------------------------------------------------------
! CONDUC(I3) Hydraulic conductivity at given suction for layer I 
! D          Distance to grounwater table (cm)     
! DEL        The last integral interval of metrix head  
! ELOG10     ln(10)
! FL         The lower limit of flow (cm/d)   
! FLOW       Flow rate. Capillary rise calculated by subroutine SUBSL2 (mm d-1)
! FLW        Chosen flow
! FU         The upper limit of flow (cm/d)
! HULP()     The upstair of the integrand         
! I          Compartment index 
! IINT       Total number of integral interval of metrix head. Maximum is 4 
! K0         Hydraulic conductivity at saturation (cm/d)
! KMS        Hydraulic conductivity                      
! KST()      Array of saturated hydraulic conductivity, per soil layer (cm/d)
! LOGST4     log (330) where 330 is teh suction at fiels capacity
! MH         Metrix head in cm
! PF         pF value soil compartment PF=log10(MH)  
! PFGAU()    Gaussian integral points 
! PFSTAN()   Standard Gaussian integral points  
! PGAU       Gaussian point data, they are 0.5-sqrt(0.15), 0.5, 0.5+sqrt(0.15)
! START      Integral interval            
! WCST       Array of water content saturation for a layer (cm3 cm-3) 
! WGAU()     Weight of Gaussian integral. .2777778+.4444444+.2777778=1 and .4444/.2777=1.6
! Z          The distance between the middle of soil layer and water table
!-----------------------------------------------------------------------
!     END SUBROUTINE SUBSL2
!=======================================================================
!----------------------------------------------------------------------*
! SUBROUTINE SUMSKM2 form ORYZA2000\SourceCode\S_soil\subsoil.for      *                      *
!                                                                      *
! Purpose: SUMSKM2 calculates the hydraulic conductivity at            *
!          given suction for compartment I on the basis of chosen      *
!          option                                                      *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! I       I4  Compartment index (-)                                 I  *
! MS      R4  Soil water suction (cm)                               I  *
! WCST    R4  Array of water content saturation / layer (cm3 cm-3)  O  *
! KMS     R4  Hydraulic conductivity (cm d-1)                       O  *
!                                                                      *
! SUBROUTINES called : SUERR, SUWCMS2                                  *
!                                                                      *
! FUNCTIONS called   : none                                            *
!                                                                      *
! FILE usage : none                                                    *
!----------------------------------------------------------------------*
 
      SUBROUTINE SUMSKM2(I,MS,WCST,KMS,
     &          VGA, VGL, VGN, VGR, KST, WCAD, WCSTRP)

      USE ModuleDefs
      IMPLICIT NONE
!-----Formal parameters
      INTEGER I
      REAL    MS, WCST, KMS
 
!-----Local variables
      REAL    HLP1, HLP2, HLP3, MSAD, TINY, VGM, WCL, WREL

!-----Common blocks
      REAL VGA(NL), VGL(NL), VGN(NL), VGR(NL)
      REAL KST(NL), WCSTRP(NL), WCAD(NL)

!-----Variables retain their values between subsequent calls
!     of this subroutine
      SAVE
 
      DATA TINY/1.E-10/
      DATA MSAD/1.E7/
!-----Check input value MS
      IF (MS.LT.-TINY.OR.MS.GT.1.E8) CALL SUERR(1,MS,0.,1.E8)
 
      IF (MS.GE.MSAD-TINY) THEN
!--------Air dry
         KMS = 0.
      ELSE
!--------Calculate conductivity
!         IF (SWITKH.EQ.1) THEN
!-----------Van Genuchten conductivity
! Jin Note: This is same as RETC's Mualem's Model model if consider VGL= l (read as el) of RETC
            WCL = 0.
!           Dummy value; WCL is returned by SUWCMS2!
           ! write(*,*)"error here MS=", MS
            CALL SUWCMS2(I,2,WCST,WCL,MS, !Calculate water content from suction
     &          VGA, VGN, VGR, WCAD, WCSTRP)
          !   write(*,*)"error here WCL=", WCL
            VGM  = 1.0-1.0/VGN(I)
            WREL = (WCL-VGR(I))/(WCSTRP(I)-VGR(I))
            HLP1 = WREL**VGL(I)
            HLP2 = 1.0-WREL**(1./VGM)
            HLP3 = 1.0-HLP2**VGM
            KMS  = KST(I)*HLP1*HLP3*HLP3
           ! Write(*,*) "Hydraulic Conductivity at ",I,"th layer is ",KMS
!         ELSE IF (SWITKH.EQ.2) THEN
!!-----------Power function conductivity
!            IF (MS.LE.1.) KMS = KST(I)
!            IF (MS.GT.1.) KMS = KST(I)*(MS**PN(I))
!         ELSE IF (SWITKH.EQ.5) THEN
!!           user can here specify preferred conductivity function;
!!           the following two lines should be removed:
!            WRITE (*,10)
!            STOP
!         END IF
         IF (KMS.LT.TINY) KMS = 0.
      END IF

10    FORMAT (///,' *** fatal error; option SWIT3=5 requires ',/, 
     &          ' specification of conductivity function')

      RETURN
      END
! C=====================================================================
!  SUMSKM2 VARIABLE DEFINITIONS: (updated Dec 2009)
!-----------------------------------------------------------------------
! HLP       Intermediate variable
! KMS       Hydraulic conductivity for given MS(cm d-1)
! KST()     Array of saturated hydraulic conductivity, per soil layer (cm/d)
! MS        Soil water suction (cm), No layer index.
! PN        Power function parameter of hydraulic conductivity for each soil layer (see ORYZA book page 130,182)
!           KMS = KST(I)*(MS**PN(I))
! SWITKH    Hydraulic conductivity switch to determines whether no hydraulic conductivity characteristics
!            are available (SWITKH = 0) or are given as parameters of the van Genuchten function (SWITKH = 1), 
!            or are given as parameters of the power function (SWITKH = 2).
!            or user can here specify preferred conductivity function(SWITKH = 5).
! VGL       van Genuchten lambda parameter
! VGN       van Genuchten n parameter
! VGR       van Genuchten residual water content
! WCL     Soil water content (m3/m3) corresponding to MS, Note: there is no layer index
! WCSTRP    Array saturated volumetric water content ripened      
!               soil per soil compartment (cm3 cm-3)   
! WREL      Relative water content  
!-----------------------------------------------------------------------
!     END SUBROUTINE SUMSKM2
!=======================================================================
