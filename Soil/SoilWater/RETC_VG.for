!=======================================================================
!  RETC_VG, Subroutine, 

!  van Genuchten, M.Th., F.J. Leij and S.R. Yates. 1991. The RETC Code for 
!  Quantifying the Hydraulic Functions of Unsaturated Soils. EPA/600/2-91/065.

!  Based on RETC codes to predict the parameters of Van Genuchten equation for 
!  soil hydraulic property. Uses a nonlinear least-squares optimization approach 
!  to estimate the m, n, alpha from data of Rawls for retention vs SW . 
!  The approach is based on the partitioning of the total sum of squares of
!  the retention data array into a part described by the fitted equation and 
!  a residual part of observed values around those predicted with the model. 
!  The aim of the curve fitting process is to find an equation that maximizes the
!  sum of squares associated with the model, while minimizing the residual sum
!  of squares. The residual sum of squares reflects the contribution of random errors. 
!  RETC minimizes errors iteratively by means of a weighted least-squares approach.

!-----------------------------------------------------------------------
!  REVISION HISTORY
!  10/29/2009 CJ/JZW adapted RETC code for DSSAT
!  
!-----------------------------------------------------------------------
!  Called by: Function Diffus_Coef
!  Calls:     SUBROUTINE Retention
!=======================================================================
      SUBROUTINE RETC_VG( 
     &  TEXTURE, Ksat, LL, DUL, SAT, wcr, h_bub,  !Input
     &  alphaVG_R, mVG_R, nVG_R)                  !Output

!-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL RETENTION, MODEL_RETC, MATINV
      SAVE

      INTEGER i, j, K, kin, kiter, kout, kp, kwater, method, mit, mtype
      Integer IOR, KLOG, NEXP, NIT, NOB, NP, NWC, NWC1, NW
      REAL Ksat, LL, DUL, SAT, STOPCR, WCR
      REAL alphaVG_R, mVG_R, nVG_R
      Double Precision h_bub  !, lambda
      Double Precision alphaVG, ARG, ARG1, ANGLE, DERL, EXPO, GA 
      Double Precision nVG, mVG, RPF, RPZ, RLF, RLX, RLY, RSQ 
      Double Precision SDEV, SECOEF, SSQ, SSQW, SSQ1, SSQ2, SSQW1, SSQW2
      Double Precision SUMB, SUM1, SUM2, SUM3,SUMY, SUMF, SUMYF, SUMY2
      Double Precision SUMF2, SUM, STEP
      Double Precision Temp, TMCOE, TPCOE, TSEC, TVAR,  TVALUE
      Double Precision W1, W2, W12, WA, WB, XLOG, Z 
      Double Precision A(7,7), B(14), D(7,7), DELZ(100,7), E(7), F(200) 
      Double Precision indx(7), P(7), PHI(7), Q(7), R(200), TB(14)
      Double Precision TH(14), W(200), X(200), Y(200)
      CHARACTER:: AB(14)*5
!     CHARACTER:: AB(14)*5, TITLE*60, HEAD*60, PRNS*1
!     CHARACTER*12:: INF, OTF, RETPLT, CONPLT 
      CHARACTER*12 TEXTURE
!     Double Precision::  Lambda
      Double Precision :: SW_Rawls(38), h_Rawls(38) 
      ! Double Precision alphaVG
      DATA STOPCR/.00010/
      
      ! Rawls data is considered as observation data
    !my retc inputs
!     Method for hydraulic conductivity (COND), retention (RET)
      mtype = 3 !HC: Mualem's model, RET: VG model m=1-1/n
!     mtype = 4 !HC: Burdine's model, RET: VG model m=1-2/n
!     mtype = 5 !HC: Mualem's model, RET: Brook & Corey model m=1-1/n
!     mtype = 6 !HC: Burdine's model, RET: Brook & Corey model m=1-2/n 
      method = 5 !output diffusivity vs. water content eq. 33 RETC.pdf
      kwater = 1 !fitting only water retention data
      kin = 1 !number of iterations to be printed
      kout = 1 !tells to print hydraulic properties
      kiter = 8 !number of iteration results to print
      mit = 50 !maximum number of iterations
      AB(8) = 'WCR' !labels of variables
      AB(9) = 'WCS'
      AB(10) = 'ALPHA'
      AB(11) = 'N'
      AB(12) = 'M'
      AB(13) = 'L'
      AB(14) = 'CONDS'

      B(8) = wcr !residual water content
      B(9) = SAT !saturation water content
!     initial alpha guess - average value of alpha from RETC manual
      B(10) = 0.059545455  
!     initial n guess - average value of n from RETC manual
      B(11) = 1.258181818 
      B(12) = 1.-1./B(11) !sets m = 1-1/n
      B(13) = .5 !L
      B(14) = Ksat !saturated hydraulic conductivity
!     0-variable will not be fitted 1-variable will be fitted
      indx(1) = 0 
      indx(2) = 0
      indx(3) = 1
      indx(4) = 1
      indx(5) = 0 
      indx(6) = 0
      indx(7) = 0
      W1 = 0
      kp = 0
    
    ! Rawls data is considered as observation data
      Call Retention(TEXTURE, LL, DUL, SAT, H_bub,    !Input
     &  SW_Rawls, h_Rawls )                           !Output

      NOB = 0
      do i = 1,38
      ! Write(444,*) "Retention",",", SW_Rawls(i),",",h_rawls(i)
        x(i) = h_Rawls(i) !tension data from rawls
        y(i) = SW_Rawls(i) !water content data
        w(i) = 1 !weighting coefficient
        NOB = NOB + 1 !# observations(could include conductivity)
!        write(*,fmt='(F25.17,3x,F25.17)')h_Rawls(i),SW_Rawls(i) !delme
      end do
    
      NWC = NOB !number of retention points
      NW = 25 !number of predicted diffusivity data points

!     ----- READ INITIAL ESTIMATES -----
      IF(KWATER.EQ.1) indx(6) = 0
      IF(KWATER.EQ.1) indx(7) = 0
      IF(KWATER.EQ.2.AND.METHOD.LE.2) indx(3) = 0
      IF(KWATER.EQ.2.AND.METHOD.GE.5) indx(3) = 0

!      WRITE(KP,1008) TITLE
      IF(KP.EQ.8) CONTINUE !WRITE(7,1008) TITLE
      GO TO (5,5,1,2,3,3) MTYPE
    1 B(11) = DMAX1(1.05D0,B(11))   ! Parameter n
      B(12) = 1.-1./B(11)           ! Parameter m
      GO TO 4
    2 B(11) = DMAX1(2.05D0,B(11))
      B(12) = 1.-2./B(11)
      GO TO 4
    3 B(11) = DMAX1(0.005,B(11))
      B(12) = 1.0
    4 indx(5) = 0
    5 CONTINUE
      KLOG = 0
      IF(2 * (METHOD/2).EQ.METHOD) KLOG = 1
      IF(KWATER.EQ.1) KLOG = 0
      IF(MIT.EQ.0) GO TO 6
      IF(KWATER.NE.2) GO TO 6
      IF(METHOD.LE.2.OR.METHOD.GT.4) GO TO 6
      indx(1) = 0
      indx(2) = 0
    6 CONTINUE
      NP = 0
      IF(NOB.GT.0) CONTINUE 
      IF(NOB.EQ.0) CONTINUE
      IF(KP.EQ.8) THEN
      IF(KP.EQ.8) CONTINUE 
      IF(NOB.GT.0) CONTINUE 
      IF(NOB.EQ.0) CONTINUE 
      ENDIF
      IF(NOB.EQ.0) GO TO 14

!     ----- WRITE EXPERIMENTAL DATA -----
      WA = 0.
      IF(KWATER.EQ.2) GO TO 8
      DO 7 I = 1,NWC
      X(I) = DMAX1(X(I),1.D-5)
      IF(W(I).LT.1.D-3) W(I) = 1.0
      WA = WA + DABS(W(I) * Y(I))
      IF(KIN.EQ.0) GO TO 7
    7 CONTINUE
      WA = WA/FLOAT(NWC)
      IF(KWATER.EQ.1) GO TO 14
    8 IF(KIN.EQ.0) GO TO 9
    9 WB = 0.0
      IF(KWATER.EQ.2.AND.NWC.EQ.NOB) NWC = 0
      NWC1 = NWC + 1
      DO 10 I = NWC1,NOB
      J = I
      IF(KWATER.EQ.2) J = I-NWC
      X(J) = X(I)
      IF(METHOD.EQ.3.OR.METHOD.EQ.4) X(J) = DMAX1(X(J),1.D-5)
      Y(J) = Y(I)
      IF(KLOG. EQ. 1) Y(J) = DLOG10(Y(J))
      W(J) = W(I)
      IF(W(J). LT. 1.D-3) W(J) = 1.0
      WB = WB + DABS(W(J) * Y(J))
      IF(KIN.EQ.0) GO TO 10
      IF(KLOG.EQ.0) CONTINUE 
      IF(KP.EQ.8) THEN
      IF(KLOG.EQ.0) CONTINUE 
      IF(KLOG.EQ.1) CONTINUE 
      ENDIF
   10 CONTINUE
      IF(KWATER.LT.2) GO TO 11
      NOB = NOB-NWC
      NWC = 0
      NWC1 = 1
   11 IF(MIT.EQ.0) GO TO 14
      IF(W1.LT.1.D-3) W1 = 1.0
      WB = WB/FLOAT(NOB-NWC)
      W2 = WA/WB
      IF(KWATER.EQ.2) W2 = 1.0
      W12 = W1 * W2
      DO 12 I = NWC1,NOB
   12 W(I) = W12 * W(I)



!      ----- INITIALIZE UNKNOWN PARAMETERS -----
   14 NP = 0
      DO 15 I = 8,14
      TB(I) = B(I) 
      IF(indx(I-7).EQ.0) GO TO 15
      NP = NP + 1
      AB(NP) = AB(I)
      B(NP) = B(I)
      TB(NP) = B(I)
      TH(NP) = B(I)
   15 TH(I) = B(I)
      GA = 0.05
      DERL = 0.002D0
      NEXP = 1 + indx(1) + indx(2) + indx(3) + indx(4) + indx(5)
      IF(KWATER.EQ.1) NOB = NWC
!
!     ----- START LEAST-SQUARES ANALYSIS -----
    ! DO I = 1,NOB
  !     Write(*,*) "Model_retc0 output", X(I),",",F(I)
   !   Enddo
      
      CALL MODEL_retc(TH,F,X,NWC,NOB,MTYPE,METHOD,indx,IOR)
     
      IF(IOR.EQ.1) GO TO 94
      IF(MIT.EQ.0) GO TO 83
      SSQ = 0.
      DO 16 I = 1,NOB
   !    Write(*,*) "Model_retc1 output", X(I),",",F(I)
      R(I) = W(I) * (Y(I)-F(I))
   16 SSQ = SSQ + R(I) * R(I)
      NIT = 0
!
!     ----- BEGIN OF ITERATION -----
   18 NIT = NIT + 1
      GA = 0.05 * GA
      DO 22 J = 1,NP
      TEMP = TH(J)
      TH(J) = (1.D0 + DERL) * TH(J)
      Q(J) = 0
      CALL MODEL_retc(TH,DELZ(1,J),X,NWC,NOB,MTYPE,METHOD,indx,IOR)
      DO 20 I = 1,NOB
      DELZ(I,J) = W(I) * (DELZ(I,J)-F(I))
   20 Q(J) = Q(J) + DELZ(I,J) * R(I)
      Q(J) = Q(J)/(TH(J) * DERL)
!
!     ----- STEEPEST DESCENT -----
   22 TH(J) = TEMP
      DO 28 I = 1,NP
      DO 26 J = 1,I
      SUM = 0.0
      DO 24 K = 1,NOB
   24 SUM = SUM + DELZ(K,I) * DELZ(K,J)
      D(I,J) = SUM/(TH(I) * TH(J) * DERL**2)
   26 D(J,I) = D(I,J)
   28 E(I) = DSQRT(D(I,I))
   30 DO 32 I = 1,NP
      DO 32 J = 1,NP
   32 A(I,J) = D(I,J)/(E(I) * E(J))
!
!     ----- A IS THE SCALED MOMENT MATRIX -----
      DO 34 I = 1,NP
      P(I) = Q(I)/E(I)
      PHI(I) = P(I)
   34 A(I,I) = A(I,I) + GA
      CALL MATINV(A,NP,P)
!
!     ----- P/E IS THE CORRECTION VECTOR -----
      STEP = 1.0
   36 DO 38 I = 1,NP
   38 TB(I) = P(I) * STEP/E(I) + TH(I)
      DO 40 I = 1,NP
      IF(TH(I) * TB(I))44,44,40
   40 CONTINUE
      CALL MODEL_retc(TB,F,X,NWC,NOB,MTYPE,METHOD,indx,IOR)
      SUMB = 0.0
      DO 42 I = 1,NOB
      R(I) = W(I) * (Y(I)-F(I))
   42 SUMB = SUMB + R(I) * R(I)
   44 SUM1 = 0.0
      SUM2 = 0.0
      SUM3 = 0.0
      DO 46 I = 1,NP
      SUM1 = SUM1 + P(I) * PHI(I)
      SUM2 = SUM2 + P(I) * P(I)
   46 SUM3 = SUM3 + PHI(I) * PHI(I)
      ARG = SUM1/DSQRT(SUM2 * SUM3)
      ARG1 = 0.
      IF(NP.GT.1) ARG1 = DSQRT(1.-ARG * ARG)
      ANGLE = 57.29578 * DATAN2(ARG1,ARG)
!
!     ----------
      DO 48 I = 1,NP
      IF(TH(I) * TB(I))50,50,48
   48 CONTINUE
      IF((SUMB-SSQ)/SSQ.LT.1.D-5) GO TO 56
   50 IF(ANGLE-30.D0) 52,52,54
   52 STEP = 0.5 * STEP
      GO TO 36
   54 GA = 20. * GA
      GO TO 30
!
!     ----- PRINT COEFFICIENTS AFTER EACH ITERATION -----
   56 CONTINUE
      DO 58 I = 1,14
   58 TH(I) = TB(I)
      IF(indx(1).EQ.0) GO TO 60
      IF(NIT.LE.4.OR.TH(1).GT.0.001) GO TO 60
      indx(1) = 0
      B(8) = 0.0
      GO TO 14
   60 IF(indx(6).EQ.0) GO TO 64
      EXPO = TH(NEXP)
      IF(EXPO.GT.1.D-3) GO TO 64
      IF(EXPO.LT.-1.D-3) GO TO 64
      IF(EXPO.LT.0.) GO TO 62
      B(13) = -0.2
      GO TO 14
   62 B(13) = 0.0001
      indx(6) = 0
      GO TO 14
   64 DO 66 I = 1,NP
      IF(DABS(P(I) * STEP/E(I))/(1.D-20 + DABS(TH(I)))-STOPCR) 66,66,68
  66  CONTINUE
      GO TO 70
   68 SSQ = SUMB
      IF(NIT. LE. MIT) GO TO 18

!     ----- END OF ITERATION LOOP -----
   70 CONTINUE
      if(NIT. GE. KITER)then
      alphaVG = TH(10)
      nVG = TH(11)
      mVG = TH(12)
      end if
     
      CALL MATINV(D,NP,P)
!
!     ----- WRITE CORRELATION MATRIX -----
      DO 72 I = 1,NP
   72 E(I) = DSQRT(DMAX1(D(I,I),1.D-20))
      DO 76 I = 1,NP
      DO 74 J = 1,I
   74 A(J,I) = D(J,I)/(E(I) * E(J))
   76 CONTINUE  
!
!     ----- CALCULATE R-SQUARED OF FITTED VS OBSERVED VALUES -----
   78 SUM = 0.0
      SUMY = 0.0
      SUMF = 0.0
      SUMY2 = 0.0
      SUMF2 = 0.0
      SUMYF = 0.0
      DO 80 I = 1,NOB
      SUM = SUM + W(I)
      SUMY = SUMY + Y(I) * W(I)
      SUMF = SUMF + F(I) * W(I)
      SUMY2 = SUMY2 + Y(I) **2 * W(I)
      SUMF2 = SUMF2 + F(I)**2 * W(I)
   80 SUMYF = SUMYF + Y(I) * F(I) * W(I)
      RSQ = (SUMYF-SUMY * SUMF/SUM)**2/
     &  ((SUMY2-SUMY**2/SUM) * (SUMF2-SUMF**2/SUM))
!
!     ----- CALCULATE 95% CONFIDENCE INTERVAL -----
      Z = 1./FLOAT(NOB-NP)
      SDEV = DSQRT(Z * SUMB)
      TVAR = 1.96 + Z * (2.3779+Z*(2.7135+Z*(3.187936 + 2.466666*Z**2)))
      DO 82 I = 1,NP
      SECOEF = E(I) * SDEV
      TVALUE = TH(I)/SECOEF
      TVALUE = DMIN1(TVALUE,999999.D0)
      TSEC = TVAR * SECOEF
      TMCOE = TH(I)-TSEC
82      TPCOE = TH(I) + TSEC
!
!     ----- GIVE FINAL OUTPUT -----
   83   SSQ1 = 0.0
      SSQ2 = 0.0
      SSQW1 = 0.0
      SSQW2 = 0.0
      DO 84 I = 1,NWC
      XLOG = DLOG10(DMAX1(1.D-5,X(I)))
      R(I) = Y(I)-F(I)
      SSQ1 = SSQ1 + R(I)**2
  84  SSQW1 = SSQW1 + (R(I) * W(I))**2
      IF(KWATER.EQ.1) GO TO 89
!
!     ----- WRITE CONDUCTIVITY OR DIFFUSIVITY DATA -----
      DO 88 I = NWC1,NOB
      R(I) = Y(I)-F(I)
      SSQ2 = SSQ2 + R(I)**2
      SSQW2 = SSQW2 + (R(I) * W(I))**2
      RLX = DLOG10(DMAX1(1.D-30,X(I)))
      RLY = DLOG10(DMAX1(1.D-30,Y(I)))
      RPZ = 10.**DMIN1(3.D1,Y(I))
      RLF = DLOG10(DMAX1(1.D-30,F(I)))
      RPF = 10.**DMIN1(3.D1,F(I))
   88 CONTINUE
   89 IF(MIT.EQ.0) GO TO 90
      SSQ = SSQ1 + SSQ2
      SSQW = SSQW1 + SSQW2
   90 CONTINUE
   94 CONTINUE 
      alphaVG = TH(10)
      nVG = TH(11)
      mVG = TH(12)

      alphaVG_R = sngl(alphaVG)
      nVG_R = sngl(nVG)
      mVG_R = sngl(mVG)

      RETURN
      END SUBROUTINE RETC_VG
!
!=====================================================================
!-----------------------------------------------------------------------
!     RETC_VG VARIABLE DEFINITIONS:
!----------------------------------------------------------------------- 
! A(J,I)   Correlation Matrix.
! AB(I)    Correlation Matrix and initeration result.
! AB(I+7)  Labels of variables names.
! ALPHA    Coefficient " used in the retention models given by (4) and (7).
! B(I+7)   Initial values for the model parameters. 
! COND     Hydraulic conductivity K.
! CONDS    Saturated hydraulic conductivity, Ks
! CONPLT   Name of file for plotting the conductivity/diffusivity curve.
! DIF      Soil water diffusivity D.
! DWC      Interval in 2 for the printed output (when KOUT = 1) as determined by the input parameter
!             NW, i.e., DWC = 1/(NW-2), with some exceptions at the dry and wet ends of the retention
!             curve (see subroutine PRINT).
! EXPO     Coefficient R (see Eqs. 8 and 41).
! F(I)     Water content which will be fitted and are fitted
! h_Rawls  Retention array data which will be fitting
! KLOG     Code that determines whether or not K/D is being log-transformed before the parameter
!             optimization process (KLOG = 1)
! I        Observation number (Rawls data is considered as observation data)
! INDEX(I) Indices for the coefficients B(I), indicating whether the I-th coefficient is an
!          unknown and must be fitted to the data (INDEX(I) = 1), or whether that
!          coefficient is assumed to be known independently (INDEX(I) = 0).
! INF      Name of the data input file. If this file does not exist, a message "missing"
!          will be given and the program will not run.
! IOR      
! KIN      Determines if the observed data are printed (KIN= 1) before the
!             least-squares analysis, or are omitted from the output file (KIN=0).
! KOUT     Determines what to print (e.g. print hydraulic properties)
! Kiter     Number of iteration results to print 
! KWATER     Input code. If KWATER = 0, a simultaneous fit of the 2(h) and K/D data is
!             carried out. If KWATER = 1 or 2, only the retention or conductivity data
!             are analyzed, while for KWATER = 3 the curves are predicted based on the
!             initial estimates and no fitting is done.
! L        A pore-connectivity parameter in Mualem's Hydraulic conductivity equation
! MIT      Maximum number of iterations. Use a large number (e.g., 30) for the
!           simultaneous fit. If MIT= 0, the optimization part is by-passed and the soil
!            hydraulic properties are calculated from the inputted parameter values
!            according to the specified method.
! MTYPE    Type of model to be fitted to the data (see Table 5).
! METHOD   Type of conductivity/diffusivity data to be entered, i.e., K(2), K(h) or D(2);
!            if METHOD = 2, 4 or 6, the K or D data are internally transformed into
!            log(K) or log(D) (see Table 6).
! NIT      Number of iterations
! NOB      Dimension of the h_Rawls array plus one
! NW       Number of points for which diffusivity need to be predicted used only by the subroutine PRINT).
! NWC = NOB !number of retention points
! NP       Number of model parameters
! NT       Number of terms used for evaluating the Incomplete Beta Function. This function is needed
!             for unrestricted m and n. NT is best kept at a very conservative value of 10.
! NW       Number of 2 points at which the hydraulic functions are printed (if KOUT= 1) after the least--
!              squares analysis (subroutine PRINT)
! RN       Coefficient n (Eqs. 4 and 7)
! RM       Coefficient m (Eq. 7)
! RSQ      R-squared for Regression of Observed vs Fitted Values
! RWC      Reduced water content S (Eq. 6) 
! SSQ      Sum of squares 
! STOPCR   Stop criterion. The iterative analysis currently terminates when the relative change in the ratio
!           of any two coefficients is less than the value for STOPCR
! alphaVG  VG parameter
! mVG      VG parameter
! nVG      VG parameter 
! OTF      Name of the output file. If this name already exists, the default name
!           RETC.OUT will be used.
! R(I)     Water content which will be fitted and are fitted
! RETPLT   Name of file for plotting the retention curve.
! RSQ      R-squared for Regression of Observed vs Fitted Values
!            Nonlinear Least-Squares Analysis: Final Iteration Results: AB(I), TH(I), SECOEF, TVALUE, TMCOE, TPCOE
!            Variable name, value, Standard Error, T-value, and lower and upper confidence levels (95% level)
! SECOEF  Iteration result
! SW_Rawls Soil water content array data which will be fitting
! SUMB    Sum of squares (objective function)
! TH(I)   coefficient values. Iteration result
!               The program prints results for 0<=NIT<=KITER; the final iteration results are also printed. Only coefficients for which INDEX= 1 are given. Messages
!                 are printed if during the iteration 0 < Theta2 < 0.001 or -0.001 < ThetaR < 0, in which case Theta2, or ThetaR? are set to zero. r
! TITLE
! TMCOE   Iteration result
! TPCOE   Iteration result
! TValue  Iteration result
! W1      Weighting coefficient W in the objective function. 1
! w1      WEIGHTING COEFFICIENTS
! w2      WEIGHTING COEFFICIENTS
! w12     WEIGHTING COEFFICIENTS
! WCR     Residual water content (Eqs. 4 and 7 in RETC.pdf) 
! WCS     Saturated water content (Eqs. 4 and 7 in RETC.pdf)
! W(I)    Water content 
! XLOG    Log(x)
! X(I)    Soil water content array data which will be fitting and are fitted
! Y(I)    Retention array data which will be fitting and are fitted
!-----------------------------------------------------------------------
!     END SUBROUTINE RETC_VG
!=======================================================================

!=======================================================================
!  MODEL_retc, Subroutine, 
!  Based on RETC codes. Calculates the soil water retention and/or hydraulic
!conductivity/diffusivity functions as determined by the input variables METHOD and MTYPE.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  
!-----------------------------------------------------------------------
!  Called by: RETC_VG
!  Calls:     None
!=======================================================================  
      
      SUBROUTINE MODEL_retc(B,                          !Input
     & Y, X,                                             !Input & output
     & NWC, NOB, MTYPE, METHOD, indx, 
     & IOR)                                              !Output
!
!     PURPOSE: TO CALCULATE THE HYDRAULIC PROPERTIES
!
!     IMPLICIT real*8 (A-H,O-Z)
      IMPLICIT NONE
      EXTERNAL BINC
      INTEGER IOR, I, IND, K, METHOD, MTYPE, NOB, NWC
      Double Precision B(14),Y(200),X(200),indx(7)
      Double Precision A, AA, ALPHA, AX, BB, BETA, BINC, CONDS, EX, EXPO
      Double Precision DIF, DW, RWC, COND, WCL
      Double Precision DLG1, DLG2, DLG3, DLG4, DLGC, DLGA, DLGD, DLGW
      Double Precision RELK, RN, RM, RMN, RMT, TERM, WCR, WCS

      K = 0
      IOR = 0
      DO 2 I = 8,14
      IF(indx(I-7).EQ.0) GO TO 2
      K = K + 1
      B(I) = B(K)
    2 CONTINUE
      WCR = B(8)
      WCS = B(9)
      ALPHA = B(10)
      IND = indx(1) + indx(2) + indx(3) + indx(4)
      IF(MTYPE.EQ.1.OR.MTYPE.EQ.3) B(11) = DMAX1(1.005D0,B(11))
      IF(MTYPE.EQ.3) B(12) = 1.-1./B(11)
      IF(MTYPE.EQ.2.OR.MTYPE.EQ.4) B(11) = DMAX1(2.005D0,B(11))
      IF(MTYPE.EQ.4) B(12) = 1.-2./B(11)
    4 IF(indx(4).EQ.1) B(IND) = B(11)
      RN = B(11)
      RM = B(12)
      EXPO = B(13)
      CONDS = B(14)
      RMN = RM * RN
      DLGA = DLOG10(ALPHA)
      RMT = FLOAT(MTYPE-2 * ((MTYPE-1)/2))
      IF(NOB.EQ.NWC) GO TO 12
!
!     -----CALCULATE COMPLETE BETA FUNCTION----- JZW we do not need
      IF(MTYPE.GT.2) GO TO 10
      AA = RM + RMT/RN
      BB = 1.-RMT/RN
      IF(BB.GT.0.004) GO TO 8
      IOR = 1
      GO TO 60
!     JZW We should not use this
    8 BETA = GAMMA(AA) * GAMMA(BB)/GAMMA(RM + 1.) 
      WCL = DMAX1(2./(2. + RM),0.2D0)
      DLG1 = (3.0-RMT) * DLOG10(RN/(BETA * (RMN + RMT)))
   10 DLG2 = 3.0-RMT + EXPO + 2.0/RMN
      DLG3 = DLOG10(RMN * ALPHA * (WCS-WCR))
      DLG4 = DLOG10(CONDS)
      DLGC = -35.0
      DLGD = -35.0
!
!     ----- CALCULATE FUNCTIONAL VALUES Y(I) -----
   12 DO 54 I = 1,NOB
      IF(METHOD.EQ.3.OR.METHOD.EQ.4) GO TO 13
      IF(I.GT.NWC) GO TO 28
   13 AX = ALPHA * X(I)
      IF(AX.LT.1.D-20) GO TO 16
      EX = RN * DLOG10(AX)
      IF(MTYPE.LT.5) GO TO 14
      IF(AX.LE.1.) GO TO 16
      IF(EX.GT.10.) GO TO 20
      GO TO 22
   14 IF(EX.GT.-10.) GO TO 18
   16 RWC = 1.0
      GO TO 26
   18 IF(EX.LT.10.) GO TO 24
      EX = RM * EX
      IF(EX.LT.30.) GO TO 22
   20 RWC = 0.0
      GO TO 26
   22 RWC = AX**(-RM * RN)
      GO TO 26
   24 RWC = (1. + AX**RN)**(-RM)
   26 Y(I) = WCR + (WCS-WCR) * RWC
      IF(I.LE.NWC) GO TO 54
      GO TO 30
!
!     ----- CONDUCTIVITY DATA -----
   28 RWC = (X(I)-WCR)/(WCS-WCR)
   30 IF(RWC.GT.1.D-10) GO TO 31
      DLGC = -30
      DLGD = -30
      COND = 1.D-30
      DIF = 1.D-30
      GO TO 50
   31 IF(RWC.LT.0.999999D0) GO TO 32
      DLGC = DLG4
      COND = CONDS
      DLGD = 30.0
      DIF = 1.D30
      GO TO 50
   32 DLGW = DLOG10(RWC)
      DLGC = DLG2 * DLGW + DLG4
      DLGD = DLGC-DLG3-(RMN + 1) * DLGW/RMN
      IF(DLGC.LT.-30..OR.DLGW.LT.(-15. * RM)) GO TO 48
      IF(MTYPE.GT.4) GO TO 46
      DW = RWC**(1./RM)
      IF(MTYPE.GT.2) GO TO 42
!
!     ----- MTYPE = 1 OR 2 (VARIABLE M,N) -----
      IF(DW.GT.1.D-06) GO TO 34
      DLGC = DLGC + DLG1
      DLGD = DLGC-DLG3-(RMN + 1.) * DLGW/RMN
      GO TO 48
   34 IF(RWC-WCL) 36,36,38
   36 TERM = BINC(DW,AA,BB,BETA) !JZW we should not use this
      GO TO 44
   38 TERM = 1.-BINC(1.-DW,BB,AA,BETA)
      GO TO 44
!
!     ----- MTYPE = 3 OR 4 (RESTRICTED M,N) -----
   42 A = DMIN1(0.999999D0,DMAX1(1.D-7,1.-DW))
      TERM = 1.D0-A**RM
      IF(DW.LT.1.D-04) TERM = RM * DW * (1.-0.5 * (RM-1.) * DW)
   44 RELK = RWC**EXPO * TERM
      IF(RMT.LT.1.5) RELK = RELK * TERM
      DLGC = DLOG10(RELK) + DLG4
      DLGD = DLGC-DLG3-(RMN + 1.) * DLGW/RMN-(RN-1.) * DLOG10(1.-DW)/RN
      GO TO 48
!
!     ----- MTYPE = 5 OR 6 -----
   46 DLGD = DLG4-DLG3 + (2.0-RMT + EXPO + 1./RN) * DLGW
   48 DLGC = DMAX1(-30.D0,DLGC)
      DLGD = DMAX1(-30.D0,DLGD)
      DLGD = DMIN1(30.D0,DLGD)
      COND = 10.**DLGC
      DIF = 10.**DLGD
   50 IF(METHOD.EQ.1.OR.METHOD.EQ.3) Y(I) = COND
      IF(METHOD.EQ.2.OR.METHOD.EQ.4) Y(I) = DLGC
      IF(METHOD.EQ.5) Y(I) = DIF
      IF(METHOD.EQ.6) Y(I) = DLGD
 1000 FORMAT(I5,6D13.5)
   54 CONTINUE
   60 CONTINUE
      RETURN
      END SUBROUTINE MODEL_retc
!
!=====================================================================
!-----------------------------------------------------------------------
!     MODEL_retc VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! B(I) Initial estimates for the model parameters. If NOB=0, MIT=0, or WATER=3,
!         these initial estimates are also the final prescribed values for the
!         forward problem (no optimization).
!-----------------------------------------------------------------------
!     END SUBROUTINE MODEL_retc
!=======================================================================
!=======================================================================
!  MATINV, Subroutine, 
!  Based on RETC codes. Performs matrix inversions needed for the least-squares analysis
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  
!-----------------------------------------------------------------------
!  Called by: 
!  Calls:     None
!=======================================================================  

      SUBROUTINE MATINV(A,NP,B)
!
!     PURPOSE: TO INVERT THE MATRIX FOR PARAMETER ESTIMATION
!
      IMPLICIT real*8 (A-H,O-Z)
      INTEGER I, IC, IR, J, K, L, NP
      Double Precision A(7,7),B(7),indx(7,2)
      Double Precision AMAX, P

      DO 2 J = 1,7
    2 indx(J,1) = 0
      I = 0
    4 AMAX = -1.0
      DO 12 J = 1,NP
      IF(indx(J,1)) 12,6,12
    6 DO 10 K = 1,NP
      IF(indx(K,1)) 10,8,10
    8 P = ABS(A(J,K))
      IF(P.LE.AMAX) GO TO 10
      IR = J
      IC = K
      AMAX = P
   10 CONTINUE
   12 CONTINUE
      IF(AMAX) 30,30,14
   14 indx(IC,1) = IR
      IF(IR.EQ.IC) GO TO 18
      DO 16 L = 1,NP
      P = A(IR,L)
      A(IR,L) = A(IC,L)
   16 A(IC,L) = P
      P = B(IR)
      B(IR) = B(IC)
      B(IC) = P
      I = I + 1
      indx(I,2) = IC
   18 P = 1./A(IC,IC)
      A(IC,IC) = 1.0
      DO 20 L = 1,NP
   20 A(IC,L) = A(IC,L) * P
      B(IC) = B(IC) * P
      DO 24 K = 1,NP
      IF(K.EQ.IC) GO TO 24
      P = A(K,IC)
      A(K,IC) = 0.0
      DO 22 L = 1,NP
   22 A(K,L) = A(K,L)-A(IC,L) * P
      B(K) = B(K)-B(IC) * P
   24 CONTINUE
      GO TO 4
   26 IC = indx(I,2)
      IR = indx(IC,1)
      DO 28 K = 1,NP
      P = A(K,IR)
      A(K,IR) = A(K,IC)
   28 A(K,IC) = P
      I = I-1
   30 IF(I) 26,32,26
   32 RETURN
      END
!
!=====================================================================
!-----------------------------------------------------------------------
!     MATINV VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! 
!-----------------------------------------------------------------------
!     END FUNCTION MATINV
!=======================================================================

!=======================================================================
!  GAMMA, FUNCTION, 
!  Based on RETC codes. Evaluates the Gamma function. We do not need this
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  
!-----------------------------------------------------------------------
!  Called by: 
!  Calls:     None
!=======================================================================  

      FUNCTION GAMMA(Z)
!
!     PURPOSE:  TO CALCULATE THE GAMMA FUNCTION FOR POSITIVE Z
!
      IMPLICIT real*8 (A-H,O-Z)
      DOUBLE PRECISION FY, GAMMA, X, Y, Z

      IF(Z.LT.33.) GO TO 2
      GAMMA = 1.D36
      RETURN
    2 X = Z
      GAMMA = 1.0
      IF(X-2.0) 10,10,8
    6 IF(X-2.0) 14,14,8
    8 X = X-1.0
      GAMMA = GAMMA * X
      GO TO 6
   10 IF(X-1.0) 12,16,14
   12 GAMMA = GAMMA/X
      X = X + 1.0
   14 Y = X-1.0
      FY = 1.0-Y * (.5771017-Y * (.985854-Y * (.8764218-
     & Y * (.8328212-Y * (.5684729-Y * (.2548205-.0514993 * Y))))))
      GAMMA = GAMMA * FY
   16 RETURN
      END
!=====================================================================
!-----------------------------------------------------------------------
!     FUNCTION GAMMA VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! 
!-----------------------------------------------------------------------
!     END FUNCTION GAMMA
!=======================================================================

!=======================================================================
!  BINC, FUNCTION, 
!  Based on RETC codes. Evaluate BINC the Incomplete Beta Function (Eq. 21 of RETC). We do not need this
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  
!-----------------------------------------------------------------------
!  Called by: 
!  Calls:     None
!=======================================================================  

      FUNCTION BINC(X,A,B,BETA)
!
!     PURPOSE: TO CALCULATE THE INCOMPLETE BETA-FUNCTION
!
      IMPLICIT real*8 (A-H,O-Z)
      INTEGER I, K, NT, NT1
      DOUBLE PRECISION A, B, BETA, BINC, T, X, Y, Y2
      DIMENSION T(200)
      DATA NT/10/
      NT1 = NT + 1
      T(1) = -(A + B) * X/(A + 1.0)
      DO 2 I = 2,NT,2
      Y = FLOAT(I/2)
      Y2 = FLOAT(I)
      T(I) = Y * (B-Y) * X/((A + Y2-1.0) * (A + Y2))
    2 T(I + 1) = -(A + Y) * (A + B + Y) * X/((A + Y2) * (A + Y2 + 1.0))
      BINC = 1.0
      DO 4 I = 1,NT
      K = NT1-I
    4 BINC = 1. + T(K)/BINC
      BINC = X**A * (1.-X)**B/(BINC * A * BETA)
!**********************************************************************
      RETURN
      END 
!=====================================================================
!-----------------------------------------------------------------------
!     FUNCTION BINC VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! 
!-----------------------------------------------------------------------
!     END FUNCTION BINC
!=======================================================================

!=======================================================================
!  Retention, Subroutine, 
!  Based on paper "Soil Water Characteristic Estimates by Texture and Organic 
!  Matter for Hydrologic Solutions" by K. E. Saxton and W. J. Rawls, Aug. 2006
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  
!-----------------------------------------------------------------------
!  Called by: Subroutine RETC_VG
!  Calls:     None
!=======================================================================

!=======================================================================
      SUBROUTINE Retention(TEXTURE, LL, DUL, SAT, H_bub,  !Input
     &  SW_Rawls, h_Rawls )                               !Output
     
      IMPLICIT NONE
    !  SAVE

      INTEGER i
      Double Precision Aparam, Bparam, tempStep
      Double Precision  H_bub
      REAL LL, DUL, SAT 
      Double Precision :: SW_Rawls(38), h_Rawls(38) 
      CHARACTER*12 TEXTURE
      
      
!     Checks whether soil is coarse to determine what kPa value to use for SDUL
      LOGICAL coarse
      coarse = .FALSE.
      IF (TEXTURE == 'Sand') THEN
       coarse = .TRUE.
        ELSE IF (TEXTURE == 'LoamySand') THEN
           coarse = .TRUE.
        ELSE IF (TEXTURE == 'SandyLoam') THEN
           coarse = .TRUE.
        END IF
      
      
      !Adjust SAT, DUL, LL by bulk density
      !NBD = (1 - SAT) * 2.65 
      !DensFact = 1.0     ! set density adjust factopr as one for now
      !BDadj = NBD * DensFact 
      !SAT_DFadj = 1 - BDadj / 2.65 
      !DUL_DFAdj = DUL - 0.2 * (SAT- SAT_DFadj) 
!       Here we assume that the DUL is at 33 kPa for fine texture soil. (Page 467 Hillel 1998) 
!       For coarse-textured soil, field capacity is at 10kPa, log(33.) should be replaced by log(10.)
      
      !calculations for non-coarse soils
      IF (.not.coarse) THEN
          Bparam = (log(1500.) - log(33.)) / (log(DUL) - log(LL))
          Aparam = exp( log(33.) + ( Bparam * log(DUL) ) )
      
          !Range of retention: 1500kPa to 33 kPa
          SW_Rawls(1) = LL
          h_Rawls(1) = Aparam * ( SW_Rawls(1) ** ( - Bparam ) )
          tempStep = (DUL - LL ) / 30
          do i = 2 , 31
             SW_Rawls(i) = LL + tempStep * (i-1)
             h_Rawls(i) = Aparam * ( SW_Rawls(i) ** (- Bparam ) )
          end do
       
          !Range of retention: 33 kPa to saturation
          tempStep = ( SAT - DUL) / 7
          do i = 32, 38
             SW_Rawls(i) = SW_Rawls( i - 1) + tempStep
             !        here H_bub in kPa
             h_Rawls(i) =33-((SW_Rawls(i) - DUL) * (33- H_bub )/(SAT -
     &       DUL))
          end do
          h_Rawls = h_Rawls * 10.197 !convert to cm
       
       !calculations for coarse soils
       ELSE
          Bparam = (log(1500.) - log(10.)) / (log(DUL) - log(LL))
          Aparam = exp( log(10.) + ( Bparam * log(DUL) ) )
      
          !Range of retention: 1500kPa to 10 kPa
          SW_Rawls(1) = LL
          h_Rawls(1) = Aparam * ( SW_Rawls(1) ** ( - Bparam ) )
          tempStep = (DUL - LL ) / 30
          do i = 2 , 31
             SW_Rawls(i) = LL + tempStep * (i-1)
             h_Rawls(i) = Aparam * ( SW_Rawls(i) ** (- Bparam ) )
          end do
       
          !Range of retention: 10 kPa to saturation
          tempStep = ( SAT - DUL) / 7
          do i = 32, 38
             SW_Rawls(i) = SW_Rawls( i - 1) + tempStep
             !        here H_bub in kPa
             h_Rawls(i) =10-((SW_Rawls(i) - DUL) * (10- H_bub )/(SAT - 
     &       DUL))
          end do
          h_Rawls = h_Rawls * 10.197 !convert to cm
       END IF
       
      RETURN
      END SUBROUTINE Retention
!=====================================================================
!-----------------------------------------------------------------------
!     Retention VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! Aparam      Tension parameter A
! Bparam      Tension parameter B
! BDAdj       Adjusted bulk density
! DensFact    Bulk Density adjustment Factor (0.9¨C1.3)
! DUL_DFAdj   Field capasity (at 33 kPa), adjusted by bulk density
! H-bub       Tension at air entry (bubbling pressure), kPa
! h_Rawls     Retention (Matric Potential) in Rawls model
! NBD         Normal bulk density, g/cm3
! SAT_DFadj   Saturated moisture (at 0 kPa), adjusted by bulk density
! SW_Rawls    Soil water content in Rawls model
!-----------------------------------------------------------------------
!     END SUBROUTINE Retention
!=======================================================================
!=======================================================================
!  calBrokCryPara, Subroutine
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  10/06/2008  
!  08/08/2010   CDJ changed DUL to 10 kPa for coarse soils
!-----------------------------------------------------------------------
! Calculate the Brooks and Corey parameter
!-----------------------------------------------------------------------
!  Called by: 
!  Calls    : 
!=======================================================================
	SUBROUTINE calBrokCryPara(TEXTURE, wcs, wcpwp, wcfc,  !Input
     +   wcr, hb, lambda)  !output                         

      CHARACTER*12 TEXTURE
!	CHARACTER*70 errmsg
	Real wcs, wcpwp, wcfc, wcr,temp
	Double Precision hb, lambda 
	Double Precision wcr_wcpwp
	LOGICAL coarse
! define file reading variables
!	CHARACTER*78 MSG
!	 Air dry water content (wcr here) is set as SWAD(L) = 0.30 * LL(L) in SUBROUTINE ESR_SoilEvap_2D
!       Data are from table 4-2 in thesis of LESLIE C. GOWDISH 
!       van Genuchten water characteristic curve parameters used in Richards¡¯numerical solutions
      coarse = .FALSE.
      
      SELECT CASE(Trim(TEXTURE))
        CASE ("Sand")        !' Coarse  
	    wcr_wcpwp =0.606d0
	    coarse = .TRUE.
        CASE ("LoamySand")   !' Coarse  
	    wcr_wcpwp =0.636d0
	    coarse = .TRUE.
        CASE ("SandyLoam")   !' Coarse 
          wcr_wcpwp =0.4316d0
          coarse = .TRUE.
	  CASE ("Loam")   
	    wcr_wcpwp =0.2308d0
        CASE ("SiltyLoam")  
	    wcr_wcpwp =0.1128d0
       ! CASE ("Silt")
       !   wcr_wcpwp =?d0
        CASE ("SandClayLoam")  
	    wcr_wcpwp =0.459d0
        CASE ("ClayLoam")   
	    wcr_wcpwp =0.3807d0
        CASE ("SiltClayLoam")
          wcr_wcpwp =0.1923d0
	  CASE ("SandyClay")  
	    wcr_wcpwp =0.4561d0
        CASE ("SiltyClay")   
	    wcr_wcpwp =0.224d0
        CASE ("Clay")
          wcr_wcpwp =0.3309d0
	  CASE DEFAULT 
	    wcr_wcpwp =0.3d0
          !write(*,*) "unknown soil texture 2", TEXTURE
	END SELECT  
      wcr = wcr_wcpwp * wcpwp
!       temp = ln(h_pwp/h_fc) based on Eq. 2-9 in thesis of LESLIE C. GOWDISH 
      if (COARSE) then
        temp = 5.0106 ! ln(1500/10)
      else
        temp = 3.8166 ! ln(1500/33)
      end if
	lambda = ALOG((wcfc -wcr)/(wcpwp-wcr) )/temp
      hb = 1500d0 * ((wcpwp -wcr)/(wcs-wcr))**(1. / lambda)  !kPa?
	RETURN      
      END SUBROUTINE calBrokCryPara
!=======================================================================
! calBrokCryPara VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! lambda     Brooks and Corey parameter
! wcr        Residual water content
! wcpwp      ?Wilting point water content  
! wcr_wcpwp  Ratio of Residual water content and ?Wilting point water content 
!-----------------------------------------------------------------------
!     END SUBROUTINE calBrokCryPara	 
!=====================================================================


C=======================================================================
C  WaterPotential, Subroutine
C  Calculates Matrix Potential 
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C-----------------------------------------------------------------------
C  Called by: SOILDYN
C=======================================================================

      SUBROUTINE WaterPotential(
     &    SW, SOILPROP, FLOOD,       !Input
     &    WPkpa)                     !Output
!-----------------------------------------------------------------------   
      USE ModuleDefs  
      IMPLICIT NONE

      !  vanGenuchten parameters
      REAL, DIMENSION(NL), INTENT(IN) :: SW
      TYPE (SoilType)    , INTENT(IN) :: SOILPROP
      REAL, DIMENSION(NL), INTENT(OUT):: WPkpa
      INTEGER L
      REAL  Se, WPcm, WCr, SAT, mVG, nVG, alphaVG, FLOOD

      WPkpa = 0.0
      IF (FLOOD > 1.E-6) RETURN

      DO L = 1, SOILPROP % NLAYR
        WCr = SOILPROP % WCr(L)
        SAT = SOILPROP % SAT(L)
        mVG = SOILPROP % mVG(L)
        nVG = SOILPROP % nVG(L)
        alphaVG = SOILPROP % alphaVG(L)

!       Normalized water content
        Se = (SW(L) - WCr) / (SAT - WCr)
        Se = MIN(MAX(Se, 0.00001),1.0)

!       Water Potential
        WPcm  = ((Se ** (-1.0/mVG) - 1.) ** (1./nVG)) / alphaVG  !cm H2O
        WPkpa(L) = WPcm * 0.0981      !kPa
      ENDDO

      RETURN      
      END SUBROUTINE WaterPotential
C=======================================================================
!=======================================================================
!     WaterPotential VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! Se    Relative soil water content
! WPcm  Matrix potential
!-----------------------------------------------------------------------
!     END SUBROUTINE WaterPotential
!=======================================================================
!=======================================================================
!  PotentialOp, Subroutine
!  Output daily info for Potential 
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  
!=======================================================================

      Subroutine PotentialOp(CONTROL, ISWITCH, SOILPROP, WPkPa) 
!-----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, YR_DOY, INCDAT
      SAVE
!-----------------------------------------------------------------------
      integer DYNAMIC, YR2, DY2, DAS, LUNPotential, L !, LIMIT_2D
      INTEGER INCDAT, N1_LYR, N2_LYR
      ! REAL, INTENT(IN) :: MgmtWTD
      TYPE (ControlType), INTENT(IN) :: CONTROL
      Type(SoilType) , INTENT(INOUT):: SOILPROP
      TYPE (SwitchType)   ISWITCH

      REAL, DIMENSION(NL):: WPkPa, DUL, LL, SAT, LrTop, DS !NLAYR
!     REAL ThetaCa(25)
      CHARACTER*17, PARAMETER :: PotentialOut = 'Potential.OUT'
      LOGICAL FEXIST, DOPRINT
      !    CALL GET('MGMT','WATTAB',MgmtWTD)

      DYNAMIC = CONTROL % DYNAMIC
      
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
        DOPRINT=.TRUE.
        IF (ISWITCH % IDETW .EQ. 'N') THEN
          DOPRINT=.FALSE.
        ENDIF
        IF (ISWITCH % ISWWAT .EQ. 'N') THEN
          DOPRINT=.FALSE.
        ENDIF
        IF (ISWITCH % IDETL /= 'D') THEN
          DOPRINT=.FALSE.
        ENDIF
        IF (.NOT. DOPRINT) RETURN

        DO L = 1, min(25, SOILPROP % NLAYR)
          DS(L) = SOILPROP % DS(L)
          LrTop(L) = DS(L) - SOILPROP % DLAYR(L)
          LL(L)   = SOILPROP % LL(L)
          DUL(L)  = SOILPROP % DUL(L)
          SAT(L)  = SOILPROP % SAT(L)
        EndDo

!       Open output file
        CALL GETLUN('PotentialOut', LUNPotential)
        INQUIRE (FILE = PotentialOut, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = LUNPotential,FILE =PotentialOut,STATUS = 'OLD',
     &          POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = LUNPotential,FILE =PotentialOut,STATUS = 'NEW')
          WRITE(LUNPotential,
     &         '("*Daily Potential for each Soil layer")')
        ENDIF

        CALL HEADER(SEASINIT, LUNPotential, CONTROL % RUN)
!        if (CONTROL % RUN .eq. 1) then
        !  WRITE (LUNPotential, '(I2)') &
        !      ('! LIMIT_2D is ', LIMIT_2D) 
  !      Endif
 
        N1_LYR = Min (11, SOILPROP % NLAYR)
        N2_LYR = Min (25, SOILPROP % NLAYR) 

        WRITE (LUNPotential,1115, ADVANCE='NO')'!    Variable ' 
        WRITE (LUNPotential,1116) ("LYR",L, L=1,N1_LYR),          
     &         ("LYR",L, L=13,N2_LYR, 2)
        WRITE (LUNPotential,1115, ADVANCE='NO')'!   Top Layer'
        WRITE (LUNPotential,1118) (LrTop(L),L=1,N1_LYR),           
     &         (LrTop(L),L=13,N2_LYR,2)
        WRITE (LUNPotential,1115, ADVANCE='NO')'!Bottom Layer'
        WRITE (LUNPotential,1118) (DS(L),L=1,N1_LYR),              
     &         (DS(L),L=13,N2_LYR,2)
        WRITE (LUNPotential,1115, ADVANCE='NO')'!          LL'
        WRITE (LUNPotential,1119) (LL(L),L=1,N1_LYR),              
     &         (LL(L),L=13,N2_LYR,2)
        WRITE (LUNPotential,1115, ADVANCE='NO')'!         DUL'
        WRITE (LUNPotential,1119) (DUL(L),L=1,N1_LYR),             
     &         (DUL(L),L=13,N2_LYR,2)
        WRITE (LUNPotential,1115, ADVANCE='NO')'!         SAT'
        WRITE (LUNPotential,1119) (SAT(L),L=1,N1_LYR),             
     &         (SAT(L),L=13,N2_LYR,2)
        WRITE (LUNPotential,1115)
 
        WRITE (LUNPotential,1120, ADVANCE='NO')'@YEAR DOY DAS '
        WRITE (LUNPotential,1116) ("LYR",L, L=1,N1_LYR), 
     &      ("LYR",L, L=13,N2_LYR, 2)
    !    WRITE (LUNPotential,1120)' mgWTD LIMIT_2D'
        
        CALL YR_DOY(INCDAT(CONTROL % YRDOY,-1),YR2,DY2)
        WRITE (LUNPotential,1300) YR2, DY2, DAS, 
     &        (WPkPa(L),L=1,N1_LYR), (WPkPa(L),L=13,N2_LYR,2)
  !      WRITE (LUNPotential,1400) MgmtWTD 
      
!***********************************************************************
!       DAILY RATE CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN        
      
        CALL YR_DOY(CONTROL % YRDOY, YR2, DY2)
        DAS = CONTROL % DAS
        WRITE (LUNPotential,1300) YR2, DY2, DAS, 
     &      (WPkPa(L),L=1,N1_LYR), (WPkPa(L),L=13,N2_LYR,2)
  !      WRITE (LUNPotential,1400) MgmtWTD  
 1115   FORMAT(13A)
 1116   FORMAT(9(2X,A5,I1), 9(1X,A5,I2)) 
 1118   FORMAT(1x, 18F8.1) 
 1119   FORMAT(1x, 18F8.2) 
 1120   FORMAT(14A)     
 1122   FORMAT(A1, 7X, A5, 1X, 18F8.3, F8.1,3X)   
 1300   FORMAT(1X,I4,1X,I3,1X,I3, 1X, 18F8.3, F8.1,3X, I2) 
 1400   FORMAT(F8.1,3X, I2) 
!***********************************************************************
!       END OF DYNAMIC IF CONSTRUCT
!***********************************************************************  
      Endif  
      RETURN
      End Subroutine PotentialOp
!=======================================================================
!=======================================================================
!     PotentialOp VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!     END SUBROUTINE PotentialOp
!=======================================================================
