C=======================================================================
C  SALUS_T, Subroutine
C
C  Determines soil temperature by layer
C  Adapted from SALUS version (Ritchie et al)
C  Revised and adpated for DSSAT4 by U.Singh and D.Godwin Feb 2004
C-----------------------------------------------------------------------
C  Called : Main
C  Calls  :
C=======================================================================

      SUBROUTINE SALUS_T(CONTROL, ISWITCH,
     &    SOILPROP, SRAD, SW, TMAX, TMIN, XLAT, TAV, TAMP,!Input
     &    SRFTEMP, ST)                                    !Output

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types,
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT  NONE
      EXTERNAL YR_DOY, FIND, WARNING, OPSTEMP, ERROR
      SAVE

      CHARACTER*1  RNMODE, ISWWAT
      CHARACTER*6  SECTION
      CHARACTER*6, PARAMETER :: ERRKEY = "STEMP "
      CHARACTER*30 FILEIO
      CHARACTER*78 MSG(10)

      INTEGER DOY, DYNAMIC, I, L, MSGCOUNT, NLAYR
      INTEGER RUN, YRDOY, YEAR
      INTEGER ERRNUM, FOUND, LINC, LNUM, LUNIO

      INTEGER DoyNH   !, COLDLAG

      !REAL A(0:20),B(0:20),D(0:20),E(0:20)
      REAL DELT,F
      REAL THETA1, THETA2,THETA3,THETA4,SWL1REL,WATERFactor,TSOIL2cm
      REAL FRMIN, ATHETA, FQRZ, ACV, ALBD, FT, ZDAMP  !, FWTEMP

      !Real Cvav,Fqrz,Frmin,Ft,Thetav
	! ABD = BdAv, ACV=CVAV ATHETA = THETAV

	REAL ABD, CUMDPT
      REAL ICWD, SRFTEMP
      REAL TAMP, TAV, TBD, XLAT, TMAX, TMIN, SRAD
      REAL TTMP, TOT_TMP,TMEAN
	REAL TMFAC1(8)
      REAL, DIMENSION(NL) :: BD, DLAYR, DS, DUL, LL, ST, SW, SWI
      REAL, DIMENSION(NL) :: SAND, CC1, C2, C3, C4, CD, LMBD, LBD, CP
	REAL, DIMENSION(NL) :: CV,SW_A2DAY, A, B, D, E, ST_YEST, SW_YEST
	REAL, DIMENSION(NL) :: SAT       !, SWDELT_YEST, WTEMP
!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "SOILPROP" is of type "SoilType".
      TYPE (SoilType) SOILPROP
      TYPE (SwitchType) ISWITCH

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY

      ISWWAT = ISWITCH % ISWWAT

      BD     = SOILPROP % BD
      DLAYR  = SOILPROP % DLAYR
      DS     = SOILPROP % DS
      DUL    = SOILPROP % DUL
      LL     = SOILPROP % LL
      NLAYR  = SOILPROP % NLAYR
      SAT    = SOILPROP % SAT
!-----------------------------------------------------------------------
      CALL YR_DOY(YRDOY, YEAR, DOY)

!***********************************************************************
!***********************************************************************
!     Run initialization - run once per simulation
!***********************************************************************
!      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
!      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE

      IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN
        IF (ISWWAT .NE. 'N') THEN

!         Read inital soil water values from FILEIO
!         (not yet done in WATBAL, so need to do here)
          OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)
          LNUM = 0
          SECTION = '*INITI'
          CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
          IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO, LNUM)

          READ(LUNIO, 150, IOSTAT=ERRNUM) ICWD ; LNUM = LNUM + 1
  150     FORMAT(40X,F6.0)
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)

          DO L = 1, NLAYR
            READ(LUNIO,'(9X,F5.3)',IOSTAT=ERRNUM) SWI(L)
            LNUM = LNUM + 1
            IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
            IF (SWI(L) .LT. LL(L)) SWI(L) = LL(L)
	      SW_YEST(L)     = SWI(L)
!	      SWDELT_YEST(L) = 0.0
          ENDDO

          CLOSE (LUNIO)
        ELSE
          SWI         = DUL
	    SW_YEST     = DUL
!	    SWDELT_YEST = 0.0
        ENDIF

        MSGCOUNT = 0
        IF (TAV  .LE. 0.0) THEN
          TAV = 20.0
          MSG(1) = " Soil Temperature routines:"
          MSG(2) = " Default value of 20°C used for average"
     &                    // " annual soil temperature."
          MSGCOUNT = 2
        ENDIF

        IF (TAMP .LE. 0.0) THEN
          TAMP = 5.0
          IF (MSGCOUNT > 0) THEN
            MSGCOUNT = MSGCOUNT + 1
          ELSE
            MSG(1) = "Soil Temperature routines:"
            MSGCOUNT = 2
          ENDIF
          MSG(MSGCOUNT) = " Default value of 5°C used for amplitude"
     &                        // " of soil temperature function."
        ENDIF

        IF (MSGCOUNT > 0) THEN
          CALL WARNING(MSGCOUNT, ERRKEY, MSG)
        ENDIF

C  ***If southern hemisiphere convert to DOY of northern hemisphere doyNH*******

   !     COLDLAG = 5
        IF (XLAT.gt.0) then
           DoyNH = DOY                        !DayofYear
        ELSE
           IF (DOY.lt.184) then
               DoyNH = DOY + 182
           ELSE
               DoyNH = DOY -182
           ENDIF
        ENDIF

        TBD    = 0.0
        ATHETA = 0.3
        ALBD   = 800
        CUMDPT = 0.0
        DO L = 1, NLAYR
           CUMDPT = CUMDPT + DLAYR(L)
!           TBD    = TBD + BD(L)
!     7/24/2006 CHP added DLAYR to TBD eqn.
           TBD    = TBD + BD(L) * DLAYR(L)
c        *** Calculating heat conductivity parameters (after McINNES)
           FRMIN = BD(L)/2.65
c        *** Clay with data of Ratcliff et al., SSSAJ 47,1983,770-775:
           FT=(0.036+8.34*LL(L)*LL(L))*FRMIN
c        *** Fqrz with data of Ratcliff et al.:
C        *** Use sand from main model instead of calculating it
           SAND(L)=(.897+2.195*LL(L)-50.6*LL(L)*LL(l))*FRMIN
           IF (SAND(L) .gt.0.0) THEN
              FQRZ = 0.6*sand(L)
           ELSE
              FQRZ = 0.0
           ENDIF

           CC1(L) = 0.57+1.73*FQRZ+0.93*(FRMIN-FQRZ)
           CC1(L) = CC1(L)/(1.-0.74*FQRZ-0.49*(FRMIN-FQRZ))
           CC1(L) = CC1(L)-2.8*FRMIN*(1.-FRMIN)
           C2(L)  = 1.06*BD(L)
           C3(L)  = 1.+2.6/sqrt(FT)
           C4(L)  = 0.03+0.1*BD(L)*BD(L)
c        *** Calculating heat capacity of solid matter (min. soils) **
           CD(L)  = FRMIN*2.23
        ENDDO
        ABD   = TBD / DS(NLAYR)
        ACV   = ABD/2.65*2.25+ATHETA*4.186
        ZDAMP = sqrt(2.*ALBD/ACV/0.0172)
C********** CALCULATE INITIAL SOIL TEMPERATURE *********************

        ST_YEST(1) = TAV+0.8+TAMP*1.1*sin(pi*float(DoyNH-105)/182.5)

C     *Theoretical DE solution (with temperature gradient):

        IF (ALBD .GT. 9000) THEN
           ST_YEST(NLAYR+1) = TAV+0.008*(CUMDPT+5.)+TAMP*(1.-0.0172*
     1         sqrt(CUMDPT+5.))*sin(pi*(DoyNH-(105.+0.212*
     2         (CUMDPT+5.)))/182.5)
        ELSE
           ST_YEST(NLAYR+1) = TAV+0.008*(CUMDPT+5.)+TAMP*
     1         exp(-(CUMDPT+5.)/ZDAMP)*cos(0.01721*(DoyNH-195)-
     2         (CUMDPT+5.)/ZDAMP)
        ENDIF

!	  WTEMP(1) = ST_YEST(1)
               !Z(L) = DS(L)
        DO L = 2,NLAYR
           ST_YEST(L)=ST_YEST(1)+DS(L)/CUMDPT*
     &               (ST_YEST(NLAYR+1)-ST_YEST(1))
!           WTEMP(L)=ST_YEST(L)

        END DO
        DO I = 1, 8
          TMFAC1(I) = 0.931+0.114*I-0.0703*I**2+0.0053*I**3
        END DO	
      ENDIF

!     Print soil temperature data in STEMP.OUT
      CALL OPSTEMP(CONTROL, ISWITCH, DOY, SRFTEMP, ST, TAV, TAMP)

!***********************************************************************
!***********************************************************************
!     Daily rate calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
        TBD = 0.0
        DO L = 1, NLAYR
           TBD = TBD + BD(L) * DLAYR(L)
        ENDDO

        ABD   = TBD / DS(NLAYR)
	  ACV   = ABD/2.65*2.25+ATHETA*4.186
        ZDAMP = sqrt(2.*ALBD/ACV/0.0172)
C     First calculate the soil temperature at the top two cm
C
        THETA1      = 1
        THETA2      = -0.5
        THETA3      = 6
        THETA4      = -0.09
        SWL1REL     = (SW(1) - LL(1))/(SAT(1) - LL(1))
        SWL1REL     = AMIN1 (1.0,SWL1REL)
        SWL1REL     = AMAX1 (0.0,SWL1REL)
        WATERFactor =  THETA1 / ((1 + (THETA2 *
     &              (EXP( -1 * THETA3 * SWL1Rel)))) ** (1 / THETA4))

        TOT_TMP=0.0
        DO I = 1, 8
           TTMP = TMIN + TMFAC1(I)*(TMAX-TMIN)
	     TOT_TMP = TOT_TMP + TTMP
        END DO
	  TMEAN = TOT_TMP/8.

        TSOIL2cm    = 4.5 + (0.188 * SRAD) + (0.24 * TMAX)
     &      + (0.38 * TMIN)  + (0.248 * TMEAN) - (3.09 * waterFactor)
        ST(1) = TSOIL2CM

        DELT = 1

!keep Delt in case somebody wants to run this subroutine more than once a day

C*     Approximation after data of Krenz, rise of temperature with depth,
C*     lower boundary temperature (ST):
C*     theoretical equation for soil homogeneous in depth and time
C*     from simplified differential equation for
C*     lower boundary (with temperature gradient)

C*     (thetav and lbdav for ZDAMP from input file)


C     ***Initialize profile parameters for the day
        DO L=1,NLAYR
           SW_A2DAY(L)= (SW_Yest(L)+ SW(L)) / 2
        END DO

        IF (ALBD .GT. 9000) THEN
           ST(NLAYR+1) = TAV+0.008*(CUMDPT+5.)+TAMP*(1.-0.0172*
     &              sqrt(CUMDPT+5.))*sin(pi*(DoyNH-(105.+0.212*
     &              (CUMDPT+5.)))/182.5)
        ELSE
           ST(NLAYR+1) = TAV+0.008*(CUMDPT+5.)+TAMP*
     &              exp(-(CUMDPT+5.)/ZDAMP)*cos(0.01721*(DoyNH-195)-
     &              (CUMDPT+5.)/ZDAMP)
        ENDIF


        DO L=1,NLAYR
           LMBD(L)   = (CC1(L)+C2(L)*SW_A2DAY(L)-(CC1(L)-C4(L))*
     &                 exp(-(C3(L)*SW_A2DAY(L))**4))*864.0
           CP(L)     = CD(L)+4.186*SW_A2DAY(L)
           CV(L)     = CD(L)+4.186*SW_Yest(L)
	     SW_YEST(L)= SW(L)
        END DO

        LMBD(NLAYR+1)=LMBD(NLAYR)

!     ***Following line added to original code***
      !  dlayr (L+1) = 5

        DO L=1,NLAYR
           LBD(L)=LMBD(L)+DLAYR(L)/(DLAYR(L)+5.)*(LMBD(L+1)-LMBD(L))
        END DO
c

!       The convective term is excluded
**********************************

!**     Following 4 lines were modified, since this version uses predicted
!       soil T for layer1 (TSoil2cm)
        D(2) = LBD(1) /(DLAYR(2)*DS(1))+LBD(2)/(DLAYR(2)*DS(2))+
     &         (CP(2)/DELT)
        E(2) = -LBD(2)/(DLAYR(2)*DS(2))
        B(2) = CV(2)*ST_YEST(2)/DELT+(LBD(1) * TSoil2cm) /
     &	     (DLAYR(2)*DS(1))


        DO L=3,NLAYR
           A(L) = -lbd(L-1)/(DLAYR(L)*DS(L-1))
           D(L) = LBD(L-1)/(DLAYR(L)*DS(L-1))+LBD(L)/(DLAYR(L)*DS(L))+
     1            CP(L)/DELT
           E(L) = -LBD(L)/(DLAYR(L)*DS(L))
           B(L) = CV(L)*ST_YEST(L)/DELT
        END DO
c
        B(NLAYR)= CV(NLAYR)*ST_YEST(NLAYR)/DELT+LBD(NLAYR)/
     &            (DLAYR(NLAYR)*DS(NLAYR))*ST(NLAYR+1)

c
*     Solution of  tridiagonal matrix
c
        DO L=3,NLAYR
           F   = A(L)/D(L-1)
           D(L)= D(L)-F*E(L-1)
           B(L)= B(L)-F*B(L-1)
        END DO

        ST(NLAYR) = B(NLAYR)/D(NLAYR)

        DO L=NLAYR-1,2,-1
           ST(L)     = (B(L)-E(L)*ST(L+1))/D(L)
           ST_YEST(L)= ST(L)
	  ENDDO

!       CHP added 12/17/2004 -- Need to export surface temperature
        SRFTEMP = ST(1)

!***********************************************************************
!***********************************************************************
!     Output & SEASEND
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
      CALL OPSTEMP(CONTROL, ISWITCH, DOY, SRFTEMP, ST, TAV, TAMP)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE SALUS_T
!=======================================================================
