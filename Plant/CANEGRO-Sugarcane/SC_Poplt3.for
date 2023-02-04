c     Calculate sugarcane stalk population
c     ::::::::::::::::::::::::::::::::::::
c     NOTE: stdaye and stddiv are UNDEFINED if water stress
c     is excluded in the FileX
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


C************************************************************************
      SUBROUTINE POPLT3 (tt,t0,pplast,newbat,rowspc,stdaye,stddiv, 
     &                   WATERBAL, CANECROP, SOIL, ISWITCH, CONTROL,
     &                   STGDOY, DTT_EM, FI)

c     Instruct compiler to use CANEGRO module defns: 
c     ::::::::::::::::::::::::::::::::::::::::::::::
      USE CNG_ModuleDefs
      USE ModuleDefs

      IMPLICIT NONE
      EXTERNAL GET_CULTIVAR_COEFF, SC_GTP_SHOOTPOP
      SAVE

c     The DSSAT simulation control object
c     ::::::::::::::::::::::::::::::::::: 
      Type (SwitchType) ISWITCH

c     The water properties 'object'
c     :::::::::::::::::::::::::::::
      TYPE (WaterType)   WaterBal

c     The CANCRP properties object:
c     :::::::::::::::::::::::::::::
      TYPE (CaneCropType) CaneCrop

c     The SOILI object:
c     :::::::::::::::::
      TYPE (CNG_SoilType) Soil

c     The DSSAT Control variable:
c     :::::::::::::::::::::::::::
      TYPE (ControlType) CONTROL

      dimension swdfar(10)

c     Declare previously undeclared parameter variables:
c     ::::::::::::::::::::::::::::::::::::::::::::::
      INTEGER NEWBAT
      REAL    PPLAST
      REAL    ROWSPC
      REAL    STDAYE
      REAL    STDDIV

c     Yesterday's heat units (Cumulative)
      REAL    T0
c     TOday's heat units (Cumulative), for shoot pop
      REAL    TT
c     Today's change in thermal time for emergence
      REAL   DTT_EM
      INTENT(IN) :: DTT_EM
c     Fractional interception of radiation (PAR)
      REAL, INTENT(IN) :: FI


c     Declare previously undeclared local variables:
c     ::::::::::::::::::::::::::::::::::::::::::::::
      INTEGER I
      INTEGER INC
      INTEGER KSTRES
      INTEGER N1
      REAL    CDEP
      REAL    DELTA
c     Delta calculated by GTP shoots module
      REAL GTP_DELTA
      REAL    EXTRA
      REAL    FX
      REAL    STRPOP
      REAL    SWDF30
      REAL    SWDFAR
      REAL    T1
      REAL    TEMPTOT
      REAL    TOTMAX
      REAL    XS


c     Declare water common block variables:
c     :::::::::::::::::::::::::::::::::::::
c     :::::::::::::::
c     In use here:
      REAL    SWDF2


c     Soil Variables:
c     :::::::::::::::
c     Used here:

      REAL    DLAYR(NL)
      REAL    DUL(NL)
      REAL    LL(NL)
      INTEGER NLAYR
      REAL    SW(NL)

c     Canopy (?) variables:
c     :::::::::::::::::::::
c     In use here:
      INTEGER LOSING
      INTEGER NTLGRP
      REAL    POPCF(10)
      REAL    POPN
c     Changed from 30 to MAX_TILLER_COHORTS (number of tiller cohorts)
      REAL    TEMPOP(MAX_TILLER_COHORTS)
      REAL    TOTPOP
c     Thermal time at which population increase ceases
      REAL    TT_POPGROWTH
c     Max tiller population
      REAL    MAX_POP
c     Shoot population for increasing popn phase as
c     calculated by the GTP shoots module
c     Today and yesterday ('Y_')
      REAL POPHA_GTP, Y_POPHA_GTP

c     Population decay coefficient: this number
c     affects how rapidly the stalk population decreases
c     after peak population
      REAL POPDECAY, TOTMAX_YEST

c     Phenological stages (events with dates)
      INTEGER STGDOY(20)
      
c     Counter:
      INTEGER REPEATS
      
c     Cultivar coeff temporary vars:
      LOGICAL CF_ERR

c     ====================================================================
c     ========  CODE  =======
c     ====================================================================

c     Run Initialisation (MJ, March 2010)
c     :::::::::::::::::::::::::::::::::::
      IF (Control%DYNAMIC .EQ. RUNINIT) THEN
         I  =  0
         INC  =  0
         KSTRES  =  0
         LOSING  =  0
         N1  =  0
         NEWBAT  =  0
         NLAYR  =  0
         NTLGRP  =  0
         REPEATS  =  0
         STGDOY  =  0
  
         CDEP  =  0.0
         DELTA  =  0.0
         ! DLAYR(NL)  =  0.0
         ! DUL(NL)  =  0.0
         EXTRA  =  0.0
         FX  =  0.0
         LL  =  0.0
         MAX_POP  =  0.0
         POPCF  =  0.0
         POPN  =  0.0
         PPLAST  =  0.0
         ROWSPC  =  0.0
         STDAYE  =  0.0
         STDDIV  =  0.0
         STRPOP  =  0.0
         ! SW(NL)  =  0.0
         SWDF2  =  0.0
         SWDF30  =  0.0
         SWDFAR  =  0.0
         T0  =  0.0
         T1  =  0.0
         TEMPOP  =  0.0
         TEMPTOT  =  0.0
         TOTMAX  =  0.0
         TOTPOP  =  0.0
         POPHA_GTP = 0.0
         TT  =  0.0
         TT_POPGROWTH  =  0.0
         XS  =  0.0
         POPDECAY  =  0.0
         TOTMAX_YEST  =  0.0
      ENDIF

c     Copy values from composite type to local variables:
c     :::::::::::::::::::::::::::::::::::::::::::::::::::
c     Water balance:
c     :::::::::::::;
        SWDF2 = WaterBal%SWDF2
c     Canopy/Crop
c     :::::::::::
        LOSING = CaneCrop%LOSING
        NTLGRP = CaneCrop%NTLGRP
        POPN   = CaneCrop%POPN
        TOTPOP = CaneCrop%TOTPOP
        DO I = 1,10
            POPCF(I)  = CaneCrop%POPCF(I)
            SWDFAR(I) = 0.
        ENDDO
        DO I = 1,MAX_TILLER_COHORTS
            TEMPOP(I) = CaneCrop%TEMPOP(I)
        ENDDO

c     Soil
c     ::::
        NLAYR = Soil%NLAYR
      DO I=1,NL
      
        DUL(I)   = Soil%DUL(I)
        LL(I)    = Soil%LL(I)
        DLAYR(I) = Soil%DLAYR(I)
        SW(I)    = Soil%SW(I)
      ENDDO

      REPEATS = 0

c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


c     ===============================================================
c     SEASONAL INITIALISATION
c     ===============================================================
      IF (Control%DYNAMIC .EQ. SEASINIT) THEN


c         Initial values for these population-related variables:
c         ::::::::::::::::::::::::::::::::::::::::::::::::::::::
          pplast = 0.
          newbat = 0
          stdaye = 0.
          stddiv = 0.
          t0 = 0.
          SWDF30 = 1.
          WaterBal%SWDF30 = 1.
          DO I=1, 10
            SWDFAR(I) = 0.
          ENDDO


c         READ FROM CULTIVAR FILE:
c         ------------------------
c         Note: defaults are set for NCo376
c         :::::::::::::::::::::::::::::::::
c         Mature stalk population:
c         ::::::::::::::::::::::::
c             Set default:
              CaneCrop%POPN       = 13.3

c             Read from file:
              CALL GET_CULTIVAR_COEFF(CaneCrop%POPN, 'POPTT16', 
     -                               CONTROL,  CF_ERR)

c             Cultivar coeff is in stalks/m2 but model wants s/ha
              CaneCrop%POPN = CaneCrop%POPN * 10000.
c         ::::::::::::::::::::::::

c         Population coefficients:
c         ::::::::::::::::::::::::
c         These describe the response of population to thermal time.
c         They change from cultivar to cultivar.
c         ::::::::::::::::::::::::
c         Set defaults:
c         :::::::::::::
              CaneCrop%POPCF    = 0.
              CaneCrop%POPCF(1) =   1.826
              CaneCrop%POPCF(2) =  -0.00201
c              CaneCrop%POPCF(3) = 866.70001
c              CaneCrop%POPCF(4) =  -0.99024
c              CaneCrop%POPCF(5) =   0.0003282 
              
              TT_POPGROWTH = 600.   
c             Maximum stalk population (x1000 stalks)
c             NCo376 is 600

c             Now in stalks/m2
              MAX_POP = 60.


c         Read from cultivar file:
c         ::::::::::::::::::::::::
!              CALL GET_CULTIVAR_COEFF(CaneCrop%POPCF(1), 'POPCF(1)',
!     -                                CONTROL, CF_ERR)
!              CALL GET_CULTIVAR_COEFF(CaneCrop%POPCF(2), 'POPCF(2)',
!     -                                CONTROL, CF_ERR)

              CALL GET_CULTIVAR_COEFF(TT_POPGROWTH, 'TT_POPGROWTH',
     -                                CONTROL, CF_ERR)
! Removed by MJ, Feb 2018: new tillering model no longer needs this.  60 /m2
! maximum is retained as a hard-coded param.
!              CALL GET_CULTIVAR_COEFF(MAX_POP, 'MAX_POP',
!     -                                CONTROL, CF_ERR)

c             Cultivar coeff is in stalks/m2 - x 10 to get to 1000 stalks/ha
c             as model wants
              MAX_POP = MAX_POP * 10.

c         New population curve param:
              POPDECAY = 0.004
              CALL GET_CULTIVAR_COEFF(POPDECAY, 'POPDECAY',
     -                                CONTROL, CF_ERR)

c
c         ::::::::::::::::::::::::
c         END OF READING CULTIVAR COEFFS
c         ------------------------------

c         INITIALISE START VALUES FOR STATE VARIABLES:
c         ============================================
          CaneCrop%DEDSTK     = 0.
          CaneCrop%LOSING     = 0
          CaneCrop%LT         = 0.
          CaneCrop%NTLGRP     = 1
          CaneCrop%TMEANDEDLF = 0.
          CaneCrop%TMEANLF    = 0.
          CaneCrop%TOTPOP     = 0.
          POPHA_GTP = 0.0

c         Copy array values
c         :::::::::::::::::

          DO I=1,MAX_TILLER_COHORTS
              CaneCrop%DEDLFN(I) = 0.
              CaneCrop%LFN(I)    = 0
              CaneCrop%POPMAX(I) = 0.
              CaneCrop%TEMPOP(I) = 0.
          ENDDO

          CaneCrop%SHGT = 0.

c        Initialise the GTP shoot population module
         Y_POPHA_GTP = 0.0
         POPHA_GTP = 0.0

         CALL SC_GTP_SHOOTPOP(CONTROL,  
     &     TT-T0, DTT_EM, FI, SWDF30, POPHA_GTP)

c     ===============================================================
c     RATE CALCULATIONS
c     ===============================================================
      ELSEIF (Control%DYNAMIC .EQ. RATE) THEN

c TT = thermal time for tillers base 16 dec C. TT was reduced to TT*0.1 for 
c regression in POPHU.wk3 that gave the popcf values. TT>1500 prevented
        T1=min(TT,1500.0)
!      CALL GETLUN('SCCAN',SCLUN)
c        WRITE(SCLUN, '(A, F10.3)') 'TT ', TT

c This is an initial attempt to account for the effect of water stress on 
c tillering and tiller senescence done by an average todate stress coefficient  
c (GIB 14/9/95)  Change in DSSAT !
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        swdf30 = 1.0
        cdep=0.0
        i=0

c     MJ, October 2006: use the switch object to determine whether or not
c     to simulate water stresses (i.e. ISWWAT determines whether or not
c     a water balance is simulated)
c     ::::::::::::::::::::::::::::::
      IF (ISWITCH%ISWWAT .EQ. 'Y') THEN
c      IF (.FALSE.) THEN
  15    i=i+1
         if(i.gt.nlayr) go to 16
         cdep=cdep+dlayr(i)
         if(cdep.le.30.0) then

c           19/6/2003 - AS and SB changed
c           strpop was negative and sw was less than ll
            fx=max(0.0,min(1. , 3.*(sw(i)-ll(i))/(dul(i)-ll(i)) ))
            swdf30=swdf30+fx*dlayr(i) 
c            WRITE(*,'(3H   ,F10.5)') FX*DLAYR(I)
            go to 15
         endif
         cdep=cdep-dlayr(i)
         swdf30=swdf30/cdep 
         SWDF30 = MIN(1.,SWDF30)

         !# MJ
         ! SWDF30 = 1.

         WaterBal%SWDF30 = SWDF30

c  16     stdaye=stdaye+(1.0-swdf2)
c        stddiv=stddiv+1.0
c        strpop=stdaye/stddiv

   16    strpop=(1.0-swdf30)
         stddiv=1.0
         stdaye=strpop
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     MJ: SWDF30 appears to be a stress factor, weighted by layer 
c     thickness, which applies to the top x layers such that
c     depth(x-1) < 30. cm and depth(x) >= 30.
c     Equivalent to the top 30ish cm of soil that fits nicely in
c     soil layers.
          
      
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


C When tillers are on decline phase use a dampened swdf2 factor to hasten
c senescence and only allow half the full effect ie loss can only be 10 % more 
c than normal

         if(LOSING.eq.1) then
           kstres=min(kstres+1,10)
           do i=2,kstres
             swdfar(i-1)=swdfar(i)
           enddo
           swdfar(kstres)=swdf2

           strpop=0.0
           do i=1,kstres
             strpop=strpop+swdfar(i) 
           enddo
           strpop=(1.0-strpop/kstres)*0.1 
         endif

c          WRITE(*,'(A, 2F10.5)') 'SWDF30, STRPOP are ', SWDF30, STRPOP
c          WRITE(*,'(A, 5(F10.5, 1X))') 'DLAYR: ', DLAYR(1:5)

c     :::::::::::::::::::::::::::::::::::::::::::
      ELSE
c         If water stress is excluded, strpop must be 0:
c         ******** ********** ********* !!!
          strpop = 0.
          stdaye = 0
          stddiv = 1.

c     End of water balance-dependant calculations
      ENDIF



c     CALCULATE STALK POPULATION
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C     This differs from poplt2 in that the potential daily increment 
c     in population is derived from 1st derivative of pop'n curve and is 
c     then modified by  a stress index
c     If pop'n is going up stress reduces increase, if it is going 
c     down stress hastens decrease
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     ---------------------------------------------------------------
c       For increasing population phase
c       TT_POPGROWTH is 600 for NCo376

        ! strpop = 0.0;
        
c       ::::::::::::::::::::::::::::::::
        if(T1 .LT. TT_POPGROWTH) then
c            Call the GTP shoot population module
             CALL SC_GTP_SHOOTPOP(CONTROL,  
     &         TT-T0, DTT_EM, FI, SWDF30, POPHA_GTP)
c            Calculate the GTP model shoots delta:
             GTP_DELTA = (POPHA_GTP - Y_POPHA_GTP) / 1000.0
!             WRITE(*, '(A, 2F10.2)') 'DELTA, POPHA_GTP = ', 
!     &         GTP_DELTA, POPHA_GTP

c            Calculate total population
             !totmax = 0. + popcf(1)*T1 + popcf(2)*T1**2. 
             totmax =  POPHA_GTP / 1000.0

c            Calculate change in stalk population (1st derivative)
             delta  =  (popcf(1) + popcf(2)*T1*2.) * (tt-t0) 

c            Modify this according to water stress affecting stalk pop.
             !delta  = max(delta * (1. - strpop),0.0)
             delta  = max(GTP_DELTA * (1. - strpop),0.0)
        else
c       For stable / decreasing population phase:
c       :::::::::::::::::::::::::::::::::::::::::
c            Growth stage:
c            ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c            STGDOY(11) is peak population
             IF (STGDOY(11) .EQ. 0) THEN
                  STGDOY(11) = Control%YRDOY
             ENDIF

             IF (CaneCrop%GROPHASE .LT. 4) THEN
c                 Peak population??
c                  STGDOY(11) = Control%YRDOY
                  CaneCrop%GROPHASE = 4
             ENDIF
c            ::::::::::::::::::::::::::::::::::::::::::::::::::::::::

c            Following line changed according to Abraham's instructions
c             totmax = popcf(3) + popcf(4)*T1 + popcf(5)*T1**2. 
             totmax = TOTMAX_YEST - (TOTMAX_YEST - (POPN/1000.))  
     &                * POPDECAY*1000. * (TT-T0)/1000.

             delta  = 0.

c            Calculate a change in stalk population only if
c            population is below 300 000. stalks/ha
             IF (totmax .lt. MAX_POP) THEN
                  delta = TOTMAX - TOTMAX_YEST
c                 Changed from this
c                 delta =  (popcf(4)  + popcf(5)*T1*2)*(tt-t0) 
             ENDIF

c            Population is not affected by water stress unless
c            it is below 300 000 stalks/ha
c             (implied by delta == 0 unless totmax < MAX_POP)
             delta=delta*(1.0+strpop)
        endif

c       Update 'yesterday' variables with today's values for tomorrow
        TOTMAX_YEST = TOTMAX
        t0=tt

c        WRITE(*,*) strpop

c       Update total population as existing population + 
c       change in population
        ! MJ, Jun 2015: ROWSPC already accounted-for by GTP model in
        ! tiller increase phase
        IF (T1 .LT. TT_POPGROWTH) THEN
          totpop=totpop+delta*1000.0
          !totpop=totpop+delta*1000.0*1.4/rowspc
        else
          totpop=totpop+delta*1000.0*1.4/rowspc
        ENDIF

        ! Was:
        ! totpop=totpop+delta*1000.0*1.4/rowspc
        

c        WRITE(*, '(A, F10.0)') 'Totpop is ', TOTPOP
c        WRITE(*, '(A, F10.4)') 'Delta is ', delta

        TOTPOP=max(TOTPOP,0.0)

c     ---------------------------------------------------------------

c After peak population totpop cannot be less than say 120 000 for 376
c        popn=120000.0*1.4/rowspc
        if(LOSING.eq.1) totpop=max(totpop,popn)
c Tillers emerging after 300000 are small enough to ignore
c         MJ, Mar 2008: the following line:
c        if(totpop.gt.300000.0) totpop=300000.0
c         was replaced with:
         if(totpop.gt.MAX_POP*1000.) totpop=MAX_POP*1000.
c NTLGRP is 1 to start with. 
        newbat=0
        NTLGRP=NTLGRP+inc 
        newbat=inc
        inc=0
        if(totpop-pplast.ge.11000.0) then
          xs=totpop-(pplast+11000.0)                                   
c          if(xs.ge.11000.0) PAUSE 'Totpop increased more than 20 000' 
          totpop=totpop-xs
          pplast=totpop
          inc=1
c          stdaye=0.0
c          stddiv=0.0
        endif


c The polynomial works up to 1500 HU, so reduce totpop linearly to final
c 'popn' between 1500 and 2000 hu. 
       IF(TT.GT.1600.0) THEN
           totpop=min(totpop-(totpop-popn)*(tt-1600.0)/(2000.-1600.),
     &                totpop)
       ENDIF
c       IF(TT.GT.1500.0) THEN
c           totpop=min(totpop-(totpop-popn)*(tt-1500.0)/(2000.-1500.),
c     &                totpop)
c       ENDIF


       IF(TT.GT.2000.0) then
             TOTPOP=min(POPN,totpop)
             DO N1=2,MAX_TILLER_COHORTS 
                TEMPOP(N1)=0.0
             ENDDO
             TEMPOP(1)=totpop
             NTLGRP=1
             goto 10 
        ENDIF

c     ---------------------------------------------------------------
C A new tiller group (up to 30) starts with each 11000 increment. 
c       MJ May 2007: replaced this:
c       IF (LOSING.EQ.1) GO TO 3
c       with:
c       MJ: If the stalk population is NOT decreasing, then:
c       ::::::::::::::::::::::::::::::::::::::::::::::::::::
        IF (LOSING .NE. 1) THEN
c         MJ: Set temporary stalk population total to 0.
          TEMPTOT=0.0

c         MJ: If at least one new 'secondary' tiller cohort has developed:
          IF (NTLGRP .GT. 1) THEN
c             MJ: Add up tiller populations in each cohort
              DO N1=1,NTLGRP-1
                  TEMPTOT = TEMPTOT + TEMPOP(N1)
              ENDDO
          ENDIF

c         MJ: If the total of tiller cohorts is less than the total 
c         tiller population calculated with polynomial, 'top up'
c         the most recent tiller cohort with the difference, then
c         'exit'
          IF (TEMPTOT.LE.TOTPOP) THEN

c             MJ: 'Top up' cohort population
              TEMPOP(NTLGRP)=TOTPOP-TEMPTOT

c             MJ: 'Exit'
              go to 10
          ENDIF

c         MJ, May 2007:
c         :::::::::::::
c         Now, if this section has not yet exited, it means that
c         TEMPTOT > TOTPOP; this can only happen if a stalk
c         population DECREASE is indicated by one of the preceding
c         calculations.  In such a case, LOSING is set to 1
c         :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

c         At this point tillers are about to senesce so we keep a record of maximum
c         tiller numbers in each cohort to get the trash component conserved
          LOSING=1
          kstres=0
c         Removed:
c          REPEATS = 0
        ENDIF
c     ---------------------------------------------------------------

c       ...and put here:
        REPEATS = 0

c     MJ, Mar 2010: Execute code from here until 10 if LOSING is true
c     i.e. population is falling.
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    3   CONTINUE 


c     MJ, Mar 2010: TEMPTOT represents "yesterday's" population: the
c     sum of population in each of the cohort groups (TEMPOP)
c     Add up population:
c     ::::::::::::::::::
      TEMPTOT=0.0
      DO N1=1, NTLGRP
        TEMPTOT = TEMPTOT + TEMPOP(N1)
      ENDDO
c     :::::::::::::::::::    

c     EXTRA is the difference between polynomial-calculated stalk
c     population and the population of the cohorts.
      EXTRA= max(TEMPTOT-TOTPOP,0.0) 

c     If population fell by less than the number of tillers stored in the
c     'youngest' tiller group, simply reduce this number by EXTRA and
c     exit
!        MJ, Mar 2010: correct the nasty loop error...
!      IF(EXTRA.LT.TEMPOP(NTLGRP)) THEN 
      IF(EXTRA.LE.TEMPOP(NTLGRP)) THEN 
        TEMPOP(NTLGRP)=TEMPOP(NTLGRP)-EXTRA
        go to 10
      ENDIF

c     On the other hand, if the decrease in population exceeds the youngest
c     tiller group/cohort size, then the decrease needs to span more than one
c     tiler group.
!        MJ, Mar 2010: correct the nasty loop error...
!        IF(EXTRA.GE.TEMPOP(NTLGRP)) THEN 
        IF(EXTRA.GT.TEMPOP(NTLGRP)) THEN 
c          Inserted by MJ, 2006/09/27
           REPEATS = REPEATS + 1
c          Reduce EXTRA by the number of tillers in the youngest cohort:
           EXTRA = EXTRA-TEMPOP(NTLGRP)
c          Set the size of the youngest cohort to zero           
           TEMPOP(NTLGRP)=0.0
c          Then reduce the number of cohorts by 1           
           NTLGRP=AMAX0(NTLGRP-1,1)

c          Inserted by MJ, 2006/09/27
c          Break out of an infinite loop...
c          This does not address the problem - fix!
c          MJ, May 2007: I think this has now been fixed; REPEATS was
c          not always initialised to 0 before the loop.
c          This is left here to alert me if the problem happens again.
           IF (REPEATS .GE. MAX_TILLER_COHORTS) THEN
              WRITE(*, '(2A)') 'In a nasty loop!! - please ',
     &          'contact Matthew Jones, matthew.jones@sugar.org.za '
              WRITE(*, '(A, I10)') 'EXTRA is ', EXTRA
!              WRITE(*, '(A, F10.5)') 'Delta is ', DELTA
              WRITE(*, '(A)') 'Please check rowspacing is specified '//
     &                       ' in CENTIMETRES.'
c              PAUSE
              GOTO 10
           ENDIF
c           MJ: there is a problem with this; causing an infinite loop
            
c            WRITE(*,'(A)') 'Going to 3'

           GO TO 3
        ENDIF



  10  CONTINUE







c     Copy values to composite type from local variables:
c     :::::::::::::::::::::::::::::::::::::::::::::::::::
c     Water balance:
c     :::::::::::::;
      WaterBal%SWDF2 = SWDF2
c     Canopy/Crop
c     :::::::::::
        CaneCrop%LOSING = LOSING
        CaneCrop%NTLGRP = NTLGRP
c        CaneCrop%POPN   = POPN
        CaneCrop%TOTPOP = TOTPOP
c        DO I = 1,10
c            CaneCrop%POPCF(I)  = POPCF(I)
c        ENDDO
        DO I = 1,MAX_TILLER_COHORTS
            CaneCrop%TEMPOP(I) = TEMPOP(I)
        ENDDO
c     Soil
c     ::::
        Soil%NLAYR = NLAYR
      DO I=1,NL
      
        Soil%DUL(I) = DUL(I)
        Soil%LL(I) = LL(I)
        Soil%DLAYR(I) = DLAYR(I)
        Soil%SW(I) = SW(I)
      ENDDO

c     ===============================================================
c     END OF DYNAMIC = RATE
c     ===============================================================

c     ===============================================================
c     INTEGRATION CALCULATIONS
c     ===============================================================
      ELSEIF(Control%DYNAMIC.EQ.INTEGR) THEN     
         ! WRITE(*, '(A)') 'Calling shootpopn integration'
         Y_POPHA_GTP = POPHA_GTP
           if(T1 .LT. TT_POPGROWTH) then
c            Call the GTP shoot population module
             CALL SC_GTP_SHOOTPOP(CONTROL,  
     &         TT-T0, DTT_EM, FI, SWDF30, POPHA_GTP)
           ENDIF

c     END OF CONTROL%DYNAMIC = ? conditional statement
c     ================================================
      ENDIF
c     :::::::::::::::::::::::::::::::::::::::::::::::::::


        RETURN
        END   

c     Variables
c     Some of the variables are listed and described:
c     ===============================================================
c     POPN
c     This is a REAL value, and is set to 133000 for NCo376.  It 
c     corresponds to POPTT16, and is defined as the stalk population 
c     after 1600 heat units.  When cumulative heat units exceed 1500, 
c     this is used to limit stalk population.
c
c     LOSING
c     This is an INTEGER value, although it is used in a BOOLEAN sense.  
c     If LOSING = 1, the simulation is in the population decline phase.  
c     Note that population does not decline because LOSING is set to 1; 
c     rather, LOSING is set to 1 when the population starts to fall.
c
c     PPLAST
c     REAL; this is the stalk population the previous day.  It is set at 
c     line 390 of the poplt3 subroutine in the DSSAT-CANEGRO version.
c
c     TOTPOP
c     REAL; total tiller population.
c
c     TEMPOP
c     This is an array of REAL values, with MAX_TILLER_COHORTS elements.  This represents 
c     the population of each tiller cohort.  If total tiller population 
c     increases by more than 11000 tillers in a day, a new cohort is 
c     created.
c  
c     INC
c     This is an INTEGER.  If the increase in tiller population is 
c     greater than 11000., INC is set to 1.  
c
c     NTLGRP
c     INTEGER; this represents the current number of tiller cohorts, 
c     and is the index of the most recent tiller cohort in the TEMPOP  
c     array. NTLGRP is incremented by INC.
c     ===============================================================
