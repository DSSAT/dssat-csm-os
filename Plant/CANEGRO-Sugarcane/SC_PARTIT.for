C*************************************************************************
C  DRY MATTER PARTITIONING
       SUBROUTINE SC_PARTIT (WaterBal, Part, Climate, CaneCrop,
     -                    Growth, Out, Control, 
     -                    STGDOY, DSUCyest, CELLSE_DM, LEAFTT)

!     2023-01-26 chp removed unused variables from argument list: ISWITCH

c     Use the modules:
      USE CNG_MODULEDEFS
      USE MODULEDEFS

      IMPLICIT NONE
      EXTERNAL GET_CULTIVAR_COEFF, GET_SPECIES_COEFF
      SAVE

c     Declare modules in use:
c     :::::::::::::::::::::::
      TYPE(WATERTYPE)    WaterBal
      TYPE(PARTTYPE)     Part
      TYPE(CLIMATETYPE)  Climate
      TYPE(CANECROPTYPE) CaneCrop
      TYPE(OUTTYPE)      Out
      TYPE(GROTHTYPE)    Growth
!     TYPE(SwitchType) ISWITCH ! MJ: no longer used

c     Dssat control variable:
      TYPE(ControlType) Control


c     Parameter variables:
c     ::::::::::::::::::::
c      INTEGER DOY
c      REAL    IRR
      INTEGER STGDOY(20)

c     Common 'OUTBLOCK' variables in use:
c     :::::::::::::::::::::::::::::::::::
c     Thermal time for leaf development --> start of stalk elongation
      ! REAL    CHUPI, LEAFTT
      REAL    LEAFTT
      REAL    ROOTDM
      REAL    ROOTDM2   !Added temporarily by MvdL 
      REAL    TRASDM

c     Common PART variables in use:
c     :::::::::::::::::::::::::::::
      REAL    AERDWDT
      REAL    AERLDM
      REAL    APFMX
      REAL    CHUPIBASE
      REAL    DELTAOUT
      REAL    DELTTMAX
      REAL    FDWDT
      REAL    FTCON
      REAL    NSUCMAS
      REAL    PCB
      REAL    STKDM
      REAL    STKPFMAX
      REAL    SUCA
      REAL    SUCMAS
      REAL    SURCON
      REAL    SWDF2AMP
c      REAL    TBDELTT
      REAL    TBFT
      REAL    TDMAS
      REAL    TOPDM



c     FIBRE PARTITIONING
c     MJ, June 2015
c     Note: Additional state variables not in the PART block
c     Cellulosic DM (t/ha)
      REAL, INTENT(OUT) ::  CELLSE_DM
c     Genetic parameters related to fibre partitioning
c     Fraction of stalk fibre that is cellulose
      REAL FF_CELLSE


c     Common WATER variables in use:
c     ::::::::::::::::::::::::::::::
      REAL    SWDF2

c     Common CLIMT variables in use:
c     ::::::::::::::::::::::::::::::
      REAL    TEMPMN
      REAL    TEMPMX

c     Common CANCRP variables in use:
c     :::::::::::::::::::::::::::::::
      REAL    TMEANDEDLF
      REAL    TMEANLF

c     Local variables:
c     ::::::::::::::::
      REAL    DELTA
      REAL    DELTT
      REAL    DNSTKDT
      REAL    DNSUCDT
      REAL    DNSUCTEMP
      REAL    DSTKDT
      REAL    DSUCDT
      REAL    DSUCTEMP
      REAL    FTDELTA
      REAL    FWDELTA
      REAL    MEANTEMP
      REAL    NNSTK
      REAL    NSUCCAP
      REAL    ROOTDWDT
      REAL    ROOTPF
      REAL    STKPF
      REAL    SUCCAP
      REAL    SUCEQ
      REAL    SUCPF

c     Maximum root partitioning fraction
      REAL    MAX_ROOTPF

c     For reading coeffs:
      LOGICAL CF_ERR, SPC_ERR

c     Yesterday's value of delta sucrose mass:
c     t/ha/d
      REAL, INTENT(OUT) :: DSUCyest


c     *******************************************************************
c     Init:
      IF (CONTROL%DYNAMIC .EQ. RUNINIT) THEN
c       MJ, Mar 2010: zero all of the partitioning variables in the composite variable:
         Part%AERDWDT  = 0.0
         Part%AERLDM  = 0.0
         Part%AMPLIF  = 0.0
         Part%APFMX  = 0.0
         Part%BRXPDM  = 0.0
         Part%CHUPIBASE  = 0.0
         Part%DELTAOUT  = 0.0
         Part%DELTTMAX  = 0.0
         Part%DMMAS(150)  = 0.0
         Part%FDWDT  = 0.0
         Part%FLODGE  = 0.0
         Part%FTCON  = 0.0
         Part%NSUCMAS  = 0.0
         Part%PARCE  = 0.0
         Part%PCB  = 0.0
         Part%PURITY  = 0.0
         Part%RESPCF  = 0.0
         Part%RFA  = 0.0
         Part%RFB  = 0.0
         Part%RFC  = 0.0
         Part%ROOTF  = 0.0
         Part%STKDM  = 0.0
         Part%STKPFMAX  = 0.0
         Part%STKWM  = 0.0
         Part%SUCA  = 0.0
         Part%SUCB  = 0.0
         Part%SUCMAS  = 0.0
         Part%SUCNEWMAS  = 0.0
         Part%SURCON  = 0.0
         Part%SWDF2AMP  = 0.0
         Part%TBASEFNS  = 0.0
         Part%TBASETOT  = 0.0
         Part%TBDELTT  = 0.0
         Part%TBFT  = 0.0
         Part%TDMAS  = 0.0
         Part%TOPDM  = 0.0
         Part%TOPRATE  = 0.0
         Part%TOPTFNS  = 0.0
         Part%TOPTTOT  = 0.0
         Part%TOTBASE  = 0.0

c      Now zero local variables 
         ! CHUPI = 0.
         ROOTDM = 0.
         TRASDM = 0.    
         AERDWDT  = 0.0
         AERLDM  = 0.0
         APFMX  = 0.0
         CHUPIBASE  = 0.0
         DELTAOUT  = 0.0
         DELTTMAX  = 0.0
         FDWDT  = 0.0
         FTCON  = 0.0
         NSUCMAS  = 0.0
         PCB  = 0.0
         STKDM  = 0.0
         STKPFMAX  = 0.0
         SUCA  = 0.0
         SUCMAS  = 0.0
         SURCON  = 0.0
         SWDF2AMP  = 0.0
         TBFT  = 0.0
         TDMAS  = 0.0
         TOPDM  = 0.0
         SWDF2  = 0.0
         TEMPMN  = 0.0
         TEMPMX  = 0.0
         TMEANDEDLF  = 0.0
         TMEANLF  = 0.0
         DELTA  = 0.0
         DELTT  = 0.0
         DNSTKDT  = 0.0
         DNSUCDT  = 0.0
         DNSUCTEMP  = 0.0
         DSTKDT  = 0.0
         DSUCDT  = 0.0
         DSUCTEMP  = 0.0
         FTDELTA  = 0.0
         FWDELTA  = 0.0
         MEANTEMP  = 0.0
         NNSTK  = 0.0
         NSUCCAP  = 0.0
         ROOTDWDT  = 0.0
         ROOTPF  = 0.0
         STKPF  = 0.0
         SUCCAP  = 0.0
         SUCEQ  = 0.0
         SUCPF  = 0.0
         MAX_ROOTPF  = 0.0
     
      ENDIF


      IF (CONTROL%DYNAMIC .EQ. SEASINIT) THEN
c         These values should come from cultivar / species file

c        Species params:
c         Part%TBDELTT   = 28.
         Part%SWDF2AMP  = 0.5

c        Get these parameters from file:
c         CALL GET_SPECIES_COEFF(Part%TBDELTT,  'TBDELTT',  
c     &                          Control, SPC_ERR)
         
         CALL GET_CULTIVAR_COEFF(Part%SWDF2AMP, 'SWDF2AMP', 
     &                           Control, SPC_ERR)

c         Set default values:
          Part%APFMX     = 0.88
          Part%CHUPIBASE = 1050.
          Part%DELTTMAX  = 0.07
          Part%FTCON     = 0.32
          Part%PCB       = 0.6
          Part%STKPFMAX  = 0.65
          Part%SUCA      = 0.58
          Part%SURCON    = 0.99
          Part%TBFT      = 25.
          MAX_ROOTPF     = 0.95


c         Read from file:
c         :::::::::::::::
c             AFPMX:
              CALL GET_CULTIVAR_COEFF(Part%APFMX, 'APFMX', 
     -                             CONTROL,  CF_ERR)              
c             CHUPIBASE:
              CALL GET_CULTIVAR_COEFF(Part%CHUPIBASE, 'CHUPIBASE', 
     -                             CONTROL,  CF_ERR)              
c             DELTTMAX:
              CALL GET_CULTIVAR_COEFF(Part%DELTTMAX, 'DELTTMAX', 
     -                             CONTROL,  CF_ERR)              
c             FTCON:
c             :::::::::
c             MJ, Mar 2008: moved FTCON to species file:
c              CALL GET_CULTIVAR_COEFF(Part%FTCON, 'FTCON', 
c     -                             CONTROL,  CF_ERR)              
              CALL GET_SPECIES_COEFF(Part%FTCON, 'FTCON', 
     -                             CONTROL,  SPC_ERR)
c             :::::::::              
c             PCB:
c             :::::::::
c             MJ, Mar 2008: moved PCB to species file
c              CALL GET_CULTIVAR_COEFF(Part%PCB, 'PCB', 
c     -                             CONTROL,  CF_ERR)              

              CALL GET_SPECIES_COEFF(Part%PCB, 'PCB', 
     -                             CONTROL,  SPC_ERR) 
c             :::::::::
             
c             STKPFMAX:
              CALL GET_CULTIVAR_COEFF(Part%STKPFMAX, 'STKPFMAX', 
     -                             CONTROL,  CF_ERR)              
c             SUCA:
              CALL GET_CULTIVAR_COEFF(Part%SUCA, 'SUCA', 
     -                             CONTROL,  CF_ERR)              
c             SURCON:
c             :::::::::
c             MJ, Mar 2008: moved SURCON to species file
c              CALL GET_CULTIVAR_COEFF(Part%SURCON, 'SURCON', 
c     -                             CONTROL,  CF_ERR)              
              CALL GET_SPECIES_COEFF(Part%SURCON, 'SURCON', 
     -                             CONTROL,  SPC_ERR)              
c             :::::::::

c             TBFT:
              CALL GET_CULTIVAR_COEFF(Part%TBFT, 'TBFT', 
     -                             CONTROL,  CF_ERR)              

c             MAX_ROOTPF
              CALL GET_SPECIES_COEFF(MAX_ROOTPF, 'MAX_ROOTPF', 
     &                               Control, SPC_ERR)


c      Fibre partitioning
c      MJ, June 2015
       FF_CELLSE = 0.50
!       CALL GET_CULTIVAR_COEFF(FF_CELLSE, 'FF_CELLSE', CONTROL, CF_ERR)
       CELLSE_DM = 0.0
          
c         :::::::::::::::::::::::::::::::::::::::::::::::::::::::


c         These are variables that need to be zeroed for each run/treatment:
          Part%TDMAS     = 0.
          Part%TOPDM     = 0.
          Part%SUCMAS    = 0.
          Part%STKDM     = 0.
          Part%NSUCMAS   = 0.
          Part%FDWDT     = 0.
          Part%DELTAOUT  = 0.
          Part%AERDWDT   = 0.
          Part%AERLDM    = 0.
          Out%TRASDM     = 0.
          Part%TOPDM     = 0.
          Out%ROOTDM     = 0.
          ROOTDM2     = 0.   !MvdL - temp
          Part%ROOTF     = 0.5    

c         No sucrose yesterday...
          DSUCyest = 0.0

      ELSEIF (CONTROL%DYNAMIC .EQ. RATE) THEN

c     MJ: copy values from common blocks to local variables:
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     WATER:
c     ::::::
      SWDF2 = WaterBal%SWDF2

c     PART:
c     :::::
      AERDWDT   = Part%AERDWDT
      AERLDM    = Part%AERLDM
      APFMX     = Part%APFMX
      CHUPIBASE = Part%CHUPIBASE
      DELTAOUT  = Part%DELTAOUT
      DELTTMAX  = Part%DELTTMAX
      FDWDT     = Part%FDWDT
      FTCON     = Part%FTCON
      NSUCMAS   = Part%NSUCMAS
      PCB       = Part%PCB
      STKDM     = Part%STKDM
      STKPFMAX  = Part%STKPFMAX
      SUCA      = Part%SUCA
      SUCMAS    = Part%SUCMAS
      SURCON    = Part%SURCON
      SWDF2AMP  = Part%SWDF2AMP
c      TBDELTT   = Part%TBDELTT
      TBFT      = Part%TBFT
      TDMAS     = Part%TDMAS
      TOPDM     = Part%TOPDM

c     CLIMT:
c     ::::::
      TEMPMN    = Climate%TEMPMN
      TEMPMX    = Climate%TEMPMX

c     Canecrop:
c     :::::::::
      TMEANDEDLF = CaneCrop%TMEANDEDLF
      TMEANLF    = CaneCrop%TMEANLF

c     Outblock:
c     :::::::::
c     Note: rootdm and trasdm should be in different block...
      ! CHUPI  = Out%CHUPI + Out%HUPI
      ROOTDM = Out%ROOTDM
      TRASDM = Out%TRASDM


c     *******************************************************************



c         Local variables:
          DELTA     = 0.
          DELTT     = 0.
          DNSTKDT   = 0.
          DNSUCDT   = 0.
          DNSUCTEMP = 0.
          DSTKDT    = 0.
          DSUCDT    = 0.
          DSUCTEMP  = 0.
          FTDELTA   = 0.
          FWDELTA   = 0.
          MEANTEMP  = 0.
          NNSTK     = 0.
          NSUCCAP   = 0.
          ROOTDWDT  = 0.
          ROOTPF    = 0.
          STKPF     = 0.
          SUCCAP    = 0.
          SUCEQ     = 0.
          SUCPF     = 0.

 !CNB JUL 2000 {NEWPART} AERDWDT IS THE AERL DAILY ALLOCATION


c     Calculate partitioning of biomass increment to roots:
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c         MJ: sample from 376 variety file:
c     ----------------------------------------------------------------
c     MAX FRAC DM TO AERLDM              APFMX       |0.880   |t/t   7
c     PARTITION COEFF.                   PCB         |0.600   |      8
c     ----------------------------------------------------------------
          ROOTDM=TDMAS-AERLDM

c         MJ, 2007-04-03: replaced 0.95 with 'MAX_ROOTPF' in species file
c	    ROOTPF=AMIN1(0.95, 1.0 - APFMX * (1.0-EXP(-PCB*TDMAS)))
	    ROOTPF=AMIN1(MAX_ROOTPF, 1.0 - APFMX * (1.0-EXP(-PCB*TDMAS)))
	    ROOTDWDT=FDWDT * ROOTPF
          Growth%GRORT   = ROOTDWDT * 100.


c     MJ: 2006/09/12:
c     changed this:
c	ROOTDM=ROOTDM * ROOTDWDT
c     to this:
          ROOTDWDT = AMAX1(ROOTDWDT, 0.00000)  !MvdL
          ROOTDM=ROOTDM + ROOTDWDT
          ROOTDM = AMAX1(ROOTDM, 0.00000)      !MvdL
          
c     Added by MvdL:
          ROOTDM2 = ROOTDM2+ROOTDWDT               
  
c         Calculate AERDWDT, ensuring that it is at least 0. 
          AERDWDT=AMAX1(FDWDT-ROOTDWDT,0.00000)    !MvdL  0.000001 - 0
           
c         Update Aerial dry mass
          AERLDM=AERLDM+AERDWDT

c         Set stalk partitioning factor to maximum from cultivar file
c          STKPF=STKPFMAX 

C     sdb added 16/10/2003
c     MJ, Feb 2010: replaced hard-coded value with STKPFMAX, as was meant to be:
c     MJ, Jun 2017: partitioning fraction exceeds 1 between 9 and 10 deg C.
c       I don't see anywhere that stkpf is restricted to 0.0-1.0!
	    stkpf = STKPFMAX + exp(-0.5 * (((tempmx+tempmn)/2.0)-7.5))


c         If the stalk has not yet emerged, 0 biomass is allocated to the
c         stalk:
c         ::::::
c         CNB/AS-Apr2002-partitioning to stalk now triggered by CHUPI
	    IF (LEAFTT .LT. Chupibase) THEN 
              STKPF=0.000 
          ELSE
c             Stalk emergence phenological phase
              IF (CaneCrop%GROPHASE .LT. 3) THEN
                  CaneCrop%GROPHASE = 3
                  STGDOY(4) = Control%YRDOY
              ENDIF
          ENDIF

c         Calculate daily increment in stalk mass
	    DSTKDT=AERDWDT*STKPF 

c         Update stalk drymass
	    STKDM=STKDM+DSTKDT

c         MJ: changed 1 to 1. in following eq.
!DAILY INCREMENT IN NON-STALK (TOPS AND TRASH)
	    DNSTKDT=AERDWDT*(1.-STKPF) 
	 
c     NONSTALK IS PROPORTIONALLY DIVIDED BETWEEN TRASH AND TOPS
c     CALCULATE TOPDM AND TRASHDM IN THE FOLLOWING BLOCK
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c         Update non-stalk aerial dry mass
	    NNSTK = AMAX1(DNSTKDT + TRASDM + TOPDM,0.0)

c         Calculate trash amount
!          TRASDM = 0.52 * NNSTK *  (TMEANDEDLF / (TMEANLF+0.00001))
!          MJ, June 2015: changed 0.52 to 0.70 based on 08RE14 experimental data
	    TRASDM = 0.70 * NNSTK *  (TMEANDEDLF / (TMEANLF+0.00001))
	    IF (TRASDM.GT.NNSTK) TRASDM=NNSTK
	    IF (NNSTK.LT.5.0) TRASDM=0.0

c         Set mass of tops to non-stalk matter minus trash
c         i.e. TOPDM is above ground non-stalk living biomass...
c         which is green leaves and meristems
	    TOPDM = NNSTK - TRASDM
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

c     Calculate sucrose partitioning
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	    MEANTEMP=(TEMPMN+TEMPMX)/2.0

c         Following line no longer active
c  	    DELTT = DELTTMAX*(1-EXP(0.2*(MEANTEMP-TBDELTT)))

c     LOGISTIC TEMP FUNCTION
	    FTDELTA=1.0/(1.0+EXP(ftcon*(MEANTEMP-TBFT)))

c     FWDELTA=(1-SWDF9) Another option that works quite well (EP1=3 instead of EP1=2) 
      	FWDELTA=(1-SWDF2)**SWDF2AMP
      
      	DELTA=DELTTMAX*(FWDELTA+FTDELTA-FWDELTA*FTDELTA)
	    IF (DELTA.LT.0.01) DELTA=0.01


c     NOTE: STKDM *MUST* be updated to today's integrated value before running this code.
	    IF (STKDM.LT.(SUCA/DELTA)) THEN
c             SUCPF IS THE PARTITIONING FRACTION TO SUCROSE IN THE STALK
c             ...and is only used as an output, if at all.
              SUCPF=DELTA*STKDM
              SUCEQ=(DELTA/2)*STKDM**2
	    ELSE
              SUCPF=SUCA
              SUCEQ=SUCA*(STKDM-0.5*SUCA/DELTA)
	    ENDIF

          SUCCAP = (SUCEQ-SUCMAS)
          NSUCCAP = (STKDM-SUCEQ)-NSUCMAS 
          DSUCTEMP = SURCON*SUCCAP
          DNSUCTEMP=SURCON*NSUCCAP

          IF (DNSUCTEMP .GT. DSTKDT) DNSUCTEMP = DSTKDT
          IF (DNSUCTEMP .LT. 0.) DNSUCTEMP = 0.00

          DNSUCDT = DNSUCTEMP
          DSUCDT  = DSTKDT - DNSUCDT 
          DSUCyest = DSUCDT
          SUCMAS  = SUCMAS + DSUCDT
          NSUCMAS = NSUCMAS + DNSUCDT

c     MJ, June 2015:
c     Partitioning of fibre into cellulosic products
c     Currently a static partitioning.
c     Non-sucrose mass: NSUCMAS
c     Leaf mass: TOPDM
c     Trash mass: TRASDM
      CELLSE_DM = (NSUCMAS + TOPDM + TRASDM) * FF_CELLSE
      

c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


! CNB 9/2002 TO OUTPUT DELTA WE NAME IT DELTAOUT BECAUSE THE ARE OTHER DELTAs
	deltaout=delta

 122   FORMAT(F5.0,10F8.3)


c     *******************************************************************


c     MJ: copy values from local variables to common blocks:
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     WATER:
c     ::::::
       WaterBal%SWDF2 = SWDF2

c     PART:
c     :::::
      Part%AERDWDT   = AERDWDT
      Part%AERLDM    = AERLDM
      Part%APFMX     = APFMX
      Part%CHUPIBASE = CHUPIBASE
      Part%DELTAOUT  = DELTAOUT
      Part%DELTTMAX  = DELTTMAX
      Part%FDWDT     = FDWDT
      Part%FTCON     = FTCON
      Part%NSUCMAS   = NSUCMAS
      Part%PCB       = PCB
      Part%STKDM     = STKDM
      Part%STKPFMAX  = STKPFMAX
      Part%SUCA      = SUCA
      Part%SUCMAS    = SUCMAS
      Part%SURCON    = SURCON
      Part%SWDF2AMP  = SWDF2AMP
c      Part%TBDELTT   = TBDELTT
      Part%TBFT      = TBFT
      Part%TDMAS     = TDMAS
      Part%TOPDM     = TOPDM


c     CLIMT:
c     ::::::
      Climate%TEMPMN = TEMPMN
      Climate%TEMPMX = TEMPMX

c     Canecrop:
c     :::::::::
      CaneCrop%TMEANDEDLF = TMEANDEDLF
      CaneCrop%TMEANLF    = TMEANLF 

c     Outblock:
c     :::::::::
c     Note: rootdm and trasdm should be in different block...
c      Out%CHUPI  = CHUPI
      Out%ROOTDM = ROOTDM
      Out%TRASDM = TRASDM


c     End of Dyanamic = RATE
      ENDIF
c     *******************************************************************


      RETURN
      END  
