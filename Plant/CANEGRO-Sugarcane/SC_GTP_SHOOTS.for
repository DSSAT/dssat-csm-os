c     Better shoot popn model from GTP Canegro
c     ----------------------------------------
c     MJ, Jun 2015.  Work done for SASRI project 08RE14.
c     The code here is reduced to the absolute minimum
c     required to calculate a total shoot population.
c     This is then handed to the POPLT3 function to assign
c     to cohort groups etc.
c     ---------------------------------------
c     Note: only the tiller population INCREASE is sim-
c     ulated here.  The senescence is handled in POPLI3.
c     This is to be called AFTER emergence.  This assumes
c     that the first plant is about to emerge.
c     ALso note, however, that the population of primary 
c     shoots is determined by thermal time to emergence
c     (base 16 deg C) while secondary shoots might have a
c     different base temperature.  Hence, DTT_EM and DTT_POP
c     are both input parameters
c     ---------------------------------------
c     BASED ON:
c     GTP Canegro
c     May 2012
c     (c) Matthew Jones SA Sugarcane Research Institute
c     Algorithms conceptualised by Geoff Inman-Bamber and Abraham Singels
c     Bloodline: 
c       Canegro-->Unified Model-->QTA/GTP Canegro-->GTP release Canegro
c       -->modified for 08RE14 ASA model
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


 


c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SC_GTP_SHOOTPOP(CONTROL,  
     &  DTT_POP, DTT_EM, FI_INTRA, SWDF30,   ! Canegro inputs
     &  POPHA) ! outputs

!     2023-01-26 chp removed unused variables from argument list: 
!         ISWITCH

c     ***************************************************************      
c     Instruct compiler to use module definitions:
c     DSSAT
      USE ModuleDefs
c     Canegro
      USE CNG_ModuleDefs      

      IMPLICIT NONE
      EXTERNAL FIND_INP, GET_CULTIVAR_COEFF, SHOOTPOPHA
      EXTERNAL GET_PLTPOP
      SAVE

    

      
c     The DSSAT Composite variables
c     :::::::::::::::::::::::::::::
c     Control variable:
      TYPE (ControlType), INTENT(IN) :: CONTROL        
c     Simulation options (switches)
!     Type (SwitchType), INTENT(IN) :: ISWITCH
      
c     GTP MODEL composite variables:
c     Array of primary stalks:
      TYPE (SHOOT_COHORT) :: Cohorts
      DIMENSION Cohorts(MAXCOHORTS)
      
c     INTENT statements (good practice!)
      INTENT(IN) :: FI_INTRA
      
c     Planting depth (mm and cm) 
!     REAL PLTDEP_CM, PLTDEP
c     Number of viable buds, per linear metre of cane row
      REAL VIABLE_BUDS

c     Real and integer version of number of ratoon crops
c     (=0 for plant crop)
      REAL R_RATOON, RATOON_N

c     IO error variable
      LOGICAL CF_ERR  !, SPC_ERR
c     Reference final shoot (tiller, stalk) population parameter 
c     tillers/ha and tillers/m2
      REAL POPN_m2
c     Row-spacing
      REAL ROWSPC       
      
c     Thermal time for germination and emergence, and population dynamics
      REAL DTT_POP, TT_POP, DTT_EM, TT_EM
      INTENT(IN) :: DTT_POP, DTT_EM
      
c     Soil water stress variables:
c     Water stress index for tillering (0-1):
      REAL, INTENT(IN) :: SWDF30

c     Primary shoots:
c     The number and daily change of potential primary shoots:
      REAL D_POT_PRIMARIES, NEW_POT_PRIMARIES, POT_PRIMARIES
c     The daily change of actual primary shoots (water stessed):
      REAL D_ACT_PRIMARIES, ACT_PRIMARIES
c     Verification counts of primary and tiller shoots:
      REAL VPN, VTN, TOTPOPV

c     Number of leaves that must emerge before stick appears
!     INTEGER STKDELAYLFNO
!     REAL R_STKDELAYLF      

c     Change in the number of primary and secondary (tiller)
c     shoot cohorts [cohorts/d]
      INTEGER D_P_COHORTS, D_T_COHORTS

c     The number of 'actual' tillers.  This will enable
c     verification and also maintain population size even
c     when the Cohorts array runs out of space.
c     This is a 'shadow' count of tillers.
      REAL ACT_TILLERS, D_ACT_TILLERS

c     The total number of shoot cohorts (P + T) per 1 m of cane row
      INTEGER NUM_COHORTS     
      
c     Tillering:
c     Offset (tillering delay), deg C days
      REAL TILLER_DELAY   
c     Intra-row fractional interception (total - not just green leaves):
      REAL FI_INTRA
c
c     Number of new primary shoots [shoots/m/d]
      REAL N_NEWPRIMS

c     Change in number of tillers per linear m per day [shoots/m]
!      INTEGER D_N_TILLERS
       REAL D_N_TILLERS
      
c     Counter
      INTEGER I   !, K

c     Ti is 'tiller index', for the NEW tiller cohort (to be 
c     integrated later today).  The index of the Cohorts
c     array in which today's tiller cohort should be stored.
      INTEGER Ti
      
!c     Unique shoot ID function call
!      INTEGER GET_UNIQUE_ID
      
c     Living shoot populations (shoots / ha), for verification (output, to be removed)
      REAL, INTENT(OUT) :: POPHA
c     The same, but per linear m:
      REAL POP_m, POP_LMD

c     Final shoot population (shoots/m2)
      REAL FINAL_POP

c     Tillering delay factor (°Cd)  [Genetic trait parameter]
      REAL  TDELAY
c     Tiller appearance rate (shoots/°Cd/primary shoot)  [Genetic trait parameter]
      REAL  TAR0

c     Primary shoots not yet allocated to a shoot cohort (only if too
c     few primary shoots appear today to warrant creating a new cohort) 
c     [shoots/m]
      REAL D_PRIM_RESID
c     The minimum number of shoots per linear m required to create a 
c     new shoot cohort (smaller means more computer memory and 
c     processing time; larger means less precise calculation of canopy
c     phenology) [shoots/m]
      REAL COHORT_SIZE
c     The same value in shoots/ha.  This is the 'user' setting.  With 
c     rowspacing, is converted to shoots/m for COHORT_SIZE
      REAL HA_COHORT_SIZE

c     Effective change in age of primary shoot for the purposes of calculating 
c     number of tillers.  The delay from primary shoot emergence to 
c     start of tillering is taken into account. [°Cd]
      REAL DTT_PPEFF

c     Daily change in the number of tillers for a primary shoot cohort
c     (local variable) [shoots/m]
      REAL D_TILLERS

c     Residual pool of tillers not yet allocated to a cohort (shoots/m)
      REAL TILLER_RESID

c     Function that returns plant population (buds m¯²)
      REAL GET_PLTPOP
  
      
c     Set the minimum number of shoots per ha required to 
c     create a new shoot cohort.
!      DATA COHORT_SIZE /0.5/
      DATA HA_COHORT_SIZE /6000./


c     ===============================================================
c     SEASONAL INITIALISATION
c     ===============================================================
c     Code to be called at the beginning of each treatment
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF (Control%DYNAMIC .EQ. SEASINIT) THEN
     
        
c       Init phenology totals        
        TT_POP = 0.
        TT_EM = 0.0

        D_POT_PRIMARIES = 0.
        NEW_POT_PRIMARIES = 0. 
        POT_PRIMARIES = 0.
        D_ACT_PRIMARIES = 0.
        D_POT_PRIMARIES = 0.
        ACT_PRIMARIES = 0.
        NUM_COHORTS = 0
        D_PRIM_RESID = 0.
        ACT_TILLERS = 0.
        FINAL_POP = 0.
        D_T_COHORTS = 0
        D_P_COHORTS = 0
        POPHA = 0.
        POP_m = 0.
        VPN = 0.
        VTN = 0.
        TILLER_RESID = 0.
        TOTPOPV = 0.

c       Initialise population-related variables in all shoots:
c       ::::::::::::::::::::::::::::::::::::::::::::::::::::::
        DO I=1, MAXCOHORTS
          Cohorts(I)%ISPRIMARY = .FALSE.
c         Number of shoots:
          Cohorts(I)%NUMSHOOTS = 0.
c         Date of emergence:
          Cohorts(I)%EMERGE_TTDATE = 0.
c         Age in degree days:
          Cohorts(I)%AGE_TT = 0. 
          Cohorts(I)%DTT_POP = 0.
c         Unique ID
          Cohorts(I)%UNIQUE_ID = -1
c         Tillering:
          Cohorts(I)%D_N_TILLERS = 0
        ENDDO

c       Read row-spacing from INP file (m):
        rowspc = 1.4
        CALL FIND_INP(ROWSPC, 'ROWSPC', Control)    

c       Read GENOTYPE PARAMETERS
c       Note: defaults are set for NCo376
c       :::::::::::::::::::::::::::::::::

c       Tiller appearance rate per primary shoot (shoots/°Cd)  [Genetic trait parameter]
        TAR0 = 0.0236   ! Units = shoots/°Cd
        CALL GET_CULTIVAR_COEFF(TAR0,'TAR0',CONTROL,CF_ERR)
        
c       Tillering delay factor (°Cd)  [Genetic trait parameter]
        TDELAY = 50.0   ! Units = °Cd
        CALL GET_CULTIVAR_COEFF(TDELAY,'TDELAY',CONTROL,CF_ERR)
        TILLER_DELAY = TDELAY  ! deg days

c       Mature stalk population:
        POPN_m2       = 13.3
!       MJ, Feb 2018: CUL param name changed from GTP's POPN_m2 to regular Canegro's
!       'POPTT16'.        
        CALL GET_CULTIVAR_COEFF(POPN_m2, 'POPTT16', CONTROL,  CF_ERR)

c       Germination and Emergence
c       ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c       Geoff Inman-Bamber (CSIRO), Michiel Smit (SASRI), 
c       Abraham Singels (SASRI) and Matthew Jones (SASRI).
c       July 2010.
c       Experiments: A/Emerge.
c       ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c       Calculate the number of viable buds.  We will assume the 
c       following:
c       1. The user enters a plausible value for plant population 
c          for plant crops.
c       2. For ratoon crops the plant pop is taken as final population
c          * 2 buds per stool
c       3. Seed is considered 'good', therefore viability is 90% of 
c          buds in the ground.

c        Calculate viable buds (plant population)
         VIABLE_BUDS = GET_PLTPOP(Control) * ROWSPC 

!         WRITE(*, '(A, F10.4)') 'TAR0 = ', TAR0

c        Find out if this is a ratoon crop
         CALL FIND_INP(R_RATOON, 'RATOON', Control)
         RATOON_N = R_RATOON * 1

c        Work out the cohort size in shoots per linear m:
         COHORT_SIZE = HA_COHORT_SIZE / 10000.0 * ROWSPC

      ENDIF


c     ===============================================================
c     RATE CALCULATIONS
c     ===============================================================
      IF (Control%DYNAMIC .EQ. RATE) THEN
c       Init major rate variables to 0.:
        D_ACT_PRIMARIES = 0.
        D_POT_PRIMARIES = 0.
        D_P_COHORTS = 0
        D_T_COHORTS = 0
        D_N_TILLERS = 0.
        D_ACT_TILLERS = 0.

        DO I=1, MAXCOHORTS
c         Set the change in primary shoots to 0:
          Cohorts(I)%D_NUMSHOOTS = 0.
c         Set the change in tillers to 0:
          Cohorts(I)%D_N_TILLERS = 0
        ENDDO

c       Tell all shoots about thermal age:
        DO I=1, NUM_COHORTS
          Cohorts(I)%DTT_POP = DTT_POP
        ENDDO

c       New primary and secondary shoots - calculated iff the
c       crop is not yet senescing:
c       :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c       EMERGENCE (PRIMARY SHOOTS)
c       :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c       How many shoots should have emerged today?
        NEW_POT_PRIMARIES = MAX(0.,  VIABLE_BUDS *    
     &    (1. - EXP(-0.0916*
     &    (MIN(600., MAX(0., TT_EM)))/12.96)))

c       So the 'potential' number of new Shoots today is:       
        D_POT_PRIMARIES = NEW_POT_PRIMARIES - POT_PRIMARIES
c       Actual number today is limited by water stress:         
        D_ACT_PRIMARIES = D_POT_PRIMARIES !  * SWDF30

c       :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

c       So, now, how many primary shoots emerging today is 
c       sufficient to warrant creating a new cohort?  In Canegro it
c       was 11000 shoots ha¯¹ = 1.1 shoots m¯².  On this basis, let 
c       us make it 1 shoot per linear m:
c       So, if the number of new primary shoots per linear m, plus
c       the remainder from the previous day, exceeds 1, then create a 
c       new shoot cohort:
        IF ( ((D_ACT_PRIMARIES + D_PRIM_RESID) .GE. COHORT_SIZE)
     &     .OR. ((TT_EM .GT. 600.0) 
     &          .AND. (D_PRIM_RESID .GT. 0.00001)) )
     &  THEN
          D_P_COHORTS = 1
c         Set the number of new primary shoots to the new
c         ones appearing today, plus any leftovers from the
c         previous day:
          N_NEWPRIMS = D_ACT_PRIMARIES + D_PRIM_RESID
c         Set the number of residual (unallocated to a cohort)
c         primary shoots to 0.  Remember, fractions of shoots 
c         are allowed in the cohorts.
          D_PRIM_RESID = 0.
        ELSE
c         Otherwise, increase the residual primary shoots that
c         have not yet been allocated to a cohort:
          D_PRIM_RESID = D_PRIM_RESID + D_ACT_PRIMARIES
        ENDIF

c       Set the properties of the new cohort (if any)
c       The 'New' cohort is the next cohort in the array.  It is 
c       treated as a temporary storage area.  When integrated, 
c       the NUM_COHORTS is incremented to include this cohort
c       that is 'in limbo' during the RATES calculation:
        IF (D_P_COHORTS .GT. 0) THEN
          IF (NUM_COHORTS+1 .LE. MAXCOHORTS) THEN
c           This is a primary shoot
            Cohorts(NUM_COHORTS+1)%ISPRIMARY = .TRUE.
c           It represents this many primary shoots:
            Cohorts(NUM_COHORTS+1)%D_NUMSHOOTS = N_NEWPRIMS
c           Set the date in emergence thermal time that this cohort appeared
            Cohorts(NUM_COHORTS+1)%EMERGE_TTDATE = TT_EM
c           Age in TT
            Cohorts(NUM_COHORTS+1)%AGE_TT = 0.
c           Set the unique ID of this cohort:
            ! Cohorts(NUM_COHORTS+1)%UNIQUE_ID = GET_UNIQUE_ID()
            Cohorts(NUM_COHORTS+1)%UNIQUE_ID = -99
          ELSE
c           If the maximum number of cohorts has been exceeded,
c           add extra primary shoots to the last cohort:
            Cohorts(NUM_COHORTS)%D_NUMSHOOTS = N_NEWPRIMS
          ENDIF
        ENDIF


c       :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c       TILLERING (SECONDARY SHOOTS)
c       :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c       Work out how many tillers (per linear m) appear each day.  
c       This is probably limited to one per primary shoot cohort.
c       :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        D_ACT_TILLERS = 0.

        DO I=1, NUM_COHORTS
          IF (Cohorts(I)%ISPRIMARY) THEN
c           Effective change in thermal time for tillering.  This
c           is 0 if tillering has not started, less than dtt if
c           just on the cusp of tillering, and dtt if after
c           start of tillering.
            DTT_PPEFF = MIN(
     &        MAX(0., Cohorts(I)%AGE_TT + DTT_POP - TILLER_DELAY),
     &        DTT_POP)

c           Number of tillers appearing today:
c           This is a probably a fraction of a tiller per day:
c           ::::::::::::::::::::::::::::::::::::::::::::::::::
c           Product of: change in TT, maximum tillering rate per
c           unit TT (TAR0), shading (FI_INTRA) and water stress (SWDF30).
c           NOTE: Bezuidenhout et al. (FCR 2003) suggested that the 
c           impact of FI_INTRA decreases with subsequent ratoons 
c           because of stool widening.  Consider including such an 
c           adjustment factor?  OR: alter calculation of intra-row
c           interception based on stool width (ratoon number).
c           Multiplied by NUMSHOOTS to weight the tiller cohort
c           according to number of primary shoots in this
c           cohort:

!           MJ, June 2015: reverted to linear function.
!           Tillering stops when Fi (not intra-FI, that is a legacy
!           issue) hits 0.75.
            D_TILLERS = DTT_PPEFF * TAR0 * 
     &        MAX(0., 1.-(FI_INTRA/0.75)) * SWDF30 *
     &        Cohorts(I)%NUMSHOOTS
!            D_TILLERS = DTT_PPEFF * TAR0 * 
!     &        MAX(0., 1.-MIN(1., (1.25*(FI_INTRA**2.0)))) * SWDF30 *
!     &        Cohorts(I)%NUMSHOOTS

c           Add to the 'shadow' tiller count:
            D_ACT_TILLERS = D_ACT_TILLERS + D_TILLERS

c           The number of tillers per primary shoot is stored
c           per primary cohort, but the actual tiller cohort
c           phenology is handled by separate tiller cohorts.
c           ::::::::::::::::::::::::::::::::::::::::::::::::
c           Storing whole tillers and retaining the fractional
c           remainder resulted in significant (20%) differences
c           between the number of tillers formally stored and
c           the number there should have been.  So hence, 
c           a continuous function of tillers is considered,
c           like the primary shoots.  
            Cohorts(I)%D_N_TILLERS = D_TILLERS

          ENDIF
        ENDDO

c       Get the total (whole) number of new tillers appearing
c       today:
c       ::::::::::::::::::::::::
        D_N_TILLERS = 0.
        DO I=1, NUM_COHORTS
          IF (Cohorts(I)%ISPRIMARY) THEN
c           Accumulate total number of tillers
            D_N_TILLERS = D_N_TILLERS  + Cohorts(I)%D_N_TILLERS
          ENDIF
        ENDDO

c       Create a tiller cohort iff at least one cohort size
c       of tillers appeared today:
c       ::::::::::::::::::::::::
        IF (D_N_TILLERS + TILLER_RESID .GT. COHORT_SIZE) THEN
c         Indicate that one tiller cohort needs to be added today.
c         ALL tillers appearing today will have the same
c         phenology, so can be simulated in a single cohort.
          D_T_COHORTS = 1
        ELSE
          D_T_COHORTS = 0
c         Add this partial cohort to the residual pool of
c         tillers not yet allocated to a cohort:
          TILLER_RESID = TILLER_RESID + D_N_TILLERS
!c         And set the change in tillers to 0
!          D_N_TILLERS = 0.
        ENDIF

c       If a new tiller cohort appeared today, it needs to be
c       defined / stored in the tiller cohort array.
c       A primary cohort might already have been added, so the
c       array offset must take that into account:
c       :::::::::::::::::::::::::::::::::::::::::
c       If a new tiller cohort is to be added, initialise it:
        IF (D_T_COHORTS .GT. 0) THEN

c         Ti is 'tiller index', for the NEW tiller cohort (to be 
c         integrated later today)
          Ti = NUM_COHORTS + D_T_COHORTS + D_P_COHORTS

          IF (Ti .LE. MAXCOHORTS) THEN
c         Init tiller:
c         ::::::::::::
c         This is a tiller cohort
          Cohorts(Ti)%ISPRIMARY = .FALSE.
c         It represents this many tillers:
          Cohorts(Ti)%D_NUMSHOOTS = D_N_TILLERS + TILLER_RESID 
c         Set the residual tillers to 0 (they have all been 
c         allocated to a cohort)
          TILLER_RESID = 0.
c         Set the date in population thermal time that this cohort appeared
          Cohorts(Ti)%EMERGE_TTDATE = TT_POP
c         Age in TT
          Cohorts(Ti)%AGE_TT = 0.
c         Set the unique ID of this cohort:
          Cohorts(Ti)%UNIQUE_ID = -99 ! GET_UNIQUE_ID()   
          ENDIF
        ENDIF
     
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


      ENDIF
c     ===============================================================
c     END OF RATE CALCULATIONS
c     ===============================================================


c     ===============================================================
c     INTEGRATION CALCULATIONS
c     ===============================================================
      IF(Control%DYNAMIC.EQ.INTEGR) THEN     

c       Integrate thermal time:
        TT_POP = TT_POP + DTT_POP
        TT_EM = TT_EM + DTT_EM     

c       Add new primary stalks
c       Increase the number of cohorts (D_P_COHORTS is either 1, or 0): 
        NUM_COHORTS = MIN(MAXCOHORTS, 
     &    NUM_COHORTS + D_P_COHORTS + D_T_COHORTS)

c       Integrate the number of potential primary shoots:
        POT_PRIMARIES = POT_PRIMARIES + D_POT_PRIMARIES

c       And the actual number of primary shoots:
        ACT_PRIMARIES = ACT_PRIMARIES + D_ACT_PRIMARIES

c       Integrate the actual number of tillers:
        ACT_TILLERS = ACT_TILLERS + D_ACT_TILLERS

c       Numeric value of shoot population (independent of cohorts):
        TOTPOPV = ACT_PRIMARIES + ACT_TILLERS

c       Integrate the age, number of shoots, etc of each shoot
c       :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        DO I=1, NUM_COHORTS
c         :::::::::::::::::::::::::::::::::::::::::::::
c          Number of shoots in each cohort:
           Cohorts(I)%NUMSHOOTS = 
     &       Cohorts(I)%NUMSHOOTS + Cohorts(I)%D_NUMSHOOTS

c          Age in degree days:
           Cohorts(I)%AGE_TT = Cohorts(I)%AGE_TT 
     &       + Cohorts(I)%DTT_POP

        ENDDO

c         Limit final population to the actual number of primary shoots
c         that appeared, in case there was severe water stress
c         (prevents the senescence function from wanting to
c         increase shoot population)
c         :::::::::::::::::::::::::::
c         AS new algorithm for final population, based on A/Space/19 (Wagon-wheel ratoon crop) data
c         March 2011
!          FINAL_POP = (5.291 * ((PEAKPOP/ROWSPC) ** -0.716)) * 
!     &      (PEAKPOP/ROWSPC)

c       Population per hectare:
        CALL SHOOTPOPHA(Cohorts, NUM_COHORTS, ROWSPC, POPHA, VPN,VTN)
        POP_LMD = POPHA / 10000. * ROWSPC

c       Population per linear m, as stored in Cohorts:  (actual pop
c       might be higher if Cohorts array ran out of space):
        POP_m = VPN + VTN
        
!       WRITE(*, '(A,F5.3,A,F10.3)') 'Fi = ', FI_INTRA, 
!     &  ' POPHA = ', POPHA


      ENDIF
c     ===============================================================
c     END OF INTEGRATION CALCULATIONS
c     ===============================================================


c     End of subroutine      
      END     
c     ***************************************************************      




c     ===============================================================      
c     A function that looks-up plant population and estimates
c     consistently if that input is not available
c     MJ, May 2012
      REAL FUNCTION GET_PLTPOP(Control)
c     ===============================================================      
c     Unified Model
      USE ModuleDefs    
c     Canegro
      USE CNG_ModuleDefs 
              
      IMPLICIT NONE
      EXTERNAL FIND_INP, GET_CULTIVAR_COEFF

c     Final reference stalk population [plants/m2]  
c     (genotype input)
      REAL POPN_m2
c     Input plant population [plants/m2]
      REAL PLTPOP
c     Input row-spacing (m)
      REAL ROWSPC
c     Ratoon number (0 for plant crop)
      REAL RATOONNO
c     Genotype file read error
      LOGICAL CF_ERR


c     Control variable required for FIND_INP calls:
      TYPE(ControlType) Control

c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

c     Try reading from FileX:
      CALL FIND_INP(PLTPOP, 'PLTPOP', Control)
!      WRITE(*, '(A, F10.3)') 'PLTPOP is ', PLTPOP
      IF (PLTPOP .GT. 0.0001) THEN
        GET_PLTPOP = PLTPOP
        RETURN
      ENDIF

c     Otherwise, estimate from final reference stalk population,
c     and row-spacing:

c     Mature stalk population:
c     Set default:
      POPN_m2       = 13.3
c     Read from genotype file:
      CALL GET_CULTIVAR_COEFF(POPN_m2, 'POPTT16', CONTROL,  CF_ERR)

c     Get row-spacing
c     Read row-spacing from INP file (m):
      ROWSPC = 1.4
      CALL FIND_INP(ROWSPC, 'ROWSPC', Control)    
      
c     Get ratoon number
      RATOONNO = 1.
      CALL FIND_INP(RATOONNO, 'RATOON', Control)    

c     Choose between plant and ratoon crops:
      IF (RATOONNO .EQ. 0.) THEN
c       Plant crop:
c       Assume 8 buds per linear metre of row  
        GET_PLTPOP = ROWSPC * 8.0
      ELSE
c       Ratoon crop:
c       Assume two buds (1.6 viable, see Bezuidenhout on tillering (2003ish)
c       per stalk of final population of previous crop.  The reference value is for
c       a row-spacing of 1.4 m, so adjust as necessary:
        GET_PLTPOP = POPN_m2 / (1.4/ROWSPC) * 1.6
      ENDIF

      RETURN

      END
c     ===============================================================  
   

c     =============================================================== 
c     This subroutine returns counts living shoots
      SUBROUTINE SHOOTPOPHA(Cohorts, NUM_COHORTS, ROWSPC, POPHA, 
     &  VPCOUNT, VTCOUNT)
c     ===============================================================      
c     Unified Model
c     Canegro
      USE CNG_ModuleDefs     
      IMPLICIT NONE
      
      TYPE(SHOOT_COHORT), INTENT(IN) :: Cohorts(MAXCOHORTS)   
      INTEGER I
c     Row-spacing (cm):
      REAL, INTENT(IN) :: ROWSPC
c     Number of shoots in the array      
      INTEGER, INTENT(IN) :: NUM_COHORTS
c     Count of primary shoots and tillers
      REAL, INTENT(OUT) :: VPCOUNT, VTCOUNT
c     Output shoot population numbers:
      REAL, INTENT(OUT) :: POPHA
      
      
c     Count the number of live primaries, tillers:
      VPCOUNT = 0.
      VTCOUNT = 0.
      DO I=1, NUM_COHORTS
        IF (Cohorts(I)%ISPRIMARY) THEN
          VPCOUNT = VPCOUNT + Cohorts(I)%NUMSHOOTS
        ELSE
          VTCOUNT = VTCOUNT + Cohorts(I)%NUMSHOOTS
        ENDIF
      ENDDO    
c     Calculate living primary shoot population per ha
      POPHA = ((VPCOUNT+VTCOUNT) / ROWSPC) * 10000.
      
      END
c     ===============================================================   



