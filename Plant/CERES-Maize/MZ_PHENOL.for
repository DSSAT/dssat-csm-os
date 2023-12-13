!======================================================================
!  MZ_PHENOL, Subroutine
!
!  Determines Phenological Stage and Growing Degree Days for Maize
!----------------------------------------------------------------------
!  Revision history
!
!                 Written
!  02/07/1993 PWW Header revision and minor changes                 
!  02/07/1993 PWW Added switch block, code cleanup                  
!  02/07/1993 PWW Modified TT calculations to reduce line #'s       
!  05/  /1994 WTB Modified for MILLET model                        
!  03/29/2001 WDB Converted to modular format                      
!  12/01/2001 WDB Major restructuring for 2002 release                    
!  06/11/2002 GH  Modified for Y2K
!  08/12/2003 CHP Added I/O error checking
!  10/12/2005 CHP/JIL Added optional temperature sensitivity parameter
!                 to ecotype file (TSEN)
!  07/13/2006 CHP Added P model
!  04/14/2021 CHP Added CropStatus
!----------------------------------------------------------------------
      SUBROUTINE MZ_PHENOL(DYNAMIC,ISWWAT,FILEIO,IDETO,           !C
     &    CUMDEP,DAYL,DLAYR,LEAFNO,LL,NLAYR,PLTPOP,SDEPTH,        !I
     &    SNOW, SRAD,SUMP,SW,TMAX,TMIN, TWILEN,                   !I
     &    XN,YRDOY,YRSIM,                                         !I
     &    CUMDTT,DTT,EARS,GPP,ISDATE, ISTAGE,MDATE,STGDOY,SUMDTT, !O
     &    XNTI,TLNO,XSTAGE,YREMRG,RUE,KCAN,KEP, P3, TSEN, CDAY,   !O
     &    SeedFrac, VegFrac, CropStatus)                          !O

      USE ModuleDefs
      IMPLICIT  NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE, DAYLEN, WARNING
      SAVE
!----------------------------------------------------------------------
!                             Define Variables
!----------------------------------------------------------------------
      INTEGER         DYNAMIC         

!      REAL            ABSTRES         
      REAL            ACOEF           
      REAL            BARFAC 
      CHARACTER*1     BLANK         
      REAL            C1   
      INTEGER         CDAY 
      INTEGER         CropStatus
      REAL            CUMDEP          
      REAL            CUMDTT          
      REAL            DAYL            
      REAL            DEC             
      REAL            DGET
      REAL            DJTI
      REAL            DLAYR(NL)       
      REAL            DLV             
      REAL            DOPT                      
      REAL            DSGT
      REAL            DSGFT
      REAL            DTT             
      REAL            DUMMY           
      REAL            EARS            
      CHARACTER*6     ECONO           
      INTEGER         ERR             
      CHARACTER*6     ERRKEY          
      PARAMETER       (ERRKEY='MZPHEN')
      INTEGER         ERRNUM
      CHARACTER*12    FILEC     
      CHARACTER*12    FILES
      CHARACTER*12    FILEE     
      CHARACTER*92    FILEGC
      CHARACTER*30    FILEIO         
      INTEGER         FOUND          
      REAL            G2             
      REAL            G3             
      REAL            GDDE
      REAL            GPP            
      INTEGER         I              
      CHARACTER*1     IDETO          
      INTEGER         IDURP     
      INTEGER         ISTAGE         
      CHARACTER*1     ISWWAT         
      REAL            KCAN
      REAL            KEP
      INTEGER         LEAFNO         
      INTEGER         L              
      INTEGER         L0             
      INTEGER         LINC           
      REAL            LL(NL)         
      INTEGER         LNUM           
      INTEGER         LUNIO          
      INTEGER         MDATE          
      INTEGER         NDAS           
      INTEGER         NLAYR          
      INTEGER         NOUTDO         
      REAL            P1             
      REAL            P2             
      REAL            P2O            
      REAL            P3             
      REAL            P5             
      REAL            P9             
      CHARACTER*80    PATHCR 
      CHARACTER*80    PATHSR
      CHARACTER*80    PATHER        
      REAL            PDTT
      REAL            PHINT          
      REAL            PLTPOP         
      REAL            PSKER          
      REAL            RATEIN         
      REAL            ROPT           
      REAL            RUE
      REAL            SDEPTH         
      CHARACTER*6     SECTION        
      REAL            S1    
!     REAL            SI1(6)         
!     REAL            SI3(6)         
      REAL            SIND           
      REAL            SNDN           
      REAL            SNOW           
      REAL            SNUP           
      REAL            SRAD           
      INTEGER         STGDOY(20)     
      REAL            SUMDTT
      REAL            SUMDTT_2 !introduced for plant P routine         
      REAL            SUMP           
      REAL            SW(NL)         
      REAL            SWCG
      REAL            SWSD           
      REAL            TBASE          
      REAL            TDSOIL         
      REAL            TEMPCN         
                                     
      REAL            TEMPCR         
      REAL            TEMPCX         
      REAL            TH             
      REAL            TLNO           
      REAL            TMAX           
      REAL            TMIN           
      REAL            TMSOIL         
      REAL            TNSOIL         
      REAL            TOPT  
      REAL            TSEN  !10/12/2005 chp         
      REAL            TWILEN         
      CHARACTER*6     VARNO          
      CHARACTER*16    VRNAME                  
      REAL            XN             
      REAL            XNTI           
      REAL            XS             
      REAL            XSTAGE         
      INTEGER         YRDOY          
      INTEGER         YREMRG         
      INTEGER         YRSIM
      INTEGER ISDATE          

      INTEGER PATHL
      PARAMETER (BLANK = ' ')
      INTEGER LUNECO

      CHARACTER*6 ECOTYP
      INTEGER ISECT
      CHARACTER*255 C255
      CHARACTER*16  ECONAM
      INTEGER LUNCRP
      CHARACTER*92 FILECC
      CHARACTER*80 C80
      CHARACTER*78 MESSAGE(10)

!     CHP added for P model
      REAL SeedFrac, VegFrac
        
!----------------------------------------------------------------------
!         DYNAMIC = RUNINIT OR DYNAMIC = SEASINIT
! ---------------------------------------------------------------------
      IF (DYNAMIC.EQ.RUNINIT .OR. DYNAMIC.EQ.SEASINIT) THEN

!       Do this just once in RUNINIT
        IF (DYNAMIC .EQ. RUNINIT) THEN
          CALL GETLUN('OUTO', NOUTDO)

          !-------------------------------------------------------
          !     Read input file name (ie. DSSAT45.INP) and path
          !-------------------------------------------------------
          CALL GETLUN('FILEIO', LUNIO)
          OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)  
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)

          READ(LUNIO,50,IOSTAT=ERR) FILES, PATHSR; LNUM = 7
   50     FORMAT(//////,15X,A12,1X,A80)
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

          READ(LUNIO,51,IOSTAT=ERR) FILEE, PATHER; LNUM = LNUM + 1
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
   51     FORMAT(15X,A12,1X,A80)

          READ(LUNIO,51,IOSTAT=ERR) FILEC, PATHCR; LNUM = LNUM + 1
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

          !------------------------------------------------------
          !   Read Planting Details Section
          !------------------------------------------------------
          SECTION = '*PLANT'
          CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
          IF (FOUND .EQ. 0) THEN
            CALL ERROR(SECTION, 42, FILEIO, LNUM)
          ELSE
!            READ(LUNIO,60,IOSTAT=ERR) PLTPOP,SDEPTH
            READ(LUNIO,60,IOSTAT=ERR) YREMRG,PLTPOP,SDEPTH
            LNUM = LNUM + 1
! 60         FORMAT(25X,F5.2,25X,F5.2)
 60         FORMAT(11X,I7,7X,F5.2,25X,F5.2)
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
          ENDIF
!     -----------------------------------------------------------------
!             Read crop cultivar coefficients
!     -----------------------------------------------------------------
          SECTION = '*CULTI'
          CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
          IF (FOUND .EQ. 0) THEN
            CALL ERROR(SECTION, 42, FILEIO, LNUM)
          ELSE
            READ (LUNIO,1800,IOSTAT=ERR) VARNO,VRNAME,ECONO,
     %                   P1,P2,P5,G2,G3,PHINT ; LNUM = LNUM + 1 
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
1800        FORMAT (A6,1X,A16,1X,A6,1X,6F6.0)    
          ENDIF
          CLOSE(LUNIO)

!     -----------------------------------------------------------------
!              Read Species Coefficients
!     -----------------------------------------------------------------

          FILECC =  TRIM(PATHSR) // FILES
          CALL GETLUN('FILEC', LUNCRP)
          OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)
         
!         ----------------------------------------------------------------
!                Find and Read TEMPERATURE Section
!         ----------------------------------------------------------------
         
          SECTION = '*SEED '
          CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
          IF (FOUND .EQ. 0) THEN
            CALL ERROR(SECTION, 42, FILECC, LNUM)
          ELSE
         
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            READ(C80,'(9X,F7.3)',IOSTAT=ERR) DSGT
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
         
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            READ(C80,'(9X,F7.3)',IOSTAT=ERR) DGET
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
            
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            READ(C80,'(9X,F7.3)',IOSTAT=ERR) SWCG
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
          ENDIF
         
          CLOSE(LUNCRP)

!-----------------------------------------------------------------------
!     Open Ecotype File FILEE
!-----------------------------------------------------------------------
          LNUM = 0
          PATHL  = INDEX(PATHER,BLANK)
          IF (PATHL .LE. 1) THEN
            FILEGC = FILEE
          ELSE
            FILEGC = PATHER(1:(PATHL-1)) // FILEE
          ENDIF

!-----------------------------------------------------------------------
!    Read Ecotype Parameter File
!-----------------------------------------------------------------------
          CALL GETLUN('FILEE', LUNECO)
          OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERRNUM)
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEE,0)
          ECOTYP = '      '
          LNUM = 0
          DO WHILE (ECOTYP .NE. ECONO)
            CALL IGNORE(LUNECO, LNUM, ISECT, C255)
            IF (ISECT .EQ. 1 .AND. C255(1:1) .NE. ' ' .AND.
     &            C255(1:1) .NE. '*') THEN
              READ(C255,3100,IOSTAT=ERRNUM) ECOTYP,ECONAM,TBASE,TOPT,
     &             ROPT,P2O,DJTI,GDDE,DSGFT,RUE, KCAN
3100          FORMAT (A6,1X,A16,1X,9(1X,F5.1))
              IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEE,LNUM)
        
              IF (ECOTYP .EQ. ECONO) THEN
!               Read optional cold sensitivity paramter. 
!               Default to TSEN = 6.0 if no value given.
                IF (C255(80:84) == '     ') THEN
                  TSEN = 6.0
                ELSE
                  READ(C255(80:84),'(F5.0)',IOSTAT=ERRNUM) TSEN
                  IF (ERRNUM .NE. 0 .OR. TSEN < 1.E-6) TSEN = 6.0
                ENDIF
        
!               Read optional number of cold days paramter. 
!               Default to CDAY = 15.0 if no value given.
                IF (C255(86:90) == '     ') THEN
                  CDAY = 15
                ELSE
                  READ(C255(86:90),'(I5)',IOSTAT=ERRNUM) CDAY
                  IF (ERRNUM .NE. 0 .OR. CDAY < 0) CDAY = 15
                ENDIF
        
                EXIT
              ENDIF

            ELSEIF (ISECT .EQ. 0) THEN
              CALL ERROR(ERRKEY,7,FILEE,LNUM)

! CHP 1/4/2004
! IMPLEMENT THIS SECTION OF CODE WHEN A DEFAULT ECOTYPE HAS BEEN ADDED
!    TO THE ECOTYPE FILE.
!            IF (ECONO .EQ. 'DFAULT') CALL ERROR(ERRKEY,35,FILEGC,LNUM)
!
!!           Write message to WARNING.OUT file that default ecotype 
!!             will be used.
!            WRITE(MESSAGE(1),5000) ECONO, FILEE
!            WRITE(MESSAGE(2),5001) 
! 5000       FORMAT('Ecotype ',A6,' not found in file: ',A12)
! 5001       FORMAT('Default ecotype parameters will be used.')
!            CALL WARNING(2, ERRKEY, MESSAGE)
!
!            ECONO = 'DFAULT'
!            REWIND(LUNECO)
!            LNUM = 0
            ENDIF
          ENDDO

          CLOSE (LUNECO)
        ENDIF

      KEP = KCAN/(1-0.07)*(1-0.25)

          DO I=1,20
              STGDOY(I) = 9999999      
          ENDDO
          STGDOY(14) = YRSIM
!          YREMRG = 9999999
          YREMRG = -99  !CHP 5/19/2011

      CUMDTT = 0.0
      SUMDTT = 0.0
      DTT = 0.0
      GPP = 0.0
      ISTAGE = 7
      XSTAGE = 0.1
      MDATE      = -99
      DUMMY = 0

      ISDATE = 0
      TNSOIL = 0.0
      TMSOIL = 0.0
      TH = 00.0
      TEMPCX = 0.
      TEMPCR = 0.0
      TDSOIL = 0.0
      SWSD = 0.0
      SNUP = 0.0
      SNDN = 0.0
      S1 = 0.0
      RATEIN = 0.0
      PSKER = 0.0
      PDTT = 0.0
      P9 = 0.0
      P3 = 0.0
      NDAS = 0.0
      L0 = 0.0
      L = 0
      DLV = 0.0
      DEC = 0.0
      C1 = 0.0
      ACOEF = 0.0
      DOPT = 0.0

!     CHP 9/10/2004  P model
      SeedFrac = 0.0
      VegFrac  = 0.0

!----------------------------------------------------------------------
!         DYNAMIC = RATE OR INTEGRATE
! ---------------------------------------------------------------------

      ELSE    

!         -------------------------------------------------------------
!             Compute Crown Temperature under snow pack.
!             Used in COLD.for
!         -------------------------------------------------------------
          ! TEMPCN = crown temperature when snow is present and 
          !   TMIN < 0. This function computes crown temperature 
          !   as higher than TMIN, C.
          ! TEMPCX = crown temp. for max. development rate, C
          ! SNOW  = Snow depth, mm
          ! XS    = temporary snow depth variable, mm

          TEMPCN = TMIN
          TEMPCX = TMAX
          XS     = SNOW
          XS     = AMIN1 (XS,15.0)
          !------------------------------------------------------------
          ! Calculate crown temperature based on temperature and
          ! snow cover. Crown temperature is higher than TAVG due
          ! to energy balance of snow pack.
          !------------------------------------------------------------
          IF (TMIN .LT. 0.0) THEN
              TEMPCN = 2.0 + TMIN*(0.4+0.0018*(XS-15.0)**2)
          ENDIF
          IF (TMAX .LT. 0.0) THEN
              TEMPCX = 2.0 + TMAX*(0.4+0.0018*(XS-15.0)**2)
          ENDIF
          TEMPCR = (TEMPCX + TEMPCN)/2.0
  
  
          !------------------------------------------------------------
          ! Compute thermal time based on new method developed by J.T.R
          ! at CYMMIT, 5/5/98.  TBASE, TOPT, and ROPT are read in 
          ! from the species file.
          !------------------------------------------------------------
          
          !   DOPT, Devlopment optimum temperature, is set to TOPT 
          !   during vegetative growth and to ROPT after anthesis
          
          DOPT = TOPT
          IF ((ISTAGE .GT. 3) .AND. (ISTAGE .LE. 6)) THEN
              DOPT = ROPT
          ENDIF

          !   Check basic temperature ranges and calculate DTT for
          !   development based on PC with JTR

          IF (TMAX .LT. TBASE) THEN
              DTT = 0.0
          ELSEIF (TMIN .GT. DOPT) THEN
          !   !
       !This statement replaces DTT = TOPT .. GoL and LAH, CIMMYT, 1999
          !   !
              DTT = DOPT - TBASE
          !   !
          !Now, modify TEMPCN, TEMPCX based on soil conditions or snow
          !   ! If wheat and barley is before terminal spiklett stage
          !   ! Or if corn and sorghum are before 10 leaves
          !   !
          ELSEIF (LEAFNO.LE.10) THEN  
          !Check for snow  (should following be GT.0 or GT.15 ?).  
          !   !Based on snow cover, calculate DTT for the day
          !   !
              IF (XS .GT. 0.0) THEN
          !       !
          !       ! Snow on the ground
          !       !
                  DTT    = (TEMPCN + TEMPCX)/2.0 - TBASE
              ELSE
          !       !
          !       ! No snow, compute soil temperature
          !       !
                  ACOEF  = 0.01061 * SRAD + 0.5902
                  TDSOIL = ACOEF * TMAX + (1.0 - ACOEF) * TMIN
                  TNSOIL = 0.36354 * TMAX + 0.63646 * TMIN
                  IF (TDSOIL .LT. TBASE) THEN
                      DTT = 0.0
                  ELSE
                      IF (TNSOIL .LT. TBASE) THEN
                          TNSOIL = TBASE
                      ENDIF
                      IF (TDSOIL .GT. DOPT) THEN
                          TDSOIL = DOPT
                      ENDIF
                      !Import DAYL from WEATHR module. chp 5-6-02
                      !CALL DAYLEN (DOY,XLAT,DAYL,DEC,SNDN,SNUP)
                      TMSOIL = TDSOIL * (DAYL/24.) + 
     &                        TNSOIL * ((24.-DAYL)/24.)
                      IF (TMSOIL .LT. TBASE) THEN
                          DTT = (TBASE+TDSOIL)/2.0 - TBASE
                      ELSE
                          DTT = (TNSOIL+TDSOIL)/2.0 - TBASE
                      ENDIF
          !           !
          !           ! Statement added ... GoL and LAH, CIMMYT, 1999
          !           !
                      DTT = AMIN1 (DTT,DOPT-TBASE)
                  ENDIF
              ENDIF
          !
          ! Now, compute DTT for when Tmax or Tmin out of range
          !
          ELSEIF (TMIN .LT. TBASE .OR. TMAX .GT. DOPT) THEN
              DTT = 0.0
              DO I = 1, 24
                  TH = (TMAX+TMIN)/2. + (TMAX-TMIN)/2. * SIN(3.14/12.*I)
                  IF (TH .LT. TBASE) THEN
                      TH = TBASE
                  ENDIF
                  IF (TH .GT. DOPT) THEN
                      TH = DOPT
                  ENDIF
                  DTT = DTT + (TH-TBASE)/24.0
              END DO
          ELSE
              DTT = (TMAX+TMIN)/2.0 - TBASE
          ENDIF

          DTT   = AMAX1 (DTT,0.0)
          SUMDTT  = SUMDTT  + DTT 
          CUMDTT = CUMDTT + DTT


!     ------------------------------------------------------------------
!           ISTAGE Definitions
!
!             7 - Sowing date
!             8 - Germination
!             9 - Emergence
!             1 - End juvenile
!             2 - Pannicle initiation
!             3 - End leaf growth
!             4 - End pannicle growth
!             5 - Grain fill
!             6 - Maturity
!     ----------------------------------------------------------


      !---------------------------------------------------------
      !               ISTAGE = 7 - Determine sowing date
      !---------------------------------------------------------
          IF (ISTAGE .EQ. 7) THEN
              STGDOY(ISTAGE) = YRDOY
              NDAS           = 0.0
              ISTAGE = 8
              SUMDTT = 0.0
              IF (ISWWAT .EQ. 'N') RETURN

              !---------------------------------------------------------
              !   New Growth Stage Occurred Today. Initialize Some Varia
              !---------------------------------------------------------
              CUMDEP = 0.0
              DO L = 1, NLAYR
                  CUMDEP = CUMDEP + DLAYR(L)
                  IF (SDEPTH .LT. CUMDEP) GO TO 100   ! Was EXIT
              END DO
  100         CONTINUE                                ! Sun Fix
              L0 = L               !L0 is layer that seed is in.

              RETURN


      !-----------------------------------------------------------------
      !               ISTAGE = 8 - Determine Germination Date
      !-----------------------------------------------------------------
          ELSEIF (ISTAGE .EQ. 8) THEN
              IF (ISWWAT .NE. 'N') THEN
                  IF (SW(L0) .LE. LL(L0)) THEN
                      SWSD = (SW(L0)-LL(L0))*0.65 + 
     &                    (SW(L0+1)-LL(L0+1))*0.35
                      NDAS = NDAS + 1

                      IF (NDAS .GE. DSGT) THEN
                          ISTAGE = 6
                          PLTPOP = 0.00
                          GPP    = 1.0

                          WRITE(MESSAGE(1),3500) DSGT
3500  FORMAT ('Crop failure because of lack of germination ',
     &           'within',F7.3,' days of sowing.')
                          CALL WARNING(1,'MZPHEN',MESSAGE)
!                         WRITE (*,3500)
                          IF (IDETO .EQ. 'Y') THEN
                              WRITE (NOUTDO,3500)
                          ENDIF
                          MDATE  = YRDOY
                          CropStatus = 12  !failure to germinate
                          RETURN
                      ENDIF
                 !Germinate when soil water > 0.02 cm3/cm3
                  IF (SWSD .LT. SWCG) RETURN  
                  ENDIF
              ENDIF
              !---------------------------------------------------------
              !   New Growth Stage Occurred Today. Initialize Some Varia
              !---------------------------------------------------------
              STGDOY(ISTAGE) = YRDOY
              ISTAGE =    9
              CUMDTT =  0.0
              SUMDTT =  0.0

              P9    = 45.0 +  GDDE*SDEPTH
              RETURN


      !-----------------------------------------------------------------
      !               ISTAGE = 9 - Determine Seedling Emergence Date
      !-----------------------------------------------------------------
          ELSEIF (ISTAGE .EQ. 9) THEN
              NDAS = NDAS + 1
              ! Emerge when P9 GDD's have been accumulated
!              IF (SUMDTT .LT. P9) RETURN 
              IF (YREMRG .LE. 0) THEN
	          IF (SUMDTT .LT. P9) RETURN
	        ELSE
	          IF (YRDOY .LT. YREMRG) RETURN
	        ENDIF
              ! If GDD's pass a threshold, terminate model run

              IF (P9 .GT. DGET) THEN
                  ISTAGE = 6
                  PLTPOP = 0.00
                  GPP    = 1.0

                  WRITE(MESSAGE(1),1399)
!1399     FORMAT (10X,'Seed ran out of metabolite due to deep planting')
1399      FORMAT (10X,'No emergence. Seed ran out of metabolite.')
                  CALL WARNING(1,'MZPHEN',MESSAGE)

!                 WRITE (*,1399)
                  IF (IDETO .EQ. 'Y') THEN
                      WRITE (NOUTDO,1399)
                  ENDIF
                  MDATE = YRDOY
                  CropStatus = 13   ! failure to emerge
                  RETURN
              ENDIF

              !---------------------------------------------------------
              !   New Growth Stage Occurred Today. Initialize Some Varia
              !---------------------------------------------------------
              STGDOY(ISTAGE) = YRDOY
              ISTAGE = 1
              SUMDTT = SUMDTT - P9
              TLNO   = 30.0
              YREMRG = STGDOY(9)   !Passed back into water balance routi
              RETURN

      !-----------------------------------------------------------------
      !       ISTAGE = 1 - Emergence to End of Juvenile Stage
      !-----------------------------------------------------------------
          ELSEIF (ISTAGE .EQ. 1) THEN
              NDAS   = NDAS + 1   !NDAS - number of days after sowing
              XSTAGE = SUMDTT/P1  !XSTAGE - noninteger growth stage (0-1
                                  !   Used to compute N demand
              ! Stage occurs when GDD threshold reached
              !Return if end of juvenile stage is not reached

!     chp 9/23/2004
!     For P model, we need to estimate the fraction of time completed 
!     between emergence and tassel initiation, VegFrac.  Because stage 2 
!     (end of juvenile stage to tassel initiation) completion is not 
!     based on physiological time, but rather on daylight hours, we 
!     will make the assumption that the physical duration of that phase 
!     is 5 days at optimum temperature.

!     CHP 5/11/2005
!     Extend VegFrac to include phases 3 & 4 (to beginning of effective 
!     grain filling).  Reduce Seed Frac to phase 5.
!             VegFrac = SUMDTT / (P1 + 5.0 * (DOPT - TBASE))
!             don't know value of P3 yet
!             VegFrac = SUMDTT / (P1 + 5. * (DOPT - TBASE) + P3 + DSGFT)

!     CHP 5/25/2007 Move inflection point back to end of stage 3
!             VegFrac = SUMDTT / (P1 + 20. * (DOPT - TBASE) + DSGFT)
!              VegFrac = SUMDTT / (P1 + 20. * (DOPT - TBASE))
! 5/30/2007 CHP Estimate of total time is way off for EAAMOD runs,
!     try using 25* instead of 20*
              VegFrac = SUMDTT / (P1 + 25. * (DOPT - TBASE))

              IF (SUMDTT .LT. P1) RETURN      

              !---------------------------------------------------------
              !   New Growth Stage Occurred Today. Initialize Some Varia
              !---------------------------------------------------------
              STGDOY(ISTAGE) = YRDOY          
              ISTAGE = 2
              SIND   = 0.0


      !-----------------------------------------------------------------
      !       ISTAGE = 2 - End of Juvenile Stage to Tassel Initiation
      !-----------------------------------------------------------------
          ELSEIF (ISTAGE .EQ. 2) THEN
              !NDAS - number of days after sowing
              NDAS   = NDAS + 1       
              !XSTAGE - noninteger growth stage (1-1.5)
              XSTAGE = 1.0 + 0.5*SIND !      Used to compute N demand.

              PDTT = DTT
              IF (ISWWAT .EQ. 'N') THEN    
                  DUMMY = DUMMY + 1       
              ENDIF
 
              IF (DUMMY .EQ. 1) THEN          
                  PDTT = SUMDTT - P1          
              ENDIF                           

!             chp 9/23/2004
!             See note for VegFrac under ISTAGE = 1, above
!             VegFrac = SUMDTT / (P1 + 5.0 * (DOPT - TBASE))
!             don't know value of P3 yet
!             VegFrac = SUMDTT / (P1 + 5. * (DOPT - TBASE) + P3 + DSGFT)

!     CHP 5/25/2007 Move inflection point back to end of stage 3
!             VegFrac = SUMDTT / (P1 + 20. * (DOPT - TBASE) + DSGFT)
!              VegFrac = SUMDTT / (P1 + 20. * (DOPT - TBASE))
! 5/30/2007 CHP Estimate of total time is way off for EAAMOD runs,
!     try using 25* instead of 20*
              VegFrac = MAX(VegFrac,SUMDTT / (P1 + 25. *(DOPT - TBASE)))

              !RATEIN - floral rate of development driven  by daylength
              ! and photoperiod sensitivity value for maize (different 
              ! for SG, ML
              !TWILEN = AMAX1 (TWILEN,P2O)

              IF (TWILEN .GT. P2O) THEN
                RATEIN = 1.0/(DJTI+P2*(TWILEN-P2O))  
              ELSE
                RATEIN = 1.0 / DJTI
              ENDIF
              PDTT   = 1.0   
              SIND = SIND + RATEIN*PDTT
              !Return if panicle initiation has not been reached
              IF (SIND .LT. 1.0) RETURN           


              !---------------------------------------------------------
              !   New Growth Stage Occurred Today. Initialize Some Varia
              !---------------------------------------------------------
              STGDOY(ISTAGE) = YRDOY          
              ISTAGE = 3
              XNTI   = SUMDTT/43.0
           !Next 2 lines: Change implemented at CIMMYT 1999 - JTR,US
              TLNO    = SUMDTT/(PHINT*0.5)+ 5.0           
              P3      = ((TLNO + 0.5) * PHINT) - SUMDTT 
              XNTI    = XN

!             chp 5/11/2005
              SUMDTT_2 = SUMDTT   !SUMDTT_2 = P1 + P2
              VegFrac = MAX(VegFrac,SUMDTT_2 / (SUMDTT_2 + P3 + DSGFT))

              SUMDTT  = 0.0

!             chp 9/23/2004, removed 5/11/2005
!              VegFrac = 1.0
 
      !-----------------------------------------------------------------
      !       ISTAGE = 3 - Tassel Initiation to End of Leaf Growth
      !-----------------------------------------------------------------
          ELSEIF (ISTAGE .EQ. 3) THEN
              ! NDAS - number of days after sowing
              NDAS   = NDAS + 1            
              ! XSTAGE - noninteger growth stage (1.5-4.5)
              !    Used to compute N demand.
              XSTAGE = 1.5 + 3.0*SUMDTT/P3 

!             chp 9/23/2004
!             For P model, we need to estimate the fraction of time 
!             completed between tassel initiation and physiological 
!             maturity, SeedFrac.  
!             CHP 5/11/2005 Extend VegFrac thru stage 4
!             SeedFrac = SUMDTT / (P3 + DSGFT + P5)

!     CHP 5/25/2007 Move inflection point back to end of stage 3
!             VegFrac = (SUMDTT + SUMDTT_2) / (SUMDTT_2 + P3 + DSGFT)
              VegFrac = MAX(VegFrac,(SUMDTT + SUMDTT_2) / (SUMDTT_2+P3))

              IF (SUMDTT .LT. P3) RETURN

              !---------------------------------------------------------
              !   New Growth Stage Occurred Today. Initialize Some Varia
              !---------------------------------------------------------
              STGDOY(ISTAGE) = YRDOY
              ISDATE = YRDOY      
              ISTAGE = 4
              SUMDTT = SUMDTT - P3
              IDURP  = 0

!     CHP 5/25/2007 Move inflection point back to end of stage 3
              VegFrac = 1.0

      !-----------------------------------------------------------------
      !       ISTAGE = 4 - End of Leaf Growth to Beginning Effective Gra
      !-----------------------------------------------------------------
          ELSEIF (ISTAGE .EQ. 4) THEN
              NDAS = NDAS + 1
              IDURP  = IDURP + 1
              ! Determine beginning of effective grain filling period fo
              !  maize.  Silking to beginning EFG is assumed to be 170 G
              XSTAGE = 4.5+5.5*SUMDTT/(P5*0.95)

!             chp 9/23/2004, 5/11/2005
!             SeedFrac = (SUMDTT + P3) / (P3 + DSGFT + P5)
!              VegFrac = (SUMDTT + SUMDTT_2 + P3) / (SUMDTT_2 + P3+DSGFT)
!              VegFrac = AMIN1(VegFrac, 1.0)

!     CHP 5/25/2007 Move inflection point back to end of stage 3
              SeedFrac = SUMDTT / P5

              IF (SUMDTT .LT. DSGFT) RETURN

              !---------------------------------------------------------
              !   New Growth Stage Occurred Today. Initialize Some Varia
              !---------------------------------------------------------

              ! When Silking phase ends and beginning of effective grain
              !  filling begins.  Compute grains per plant, ears per pla
              !  and barrenness

              PSKER = SUMP*1000.0/IDURP*3.4/5.0
              GPP   = G2*PSKER/7200.0 + 50.0
              GPP   = AMIN1 (GPP, G2)
              GPP   = AMAX1 (GPP,0.0)
              EARS  = PLTPOP

              !Determine barrenness for maize
              GPP = AMAX1 (GPP,51.0)
             !
             ! Barreness (mod. US and PWW, 7-21-98)
             ! Barreness function based on stress (PSKER f(SUMP))
             ! Smoothing function for ear number reduction
             !
              IF (GPP .LT. G2*0.15) THEN
                  EARS = PLTPOP*(GPP/(G2*0.15))**0.33
              ELSE
                  !
                  ! CIMMYT - US & JTR revised barreness function
                  !   
                  IF (PLTPOP .GT. 12.0) THEN
                  !
                  ! Barreness from high population
                  !
                      IF (GPP .LT. G2*0.5) THEN
                   !       ABSTRES = AMAX1 (SI1(3), SI3(3))
                      !
                      !    Barreness effect with min. N and H2O stress
                      !
                   !       IF (ABSTRES .LT. 0.25) THEN
                              BARFAC = 0.0085*(1.0-GPP/G2)*PLTPOP**1.5
                              EARS = PLTPOP*(GPP/(G2*0.50))**BARFAC
                   !       ENDIF
                      ENDIF
                  ENDIF
              ENDIF

              EARS           = AMAX1 (EARS,0.0)
              STGDOY(ISTAGE) = YRDOY
              ISTAGE = 5

!             CHP 5/11/2005
!     CHP 5/25/2007 Move inflection point back to end of stage 3
!              VegFrac = 1.0

      !-----------------------------------------------------------------
      !       ISTAGE = 5 - Beginning to end of effective grain filling p
      !-----------------------------------------------------------------

          ELSEIF (ISTAGE .EQ. 5) THEN
              NDAS = NDAS + 1
              XSTAGE = 4.5 + 5.5*SUMDTT/P5
          
!             chp 9/23/2004, 5/11/2005
!             SeedFrac = (SUMDTT + P3) / (P3 + DSGFT + P5)
          
!             CHP 5/25/2007 Move inflection point back to end of stage 3
!             SeedFrac = (SUMDTT - DSGFT) / (P5 - DSGFT)
              SeedFrac = SUMDTT / P5
          
!             End of EFP assumed to be 95%
              IF (SUMDTT .LT. P5*0.95) RETURN  
!              -------------------------------------------------------------
!                 New Growth Stage Occurred Today. Initialize Some Variables
!              -------------------------------------------------------------
              STGDOY (ISTAGE) = YRDOY
              ISTAGE = 6

      !-----------------------------------------------------------------
      !       ISTAGE = 6 - End Effective Grain Filling to Physiological 
      !-----------------------------------------------------------------
          ELSEIF (ISTAGE .EQ. 6) THEN
              IF (DTT .LT. 2.0) SUMDTT = P5

!             chp 9/23/2004, 5/11/2005
!             SeedFrac = (SUMDTT + P3) / (P3 + DSGFT + P5)
              SeedFrac = (SUMDTT - DSGFT) / (P5 - DSGFT)

!     CHP 5/25/2007 Move inflection point back to end of stage 3
!             SeedFrac = (SUMDTT - DSGFT) / (P5 - DSGFT)
              SeedFrac = SUMDTT / P5

              IF (SUMDTT .LT. P5)  RETURN
              !---------------------------------------------------------
              !   New Growth Stage Occurred Today. Initialize Some Varia
              !---------------------------------------------------------
              STGDOY(ISTAGE) = YRDOY
              MDATE          = YRDOY
              CropStatus     = 1  !crop matured normally
              !ISTAGE = 7
              ISTAGE = 10  !CHP - Prevents growth parameters from being
                           ! set back to initial values.  08/11/03
              CUMDTT = 0.0
              DTT    = 0.0
              IF (PLTPOP .NE. 0.0) THEN
                  IF (GPP .LE. 0.0) THEN
                      GPP = 1.0
                  ENDIF
              ENDIF

!             chp 5/11/2005
              SeedFrac = 1.0
! ----------------------------------------------------------------------
          ENDIF            ! End ISTAGE Loop
! ----------------------------------------------------------------------

      ENDIF  ! End DYNAMIC STRUCTURE
      RETURN

!-----------------------------------------------------------------------
!     Format Strings
!-----------------------------------------------------------------------


      END SUBROUTINE MZ_PHENOL

! DYNAMIC    Modular control
! ABSTRES    Maximum of water stress stage 1 and 3
! ACOEF      Coefficient
! BARFAC     Factor to reduce ears/plant under high populations (barrenn
! C1         Used to comptue daylength (computed in maize.for)
! CUMDEP     Cumulative depth of soil, cm
! CUMDTT     Cumulative daily thermal time after germination, C
! DAYL       Daylength, hours
! DEC        Used to compute daylength
! DGET       Threshold defined as growing degree days between germination and emergence.
!            If this threshold is exceeded, crop failure ocurrs.
! DJTI       Minimum days from end of juvenile stage to tassel initiation if the cultivar
!            is not photoperiod sensitive, DJTI
! DLAYR(L)   Soil thickness in layer L (cm)
! DLV        Used to compute daylength
! DOPT       Development optimum temperature
! DSGFT      GDD from silking to effective grain filling period, C
! DSGT       Maximum number of days from sowing to germination before crop failure occurs.
! DTT        Growing degree days today, C
! DUMMY      Temporary variable
! EARS       Ears per m2, computed here and used in grosub.
! ECONO      Ecotype number for the variety (not really used in maize ye
! ERR        Determines if error in reading file (0=ok, 1=error)
! ERRKEY     Variable containing routine where error occurred
! (ERRKEY='MZ_PHENL')
! FILEC      Filename of .SPE or species file
! FILEIO     Filename containing model inputs (IBSNAT35.INP)
! FOUND      Indicates if a section in a file is found
! G2         Potential kernel number, kernels/plant
! G3         Potential kernel growth rate mg/kernel/day
! GDDE       Growing degree days per cm seed depth required for emergence, GDD/cm
! GPP        Grain number per plant, grains/plant
! I          Loop counter
! IDETO      Screen output switch (Y/N)
! IDURP      Duration of ISTAGE 4, calendar days
! ISTAGE     Growth stage
! ISWWAT     Water balance switch (Y/N)
! LEAFNO     Number of oldest leaf per plant (same as XN)
! L          Loop counter
! L0         Temporary soil layer number
! LINC       Indicates if a line is a good line
! LL(NL)     Soil water lower limit, cm3/cm3
! LNUM       Line number in an input file
! LUNIO      Logical input number for model input file
! LUNIO      Assign value to LUNIO for local use.
! MDATE      Year and day of year of maturity
! NDAS       Number of days after sowing
! NLAYR      Number of soil layers
! NOUTDO     Output file number
! P1         GDD from seedling emergence to end of juvenile phase, C
! P2         Photoperiod sensitivity coefficient, 1/hr
! P2O        Minimum daylength below which daylength does not affect dev
! P3         Cumulative GDD required to complete ISTAGE 3, C
! P5         GDD from silking to physiological maturity, C
! P9         Growing degree days from germination to emergence, C
! PATHCR     Pathname of species file
! DTT
! PHINT      Phyllochron interval. Number of GDD required for new leaf e
! PLTPOP     Plant population, no./m2
! PSKER      Average rate of photosynthesis during ISTAGE 4
! RATEIN     Rate of floral induction
! ROPT       Second optimum temperature for development from species fil
! SDEPTH     Sowing depth, cm
! SECTION    Temporary variable used to identify section in a file
! S1         Used to compute daylength (computed in maize.for)
! SI1(6)     Water stress during a growth stage used for output
! SI3(6)     Water stress during a growth stage used for output
! SIND       Summed photoperiod induction rate
! SNDN       Sun down
! SNOW       Snow, mm
! SNUP       Sun up
! SRAD       Daily solar radiation, MJ/m2/day
! STGDOY(20) Year and day of year that a growth stage occurred on
! SUMDTT     Sum of GDD for a given stage, C
! SUMP       Cumulative plant growth during ISTAGE 4, g/plant
! SW(NL)     Soil water content in layer, cm3/cm3
! SWCG       Minimum soil water available required for germination to occur, cm3/cm3
! SWSD       Modified soil water content for computing emergence
! TBASE      Base temperature for development from ecotype file, C
! TDSOIL     Weighted average soil temperature, C
! TEMPCN     Crown temperature when snow is present and TMIN < 0. This f
!            computes crown temperature as higher than TMIN, C.
! TEMPCR     Crown temperature, C
! TEMPCX     Crown temperature for maximum development rate, C
! TH         Intermedate variable for computing GDD today, C
! TLNO       Total leaf numbers that will eventually develop
! TMAX       Daily maximum temperature, C
! TMIN       Daily minimum temperature, C
! TMSOIL     Weighted average soil temperature, C
! TNSOIL     Weighted average soil temperture, C
! TOPT       Optimum temperature for development from species file, C
! TWILEN     Twilight definition of daylength
! VARNO      Variety identification number
! VRNAME     Variety name
! WTHADJ(2,8)Note, used here, but not passed into maize.for from cropgro
! WMODB*1    Note, used here, but not passed into maize.for from cropgro
! XLAT       Latitude
! XN         Number of oldest expanding leaf
! XNTI       Number of leaves at tassel initiation (used in grosub)
! XS         Temporary snow depth variable
! XSTAGE     Non-integer growth stage indicator
! YRDOY      Year and day of year
! YREMRG     Year and day of year of emergence (passed back to water bal
! YRSIM      Year and day of year of first day of simulation

