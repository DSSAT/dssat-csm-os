!======================================================================
!  SU_PHENOL, Subroutine
!
!  Determines Phenological Stage and Growing Degree Days for Sunflower
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
!----------------------------------------------------------------------
      SUBROUTINE SU_PHENOL(DYNAMIC,ISWWAT,FILEIO,IDETO,  !C
     &    CUMDEP,DAYL,DLAYR,LL,NLAYR,PLTPOP,SDEPTH,      !I  LEAFNO,
     &    SW,TMAX,TMIN, TWILEN,                          !I  SNOW, SRAD,
     &    YRDOY,YRSIM,                                   !I
!    &    IDURP,                                         !I
     &    CUMDTT,DTT,GPP,ISDATE,ISTAGE,MDATE,STGDOY,SUMDTT, !O
     &    TLNO,XSTAGE,YREMRG,RUE,KCAN,KEP, TSEN, CDAY,      !O !, P3
     &    SeedFrac,VegFrac,P3P,P9)
   

      USE ModuleDefs
      IMPLICIT  NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE, WARNING
      SAVE
!----------------------------------------------------------------------
!                             Define Variables
!----------------------------------------------------------------------
      INTEGER         DYNAMIC               
      REAL            ACOEF           
      CHARACTER*1     BLANK         
      REAL            C1   
      INTEGER         CDAY 
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
      CHARACTER*6     ECONO           
      INTEGER         ERR             
      CHARACTER*6     ERRKEY          
      PARAMETER       (ERRKEY='SUPHEN')
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
!     INTEGER         IDURP     
      INTEGER         ISTAGE         
      CHARACTER*1     ISWWAT         
      REAL            KCAN
      REAL            KEP
!     INTEGER         LEAFNO         
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
!     REAL            P3   
      REAL            P3P          
      REAL            P5             
      REAL            P9             
      CHARACTER*80    PATHCR 
      CHARACTER*80    PATHSR
      CHARACTER*80    PATHER        
      REAL            PDTT
      REAL            PLTPOP       
      REAL            PSKER          
      REAL            RATEIN         
      REAL            ROPT           
      REAL            RUE
      REAL            SDEPTH         
      CHARACTER*6     SECTION        
      REAL            S1    
      REAL            SIND           
      REAL            SNDN           
!     REAL            SNOW           
      REAL            SNUP           
!     REAL            SRAD           
      INTEGER         STGDOY(20)     
      REAL            SUMDTT
      REAL            SUMDTT_2 !introduced for plant P routine                
      REAL            SW(NL)         
      REAL            SWCG
      REAL            SWSD           
      REAL            TBASE          
      REAL            TDSOIL         
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
      REAL            XSTAGE  
      REAL O1
      INTEGER         YRDOY          
      INTEGER         YREMRG         
      INTEGER         YRSIM
      INTEGER ISDATE          
      INTEGER PATHL
      PARAMETER (BLANK = ' ')
      INTEGER LUNECO
      REAL TTMP,ZSIND
      CHARACTER*6 ECOTYP
      INTEGER ISECT
      CHARACTER*255 C255
      CHARACTER*16  ECONAM
      INTEGER LUNCRP
      CHARACTER*92 FILECC
      CHARACTER*80 C80
      CHARACTER*78 MESSAGE(10)
      REAL TMFAC1(10)

!     CHP added for P model
      REAL SeedFrac, VegFrac
     
       DO I = 1, 8
         TMFAC1(I) = 0.931 + 0.114*I-0.0703*I**2+0.0053*I**3
      END DO
     
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
     %                   P1,P2,P5,G2,G3,O1 ; LNUM = LNUM + 1 

            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
1800        FORMAT (A6,1X,A16,1x,A6,1X,F5.1,1X,F5.2,1X,F5.1,1X,
     &        F5.0,1X,F5.2,1X,F5.2)   


          ENDIF
          CLOSE(LUNIO)
!XXXXX CONT

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
!               Default to TSEN = -3.0 if no value given.
                IF (C255(80:84) == '     ') THEN
                  TSEN = -3.0
                ELSE
                  READ(C255(80:84),'(F5.0)',IOSTAT=ERRNUM) TSEN
                  IF (ERRNUM .NE. 0 .OR. TSEN < 1.E-6) TSEN = -3.0
                ENDIF
        
!               Read optional number of cold days paramter. 
!               Default to CDAY = 7 if no value given.
                IF (C255(86:90) == '     ') THEN
                  CDAY = 7
                ELSE
                  READ(C255(86:90),'(I5)',IOSTAT=ERRNUM) CDAY
                  IF (ERRNUM .NE. 0 .OR. CDAY < 0) CDAY = 7
                ENDIF
        
                EXIT
              ENDIF

            ELSEIF (ISECT .EQ. 0) THEN
              CALL ERROR(ERRKEY,7,FILEE,LNUM)

! CHP 1/4/2004
! IMPLEMENT THIS SECTION OF CODE WHEN A DEFAULT ECOTYPE HAS BEEN ADDED
!    TO THE ECOTYPE FILE.
            IF (ECONO .EQ. 'DFAULT') CALL ERROR(ERRKEY,35,FILEGC,LNUM)
!           Write message to WARNING.OUT file that default ecotype 
!             will be used.
            WRITE(MESSAGE(1),5000) ECONO, FILEE
            WRITE(MESSAGE(2),5001) 
 5000       FORMAT('Ecotype ',A6,' not found in file: ',A12)
 5001       FORMAT('Default ecotype parameters will be used.')
            CALL WARNING(2, ERRKEY, MESSAGE)
!
            ECONO = 'DFAULT'
            REWIND(LUNECO)
            LNUM = 0
            ENDIF
          ENDDO

          CLOSE (LUNECO)
        ENDIF

C     KEP = KCAN/(1-0.07)*(1-0.25)
      
C     recalculated KEP taken into account that K(FR)=0.5 K(PAR)
C     taking L=1

      KEP=-LOG(0.5*EXP(-KCAN)+.5*EXP(-0.5*KCAN))
   

          DO I=1,20
              STGDOY(I) = 9999999      
          ENDDO
          STGDOY(14) = YRSIM
!          YREMRG = 9999999
          YREMRG = -99  !CHP 5/19/2011

      CUMDTT = 0.0
      SUMDTT = 0.0
      DTT = 0.0
      
      ISTAGE = 7
      XSTAGE = 0.1
      MDATE      = -99
      DUMMY = 0
      GPP=0.
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
      P3P=0.0
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
!        IF (ISTAGE.GE.1.AND.ISTAGE.LE.6) THEN
!          TBASE=4.0
!        ELSE
!          TBASE=6.0
!       ENDIF
       DTT    = .5*(TMAX+TMIN) - TBASE
      IF (TMIN .LE. TBASE .OR. TMAX .GE. 28.0) THEN
         IF (TMAX .LT. TBASE) THEN
            DTT = 0.0
         ENDIF
         IF (DTT .NE. 0.0) THEN
            DTT = 0.0
            DO I = 1, 8
               TTMP = TMIN + TMFAC1(I)*(TMAX-TMIN)
               IF (TTMP .GT. TBASE .AND. TTMP .LE. 28.0) THEN
                  DTT = DTT + (TTMP-TBASE)/8.0
               ENDIF
               IF (TTMP .GT. 28.0 .AND. TTMP .LT. 40.0) THEN
                  DTT = DTT + (28.0-TBASE)*(1.-.007*(TTMP-28.0)**2.)/8.0
              ENDIF
            END DO
         ENDIF
      ENDIF


          DTT   = AMAX1 (DTT,0.0)
! thermal time from end of grain fillin to physiological maturity has Tb=0 C  and takes 210 C d        
          IF (ISTAGE.EQ.6) THEN
            DTT=.5*(TMAX+TMIN)
          ENDIF  
          SUMDTT  = SUMDTT  + DTT 
          CUMDTT = CUMDTT + DTT
          

!     ------------------------------------------------------------------
!           ISTAGE Definitions
!
!  �������������������������������������͸
!  �7 - Sowing date                      �
!  �8 - Germination                      �
!  �9 - Emergence                        �
!  �1 - End juvenile                     �
!  �2 - Floret initiation              �
!  �3 - Flowering                �
!  �4 - Star grain filling             �
!  �5 - End grain filling                      �
!  �6 - Maturity                         �
!  �������������������������������������;


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

                          WRITE(MESSAGE(1),3500)
                          CALL WARNING(1,'MZPHEN',MESSAGE)
                          WRITE (     *,3500)
                          IF (IDETO .EQ. 'Y') THEN
                              WRITE (NOUTDO,3500)
                          ENDIF
                          MDATE  = YRDOY
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

              P9     = 66.0 + 11.9*SDEPTH                ! SUN

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
                  CALL WARNING(1,'SUPHEN',MESSAGE)

                  WRITE (     *,1399)
                  IF (IDETO .EQ. 'Y') THEN
                      WRITE (NOUTDO,1399)
                  ENDIF
                  MDATE = YRDOY
                  RETURN
              ENDIF

              !---------------------------------------------------------
              !   New Growth Stage Occurred Today. Initialize Some Varia
              !---------------------------------------------------------
              STGDOY(ISTAGE) = YRDOY
              ISTAGE = 1
              SUMDTT = SUMDTT - P9

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
              VegFrac = SUMDTT / (P1 + 25. * (28. - TBASE))

              IF (SUMDTT .LT. P1) RETURN      

              !---------------------------------------------------------
              !   New Growth Stage Occurred Today. Initialize Some Varia
              !---------------------------------------------------------
              STGDOY(ISTAGE) = YRDOY          
              ISTAGE = 2
              SIND   = 0.0


      !-----------------------------------------------------------------
      !       ISTAGE = 2 - End of Juvenile Stage to Floret Initiation
      !-----------------------------------------------------------------
          ELSEIF (ISTAGE .EQ. 2) THEN
              !NDAS - number of days after sowing
              NDAS   = NDAS + 1       
            

 
!             chp 9/23/2004
!
              VegFrac = MAX(VegFrac,SUMDTT / (P1 + 25. *(28. - TBASE)))

              !RATEIN - floral rate of development driven  by daylength
              ! and photoperiod sensitivity value for sunflower  
              DAYL=TWILEN
              DAYL   = AMIN1  (DAYL,15.0)
              RATEIN = 1.0/(3.0 + P2 * (15.0 - DAYL))    ! SUN
              SIND   = SIND + RATEIN
              ZSIND  = AMIN1 (SIND,1.0)
              XSTAGE = 1.0 + 0.5 * ZSIND
              IF (SIND .LT. 1.0) RETURN
          
              P3P  = 2.0*P1
              !
              ! P3P will define the start of anthesis

             !---------------------------------------------------------
              !   New Growth Stage Occurred Today. Initialize Some Varia
              !---------------------------------------------------------
              STGDOY(ISTAGE) = YRDOY          
              ISTAGE = 3               

!             chp 5/11/2005
              SUMDTT_2 = SUMDTT   !SUMDTT_2 = P1 + P2
              VegFrac = MAX(VegFrac,SUMDTT_2 / (SUMDTT_2 + P3P + DSGFT))
              TLNO = IFIX(SUMDTT/14.0+2.0)  
              SUMDTT  = 0.0

 
      !-----------------------------------------------------------------
      !       ISTAGE = 3 - Tassel Initiation to End of Leaf Growth
      !-----------------------------------------------------------------
          ELSEIF (ISTAGE .EQ. 3) THEN
              ! NDAS - number of days after sowing
              NDAS   = NDAS + 1            
              ! XSTAGE - noninteger growth stage (1.5-4.5)
              !    Used to compute N demand.
              XSTAGE = 1.5 + 3.0*SUMDTT/P3P 
c                
              VegFrac=MAX(VegFrac,(SUMDTT+SUMDTT_2) /(SUMDTT_2+P3P)) 

              IF (SUMDTT .LT. P3P) RETURN

              !---------------------------------------------------------
              !   New Growth Stage Occurred Today. Initialize Some Varia
              !---------------------------------------------------------
              STGDOY(ISTAGE) = YRDOY
              ISDATE = YRDOY      
              ISTAGE = 4               !FIRST ANTHESIS, END POLLINATION
              SUMDTT = SUMDTT - P3P
    

!     CHP 5/25/2007 Move inflection point back to end of stage 3
              VegFrac = 1.0

      !-----------------------------------------------------------------
      !       ISTAGE = 4 - End of Leaf Growth to Beginning Effective Gra
      !-----------------------------------------------------------------
          ELSEIF (ISTAGE .EQ. 4) THEN
              NDAS = NDAS + 1

              ! Determine beginning of effective grain filling period fo
              !  maize.  Silking to beginning EFG is assumed to be 170 G
              XSTAGE = 4.5+5.5*SUMDTT/(P5*0.95)


              SeedFrac = SUMDTT / P5

              IF (SUMDTT .LT. DSGFT) RETURN
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
          

              SeedFrac = SUMDTT / P5
          
              IF (SUMDTT .LT. P5) RETURN  !End of EFP assumed to be P5
!              -------------------------------------------------------------
!                 New Growth Stage Occurred Today. Initialize Some Variables
!              -------------------------------------------------------------
              STGDOY (ISTAGE) = YRDOY
              SUMDTT = 0.
              ISTAGE = 6

!      -----------------------------------------------------------------
!             ISTAGE = 6 - End Effective Grain Filling to Harvest Maturity 
!      -----------------------------------------------------------------
          ELSEIF (ISTAGE .EQ. 6) THEN
              

              

              IF (SUMDTT .LT. 210.)  RETURN
              !---------------------------------------------------------
              !   New Growth Stage Occurred Today. Initialize Some Varia
              !---------------------------------------------------------
              STGDOY(ISTAGE) = YRDOY
              MDATE          = YRDOY
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

 1399  FORMAT (10X,'Seed ran out of metabolite due to deep planting')
 3500  FORMAT ('Crop failure because of lack of germination ',
     &           'within 15 days of sowing')

      END SUBROUTINE SU_PHENOL

! DYNAMIC    Modular control
! ABSTRES    Maximum of water stress stage 1 and 3
! ACOEF      Coefficient

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
! DSGFT      GDD from anthesis to effective grain filling period, C d
! DSGT       Maximum number of days from sowing to germination before crop failure occurs.
! DTT        Growing degree days today, C
! DUMMY      Temporary variable

! ECONO      Ecotype number for the variety (not really used in maize ye
! ERR        Determines if error in reading file (0=ok, 1=error)
! ERRKEY     Variable containing routine where error occurred
! (ERRKEY='SU_PHENL')
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
! LEAFNO     Number of oldest leaf per plant 
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
! P3P        Cumulative GDD required to complete ISTAGE 3, C
! P5         GDD from silking to physiological maturity, C
! P9         Growing degree days from germination to emergence, C
! PATHCR     Pathname of species file
! DTT
! PLTPOP     Plant population, no./m2
! PSKER      Average rate of photosynthesis during ISTAGE 4
! RATEIN     Rate of floral induction
! ROPT       Second optimum temperature for development from species fil
! SDEPTH     Sowing depth, cm
! SECTION    Temporary variable used to identify section in a file
! S1         Used to compute daylength (computed in maize.for)
! SIND       Summed photoperiod induction rate
! SNDN       Sun down
! SNOW       Snow, mm
! SNUP       Sun up
! SRAD       Daily solar radiation, MJ/m2/day
! STGDOY(20) Year and day of year that a growth stage occurred on
! SUMDTT     Sum of GDD for a given stage, C
! SW(NL)     Soil water content in layer, cm3/cm3
! SWCG       Minimum soil water available required for germination to occur, cm3/cm3
! SWSD       Modified soil water content for computing emergence
! TBASE      Base temperature for development from ecotype file, C
! TDSOIL     Weighted average soil temperature, C
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
! XSTAGE     Non-integer growth stage indicator
! YRDOY      Year and day of year
! YREMRG     Year and day of year of emergence (passed back to water bal
! YRSIM      Year and day of year of first day of simulation

