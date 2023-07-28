c     SUBROUTINE: SC_OPGROW_SAM()

c     This subroutine handles the output of growth
c     aspects (yields, roots, etc)

!     ***************************************************************
!     *** This subroutine was borrowed from CANEGRO at 01/08/2020 ***
!     *** Adapted with permission of chp by Murilo Vianna         ***
!     *** source: SC_OUTPUT.for                                   ***
!     ***************************************************************

      SUBROUTINE SC_OPGROW_SAM (CONTROL, CaneCrop,
     & YRPLT)

c     Define DSSAT composite variables:
      USE ModuleDefs
      USE ModuleData

c     SAMUCA's composite variables:
      USE SAM_ModuleDefs

      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, YR_DOY
      SAVE
      
c     Declare composite variables:
      TYPE (ControlType)  CONTROL   ! DSSAT's control
      Type (SwitchType)   ISWITCH   ! DSSAT's switches
      TYPE (CaneSamuca)   CaneCrop  ! Samuca's Composite variables
      
c     DSSAT inputs:
      INTEGER         NOUTDG        ! file unit # for growth output file
      INTEGER         RUN           ! local run number
      INTEGER         ERRNUM        ! Error status
      INTEGER         DYNAMIC       ! Dynamic
      LOGICAL         FILE_EXISTS   ! Does the file exist, 1st run?
      CHARACTER*1     IDETG         ! Print Output flag
      CHARACTER*20    FILEIO        ! local ?
      CHARACTER*20    OFILE         ! Filename
      
!     real    SW(NL)                ! Soil water content
!     real    watdmd                ! water demand (TRWUP/Transp)
      
      !--- Time control
      INTEGER DAP, DAS, FROP, YRDOY, YRPLT, YEAR, DOY
!     INTEGER TIMDIF      
            
      !--- Output setup
      INTEGER I, J        ! Varible counters
      integer NUM_OVARS   ! Number of output variables
      integer VAR_WIDTH   ! Width of output columns
      
      !--- Number of variables and collumn widht
      parameter(NUM_OVARS = 45, VAR_WIDTH = 12)
      
c     MJ:
c     GROHEAD is a 2-dimensional array with each column header stored
c     separately      
      CHARACTER*15 GROHEAD(4, NUM_OVARS)
      CHARACTER*15 GRO_OUT(NUM_OVARS)     
      
!     Length of variable name string (excluding leading and trailing whitespace)
      INTEGER VLEN            
      INTEGER SKIP            ! How many spaces need to be skipped?
      
!     String equivalents of VLEN and SKIP
      CHARACTER*10 SKIP_STR, VLEN_STR, WIDTH_STR  
      CHARACTER*1024 FMT_STR, T_FMT_STR       ! Runtime format statement

c     General format statments for outputting heading comments
c     and daily values
      CHARACTER*100 G_FMT_STR, D_FMT_STR

c     String lengths:
      INTEGER TLEN, FLEN, ENDI

      !---------------------!
      !--- Output Header ---!
      !---------------------!      
        DATA GROHEAD(1,1) /'Year'/
        DATA GROHEAD(2,1) /'of'/
        DATA GROHEAD(3,1) /'Simul'/
        DATA GROHEAD(4,1) /'@YEAR'/

        DATA GROHEAD(1,2) /'Day'/
        DATA GROHEAD(2,2) /'of'/
        DATA GROHEAD(3,2) /'Year'/
        DATA GROHEAD(4,2) /'DOY'/

        DATA GROHEAD(1,3) /'Days'/
        DATA GROHEAD(2,3) /'after'/
        DATA GROHEAD(3,3) /'Simul'/
        DATA GROHEAD(4,3) /'DAS'/

        DATA GROHEAD(1,4) /'Days'/
        DATA GROHEAD(2,4) /'after'/
        DATA GROHEAD(3,4) /'Plant'/
        DATA GROHEAD(4,4) /'DAP'/

        DATA GROHEAD(1,5) /'PlCane'/
        DATA GROHEAD(2,5) /'or'/
        DATA GROHEAD(3,5) /'Ratoon'/
        DATA GROHEAD(4,5) /'RCANE'/

        DATA GROHEAD(1,6) /'Seq'/
        DATA GROHEAD(2,6) /'Ratoon'/
        DATA GROHEAD(3,6) /'Simul'/
        DATA GROHEAD(4,6) /'SEQR'/

        DATA GROHEAD(1,7) /'Actual'/
        DATA GROHEAD(2,7) /'Crop'/
        DATA GROHEAD(3,7) /'DStage'/
        DATA GROHEAD(4,7) /'GSTD'/

        DATA GROHEAD(1,8) /'Accum'/
        DATA GROHEAD(2,8) /'D.Day'/
        DATA GROHEAD(3,8) /'oC.d'/
        DATA GROHEAD(4,8) /'TTTOT'/

        DATA GROHEAD(1,9) /'Plant'/
        DATA GROHEAD(2,9) /'Weight'/
        DATA GROHEAD(3,9) /'DMt/ha'/
        DATA GROHEAD(4,9) /'TDRW'/

        DATA GROHEAD(1,10) /'Aerial'/
        DATA GROHEAD(2,10) /'Weight'/
        DATA GROHEAD(3,10) /'DMt/ha'/
        DATA GROHEAD(4,10) /'BADMD'/

        DATA GROHEAD(1,11) /'BelowG'/
        DATA GROHEAD(2,11) /'Weight'/
        DATA GROHEAD(3,11) /'DMt/ha'/
        DATA GROHEAD(4,11) /'BBDMD'/

        DATA GROHEAD(1,12) /'Stalk'/
        DATA GROHEAD(2,12) /'Weight'/
        DATA GROHEAD(3,12) /'DMt/ha'/
        DATA GROHEAD(4,12) /'SMDMD'/

        DATA GROHEAD(1,13) /'Leaves'/
        DATA GROHEAD(2,13) /'Weight'/
        DATA GROHEAD(3,13) /'DMt/ha'/
        DATA GROHEAD(4,13) /'LGDMD'/

        DATA GROHEAD(1,14) /'Root'/
        DATA GROHEAD(2,14) /'Weight'/
        DATA GROHEAD(3,14) /'DMt/ha'/
        DATA GROHEAD(4,14) /'RDMD'/

        DATA GROHEAD(1,15) /'Stalk'/
        DATA GROHEAD(2,15) /'Weight'/
        DATA GROHEAD(3,15) /'FMt/ha'/
        DATA GROHEAD(4,15) /'SMFMD'/

        DATA GROHEAD(1,16) /'Sucros'/
        DATA GROHEAD(2,16) /'Weight'/
        DATA GROHEAD(3,16) /'DMt/ha'/
        DATA GROHEAD(4,16) /'SUCMD'/

        DATA GROHEAD(1,17) /'Dead'/
        DATA GROHEAD(2,17) /'Leaves'/
        DATA GROHEAD(3,17) /'DMt/ha'/
        DATA GROHEAD(4,17) /'DDLMD'/

        DATA GROHEAD(1,18) /'Dead'/
        DATA GROHEAD(2,18) /'Tillers'/
        DATA GROHEAD(3,18) /'DMt/ha'/
        DATA GROHEAD(4,18) /'DDTMD'/

        DATA GROHEAD(1,19) /'Stalk'/
        DATA GROHEAD(2,19) /'POL'/
        DATA GROHEAD(3,19) /'%'/
        DATA GROHEAD(4,19) /'SU%FMD'/

        DATA GROHEAD(1,20) /'Canopy'/
        DATA GROHEAD(2,20) /'LAI'/
        DATA GROHEAD(3,20) /'m2/m2'/
        DATA GROHEAD(4,20) /'LAIGD'/

        DATA GROHEAD(1,21) /'Tiller'/
        DATA GROHEAD(2,21) /'Pop'/
        DATA GROHEAD(3,21) /'#/m2'/
        DATA GROHEAD(4,21) /'T#AD'/

        DATA GROHEAD(1,22) /'Plant'/
        DATA GROHEAD(2,22) /'Height'/
        DATA GROHEAD(3,22) /'m'/
        DATA GROHEAD(4,22) /'SHTD'/

        DATA GROHEAD(1,23) /'Total'/
        DATA GROHEAD(2,23) /'Leaves'/
        DATA GROHEAD(3,23) /'#/Stk'/
        DATA GROHEAD(4,23) /'T#SD'/

        DATA GROHEAD(1,24) /'Stress'/
        DATA GROHEAD(2,24) /'Factor'/
        DATA GROHEAD(3,24) /'Expans'/
        DATA GROHEAD(4,24) /'WSGD'/

        DATA GROHEAD(1,25) /'Stress'/
        DATA GROHEAD(2,25) /'Factor'/
        DATA GROHEAD(3,25) /'Photos'/
        DATA GROHEAD(4,25) /'WSPD'/

        DATA GROHEAD(1,26) /'Actual'/
        DATA GROHEAD(2,26) /'Crop'/
        DATA GROHEAD(3,26) /'Status'/
        DATA GROHEAD(4,26) /'CSTAT'/

        DATA GROHEAD(1,27) /'Frac'/
        DATA GROHEAD(2,27) /'IPAR'/
        DATA GROHEAD(3,27) /'GLAI'/
        DATA GROHEAD(4,27) /'GLI%D'/

        DATA GROHEAD(1,28) /'Frac'/
        DATA GROHEAD(2,28) /'IPAR'/
        DATA GROHEAD(3,28) /'TLAI'/
        DATA GROHEAD(4,28) /'TLI%D'/

        DATA GROHEAD(1,29) /'Root'/
        DATA GROHEAD(2,29) /'Depth'/
        DATA GROHEAD(3,29) /'m'/
        DATA GROHEAD(4,29) /'RDPD'/

        DATA GROHEAD(1,30) /'Root_Len.'/
        DATA GROHEAD(2,30) /'density(1)'/
        DATA GROHEAD(3,30) /'cm3/cm3'/
        DATA GROHEAD(4,30) /'RL1D'/

        DATA GROHEAD(1,31) /'Root_Len.'/
        DATA GROHEAD(2,31) /'density(2)'/
        DATA GROHEAD(3,31) /'cm3/cm3'/
        DATA GROHEAD(4,31) /'RL2D'/

        DATA GROHEAD(1,32) /'Root_Len.'/
        DATA GROHEAD(2,32) /'density(3)'/
        DATA GROHEAD(3,32) /'cm3/cm3'/
        DATA GROHEAD(4,32) /'RL3D'/

        DATA GROHEAD(1,33) /'Root_Len.'/
        DATA GROHEAD(2,33) /'density(4)'/
        DATA GROHEAD(3,33) /'cm3/cm3'/
        DATA GROHEAD(4,33) /'RL4D'/

        DATA GROHEAD(1,34) /'Root_Len.'/
        DATA GROHEAD(2,34) /'density(5)'/
        DATA GROHEAD(3,34) /'cm3/cm3'/
        DATA GROHEAD(4,34) /'RL5D'/

        DATA GROHEAD(1,35) /'Root_Len.'/
        DATA GROHEAD(2,35) /'density(6)'/
        DATA GROHEAD(3,35) /'cm3/cm3'/
        DATA GROHEAD(4,35) /'RL6D'/

        DATA GROHEAD(1,36) /'Root_Len.'/
        DATA GROHEAD(2,36) /'density(7)'/
        DATA GROHEAD(3,36) /'cm3/cm3'/
        DATA GROHEAD(4,36) /'RL7D'/

        DATA GROHEAD(1,37) /'Root_Len.'/
        DATA GROHEAD(2,37) /'density(8)'/
        DATA GROHEAD(3,37) /'cm3/cm3'/
        DATA GROHEAD(4,37) /'RL8D'/

        DATA GROHEAD(1,38) /'Root_Len.'/
        DATA GROHEAD(2,38) /'density(9)'/
        DATA GROHEAD(3,38) /'cm3/cm3'/
        DATA GROHEAD(4,38) /'RL9D'/

        DATA GROHEAD(1,39) /'Root_Len.'/
        DATA GROHEAD(2,39) /'density(10)'/
        DATA GROHEAD(3,39) /'cm3/cm3'/
        DATA GROHEAD(4,39) /'RL10D'/

        DATA GROHEAD(1,40) /'TPRT'/
        DATA GROHEAD(2,40) /'WatUp'/
        DATA GROHEAD(3,40) /'mm/d'/
        DATA GROHEAD(4,40) /'TRWUP'/

        DATA GROHEAD(1,41) /'Pot'/
        DATA GROHEAD(2,41) /'Transp'/
        DATA GROHEAD(3,41) /'mm/d'/
        DATA GROHEAD(4,41) /'EOP'/

        DATA GROHEAD(1,42) /'Gross'/
        DATA GROHEAD(2,42) /'photos'/
        DATA GROHEAD(3,42) /'rate t/ha/d'/
        DATA GROHEAD(4,42) /'PGRD'/

        DATA GROHEAD(1,43) /'PARCE'/
        DATA GROHEAD(2,43) /'RUE'/
        DATA GROHEAD(3,43) /'gDW/MJPAR'/
        DATA GROHEAD(4,43) /'PARCE'/

        DATA GROHEAD(1,44) /'G_RESP'/
        DATA GROHEAD(2,44) /'Growth'/
        DATA GROHEAD(3,44) /'tDW/ha'/
        DATA GROHEAD(4,44) /'G_RESP'/

        DATA GROHEAD(1,45) /'M_RESP'/
        DATA GROHEAD(2,45) /'Resp.'/
        DATA GROHEAD(3,45) /'tDW/ha'/
        DATA GROHEAD(4,45) /'M_RESP'/
		

c     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c     ~~~~~~~~ SUBROUTINE CODE ~~~~~~~~
c     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      !--- Get dynamic
      DYNAMIC = CONTROL%DYNAMIC
      
      !--- Get RUN
      RUN     = CONTROL%RUN
      
      !--- Get fileio
      FILEIO  = CONTROL%FILEIO
      
      !--- Head to dynamic
      IF (DYNAMIC.EQ.RUNINIT) THEN
          
          !---------------!
          !--- RUNINIT ---!
          !---------------!

      ELSEIF (DYNAMIC.EQ.SEASINIT) THEN
          
          !----------------!
          !--- SEASINIT ---!
          !----------------!

          CALL GET(ISWITCH)
          IDETG = ISWITCH % IDETG
          IF (IDETG .NE. 'Y') RETURN

          !-----------------------!
          !--- Create PlantGro ---!
          !-----------------------!
          
c         !--- Set file name:
          OFILE = 'PlantGro.OUT'

c         Get file unit number:
          CALL GETLUN('OUTG', NOUTDG)

c         Check that the file exists (LBYL):
          FILE_EXISTS = .FALSE.
          INQUIRE(FILE=OFILE, EXIST=FILE_EXISTS)

c         Open the file
          IF (FILE_EXISTS) THEN
c             In append mode if the file already exists
              OPEN (UNIT=NOUTDG, FILE=OFILE, STATUS='OLD',
     &        IOSTAT=ERRNUM, POSITION='APPEND')
          ELSE
c             A new file if not existing
              OPEN (UNIT=NOUTDG, FILE=OFILE, STATUS='NEW',
     &        IOSTAT = ERRNUM)
              WRITE(NOUTDG,'("*GROWTH ASPECTS OUTPUT FILE")')
          ENDIF

c         Output a header (treatment / run info, etc)
          CALL HEADER(SEASINIT, NOUTDG, RUN)

c         Output header's variable names:

c         MJ:
c         Now, there is a problem.  GBuild expects column labels
c         to be right-aligned. Fortran, so far as I can find out,
c         only supports left-aligned text, BUT always right-
c         aligns Fx.x floating point numbers.  I do not know
c         why.  I have to then create a runtime format string
c         to correctly right-align the text in the headings:

          FMT_STR = '(A5, 1X, A3, '

          DO J=3, NUM_OVARS
c             Get length of variable name
              VLEN = LEN_TRIM(GROHEAD(4, J))
c             How many spaces need to be skipped?
              SKIP = VAR_WIDTH - VLEN
c             Create strings of these:
              WRITE(SKIP_STR, '(I3)') SKIP
              WRITE(VLEN_STR, '(I3)') VLEN

c             Add to format statement:
              IF (J .EQ. NUM_OVARS) THEN
c                 If this is format info for the last variable:
                  T_FMT_STR = TRIM(SKIP_STR) // 'X,'
     &                      // 'A' // TRIM(VLEN_STR)
     &                      // ')'
              ELSE
c                 For any variable
                  T_FMT_STR = TRIM(SKIP_STR) // 'X,'
     &                      // 'A' // TRIM(VLEN_STR)
     &                      // ','
              ENDIF

              TLEN = LEN_TRIM(T_FMT_STR) + 1
              FLEN = LEN_TRIM(FMT_STR) + 1
              ENDI = FLEN + TLEN - 1


              WRITE(FMT_STR(FLEN:ENDI), '(A)') T_FMT_STR(1:TLEN)
              
          ENDDO

c         Format string for general headings:
          WRITE(WIDTH_STR, '(I3)') VAR_WIDTH-1
          G_FMT_STR = '(A5,1X,A3,1X,50(1H|, A' 
     &                 // TRIM(WIDTH_STR) // '))'

c         Loop through each row of GROHEAD, outputting
c         each column value per row
          DO I=1, 4
              DO J=1,NUM_OVARS    
                  GRO_OUT(J) = GROHEAD(I, J)  
              ENDDO
               IF (I .LT. 4) THEN

                  WRITE (NOUTDG,FMT=G_FMT_STR) 
     &                       GRO_OUT(1:NUM_OVARS)
               ELSE
c                   Write right-aligned column headings
                    WRITE (NOUTDG,FMT=FMT_STR) 
     &                       GRO_OUT(1:NUM_OVARS)
               ENDIF 
          ENDDO


c         Output format string for daily values
          D_FMT_STR = '(1X, I4, 1X, I3, 2(1X, I' 
     &                // TRIM(WIDTH_STR) // 
     &                '), 6X, A6, 10X, I2, 95(1X, F' // 
     &                TRIM(WIDTH_STR) // '.3))'
 
c     Frenquency of outputs
      FROP   = CONTROL%FROP

      ELSEIF(DYNAMIC.EQ.RATE) THEN
      
          !------------!
          !--- RATE ---!
          !------------!

      ELSEIF(DYNAMIC.EQ.INTEGR) THEN
      
          !-------------------!
          !--- INTEGRATION ---!
          !-------------------!
          
      ELSEIF(DYNAMIC.EQ.OUTPUT) THEN
          
          !--------------------!
          !--- WRITE OUTPUT ---!
          !--------------------!
      
          !--- Return if no outputs should be printed
          IF (IDETG .NE. 'Y') RETURN

c         Print daily output:
          YRDOY  = CONTROL%YRDOY

          IF (YRDOY .GE. YRPLT) THEN
              
              !--- Get time controls
              DAP = CaneCrop % dap
              DAS = CONTROL % DAS
              CALL YR_DOY(YRDOY, YEAR, DOY)

              !--------------------------------------------------
              !  Write output based on user specified frequency 
              !--------------------------------------------------

              IF ((MOD(DAS,FROP) .EQ. 0)   !Daily output every FROP days
     &            .OR. (YRDOY .EQ. YRPLT)) THEN ! and on planting date
            
                  !------------------------------!
                  !--- Write Standard Outputs ---!
                  !------------------------------!
                  
                  WRITE(NOUTDG,FMT=D_FMT_STR)                  
     &                YEAR                          , 
     &                DOY                           ,  
     &                DAS                           , 
     &                DAP                           ,
     &                CaneCrop % pltype             ,
     &                CaneCrop % nratoon            ,
     &                CaneCrop % gstd               ,
     &                CaneCrop % diac               ,    
     &                CaneCrop % dw_total           ,
     &                CaneCrop % dw_aerial          ,
     &                CaneCrop % dw_BG              ,
     &                CaneCrop % dw_it_AG           ,
     &                CaneCrop % dw_lf              ,
     &                CaneCrop % dw_rt              ,
     &                CaneCrop % fw_it_AG           ,
     &                CaneCrop % suc_it_AG          ,
     &                CaneCrop % dw_lf_dead         ,
     &                CaneCrop % dw_it_dead_AG      ,
     &                CaneCrop % pol                ,
     &                CaneCrop % lai                ,
     &                CaneCrop % nstk               ,
     &                CaneCrop % stk_h              ,
     &                CaneCrop % n_lf_AG_dewlap*1.  ,
     &                CaneCrop % swface             ,
     &                CaneCrop % swfacp             ,
     &                CaneCrop % cstat              ,
     &                CaneCrop % frac_li_pho        ,
     &                CaneCrop % frac_li_till       ,
     &                CaneCrop % rd                 ,
     &                CaneCrop % rld(1:10)          ,
     &                CaneCrop % trwup              ,
     &                CaneCrop % eop                ,             
     &                CaneCrop % dtg                ,
     &                CaneCrop % drue_calc          ,
     &                CaneCrop % tot_gresp_crop     ,
     &                CaneCrop % tot_mresp_crop
      

          ENDIF
      ENDIF
      
      ELSEIF(DYNAMIC.EQ.SEASEND) THEN
          
          !--------------------!
          !--- CLOSE OUTPUT ---!
          !--------------------!
          
          IF (IDETG .NE. 'Y') RETURN
          CLOSE(UNIT=NOUTDG)

      ENDIF

      END
