!----------------------------------------------------------------------*
! SUBROUTINE ORYZA1                                                    *
! Rice crop growth module of ORYZA2000 model                           *
!                                                                      *
! Date      : November 2002                                            *
!          Version august, 503                                        *
! History   : Adapted from ORYZA1 (1995), and ORYZA_W (1996) models    *
!             This version for release under FSEWin                    *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! ITASK   I4  Task that subroutine should perform (-)               I  *
! IUNITD  I4  Unit that can be used for input files (-)             I  *
! IUNITL  I4  Unit number for log file messages (-)                 I  *
! FILEI1  C*  Name of file with input model data (-)                I  *
! FILEIT  C*  Name of experimental data file (-)                    I  *
! OUTPUT  L4  Flag to indicate if output should be done (-)         I  *
! TERMNL  L4  Flag to indicate if simulation is to stop (-)        I/O *
! IDOY    I4  Day number within year of simulation (d)              I  *
! DOY     R4  Day number (January 1 = 1) (d)                        I  *
! TIME    R4  Time of simulation (d)                                T  *
! DELT    R4  Time step of integration (d)                          I  *
! LAT     R4  Latitude of site (dec.degr.)                          I  *
! RDD     R4  Daily shortwave radiation (J m-2 d-1)                 I  *
! TMMN    R4  Daily minimum temperature (degrees C)                 I  *
! TMMX    R4  Daily maximum temperature (degrees C)                 I  *
! NFLV    R4  N fraction in the leaves (g N m-2)                    I  *
! NSLLV   R4  N stress factor that accelerates leaf death (-)       I  *
! RNSTRS  R4  N stress reduction factor for RGRL (-)                I  *
! ESTAB   C*  Mode of establishment (-)                             I  *
! TKLT    R4  Thickness of combined soil layers (m)                 I  *
! ZRTMS   R4  Maximum rooting depth of soil (m)                     I  *
! CROPSTA I4  Crop stage (-)                                        I  *
! LRSTRS  R4  Leaf rolling stress factor (-)                        I  *
! LDSTRS  R4  Leaf death stress factor (-)                          I  *
! LESTRS  R4  Leaf expansion stress factor (-)                      I  *
! PCEW    R4  Reduction in potential transpiration rate (-)         I  *
! WCL     R4  Array of actual soil water content (m3/m3)               *
! WL0     R4  Surface water depth (cm)                              I  *
! DAE     R4  Days after emergence (d)                              O  *
! LAIROL  R4  Leaf area index rolled (ha ha-1)                      O  *
! ZRT     R4  Rooting depth (m)                                     O  *
! DVS     R4  Development stage of the crop (-)                     O  *
! LLV     R4  Loss rate of leaves (kg ha-1 d-1)                     O  *
! DLDR    R4  Death rate of leaves caused by drought (kg ha-1 d-1)  O  *
! LAIRD   R4  Deat rate of leaves casued by light shortage (kg ha-1 d-1)
! SHADET  R4  The tolerance of crop to shading (1.0, full shading, 0, no tolerance* 
! FORCEI  C   Force crop simulation to start with given values, THE FILE NAME      I  *        
! WLVG    R4  Dry weight of green leaves (kg ha-1)                  O  *
! WST     R4  Dry weight of stems (kg ha-1)                         O  *
! WSO     R4  Dry weight of storage organs (kg ha-1)                O  *
! GSO     R4  Growth rate of storage organs (kg ha-1 d-1)           O  *
! GGR     R4  Rate of increase in grain weight (kg ha-1 d-1)        O  *
! GST     R4  Growth rate of stems (kg ha-1 d-1)                    O  * 
! GLV     R4  Growth rate of leaves (kg ha-1 d-1)                   O  *
! PLTR    R4  Intermediate variable for planting density (-)        O  *
!                                                                      *
! SUBROUTINES called: PHENOL, SUBLAI3, SUBDD, SUBCD, SUBCBC, SUBGRN    *
!                     GPPARSET, SGPCDT                                 *                  
!                                                                      *
! Files included:      -                                               *
!                                                                      *
!----------------------------------------------------------------------*
      SUBROUTINE ORYZA1(ITASK,  IUNITD, IUNITL, FILEI1, FILEI2,FILEIT, &
                        OUTPUT, TERMNL, IDOY  , DOY, &
                        TIME,   DELT,   LAT,    RDD,    TMMN,   TMMX, &
                        NFLV,   NSLLV,  NRT,	RNSTRS,                 &
                        ESTAB,  TKLT,   ZRTMS,  CROPSTA, &
                        LRSTRS, LDSTRS, LESTRS, PCEW,  CPEW, TRC, &
                        DAE,    SLA, LAI,    LAIROL, ZRT,    DVS, &
                        LLV,    DLDR, WLVG, WST, WSO, GSO, GGR, GST, GLV, GRT, &
                        PLTR, WCL, WL0, WRT, WRR14, NGR, HU)

!===================================================================*
!     DECLARATION SECTION                                           *
!===================================================================*
 	  USE Public_Module		!VARIABLES
	  use RootGrowth
      use Module_OutDat
	  
      IMPLICIT NONE
 
!-----Formal parameters
      INTEGER       ITASK , IUNITD, IUNITL, CROPSTA, IDOY, I, K
      LOGICAL       OUTPUT, TERMNL
      CHARACTER (*) FILEI1, FILEIT, FILEI2
      CHARACTER (*) ESTAB
      REAL          DOY , TIME, DELT  , LAT  , RDD, TRC, RBH,RT, DIF,RBW,RSWP,RSWA, TempR, KDF2
      REAL          TMMN, TMMX, TMAXC, TMINC, TKLT  , ZRTMS, LRSTRS, LDSTRS, LESTRS, NRT
      REAL          PCEW, CPEW, DAE , LAIROL, ZRT  , DVS, NSLLV, RNSTRS, WCL(10), WL0,DAYSFL
      !DAYSFL is the days to flowering FOR PHENOLOGY STUDY
!-----Local variables
      LOGICAL DLEAF, DROUT, INQOBS  , GRAINS

      INTEGER IMX, IMXX 
      PARAMETER (IMX=40, IMXX = 800)
      INTEGER ILDRLV, ILEFFT, ILFLVT, ILFSHT, ILFSOT, ILFSTT
      INTEGER ILKDFT, ILKNFT, ILSLAT,ILNMIN,ILNMAX 
      INTEGER ILREDF, ILSSGA, ILTMCT, ILTMAXCT,ILCO2A
      REAL    DRLVT(IMX) , EFFTB(IMX), SLATB(IMX) 
      REAL    FLVTB(IMX) , FSHTB(IMX), FSOTB(IMX) , FSTTB(IMX)
      REAL    KDFTB(IMX) , KNFTB(IMX), NMINLT(IMX),NMAXLT(IMX),RFNLV
      REAL    REDFTT(IMX), SSGATB(IMX)
      REAL TMCTB(IMXX), TMINCTB(IMXX), TMAXCTB(IMXX)
	  ! TaoLi, Nov. 2010
      REAL    ALAI  , AMAX, tkl(15)
      REAL    CBCHK , CKCIN , CKCFL
      REAL    CO2   , CO2EFF, CO2LV, CO2ST, CO2STR, CO2SO , CO2REF, CO2RT
      REAL    CO2ADAY, CO2A(IMXX)  !PROVIDE DAILY CO2 CONCENTRATION AFTER EMERGENCY, CO2A CAN ONLY PROVIDE ONE YEAR DATA
      REAL    CRGCR , CRGLV , CRGRT, CRGSO, CRGST , CRGSTR, CTRANS 
      REAL    DAYL  , DAYLP , DLDR , DLDRT, DPAR  , DPARI , DTGA  , DTR
      REAL    DVEW  , DVR   , DVRI , DVRJ , DVRP  , DVRR  , DVSI 
      REAL    EFF   , FCLV  , FCRT , FCSO , FCST  , FCSTR , FLV   , FSH
      REAL    FRPAR , FSO   , FRT  , FST  , FSTR
      REAL    GCR   , GGR   , GLAI , GLV  , GNGR  , GNSP  , GRT   , GRT1
	  REAL    GSO   , GST   , GST1 , GSTR , GZRT  , CTSTER
      REAL    HU    , HULV 
      REAL    KEEP  , KDF   , KNF     , LAI, AMaxSLN0, MinSLN
      REAL    LAPE  , LLV   , LRSTR   , LSTR
      REAL    MAINLV, MAINRT, MAINSO  , MAINST, MNDVS , MOPP
      REAL    NCOLD , NFLV  , NGCR  , NGR   , NGRM2
      REAL    NH    , NPLDS , NPLH    , NPLSB , NSP   , NSPM2
      REAL    PARCM1, PARCUM, PARI1   , PLTR  , PPSE  , PWRR  , Q10
      REAL    RAPCDT, RDAE  , REDFT , RGCR    , RGRL  , RMCR  , RTNASS, RWLVG 
      REAL    RWLVG1, RWSTR , RWSTR1, SUMROOTN
      REAL    SAI   , SCP   , SF1     , SF2   , SHCKD , SHCKL , SLA  
      REAL    SPFERT, SPGF  , SSGA 
      REAL    TAV   , TAVD  , TBD     , TBLV  , TCLSTR, TCOR  , TDRW
      REAL    TEFF  , TMAX  , TMIN    , TMPCOV, TMPSB , TMD   , TNASS 
      REAL    TOD   , TS    , TSHCKD  , TSHCKL, TSLV  , TREF  
      REAL    WAG   , WAGT  , WGRMX   , WLV   , WLVG  , WLVGI , WLVGIT
      REAL    WLVD  , WRR   , WRR14   , WRT   , WRTI  , WST   , WSTI  
      REAL    WSTRI , WLVDI 
      CHARACTER*128 FORCEI  !FORCE CROP START WITH GIVEN VALUES, THEN GIVEN INITAL FILE FULL NAME
      REAL    WSO   , WSOI  , WSTS    , WSTR  , WSO_O !TAOLI 19May 2012 
      REAL    ZRTI  , ZRTM  , ZRTTR   , ZRTMCW, ZRTMCD, RGRLMX, RGRLMN
      REAL    ASLA  , BSLA  , CSLA    , DSLA  , SLAMAX, LAIRD, FCLIMAT
      REAL    SHADET  !Index of crop tolerate shade, 0.5 is the default, 0 to 1 increasing tolerance
      INTEGER   SWIRTR 
      REAL    COLDMIN,COLDEAD, FSWTD,  LLRT, ULRT,SWIRTRF,COLDREP,ULLS
!-----TAOLI, JUNE 10, 509
	  REAL    SHOUR,  EHOUR,  TTEMP,    TCHANG,	SDAY,	EDAY
	  REAL    xTAV,   xTMIN,   xTMAX,   xHU, xTAVD !OUTPUT, THE NET CHANGE
	  INTEGER ISTEMC, CONTRM, sl
	  logical RDINQR, TEMPC, INITIALF				
!-----TAOLI, 6 AUG, 2010
	  REAL SFLOWER, IFLOWER, TFLOWER
	  real TrootD  !root depth at optimal condition
	  CHARACTER ROOTOBS*10, xx*2			!Use for root observation outputs
      CHARACTER (10) SWISLA
	  CHARACTER (2) TMPCH1, TMPCH2

      REAL CHECKTB, TSTCHK, TWRT, LROOTC,LROOTN, XXX
      !--- ONE VARIABLE IS ADDED FOR OUTPUT ROOT MASS IF THE OBSERVATION EXISTED, TAOLI 23 FEB2012
      INTEGER ROOTCOB(15)
	  LOGICAL ISOPEN
		REAL TEMPV(15), Y1, Y2

      REAL    LINT2, INSW, NOTNUL, GETOBS, INTGRL, INTGR2
      SAVE         ! TAOLI
 
!===================================================================*
!     INITIALIZATION SECTION                                        *
!===================================================================*
      IF (ITASK.EQ.1) THEN

!     WRITE (*,*) 'SUCCESS'

!--------Open experimental data input file
         CALL RDINIT (IUNITD, IUNITL, FILEIT)
!        Read initial states
         IF(RDINQR('FORCEI')) THEN
            CALL RDSCHA('FORCEI',FORCEI)
            IF(LEN_TRIM(FORCEI).GT.2) THEN
                INITIALF = .TRUE.
            ELSE
                INITIALF = .FALSE.
            END IF
         ELSE
            INITIALF = .FALSE.
         END IF
         
         CALL RDSREA('LAPE  ',LAPE )
         CALL RDSREA('DVSI  ',DVSI )
         CALL RDSREA('WLVGI ',WLVGI)
         CALL RDSREA('WRTI  ',WRTI )
         CALL RDSREA('WSOI  ',WSOI )
         CALL RDSREA('WSTI  ',WSTI )
         CALL RDSREA('ZRTI  ',ZRTI )
         CALL RDSREA('ZRTTR ',ZRTTR)
         
!--------TAOLI, June 10, 509
		 IF(RDINQR('ISTEMC')) THEN
			CALL RDSINT('ISTEMC',ISTEMC )
			IF(ISTEMC.GT.0) THEN		 
				TEMPC = .TRUE.
			ELSE
				TEMPC = .FALSE.
			ENDIF
			IF(TEMPC) THEN
			    CALL RDSREA('TTEMP ',TTEMP  )
			    CALL RDSREA('TCHANG',TCHANG )
			    CALL RDSREA('SHOUR ',SHOUR  )
			    CALL RDSREA('EHOUR ',EHOUR  )
			    CALL RDSREA('SDAY  ',SDAY   )
			    CALL RDSREA('EDAY  ',EDAY   )
			    CALL RDSINT('CONTRM',CONTRM )
			 ENDIF
		 ENDIF
		 
!        Read management parameters
         CALL RDSCHA ('ESTAB'   , ESTAB  )
         CALL UPPERC (ESTAB  )
         IF (ESTAB.EQ.'TRANSPLANT') THEN
            CALL RDSREA('NH    ',NH   )
            CALL RDSREA('NPLH  ',NPLH )
            CALL RDSREA('NPLSB ',NPLSB)
         ELSE IF (ESTAB.EQ.'DIRECT-SEED') THEN
            CALL RDSREA('NPLDS ',NPLDS)
         END IF
         CALL RDSREA('TMPSB ',TMPSB)
         ILTMCT = 0; ILTMAXCT = 0
		 IF(RDINQR('TMCTB')) THEN
			CALL RDAREA('TMCTB ',TMCTB,IMXX,ILTMCT)
		 ELSEIF(RDINQR('TMAXCTB')) THEN
			CALL RDAREA('TMINCTB ',TMINCTB,IMXX,ILTMAXCT)
			CALL RDAREA('TMAXCTB ',TMAXCTB,IMXX,ILTMAXCT)
		 ENDIF
		 IF(RDINQR('CO2A')) THEN
		    CALL RDAREA('CO2A',CO2A,IMXX,ILCO2A)
		 ELSE
		    ILCO2A = 0
		 END IF
!--------Close experimental data input file
         CLOSE (IUNITD)

!--------Open crop input file
         CALL RDINIT(IUNITD,IUNITL,FILEI1)
!        Read model parameters
         CALL RDSREA('FRPAR ',FRPAR )
         CALL RDSREA('CO2   ',CO2   )
         CALL RDSREA('CO2REF',CO2REF)
         CALL RDSREA('CRGLV ',CRGLV )
         CALL RDSREA('CRGRT ',CRGRT )
         CALL RDSREA('CRGSO ',CRGSO )
         CALL RDSREA('CRGST ',CRGST )
         CALL RDSREA('CRGSTR',CRGSTR)
         CALL RDSREA('DVRI  ',DVRI  )
         CALL RDSREA('DVRJ  ',DVRJ  )
         CALL RDSREA('DVRP  ',DVRP  )
         CALL RDSREA('DVRR  ',DVRR  )
         IF(RDINQR('COLDREP')) THEN
		    CALL RDSREA('COLDREP', COLDREP)			!special root length cm/g C
		 ELSE
		    COLDREP = 22.0
		 END IF
         CALL RDSREA('FCLV  ',FCLV  )
         CALL RDSREA('FCRT  ',FCRT  )
         CALL RDSREA('FCSO  ',FCSO  )
         CALL RDSREA('FCST  ',FCST  )
         CALL RDSREA('FCSTR ',FCSTR )
         CALL RDSREA('FSTR  ',FSTR  )
         CALL RDSREA('LRSTR ',LRSTR )
         CALL RDSREA('MAINLV',MAINLV)
         CALL RDSREA('MAINRT',MAINRT)
         CALL RDSREA('MAINSO',MAINSO)
         CALL RDSREA('MAINST',MAINST)
         CALL RDSREA('MOPP  ',MOPP  )
         CALL RDSREA('PPSE  ',PPSE  )
         CALL RDSREA('Q10   ',Q10   )
         CALL RDSREA('RGRLMX',RGRLMX)
         CALL RDSREA('RGRLMN',RGRLMN)
         CALL RDSREA('SCP   ',SCP   )
         CALL RDSREA('SHCKD ',SHCKD )
         CALL RDSREA('SHCKL ',SHCKL )
         CALL RDSREA('SPGF  ',SPGF  )
         CALL RDSREA('TBD   ',TBD   )
         CALL RDSREA('TBLV  ',TBLV  )
         CALL RDSREA('TCLSTR',TCLSTR)
         CALL RDSREA('TMD   ',TMD   )
         CALL RDSREA('TOD   ',TOD   )
         CALL RDSREA('COLDMIN',COLDMIN)
         CALL RDSREA('COLDEAD',COLDEAD)
		 CALL RDSREA('CTSTER' ,CTSTER)
         CALL RDSREA('TREF  ',TREF  )
         CALL RDSREA('WGRMX ',WGRMX )
         CALL RDSREA('ZRTMCW',ZRTMCW)
         CALL RDSREA('GZRT  ',GZRT  )
!--------ADD BY TAOLI, JUNE 17
         IF(RDINQR('SROOTL')) THEN
		    CALL RDSREA('SROOTL', SROOTL)			
		 ELSE
		    SROOTL = 90.0
		 END IF
         CALL RDSREA('RMINT',  RMINT)			
         CALL RDSREA ('ROPTT', ROPTT)			
         CALL RDSREA ('RTBS',  RTBS)				
         CALL RDSREA('RFNLV', RFNLV)
         CALL RDAREA('NMAXLT',NMAXLT,IMX,ILNMAX)
         IF(RDINQR('RCNL')) THEN
            CALL RDSREA('RCNL',   RCNL)				
         ELSE
            RCNL = RFNLV            
         END IF			
		 IF(RDINQR('MAXD')) THEN
		    CALL RDSREA('MAXD  ', MAXD)				
		    ZRTMCD = MAXD/100.0
		 ELSEIF(RDINQR('ZRTMCD')) THEN
         CALL RDSREA('ZRTMCD',ZRTMCD)
		    MAXD = ZRTMCD *100.0
		 ELSE
		    CALL FATALERR ('ROOTG','Missing the information for maximum rooting depth!') 
		 END IF
		 CALL RDSREA('SODT  ', SODT)             
		IF(RDINQR('SFLOWER')) THEN
			CALL RDSREA('SFLOWER',SFLOWER)
		ELSE
			SFLOWER=9.0
		ENDIF
		IF(RDINQR('IFLOWER')) THEN
			CALL RDSREA('IFLOWER',IFLOWER)
		ELSE
			IFLOWER=5.0
		ENDIF
		IF(RDINQR('TFLOWER')) THEN
			CALL RDSREA('TFLOWER',TFLOWER)
		ELSE
			TFLOWER=34.0
		ENDIF
		!Following parameter for shading tolerance index
		IF(RDINQR('SHADET')) THEN
			CALL RDSREA('SHADET',SHADET)
		ELSE
			SHADET=0.5
		ENDIF
		SHADET = max(0.01,min(0.99,SHADET)) !limited SHADET between 0.01 and 0.99
		!TAOLI, 31 oct 2012
        CALL RDSREA ('ULLS', ULLS)
		CALL RDSINT('SWIRTR',SWIRTR)
		IF(SWIRTR.EQ.1) THEN
             CALL RDSREA ('LLRT', LLRT)
             CALL RDSREA ('ULRT', ULRT)
             FSWTD =max(0.1,1.0+(LOG10(LLRT)-LOG10(10.0))/(LOG10(ULRT)-LOG10(LLRT)))
		 ELSEIF(SWIRTR.EQ.2) THEN		
			IF(RDINQR('SWIRTRF')) THEN
				CALL RDSREA('SWIRTRF',SWIRTRF)  
			ELSE
				SWIRTRF=0.003297			
			ENDIF
			FSWTD = INT(2.537**(2.0/(1.0+EXP(SWIRTRF*30.0)))*100.0)/100.0
		 ELSEIF(SWIRTR.EQ.3) THEN
		IF(RDINQR('FSWTD')) THEN
			    CALL RDSREA('FSWTD', FSWTD)  !default value is 0.41
			IF(FSWTD.GT.0.0) THEN
			    FSWTD = 1.0/FSWTD
			ELSE
			        FSWTD = 2.42
			    END IF
		     ELSE
			    FSWTD = 2.42
			ENDIF    
		 ENDIF
		 IF(RDINQR('AMAXSLN0')) THEN
		    CALL RDSREA('AMAXSLN0',AMaXSLN0)
		 ELSE
		    AMAXSLN0 = 22.0
		 END IF
		 IF(RDINQR('MINSLN')) THEN
		    CALL RDSREA('MINSLN',MINSLN)
		 ELSE
		    MINSLN =0.2
		 END IF
!-------END SECTION
!        Read tables
         CALL RDAREA('REDFTT',REDFTT,IMX,ILREDF)
         CALL RDAREA('EFFTB ',EFFTB ,IMX,ILEFFT)
         CALL RDAREA('KDFTB ',KDFTB ,IMX,ILKDFT)
         CALL RDAREA('KNFTB ',KNFTB ,IMX,ILKNFT)
         CALL RDAREA('FSHTB ',FSHTB ,IMX,ILFSHT)
         CALL RDAREA('FLVTB ',FLVTB ,IMX,ILFLVT)
         CALL RDAREA('FSTTB ',FSTTB ,IMX,ILFSTT)
         CALL RDAREA('FSOTB ',FSOTB ,IMX,ILFSOT)
         CALL RDAREA('DRLVT ',DRLVT ,IMX,ILDRLV)
         CALL RDAREA('SSGATB',SSGATB,IMX,ILSSGA)
         CALL RDSCHA('SWISLA',SWISLA)
         CALL UPPERC (SWISLA)
         IF (SWISLA .EQ. 'TABLE') THEN
             CALL RDAREA('SLATB ',SLATB ,IMX,ILSLAT)
         ELSE IF (SWISLA .EQ. 'FUNCTION') THEN
            CALL RDSREA('ASLA',ASLA)
            CALL RDSREA('BSLA',BSLA)
            CALL RDSREA('CSLA',CSLA)
            CALL RDSREA('DSLA',DSLA)
            CALL RDSREA('SLAMAX',SLAMAX)
         ELSE 
            CALL FATALERR ('Crop data file','Unknown name for SWISLA')
         END IF

! (BB: MAY 2003): Check on input data validity
         CHECKTB = 0. 
         DO TSTCHK = 0.,2.,0.1
            CHECKTB = LINT2('FLVTB',FLVTB,ILFLVT,TSTCHK) + &
                      LINT2('FSTTB',FSTTB,ILFSTT,TSTCHK) + &
                      LINT2('FSOTB',FSOTB,ILFSOT,TSTCHK)
            IF (CHECKTB .GT. 1.01 .OR. CHECKTB .LE. 0.99) THEN
               CALL FATALERR ('Crop data file','FLV, FST and FSO do not add up to 1.')
            END IF
         END DO

!--------Close crop data input file
         CLOSE (IUNITD)

!--------Added by TAOLI, June 17, 2009
	    IF(PV%PROOT_NUTRIENT) THEN
		     CALL RDINIT(IUNITD, IUNITL, FILEI2)
		     CALL RDSINT('NL', PV%PNL)
		     CALL RDAREA('TKL',TKL    ,10, SL)   		
		     SL = PV%PNL
		     IF(RDINQR('SANDX')) THEN
			     CALL RDAREA('SANDX',PV%PSAND ,10, SL)		!soil sand content
			     CALL RDAREA('CLAYX',PV%PCLAY ,10, SL)		!soil clay content
			     PV%PROOT_NUTRIENT = .TRUE.
		     ELSE
			     PV%PROOT_NUTRIENT = .FALSE.
		     ENDIF
		     CALL RDAREA('BD',PV%PBD    ,10, SL)
             IF(RDINQR('PLOWPAN')) THEN
			          CALL RDSREA('PLOWPAN', PV%PPLOWDEPTH)			!THE DEPTH OF PLOWPAN, NEGATIVE VALUE INDICATES NO PLOWPAN
		        ELSE
			          PV%PPLOWDEPTH = -1.
		        ENDIF
		    CLOSE (IUNITD)
		 END IF
		 !FOLLOWING SECTION IS ADDED FOR ROOT MASS OUTPUT, TAOLI 23FEB 2012
		 CALL RDINIT (IUNITD, IUNITL, FILEIT)
		    DO i=1, sl
				Rootobs = ' '
				write(xx,'(I2)') I
				rootobs = trim('ROOTM') // trim(adjustl(xx))//"_OBS"
				IF (RDINQR(trim(rootobs))) THEN 
				    ROOTCOB(I) = 1
				ENDIF
			ENDDO
		 CLOSE(IUNITD)
		 !END THE SECTION
		!---- INITIAL ROOT VARIABLES
			RRCC=0.0;RRNC=0.0;RRDCL=0.0;RRDENSIT=0.0;RRDNL=0.0
			MaxDep = ZRTMCD *100.0  !From m to cm
			ROOTN=0.0;ROOTC=0.0;RDCL=0.0; RDNL=0.0;RDENSITY=0.0
			NROOTC=0.0; NROOTN=0.0
			REFFECD=0.0; REFCD_O=0.0; ROOTNC=0.0
        
!-------END THE SECTION, TAOLI, JUNE 17, 509
!--------Initialize state variables
         DVS    = 0.
         PARCUM = 0.
         PARCM1 = 0.
         WLVG   = 0.01
         WLVD   = 0.
         WSTS   = 0.01
         WSTR   = 0.
         WSO    = 0.
         WSO_O = 0.
         WRT    = 0.
         WST    = WSTS + WSTR
         WLV    = WLVG + WLVD
         WAG    = WLVG + WST  + WSO
         WAGT   = WLV  + WST  + WSO
         TDRW   = WLV  + WST  + WSO + WRT
         WRR    = 0.
         WRR14  = 0.
         PWRR   = 0.
         NGR    = 0.
         NSP    = 0.
         DAE    = -1.0      !0.
         TS     = 0.
         TMAXC  = 0.
         TMINC  = 0.
         TSLV   = 0.
         TNASS  = 0.
         WLVGIT = 0.
         LAI    = 0.
         LAIROL = 0.
         DVR    = 0.
         TSHCKD = 0.
         TSHCKL = 0.
         NSPM2  = 0.
         NGRM2  = 0.
         HU     = 0.
         HULV   = 0.
         NCOLD  = 0.
         ZRT    = 0.
         DLDRT  = 0.
         DLDR   = 0.
         KEEP   = 0.
         NSP    = 0.
         GNSP   = 0.
         DLEAF    = .FALSE.
         DROUT    = .FALSE.
         GRAINS   = .FALSE.
         FCLIMAT = MIN(1.0, MAX(0.0, 1.0-0.1*(ABS(LAT)-15.0)/15.0))
         CO2ADAY = 0.0
         DAYSFL = -1.0
         SUMROOTN = 0.0
!		TAOLI, JULY 23, 2013
        IF(INITIALF) THEN
            CALL RDINIT (IUNITD, IUNITL, FORCEI)
            CALL RDSREA('WSTRI  ',WSTR )
            CALL RDSREA('WSTSI  ',WSTS )
            CALL RDSREA('WLVDI  ',WLVD )
            CALL RDSINT('CROPSTAI',CROPSTA)
            CALL RDSREA('LAPE  ',LAI )
            CALL RDSREA('DVSI  ',DVS )
            CALL RDSREA('WLVGI ',WLVG)
            CALL RDSREA('WRTI  ',WRT )
            CALL RDSREA('WSOI  ',WSO )
            CALL RDSREA('WSTI  ',WST )
            CALL RDSREA('WRRI  ',WRR )
            CALL RDSREA('WRR14I',WRR14)
            CALL RDSREA('NGRI  ',NGR )
            CALL RDSREA('NSPI  ',NSP )
            CALL RDSREA('ZRTI  ',ZRT )
            CALL RDSREA('ZRTTR ',ZRTTR)
            CALL RDAREA('ROOTCI',ROOTC, 10, SL)
	        CALL RDAREA('ROOTNI',ROOTN, 10, SL)
	        CALL RDSREA('DAEI',DAE) 
            CALL RDSREA('PARCUMI',PARCUM)         

             WSO_O = WSO
             WST    = WSTS + WSTR
             WLV    = WLVG + WLVD
             WAG    = WLVG + WST  + WSO
             WAGT   = WLV  + WST  + WSO
             TDRW   = WLV  + WST  + WSO + WRT
             PWRR   = NGR*WGRMX              
            IF(WRR.GT.0.0) THEN
                GRAINS = .TRUE.
            ELSE
                GRAINS   = .FALSE.
            END IF      
         END IF
!===================================================================*
!     RATE CALCULATION SECTION                                      *
!===================================================================*
      ELSE IF (ITASK.EQ.2) THEN
	!--------Re-initialize weights and LAI at day of emergence
         IF (CROPSTA .EQ. 1) THEN
            DVS  = DVSI
            WLVG = WLVGI
            WLVD = 0.
            WSTS = WSTI
            WSTR = 0.
            WST  = WSTS+WSTR
            WSO  = WSOI
            WRT  = WRTI
            ZRT  = ZRTI
			IROOTD = ZRTI			!BY TAOLI, 22 JUNE, 2011
			DAE = 0.0               !BY TAOLI, 5 APRIL 2011
			ROOTN=0.0;ROOTC=0.0
            IF (ESTAB.EQ.'TRANSPLANT' ) LAI= LAPE * NPLSB
            IF (ESTAB.EQ.'DIRECT-SEED') LAI= LAPE * NPLDS
         END IF

!--------Re-initialize rooting depth at day of transplanting
         IF (CROPSTA .EQ. 3) THEN
            ZRT = ZRTTR
			IROOTD = ZRTTR			!BY TAOLI, 22 JUNE, 509
         END IF

!=======SKIP ALL RATE CALCULATIONS BEFORE EMERGENCE
        IF (CROPSTA .GE. 1) THEN
!----------Set DROUT when leaf expansion is reduced in the
!          vegetative growth phase (=> extra root growth)
           IF ((DVS.LT. 1).AND.(LESTRS.LT.1.)) THEN
              DROUT = .TRUE.
           ELSE
              DROUT = .FALSE.
           END IF
           SUMROOTN = SUM(ROOTN) + SUM(RDNL)
            IF (CROPSTA .EQ. 3) SUMROOTN =0.0
!----------Computation of weather variables
           IF (CROPSTA .LE. 2) THEN
              TMPCOV = TMPSB
           ELSE
              TMPCOV = 0.
           END IF
		   IF((ILTMCT.GT.0).and.(ILTMAXCT.LE.0)) THEN
				TCOR = LINT2('TMCTB',TMCTB,ILTMCT,DOY)
				TMAX = TMMX+TCOR+TMPCOV
				TMIN = TMMN+TCOR
			ELSEIF((ILTMAXCT.GT.0).AND.(ILTMCT.LE.0)) THEN	!Added by TaoLi, 13 Oct. 2010
				TCOR = LINT2('TMAXCTB',TMAXCTB,ILTMAXCT,DOY)
				TMAX = TMMX+TCOR+TMPCOV
				TCOR = LINT2('TMINCTB',TMINCTB,ILTMAXCT,DOY)
				TMIN = TMMN+TCOR		!Added by TaoLi, 13 Oct. 2010
			ENDIF
           TAV  = (TMIN+TMAX)/2.
           TAVD = (TMAX+TAV )/2.
!----------TAOLI, June 10, 2009
!----------We supposed the TMAX would not change
			 IF((TEMPC).AND.(DOY.GE.SDAY).AND.(DOY.LE.EDAY)) THEN
				 CALL TSHIFT(TMAX,TMIN,DAYL, SHOUR, EHOUR, TTEMP, TCHANG, &
					ISTEMC, CONTRM, TBD,TOD,TMD,HU, xTAV, xTMIN, xTMAX, xHU,xTAVD)
			 ELSE
						   !----------Phenological development 
				 CALL SUBDD (TMAX,TMIN,TBD,TOD,TMD,HU)
				 xTAV=0.0; xTMIN=0.0; xTMAX=0.0; xHU=0.0; xTAVD=0.0
			 ENDIF
!----------The net change has been calculated here, so recalculate TAV and TAVD
		    TAV = TAV + XTAV
		    TAVD = TAVD + XTAVD

!----------HU has been calculated in TSHIFT, TAOLI, JUNE 10, 509
!----------UPDATE HU, TAOLI, JUNE 10, 509
!		   WRITE(*,*) DOY, TAV, XTAV, HU, XHU, XTAVD		
!----------END, TAOLI, JUNE 19, 509
           DTR  = RDD

!----------Counter for days after emergence
           RDAE = 1.
		       HU=HU+XHU
           CALL SUBCD2 (COLDMIN,CROPSTA,TAV,TIME,NCOLD)
           CALL OR_PHENOL(ESTAB,DVS,DVRJ,DVRI,DVRP,DVRR,HU,DAYL,MOPP,PPSE, &
                        TS,SHCKD,CROPSTA,DVR,TSHCKD)


!----------Effect of drought stress on development rate
           IF (DVS.LT.1.0) THEN
! BB: REMOVE THIS; IT CAN TAKE MORE THAN 1 YEAR TO COMPLETE A CROP CYCLE!!
!              DVEW = LESTRS + (DVS*(1.-LESTRS))
              DVEW = 1.
           ELSE IF (DVS.GE.1.) THEN
              DVEW = 1.
           END IF
           DVR = DVR*DVEW

!----------CO2 concentration
           IF(ILCO2A.GT.1) THEN
               CO2ADAY = LINT2('CO2A',CO2A,ILCO2A,DAE)
               IF(CO2ADAY.LE.0.0) THEN
                  CO2ADAY = CO2
               ENDIF
           ELSE
               CO2ADAY = CO2
           END IF           
           !CO2EFF = (1.-EXP(-0.00305*CO2ADAY   -0.222))  &
           !        /(1.-EXP(-0.00305*CO2REF-0.222))      !Commented out by TAOLI 13 Nov 2013
           CO2EFF = (1.-EXP(-0.00305*CO2ADAY   -1.022))  &
                   /(1.-EXP(-0.00305*CO2REF-1.022))       !To reduce the effects of CO2, TAOLI 13Nov 2013
           EFF = LINT2('EFFTB',EFFTB,ILEFFT,TAVD)*CO2EFF
 
!----------Leaf rolling under drought stress (only for photosynthesis)
           LAIROL = LAI*(0.5*LRSTRS+0.5)

!--------- Add specific stem area to leaf area
           SSGA = LINT2('SSGATB',SSGATB,ILSSGA,DVS)
           SAI  = SSGA*WST
           ALAI   = LAIROL+0.5*SAI

!----------Intercepted solar radiation
           KDF   = LINT2('KDFTB' ,KDFTB,ILKDFT,DVS)
           REDFT = LINT2('REDFTT',REDFTT,ILREDF,TAVD)
           KNF   = LINT2('KNFTB' ,KNFTB,ILKNFT,DVS)

!----------Daily gross canopy CO2 assimilation (DTGA)
           CALL GPPARSET (CO2ADAY, KNF, NFLV, REDFT, AMaxSLN0, MinSLN)
!----------The value 2 in next argument list: accuracy for Gauss integration over canopy. 
!          If value=1 => 3-points Gauss over canopy (as in TOTASP); of value = 2 => 
!          enhanced accuracy if required as detrmined within the subroutine TRY!
           CALL SGPCDT   (1, IDOY , LAT   , DTR  , FRPAR, &
                          SCP, AMAX , EFF   , KDF, ALAI , &
                          DAYL, DAYLP, DTGA, RAPCDT)
           PARI1 = RAPCDT/1.E6
           DPARI = RAPCDT/1.E6
           DPAR  = FRPAR*DTR/1.E6

!----------Unrolling of ALAI again 
           ALAI  = LAI+0.5*SAI
!----------dded by TAOLI, 25 Mar 2010
			 KDF2=KDF
			 IF(TRC.GT.0.0) THEN
!				DIF = PV%PDT
			 ELSE
				DIF=0.0
			 ENDIF
		   if(PV%PROOT_NUTRIENT) then
			   TEMPR = PCEW
		     DTGA  = DTGA*min(RNSTRS,pcew)     					
    	   Else
		     DTGA  = DTGA*min(RNSTRS,pcew)   
		   Endif


!----------Relative growth rates of shoots and roots
!          Effect of drought stress on shoot-root partitioning ???? 
!BB: Changed according to SUCROS2
					 !RELATIVE GROWTH RATE OF SHOOT
           FSH = LINT2('FSHTB',FSHTB,ILFSHT,DVS)
           !----------Relative growth rates of shoot organs
           FLV = LINT2('FLVTB',FLVTB,ILFLVT,DVS)
           FST = LINT2('FSTTB',FSTTB,ILFSTT,DVS)
           FSO = LINT2('FSOTB',FSOTB,ILFSOT,DVS)
		   IF(PV%PROOT_NUTRIENT) then		        
				CALL PARTITION_K(KDF2,LAI,MAX(0.0, -LOG(6.9*DAYLP*3600.0/(0.5*RDD))/KDF),FCLIMAT, &
				                PCEW,RNSTRS,fswtd,1,FSH,FRT,FLV,FST,FSO)  !ADDED BY TAOLI, APRIL 29, 2010
		   ELSE
				IF (DVS.LT.1.) THEN					
					FSH  = (FSH*CPEW)/NOTNUL((1.+(CPEW-1.)*FSH))
				END IF
				FRT = 1.-FSH
		   ENDIF

       IF (GRAINS) THEN

  			 IF(GGR.GE.(PWRR-WRR))THEN
			  		FSO = MAX(0.,(PWRR-WRR)/NOTNUL((GCR*FSH)))
					FST = 1.-FSO-FLV
				 ENDIF
		   END IF
!           TAOLI, 22Feb 2012
!----------Loss rates of green leaves and stem reserves
           LLV  = NSLLV*WLVG*LINT2('DRLVT',DRLVT,ILDRLV,DVS)
           LSTR = INSW(DVS-1.,0.,WSTR/TCLSTR)
 
!----------Maintenance requirements
           TEFF = Q10**((TAV-TREF)/10.)
           MNDVS = WLVG/NOTNUL(WLVG+WLVD)
           RMCR  = (WLVG*MAINLV+WST*MAINST+WSO*MAINSO+WRT*MAINRT) &
                   *TEFF*MNDVS

!----------Carbohydrate requirement for dry matter production (growth respiration)
           CRGCR = FSH*(CRGLV*FLV+CRGST*FST*(1.-FSTR)+CRGSTR*FSTR*FST+ &
                       CRGSO*FSO)+CRGRT*FRT 
		  
!----------Gross and net growth rate of crop (GCR, NGCR)
           GCR   =((DTGA*30./44.)-RMCR+(LSTR*LRSTR*FCSTR*30./12.))/NOTNUL(CRGCR)
           NGCR  = MAX(0.,GCR-LSTR*LRSTR*FCSTR*30./12.)

!----------Set transplanting effect
           IF (CROPSTA .EQ. 3) THEN
             PLTR = NPLH*NH/NPLSB
           ELSE
             PLTR = 1.
           END IF

!----------Growth rates of crop organs at transplanting
           RWLVG1 = (WLVG*(1.-PLTR))/DELT
           GST1   = (WSTS*(1.-PLTR))/DELT
           RWSTR1 = (WSTR*(1.-PLTR))/DELT
           GRT1   = (WRT *(1.-PLTR))/DELT
 
!----------Growth rates of crop organs
           GRT    = GCR*FRT-GRT1
           GLV    = GCR*FSH*FLV-RWLVG1
           RWLVG  = GLV-LLV
           GST    = GCR*FSH*FST*(1.-FSTR)-GST1
           GSTR   = GCR*FSH*FST*FSTR-RWSTR1
           RWSTR  = GSTR-LSTR
           GSO    = GCR*FSH*FSO

           IF (DVS.GT.0.95) THEN
              GGR = GSO
           ELSE 
              GGR = 0.;WRR = 0.0
           END IF
		  
           !root growth section was removed to ITASK = 3, TAOLI, 17OCT 2013

!----------Growth rate of number of spikelets and grains
 		   CALL SUBGRN(GCR,CROPSTA,LRSTRS,DVS,SF2,SF1,SPGF,TAV,TMAX, &
						NSP,CTSTER,COLDREP,GNSP,GNGR,SPFERT,GRAINS)
!!		   CALL SUBGRN3(GCR,CROPSTA,LRSTRS, DVS,SF2,SF1,SPGF,TAV,TMAX, &
!!						SFLOWER,IFLOWER,TFLOWER,NSP,GNSP,GNGR, &
!!                        SPFERT,GRAINS)
!--------- Leaf area growth (after calculation on leaf growth and loss rates!)
!----------USE NIGHT.F90 TO CALCULATE XHU, THEN RECALCULATE HULV
			 IF((TEMPC).AND.(DOY.GE.SDAY).AND.(DOY.LE.EDAY)) THEN
				 CALL TSHIFT(TMAX,TMIN,DOY, SHOUR, EHOUR, TTEMP, TCHANG, &
					ISTEMC, CONTRM, TBLV,30.,42.,HULV, xTAV, xTMIN, xTMAX, xHU, XTAVD)
			 ELSE
!----------Temperature sum for leaf development
				 CALL SUBDD (TMAX,TMIN,TBLV,30.,42.,HULV)    !TAOLI, JUNE 10, 509 
				 xTAV=0.0; xTMIN=0.0; xTMAX=0.0; xHU=0.0; XTAVD=0.0
			 ENDIF
		    HULV = HULV + XHU	

!----------Specific leaf area
           IF (SWISLA .EQ. 'TABLE') THEN
              SLA  = LINT2('SLATB',SLATB,ILSLAT,DVS)
           ELSE
           SLA = ASLA + BSLA*EXP(CSLA*(DVS-DSLA))
           SLA = MIN(SLAMAX, SLA)
           END IF

! BB: NEW LAI ROUTINE
!----------Leaf area index growth
           CALL SUBLAI3(CROPSTA,RGRLMX,RGRLMN,TSLV,HULV, &
                          SHCKL,LESTRS,RNSTRS,SLA,NH,NPLH,NPLSB,DVS,LAI, &
                          ESTAB,RWLVG,DLDR,WLVG,GLAI,RGRL)

!----------Leaf death as caused by drought stress
           DLDR = 0.
		   
           IF (LDSTRS.EQ.1.) THEN
               DLEAF = .FALSE.
               DLDRT = 0.
           END IF
           IF ((LDSTRS.LT.1.).AND.(.NOT.DLEAF)) THEN
              WLVGIT = WLVG
              DLEAF  = .TRUE.
              KEEP   = LDSTRS
		   ELSEIF(WLVG.LE.0.0) THEN		!!Added by TaoLi, Jan4, 2011
			  WLVGIT = WLVG				!!Added by TaoLi, Jan4, 2011
              DLEAF  = .FALSE.			!!Added by TaoLi, Jan4, 2011
              KEEP   = LDSTRS			!!Added by TaoLi, Jan4, 2011
           END IF
           IF (DLEAF) THEN
              IF (LDSTRS.LE.KEEP) THEN
                 DLDR  = (WLVGIT/DELT)*(1.-LDSTRS)-DLDRT/DELT
				 !TAOLI, 27 Aug, 2010
				 DLDR = MIN((WLVGIT+RWLVG*DELT)/DELT,DLDR)
                 KEEP  = LDSTRS
                 DLDRT = DLDR*DELT+DLDRT
              END IF
           END IF

!----------Growth respiration of the crop (RGCR)
           CO2RT  = 44./12.*(CRGRT *12./30.-FCRT )
           CO2LV  = 44./12.*(CRGLV *12./30.-FCLV )
           CO2ST  = 44./12.*(CRGST *12./30.-FCST )
           CO2STR = 44./12.*(CRGSTR*12./30.-FCSTR)
           CO2SO  = 44./12.*(CRGSO *12./30.-FCSO )
 
           RGCR = (GRT+GRT1)*CO2RT + (GLV+RWLVG1)*CO2LV +  &
                  (GST+GST1)*CO2ST + GSO*CO2SO+(GSTR+RWSTR1)*CO2STR+ &
                  (1.-LRSTR)*LSTR*FCSTR*44./12.
 
           CTRANS = RWLVG1*FCLV+GST1*FCST+RWSTR1*FCSTR+GRT1*FCRT
           RTNASS = ((DTGA*30./44.-RMCR)*44./30.)-RGCR-(CTRANS*44./12.)
		   	
!----------Carbon balance check
           
           CKCIN  = (WLVG+WLVD-WLVGI)*FCLV+(WSTS-WSTI)*FCST+WSTR*FCSTR &
                          +(WRT-WRTI)*FCRT+(WSO+WSO_O)*FCSO
           CKCFL  = TNASS*(12./44.)
           
           CALL SUBCBC(CKCIN,CKCFL,TIME,CBCHK,TERMNL)
 !----------Output section
           IF (OUTPUT) THEN
            CALL OUTDAT(2,0,'DVS   ',DVS)
 !           CALL OUTDAT(2,0,'DAE   ',DAE)
            CALL OUTDAT(2,0,'RDD   ',RDD)
            CALL OUTDAT(2,0,'TMIN   ',TMIN)
            CALL OUTDAT(2,0,'TMAX   ',TMAX)
            CALL OUTDAT(2,0,'HU   ',HU)
!NEW
!            CALL OUTDAT(2,0,'DTR   ',DTR)
!            CALL OUTDAT(2,0,'RAPCDT   ',RAPCDT)
!            CALL OUTDAT(2,0,'PARCUM   ',PARCUM)
    !            CALL OUTDAT(2,0,'PAR1M   ',PAR1M)
! END NEW
            CALL OUTDAT(2,0,'NFLV  ',NFLV)
            CALL OUTDAT(2,0,'SLA   ',SLA)
            CALL OUTDAT(2,0,'SLASIM',LAI/NOTNUL(WLVG))
            CALL OUTDAT(2,0,'LESTRS ',LESTRS)
            CALL OUTDAT(2,0,'LRSTRS ',LRSTRS)
 !@@ WRITE LDSTRS IN RES.DAT, TRI NOV 28, 2011
            CALL OUTDAT(2,0,'LDSTRS ',LDSTRS)           
            CALL OUTDAT(2,0,'PCEW  ',PCEW)
            CALL OUTDAT(2,0,'NSP  ',NSP)
            CALL OUTDAT(2,0,'LAI   ',LAI  )
            CALL OUTDAT(2,0,'WAGT  ',WAGT  )
            CALL OUTDAT(2,0,'WST   ',WST)
            CALL OUTDAT(2,0,'WLVG  ',WLVG) 
            CALL OUTDAT(2,0,'WLVD  ',WLVD)
            CALL OUTDAT(2,0,'WSO   ',WSO)
            CALL OUTDAT (2,0,'WRR14 ',WRR14 )
            CALL OUTDAT(2,0,'ZRT',ZRT)
            DO i=1, sl
				Rootobs = ' '
				write(xx,'(I2)') I
					rootobs = trim('ROOTM') // trim(adjustl(xx))
				IF(ROOTCOB(I).GT.0) THEN
					CALL OUTDAT (2, 0, trim(rootobs),ROOTC(I))
				END IF
				IF (INQOBS (FILEIT,trim(rootobs))) THEN !TAOLI, 20Feb2012
					CALL OUTDAT (2, 0, trim(rootobs)//'_OBS',GETOBS(FILEIT,trim(rootobs)))
				ENDIF
			ENDDO

            IF (INQOBS (FILEIT,'NFLV')) THEN
                    CALL OUTDAT (2, 0, 'NFLV_OBS',GETOBS(FILEIT,'NFLV'))
            ENDIF
            IF (INQOBS (FILEIT,'FNLV')) THEN
                      CALL OUTDAT (2, 0, 'FNLV_OBS',GETOBS(FILEIT,'FNLV'))
            ENDIF
            IF (INQOBS (FILEIT,'FNST')) THEN
                    CALL OUTDAT (2, 0, 'FNST_OBS',GETOBS(FILEIT,'FNST'))
            ENDIF
            IF (INQOBS (FILEIT,'FNSO')) THEN
                    CALL OUTDAT (2, 0, 'FNSO_OBS',GETOBS(FILEIT,'FNSO'))
            ENDIF
            IF (INQOBS (FILEIT,'LAI')) THEN
                    CALL OUTDAT (2, 0, 'LAI_OBS',GETOBS(FILEIT,'LAI'))
            ENDIF
            IF (INQOBS (FILEIT,'WLVG')) THEN
                    CALL OUTDAT (2, 0, 'WLVG_OBS',GETOBS(FILEIT,'WLVG'))
            ENDIF
            IF (INQOBS (FILEIT,'WLVD')) THEN
                    CALL OUTDAT (2, 0, 'WLVD_OBS',GETOBS(FILEIT,'WLVD'))
            ENDIF
            IF (INQOBS (FILEIT,'WST')) THEN
                    CALL OUTDAT (2, 0, 'WST_OBS',GETOBS(FILEIT,'WST'))
            ENDIF
            IF (INQOBS (FILEIT,'WSO')) THEN
                    CALL OUTDAT (2, 0, 'WSO_OBS',GETOBS(FILEIT,'WSO'))
            ENDIF
            IF (INQOBS (FILEIT,'WAGT')) THEN
                    CALL OUTDAT (2, 0, 'WAGT_OBS',GETOBS(FILEIT,'WAGT'))
            END IF
            IF (INQOBS (FILEIT,'ANLV')) THEN
                    CALL OUTDAT (2, 0, 'ANLV_OBS',GETOBS(FILEIT,'ANLV'))
            END IF
            IF (INQOBS (FILEIT,'ANST')) THEN
                    CALL OUTDAT (2, 0, 'ANST_OBS',GETOBS(FILEIT,'ANST'))
            END IF
            IF (INQOBS (FILEIT,'ANSO')) THEN
                    CALL OUTDAT (2, 0, 'ANSO_OBS',GETOBS(FILEIT,'ANSO'))
            END IF
            IF (INQOBS (FILEIT,'ANCR')) THEN
                    CALL OUTDAT (2, 0, 'ANCR_OBS',GETOBS(FILEIT,'ANCR'))
            END IF
            IF (INQOBS (FILEIT,'DVS')) THEN
                    CALL OUTDAT (2, 0, 'DVS_OBS',GETOBS(FILEIT,'DVS'))
            END IF
            IF (INQOBS (FILEIT,'DAE')) THEN
                    CALL OUTDAT (2, 0, 'DAE_OBS',GETOBS(FILEIT,'DAE'))
				 END IF
            IF (INQOBS (FILEIT,'ZRT')) THEN
                    CALL OUTDAT (2, 0, 'ZRT_OBS',GETOBS(FILEIT,'ZRT'))
	             END IF
           END IF

!=======SET EXPORTED VARIABLES FOR SOIL BALANCE AT 0 BEFORE EMERGENCE
        ELSE IF (CROPSTA .EQ. 0) THEN
           LAI    = 0.
           ALAI   = 0.
           LAIROL = 0
        END IF
!=======END OF SKIP WHOLE RATE CALCULATIONS BEFORE EMERGENCE

!===========Checks on simulation run
!-----------If biomass is negative: set at 0 and abort simulation
            IF (WSO.LT.0..OR.WLVG.LT.0..OR.WST.LT.0..OR.WRT.LT.0.0) THEN  
               WRITE (*,*) 'Negative biomass=> simulation stopped'
               CALL OUTCOM('Negative biomass => simulation stopped')
! BAS: removed, Spet 2006  \Commented them back by TAOLI, May 7 2010
				IF (WSO.LT.0.) WSO = 0.
               IF (WRR.LT.0.) WRR = 0.
               IF (WST.LT.0.) WST = 0.
               IF (WLVG.LT.0.) WLVG = 0.
			   IF (WRT.LT.0.) WRT = 0.
               TERMNL = .TRUE.
            END IF

!-----------If LAI is negative: set at 0 and abort simulation
            IF (LAI.LT.-0.01) THEN
               WRITE (*,*) 'Negative LAI=> simulation stopped'
               CALL OUTCOM('Negative LAI => simulad:\simulation\strasa\rainfedtion stopped')
!               IF (LAI.LT.0.) LAI = 0.
               TERMNL = .TRUE.
            END IF

!-----------The following only in main field
            IF (CROPSTA .GE. 4) THEN
!           Check if lower limit dead leaves is reached
               IF (LDSTRS.LE.0.) THEN
                  WRITE (*,*) 'Soil dryer than lower limit dead leaves'
                  WRITE (*,*) 'LDSTRS = 0 => pay attention!'
                  CALL OUTCOM('LDSTRS = 0 => pay attention!')
               !   TERMNL = .TRUE.
               !Commented out, to aviod the stop under super drought stress, TAOLI, 30NOV, 2011
               END IF
!-----------End if only in main field
            END IF
 
!===================================================================*
!     INTEGRATION SECTION                                           *
!===================================================================*
      ELSE IF (ITASK.EQ.3) THEN

!=======SKIP WHOLE STATE UPDATE BEFORE EMERGENCE
         IF (CROPSTA .GE. 1) THEN
!-----------Leaf area index and total area index (leaves + stems)
            LAI    = INTGR2(LAI, GLAI, DELT, FILEIT, 'LAI')
			LAI = MAX(LAI, 0.0001)				!by TaoLi, 8 March 2011	
            IF(SHADET.LT.1.0) THEN
                LAIRD = (1.0-SHADET)/0.5*1.6
                LAIRD = LOG(LAIRD*DAYLP*3600.0/RDD/KDF)
                LAIRD = MAX(0.0, LAI- MAX(0.0, -LAIRD))  
            ELSE
                LAIRD = 0.0
            ENDIF
            LAI = MAX(0.0,LAI-LAIRD/5.0)
            LAIRD = (LAIRD/SLA/5.0)/DELT                          		
            ALAI   = LAI+0.5*SAI
   !-----------Integrate rate variables
            PARCUM = INTGRL(PARCUM,DPARI,DELT)
            PARCM1 = INTGRL(PARCM1,PARI1,DELT)
            TS     = INTGRL(TS    ,HU   ,DELT)
            TSLV   = INTGRL(TSLV  ,HULV ,DELT)
            TMAXC  = INTGRL(TMAXC ,TMAX ,DELT)
            TMINC  = INTGRL(TMINC ,TMIN ,DELT)
            DVS    = INTGRL(DVS   ,DVR  ,DELT)
            WLVG   = INTGRL(WLVG  ,RWLVG-DLDR-LAIRD,DELT)
            WLVD   = INTGRL(WLVD  ,LLV+DLDR+LAIRD  ,DELT)
            WSTS   = INTGRL(WSTS  ,GST  ,DELT)
            WSTR   = INTGRL(WSTR  ,RWSTR,DELT)
            WSO    = INTGRL(WSO+WSO_O   ,GSO  ,DELT)
            
            !TAOLI 19MAY 2012
            IF((WSO.LT.0.0)) THEN !.AND.(WSO.GE.-2.0)) THEN   
                 WSO_O = WSO; WSO = 0.0
            ELSE
                WSO_O = 0.0 
            END IF      !TAOLI 19MAY 2012           

!-----------TAOLI, JUNE 17 2009, ROOT GROWTH
            WRT    = INTGRL(WRT,GRT,DELT)
            IF(WRT.LE.0.0) THEN
                TERMNL = .TRUE.
                WRITE (*,*) 'SIMULATION STOPPED DUE TO NAGETIVE ROOT BIOMASS!'
            ELSE
             IF(PV%PROOT_NUTRIENT) THEN
!----------TAOLI, APRIL 2, 2009               
                !ROOTMAXN =RCNL/RFNLV*LINT2('NMAXLT',NMAXLT,ILNMAX,DVS)
                ROOTMAXN = LINT2('NMAXLT',NMAXLT,ILNMAX,DVS)
	     	    NROOTC=GRT*DELT			!IN kg DM/ha/d
				!IF(ROOTNC.gt.0.0) then
					IF((CROPSTA.GT.3).AND.(PV%NBALANCE)) THEN
						NROOTN = NRT		
					ELSE
						Y1 = sum(ROOTN(1:SL)); Y2=SUM(ROOTC(1:SL))
						IF((CROPSTA.NE.3).AND.(NROOTC.LT.0.0)) THEN
						    NROOTN = NROOTC*ROOTMAXN !RCNL						
						ELSE
						    IF(Y2.NE.0.0) THEN
						    Y2 = MAX(RCNL,Y1/Y2)
						    ELSE
						        Y2=ROOTMAXN
						    END IF
						    NROOTN = NROOTC*MIN(Y2, ROOTMAXN)
						END IF						
					END IF
				!ELSE
				!	NROOTN=NROOTC* RCNL * 4.0 
				!ENDIF				
				LROOTC =0.0; LROOTN = 0.0
         		CALL ROOTG(CROPSTA,DVS,DELT, LROOTC ,LROOTN, 1) 
         		! TAOLI, 30NOV, 2011, above section moved from ITASK = 2, TAOLI 17Oct 2013

				 TWRT=0.0
				 DO I=1, SL
					 ROOTC(I) =INTGRL(ROOTC(I), RRCC(I), DELT)
					 ROOTN(I) =INTGRL(ROOTN(I), RRNC(I), DELT) !UPTAKE NITROGEN HAS ALREADY INVLUDED INTO ROOTN(I) IN NCROP3 ROUTINE
					 RDCL(I) = INTGRL(RDCL(I), RRDCL(I), DELT)
					 TWRT = TWRT + ROOTC(I) + RDCL(I)
					 RDNL(I) = INTGRL(RDNL(I), RRDNL(I), DELT)
!					 RDENSITY(I)= INTGRL(RDENSITY(I), RRDENSIT(I), DELT)
					 RDENSITY(I)= ROOTC(I)*RRDENSIT(I)   !Changed by TAOLI, 25 Feb 2010	
					 RRDENSIT(I) = RRCC(I)*RRDENSIT(I)*Delt		!Changed by TAOLI, 25 Feb 2010		 
					 pv%prestype(i) = 1; pv%presc(i,1)=pv%presc(i,1)+RRDCL(I)*delt
					 pv%presn(i, 1) = pv%presn(i, 1)+RRDNL(I) * delt;	pv%prootden(i) = rdensity(i)
				 ENDDO

!--------CHECK IF THE WEIGHT OF ROOT FROM WHOLE AND LAYERS ARE THE SAME
				 IF(ABS(TWRT-WRT)/MAX(0.00001, WRT).GT.0.05) THEN
					 CALL FATALERR ('ROOTG','THE TOTAL WEIGHT OF ROOTS IS NOT SAME BY CALCULATING AS WHOLE AND LAYERS!')
				 ENDIF
				 !IF(CROPSTA.GT.3) THEN
				    TWRT=SUM(ROOTN)+SUM(RDNL)
			     IF(SUM(ROOTN).LE.0.0) THEN
			        ROOTN = 0.0
			     ELSE
				     IF(ABS(NROOTN-(SUM(ROOTN)+SUM(RDNL)-SUMROOTN))/MAX(0.0000001,TWRT).GT.0.01) THEN
				         CALL FATALERR ('ROOTG','THE TOTAL NITROGEN OF ROOTS IS NOT SAME BY CALCULATING AS WHOLE AND LAYERS!')
				     END IF
				END IF
				 NROOTC=0.0; NROOTN=0.0
				!END IF
			 ENDIF
			 ENDIF
			
!---------END SECTION, TAOLI, 17 JUNE 2009

            IF (WRR.LT.0.) THEN
                WRR = 0.
            ENDIF
!-----------END OF SECTION AVOIDING NEGATIVE GRAIN GROWTH RATE

            WRR    = INTGRL(WRR,GGR,DELT)
            NGR    = INTGRL(NGR,GNGR,DELT)
            NSP    = INTGRL(NSP,GNSP,DELT)
            DAE    = INTGRL(DAE,RDAE,DELT)
            TNASS  = INTGRL(TNASS,RTNASS,DELT)

!-----------Calculate sums of states
            WST    = WSTS + WSTR
            WLV    = WLVG + WLVD
            WAG    = WLVG + WST  + WSO
            WAGT   = WLV  + WST  + WSO
            TDRW   = WLV  + WST  + WSO + WRT

            PWRR   = NGR*WGRMX
            NGRM2  = NGR/10000.
            NSPM2  = NSP/10000.

!-----------Weight rough rice with 14% moisture
            IF(WRR.LE.0.0) WRR = 0.0        !TAOLI 2 JAN 2012
            WRR14  = (WRR/0.86)
			 IF(PV%PROOT_NUTRIENT) THEN
				 ZRT =  min(MaxDep/100.0,REFFECD)			!BY TAOLI, 17 JUNE, 509
			 ELSE
!-----------Root length
				 IF ((.NOT.DROUT).AND.(ZRT.LE.ZRTMCW)) THEN	!COMMENT OUT BY TAOLI, 1 JUNE 2009
					 ZRTM = MIN(ZRTMCW,ZRTMS,TKLT)
				 ELSE IF ((.NOT.DROUT).AND.(ZRT.GT.ZRTMCW)) THEN
					 ZRTM = MIN(ZRT,ZRTMS,TKLT)
				 ELSE IF (DROUT) THEN
					 ZRTM = MIN(ZRTMCD,ZRTMS,TKLT)
				 END IF
				 ZRT    = INTGRL(ZRT,GZRT,DELT)	
				 ZRT    = MIN(ZRT,ZRTM)				!COMMENT OUT BY TAOLI, 1 JUNE 2009
			 ENDIF
			IF(DAYSFL.LT.1) THEN    !TAOLI, 28 JULY 2013
			    IF(DVS.GE.1.0) THEN
			        DAYSFL = DAE
			    END IF
			ENDIF
!-----------Terminate simulation settings
            IF (DVS.GT.2.0) THEN
               TERMNL = .TRUE.
!              WRITE (*,*) 'Crop reached maturity'
             END IF
! BB 2006: Crop growth stops below certain lower threshold T days
            IF (NCOLD.GT.COLDEAD) THEN
               TERMNL = .TRUE.
               WRITE (*,*) 'Crop died because of low temperature'
             END IF

!========END OF SKIP WHOLE RATE CALCULATIONS BEFORE EMERGENCE
         END IF

!===================================================================*
!     TERMINAL SECTION                                              *
!===================================================================*
      ELSE IF (ITASK.EQ.4) THEN
!        Terminal calculations
!        Terminal output
         IF(INT(WRR14*100.0)/100.0.LT.1.0) WRR14 =0.0
         IF(INT(WAGT*100.0)/100.0.LT.1.0) WAGT =0.0
         IF(INT(WSO*100.0)/100.0.LT.1.0) WSO =0.0
         IF(LEN_TRIM(PV%OPSTRING).GT.1) THEN
           IF(INDEX(PV%OPSTRING,'WRR14').GT.0) CALL OPSTOR ('WRR14', max(0.0,INT(WRR14*100.0)/100.0))
           IF(INDEX(PV%OPSTRING,'WSO').GT.0) CALL OPSTOR ('WSO', max(0.0,INT(WSO*100.0)/100.0))
           IF(INDEX(PV%OPSTRING,'WAGT').GT.0) CALL OPSTOR ('WAGT', max(0.0,INT(WAGT*100.0)/100.0))
           IF(INDEX(PV%OPSTRING,'PARCUM').GT.0) CALL OPSTOR ('PARCUM', INT(PARCUM*100.0)/100.0)
           IF(INDEX(PV%OPSTRING,'TS').GT.0) CALL OPSTOR ('TS', INT(TS*100.0)/100.0)
           IF(INDEX(PV%OPSTRING,'TMAXC').GT.0) CALL OPSTOR ('TMAXC', INT(TMAXC*100.0)/100.0)
           IF(INDEX(PV%OPSTRING,'TIMNC').GT.0) CALL OPSTOR ('TMINC', INT(TMINC*100.0)/100.0)
           IF(INDEX(PV%OPSTRING,'TAVERC').GT.0) CALL OPSTOR ('TAVERC', INT((TMAXC+TMINC)/2.*100.0)/100.0)     
           IF(INDEX(PV%OPSTRING,'SF1').GT.0) CALL OPSTOR ('SF1', INT(SF1*1000.0)/1000.0)
           IF(INDEX(PV%OPSTRING,'SF2').GT.0) CALL OPSTOR ('SF2', INT(SF2*1000.0)/1000.0)
           IF(INDEX(PV%OPSTRING,'SPFERT').GT.0) CALL OPSTOR ('SPFERT', INT(SPFERT*1000.0)/1000.0)           
         ELSE 
         CALL OPSTOR ('WRR14', max(0.0,INT(WRR14*100.0)/100.0))
         CALL OPSTOR ('WSO', max(0.0,INT(WSO*100.0)/100.0))
         CALL OPSTOR ('WAGT', max(0.0,INT(WAGT*100.0)/100.0))
         CALL OPSTOR ('PARCUM', INT(PARCUM*100.0)/100.0)
         CALL OPSTOR ('TS', INT(TS*100.0)/100.0)
         CALL OPSTOR ('TMAXC', INT(TMAXC*100.0)/100.0)
         CALL OPSTOR ('TMINC', INT(TMINC*100.0)/100.0)
         CALL OPSTOR ('TAVERC', INT((TMAXC+TMINC)/2.*100.0)/100.0)
           CALL OPSTOR ('SF1', INT(SF1*1000.0)/1000.0)
           CALL OPSTOR ('SF2', INT(SF2*1000.0)/1000.0)
           CALL OPSTOR ('SPFERT', INT(SPFERT*1000.0)/1000.0)  
           !@@@CALL OPSTOR ('DAYSFL', DAYSFL)
         !		update the soil fresh organic matter, living root matter will be added into organic C and N pools
         END IF
			DO I=1, SL
				pv%prestype(i) = 1; pv%presc(i,1)=pv%presc(i,1)+rootc(i)
				pv%presn(i, 1) = pv%presn(i, 1)+rootn(i)
				ROOTC(I) = 0.0; ROOTN(I) = 0.0
			ENDDO
	
	5000 FORMAT(I3, 2(",",F12.8))
      END IF
      RETURN
      END
