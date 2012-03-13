
!!!!!!!!!!!!! oryza wrappers to DSSAT system
      SUBROUTINE FatalERR (ERRKEY,MESSAG)
      Use infrastructure2
      IMPLICIT NONE
      CHARACTER(LEN=*) :: ERRKEY, MESSAG
      CALL WARNING(1,ERRKEY,MESSAG)
      call ERROR(ERRKEY,99,MESSAG,0)
      end subroutine

      ! Replacement routine for oryza. We need to supply a valid LUN as oryza will close() it later.
      SUBROUTINE RDINIT (IUNIT,IULOG,DATFIL)
      IMPLICIT NONE
      INTEGER IUNIT,IULOG
      CHARACTER*(*) DATFIL
      call GETLUN(DATFIL,IUNIT)
      open (unit = iunit, file = datfil)
      end subroutine

!      logical function isReadFromParameters (xname)
!      use Infrastructure2
!      IMPLICIT NONE
!      CHARACTER*(*) XNAME
!
!      RETURN
!      END

!      ! See if a variable exists
!      LOGICAL FUNCTION RDINQR (XNAME)
!      use Infrastructure2
!      IMPLICIT NONE
!      CHARACTER*(*) XNAME
!      CHARACTER X(100)*(100)
!      integer found, numvals
!      X(:) = ' '
!      found = ReadStringArray(XNAME   &   ! Variable Name
!            , '()'           &   ! Units
!	    , IsOptional     &
!            , X, numvals, 100)
!      RDINQR = found .gt. 0
!      RETURN
!      END

      ! Read a single real
      SUBROUTINE RDSREA (XNAME,X)
      use Infrastructure2
      use Oryza2Module
      IMPLICIT NONE
      CHARACTER*(*) XNAME
      REAL X
      integer found

      SELECT CASE (XNAME)
      CASE ('NMAXSO') X = NMAXSO    !  C:\CSM_ORYZA\Source\ORYZA\Ncrop3.f90(158):       
      CASE ('NMAXUP', NMAXUP)       !  C:\CSM_ORYZA\Source\ORYZA\Ncrop3.f90(159):       
      CASE ('NFLVI', NFLVI)         !  C:\CSM_ORYZA\Source\ORYZA\Ncrop3.f90(160):       
      CASE ('FNLVI', FNLVI) !  C:\CSM_ORYZA\Source\ORYZA\Ncrop3.f90(161):       
      CASE ('RFNLV', RFNLV)  !  C:\CSM_ORYZA\Source\ORYZA\Ncrop3.f90(162):       
      CASE ('RCNL  ', RFNRT)!  C:\CSM_ORYZA\Source\ORYZA\Ncrop3.f90(164):					!TAOLI 27 JULY 2009
      CASE ('RFNST', RFNST)  !  C:\CSM_ORYZA\Source\ORYZA\Ncrop3.f90(168):       
      CASE ('TCNTRF', TCNTRF)!  C:\CSM_ORYZA\Source\ORYZA\Ncrop3.f90(169):       
      CASE ('FNTRT', FNTRT)  !  C:\CSM_ORYZA\Source\ORYZA\Ncrop3.f90(170):       
      CASE ('SMINNH4', SMINNH !  C:\CSM_ORYZA\Source\ORYZA\Ncrop3.f90(174):		4)
      CASE ('SMINNO3', SMINNO !  C:\CSM_ORYZA\Source\ORYZA\Ncrop3.f90(179):		3)
      CASE ('NFLVI', NFLVI)  !  C:\CSM_ORYZA\Source\ORYZA\Nnostress2.f90(63):    
      CASE ('LAPE  ',LAPE )  !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(166):       
      CASE ('DVSI  ',DVSI )  !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(167):       
      CASE ('WLVGI ',WLVGI)  !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(168):       
      CASE ('WRTI  ',WRTI )  !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(169):       
      CASE ('WSOI  ',WSOI )  !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(170):       
      CASE ('WSTI  ',WSTI )  !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(171):       
      CASE ('ZRTI  ',ZRTI )  !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(172):       
      CASE ('ZRTTR ',ZRTTR)  !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(173):       
      CASE ('TTEMP ',TTEMP  )!  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(183):		
      CASE ('TCHANG',TCHANG )!  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(184):		
      CASE ('SHOUR ',SHOUR  )!  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(185):		
      CASE ('EHOUR ',EHOUR  )!  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(186):		
      CASE ('SDAY  ',SDAY   )!  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(187):		
      CASE ('EDAY  ',EDAY   )!  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(188):		
      CASE ('NH    ',NH   )  !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(197):       
      CASE ('NPLH  ',NPLH )  !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(198):       
      CASE ('NPLSB ',NPLSB)  !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(199):       
      CASE ('NPLDS ',NPLDS)  !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(201):       
      CASE ('TMPSB ',TMPSB)  !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(203):       
      CASE ('FRPAR ',FRPAR ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(216):       
      CASE ('CO2   ',CO2   ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(217):       
      CASE ('CO2REF',CO2REF) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(218):       
      CASE ('CRGLV ',CRGLV ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(219):       
      CASE ('CRGRT ',CRGRT ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(220):       
      CASE ('CRGSO ',CRGSO ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(221):       
      CASE ('CRGST ',CRGST ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(222):       
      CASE ('CRGSTR',CRGSTR) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(223):       
      CASE ('DVRI  ',DVRI  ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(224):       
      CASE ('DVRJ  ',DVRJ  ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(225):       
      CASE ('DVRP  ',DVRP  ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(226):       
      CASE ('DVRR  ',DVRR  ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(227):       
      CASE ('FCLV  ',FCLV  ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(228):       
      CASE ('FCRT  ',FCRT  ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(229):       
      CASE ('FCSO  ',FCSO  ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(230):       
      CASE ('FCST  ',FCST  ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(231):       
      CASE ('FCSTR ',FCSTR ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(232):       
      CASE ('FSTR  ',FSTR  ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(233):       
      CASE ('LRSTR ',LRSTR ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(234):       
      CASE ('MAINLV',MAINLV) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(235):       
      CASE ('MAINRT',MAINRT) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(236):       
      CASE ('MAINSO',MAINSO) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(237):       
      CASE ('MAINST',MAINST) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(238):       
      CASE ('MOPP  ',MOPP  ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(239):       
      CASE ('PPSE  ',PPSE  ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(240):       
      CASE ('Q10   ',Q10   ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(241):       
      CASE ('RGRLMX',RGRLMX) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(242):       
      CASE ('RGRLMN',RGRLMN) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(243):       
      CASE ('SCP   ',SCP   ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(244):       
      CASE ('SHCKD ',SHCKD ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(245):       
      CASE ('SHCKL ',SHCKL ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(246):       
      CASE ('SPGF  ',SPGF  ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(247):       
      CASE ('TBD   ',TBD   ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(248):       
      CASE ('TBLV  ',TBLV  ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(249):       
      CASE ('TCLSTR',TCLSTR) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(250):       
      CASE ('TMD   ',TMD   ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(251):       
      CASE ('TOD   ',TOD   ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(252):       
      CASE ('COLDMIN',COLDMIN !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(253):       )
      CASE ('COLDEAD',COLDEAD !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(254):       )
      CASE ('CTSTER' ,CTSTER)!  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(255):		
      CASE ('TREF  ',TREF  ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(256):       
      CASE ('WGRMX ',WGRMX ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(257):       
      CASE ('ZRTMCW',ZRTMCW) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(258):       
      CASE ('ZRTMCD',ZRTMCD) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(259):       
      CASE ('GZRT  ',GZRT  ) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(260):       
      CASE ('SROOTL', SROOTL)!  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(262):					!special root length cm/g C
      CASE ('RMINT',  RMINT)!  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(263):       				!minimum temperature for root growth
      CASE  ('ROPTT', ROPTT)!  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(264):       			!optimum temperature of root growth
      CASE  ('RTBS',  RTBS)	!  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(265):       			!minimim temperature for root to survive
      CASE ('RCNL',   RCNL)	!  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(266):       			!lowest root nitrogen content (residue root N content)
      CASE ('MAXD', MAXD)	!  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(267):					!MAXIMUM ROOTING DEPTH (CM)
      CASE ('SODT  ', SODT)  !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(268):		           !TOLERANCE OF OXYGEN DEFICIENCY
      CASE ('SFLOWER',SFLOWER !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(270):		)
      CASE ('IFLOWER',IFLOWER !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(275):		)
      CASE ('TFLOWER',TFLOWER !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(280):		)
      CASE ('FSWTD', FSWTD)  !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(285):		
      CASE ('AMAXSLN0',AMaxSL !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(295):		N0)
      CASE ('MINSLN',MINSLN) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(300):		
      CASE ('ASLA',ASLA)     !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(321):       
      CASE ('BSLA',BSLA)     !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(322):       
      CASE ('CSLA',CSLA)     !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(323):       
      CASE ('DSLA',DSLA)     !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(324):       
      CASE ('SLAMAX',SLAMAX) !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(325):       
      CASE ('PLOWPAN', PV%PPL !  C:\CSM_ORYZA\Source\ORYZA\ORYZA1.f90(423):		OWDEPTH)			!THE DEPTH OF PLOWPAN, NEGATIVE VALUE INDICATES NO PLOWPAN
      CASE  ('ULLS', ULLS)   !  C:\CSM_ORYZA\Source\ORYZA\WStress2.f90(119):     
      CASE  ('LLLS', LLLS)   !  C:\CSM_ORYZA\Source\ORYZA\WStress2.f90(120):     
      CASE  ('ULDL', ULDL)   !  C:\CSM_ORYZA\Source\ORYZA\WStress2.f90(121):     
      CASE  ('LLDL', LLDL)   !  C:\CSM_ORYZA\Source\ORYZA\WStress2.f90(122):     
      CASE  ('LLLE', LLLE)   !  C:\CSM_ORYZA\Source\ORYZA\WStress2.f90(123):     
      CASE  ('ULLE', ULLE)   !  C:\CSM_ORYZA\Source\ORYZA\WStress2.f90(124):     
      CASE  ('LLRT', LLRT)   !  C:\CSM_ORYZA\Source\ORYZA\WStress2.f90(126):     
      CASE  ('ULRT', ULRT)   !  C:\CSM_ORYZA\Source\ORYZA\WStress2.f90(127):     
      CASE ('SWIRTRF',SWIRTRF !  C:\CSM_ORYZA\Source\ORYZA\WStress2.f90(130):		) !IF SETTING IN FUNCTION, 
      CASE ('FSWTD', FSWTD)  !  C:\CSM_ORYZA\Source\ORYZA\WStress2.f90(136):		

!  C:\CSM_ORYZA\Source\ttutil\rdsrea.for(1):      SUBROUTINE RDSREA (XNAME,X)
!  C:\CSM_ORYZA\Source\ttutil\rdsrea.for(21):      CALL RDDATA (5,'RDSREA',0,0,' ',IS,XNAME,'R',D,R,I,C,L,
!  C:\CSM_ORYZA\Source\ORYZA\CSMWrapper.f90(44):      SUBROUTINE RDSREA (XNAME,X)
!  Matching lines: 109    Matching files: 7    Total files searched: 858
      end subroutine









      ! read a single INTEGER number 
      SUBROUTINE RDSINT (XNAME,X)
      use Infrastructure2
      use Oryza2Module

      IMPLICIT NONE 
      CHARACTER*(*) XNAME
      INTEGER X

      integer found
      if (XNAME .eq. 'NL') then
         X = g%SoilProfile%num_dlayer
         found = 1
      else
         found = ReadInteger(XNAME &   ! Variable Name
                            , '()'  &   ! Units
	                    , 1     &   !isOptional
                            , X, -1000000, 1000000)
      end if
      if (found .eq. 0) then
        call FatalERR('oryza', 'Cannot read parameter '//xname)
      endif 
      end subroutine

      ! read a single CHARACTER value
      SUBROUTINE RDSCHA (XNAME,X)
      use Infrastructure2
      IMPLICIT NONE
      CHARACTER*(*) XNAME,X
      integer found

      found = ReadParam(XNAME  &   ! Variable Name
            , '()'             &   ! Units
	    , 1                &   !isOptional
            , X)
      if (found .eq. 0) then
        call FatalERR('oryza', 'Cannot read parameter '//xname)
      endif 
      end subroutine











      ! Read an array of reals
      SUBROUTINE RDAREA (XNAME,X,ILDEC,IFND)
      use Infrastructure2
      IMPLICIT NONE
      INTEGER ILDEC,IFND
      REAL X
      DIMENSION X(ILDEC)
      CHARACTER*(*) XNAME
      integer found
      found =  readParam(XNAME   &   ! Variable Name
            , '()'            &   ! Units
	    , NotOptional     &   !isOptional
            , X               &   ! Variable
	    , IFND            &   ! number returned
	    , ILDEC           &   ! Size of array
            , -1.0E9, 1.0E9)       !lower, upper
      if (IFND .eq. 0 .OR. found .eq. 0) then
        call FatalERR('oryza', 'Cannot read parameter '//xname)
      end if 
      end subroutine

      ! Dont know what this one does
      SUBROUTINE RDFREA (XNAME,X,ILDEC,IVALS)
      Use infrastructure2
      IMPLICIT NONE
      INTEGER ILDEC,IVALS
      REAL X
      DIMENSION X(ILDEC)
      CHARACTER*(*) XNAME
      call fatal_error(err_internal,'RDFREA called')
      end subroutine

      ! Translate lower case characters
      SUBROUTINE UPPERC (STRING)
      use Infrastructure2
      IMPLICIT NONE
      CHARACTER(LEN=*) :: STRING
       integer   char_code             ! Code of character (ASCII on PC's)
       integer   char_index            ! Index into character string
       character Charact*1             ! Current character
       integer   Code_diff             ! Difference in codes between 'A' - 'a'
       integer   lower_char            ! Lowercase character code
       integer   string_end            ! end of string position

   !- Implementation Section ----------------------------------

      ! Calculate the difference between 'A' and 'a' and apply this difference
      ! to each character in the character string.

      Code_diff = ichar ('a') - ichar ('A')

      string_end = len_trim(STRING)

      do 10 char_index = 1, string_end
         Charact = STRING(char_index:char_index)
         if (Charact .ge. 'a' .and. Charact .le. 'z') then

            ! Character is lowercase - convert to uppercase

            char_code = ichar (Charact)
            lower_char = char_code - Code_diff
            STRING(char_index:char_index) = char (lower_char)

         else
            ! Character is already lowercase.
         endif
10    continue
      end subroutine

      ! Linear interpolation		 
      REAL FUNCTION LINT2 (TABNAM,TABLE,ILTAB,X)
      Use infrastructure2
      IMPLICIT NONE
      CHARACTER*(*) TABNAM
      INTEGER ILTAB
      REAL TABLE(ILTAB), X
      INTEGER I1, IUP, IL, ILEN
      REAL SLOPE, TINY
      PARAMETER (TINY=1.E-7)
      LOGICAL ERR
      character(256) errmsg
      SAVE
      ERR  = .FALSE.
      IF (MOD(ILTAB,2).NE.0 .OR. ILTAB.LE.2) THEN
      IL = MAX (1, LEN_TRIM (TABNAM))
      WRITE (errmsg,'(2A,/,A)')&
        ' Number of elements in interpolation table: ',TABNAM(1:IL),&
        ' not correct !'
      ERR = .TRUE.
      ELSE
      IUP = 0
      DO 10 I1=3,ILTAB,2
      IF (TABLE(I1).LE.TABLE(I1-2)) THEN
      IL = MAX (1, LEN_TRIM (TABNAM))
      WRITE (errmsg,'(2A,/,A,I4)')&
        ' X-coordinates in interpolation table: ',TABNAM(1:IL),&
        ' not in ascending order at point',I1
      ERR = .TRUE.
      END IF
      IF (IUP.EQ.0 .AND. TABLE(I1).GE.X) IUP = I1
10    CONTINUE
      END IF
      IF (.NOT.ERR .AND. X.LT.TABLE(1)) THEN
      IUP = 3
      IF ((TABLE(1)-X) .GT. ABS(X)*TINY) THEN
      IL = MAX (1, LEN_TRIM (TABNAM))
      WRITE (errmsg,'(A,G13.5,/,2A)')&
        ' WARNING in LINT2: X-value below defined region at X=',X,&
        ' in interpolation table: ',TABNAM(1:IL)
      END IF
      ELSE IF (.NOT.ERR .AND. X.GT.TABLE(ILTAB-1)) THEN
      IUP = ILTAB-1
      IF ((X-TABLE(ILTAB-1)) .GT. ABS(X)*TINY) THEN
      IL = MAX (1, LEN_TRIM (TABNAM))
      WRITE (*,'(A,G13.5,/,2A)')&
        ' WARNING in LINT2: X-value above defined region at X=',X,&
        ' in interpolation table: ',TABNAM(1:IL)
      END IF
      END IF
      IF (ERR) CALL FatalERR ('Oryza2/LINT2',errmsg)
      SLOPE = (TABLE(IUP+1)-TABLE(IUP-1))/(TABLE(IUP)-TABLE(IUP-2))
      LINT2 = TABLE(IUP-1)+(X-TABLE(IUP-2))*SLOPE
      RETURN
      END

      !accessory to prevent divide by zero
      REAL FUNCTION NOTNUL (X)
      IMPLICIT NONE
      REAL X
      IF (X.NE.0.) THEN
         NOTNUL = X
      ELSE
         NOTNUL = 1.
      END IF
      RETURN
      END

      ! integrate a state
      REAL FUNCTION INTGRL (STATE, RATE, DELT)
      IMPLICIT NONE
      REAL STATE, RATE, DELT
      SAVE
      INTGRL = STATE + RATE * DELT
      RETURN
      END

      ! Oryza predicted:observed helper. Unimplmented
      LOGICAL FUNCTION INQOBS (FILEIN,VARNAM)
      IMPLICIT NONE
      CHARACTER (*) FILEIN, VARNAM
      inqobs = .false.
	  return
	  end
	  
      ! Oryza predicted:observed helper. Unimplmented
      REAL FUNCTION GETOBS (FILEIN,VARNAM)
      IMPLICIT NONE
      CHARACTER (*) FILEIN, VARNAM
      getobs = 0.0
	  return
	  end

      ! Oryza predicted:observed helper. Unimplmented
      SUBROUTINE OPSTOR (VARNAM,VARVAL)
      IMPLICIT NONE
      CHARACTER (*) VARNAM
      REAL          VARVAL
	  return
	  end

      ! Limit a variable to min/max values
      REAL FUNCTION LIMIT (MIN,MAX,X)
      IMPLICIT NONE
      REAL MIN,MAX,X
      IF (X.LT.MIN) THEN
      LIMIT = MIN
      ELSE IF (X.LE.MAX) THEN
      LIMIT = X
      ELSE
      LIMIT = MAX
      END IF
      RETURN
      END

      ! ??	     
      SUBROUTINE OUTCOM (STR)
      Use infrastructure2
      IMPLICIT NONE
      CHARACTER*(*) STR
      call WriteLine('oryza2 '// str)
      RETURN
      END

      ! fortran style switch
      REAL FUNCTION INSW (X1,X2,X3)
      IMPLICIT NONE
      REAL X1,X2,X3
      IF (X1.LT.0.) THEN
      INSW = X2
      ELSE
      INSW = X3
      END IF
      RETURN
      END

!=======================================================================
!  WaterPotential, Subroutine
!  Calculates Matrix Potential 
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!-----------------------------------------------------------------------
!  Called by: SOILDYN
!=======================================================================

      SUBROUTINE WaterPotential( &
         SW, SOILPROP, FLOOD,    &
         WPkpa)                 
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
!      IF (FLOOD > 1.E-6) RETURN
 
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
 
!C=======================================================================
!C  YR_DOY, Subroutine, N.B. Pickering, 09/13/91
!C  Converts YRDOY to YR and DOY.
!C-----------------------------------------------------------------------
!C  Input : YRDOY
!C  Output: YR,DOY
!C=======================================================================

      SUBROUTINE YR_DOY(YRDOY,YR,DOY)

      IMPLICIT NONE

      INTEGER DOY,YR,YRDOY

      YR  = INT(YRDOY / 1000)
      DOY = YRDOY - YR * 1000

      END SUBROUTINE YR_DOY

!C=======================================================================
!C  INCDAT, Integer Function,J.Hansen
!C  Similar to INCYD without the restriction that DELTA <= 365.
!C-----------------------------------------------------------------------
!C  Input : YRDOY(ADATE)
!C  Output: INCDAT
!C  Local : NDYR,AYR,ADOY,DELTA,ENDYR,YDOY
!C-----------------------------------------------------------------------

      INTEGER FUNCTION INCDAT(ADATE, DELTA)

      IMPLICIT NONE
      INTEGER NDYR, AYR, ADOY, ADATE, DELTA, ENDYR, YDOY
      EXTERNAL ENDYR, YDOY

      CALL YR_DOY(ADATE, AYR, ADOY)
      NDYR = ENDYR(AYR)
      ADOY = ADOY + DELTA
  100 CONTINUE
      IF (ADOY .GT. NDYR) THEN
        AYR = AYR + 1
        ADOY = ADOY - NDYR
        GO TO 100
      END IF
  200 IF (ADOY .LE. 0) THEN
        AYR = AYR - 1
        NDYR = ENDYR(AYR)
        ADOY = ADOY + NDYR
        GO TO 200
      END IF
      INCDAT = YDOY(AYR, ADOY)

      RETURN
      END FUNCTION INCDAT

!C=======================================================================
!C  YDOY, Integer Function, N.B. Pickering, 09/13/91
!C  Converts YR and DOY to YRDOY.
!C-----------------------------------------------------------------------
!C  Input : YR,DOY
!C  Output: YRDOY
!C=======================================================================

      INTEGER FUNCTION YDOY(YR,DOY)

      IMPLICIT NONE
      INTEGER DOY,YR

      YDOY = YR * 1000 + DOY
      
      END FUNCTION YDOY

!C=======================================================================
!C  ENDYR, Integer Function, N.B. Pickering, 06/05/92
!C  Computes end-of-year (365 or 366) depending if leap year.
!C-----------------------------------------------------------------------
!C  Input : YR
!C  Output: ENDYR
!C  Local :
!C=======================================================================

      INTEGER FUNCTION ENDYR(YR)

      INTEGER YR
      LOGICAL LEAP

      IF (LEAP(YR)) THEN; ENDYR = 366
      ELSE;               ENDYR = 365
      ENDIF

      END FUNCTION ENDYR

!=======================================================================
!  LEAP, Function
!
!  Determines leap years
!-----------------------------------------------------------------------
!  Revision history
!
!  11/16/2007 CHP Written 
!-----------------------------------------------------------------------
!  INPUT  : YR
!  OUTPUT : LEAP
!=======================================================================

      LOGICAL FUNCTION LEAP (YR)

      IMPLICIT    NONE
      INTEGER YR

      IF     (MOD(YR,400) == 0) THEN; LEAP = .TRUE.
      ELSEIF (MOD(YR,100) == 0) THEN; LEAP = .FALSE.
      ELSEIF (MOD(YR,  4) == 0) THEN; LEAP = .TRUE.
      ELSE;                           LEAP = .FALSE.
      ENDIF
   
      RETURN
      END FUNCTION LEAP

!!!!!!!!!!!!!!!!!!!!!!ORYZA routines	  
!     Write output files (empty)
      SUBROUTINE OR_OPGROW (CONTROL, ISWITCH, SOILPROP,  &
         CPEW, DVS, HU, LAI, LDSTRS, LESTRS, LRSTRS,     &
         NFLV, NGR, RNSTRS, PCEW, RDCL,               &
         WAGT, WLVD, WLVG, WRR, WRT, WSO, WST, YRPLT, ZRT, &
         NACR, ANRT, ANLV, ANSO, ANST, ANLD, SNN1C)
      USE ModuleDefs
      IMPLICIT  NONE
      INTEGER  YRPLT

	  REAL CPEW, DVS, HU, LAI, LDSTRS, LESTRS, LRSTRS,     &
         NFLV, NGR, RNSTRS, PCEW,                                &
         WAGT, WLVD, WLVG, WRR, WRT, WSO, WST, ZRT 

      REAL NACR, ANRT, ANLV, ANSO, ANST, ANLD, VNAD, CNAD, SNN1C
      REAL, DIMENSION(10) :: RDCL

      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH
      TYPE (ResidueType) SENESCE
      TYPE (SoilType) SOILPROP
      end subroutine

      ! Gather sowing information
      ! Defines PLANTS, PLTPOP, PLME, PLDS, ROWSPC, PLDP, SDWTPL, PAGE, ATEMP, PLPH
      SUBROUTINE OR_IPRICE (CONTROL,               &
                            FILEI1, PLANTS, PLTPOP, PLME, PLDS,      &
                            ROWSPC, PLDP, SDWTPL, PAGE, ATEMP, PLPH, &
                            STGDOY, STNAME)
      USE ScienceAPI2
      USE ModuleDefs
      USE Oryza2Module
      IMPLICIT     NONE
      ! Sunroutine Arguments
      CHARACTER*1   PLME, PLDS
      CHARACTER*10  STNAME(20)     
      CHARACTER*128 FILEI1
      INTEGER STGDOY(20)
      REAL PLANTS, PLTPOP, ROWSPC, PLDP, SDWTPL, PAGE, ATEMP, PLPH
      TYPE (ControlType) CONTROL

      call SetSearchOrder(trim(g%sow_Cultivar))
      ! read PLANTS, PLTPOP, PLME, PLDS, ROWSPC, PLDP, SDWTPL, PAGE, ATEMP, PLPH
      PLANTS = 999.0 ! plants / m^2 in nursery                         (unused)
      PLME = 'T'   ! transplant code: t=transplant, d=direct  (always 'T' for oryza; see emergence date calcs) 
      PLDS = ' '   !PLDS = 'H'  ! planting distribution : hills, broadcast , rows (unused)
      ROWSPC = 0.0 !ROWSPC = g%sow_row_spacing  ! row spacing (pl/m^2) (unused)
      PLDP = 0.0   !PLDP   = g%sow_sowing_depth ! planting depth (mm)  (unused)

      SDWTPL = 5.0               ! seed weight at planting (?
      PAGE   = 0.0               ! number of days plant is in seedbed 
      ATEMP  = 12.0              ! average temp in seedbed
      ! PLPH   = g%sow_nplds       ! plants per hill
      call AppendSearchOrder(trim(g%sow_Establishment))

      STNAME(:)  = ' '
      STGDOY     = 9999999
      end subroutine

      SUBROUTINE IPSOIL (CONTROL, RESTYPE, CROP,        &!Input
           AM, DMINR, DSNC, EXTFAC, PRCEL, PRCHO,         &!Output
           PRLIG, PSLIG, RCN, RCP, RDCEL, RDCHO, RDLIG,   &!Output
           SCN, SCP, WATFAC)                               !Output
          USE ModuleDefs
          implicit none
          TYPE (ControlType), INTENT(IN) :: CONTROL
          CHARACTER(len=5), INTENT(IN), OPTIONAL :: RESTYPE
          CHARACTER(len=2), INTENT(IN), OPTIONAL :: CROP
          REAL, INTENT(OUT), OPTIONAL :: AM, DMINR, DSNC, EXTFAC 
          REAL, INTENT(OUT), OPTIONAL :: PRCEL, PRCHO, PRLIG, PSLIG 
          REAL, INTENT(OUT), OPTIONAL :: RCN, RCP, RDCEL, RDCHO, RDLIG
          REAL, INTENT(OUT), OPTIONAL :: SCN, SCP, WATFAC

!       Lignin content of senesced root and surface matter
	  prlig = 0.0 ! FIXME
	  pslig = 0.0

      END SUBROUTINE
 
      SUBROUTINE ExperimentFileEdit(OUTPUTFILE, YRSIM, EDATE, & 
                PRODENV, NITROENV, ESTAB,  SBDUR, NPLH, PLTPOP, NPLSB, NPLDS, ZRTTR, & 
                IIRRI, IRRCOD, IRMTAB, RIRRIT, IRRI, WL0MIN, KPAMIN, SLMIN, WLODAY, ISTAGET, &
                TMCTB)  !Other parameters 
      IMPLICIT NONE
      CHARACTER*(*) OUTPUTFILE 
      CHARACTER*(*) PRODENV, NITROENV, ESTAB, IIRRI
      INTEGER IYEAR, SBDUR, ICOMBA, I, J, J1, WL0DAY, YRSIM, EDATE, EMYR, EMD, STTIME, IRRCOD
      
      REAL NPLH, NH, PLYPOP,NPLSB, NPLDS, ZRTTR, WL0MIN, KPAMIN, WCMIN, IRRI, SLMIN
      REAL IRMTAB(300), RIRRIT(750),ISTAGET(900), TMCTB(750)
      REAL  PLTPOP, WLOMIN, WLODAY

      end subroutine

      SUBROUTINE SOILFILEEDIT(FILEI2,NL,TKL,SANDX, CLAYX, BD, SOC, SON, SNH4X, SNO3X, SUREA, PLOWPAN)
          USE PUBLIC_MODULE
          IMPLICIT NONE
          
          INTEGER  NL
          CHARACTER*(*) FILEI2
          REAL SANDX(10), CLAYX(10), BD(10), SOC(10), SON(10)
          REAL TKL(10), SNH4X(10), SNO3X(10), SUREA(10), PLOWPAN
      end subroutine

      SUBROUTINE OR_OPHARV (CONTROL, ISWITCH, &
        HARVFRAC, ISDATE, ISTAGE, LAI, LESTRS, MDATE, &   !Input
        NGR, NSLLV, PCEW, STGDOY, STNAME,              &  !Input
        WAGT, WLVD, WLVG, WRR, WSO, WST, YRPLT)          !Input

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT     NONE
      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH
      REAL, DIMENSION(2) :: HARVFRAC
      INTEGER ISDATE
      INTEGER ISTAGE
      REAL LAI
      REAL LESTRS
      INTEGER MDATE
      REAL NGR, NSLLV, PCEW
      INTEGER STGDOY(20)
      CHARACTER*10 STNAME(20)
      REAL WAGT, WST, WLVG, WLVD, WSO, WRR
      INTEGER YRPLT
      end subroutine

     ! Initialise the oryza observations/ forcing system
      subroutine obsini()
      implicit none
      END SUBROUTINE
      REAL FUNCTION INTGR2 (STATE,RATE,DELT,FILEIN,STATNM)

!     Integrates similar to the INTGRL function when no observations
!     and/or forcing was given. When forcing was enabled, the
!     estimated observation is returned

!     STATE  - Old value of state                                    I
!     RATE   - Rate as calculated by model                           I
!     DELT   - Time step to be applied when no forcing takes place   I
!     FILEIN - Name of datafile in which STATNM is possibly forced   I
!     STATNM - Name of state variable                                I

!     INTGR2 - Function result, integrated or forced                 I

      IMPLICIT NONE
!     formal parameters
      REAL          STATE, RATE, DELT
      CHARACTER (*) STATNM, FILEIN

      INTGR2 = STATE+RATE*DELT

      RETURN
      END

      subroutine PotentialOp(CONTROL, ISWITCH, SOILPROP, WPkPa)
      USE ModuleDefs
      IMPLICIT     NONE
      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH
      TYPE (SoilType) SOILPROP
	  REAL, DIMENSION(NL) :: WPkPa
      end subroutine
      
      ! Replacement routine for oryza. We need to supply a valid LUN as oryza will close() it later.
      SUBROUTINE GETLUN(FileVarName, LUN)
      IMPLICIT NONE

      CHARACTER*(*), INTENT(IN) :: FileVarName
      INTEGER, INTENT(OUT) :: LUN
      select case (FileVarName) 
        case('ORYZA1'); LUN = 110
        case('ORYZA2'); LUN = 111
        case('OUTPN');  LUN = 112
        case('OUTG');   LUN = 113
        case DEFAULT;   LUN = 0
      end select
      end subroutine
	  
      SUBROUTINE RDDTMP (IUNIT)
      IMPLICIT NONE
      INTEGER IUNIT
      end subroutine

      MODULE ModuleData
      INTERFACE GET
         MODULE PROCEDURE GET_Real 
      END INTERFACE

      CONTAINS

      Subroutine GET_Real(ModuleName, VarName, Value)
      Use infrastructure2
      IMPLICIT NONE
      Character*(*) ModuleName, VarName
      Real Value
      SELECT CASE (VarName)
        Case ('ET'); Value = 0.0
        Case ('TOTIR'); Value = 0.0 ! FIXME
        Case DEFAULT; Call fatal_error (Err_internal, 'Cant GET '// VarName)
      end select
      end subroutine
      end module
