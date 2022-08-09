C=======================================================================
C COPYRIGHT 1998-2022
C                     DSSAT Foundation
C                     University of Florida, Gainesville, Florida
C                     International Fertilizer Development Center
C
C ALL RIGHTS RESERVED
!  
!  Redistribution and use in source and binary forms, with or without modification, 
!  are permitted provided that the following conditions are met:
!  
!  1. Redistributions of source code must retain the above copyright notice, this 
!     list of conditions and the following disclaimer.
!  
!  2. Redistributions in binary form must reproduce the above copyright notice, 
!     this list of conditions and the following disclaimer in the documentation 
!     and/or other materials provided with the distribution.
!  
!  3. Neither the name of the copyright holder nor the names of its contributors 
!     may be used to endorse or promote products derived from this software 
!     without specific prior written permission.
!  
!  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
!  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
!  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
!  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE 
!  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
!  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
!  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
!  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
!  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
!  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
C=======================================================================
C=======================================================================
C
C     CROPPING SYSTEM MODEL Version 4.8.1
C
C     Decision Support System for Agrotechnology Transfer (DSSAT)
C
C     July 2022  CSM Version 4.8.1
C
C     Hoogenboom, G., C.H. Porter, V. Shelia, K.J. Boote, U. Singh,  
C     J.W. White, W. Pavan, F.A. de Oliveira, L.P. Moreno, J.I. Lizaso, 
C     S. Asseng, D.N.L. Pequeno, B.A. Kimball, P. Alderman, K.R. Thorp, 
C     M.R. Jones, S.V. Cuadra, M. Vianna, F.J. Villalobos, T.B. Ferreira,  
C     W.D. Batchelor, J. Koo, L.A. Hunt, and J.W. Jones
C=======================================================================
C
C=======================================================================
C  REVISION       HISTORY
C  11/04/2001 GH  Written.
C  12/12/2001 GH  Rename to CSM and integrate with Land/CROPGRO routines
C  01/13/2002 CHP Add debug mode
C  02/02/2002 GH  Revise driver for argument calls
C  04/20/2002 GH  Revisions for sequence analysis
C  06/10/2002 GH  Revisions for outputs of sequence analysis
C  06/11/2002 GH  Modified for Y2K
C  07/22/2002 CHP Added calls to OPCLEAR and OPNAMES
C  11/25/2002 GH  Upgrade to CSM Version 3.9, 020 for December Workshop
C  08/12/2003 CHP Added I/O error checking
C  03/31/2004 GH  Upgrade to CSM Version 4.0, 040 for March 31 Release
C  09/03/2004 CHP Added GetPut_Control call to push control information
C                   into constructed variable which is accessible to
C                   all modules.  Added TRTNUM to CONTROL variable.
C  11/23/2004 CHP Increased length of PATHX (path for executable) to 120.
C  02/08/2005 CHP Changed criteria for ending a sequence run.
C  06/14/2005 CHP Added FILEX to CONTROL variable, read FILEX from FILEIO
C  02/20/2006 GH  Add RNMODE="G" option for GENCALC
C  01/11/2007 CHP Changed GETPUT calls to GET and PUT
C  01/12/2007 CHP Read trt number and rotation number for sequence mode
C  10/09/2020 FO  Y4K implementation for weather files
C=======================================================================
      PROGRAM CSM

      USE ModuleDefs 
      USE ModuleData
      USE HeaderMod

      IMPLICIT NONE
C-----------------------------------------------------------------------
      CHARACTER*1   ANS,RNMODE,BLANK,UPCASE
      CHARACTER*6   ERRKEY,FINDCH,TRNARG
      CHARACTER*8   FNAME,DUMMY,MODELARG
      CHARACTER*12  FILEX   !,DSCSM,INPUT
      CHARACTER*30  FILEB,FILEIO,FILEIOH
      CHARACTER*78  MSG(10)
      CHARACTER*80  PATHEX
      CHARACTER*102 DSSATP
!     CHARACTER*120 INPUTX
      CHARACTER*120 FILECTL !12/11/08 control file includes path
      CHARACTER*120 PATHX
      CHARACTER*130 CHARTEST

      INTEGER       YRDOY,YRSIM,YRPLT,MDATE,YREND,YR,ISIM, YR0, ISIM0
      INTEGER       MULTI,NYRS,INCYD,YEAR,DOY,DAS,TIMDIF,ENDYRS
      INTEGER       ERRNUM,LUNIO,TRTALL,TRTNUM,EXPNO,I,RUN
      INTEGER       YRSIM_SAVE, YRDIF, YRDOY_END !IP,IPX, 
      INTEGER       LUNBIO,LINBIO,ISECT,IFIND,LN, LNUM, FOUND
      INTEGER       NREPS, REPNO,END_POS, ROTNUM, TRTREP, NARG

      LOGICAL       FEXIST, DONE

      PARAMETER (ERRKEY = 'CSM   ')      
      PARAMETER (BLANK  = ' ')

C     Define constructed variable types based on definitions in
C     ModuleDefs.for.

C     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

C     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH

!C-----------------------------------------------------------------------

      DONE = .FALSE.
      YRDOY_END = 9999999

!     Pick up model version for setting the name of some files
      WRITE(ModelVerTxt,'(I2.2,I1)') Version%Major, Version%Minor

      !Delete existing output files
      CALL OPCLEAR

      CALL GETLUN('FILEIO', LUNIO)
      FILEIO = 'DSSAT48.INP'

C-----------------------------------------------------------------------
C    Get argument from runtime module to determine path of the EXE files
C-----------------------------------------------------------------------
      CALL GETARG(0,PATHX)   !,IPX
      CALL GETARG(1,DUMMY)   !,IP
      IF ((DUMMY(1:1) .NE. BLANK) .AND. (DUMMY(2:2) .EQ. BLANK))
     &    THEN
        CALL GETARG(1,RNMODE)   !,IP
        NARG = 1
        CALL CheckRunMode(RNMODE)
      ELSE
        CALL GETARG(1,MODELARG)  !,IP
        CALL GETARG(2,RNMODE)    !,IP
        CALL CheckRunMode(RNMODE)
        NARG = 2
      ENDIF

C-----------------------------------------------------------------------
C     RNMODE:  
C      A - Run all treatments.  User specifies fileX on the command
C          line and the model runs all treatments
C      B - Batch mode. User defines fileX and treatment numbers in 
C          Batch file
C      C - Command line mode.  Use input from the command line.
C      D - Debug mode.  Model skips input module and reads temp
C          file from the command line
C      E - Sensitivity analysis.  User defines fileX and treatment
C          number in Batch file 
C      F - Farm model.  Use Batch file to define experiment
C      G - Gencalc. Use Command line to define experiment and treatment
C      I - Interactive mode.  Use model interface for exp. & trtno.
C      L - Gene based model (Locus). Use Batch file to define experiment
C      N - Seasonal analysis. Use Batch file to define experiment and 
C          treatments 
C      Q - Sequence analysis. Use Batch file to define experiment
C      S - Spatial.  Use Batch file to define experiment
C      T - Gencalc. Use Batch file to define experiments and treatment
C      Y - Yield forecast mode. Use batch file.
C-----------------------------------------------------------------------

      RNMODE = UPCASE(RNMODE)
      ROTNUM = 0
      TRTNUM = 0
      SELECT CASE(RNMODE)

!     Read experiment file from command line -- run all treatments
      CASE('A')   !run All treatments
        CALL GETARG(NARG+1,FILEX)   !,IP   !Experiment file
        CALL GETARG(NARG+2,FILECTL) !,IP   !Simulation control file name

!     Read experiment file and treatment number from command line
      CASE('C','G')   !Command line, Gencalc
        CALL GETARG(NARG+1,FILEX)   !,IP   !Experiment file
        CALL GETARG(NARG+2,TRNARG)  !,IP   !Treatment number
        CALL GETARG(NARG+3,FILECTL) !,IP   !Simulation control file name
        READ(TRNARG,'(I6)') TRTNUM

!     Get experiment and treatment from batch file
      CASE('B','N','Q','S','F','T','E','L','Y')
!           Batch, seasoNal, seQuence, Spatial, 
!           Farm, Gencalc(T), sEnsitivity, Locus, Yield forecast
        CALL GETARG(NARG+1,FILEB)   !,IP   !Batch file name
        CALL GETARG(NARG+2,FILECTL) !,IP   !Simulation control file name

!     Debug mode -- bypass input module and read FILEIO
      CASE ('D')  !Debug
        CALL GETARG(NARG+1,FILEIO)  !,IP   !INP file
        DO I = 1, LEN(FILEIO)
          FILEIO(I:I) = UPCASE(FILEIO(I:I))
          ROTNUM = 0
          TRTNUM = 0
        END DO
        
!     Interactive mode, no command line arguments     
      CASE DEFAULT    !Interactive mode.  
        RNMODE = 'I'
      END SELECT

C-----------------------------------------------------------------------
C    Delete previouse copies of temporary input file
C-----------------------------------------------------------------------
      IF (RNMODE .NE. 'D') THEN
        INQUIRE (FILE = FILEIO,EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (LUNIO, FILE = FILEIO,STATUS = 'UNKNOWN',IOSTAT=ERRNUM)
          CLOSE (LUNIO,STATUS = 'DELETE')
        ENDIF
        LN = LEN(TRIM(FILEIO))
        FILEIOH = FILEIO
        WRITE(FILEIOH(LN:LN),'(A1)') 'H'
        INQUIRE (FILE = FILEIOH,EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (LUNIO, FILE = FILEIOH,STATUS = 'UNKNOWN',IOSTAT=ERRNUM)
          CLOSE (LUNIO,STATUS = 'DELETE')
        ENDIF

C-----------------------------------------------------------------------
C    Open BATCH file
C-----------------------------------------------------------------------
        IF (INDEX('NQSFBETY',RNMODE) .GT. 0) THEN
           CALL GETLUN('BATCH ', LUNBIO)
           FINDCH='$BATCH'
           OPEN (LUNBIO, FILE = FILEB,STATUS = 'UNKNOWN',IOSTAT=ERRNUM)
           IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,28,FILEB,LINBIO)
           CALL FIND (LUNBIO,FINDCH,LINBIO,IFIND)
           IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,26,FILEB,LINBIO)
        ENDIF
      ENDIF 

C-----------------------------------------------------------------------
C    Set run number and replication number
C-----------------------------------------------------------------------
      RUN   = 0
      REPNO = 1
      CONTROL % REPNO = REPNO

C*********************************************************************** 
C*********************************************************************** 
C     RUN INITIALIZATION
C***********************************************************************
      RUN_LOOP: DO WHILE (.NOT. DONE)
      YREND = -99
      RUN = RUN + 1
      CONTROL % RUN = RUN
      CONTROL % YRDOY = 0
      CALL PUT(CONTROL)

      IF ((INDEX('NSFBTY',RNMODE) .GT. 0) .OR. 
     &    (INDEX('E',RNMODE) .GT. 0 .AND. RUN .EQ. 1)) THEN
        CALL IGNORE (LUNBIO,LINBIO,ISECT,CHARTEST)
        IF (ISECT .EQ. 1) THEN
          END_POS = LEN(TRIM(CHARTEST(1:92)))+1
          FILEX = CHARTEST((END_POS-12):(END_POS-1))
          PATHEX = CHARTEST(1:END_POS-13)
          READ(CHARTEST(93:113),110,IOSTAT=ERRNUM) TRTNUM,TRTREP,ROTNUM
 110      FORMAT(3(1X,I6))
          IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,26,FILEB,LINBIO)
        ELSE
          DONE = .TRUE.
          GO TO 2000
        ENDIF
      ENDIF

      IF (INDEX('Q',RNMODE) .GT. 0) THEN
        CALL IGNORE (LUNBIO,LINBIO,ISECT,CHARTEST)
        IF (ISECT .EQ. 0 .OR. RUN .EQ. 1) THEN
          REWIND(LUNBIO)
          FINDCH='$BATCH'
          CALL FIND (LUNBIO,FINDCH,LINBIO,IFIND)
          CALL IGNORE (LUNBIO,LINBIO,ISECT,CHARTEST)
        ENDIF
        END_POS = INDEX(CHARTEST,BLANK)
        FILEX = CHARTEST((END_POS-12):(END_POS-1))
        PATHEX = CHARTEST(1:END_POS-13)
        READ (CHARTEST(93:113),110,IOSTAT=ERRNUM) TRTNUM,TRTREP,ROTNUM
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,26,FILEB,LINBIO)
      ENDIF

      CONTROL % FILEIO  = FILEIO
      CONTROL % FILEX   = FILEX
      CONTROL % RNMODE  = RNMODE
      CONTROL % ROTNUM  = ROTNUM
      CONTROL % TRTNUM  = TRTNUM
      CONTROL % ERRCODE = 0
      CALL PUT(CONTROL)

C-KRT**************************************************************
      IF (RNMODE .EQ. 'A') THEN
        PATHEX=""
      ENDIF
C-KRT*************************************************************

C-----------------------------------------------------------------------
C    Run INPUT module
C-----------------------------------------------------------------------
      IF (RNMODE .NE. 'D') THEN
        CALL INPUT_SUB(
     &    FILECTL, FILEIO, FILEX, MODELARG, PATHEX,       !Input
     &    RNMODE, ROTNUM, RUN, TRTNUM,                    !Input
     &    ISWITCH, CONTROL)                               !Output
      ELSE
        FILEX = '            '    !Debug mode - no FILEX
        CALL PATHD  (DSSATP,PATHX,LEN_TRIM(PATHX))
        CONTROL % DSSATP = DSSATP
      ENDIF
C-----------------------------------------------------------------------
C    Check to see if the temporary file exists
C-----------------------------------------------------------------------
      INQUIRE (FILE = FILEIO,EXIST = FEXIST)
      IF (.NOT. FEXIST) THEN
        CALL ERROR(ERRKEY,2,FILEIO,LUNIO)
      ENDIF

      OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,0)
      READ (LUNIO,300,IOSTAT=ERRNUM) EXPNO,TRTNUM,TRTALL
 300  FORMAT(36X,3(1X,I5))
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,1)
      READ (LUNIO,'(//,15X,A12)',IOSTAT=ERRNUM) FILEX
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,1)
      IF (RUN .EQ. 1) THEN
        READ(LUNIO,'(8(/),A6,9X,A8)',IOSTAT=ERRNUM) FINDCH, FNAME
        IF (RNMODE .EQ. 'Y' .AND. FINDCH .NE. 'OUTPUT') THEN
!       There might be 2 weather files listed for yield forecast mode
          READ(LUNIO,'(A6,9X,A8)',IOSTAT=ERRNUM) FINDCH, FNAME
        ENDIF
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,13)
        READ(LUNIO,400,IOSTAT=ERRNUM) NYRS, NREPS, YRSIM
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,15)
 400    FORMAT(/,15X,I5,1X,I5,7X,I7)
      ELSE IF (RNMODE .NE. 'Q') THEN
        REWIND (LUNIO)
        FINDCH = '*SIMUL'
        CALL FIND(LUNIO, FINDCH, LNUM, FOUND) 
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(FINDCH, 42, FILEIO, LNUM)
        ELSE
          READ(LUNIO,500,IOSTAT=ERRNUM) NYRS, NREPS, YRSIM
          IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,15)
 500      FORMAT(15X,I5,1X,I5,7X,I7)
          LNUM = LNUM + 1
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
        ENDIF
      ENDIF
      CLOSE(LUNIO)

      IF (NYRS > 1) THEN
        YRSIM_SAVE = YRSIM
      ENDIF

      IF (INDEX('FQ',RNMODE) .GT. 0) THEN
        IF (RUN .EQ. 1) THEN
          CALL YR_DOY(YRSIM,YR,ISIM)
          YRDOY_END = (YR + NYRS) * 1000 + ISIM
          YRDOY_END = INCYD(YRDOY_END, -1)
        ENDIF
        NYRS  = 1
      ENDIF

      IF (INDEX('Y',RNMODE) .GT. 0) THEN
        REPNO = 1
      ENDIF

      IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1) THEN
        YRDOY = YRSIM
      ENDIF

      MULTI  = 0
      YRDIF  = 0
      ENDYRS = 0
      
      IF (INDEX('FQ',RNMODE).GT. 0 .AND. RUN .GT. 1) THEN
         YRSIM = INCYD(YRDOY,1)
         CALL YR_DOY(YRSIM_SAVE, YR0, ISIM0)
         CALL YR_DOY(YRSIM,      YR,  ISIM)
         YRDIF = YR - YR0
         CONTROL % YRDIF = YRDIF
      ENDIF
       
      CONTROL % FILEX   = FILEX
      CONTROL % NYRS    = NYRS
      CONTROL % MULTI   = MULTI
      CONTROL % RUN     = RUN
      CONTROL % TRTNUM  = TRTNUM
      CONTROL % YRDIF   = YRDIF
      CONTROL % YRDOY   = YRDOY
      CONTROL % YRSIM   = YRSIM
      CONTROL % DYNAMIC = RUNINIT
      CALL PUT(CONTROL)

      CALL RUNLIST(CONTROL)

      WRITE(MSG(1),'("RNMODE = ",A)')  RNMODE
      WRITE(MSG(2),'("PATHEX = ",A)')  PATHEX(1:67)
      WRITE(MSG(3),'("FILEX  = ",A)')  FILEX
      WRITE(MSG(4),'("FILEB  = ",A)')  FILEB
      WRITE(MSG(5),'("FILEIO = ",A)')  FILEIO
      WRITE(MSG(6),'("MODEL  = ",A)')  CONTROL % MODEL
      WRITE(MSG(7),'("TRTNUM = ",I5)') TRTNUM
      WRITE(MSG(8),'("ROTNUM = ",I5)') ROTNUM
      IF (INDEX('FQ',RNMODE) > 0) THEN
        CALL INFO(8,ERRKEY,MSG)
      ELSE
        CALL INFO(7,ERRKEY,MSG)
      ENDIF

      CALL LAND(CONTROL, ISWITCH, 
     &          YRPLT, MDATE, YREND)

C*********************************************************************** 
C*********************************************************************** 
C-----------------------------------------------------------------------
C     BEGINNING of SEASONAL SIMULATION loop
C-----------------------------------------------------------------------
C     SEASONAL INITIALIZATION
C*********************************************************************** 
      SEAS_LOOP: DO WHILE (ENDYRS .NE. NYRS)
C***********************************************************************
      IF (NYRS .GT. 1) THEN 
        ENDYRS = ENDYRS + 1
        IF (RNMODE .NE. 'Y') THEN
          MULTI = MULTI + 1
        ENDIF
      ELSE
        MULTI = 1
        ENDYRS = 1
      ENDIF

      IF (MULTI .GT. 1) THEN
        RUN   = RUN + 1
        CALL MULTIRUN(RUN, 0)  !chp 3/17/2011
        YRSIM = YRSIM_SAVE
        CALL YR_DOY(YRSIM,YR,ISIM)
        YRSIM = (YR + MULTI - 1) * 1000 + ISIM
        YREND = -99
        IF (CONTROL%ErrCode /= 0) THEN
          CONTROL%ErrCode = 0
!         EXIT SEAS_LOOP
          IF (INDEX('QY',RNMODE) > 0) EXIT SEAS_LOOP
        ENDIF
      ENDIF

!     Forecast mode
      IF (RNMODE .EQ. 'Y') THEN
        IF (ENDYRS .GT. 1) THEN
          RUN = RUN + 1
          REPNO = REPNO + 1
          CALL MULTIRUN(RUN, 0)  
          YREND = -99
        ENDIF
      ENDIF

      IF (RNMODE .NE. 'Q' .OR. RUN .GT. 1) THEN
        YRDOY = YRSIM
      ENDIF
      
      CONTROL % DAS     = 0
      CONTROL % RUN     = RUN
      CONTROL % YRSIM   = YRSIM
      CONTROL % YRDOY   = YRDOY
      CONTROL % MULTI   = MULTI
      CONTROL % DYNAMIC = SEASINIT
      CONTROL % ENDYRS  = ENDYRS
      CONTROL % REPNO   = REPNO
      CALL PUT(CONTROL)
   
      CALL LAND(CONTROL, ISWITCH, 
     &          YRPLT, MDATE, YREND)

      YRDOY = INCYD(YRDOY,-1)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     BEGINNING of DAILY SIMULATION loop
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DAY_LOOP: DO WHILE (YRDOY .GT. YREND)
C-----------------------------------------------------------------------
C     Increment day (YRDOY)
C-----------------------------------------------------------------------
      YRDOY = INCYD(YRDOY,1)

C-----------------------------------------------------------------------
C     Calculate days after simulation (DAS) 
C-----------------------------------------------------------------------
      CALL YR_DOY(YRDOY,YEAR,DOY)
!     DAS   = MAX(0,TIMDIF(YRSIM,YRDOY))
      DAS   = MAX(0,TIMDIF(INCYD(YRSIM,-1),YRDOY))
      CONTROL % YRDOY   = YRDOY
      CONTROL % DAS     = DAS
C*********************************************************************** 
C     RATE CALCULATIONS
C*********************************************************************** 
      CONTROL % DYNAMIC = RATE
      CALL PUT(CONTROL)

      CALL LAND(CONTROL, ISWITCH, 
     &          YRPLT, MDATE, YREND)

C*********************************************************************** 
C     INTEGRATION 
C*********************************************************************** 
      CONTROL % DYNAMIC = INTEGR
      CALL PUT(CONTROL)

      CALL LAND(CONTROL, ISWITCH, 
     &          YRPLT, MDATE, YREND)

C*********************************************************************** 
C     OUTPUT
C*********************************************************************** 
      CONTROL % DYNAMIC = OUTPUT
      CALL PUT(CONTROL)

      CALL LAND(CONTROL, ISWITCH, 
     &          YRPLT, MDATE, YREND)

C***********************************************************************
      ENDDO DAY_LOOP   !End of daily loop
C-----------------------------------------------------------------------
C     END of DAILY SIMULATION loop
C----------------------------------------------------------------------
C*********************************************************************** 
C     End of Season 
C*********************************************************************** 
      CONTROL % DYNAMIC = SEASEND
      CALL PUT(CONTROL)

      CALL LAND(CONTROL, ISWITCH, 
     &          YRPLT, MDATE, YREND)

C-----------------------------------------------------------------------
      ENDDO SEAS_LOOP  
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     END of SEASONAL SIMULATION loop
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C    Check to see if all treatments have been run for RNMODE = 'A'
C-----------------------------------------------------------------------
      I = INDEX('A', RNMODE)
      IF (INDEX('A',RNMODE) .GT. 0 .AND. TRTNUM .GE. TRTALL) THEN
         DONE = .TRUE.
      
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      ELSE IF (INDEX('GDC',RNMODE) .GT. 0) THEN
        DONE = .TRUE.

      ELSE IF (INDEX('FQ',RNMODE).GT. 0 .AND. YRDOY .GE. YRDOY_END) THEN
        REPNO = REPNO + 1
        CONTROL % REPNO = REPNO
        IF (REPNO .GT. NREPS) THEN
          DONE = .TRUE.
        ELSE
          RUN = 0
        ENDIF

      ELSE IF (INDEX('IE',RNMODE) .GT. 0) THEN
        WRITE(*,1700)
 1700   FORMAT(/,1X,'Do you want to run more simulations ? ',
     &         /,1X,'Y or N ? [Default = "N"] ===> ',$)
        READ (5,1800) ANS
 1800   FORMAT(A1)
        ANS = UPCASE(ANS)
        IF (ANS .NE. 'Y') DONE = .TRUE.
      ENDIF

 2000 CONTINUE
      ENDDO RUN_LOOP 

!     Final end-of-run call to land unit module
      CONTROL % DYNAMIC = ENDRUN
      CALL PUT(CONTROL)
      CALL LAND(CONTROL, ISWITCH, 
     &          YRPLT, MDATE, YREND)

      !Change output file names if FNAME set
      CALL OPNAMES(FNAME)

      CALL RUNLIST(CONTROL)

      END PROGRAM CSM 

!===========================================================================
! Variable listing for main program
! ---------------------------------
! BLANK   Blank character 
! CONTROL Composite variable containing variables related to control and/or 
!           timing of simulation.  The structure of the variable 
!           (ControlType) is defined in ModuleDefs.for. 
! DAS     Days after start of simulation (d)
! DONE    Logical variable. TRUE if all runs have been completed. FALSE 
!           otherwise. 
! DOY     Current day of simulation (d)
! DSCSM   Name of CSM model executable (i.e., DSCSM040.EXE)
! ERRKEY  Subroutine name for error file 
! ERRNUM  Error number for input 
! EXPNO   Experiment number 
! FEXIST  Logical variable 
! FILEARG Run-time argument which contains name of input file (either 
!           FILEIO, FILEB or FILEX depending on run mode). 
! FILEB   Name of batch file (i.e., D4batch.dv4) 
! FILEIO  Filename for input file (e.g., IBSNAT35.INP) 
! FILEX   Experiment file, e.g., UFGA7801.SBX 
! FNAME   Output file name, usually 'OVERVIEW' 
! I       Loop counter 
! INPUT   Name of input module executable (i.e., MINPT040.EXE) 
! INPUTX  Command line for system call to run input module. 
! IP      Return status of GETARG command 
! IPX     Length of path plus filename for CSM executable 
! ISECT   Indicator of completion of IGNORE routine: 0 - End of file 
!           encountered, 1 - Found a good line to read, 2 - End of Section 
!           in file encountered denoted by * in column 1. 
! ISIM    Day portion of Julian date 
! ISWITCH Composite variable containing switches which control flow of 
!           execution for model.  The structure of the variable 
!           (SwitchType) is defined in ModuleDefs.for. 
! LN      Pest number 
! LUNIO   Logical unit number for FILEIO 
! MDATE   Harvest maturity date (YYYYDDD)
! MULTI   Current simulation year (=1 for first or single simulation, =NYRS 
!           for last seasonal simulation) 
! NREPS   Number of replications for sequenced simulation 
! NYRS    Number of years of simulations to perform for multi-season run 
!           (each with identical intitial conditions, but different weather 
!           years) 
! REPNO   Replication number for current simulation 
! RNMODE    Simulation run mode (I=Interactive, A=All treatments, 
!             B=Batch mode, E=Sensitivity, D=Debug, N=Seasonal, Q=Sequence)
! RUN     Change in date between two observations for linear interpolation 
! TRTNUM   Treatment number being simulated (from FILEX) 
! YEAR    Year of current date of simulation 
! YR      Year portion of date 
! YRDIF   Increment in years which must be added to operations dates for 
!           seasonal or sequenced simulations (yr)
! YRDOY   Current day of simulation (YYYYDDD)
! YREND   Date for end of season (usually harvest date) (YYYYDDD)
! YRPLT   Planting date (YYYYDDD)
! YRSIM   Start of simulation date (YYYYDDD)
!===========================================================================
