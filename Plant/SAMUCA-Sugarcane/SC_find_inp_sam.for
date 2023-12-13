
      !-----------------------------!
      ! *** Copied from CANEGRO *** !
      !-----------------------------!
      ! Murilo Vianna at 01/09/2020 !
      !-----------------------------!
      
c     MJ, November 2006
c     ::::::::::::::::::
c     This subroutine looks up variables from the input file 
c     (DSSAT45.INP)
c     ===============================================================
      SUBROUTINE find_inp_sam(var, varname, Control)
          USE ModuleDefs
          IMPLICIT NONE
          EXTERNAL GETLUN, FIND, ERROR
          SAVE


          TYPE(CONTROLTYPE) CONTROL
          REAL var
          CHARACTER*(*) varname
c         Planting method
          CHARACTER*1   PLTRAT

          INTEGER LUNIO, ERR, LNUM
          CHARACTER*6     ERRKEY          
          PARAMETER       (ERRKEY='SC_CNG')   
          CHARACTER*30    FILEIO 
          CHARACTER*12    FILEC     
       !chp   CHARACTER*92    FILECC
          CHARACTER*12    FILES
          CHARACTER*12    FILEE 
          CHARACTER*80    PATHCR 
          CHARACTER*80    PATHSR
          CHARACTER*80    PATHER 

          REAL PLTPOP,ROWSPC, plantdepth !, dummy
          CHARACTER*6     SECTION 
          INTEGER         LINC
          INTEGER         FOUND  

c         Has the file been read?
          LOGICAL HASBEENREAD
      

c     ===============================================================
c     CODE
c     ===============================================================
c     Set variables
      HASBEENREAD = .FALSE.
      FILEIO  = CONTROL % FILEIO

c     Open inp file:
c     NOTE: code copied (with permission) from MZ_GROSUB - thanks 
c           Cheryl!
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF (.NOT.(HASBEENREAD)) THEN
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

          READ(LUNIO,51,IOSTAT=ERR) FILEC, PATHCR; LNUM = LNUM + 1 
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
   51     FORMAT(15X,A12,1X,A80)
          !------------------------------------------------------
          !   Read Planting Details Section
          !------------------------------------------------------
          SECTION = '*PLANT'
          CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
          IF (FOUND .EQ. 0) THEN
            CALL ERROR(SECTION, 42, FILEIO, LNUM)
          ELSE
c           Read plant population, rowspacing, and planting method
            READ(LUNIO,62,IOSTAT=ERR) PLTPOP, PLTRAT, ROWSPC,plantdepth
            LNUM = LNUM + 1
 60         FORMAT(25X,F5.2,13X,F5.2,7X,F5.2)
c           Added by MJ, 2007-05-04
 61       FORMAT(25X,F5.2,5X, A1,7X,F5.2,7X,F5.2)
          !--- Added by MV, 09-jan-2020 (to read the plating depth)
62        FORMAT(25X,F5.2,5X, A1,7X,F5.2,7X,F5.2,7X,F5.2,7X,F5.2)
   
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
          ENDIF

c         Set variable so that file is not reread for every parameter
c         request
          HASBEENREAD = .TRUE.
C     -----------------------------------------------------------------
C             Read crop cultivar coefficients
C     -----------------------------------------------------------------
c          SECTION = '*CULTI'
c          CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
c          IF (FOUND .EQ. 0) THEN
c              CALL ERROR(SECTION, 42, FILEIO, LNUM)
c          ELSE
c            READ (LUNIO,1800,IOSTAT=ERR) VARNO,VRNAME,ECONO,
c     %                 P1,P2,P5,G2,G3,PHINT  
!CHP 1800        FORMAT (A6,1X,A16,1X,A6,1X,F6.1,F6.3,2(F6.1),2(F6.2))    
c1800        FORMAT (A6,1X,A16,1X,A6,1X,6F6.0)    
c            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
c          ENDIF

        CLOSE(LUNIO)
      ENDIF

c     ===============================================================
c         Search for requested parameter:
c         :::::::::::::::::::::::::::::::
      SELECT CASE (varname)
c         Row spacing:
c         ::::::::::::
          CASE ('ROWSPC')
              var = rowspc/100.

          CASE ('RATOON')
c             If a ratoon is specifically selected, then use ratoon
              IF (PLTRAT .EQ. 'R') THEN
                  var = 1.
              ELSE
c             Any other form of planting will be treated as seed(cane) = plant
                  var = 0.
              ENDIF

          CASE ('PLTPOP')
              var = PLTPOP
              
          case('PLDP')
              var = plantdepth
              
      END SELECT

      CONTINUE

      END
	  