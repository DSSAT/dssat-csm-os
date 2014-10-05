!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.RUNINIT) ! Initialization lines 2576 - 2647 of the original CSCAS code. the
! names of the dummy arguments are the same as in the original CSCAS code and the call statement. the variables are described
! in CSCAS.
!
! This subroutine creates the file names for the genotypw files.
!***************************************************************************************************************************

    
    SUBROUTINE CS_Cult_Eco_Spp ( &
        RNMODE       &
        )
        
        !USE CRSIMDEF                                                                MF 15SE14 Declared in ModuleDefs
        USE Module_CSCAS_Vars_List
        
        IMPLICIT NONE
        
        CHARACTER (LEN=1)   RNMODE   
        
        !-----------------------------------------------------------------------
        !       Create genotype file names
        !-----------------------------------------------------------------------
        
        IF (FILEIOT(1:2).EQ.'DS') THEN
            ! Cultivar
            PATHL = INDEX(PATHCR,' ') 
            IF (PATHL.LE.5.OR.PATHCR(1:3).EQ.'-99') THEN
                CUDIRFLE = CUFILE
            ELSE
                IF (PATHCR(PATHL-1:PATHL-1) .NE. SLASH) THEN          
                    CUDIRFLE = PATHCR(1:(PATHL-1)) // SLASH // CUFILE   
                ELSE
                    CUDIRFLE = PATHCR(1:(PATHL-1)) // CUFILE
                ENDIF
            ENDIF
            ! Ecotype
            PATHL = INDEX(PATHEC,' ')
            IF (PATHL.LE.5.OR.PATHEC(1:3).EQ.'-99') THEN
                ECDIRFLE = ECFILE
            ELSE
                IF (PATHEC(PATHL-1:PATHL-1) .NE. SLASH) THEN          
                    ECDIRFLE = PATHEC(1:(PATHL-1)) // SLASH // ECFILE   
                ELSE
                    ECDIRFLE = PATHEC(1:(PATHL-1)) // ECFILE
                ENDIF
            ENDIF
            ! Species
            PATHL = INDEX(PATHSP, ' ')
            IF (PATHL.LE.5.OR.PATHSP(1:3).EQ.'-99') THEN
                SPDIRFLE = SPFILE
            ELSE
                IF (PATHSP(PATHL-1:PATHL-1) .NE. SLASH) THEN          
                    SPDIRFLE = PATHSP(1:(PATHL-1)) // SLASH // SPFILE
                ELSE
                    SPDIRFLE = PATHSP(1:(PATHL-1)) // SPFILE
                ENDIF
            ENDIF
        ELSE
            IF (CUDIRFLE.NE.CUDIRFLPREV .OR. VARNO.NE.VARNOPREV) THEN
                ! Cultivar
                CUFILE = CROP//MODNAME(3:8)//'.CUL'
                INQUIRE (FILE = CUFILE,EXIST = FFLAG)
                IF (FFLAG) THEN
                    CUDIRFLE = CUFILE
                ELSE
                    CALL FINDDIR (FNUMTMP,CFGDFILE,'CRD',CUFILE,CUDIRFLE)
                ENDIF
                IF (RNMODE.EQ.'G'.OR.RNMODE.EQ.'T') THEN
                    CUFILE = 'GENCALC2.CUL'
                    CUDIRFLE = ' '
                    CUDIRFLE(1:12) = CUFILE
                ENDIF
                ! Ecotype
                ECFILE = CROP//MODNAME(3:8)//'.ECO'
                INQUIRE (FILE = ECFILE,EXIST = FFLAG)
                IF (FFLAG) THEN
                    ECDIRFLE = ECFILE
                ELSE
                    CALL FINDDIR (FNUMTMP,CFGDFILE,'CRD',ECFILE,ECDIRFLE)
                ENDIF
                IF (RNMODE.EQ.'G'.OR.RNMODE.EQ.'T') THEN
                    ECFILE = 'GENCALC2.ECO'
                    ECDIRFLE = ' '
                    ECDIRFLE(1:12) = ECFILE
                ENDIF
                ! Species
                SPFILE = CROP//MODNAME(3:8)//'.SPE'
                INQUIRE (FILE = SPFILE,EXIST = FFLAG)
                IF (FFLAG) THEN
                    SPDIRFLE = SPFILE
                ELSE
                    CALL FINDDIR (FNUMTMP,CFGDFILE,'CRD',SPFILE,SPDIRFLE)
                ENDIF
            ENDIF
        ENDIF     ! End Genotype file names creation
        
        !-----------------------------------------------------------------------
        !       Check for cultivar number, genotype files' existance and version
        !-----------------------------------------------------------------------
        
        IF (VARNO.EQ.'-99   ') THEN
            OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
            WRITE(fnumerr,*)' '
            WRITE(fnumerr,*)'Cultivar number not found '
            WRITE(fnumerr,*)'Maybe an error in the the X-file headings'
            WRITE(fnumerr,*)'(eg.@-line dots connected to next header)'
            WRITE(fnumerr,*)' (OR sequence or crop components > 1,no 1)'
            WRITE(fnumerr,*)'Please check'
            WRITE (*,*) ' Problem reading the X-file'
            WRITE (*,*) ' Cultivar number not found '
            WRITE (*,*) ' Maybe an error in the the X-file headings'
            WRITE (*,*) ' (eg.@-line dots connected to next header)'
            WRITE (*,*) ' (OR sequence or crop components > 1,no 1)'
            WRITE (*,*) ' Program will have to stop'
            WRITE (*,*) ' Check WORK.OUT for details'
            CLOSE (fnumerr)
            STOP ' '
        ENDIF
        
        INQUIRE (FILE = CUDIRFLE,EXIST = FFLAG)
        IF (.NOT.(FFLAG)) THEN
            WRITE(fnumwrk,*) ' '
            WRITE(fnumwrk,*) 'Cultivar file not found!     '
            WRITE(fnumwrk,*) 'File sought was:          '  
            WRITE(fnumwrk,*) Cudirfle(1:78)
            WRITE(fnumwrk,*) 'Will search in the working directory for:'
            CUDIRFLE = CUFILE
            WRITE(fnumwrk,*)  Cudirfle(1:78)
            INQUIRE (FILE = CUDIRFLE,EXIST = FFLAG)
            IF (.NOT.(FFLAG)) THEN
                OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
                WRITE(fnumerr,*) ' '
                WRITE(fnumerr,*) 'Cultivar file not found!     '
                WRITE(fnumerr,*) 'File sought was:          '  
                WRITE(fnumerr,*) Cudirfle(1:78)
                WRITE(fnumerr,*) 'Please check'
                WRITE (*,*) ' Cultivar file not found!     '
                WRITE(*,*) 'File sought was:          '
                WRITE(*,*) Cudirfle(1:78)
                WRITE(*,*) ' Program will have to stop'
                CLOSE (fnumerr)
                STOP ' '
            ENDIF
        ENDIF
        
        INQUIRE (FILE = ECDIRFLE,EXIST = FFLAGEC)
        IF (.NOT.(FFLAGEC)) THEN
            WRITE(fnumwrk,*) ' '
            WRITE(fnumwrk,*) 'Ecotype file not found!     '
            WRITE(fnumwrk,*) 'File sought was: ',Ecdirfle(1:60)  
            ECDIRFLE = ECFILE
            WRITE(fnumwrk,*) 'Will search in the working directory for:',Ecdirfle(1:60)
            INQUIRE (FILE = ECDIRFLE,EXIST = FFLAGEC)
            IF (.NOT.(FFLAGEC)) THEN
                OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
                WRITE(fnumwrk,*) 'File not found in working directory!'
                WRITE(fnumwrk,*) 'Please check'
                WRITE(*,*) ' Ecotype file not found!     '
                WRITE(*,*) ' File sought was: ',Ecdirfle(1:60)
                WRITE(*,*) ' Program will have to stop'
                STOP ' '
            ENDIF
        ENDIF
        
        INQUIRE (FILE = SPDIRFLE,EXIST = FFLAG)
        IF (.NOT.(FFLAG)) THEN
            OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
            WRITE(fnumerr,*) ' '
            WRITE(fnumerr,*) 'Species file not found!     '
            WRITE(fnumerr,*) 'File sought was:          '
            WRITE(fnumerr,*) Spdirfle
            WRITE(fnumerr,*) 'Please check'
            WRITE(*,*) ' Species file not found!     '
            WRITE(*,*) 'File sought was:          '
            WRITE(*,*) Spdirfle
            WRITE(*,*) ' Program will have to stop'
            CLOSE (fnumerr)
            STOP ' '
        ENDIF
        
    END SUBROUTINE CS_Cult_Eco_Spp