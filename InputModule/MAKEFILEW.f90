!=======================================================================
!  COPYRIGHT 1998-2020 DSSAT Foundation
!                      University of Florida, Gainesville, Florida
!                      Inernational Fertilizer Development Center
!  
!  ALL RIGHTS RESERVED
!=======================================================================
!=======================================================================
!  MAKEFILEW, Subroutine, Fabio Olivera, Willingthon Pavan, Gerrit Hoogenboom
!
! This subroutine open the FILEX and read the weather station characters (WSTA).
! Then create the weather filename and check if FILEW exists.
!-----------------------------------------------------------------------
!  Revision history
!
!  01/23/2020 FO  Written
!  11/10/2020 CHP Need to make revisions here like in IPEXP for forecast mode
!       This mode needs to keep FILEW (WTH file) and potentially also 
!       FILEWC (CLI) file and FILEWG (WTG file).
!  10/21/2021 FO  Updated GETLUN for weather file unit
!-----------------------------------------------------------------------
!  INPUT  : DSSATP, PATHEX, FILEX
!
!  OUTPUT : YEAR
!-----------------------------------------------------------------------
!=======================================================================

      SUBROUTINE MAKEFILEW(LUNEXP,DSSATP,PATHEX,FILEX,        &              
                    SimLevel,LNSIM,LNPLT,LNFLD)  !Input: Factor Levels

      USE ModuleDefs
      IMPLICIT      NONE
      EXTERNAL ERROR, WARNING, IGNORE2, GETLUN, FIND, HFIND, INFO, PATH

      CHARACTER*1   BLANK, WTHER
      CHARACTER*3   PROCOD
      CHARACTER*6   ERRKEY,SECTION
      CHARACTER*5   FINDH
      CHARACTER*8   WSTA,FILEW4,FILEX
      CHARACTER*12  FILEW,NAMEF, LastFileW
      CHARACTER*78  MSG(4)
      CHARACTER*80  PATHWT,PATHEX,CHARTEST
      CHARACTER*92  FILEWW,FILETMP
      CHARACTER*102 DSSATP
      CHARACTER*255 LINE

      LOGICAL       FEXIST,SimLevel
      
      INTEGER       LUNEXP,LINEXP,LNUM,ISECT,ERRNUM,PATHL,IFIND !,FOUND
      INTEGER       ICASAF,UNIT  !,LINWTH,LUNWTH
      INTEGER       SDATE,PDATE,YRDOY
      INTEGER       LNFLD,TLNFLD
      INTEGER       LNSIM,TLNSIM
      INTEGER       LNPLT,TLNPLT
      INTEGER       YEARDOY,YEAR,YR,DOY
      
      DATA LastFileW /" "/

      PARAMETER (ERRKEY = 'MAKEFW')
      PARAMETER (BLANK = ' ')
      
      
      SDATE = -99
      PDATE = -99
      WTHER = 'M'
      
!-----------------------------------------------------------------------
!    Open FILEX and read Weather station characters (WSTA)
!-----------------------------------------------------------------------

      LNUM   = 0
      LINEXP = 0
      !FO - Rewind because ipexp.for call FileX LUNEXP as a constant
      REWIND(LUNEXP)
      
      IF(LNFLD .GT. 0) THEN
        !FO - Find FIELD section to search for weather station
        SECTION ='*FIELD'
        CALL FIND (LUNEXP,SECTION,LINEXP,IFIND)
        IF(IFIND .NE. 0) THEN
          
          !FO - Find the specific section header
          FINDH ='L ID_'
          CALL HFIND(LUNEXP,FINDH,LINEXP,IFIND)
          IF(IFIND .EQ. 1) THEN
            !FO - Loop through good lines to find and read the weather station
            DO
              CALL IGNORE2(LUNEXP,LINEXP,ISECT,CHARTEST)
              
              IF(ISECT .EQ. 1) THEN
                READ (CHARTEST,'(I3,9X,A8)', IOSTAT=ERRNUM) TLNFLD,WSTA
                IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,4,FILEX,LINEXP)
                IF (WSTA(1:1) .EQ. BLANK) CALL ERROR (ERRKEY,3,FILEX,LINEXP)
              ELSE
                CALL ERROR (ERRKEY,3,FILEX,LINEXP)
              ENDIF
              
              !Until FL factor lvl equals to the current section.
              IF (TLNFLD .EQ. LNFLD .AND. LNFLD .GT. 0) THEN
                EXIT
              ENDIF
            ENDDO          
          ELSE
            !Error - If FINDH ='L ID_' not found
            CALL ERROR (ERRKEY,2,FILEX,LINEXP)  
          ENDIF
        ELSE
          !Error - If SECTION ='*FIELD' not found
          CALL ERROR (ERRKEY,2,FILEX,LINEXP)  
        ENDIF        
      ELSE
        !Error - If LNFLD  = 0
        CALL ERROR (ERRKEY,1,FILEX,LINEXP)
      ENDIF
      
!-----------------------------------------------------------------------
!    Read FILEX YRSIM and weather method from *SIMUALTION CONTROLS
!    Otherwise, read planting date for experiments (LNSIM = 0) 
!-----------------------------------------------------------------------
      
      LNUM   = 0
      LINEXP = 0
      !FO - Rewind FileX, because find always set LINEXP to 0
      REWIND(LUNEXP)

      !FO - Search for *SIMUL CONTROLS if SM factor level is gt 0
      IF(LNSIM .GT. 0 .AND. SimLevel) THEN
        
        !FO - Find *SIMULATION CONTROLS section to search for start simulation date
        !FO - DO/UNTIL loop is used to iterate until find the right section
        SECTION = '*SIMUL'
        CALL FIND (LUNEXP,SECTION,LINEXP,IFIND)
        LNUM = LNUM + LINEXP
        
        IF(IFIND .NE. 0) THEN
          DO
            !FO - Find the specific header to search for SDATE
            FINDH ='N GEN'
            CALL HFIND(LUNEXP,FINDH,LINEXP,IFIND)
            IF(IFIND .EQ. 1) THEN
              CALL IGNORE2(LUNEXP,LINEXP,ISECT,CHARTEST)      
              IF(ISECT .EQ. 1) THEN
                READ (CHARTEST,'(I2,30X,I6)', IOSTAT=ERRNUM) TLNSIM,SDATE
                IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,8,FILEX,LNUM)           
              ELSE
                CALL ERROR (ERRKEY,7,FILEX,LNUM)
              ENDIF
            ELSE
              !Error - If !FINDH = 'N GEN'
              CALL ERROR (ERRKEY,6,FILEX,LNUM)  
            ENDIF
            
            !FO - Find the specific header to search for WTHER
            FINDH ='N MET'
            CALL HFIND(LUNEXP,FINDH,LINEXP,IFIND)
            IF(IFIND .EQ. 1) THEN
              
              CALL IGNORE2(LUNEXP,LINEXP,ISECT,CHARTEST)  
              IF(ISECT .EQ. 1) THEN
                READ (CHARTEST,'(I2,17X,A1)', IOSTAT=ERRNUM) TLNSIM,WTHER
                IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,11,FILEX,LNUM)
                IF (WTHER .EQ. BLANK) CALL ERROR (ERRKEY,10,FILEX,LINEXP)            
              ELSE
                CALL ERROR (ERRKEY,10,FILEX,LNUM)
              ENDIF
            ELSE
              !Error - If !FINDH ='N MET'
              CALL ERROR (ERRKEY,9,FILEX,LNUM)
            ENDIF
            
            !Until SM factor lvl equals to the current section.
            IF (TLNSIM .EQ. LNSIM .AND. LNSIM .GT. 0) THEN
              EXIT
            ENDIF
          ENDDO
        ELSE
          !Error - If LNSIM = 0
          CALL ERROR (ERRKEY,6,FILEX,LNUM)
        ENDIF
      
      !FO - Search for *PLANTING DETAILS if MP factor level is gt 0  
      ELSE IF(LNPLT .GT. 0) THEN
        
        SECTION = '*PLANT'
        CALL FIND (LUNEXP,SECTION,LINEXP,IFIND)
        LNUM = LNUM + LINEXP
        
        IF(IFIND .NE. 0) THEN
          
          !FO - Find the specific header to search for PDATE
          FINDH ='P PDA'
          CALL HFIND(LUNEXP,FINDH,LINEXP,IFIND)
          IF(IFIND .EQ. 1) THEN
            DO
              CALL IGNORE2(LUNEXP,LINEXP,ISECT,CHARTEST)      
              IF(ISECT .EQ. 1) THEN
                
                READ (CHARTEST,'(I2,I6)', IOSTAT=ERRNUM) TLNPLT,PDATE
                IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,14,FILEX,LNUM)            
                
              ELSE
                !Error - If IGNORE2
                CALL ERROR (ERRKEY,13,FILEX,LNUM)
              ENDIF              
              
              !Until SM factor lvl equals to the current section.
              IF (TLNPLT .EQ. LNPLT .AND. LNPLT .GT. 0) THEN
                EXIT
              ENDIF
            ENDDO
          ELSE
            !Error - If !FINDH ='P PDA'
            CALL ERROR (ERRKEY,12,FILEX,LNUM)  
          ENDIF
        ELSE
          !Error - If !SECTION ='*PLANT'
          CALL ERROR (ERRKEY,12,FILEX,LNUM)
        ENDIF
        
      ELSE
        !Error - If both (SM, PLT) are not found
        CALL ERROR (ERRKEY,5,FILEX,LNUM)
      ENDIF

!-----------------------------------------------------------------------
!    Converts SDATE or PDATE to YR and DOY
!-----------------------------------------------------------------------
      IF(SDATE .GT. 0 ) THEN
        YRDOY = SDATE
      ELSEIF(PDATE .GT. 0) THEN
        YRDOY = PDATE
      ELSE
        CALL ERROR (ERRKEY,15,FILEX,LNUM)
      ENDIF
      
      !FO - Converts YRDOY to YR and DOY  
      YR  = INT(YRDOY / 1000)
      DOY = YRDOY - YR * 1000   
      
!-----------------------------------------------------------------------
!    Create WTH file name and path
!-----------------------------------------------------------------------
!    Establish the weather file FILEW as WSTA + .WT?  where ? :
!          M = observed data
!          G = generated data
!          S = interactively generated
!-----------------------------------------------------------------------
      IF(WTHER .EQ. 'M') THEN
        IF (WSTA(5:5) .EQ. BLANK) THEN
          WRITE (FILEW(1:12),'(A4,I2.2,A6)') WSTA,YR,'01.WTH'
        ELSE
          WRITE (FILEW(1:12),'(A8,A4)') WSTA,'.WTH'
        ENDIF
        PROCOD = 'WED'
      ELSEIF(WTHER .EQ. 'G') THEN
        
        IF (WSTA(5:5) .EQ. BLANK) THEN
          WRITE (FILEW(1:12),'(A4,I2.2,A6)') WSTA,YR,'01.WTG'
        ELSE
          WRITE (FILEW(1:12),'(A8,A4)') WSTA,'.WTG'
        ENDIF
        PROCOD = 'WGD'
      ELSEIF (WTHER .EQ. 'S' .OR. WTHER .EQ. 'W') THEN
         WRITE (FILEW(1:12),'(A4,A8)') WSTA,'.CLI    '
         PROCOD = 'CLD'
      ELSE
         CALL ERROR (ERRKEY,16,FILEX,LINEXP)
      ENDIF
            
!     Check weather filename in current directory
      INQUIRE (FILE = FILEW,EXIST = FEXIST)
      IF (FEXIST) THEN
        PATHWT = BLANK
!     Check weather filename in data directory
      ELSE
        FILETMP = TRIM(PATHEX)//FILEW
        INQUIRE (FILE = FILETMP,EXIST = FEXIST)
        IF (FEXIST) THEN
          PATHWT = TRIM(PATHEX)
!       Check weather filename in default DSSAT directory
        ELSE
          CALL PATH(PROCOD,DSSATP,PATHWT,1,NAMEF)
          FILETMP = TRIM(PATHWT) // FILEW
          INQUIRE (FILE=FILETMP, EXIST = FEXIST)
          IF (FEXIST) THEN
            PATHWT = PATHWT
!         Check 4-character file name in data directory
          ELSE
            FILEW4 = FILEW(1:4) // ".WTH"
            FILETMP = TRIM(PATHEX) // FILEW4
            INQUIRE (FILE=FILETMP, EXIST = FEXIST)
            IF (FEXIST) THEN
              PATHWT = TRIM(PATHEX)
              FILEW = FILEW4
!           Check 4-character filename in default DSSAT directory
            ELSE
              FILEW4 = FILEW(1:4) // ".WTH"
              FILETMP = TRIM(PATHWT) // FILEW4
              INQUIRE (FILE=FILETMP, EXIST = FEXIST)
              IF (FEXIST) THEN
                PATHWT = TRIM(PATHWT)
                FILEW = FILEW4
              ELSE
                MSG(1) = "Weather file not found."
                MSG(2) = "  Neither " // FILEW // " nor " // FILEW4
                MSG(3) = "  were found in weather or experiment directories."
                MSG(4) = "Simulation will end."
                CALL WARNING(4,ERRKEY,MSG)
                CALL ERROR(ERRKEY,18,FILEW,0)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      
!-----------------------------------------------------------------------
!    Open and Read Weather file data
!-----------------------------------------------------------------------
      PATHL  = INDEX(PATHWT,BLANK)
      IF (PATHL .LE. 1) THEN
         FILEWW = FILEW
       ELSE
         FILEWW = PATHWT(1:(PATHL-1)) // FILEW
      ENDIF
      
      !FO - Process FILEW if last FILEW is different.
      IF(LastFileW /= FILEW) THEN
      
        LastFileW = FILEW
        
        ICASAF  = 0
        YEAR    = -99
        DOY     = -99
        YEARDOY = -99
        
        CALL GETLUN('LUNMFW', UNIT)
        
        INQUIRE(FILE=FILEWW, EXIST = FEXIST)
        IF(FEXIST) THEN
          OPEN (UNIT, FILE = FILEWW,STATUS = 'OLD',IOSTAT=ERRNUM)
          IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,18,FILEW,0)
        ELSE
          CALL ERROR (ERRKEY,18,FILEW,0)
        ENDIF
              
        DO WHILE (ERRNUM .EQ. 0)
          READ (UNIT,'(A)', IOSTAT=ERRNUM) LINE
          
          !FO - Look for 1st header line beginning with '$'
          IF(INDEX(LINE, '$WEATHER') .GT. 0 .AND. ICASAF .EQ. 0) THEN
            ICASAF = 1  
          
          !FO - New Weather file format true read 7-digit. Otherwise keep the default.
          ELSEIF(ICASAF .EQ. 1) THEN
            
            !FO - Look for header lines beginning with '@'  
            IF(LINE(1:1) .EQ. '@' .AND. INDEX(LINE, 'DATE') .GT. 0) THEN
                !FO - Read 7-digit First Weather YEAR
                READ (UNIT,'(I7)', IOSTAT=ERRNUM) YEARDOY
                IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,17,FILEW,0)
                
                EXIT
            ELSE IF(LINE(1:1) .EQ. '@' .AND. INDEX(LINE, 'YEAR') .GT. 0) THEN
                !FO - Read ICASA format
                READ (UNIT,'(I4,1X,I3)', IOSTAT=ERRNUM) YEAR, DOY 
                IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,17,FILEW,0)
                
                YEARDOY = YEAR * 1000 + DOY
                
                EXIT
              ENDIF
            ENDIF  
            
        ENDDO
        
        CLOSE(UNIT)
        
!-----------------------------------------------------------------------
!    Convert SDATE to 7-digits and check if is in the date range 
!-----------------------------------------------------------------------      
        
        IF (YEARDOY .GT. 0 .AND. ICASAF .EQ. 1) THEN
          FirstWeatherDate = YEARDOY
          
          YEARDOY = INT(FirstWeatherDate/100000) * 100000 + YRDOY
          
          IF(YEARDOY .GE. FirstWeatherDate) THEN
            NEWSDATE = YEARDOY
          ELSE
            NEWSDATE = INT((FirstWeatherDate+99000)/100000)*100000+YRDOY
          ENDIF
          
          ! Error Checking
          IF(NEWSDATE .LT. FirstWeatherDate .OR. NEWSDATE .GT. FirstWeatherDate+99000) THEN
            CALL ERROR (ERRKEY,19,FILEX,0)
          ENDIF
          
        ELSE
          MSG(1) = 'Y4KDOY - Weather file'
          MSG(2) = 'Not able to find 7-digits first weather date'
          MSG(3) = 'Set simulation to 5-digits first weather date'
          FirstWeatherDate = -99
          
          CALL INFO(3,ERRKEY,MSG)
        ENDIF
      
      ENDIF
      
    END SUBROUTINE MAKEFILEW

