!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == RUNINIT) ! Initialization lines 2530 - 2570 of the original CSCAS code. The 
! names of the dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The 
! variables that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are 
! those of the original CSCAS.FOR code.
!
!Subroutine YCA_SeasInit_PlHarvDat sets the planting and harvest dates.
!***************************************************************************************************************************

    SUBROUTINE YCA_SeasInit_PlHarvDat ( &
        DOY         , YEAR         &
        )
      
      USE YCA_First_Trans_m
      
      IMPLICIT NONE
      EXTERNAL YR_DOY
      
      INTEGER DOY         , YEAR 
      
      !-----------------------------------------------------------------------
      !       Set planting/harvesting dates (Will change if runs repeated)
      !-----------------------------------------------------------------------
      
      ! CHP 5/4/09 - for DSSAT runs, always set PLYEAR = YEAR
      ! CHP 09/28/09 - account for planting date >> simulation date.
      !LPM 07/17/20 - account for simulation date when is a year before planting date
      !Avoid wrong value of yeardoyharf
      IF (FILEIOT(1:2) == 'DS' .AND. YEAR > PLYEAR) THEN
          IF (YEAR < PLYEARREAD) THEN
              PLYEAR = PLYEARREAD
              PLYEARTMP = PLYEARREAD
          ELSE
              PLYEAR = YEAR
              PLYEARTMP = YEAR
          ENDIF
      ENDIF
      
      ! Check final harvest date for seasonal runs        
!      CALL CSYR_DOY(YEARDOYHARF,HYEAR,HDAY)
      CALL YR_DOY(YEARDOYHARF,HYEAR,HDAY)
      PLTOHARYR = HYEAR - PLYEARREAD
      ! Upgrade harvest date for seasonal and sequential runs
      !yeardoyharf = (plyear+pltoharyr)*1000 +hday
      !LPM 10FEB2021 Move yeardoyharf after updating the PLYEAR when date of simulation is 
      !the year before planting
      
!     IF (IPLTI /= 'A') THEN
      IF (IPLTI /= 'A' .AND. IPLTI /= 'F') THEN
          IF (PLDAY >= DOY) THEN
              PLYEARDOYT = PLYEARTMP*1000 + PLDAY
          ELSEIF (PLDAY < DOY) THEN
              PLYEARDOYT = (YEAR+1)*1000 + PLDAY
              PLYEAR = YEAR+1
          ENDIF
      ELSE
          PLYEARDOYT = 9999999
          IF (PWDINF > 0 .AND. PWDINF < YEARDOY) THEN
              TVI1 = INT((YEARDOY-PWDINF)/1000)
              PWDINF = PWDINF + TVI1*1000
              PWDINL = PWDINL + TVI1*1000
              IF (HFIRST > 0) HFIRST = HFIRST + TVI1*1000
              IF (HLAST > 0)  HLAST  = HLAST + (TVI1+1)*1000
          ENDIF
      ENDIF
      yeardoyharf = (plyear+pltoharyr)*1000 +hday
      !-----------------------------------------------------------------------
      !       Set control flags if not already done
      !-----------------------------------------------------------------------
      
      IF (CFLLFLIFE == 'T'.OR.CFLLFLIFE == 'P'.OR.CFLLFLIFE == 'D') THEN
          CFLLFLIFE = CFLLFLIFE
      ELSE  
          CFLLFLIFE = 'T'  ! Default to thermal time 
      ENDIF
    END SUBROUTINE YCA_SeasInit_PlHarvDat 