!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.RUNINIT) ! Initialization lines 2530 - 2570 of the original CSCAS code. the
! names of the dummy arguments are the same as in the original CSCAS code and the call statement. the variables are described
! in CSCAS.
!
! This subroutine sete the planting and harvest dates.
!
!***************************************************************************************************************************


    SUBROUTINE CS_Plant_harv_Dat ( &
        DOY         , YEAR         &
        )
      
      !USE CRSIMDEF                                                                MF 15SE14 Declared in ModuleDefs
      USE Module_CSCAS_Vars_List
      
      IMPLICIT NONE
      
      INTEGER DOY         , YEAR 
      
      !-----------------------------------------------------------------------
      !       Set planting/harvesting dates (Will change if runs repeated)
      !-----------------------------------------------------------------------
      
      ! CHP 5/4/09 - for DSSAT runs, always set PLYEAR = YEAR
      ! CHP 09/28/09 - account for planting date >> simulation date.
      IF (FILEIOT(1:2).EQ.'DS' .AND. YEAR > PLYEAR) THEN
          PLYEAR = YEAR
          PLYEARTMP = YEAR
      ENDIF
      
      ! Check final harvest date for seasonal runs        
      CALL CSYR_DOY(YEARDOYHARF,HYEAR,HDAY)
      PLTOHARYR = HYEAR - PLYEARREAD
      ! Upgrade harvest date for seasonal and sequential runs
      yeardoyharf = (plyear+pltoharyr)*1000 +hday
      
      IF (IPLTI.NE.'A') THEN
          IF (PLDAY.GE.DOY) THEN
              PLYEARDOYT = PLYEARTMP*1000 + PLDAY
          ELSEIF (PLDAY.LT.DOY) THEN
              PLYEARDOYT = (YEAR+1)*1000 + PLDAY
          ENDIF
      ELSE
          PLYEARDOYT = 9999999
          IF (PWDINF.GT.0 .AND. PWDINF.LT.YEARDOY) THEN
              TVI1 = INT((YEARDOY-PWDINF)/1000)
              PWDINF = PWDINF + TVI1*1000
              PWDINL = PWDINL + TVI1*1000
              IF (HFIRST.GT.0) HFIRST = HFIRST + TVI1*1000
              IF (HLAST.GT.0)  HLAST  = HLAST + (TVI1+1)*1000
          ENDIF
      ENDIF
      
      !-----------------------------------------------------------------------
      !       Set control flags if not already done
      !-----------------------------------------------------------------------
      
      IF (CFLLFLIFE.EQ.'T'.OR.CFLLFLIFE.EQ.'P'.OR.CFLLFLIFE.EQ.'D') THEN
          CFLLFLIFE = CFLLFLIFE
      ELSE  
          CFLLFLIFE = 'T'  ! Default to thermal time 
      ENDIF
    END SUBROUTINE CS_Plant_harv_Dat 