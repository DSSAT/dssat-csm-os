!CALL ExperimentFileEdit(FILEIT, YRSIM, EDATE,& 
!                ISWWAT, ISWNIT, PLME, PAGE, PLPH, PLYPOP,PLANTS, PLANTS, PLDP, &
!                 IIRRI, IRRCOD,IRMTAB, RIRRIT, IRRI, WL0MIN, KPAMIN, SLMIN, WLODAY, ISTAGET, TMCTB)

SUBROUTINE ExperimentFileEdit(OUTPUTFILE, YRSIM, EDATE, & 
                PRODENV, NITROENV, ESTAB,  SBDUR, NPLH, PLTPOP, NPLSB, NPLDS, ZRTTR, &  !PLANT ESTABLISHMENT AND DENSITY
                IIRRI, IRRCOD, IRMTAB, RIRRIT, IRRI, WL0MIN, KPAMIN, SLMIN, WLODAY, ISTAGET, &  !FOR IRRIGATION
                TMCTB, &  !Other parameters 
                LAPE, DVSI, WLVGI, WSTI, WRTI, WSOI, ZRTI)  !initial biomass

IMPLICIT NONE

!   CHP 4.19/2011 Changed SLMIN from integer to real
CHARACTER*(*) OUTPUTFILE 
CHARACTER*(*) PRODENV, NITROENV, ESTAB, IIRRI
INTEGER IYEAR, SBDUR, ICOMBA, I, J, J1, WL0DAY, YRSIM, EDATE, EMYR, EMD, STTIME, IRRCOD
!CHP - flexible unit numbers - DSSAT uses lots of unit numbers - need to avoid conflicts.
INTEGER LUN

REAL NPLH, NH, PLYPOP,NPLSB, NPLDS, ZRTTR, WL0MIN, KPAMIN, WCMIN, IRRI, SLMIN
REAL IRMTAB(300), RIRRIT(750),ISTAGET(900), TMCTB(750)
REAL DELT, PLTPOP, WLOMIN, WLODAY
REAL LAPE, DVSI, WLVGI, WSTI, WRTI, WSOI, ZRTI  !initial biomass

!Get assigned unit number  CHP
CALL GETLUN('OUTPUTFILE',LUN)

IYEAR = INT(YRSIM/1000.0);STTIME = YRSIM-IYEAR*1000.0
EMYR = INT(EDATE/1000.0);EMD = EDATE-EMYR*1000.0
IF((IYEAR.GT.EMYR).OR.((IYEAR.EQ.EMYR).AND.(STTIME.GT.EMD))) THEN
    WRITE(*,*) 'Simulation starting date is later than emergence date. Setting to emergence date.'
    IYEAR =EMYR; STTIME = EMD
END IF
!OUTPUTFILE = "Test.exp"
DELT = 1.0
WCMIN = 0.0
WLOMIN = 0.0
WLODAY = 0

OPEN(UNIT=LUN, FILE = OUTPUTFILE, STATUS='REPLACE',ACTION='WRITE')
    WRITE(LUN, '(A)') "*--------------------------------------------------------------------*"
    WRITE(LUN, '(A)') "* 1. Selection of modes of running                                   *"
    WRITE(LUN, '(A)') "*--------------------------------------------------------------------*"
    WRITE(LUN, '(A)') "*--  RUNMODE is mode of running ORYZA"
    WRITE(LUN, '(A)') "RUNMODE = 'EXPERIMENT'       ! ORYZA simulates particular experiment"
    WRITE(LUN, '(A)') "*--  PRODENV is Water production situation setting"
    CALL UPPERC(PRODENV)
    IF(INDEX(PRODENV, "N").GT.0) THEN
        WRITE(LUN, '(A)') "PRODENV = 'POTENTIAL'       ! 'POTENTIAL' for potential production or 'WATER BALANCE' for production may be water-limited"
        WRITE(LUN, '(A)') "*WATBAL = 'PADDY'    ! PADDY water balance (for lowland soils)"
    ELSE
        WRITE(LUN, '(A)') "PRODENV = 'WATER BALANCE'       ! 'POTENTIAL' for potential production or 'WATER BALANCE' for production may be water-limited"
        WRITE(LUN, '(A)') "WATBAL = 'PADDY'    ! PADDY water balance (for lowland soils)"
    ENDIF
    WRITE(LUN, '(A)') "*--  NITROENV is Nitrogen production situation setting"
    CALL UPPERC(NITROENV)
    IF(INDEX(NITROENV, "Y").GT.0) THEN
        WRITE(LUN, '(A)') "NITROENV = 'NITROGEN BALANCE'       ! Production may be nitrogen-limited"
    ELSE
        WRITE(LUN, '(A)') "NITROENV = 'POTENTIAL'       ! for potential production OF NITROGEN"
    ENDIF
    WRITE(LUN, '(A)') "*--------------------------------------------------------------------*"
    WRITE(LUN, '(A)') "* 2. Timer data for simulation                                       *"
    WRITE(LUN, '(A)') "*--------------------------------------------------------------------*"
    WRITE(LUN, 6000) "IYEAR",  IYEAR, "      ! Start year of simulation (year)"
    WRITE(LUN, 6000) "STTIME",  STTIME,".      ! Start time  (day number)"
    WRITE(LUN, '(A)') "FINTIM = 7000"               ! Finish time (days after start)"
    WRITE(LUN, 7000) "DELT", DELT, "                  ! Time step   (day)"
    WRITE(LUN, '(A)') "TMCTB =  0., 0.,             ! Table for temperature increase"
    WRITE(LUN, '(A)') "366., 0.              ! Climatic Change studies"
    WRITE(LUN, '(A)') "TMPSB = 0.          ! Temperature increase in seed-bed due to cover;"
    WRITE(LUN, '(A)') "! Zero when no cover over seed-bed; 9.5 with seed-bed"
    
    WRITE(LUN, '(A)') "*--------------------------------------------------------------------*"
    WRITE(LUN, '(A)') "* 3. Establishment data"
    WRITE(LUN, '(A)') "*--------------------------------------------------------------------*"
    WRITE(LUN, '(A)') "*--  ESTAB is method of establishment; 'TRANSPLANT' or 'DIRECT-SEED'"
    CALL UPPERC(ESTAB)
    IF(INDEX(ESTAB, "T").GT.0) THEN
        WRITE(LUN, '(A)') "ESTAB='TRANSPLANT'    !TRANSPLANT or DIRECT SEEDING"
    ELSE
        WRITE(LUN, '(A)') "ESTAB='DIRECT-SEED'    !TRANSPLANT or DIRECT SEEDING"    
    END IF
    WRITE(LUN, '(A)') "* Transplanting date May 25 (145), 2001; sowing date April 15; 50% emergence April 29 (119)"
    WRITE(LUN, 6000) "EMD", EMD, "     ! Day of emergence (either direct, or in seed-bed)"
    WRITE(LUN, 6000) "EMYR", EMYR, "     ! Year of emergence"
    WRITE(LUN, 6000) "SBDUR", SBDUR, "      ! Seed-bed duration (days between emerging and transplanting)"
    
    WRITE(LUN, '(A)') "*--------------------------------------------------------------------*"
    WRITE(LUN, '(A)') "* 4. Management parameters                                           *"
    WRITE(LUN, '(A)') "*--------------------------------------------------------------------*"
    WRITE(LUN, 7000) "NPLH", NPLH, "        ! Number of plants per hill"
    WRITE(LUN, 7000) "NH", PLTPOP/NPLH       ! Number of hills/m2 (13 x 27 cm)"
    WRITE(LUN, 7000) "NPLSB", NPLSB, "      ! Number of plants in seed-bed (???)"    
    WRITE(LUN, 7000) "NPLDS", NPLDS, "      ! Number of plants/m2 direct-seeded"
 
    WRITE(LUN, '  ')   
    WRITE(LUN, '(A)') "*-- Initial data at emergence, for either direct-seeding or seed-bed"
    WRITE(LUN, '(A)') "*   Standard data used."
    WRITE(LUN, '(A,F8.5,A)') "LAPE   = ", LAPE, "      ! Initial leaf area per plant"
    WRITE(LUN, '(A,F8.3,A)') "DVSI   = ", DVSI, "      ! Initial development stage"
    WRITE(LUN, '(A,F8.3,A)') "WLVGI  = ", WLVGI,"      ! Initial leaf weight"
    WRITE(LUN, '(A,F8.3,A)') "WSTI   = ", WSTI, "      ! Initial stem weight"
    WRITE(LUN, '(A,F8.3,A)') "WRTI   = ", WRTI, "      ! Initial stem weight"
    WRITE(LUN, '(A,F8.3,A)') "WSOI   = ", WSOI, "      ! Initial weight storage organs"
    WRITE(LUN, '(A,F8.5,A)') "ZRTI   = ", ZRTI, "      ! Initial root depth (m)"
    
    WRITE(LUN, '  ')   
    WRITE(LUN, '(A)') "*-- Re-initialization at transplanting (standard data used)"
    WRITE(LUN, 7000) "ZRTTR", ZRTTR/100.0, "       ! Root depth at transplanting (m)"
    
    WRITE(LUN, '(A)') "*---------------------------------------------------------------*"
    WRITE(LUN, '(A)') "* 5. Irrigation parameters"
    WRITE(LUN, '(A)') "* Need only to be filled-in when PRODENV = 'WATER BALANCE'"
    WRITE(LUN, '(A)') "*---------------------------------------------------------------*"
    WRITE(LUN, '(A)') "DVSIMAX = 2.0 ! Development stage after which no more irrigation is applied"
    WRITE(LUN, '(A)') "* The determination for switch critical. 1; Use Julian day; 2; Use DVS and"
    WRITE(LUN, '(A)') "*3; Use mixture of DVS and Julian day, but the Julian day is not allowed to be smaller than 2"
    CALL UPPERC(IIRRI)
    IF((IIRRI.EQ."R").OR.(IIRRI.EQ."P").OR.(IIRRI.EQ."W").OR.(IIRRI.EQ."N")) THEN
        WRITE(LUN, '(A)') "ICOMBA = 1" 
    ELSEIF(IIRRI.EQ."D") THEN
        WRITE(LUN, '(A)') "ICOMBA = 4"
    ELSEIF((IIRRI.EQ."A").OR.(IIRRI.EQ."F")) THEN
        WRITE(LUN, '(A)') "ICOMBA = 2"
    ELSE
        WRITE(LUN, '(A)') "ICOMBA = 1" 
    END IF !CAN BE MORE OPTIONS
    WRITE(LUN, '(A)') "*  Combining irrigation management methods table IRMTAB, it must have at least two lines,"
    WRITE(LUN, '(A)') "*      X (Julian day or DVS or DVS+Julian, present the switching day), Y (methods in real number)"
    J = INT(SIZE(IRMTAB)); J1=0
    DO I = j-1, 1, -2
        IF(IRMTAB(I).GT.0.0) THEN
            j1=int((i+1)/2.0)
            EXIT
        END IF
    END DO
    J=J1 
    IF(J.LT.2) THEN
        WRITE(LUN, 3000) "IRMTAB", 0.0,IRMTAB(2)
        WRITE(LUN, 3100) 366.0, IRMTAB(2)
    ELSEIF(J.EQ.2) THEN
        WRITE(LUN, 3000) "IRMTAB", IRMTAB(1),IRMTAB(2)
        WRITE(LUN, 3100) IRMTAB(3), IRMTAB(4)
    ELSE
        WRITE(LUN, 3000) "IRMTAB", IRMTAB(1),IRMTAB(2)
        DO I=2, J-1
            WRITE(LUN, 3200) IRMTAB((I-1)*2+1),IRMTAB((I-1)*2+2)
        END DO 
        WRITE(LUN, 3100) IRMTAB((J-1)*2+1), IRMTAB((J-1)*2+2)
    END IF
    
    WRITE(LUN, '(A)') "** Select from the following options;"
    WRITE(LUN, '(A)') "*SWITIR = 0 ! No irrigation; rainfed"
    WRITE(LUN, '(A)') "*SWITIR = 1 ! Irrigation supplied as input data"
    WRITE(LUN, '(A)') "*SWITIR = 2 ! Irrigation at minimum standing soil water depth"
    WRITE(LUN, '(A)') "*SWITIR = 3 ! Irrigation at minimum soil water potential"
    WRITE(LUN, '(A)') "*SWITIR = 4 ! Irrigation at minimum soil water content"
    WRITE(LUN, '(A)') "*SWITIR = 5 ! Irrigation at x days after disapp. standing water"
    WRITE(LUN, '(A)') "*SWITIR = 6 ! Irrigation at minimum soil water potential in defined periods only"
    WRITE(LUN, '(A)') ""
    
    IF(SIZE(RIRRIT)>=4) THEN
        WRITE(LUN, '(A)') "*If SWITIR = 1, supply irrigation table, amount of irrigation"
        WRITE(LUN, '(A)') "*(y in mm) for a given calendar * day (x), used if"
        J = INT(SIZE(RIRRIT));J1=1
        DO I = J-1, 1, -2
            IF(RIRRIT(I).GT.0.0) THEN
                J1=INT((I+1)/2.0)
                EXIT
            END IF
        END DO 
        J=J1
        IF(J.LT.2) THEN
            WRITE(LUN, 3000) "RIRRIT", 0.0,RIRRIT(2)
            WRITE(LUN, 3100) 366.0, RIRRIT(2)
        ELSEIF(J.EQ.2) THEN
            WRITE(LUN, 3000) "RIRRIT", RIRRIT(1),RIRRIT(2)
            WRITE(LUN, 3100) RIRRIT(3), RIRRIT(4)
        ELSE
            WRITE(LUN, 3000) "RIRRIT", RIRRIT(1),RIRRIT(2)
            DO I=2, J-1
                WRITE(LUN, 3200) RIRRIT((I-1)*2+1),RIRRIT((I-1)*2+2)
            END DO 
            WRITE(LUN, 3100) RIRRIT((J-1)*2+1), RIRRIT((J-1)*2+2)
        END IF
    END IF
    
    WRITE(LUN, '(A)') "** If SWITIR = 2;"
    WRITE(LUN, '(A)') "***1) supply amount of irrigation IRRI2 (mm)"
    WRITE(LUN, '(A)') "***2) supply minimum standing water depth WL0MIN (mm) below which irrigation water is applied"
    
    IF(WL0MIN.GT.0.0) THEN
        WRITE(LUN, '(A)') "** If SWITIR = 2;"
        WRITE(LUN, '(A)') "***1) supply amount of irrigation IRRI2 (mm)"
        WRITE(LUN, '(A)') "***2) supply minimum standing water depth WL0MIN (mm) below which irrigation water is applied"  
        WRITE(LUN, 7000) "IRRI2", IRRI, " ! Irrigation gift (mm) !IT MUST BE REAL DATA"  
        WRITE(LUN, 7000) "WL0MIN", WLOMIN, "   ! Minimum standing water depth (mm) !IT MUST BE REAL DATA"
    END IF
    IF((KPAMIN.GT.0.0).AND.(SLMIN.GT.0.0)) THEN
        WRITE(LUN, '(A)') "** IF SWITIR =3;"
        WRITE(LUN, '(A)') "***1) supply amount of irrigation IRRI3 (mm)"
        WRITE(LUN, '(A)') "***2) supply minimum soil water potential KPAMIN (KPa)"
        WRITE(LUN, '(A)') "***3) Supply soil layer for which KPAMIN aplied, SLMIN3"
        WRITE(LUN, 7000) "IRRI3", IRRI
        WRITE(LUN, 7000) "KPAMIN", KPAMIN,"            !IT MUST BE REAL DATA"
        WRITE(LUN, 6000) "SLMIN3", SLMIN, "            !IT MUST BE INTEGER DATA"
    END IF
    IF((WCMIN.GT.0.0).AND.(SLMIN.GT.0.0)) THEN
        WRITE(LUN, '(A)') "** IF SWITIR = 4;"
        WRITE(LUN, '(A)') "***1) supply amount of irrigation IRRI4 (mm)"
        WRITE(LUN, '(A)') "***2) supply minimum soil water conten WCAMIN (-)"
        WRITE(LUN, '(A)') "***3) Supply soil layer for which KPAMIN aplied, SLMIN4"
        WRITE(LUN, 7000) "IRRI4", IRRI
        WRITE(LUN, 7000) "WCMIN", WCMIN, "      !IT MUST BE REAL DATA"
        WRITE(LUN, 6000) "SLMIN4", SLMIN, "      !IT MUST BE INTEGER DATA"
    END IF
    IF(WLODAY.GT.0.0) THEN
        WRITE(LUN, '(A)') "** IF SWITIR = 5;"
        WRITE(LUN, '(A)') "***1) supply amount of irrigation IRRI5 (mm)"
        WRITE(LUN, '(A)') "***2) supply number of days after disappearence of standing water (WL0DAY) at which irrigation water is applied"
        WRITE(LUN, 7000) "IRRI5", IRRI
        WRITE(LUN, '(A)') "WL0DAY = 5    ! number of days after disappearence of (-) INTEGER!!"
    END IF
    IF(SIZE(ISTAGET).GT.1) THEN
        WRITE(LUN, '(A)') "** IF SWITIR = 6;"
        WRITE(LUN, '(A)') "***1) supply amount of irrigation IRRI6 (mm)"
        WRITE(LUN, '(A)') "***2) Supply soil layer for which KPAMIN aplied, SLMIN6"
        WRITE(LUN, '(A)') "***3) period table as; Start; DVS ' 'finish DVS' 'KPAMIN during period'"
        WRITE(LUN, '(A)') "*       Irrigation will be applied in the periods between 'start DVs' to 'end DVS'"
        WRITE(LUN, '(A)') "*       and only when the soil water tension in layer SLMIN is above KPAMIN in that period"
        WRITE(LUN, '(A)') "*       Note; at maximum 5 stages can de defined (no more than 15 data in table)!" 
        WRITE(LUN, 7000) "IRRI6", IRRI
        WRITE(LUN, 6000) "SLMIN6", SLMIN, "   !Layer for threshold value!"
        j=int(SIZE(ISTAGET));J1=0
        DO I = J-2, 1, -3
            IF(ISTAGET(I).GT.0.0) THEN
                J1=INT((I+2)/3.0)
                EXIT
            END IF
        END DO
        J=J1 
        IF(J.LT.2) THEN
!            PRINT *, "Data is not the multiple of 3. please check it"
        elseif (j.eq.2) then
            WRITE(LUN, 1000) "ISTAGET", ISTAGET(1), ISTAGET(2), ISTAGET(3)  
            WRITE(LUN, 1100) ISTAGET(4), ISTAGET(5), ISTAGET(6)
        ELSE
            WRITE(LUN, 1000) "ISTAGET", ISTAGET(1), ISTAGET(2), ISTAGET(3) 
            DO I=2, J-1
                WRITE(LUN, 1200) "ISTAGET", ISTAGET((I-1)*3+1), ISTAGET((I-1)*3+2), ISTAGET((I-1)*3+3)
            END DO 
            WRITE(LUN, 1100) ISTAGET((J-1)*3+1), ISTAGET((J-1)*3+2), ISTAGET((J-1)*3+3)
            WRITE(LUN, '(A)') "1.50, 1.60, 50.,"
            WRITE(LUN, '(A)') "1.70, 1.80, 5."
        END IF
    end if
    !WRITE(LUN, '(A)') ""
    !WRITE(LUN, '(A)') "*--------------------------------------------------------------------*"
    !WRITE(LUN, '(A)') "* 6. Nitrogen parameters                                             *"
    !WRITE(LUN, '(A)') "*--------------------------------------------------------------------*"
    !WRITE(LUN, '(A)') "*TWO SOIL C AND N DYNAMICS"
    !WRITE(LUN, '(A)') "NUTRIENT = '"//ADJUSTL(TRIM(NUTRIENT))//",    !GENERAL SOM' AND 'APSIM SOILN"     
    !WRITE(LUN, '(A)') "* Table of recovery fraction of Nitrogen in the soil (-) second column"
    !WRITE(LUN, '(A)') "* versus development stage (DVS) (first column) STANDARD VALUE"
    !WRITE(LUN, '(A)') "RECNIT ="
    !WRITE(LUN, '(A)') "0.0, 0.30,"
    !WRITE(LUN, '(A)') "0.2, 0.35,"
    !WRITE(LUN, '(A)') "0.4, 0.50,"
    !WRITE(LUN, '(A)') "0.8, 0.75,"
    !WRITE(LUN, '(A)') "1.0, 0.75,"
    !WRITE(LUN, '(A)') "2.5, 0.75"
    !WRITE(LUN, '(A)') ""
    !WRITE(LUN, '(A)') "* NO DATA ON SOILSP; THIS 0.8 IS FOR IRRI CONDITIONS IN THE DS......"
    !WRITE(LUN, '(A)') "SOILSP = 0.8  ! Soil N mineralization rate (kg N/ha/d)"
    !WRITE(LUN, '(A)') ""
    !WRITE(LUN, '(A)') "* Table of fertilizer rate (kg N/ha) (second column) versus days after sowing"
    !WRITE(LUN, '(A)') "* in the seed-bed (!) (first column)"
    !IF(SIZE(FERTIL).GE.4) THEN
    !    J=INT(SIZE(FERTIL)/2.0)
    !    IF(J.LE.2) THEN
    !        WRITE(LUN, 4000) "FERTIL", FERTIL(1), FERTIL(2)
    !        WRITE(LUN,4100) FERTIL(3), FERTIL(4)
    !    ELSE
    !        WRITE(LUN, 4000) "FERTIL", FERTIL(1), FERTIL(2)
    !        DO I=2, J-1
    !            WRITE(LUN, 4200) FERTIL((I-1)*2+1), FERTIL((I-1)*2+2)
    !        END DO
    !        WRITE(LUN,4100) FERTIL((J-1)*2+1), FERTIL((J-1)*2+2)
    !    END IF
    !ELSE
    !    WRITE(LUN, '(A)') "*FERTIL = "
    !END IF 
    Close (LUN)
1000 FORMAT(A8, "=",2(f6.2,","),f6.2)
1100 FORMAT(A8, "=",3(f6.2,","))
1200 FORMAT(A8, "=",3(F6.2,","))   !CHP fixed runtime error, may have caused other problems
2000 FORMAT(A8, "=",2(I2,","),I2)
2100 FORMAT(A8, "=",3(i2,","))
2200 FORMAT(3(I2,","))
2300 FORMAT(2(I2,","),I2)
3000 FORMAT(A8, "=", 2(F8.4,","))       !FOR FIRST ROW OF TWO COLUMN TABLE VALUE WITH REAL VALUE
3100 FORMAT((F8.4,","), F8.4)          !FOR LAST ROW OF TWO COLUMN TABLE VALUE WITH REAL VALUE
3200 FORMAT(2(F8.4,","))          !FOR MIDDLE ROW OF TWO COLUMN TABLE VALUE WITH REAL VALUE
4000 FORMAT(A8, "=", 2(I2,","))       !FOR FIRST ROW OF TWO COLUMN TABLE VALUE  WITH INTEGER VALUE
4100 FORMAT((I2,","), I2)          !FOR LAST ROW OF TWO COLUMN TABLE VALUE WITH INTEGER VALUE
4200 FORMAT(2(I2,","))          !FOR MIDDLE ROW OF TWO COLUMN TABLE VALUE WITH INTEGER VALUE
5000 FORMAT('(A)')
6000 FORMAT(A8, "=",I4, A)
7000 FORMAT(A8, "=",F8.2, A)
END SUBROUTINE EXPERIMENTFILEEDIT