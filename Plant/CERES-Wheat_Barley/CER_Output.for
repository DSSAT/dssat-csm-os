!**********************************************************************
! Subroutine CER_Output takes all the code from original CSCER code 
! lines 6134 - 8983.
!**********************************************************************

      SUBROUTINE CER_Output (OLAB, LAI, CANHT, CN, DOY,
     &     DYNAMIC, EOP, IDETG, IDETL, IDETO, IDETS,
     &     ISWNIT, ISWWAT, NFP, ON, REP,
     &     RLV, RN, RNMODE, RUN, RUNI, SN, STEP, STGDOY,
     &     TOTIR, TN, YEAR)

! 2023-01-25 CHP removed unused variables from argument list
!     NLAYR, CO2, RAIN, UNH4ALG, UNO3ALG, 

        USE ModuleDefs
        USE CSVOUTPUT  ! VSH
        USE CER_First_Trans_m

        IMPLICIT NONE
        EXTERNAL YR_DOY, GETLUN, SUMVALS, HEADER, TVILENT, TVICOLNM, 
     &    TL10FROMI, LTRIM, CSTIMDIF, CSOPLINE, CALENDAR, DAPCALC, 
     &    LTRIM2, AREADR, AREADI, CSYDOY, GETSTRI, CSCLEAR5, GETSTR, 
     &    GETSTRR, TIMDIF, READA_Y4K, READA_Dates
        SAVE
        
        INTEGER CN, DOY, DYNAMIC, ON, REP, RN !NLAYR
        INTEGER RUN, RUNI, SN, STGDOY(20), TN, YEAR
        INTEGER CSTIMDIF, STEP, TRT_ROT
        INTEGER TVICOLNM, TVILENT, CSYDOY, DAPCALC
        INTEGER IFLR, IJDAT, SEDAT, LFDAT, IADAT, ITDAT, IGS059
        INTEGER IDDAT, IEDT, IMAT, ISENS
        
        REAL LAI, CANHT, NFP, RLV(20) !, RAIN
        REAL EOP !CO2, UNH4ALG(20), UNO3ALG(20), 
        REAL TOTIR
        
        CHARACTER(LEN=1) IDETG, IDETL, IDETO, IDETS, ISWNIT, ISWWAT 
        CHARACTER(LEN=1) RNMODE     
        CHARACTER(LEN=10) TL10FROMI
        
        CHARACTER*6, DIMENSION(EvaluateNum) :: OLAB, OLAP
        CHARACTER*12 X(EvaluateNum)
        CHARACTER*80 PATHEX
        
        TYPE (ControlType) CONTROL
        
        OLAP = OLAB
        
        IF (YEARDOY.GE.YEARPLT .AND. STEP.EQ.STEPNUM) THEN             
          ! General file header
          IF (TN.LT.10) THEN
            WRITE (OUTHED,7104) RUNRUNI(1:5),EXCODE,TN,TRUNNAME
 7104       FORMAT ('*RUN ',A5,':',A10,' ',I1,' ',A40,'  ')
          ELSEIF (TN.GE.10. AND. TN.LT.100) THEN
           WRITE (OUTHED,7105) RUNRUNI,EXCODE,TN,RN,TRUNNAME
 7105      FORMAT ('*RUN ',A5,A10,' ',I2,',',I1,' ',A40,' ')
          ELSEIF (TN.GE.10 .AND. TN.LT.100) THEN
           WRITE (OUTHED,7106) RUNRUNI,EXCODE,TN,RN,TRUNNAME
 7106      FORMAT ('*RUN ',A5,A10,' ',I3,',',I1,' ',A40)
          ENDIF
 
          ! If seeding day
          IF (YEARDOY.EQ.STGDOY(7)) THEN
            CNCHAR = ' '
            IF (CN.EQ.1) THEN
              OUTPG = 'PlantGro.OUT'
              OUTPG2 = 'PlantGr2.OUT'
              OUTPGF = 'PlantGrf.OUT'
              OUTPN = 'PlantN.OUT  '

! File names are changed at end of simulation by CSM
! Changing names here eliminates wheat output in sequence runs.
              !IF (FNAME.EQ.'Y') THEN
              !  OUTPG = EXCODE(1:8)//'.OPG'
              !  OUTPG2 = EXCODE(1:8)//'.OG2'
              !  OUTPGF = EXCODE(1:8)//'.OGF'
              !  OUTPN = EXCODE(1:8)//'.ONI'
              !ENDIF

              CALL GETLUN ('PlantGro.OUT',NOUTPG)
              CALL GETLUN ('PlantN.OUT',NOUTPN)
              CALL GETLUN ('PlantGr2.OUT',NOUTPG2)
              CALL GETLUN ('PlantGrf.OUT',NOUTPGF)
            ELSE
              CNCHAR = TL10FROMI(CN)
              OUTPG = 'PlantGro.OU'//CNCHAR(1:1)
              OUTPN = 'PlantN.OU'//CNCHAR(1:1)
              CALL GETLUN (OUTPG,NOUTPG)
              CALL GETLUN (OUTPN,NOUTPN)
              CALL GETLUN (OUTPG2,NOUTPG2)
              CALL GETLUN (OUTPGF,NOUTPGF)
            ENDIF
            ! Open output file(s)
            IF (RUN.EQ.1 .AND. RUNI.LE.1) THEN
              OPEN (UNIT = NOUTPG, FILE = OUTPG)
              WRITE (NOUTPG,'(A27)')
     &        '$GROWTH ASPECTS OUTPUT FILE'
              OPEN (UNIT = NOUTPG2, FILE = OUTPG2)
              WRITE (NOUTPG2,'(A37)')
     &        '$GROWTH ASPECTS SECONDARY OUTPUT FILE'
              OPEN (UNIT = NOUTPN, FILE = OUTPN)
              WRITE (NOUTPN,'(A35)')
     &        '$PLANT NITROGEN ASPECTS OUTPUT FILE'
              OPEN (UNIT = NOUTPGF, FILE = OUTPGF)
              WRITE (NOUTPGF,'(A26)')
     &        '$GROWTH FACTOR OUTPUT FILE'
              CLOSE (NOUTPG)
              CLOSE (NOUTPG2)
              CLOSE (NOUTPN)
              CLOSE (NOUTPGF)
            ENDIF

            IF (IDETG.NE.'N'.AND.IDETL.NE.'0') THEN
              OPEN (UNIT = NOUTPG, FILE = OUTPG, STATUS='UNKNOWN',
     &        POSITION = 'APPEND')
              OPEN (UNIT = NOUTPN, FILE = OUTPN, STATUS = 'UNKNOWN',
     &        POSITION = 'APPEND')
     
              IF (FILEIOT(1:2).EQ.'DS') THEN
                CALL HEADER(2, NOUTPG, RUN)
                CALL HEADER(2, NOUTPN, RUN)
              ELSE
               WRITE (NOUTPG,'(/,A77,/)') OUTHED
               WRITE (NOUTPN,'(/,A77,/)') OUTHED
               WRITE (NOUTPG,103) MODEL
               WRITE (NOUTPN,103) MODEL
  103          FORMAT (' MODEL            ',A8)
               WRITE (NOUTPG,1031) MODNAME
               WRITE (NOUTPN,1031) MODNAME
 1031          FORMAT (' MODULE           ',A8)
               WRITE (NOUTPG,104)
     &          EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
               WRITE (NOUTPN,104)
     &          EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
  104          FORMAT (' EXPERIMENT       ',A8,A1,A2,A2,A47)
               WRITE (NOUTPG,102) TN,TNAME
               WRITE (NOUTPN,102) TN,TNAME
  102          FORMAT (' TREATMENT',I3,'     ',A25)
               WRITE (NOUTPG,107) CROP,VARNO,VRNAME
               WRITE (NOUTPN,107) CROP,VARNO,VRNAME
  107          FORMAT (' GENOTYPE         ',A2,A6,'  ',A16)
               CALL Calendar (year,doy,dom,month)
               WRITE (NOUTPG,108) month,dom,NINT(pltpop),NINT(rowspc)
               WRITE (NOUTPN,108) month,dom,NINT(pltpop),NINT(rowspc)
  108          FORMAT(' ESTABLISHMENT    ',A3,I3,2X,I4,' plants/m2 in ',
     &          I3,' cm rows',/)
              ENDIF
              ! Write variable headings
              WRITE (NOUTPG,2201)
 2201         FORMAT ('@YEAR DOY   DAS   DAP TMEAN TKILL',
     &        '  GSTD  L#SD',
     &        ' PARID PARUD  AWAD',
     &        '  LAID  SAID  CAID',
     &        '  TWAD SDWAD  RWAD  CWAD  LWAD  SWAD  HWAD  HIAD',
     &        ' CHWAD  EWAD RSWAD SNWPD SNWLD SNWSD',
     &        '  RS%D',
     &        '  H#AD  HWUD',
     &        '  T#AD  SLAD  RDPD  PTFD',
     &        '  SWXD WAVRD',
     &        ' WUPRD  WFTD  WFPD  WFGD',
     &        '  NFTD  NFPD  NFGD NUPRD',
     &        '  TFPD  TFGD',
     &        ' VRNFD DYLFD')
     
              WRITE (NOUTPN,2251)
!             2021-02-15 chp Change NUAD to NUAC in header.
 2251         FORMAT ('@YEAR DOY   DAS   DAP TMEAN  GSTD  NUAC',
     &        '  TNAD SDNAD  RNAD  CNAD  LNAD  SNAD  HNAD   HIND',
     &        '  RSNAD  SNNPD SNN0D SNN1D',
     B        '  RN%D  LN%D  SN%D   HN%D  SDN%D   VN%D',
     C        ' LN%RD SN%RD RN%RD  VCN%  VMN% NUPRD',
     &        ' NDEMD')
  
              ! Delete PlantN if N switched off   
              IF (ISWNIT.NE.'Y') THEN
                CLOSE (UNIT=NOUTPN, STATUS = 'DELETE')
              ENDIF
     
              OPEN (UNIT = NOUTPG2, FILE = OUTPG2, STATUS='UNKNOWN',
     &        POSITION = 'APPEND')
              IF (FILEIOT(1:2).EQ.'DS') THEN
                CALL HEADER(2, NOUTPG2, RUN)
              ELSE
                WRITE (NOUTPG2,'(/,A79,/)') OUTHED
                WRITE (NOUTPG2,103) MODEL
                WRITE (NOUTPG2,1031) MODNAME
                WRITE (NOUTPG2,104)
     &           EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
                WRITE (NOUTPG2,102) TN,TNAME
                WRITE (NOUTPG2,107) CROP,VARNO,VRNAME
                WRITE(NOUTPG2,108)
     &           month,dom,yearplt,NINT(pltpopp),NINT(rowspc)
              ENDIF 
              WRITE (NOUTPG2,2252)
 2252         FORMAT ('@YEAR DOY   DAS   DAP TMEAN  GSTD  RSTD',
     A        ' LAIPD LAISD  LAID  CHTD SDWAD SNWLD SNWSD',
     a        '  H#AD  HWUD',
     B        ' SHRTD  PTFD  RDPD',
     C        '  RL1D  RL2D  RL3D  RL4D  RL5D  RL6D',
     D        '  RL7D  RL8D  RL9D RL10D')
     
            ! PlantGroReductionFactors
              OPEN (UNIT = NOUTPGF, FILE = OUTPGF, STATUS='UNKNOWN',
     &        POSITION = 'APPEND')  !chp 4/16/10
              IF (FILEIOT(1:2).EQ.'DS') THEN
                CALL HEADER(2, NOUTPGF, RUN)
              ELSE
                WRITE (NOUTPGF,'(/,A79,/)') OUTHED
                WRITE (NOUTPGF,103) MODEL
                WRITE (NOUTPGF,1031) MODNAME
                WRITE (NOUTPGF,104)
     &         EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
                WRITE (NOUTPGF,102) TN,TNAME
                WRITE (NOUTPGF,107) CROP,VARNO,VRNAME
                CALL Calendar (year,doy,dom,month)
                WRITE(NOUTPGF,108)
     &           month,dom,yearplt,NINT(pltpopp),NINT(rowspc)
              ENDIF
              WRITE (NOUTPGF,2215)
 2215         FORMAT ('!........DATES.......  TEMP STAGE ',
     N        ' ...PHENOLOGY.... ',
     1        ' .......PHOTOSYNTHESIS....... ', 
     M        ' .....GROWTH.....  ..TILLERS. ',
     2        'WATER STRESS DETERMINANTS',
     2        ' N STRESS DETERMINANTS       ')
              WRITE (NOUTPGF,2205)
 2205         FORMAT ('@YEAR DOY   DAS   DAP TMEAN  GSTD',
     N        '    DU VRNFD DYLFD TFGEM  WFGE',
     1        '  TFPD  WFPD  NFPD CO2FD RSFPD', 
     M        '  TFGD  WFGD  NFGD  WFTD  NFTD',
     &        ' WAVRD WUPRD  SWXD  EOPD',
     &        '  SNXD LN%RD SN%RD RN%RD      ')
     
            ELSE
              OPEN (UNIT=NOUTPG, FILE=OUTPG, STATUS = 'UNKNOWN')
              CLOSE (UNIT=NOUTPG, STATUS = 'DELETE')
              OPEN (UNIT=NOUTPN, FILE=OUTPN, STATUS = 'UNKNOWN')
              CLOSE (UNIT=NOUTPN, STATUS = 'DELETE')
              OPEN (UNIT=NOUTPG2, FILE=OUTPG2, STATUS = 'UNKNOWN')
              CLOSE (UNIT=NOUTPG2, STATUS = 'DELETE')
              OPEN (UNIT=NOUTPGF, FILE=OUTPGF, STATUS = 'UNKNOWN')
              CLOSE (UNIT=NOUTPGF, STATUS = 'DELETE')
            ENDIF
          ENDIF
 
          IF ((MOD(DAS,FROPADJ).EQ.0)
     &     .OR. (YEARDOY.EQ.STGDOY(7))
     &     .OR. (YEARDOY.EQ.STGDOY(11))) THEN
            IF (IDETG.NE.'N'.OR.IDETL.EQ.'0') THEN
              OUTCHOICE = 1
              ! Note possibilities. To change must recompile.
              IF (OUTCHOICE.EQ.1) THEN
                ! 1. Include reserves. Stem includes sheath
                LLWADOUT = AMAX1(0.0,LLWAD+LLRSWAD)
                LSHWADOUT = AMAX1(0.0,LSHWAD+LSHRSWAD)
                STWADOUT = AMAX1(0.0,STWAD+STRSWAD+LSHWAD+LSHRSWAD)
                SAIDOUT = SAID + LSHAI
                CHWADOUT = AMAX1(0.0,CHWAD+CHRSWAD)
                IF (LFWT.GT.1.0E-6) 
     &           SLAOUT = (PLA-SENLA) / (LFWT*(1.0-LSHFR)+LLRSWT)
              ELSEIF (OUTCHOICE.EQ.2) THEN
                ! 2. No reserves. Stem does not includes sheath
                LLWADOUT = AMAX1(0.0,LLWAD)
                LSHWADOUT = AMAX1(0.0,LSHWAD)
                STWADOUT = AMAX1(0.0,STWAD)
                SAIDOUT = SAID
                CHWADOUT = AMAX1(0.0,CHWAD)
                SLAOUT = -99.0
                IF (LFWT.GT.1.0E-6) 
     &           SLAOUT = (PLA-SENLA) / (LFWT*(1.0-LSHFR))
              ENDIF  
              IF (SLA.LE.0.0) SLAOUT = -99.0
              
              CALL Csopline(senw0c,senwal(0))
              CALL Csopline(senwsc,senwas)
              IF (PARIP.GE.0.0) THEN
                PARIOUT = PARIP/100.0
              ELSE
                PARIOUT = PARI
              ENDIF
              CALL Csopline(hwudc,gwud)

              IF (IDETG.NE.'N') THEN
              WRITE (NOUTPG,
     &        '(I5,I4,2I6,2F6.1,
     &        2F6.2,
     &        2F6.2,F6.1,F6.2,F6.3,
     &        F6.2,
     &        7I6,F6.2,
     &        4I6,2A6,
     &        F6.1,
     &        I6,A6,
     &        2I6,2F6.2,
     &        2F6.1,
     &        4F6.2,
     &        3F6.2,F6.1,
     &        2F6.2,
     &        2F6.2,
     &        F6.1)')      
     &        YEAR,DOY,DAS,DAP,TMEAN,TKILL,
     &        ZSTAGE,LNUMSD,   ! Zadoks staging
!     &        XSTAGE,LNUMSD,    ! Ceres staging
     &        PARIOUT,PARUED,AMIN1(999.9,CARBOA),
     &        LAI,SAIDOUT,LAI+SAIDOUT,
     &        NINT(TWAD),
     &        NINT(SDWAD),
     &        NINT(RWAD),
     &        NINT(CWAD),
     &        NINT(LLWADOUT),
     &        NINT(STWADOUT),NINT(GWAD),HIAD,
     &        NINT(CHWADOUT),
     &        NINT(EWAD),
     &        NINT(RSWAD),
     &        NINT(DWAD),SENW0C,SENWSC,
     &        RSCD*100.0,
     &        NINT(GRNUMAD),HWUDC,
              ! NB. SLA includes reserves   
     &        NINT(TNUMAD),NINT(SLAOUT),RTDEP/100.0,PTF,
     &        H2OA,AMIN1(99.9,WAVR),
     &        AMIN1(15.0,WUPR),1.0-WFT,1.0-WFP,1.0-WFG,
     &        1.0-NFT,1.0-NFP,1.0-NFG,AMIN1(2.0,NUPR),
     &        1.0-TFP,1.0-TFG,
     &        1.0-VF,1.0-DF 

!     VSH CSV output corresponding to PlantGro.OUT
      IF (FMOPT == 'C') THEN 
         CALL CsvOut(EXCODE, RUN,TN,RN,SN, ON, REP, CN, YEAR,DOY,
     &DAS, DAP, TMEAN, TKILL, ZSTAGE, LNUMSD, PARIOUT, PARUED, CARBOA, 
     &LAI, SAIDOUT, TWAD, SDWAD, RWAD, CWAD, LLWADOUT, STWADOUT, GWAD,
     &HIAD, CHWADOUT, EWAD, RSWAD, DWAD, SENW0C, SENWSC, RSCD, GRNUMAD,
     &HWUDC, TNUMAD, SLAOUT, RTDEP, PTF, H2OA, WAVR, WUPR, WFT, WFP,
     &WFG, NFT, NFP, NFG, NUPR, TFP, TFG, VF, DF,
     &vCsvlineCsCer, vpCsvlineCsCer, vlngthCsCer)
     
         CALL LinklstCsCer(vCsvlineCsCer)
      END IF

              LAIPROD = PLA*PLTPOP*0.0001
              CALL Csopline(laic,lai)
              CALL Csopline(laiprodc,laiprod)
              CALL Csopline(canhtc,canht)
              CALL Csopline(gstagec,Zstage)
              !L = MAX(1,LNUMSG-INT(LLIFG))
              WRITE (NOUTPG2,503)
     A         YEAR,DOY,DAS,DAP,TMEAN,GSTAGEC,RSTAGE,
     B         LAIPRODC,SENLA*PLTPOP*0.0001,LAIC,CANHTC,SDWAD,
     &         SENW0C,SENWSC,
     &         NINT(GRNUMAD),hwudc,
     D         SHRTD,PTF,RTDEP/100.0,(RLV(I),I=1,10)
  503         FORMAT(
     A         I5,I4,2I6,F6.1,A6,F6.2,
     B         A6,F6.2,A6,A6,F6.1,2A6,I6,A6,
     D         2F6.2,F6.3,10F6.2)
             
!     VSH CSV output corresponding to PlantGr2.OUT
      IF (FMOPT == 'C') THEN
         CALL CsvOutPlGr2(EXCODE, RUN,TN,RN,SN, ON, REP, CN, YEAR,
     &DOY, DAS, DAP, TMEAN, GSTAGEC, RSTAGE, LAIPRODC, SENLA, PLTPOP, 
     &LAIC, CANHTC, SDWAD, SENW0C, SENWSC, GRNUMAD, hwudc, SHRTD, PTF,
     &RTDEP, NL, RLV,
     &vCsvlinePlGr2, vpCsvlinePlGr2, vlngthPlGr2)
     
         CALL LinklstPlGr2(vCsvlinePlGr2)
      END IF 
            
              ! Plant Growth factors outputs
              WRITE (NOUTPGF,507)
     A        YEAR,DOY,DAS,DAP,TMEAN,ZSTAGE,
     B        DU,1.0-VF,1.0-DF,1.0-TFGEM,1.0-WFGE,
     C        1.0-TFP,1.0-WFP,1.0-NFP,1.0-CO2FP,1.0-RSFP,
     D        1.0-TFG,1.0-WFG,1.0-NFG,1.0-WFT,1.0-NFT,
     H        AMIN1(99.9,WAVR),AMIN1(15.0,WUPR),H2OA,EOP,
     I        SNH4PROFILE+SNO3PROFILE,LCNF,SCNF,RCNF
  507         FORMAT(
     a        I5,I4,2I6,F6.1,F6.1,
     b        F6.1,4F6.2,
     c        5F6.2,
     d        5F6.2,
     e        2F6.2,2F6.1,F6.1,3F6.2)
 
 !    VSH CSV output corresponding to PlantGrf.OUT
      IF (FMOPT == 'C') then
       CALL CsvOutPlGrf(EXCODE, RUN,TN,RN,SN, ON, REP, CN, YEAR,DOY,
     &DAS, DAP, TMEAN, ZSTAGE, DU, VF, DF, TFGEM, WFGE, TFP, WFP, NFP, 
     &CO2FP, RSFP, TFG, WFG, NFG, WFT, NFT, WAVR, WUPR, H2OA, EOP, 
     &SNH4PROFILE, SNO3PROFILE, LCNF, SCNF, RCNF, 
     &vCsvlinePlGrf, vpCsvlinePlGrf, vlngthPlGrf)
     
         CALL LinklstPlGrf(vCsvlinePlGrf)
      END IF
 
!!             2021-02-14 chp 
!!             NUAD should be a daily variable, but here it's cumulative. 
!!             Introduce a new variable that is daily.
!              Nuptake_daily = NUAD - NUAD_Y

              ! Plant N outputs
              IF (ISWNIT.EQ.'Y') THEN
                CALL Csopline(senn0c,sennal(0))
                CALL Csopline(sennsc,sennas)
                WRITE (NOUTPN,'(
     &           I5,I4,2I6,F6.1,F6.2,F6.1,
     &           F6.1,F6.2,
     &           F6.2,
     &           3F6.1,
     &           1F6.1,3F7.2,2A6,
     &           3F6.3,
     &           3F7.2,
     &           3F6.2,
     &           F6.1,F6.2,
     &           2F6.2)')
     &           YEAR,DOY,DAS,DAP,TMEAN,ZSTAGE,NUAD,
!    &           YEAR,DOY,DAS,DAP,TMEAN,ZSTAGE,
!    &           Nuptake_daily,
     &           TNAD,SDNAD,
     &           RNAD,
     &           CNAD,LLNAD,SNAD,
     &           GNAD,HIND,RSNAD,DNAD,SENN0C,SENNSC,
     &           RANC*100.0, LANC*100.0, SANC*100.0,
     &           GRAINANC*100.0,SDNC*100.0, VANC*100.0,
     &           LCNF, SCNF, RCNF,
     &           VCNC*100.0, VMNC*100.0, AMIN1(2.0,NUPR),ANDEM

!     VSH
      IF (FMOPT == 'C') then  
         CALL CsvOutPlNCsCer(EXCODE, RUN, TN, 
     &RN, REP, YEAR, DOY, DAS, DAP, 
     &TMEAN,ZSTAGE,NUAD,TNAD,SDNAD,RNAD,CNAD,LLNAD,SNAD, GNAD, 
     &HIND,RSNAD,DNAD,SENN0C,SENNSC,RANC, LANC, SANC, GRAINANC, 
     &SDNC, VANC,LCNF, SCNF, RCNF,VCNC, VMNC, NUPR,ANDEM,  
     &vCsvlinePlNCsCer, vpCsvlinePlNCsCer, vlngthPlNCsCer)
     
         CALL LinklstPlNCsCer(vCsvlinePlNCsCer)
      END IF 

              ENDIF  ! ISWNIT = Y

!!             2021-02-14 chp 
!!             Keep cumulative value for use tomorrow.
!              NUAD_Y = NUAD

            ENDIF    ! IDETG.NE.'N'
            ENDIF    ! IDETG.NE.'N'.OR.IDETL.EQ.'0'
          ENDIF      ! MOD(FROPADJ)

          ! Harvest date or failure writes
          IF (STGDOY(11).EQ.YEARDOY .OR.
     &     DYNAMIC.EQ.SEASEND .AND. SEASENDOUT.NE.'Y') THEN
     
            IF (DYNAMIC.EQ.SEASEND) THEN
!              WRITE (fnumwrk,*)' '
!              WRITE (fnumwrk,'(A46,A25)')
!     &         ' RUN TERMINATED PREMATURELY (PROBABLY BECAUSE ',
!     &         'OF MISSING WEATHER DATA) '
            ENDIF
            
!            WRITE(fnumwrk,*)' '
!            WRITE(fnumwrk,'(A17,I2)')' CROP COMPONENT: ',CN
!            WRITE(fnumwrk,'(A32,F8.1)')
!     &       '  DEAD MATERIAL LEFT ON SURFACE  ',SENWAL(0)
!            WRITE(fnumwrk,'(A32,F8.1)')
!     &       '  DEAD MATERIAL LEFT IN SOIL     ',SENWAS
!            WRITE(fnumwrk,'(A32,F8.1)')
!     &       '  ROOT WEIGHT AT HARVEST         ',RWAD
!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,'(A20,A10,I3)')
!     &       ' ROOTS BY LAYER FOR ',excode,tn
!            WRITE (fnumwrk,'(A19)')
!     &       '  LAYER  RTWT   RLV'
!            DO L=1,NLAYR
!              WRITE (fnumwrk,'(I6,F7.1,F6.2)')
!     &        L,RTWTAL(L),RLV(L)
!            ENDDO
!            IF (RTSLXDATE.GT.0) THEN
!              WRITE(fnumwrk,'(A30,I7)')
!     &         '  FINAL SOIL LAYER REACHED ON ',RTSLXDATE
!              WRITE(fnumwrk,'(A23,I7,A1)')
!     &         '  (MATURITY/FAILURE ON ',YEARDOY,')'
!            ELSE  
!              WRITE(fnumwrk,*)' FINAL SOIL LAYER NOT REACHED '
!            ENDIF
!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,'(A15,A10,I3)')' N BALANCE FOR ',excode,tn
!            WRITE (fnumwrk,'(A25,F8.4)')'   N UPTAKE + SEED       ',
!     &       NUAD+SDNAP
!            WRITE (fnumwrk,'(A25,3F8.4)')'   TOTAL N SENESCED      ',
!     &       SENNAL(0)+SENNAS,SENNAL(0),SENNAS
!            WRITE (fnumwrk,'(A25,F8.4)')'   N IN DEAD MATTER      ',
!     &       DNAD
!            WRITE (fnumwrk,'(A25,F8.4)')'   TOTAL N IN PLANT      ',
!     &       TNAD
!            WRITE (fnumwrk,'(A25,F8.4)')'   BALANCE (A-(B+C+D))   ',
!     &       NUAD+SDNAP
!     &       - (SENNAL(0)+SENNAS)
!     &       - TNAD
!            IF (TNAD.GT.0.0 .AND.
!     &       ABS(NUAD+SDNAP-(SENNAL(0)+SENNAS)-TNAD)/TNAD.GT.0.01)
!     &       WRITE(fnumwrk,'(A26,A10,A1,I2)')
!     &       '   PROBLEM WITH N BALANCE ',EXCODE,' ',TN
!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,'(A18,A10,I3)')' CH2O BALANCE FOR ',excode,tn
!            WRITE (fnumwrk,'(A27, F11.4)')'   SEED + CH2O FIXED A     ',
!     &       SDRATE+CARBOAC
!            WRITE (fnumwrk,'(A27, F11.4)')'   CH2O RESPIRED B         ',
!     &       RESPAC
     
!            WRITE (fnumwrk,'(A27,3F11.4)')'   CH2O SENESCED C  Tops,rt',
!     &       SENWAL(0)+SENWAS,SENWAL(0),SENWAS                          
!            WRITE (fnumwrk,'(A27,F11.4)') '   CH2O LF RESERVES LOST C2',
!     &       SENRSC*10.0*PLTPOP
!            WRITE (fnumwrk,'(A27, F11.4)')'   CH2O IN LIVE+DEAD D     ',
!     &       TWAD
!            WRITE (fnumwrk,'(A27, F11.4)')'   CH2O IN DEAD MATTER     ',
!     &       DWAD
!            WRITE (fnumwrk,'(A27, F11.4)')'   CH2O IN LIVE PLANT      ',
!     &       TWAD-DWAD
!            WRITE (fnumwrk,'(A27, F11.4)')'   POST MATURITY RESERVES E',
!     &       RSWADPM
!            WRITE (fnumwrk,'(A27, F11.4)')'   BALANCE (A-(B+C+C2+D+E))',
!     &         SDRATE+CARBOAC-RESPAC-(SENWAL(0)+SENWAS)
!     &       - TWAD-RSWADPM-(SENRSC*10.0*PLTPOP)
!            IF (TWAD.GT.0.0 .AND.
!     &       ABS(SDRATE+CARBOAC-RESPAC-(SENWAL(0)+SENWAS)
!     &       - TWAD-RSWADPM-(SENRSC*10.0*PLTPOP)     )
!     &       /TWAD .GT. 0.01)
!     &       WRITE(fnumwrk,'(A29,A10,A1,I2)')
!     &       '   PROBLEM WITH CH2O BALANCE ',EXCODE,' ',TN

!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,'(A22,A10,I3)')
!     &       ' STAGE CONDITIONS FOR ',excode,tn
!            WRITE (fnumwrk,'(A38,F6.1)')
!     &       '  Temperature mean,germ+emergence      ',GETMEAN
!            WRITE (fnumwrk,'(A38,F6.1)')
!     &       '  Temperature mean,first 20 days       ',TMEAN20P
!            WRITE (fnumwrk,'(A38,F6.1)')
!     &       '  Temperature mean,20d around anthesis ',TMEAN20A
!            WRITE (fnumwrk,'(A38,F6.1)')
!     &       '  Solar radn. mean,20d around anthesis ',SRAD20A
!            WRITE (fnumwrk,'(A38,F6.1)')
!     &       '  Stress fac. mean,20d around anthesis ',STRESS20A
!            WRITE (fnumwrk,'(A38,F6.1)')
!     &       '  Temperature mean,grain filling       ',GFTMEAN
!            WRITE (fnumwrk,'(A38,F6.1)')
!     &       '  Temperature mean,grain maturing      ',GMTMEAN
!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,'(A17,A10,I3)')' STAGE DATES FOR ',excode,tn
!            WRITE (fnumwrk,'(A26)')
!     &       '  STAGE   DATE  STAGE NAME'
!            DO I = 1, 11
!              WRITE (fnumwrk,'(I7,I8,A1,A10)')
!     &               I,STGDOY(I),' ',STNAME(I)
!            ENDDO
!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,'(A27,A10,I3)')
!     &       ' LEAF NUMBER AND SIZES FOR ',excode,tn
!            WRITE (fnumwrk,'(A15,F4.1)') '   LEAF NUMBER ',LNUMSD
!            WRITE (fnumwrk,'(A55)')
!     &       '   LEAF AREAP AREA1 AREAT AREAS TNUML  WFLF  NFLF  AFLF'
!            IF (LNUMSG.GT.0) THEN
!              DO I = 1, LNUMSG
!                WRITE (fnumwrk,'(I7,8F6.1)')
!     &           I,LAPOT(I),LATL(1,I),LAP(I),LAPS(I),TNUML(I),
!     &            1.0-WFLF(I),1.0-NFLF(I),1.0-AFLF(I)
!              ENDDO
!            ELSE
!              WRITE (fnumwrk,*) ' Leaf number < 1!'
!            ENDIF
!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,'(A28,A10,I3)')
!     &       ' STRESS FACTOR AVERAGES FOR ',excode,tn
!            WRITE (fnumwrk,'(A55)')
!     &       '  PHASE  H2O(PS)   H2O(GR)   N(PS)     N(GR)  PHASE END'
!            DO tvi1=1,5
!              WRITE (fnumwrk,'(I6,F8.2,3F10.2,2X,A10)')
!     &        tvi1,1.0-wfpav(tvi1),1.0-wfgav(tvi1),
!     &        1.0-nfpav(tvi1),1.0-nfgav(tvi1),stname(tvi1)
!            ENDDO
!            WRITE (fnumwrk,'(A42)')
!     &       '  NB 0.0 = minimum ; 1.0 = maximum stress.'
!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,'(A22,A10,I3)')
!     &       ' RESERVES STATUS FOR ',excode,tn
!            WRITE (fnumwrk,'(A20,I6)')'  Kg/ha at anthesis ',NINT(RSWAA)
!            WRITE (fnumwrk,'(A20,I6)')'  Kg/ha at maturity ',NINT(RSWAD)
!            IF (cwaa.GT.0) WRITE (fnumwrk,'(A20,F6.1)')
!     &       '  % at anthesis     ',rsca*100.0
!            IF (lfwt+stwt+rswt.GT.0) WRITE (fnumwrk,'(A20,F6.1)')
!     &       '  % at maturity     ',rswt/(lfwt+stwt+rswt)*100.0
!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,'(A20,F6.2)')'  Reserves coeff    ',RSPCS
!            WRITE (fnumwrk,'(A20,F6.2)')'  Stem gr end stage ',P4SGE
!            WRITE (fnumwrk,'(A20,F6.2)')
!     &       '  Anthesis stage    ',(4.0+PD4(1)/PD(4))
!            WRITE (fnumwrk,*) ' '
!            IF (grnum.GT.0.0) WRITE (fnumwrk,'(A20,F6.1)')
!     &       '  Grain weight mg   ',GRWT/GRNUM*1000.0
!            WRITE (fnumwrk,'(A20,F6.1)')
!     &       '  Grain weight coeff',g2kwt
! FO/GH - 11-13-2021 - Removed division by zero issue for GRNUM
!            IF (GRNUM.GT.0.0) THEN
!              WRITE (fnumwrk,'(A34)')
!     &         '  Some limitation on grain growth!'
!              WRITE(fnumwrk,'(A22,I4)')'   Days of Ch2o limit ',ch2olim
!              WRITE(fnumwrk,'(A22,I4)')'   Days of N limit    ',nlimit
!              WRITE(fnumwrk,'(A22,I4)')'   Days of temp limit ',tlimit
!            ENDIF
!            IF (grwt.GT.0.0) WRITE (fnumwrk,'(A20,F6.1)')
!     &       '  Grain N %         ',grainn/grwt*100.0
!            WRITE (fnumwrk,'(A20,F6.1)')
!     &       '  Minimum grain N % ',grnmn
!            WRITE (fnumwrk,'(A20,F6.1)')
!     &       '  Standard grain N %',grns
     
            ! BEGIN MAIN OUTPUTS 
            
            CR = CROP
            GWAM = GWAD
            PWAM = GWAD + CHWAD
            GWUM = GWUD
            NUAM = NUAD
            HNUMAM = GRNUMAD
            HIAM = HIAD
            LNUMSM = LNUMSD
            TNUMAM = TNUMAD
            SENWATC = SENWAL(0) + SENWAS
            CWAM = CWAD
            VWAM = VWAD
            CNAM = CNAD
            VNAM = VNAD
            SENNATC = SENNAL(0) + SENNAS
            GNAM = GNAD
            RWAM = RWAD
            RNAM = RNAD
            RSWAM = RSWAD
            HINM = HIND
            GNPCM = GRAINANC *100.0
            VNPCM = VANC * 100.0 
            VNAA = CNAA
            
            IF (tnumam.GT.0.0) hnumgm = hnumam/tnumam
            gwgm = gwum*1000.0    ! mg

            ! Check that -99 not multiplied or divided 
            IF (hnumgm.LT.0.0) hnumgm = -99
            IF (hnumam.LT.0.0) hnumam = -99
            IF (hnumgmm.LT.0.0) hnumgmm = -99
            IF (hnumamm.LT.0.0) hnumamm = -99

            ! Put N variables to -99 if N switched off
            IF (ISWNIT.EQ.'N') THEN
              gnpcm = -99
              vnpcm = -99
              cnam = -99
              gnam = -99
              hinm = -99
              sdnap = -99
              rnam = -99
              nuam = -99
            ENDIF
            
!            WRITE (fnumwrk,*) ' '

            IF (DYNAMIC.EQ.SEASEND) THEN
            
!              WRITE(fnumwrk,*)  'WRITING END OF RUN OUTPUTS     '

              ! Simulated outputs only
              !  IDETG (GROUT in controls (Y,N))  Plant growth outputs
              !   Y->Work_details+Plantgro+Plantgr2+Plantgrf
              !      +PlantN(If N switched on)
              !   FROUT->#=number of days between outputs
              !  IDETS (SUMRY in controls (Y,N)) Summary outputs
              !   Y->Summary+Plantsum+Work(Harvest)                        
              !
              ! Simulated+Measured outputs
              !  IDETO (OVVEW in controls (Y,E,N)) Overview outputs
              !   Y->Overview+Evaluate(+Measured if IDETG=Y)
              !   E->Evaluate only
              !  IDETL (VBOSE in controls (0,N,Y,D,A))
              !   Y->Leaves+Phases+Measured                 
              !   D->+Phenols+Phenolm+Plantres+Plantrem
              !   A->Errora+Errors+Errort+Full Reads
              !   0,A are meta switches:
              !    0 switches everything to N apart from IDETS,which->Y,      
              !      and IDETO,which->E when RNMODE is not N (seasonal)
              !    A switches ALL outputs on  
              
              laix = -99.0
              gwam = -99.0
              gwum = -99.0
              hnumam = -99.0
              hnumgm = -99.0
              hiam = -99.0
              lnumsm = -99.0
              tnumam = -99.0
              cwam = -99.0
              vwam = -99.0
              rwam = -99.0
              carboac = -99.0
              senwatc = -99.0
              rswam = -99.0
              sennatc = -99.0
              cnam = -99.0
              rnam = -99.0
              vnam = -99.0
              gnam = -99.0
              hinm = -99.0
              gnpcm = -99.0
              cwaa = -99.0
              vnaa = -99.0
              lnpca = -99.0
              stress20a = -99.0
              mday = -99
              hayear = -99
              haday = -99
!            ELSE 
!              WRITE(fnumwrk,*)  'WRITING HARVEST DAY OUTPUTS         '
            ENDIF  
            
            IF (STEP.NE.1) THEN
!              WRITE (fnumwrk,*) ' '
!              WRITE (fnumwrk,*) ' Step number greater than 1!'
!              WRITE (fnumwrk,*) ' Not set up for hourly runs!'
!              WRITE (fnumwrk,*) ' Will skip final outputs.'
              GO TO 8888
            ENDIF
!            WRITE(fnumwrk,*)
!     &       ' Harvest percentage (Technology coeff) ',hpc
            
            CNCHAR = ' '
            CNCHAR2 = '  '
            IF (CN.EQ.1) THEN
              OUT = 'OUT'
              CNCHAR2= '1 '
            ELSE
              CNCHAR = TL10FROMI(CN)
              OUT = 'OU'//CNCHAR(1:1)
              CNCHAR2(1:1) = CNCHAR(1:1)
            ENDIF
            
!            CALL CSYR_DOY (STGDOY(7),PLYEAR,PLDAY)
            CALL YR_DOY (STGDOY(7),PLYEAR,PLDAY)
            IF (ADAT.GT.0) THEN
!              CALL CSYR_DOY (ADAT,AYEAR,ADAY)
              CALL YR_DOY (ADAT,AYEAR,ADAY)
            ELSE
              AYEAR = -99
              ADAY = -99
            ENDIF
            IF (STGDOY(5).GT.0) THEN
!              CALL CSYR_DOY (STGDOY(5),MYEAR,MDAY)
              CALL YR_DOY (STGDOY(5),MYEAR,MDAY)
            ELSE
              MYEAR = -99
              MDAY = -99
            ENDIF
!            CALL CSYR_DOY (STGDOY(11),HAYEAR,HADAY)
            CALL YR_DOY (STGDOY(11),HAYEAR,HADAY)
            
!-----------------------------------------------------------------------
            
            ! SCREEN OUTPUTS (CROPSIM)
            
            IF (FILEIOT(1:3).EQ.'XFL'.AND.CN.EQ.1.AND.RNMODE.NE.'T')THEN
            
              IF (OUTCOUNT.LE.0 .OR. OUTCOUNT.EQ.25) THEN
                WRITE (*,499)
  499           FORMAT ('   RUN EXCODE      TN RN',
     X           ' TNAME..................',
     X           '.. REP  RUNI S O C CR  GWAM')
              ENDIF
              IF (OUTCOUNT .EQ. 25) THEN
                OUTCOUNT = 1
              ELSE
                OUTCOUNT = OUTCOUNT + 1
              ENDIF
              WRITE (*,410) run,excode,tn,rn,tname(1:25),
     X        rep,runi,sn,on,cn,cr,NINT(gwam)
  410         FORMAT (I6,1X,A10,I4,I3,1X,A25,
     X        I4,I6,I2,I2,I2,1X,A2,I6)
            ENDIF
            
            ! END OF SCREEN OUTPUTS
            
!-----------------------------------------------------------------------
            
            ! PLANT SUMMARY
           
            IF ((IDETS.NE.'N'.AND.IDETL.NE.'0').OR.IDETL.EQ.'A') THEN

!              WRITE (fnumwrk,*) ' '                       
!              WRITE (fnumwrk,*) 'Writing PLANT SUMMARY'
              
              FNAMETMP = ' '
              FNAMETMP(1:12) = 'Plantsum.'//out
              
              IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
               OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,STATUS = 'UNKNOWN')
               WRITE (FNUMTMP,'(A14)') '$PLANT SUMMARY'
               WRITE (FNUMTMP,*) ' '
               WRITE (fnumtmp,96)
               WRITE (fnumtmp,99)
   96          FORMAT ('*PLANTSUM',/)
   99          FORMAT ('@  RUN EXCODE    TRNO RN ',                     
     X         'TNAME..................',
     X         '.. REP  RUNI S O C    CR',
     X         ' PYEAR  PDOY  TSAP  ADAP  MDAP   FLN FLDAP',
     x         ' HYEAR  HDAY SDWAP CWAHC  CWAM',
     X         ' PARUE',
     X         '  HWAM  HWAH  BWAH  HWUM  H#AM  H#UM',
     x         ' SDNAP  CNAM  HNAM  RNAM  TNAM  NUAM   HN%M  VN%M',
     E         ' D1INI D2INI D3INI ')
               CLOSE(fnumtmp)
              ENDIF
              
              OPEN (UNIT=fnumtmp,FILE=FNAMETMP,POSITION='APPEND')

              IF (tsdat.GT.0) THEN 
                tsdap = Dapcalc(tsdat,plyear,plday)
              ELSE
                tsdap = -99
              ENDIF  
              IF (lldat.GT.0) THEN 
                lldap = Dapcalc(lldat,plyear,plday)
              ELSE
                lldap = -99
              ENDIF  
              IF (adat.LT.9999999) THEN 
                adap = Dapcalc(adat,plyear,plday)
              ELSE
                adap = -99
              ENDIF  
              IF (stgdoy(5).LT.9999999) THEN 
                 mdap = Dapcalc(stgdoy(5),plyear,plday)
              ELSE 
                mdap = -99
                stgdoy(5) = -99
              ENDIF     
              
              WRITE (fnumtmp,400) run,excode,tn,rn,tname(1:25),
     X          rep,runi,sn,on,cn,cr,
     X          plyear,plday,tsdap,adap,mdap,
     x          flnmodel,lldap,hayear,haday,
     X          NINT(sdrate),
     X          0,NINT(cwam),pariuem,NINT(gwam),
     X          NINT(gwam*hpc/100.0),NINT(cwam-gwam),
     X          gwgm/1000.0,NINT(hnumam),NINT(hnumgm),
     X          sdnap,NINT(cnam),NINT(gnam),NINT(rnam),
     X          NINT(AMAX1(-99.0,cnam+rnam)),NINT(nuad),
     X          gnpcm,vnpcm,'   -99','   -99','   -99'
  400         FORMAT (I6,1X,A10,I4,I3,1X,A25,
     X         I4,I6,I2,I2,I2,4X,A2,
     X         I6,I6,I6,I6,I6,F6.1,I6,I6,I6,I6,
     X         I6,
     X         I6,F6.1,I6,
     X         I6,I6,
     X         1X,F5.3,I6,I6,
     X         F6.1,I6,I6,I6,
     X         I6,I6,
     X         F7.2,F6.2,
     X         3A6)
              
              CLOSE(fnumtmp)       
             
            ELSE
            
               OPEN (UNIT=FNUMTMP,FILE='Plantsum.out',STATUS='UNKNOWN')
               CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
               
            ENDIF  ! END OF PLANT SUMMARY
            
!-----------------------------------------------------------------------            
            
            ! LEAVES
           
            IF (IDETL.EQ.'Y'.OR.IDETL.EQ.'A') THEN

              IF (CN.EQ.1) THEN
                IF (FNUMLVS.LE.0.OR.FNUMLVS.GT.1000) THEN
                  CALL Getlun ('Leaves.OUT',fnumlvs)
                ENDIF
                IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
                 OPEN(UNIT=FNUMLVS,FILE='Leaves.OUT',STATUS='UNKNOWN')
                 WRITE (FNUMLVS,'(A11)') '$LEAF SIZES'
                 CLOSE(FNUMLVS)
                ENDIF
                OPEN(UNIT=FNUMLVS,FILE='Leaves.OUT',POSITION='APPEND')
                WRITE (FNUMLVS,'(/,A77,/)') OUTHED
                WRITE (FNUMLVS,'(A14,F6.1)') '! LEAF NUMBER ',LNUMSD
                IF (LNSWITCH.GT.0.0) THEN
                  WRITE(FNUMLVS,'(A42,F6.1)')
     &             '! LEAF NUMBER WHEN INCREASE FACTOR CHANGED',lnswitch
                  WRITE(FNUMLVS,'(A35,F6.2)')
     &             '! AREA OF LEAF WHEN FACTOR CHANGED  ',laswitch
                ENDIF     
                WRITE (FNUMLVS,'(/,A42,A30)')
     &          '@ LNUM AREAP AREA1 AREAT AREAS  T#PL  T#AL',
     &          '  WFLF  NFLF  AFLF  TFLF DAYSG'
                IF (LNUMSG.GT.0) THEN
                  DO I = 1, LNUMSG
                    WRITE (fnumlvs,'(I6,5F6.1,I6,5F6.1)')
     &               I,LAPOT(I),LATL(1,I),LAP(I),LAPS(I),
     &               TNUML(I),NINT(TNUML(I)*PLTPOP),
     &               1.0-WFLF(I),1.0-NFLF(I),1.0-AFLF(I),1.0-TFLF(I),
     &               WFLFNUM(I)
                  ENDDO
                ENDIF
                IF (run.EQ.1.AND.runi.EQ.1) THEN
                  WRITE(fnumlvs,*)' '
                  WRITE(fnumlvs,'(A37)')
     &             '! LNUM  = Number of leaf on main axis'
                  WRITE(fnumlvs,'(A38)')
     &             '! AREAP = Potential area of leaf (cm2)'
                  WRITE(fnumlvs,'(A41)')
     &             '! AREA1 = Area of leaf on main axis (cm2)'
                  WRITE(fnumlvs,'(A44,A16)')
     &             '! AREAT = Area of leaves on all axes at leaf',
     &             ' position (cm2) '
                  WRITE(fnumlvs,'(A50,A6)')
     &             '! AREAS = Area of leaves senesced at leaf position',
     &             ' (cm2)'
                  WRITE(fnumlvs,'(A46)')
     &             '! T#PL  = Tiller number/plant at leaf position'
                  WRITE(fnumlvs,'(A49)')
     &             '! T#AL  = Tiller number/area(m2) at leaf position'
                  WRITE(fnumlvs,'(A38,A17)')
     &             '! WFLF  = Water stress factor for leaf',
     &             ' (0-1,0=0 stress)'
                  WRITE(fnumlvs,'(A51)')
     &             '! NFLF  = N stress factor for leaf (0-1,0=0 stress)'
                  WRITE(fnumlvs,'(A36,A24)')
     &             '! AFLF  = Assimilate factor for leaf',
     &             ' (0-1,0=0 no limitation)'
                  WRITE(fnumlvs,'(A37,A24)')
     &              '! TFLF  = Temperature factor for leaf',
     &              ' (0-1,1=0 no limitation)'
                  WRITE(fnumlvs,'(A37)')
     &              '! DAYSG = Number of days of growth   '
                ENDIF
                CLOSE (FNUMLVS)
              ENDIF

            ELSE
            
               OPEN (UNIT=FNUMTMP,FILE='Leaves.OUT',STATUS='UNKNOWN')
               CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
               
            ENDIF  ! END OF LEAVES
            
!-----------------------------------------------------------------------
            
            ! A-FILE READS
            
            adatm = -99
            adatt = -99
            adayh = -99
            carboacm = -99
            cnaam = -99
            cnamm = -99
            cwadt = -99
            cwamm = -99
            drdatm = -99
            edatm = -99
            gnamm = -99
            gnpcmm = -99
            gstdm = -99
            gwadt = -99
            hiadt = -99
            hiamm = -99
            hinmm = -99
            hnumamm = -99
            hnumat = -99
            hnumet = -99
            hnumgmm = -99
            hwadm = -99
            hwahm = -99
            gwamm = -99
            gwut = -99
            gwumm = -99
            jdatm = -99
            laixm = -99
            lnaam = -99
            lnpcam = -99
            lnumsmm = -99
            lnumt = -99
            lwaam = -99
            mdatm = -99
            mdatt = -99
            nuamm = -99
            rnamm = -99
            rswamm = -99
            rwamm = -99
            sennatcm = -99
            senwatcm = -99
            tnumamm = -99
            tnumt = -99
            tsdatm = -99
            vnamm = -99
            vnpcmm = -99
            vwamm = -99                       
            cwaam = -99
            
            CALL LTRIM2 (FILEIO,filenew)
            FILELEN = TVILENT(FILENEW)
            ! Jed Greene, Citadel - keep index from neg
            FILELEN = MAX(FILELEN-12, 0) 
            
            IF (TVILENT(FILEADIR).GT.3) THEN
              IF (FILEADIR(TVILENT(FILEADIR):
     &            TVILENT(FILEADIR)).NE.SLASH)THEN
                FILEA = FILEADIR(1:TVILENT(FILEADIR))//
     &           SLASH //EXCODE(1:8)//'.'//EXCODE(9:10)//'A'
              ELSE
                FILEA = FILEADIR(1:TVILENT(FILEADIR))//
     &           EXCODE(1:8)//'.'//EXCODE(9:10)//'A'
              ENDIF
            ELSE
              FILEA = FILENEW(1:FILELEN-12)//EXCODE(1:8)//'.'//
     &         EXCODE(9:10)//'A'
            ENDIF       
            FEXISTA = .FALSE.
            INQUIRE (FILE = FILEA,EXIST = FEXISTA)
            IF (.not.FEXISTA) THEN
!              WRITE (fnumwrk,*) 'A-file not found!'
            ELSE

              IF (INDEX('FQ',RNMODE) > 0) THEN
                TRT_ROT = CONTROL % ROTNUM
              ELSE
                TRT_ROT = TN
              ENDIF
              CALL READA_Y4K(FILEA, PATHEX,OLAB, TRT_ROT, YEARSIM, X)
             
              CALL READA_Dates(X(1), YEARSIM, IFLR)
              IF (IFLR .GT. 0 .AND. IPLTI .EQ. 'R' 
     &            .AND. ISENS .EQ. 0) THEN
                adapm = CSTIMDIF(YEARPLT,IFLR)
              ELSE
                adapm  = -99
              ENDIF
              
              CALL READA_Dates(X(2), YEARSIM, IMAT)
              IF (IMAT .GT. 0 .AND. IPLTI .EQ. 'R' 
     &            .AND. ISENS .EQ. 0) THEN
                mdatm = CSTIMDIF(YEARPLT,IMAT)
              ELSE
                mdatm  = -99
              ENDIF              
                            
              IF(X(3) .NE. "   -99") THEN
                READ(X(3), '(F12.0)') gwamm
              ELSE
                gwamm = -99
              ENDIF

              IF(X(4) .NE. "   -99") THEN
                READ(X(4), '(F12.0)') hnumamm
              ELSE
                hnumamm = -99
              ENDIF

              IF(X(5) .NE. "   -99") THEN
                READ(X(5), '(F12.0)') gwumm
              ELSE
                gwumm = -99
              ENDIF

              IF(X(6) .NE. "   -99") THEN
                READ(X(6), '(F12.0)') hnumgmm
              ELSE
                hnumgmm = -99
              ENDIF

              IF(X(7) .NE. "   -99") THEN
                READ(X(7), '(F12.0)') cwamm
              ELSE
                cwamm = -99
              ENDIF

              IF(X(8) .NE. "   -99") THEN
                READ(X(8), '(F12.0)') laixm
              ELSE
                laixm = -99
              ENDIF

              IF(X(9) .NE. "   -99") THEN
                READ(X(9), '(F12.0)') hiamm
              ELSE
                hiamm = -99
              ENDIF

              IF(X(10) .NE. "   -99") THEN
                READ(X(10), '(F12.0)') gnamm
              ELSE
                gnamm = -99
              ENDIF

              IF(X(11) .NE. "   -99") THEN
                READ(X(11), '(F12.0)') cnamm
              ELSE
                cnamm = -99
              ENDIF

              IF(X(12) .NE. "   -99") THEN
                READ(X(12), '(F12.0)') gnpcmm
              ELSE
                gnpcmm = -99
              ENDIF

              IF(X(13) .NE. "   -99") THEN
                READ(X(13), '(F12.0)') cwaam
              ELSE
                cwaam = -99
              ENDIF

              IF(X(14) .NE. "   -99" .AND. adapm .LE. 0.0) THEN
                READ(X(14), '(F12.0)') cnamm
              ENDIF

              IF(X(15) .NE. "   -99") THEN
                READ(X(15), '(F12.0)') lnumsmm
              ELSE
                lnumsmm = -99
              ENDIF

              CALL READA_Dates(X(16), YEARSIM, IEDT)
              IF (IEDT .GT. 0 .AND. IPLTI .EQ. 'R' 
     &            .AND. ISENS .EQ. 0) THEN
                edatm = CSTIMDIF(YEARPLT,IEDT)
              ELSE
                edatm  = -99
              ENDIF              
              IF(X(17) .NE. "   -99") THEN
                READ(X(17), '(F12.0)') hwahm
              ELSE
                hwahm = -99
              ENDIF

              IF(X(18) .NE. "   -99" .AND. gwumm .LE. 0.0) THEN
                READ(X(18), '(F12.0)') gwumm
              ENDIF

              IF(X(19) .NE. "   -99") THEN
                READ(X(19), '(F12.0)') vwamm
              ELSE
                vwamm = -99
              ENDIF

              IF(X(20) .NE. "   -99") THEN
                READ(X(20), '(F12.0)') tnumamm
              ELSE
                tnumamm = -99
              ENDIF

              IF(X(21) .NE. "   -99") THEN
                READ(X(21), '(F12.0)') vnamm
              ELSE
                vnamm = -99
              ENDIF

              IF(X(22) .NE. "   -99") THEN
                READ(X(22), '(F12.0)') lnpcam
              ELSE
                lnpcam = -99
              ENDIF

              IF(X(23) .NE. "   -99") THEN
                READ(X(23), '(F12.0)') gnpcmm
              ELSE
                gnpcmm = -99
              ENDIF

              IF(X(24) .NE. "   -99") THEN
                READ(X(24), '(F12.0)') vnpcmm
              ELSE
                vnpcmm = -99
              ENDIF

              IF(X(25) .NE. "   -99") THEN
                READ(X(25), '(F12.0)') vnpcmm
              ELSE
                vnpcmm = -99
              ENDIF

              IF(X(26) .NE. "   -99") THEN
                READ(X(26), '(F12.0)') vnpcmm
              ELSE
                vnpcmm = -99
              ENDIF

              IF(X(27) .NE. "   -99") THEN
                READ(X(27), '(F12.0)') lnumsmm
              ELSE
                lnumsmm = -99
              ENDIF            

              CALL READA_Dates(X(28), YEARSIM, IDDAT)
              IF (IDDAT .GT. 0 .AND. IPLTI .EQ. 'R' 
     &            .AND. ISENS .EQ. 0) THEN
                drdatm = CSTIMDIF(YEARPLT,IDDAT)
              ELSE
                drdatm  = -99
              ENDIF

              CALL READA_Dates(X(29), YEARSIM, ITDAT)
              IF (ITDAT .GT. 0 .AND. IPLTI .EQ. 'R' 
     &            .AND. ISENS .EQ. 0) THEN
                tsdatm = CSTIMDIF(YEARPLT,ITDAT)
              ELSE
                tsdatm  = -99
              ENDIF

              CALL READA_Dates(X(30), YEARSIM, IADAT)
              IF (IADAT .GT. 0 .AND. IPLTI .EQ. 'R' 
     &            .AND. ISENS .EQ. 0) THEN
                a1datm = CSTIMDIF(YEARPLT,IADAT)
              ELSE
                a1datm  = -99
              ENDIF

              CALL READA_Dates(X(31), YEARSIM, LFDAT)
              IF (LFDAT .GT. 0 .AND. IPLTI .EQ. 'R' 
     &            .AND. ISENS .EQ. 0) THEN
                lldatm = CSTIMDIF(YEARPLT,LFDAT)
              ELSE
                lldatm  = -99
              ENDIF

              CALL READA_Dates(X(32), YEARSIM, SEDAT)
              IF (SEDAT .GT. 0 .AND. IPLTI .EQ. 'R' 
     &            .AND. ISENS .EQ. 0) THEN
                spdatm = CSTIMDIF(YEARPLT,SEDAT)
              ELSE
                spdatm  = -99
              ENDIF

              CALL READA_Dates(X(33), YEARSIM, IJDAT)
              IF (IJDAT .GT. 0 .AND. IPLTI .EQ. 'R' 
     &            .AND. ISENS .EQ. 0) THEN
                jdatm = CSTIMDIF(YEARPLT,IJDAT)
              ELSE
                jdatm  = -99
              ENDIF

              CALL READA_Dates(X(34), YEARSIM, IGS059)
              IF (IGS059 .GT. 0 .AND. IPLTI .EQ. 'R' 
     &            .AND. ISENS .EQ. 0 .AND. adapm .LE. 0.0) THEN
                !2 days after GS59 -- according to original code
                adatm = CSTIMDIF(YEARPLT,IGS059 + 2) 
              ELSE
                adatm  = -99
              ENDIF
              
              IF (HIAMM.GE.1.0) HIAMM = HIAMM/100.0

            ENDIF
            
            ! If nothing in A-file,use X-file
            IF (EDATM.LE.0) edatm = emdatm   
            
            ! END OF A-FILE READS
            
!-----------------------------------------------------------------------
            
            ! CHECK DATA AND USE EQUIVALENTS IF NECESSARY
            
            ! Product wt at maturity
            IF (hwahm.GT.0 .AND. gwamm.LE.0) gwamm = hwahm/(hpc/100.0)
            
            ! Product wt at harvest
            IF (gwamm.GT.0 .AND. hwahm.LE.0) hwahm = gwamm*(hpc/100.0)
            
            ! Canopy wt at maturity
            IF (vwamm.GT.0 .AND. gwamm.GT.0) cwamm = vwamm+gwamm
            
            ! Vegetative wt at maturity
            IF (gwamm.GT.0 .AND. cwamm.GT.0) vwamm = cwamm-gwamm
            
            ! Harvest index at maturity
            IF (hiamm.LE.0.0) THEN
              IF (cwamm.GT.0 .AND. gwamm.GT.0) hiamm = gwamm/cwamm
            ELSE
              IF (cwamm.GT.0 .AND. gwamm.GT.0) THEN
                hiammtmp = gwamm/cwamm
                IF (hiammtmp/hiam.GT.1.1 .OR. hiammtmp/hiam.LT.0.9) THEN
!                  IF (ABS(hiammtmp-hiamm)/hiamm.GT.0.05) THEN
!                    WRITE (fnumwrk,*) 'Reported HI not consistent',
!     &               ' with yield and total weight data!!'
!                    WRITE (fnumwrk,*) ' Reported HI   ',hiamm
!                    WRITE (fnumwrk,*) ' Calculated HI ',hiammtmp
!                    WRITE (fnumwrk,*) ' Will use reported value '
!                  ENDIF
                ENDIF
              ENDIF
            ENDIF
            
            ! Product unit wt at maturity
            IF (gwumm.GT.1.0) gwumm = gwumm/1000.0 ! mg->g
            IF (gwumm.LE.0 .AND. hnumamm.GT.0) THEN
              IF (gwamm.GT.0.0) gwumm=gwamm*0.1/hnumamm  ! kg->g
            ELSE
              IF (gwamm.gt.0.0.AND.hnumamm.GT.0.0) THEN
                gwumyld = gwamm*0.1/hnumamm
!                IF (ABS(gwumyld-gwumm)/gwumm.GT.0.05) THEN
!                  WRITE (fnumwrk,*) 'Reported kernel wt.not consistent',
!     &            ' with yield and kernel # data!!'
!                  WRITE (fnumwrk,*) ' Reported wt   ',gwumm
!                  WRITE (fnumwrk,*) ' Calculated wt ',gwumyld
!                  WRITE (fnumwrk,*) '   Yield       ',gwamm
!                  WRITE (fnumwrk,*) '   Kernel #    ',hnumamm
!                  WRITE (fnumwrk,*) ' Will use reported value '
!                ENDIF
              ENDIF
            ENDIF
            gwgmm = gwumm*1000.0  ! mg
            
            ! Product number at maturity
            IF (HNUMAMM.LE..0.AND.HNUMGMM.GT..0.AND.TNUMAMM.GT..0) THEN
             HNUMAMM = HNUMGMM * TNUMAMM
!             WRITE(fnumwrk,*)'Tiller # * grains/tiller used for HNUMAMM'
            ENDIF
            IF (hnumgmm.LE.0. AND. tnumamm.GT.0 .AND. hnumamm.GT.0) THEN
              hnumgmm = hnumamm/tnumamm
!              WRITE(fnumwrk,*)'Grains/area / tiller # used for HNUMGMM'
            ENDIF
            
            ! Tiller number at maturity
            IF (tnumamm.LE.0 .AND. hnumamm.GT.0. AND. hnumgmm.GT.0)
     &         tnumamm = hnumamm/hnumgmm
            
            ! Canopy N at maturity
            IF (vnamm.GT.0 .AND. gnamm.GT.0 .AND. cnamm.LE.0)
     &        cnamm = vnamm + gnamm
            
            ! Vegetative N at maturity
            IF (vnamm.LE.0) THEN
             IF (gnamm.GE.0 .AND. cnamm.GT.0) vnamm=cnamm-gnamm
            ENDIF
            
            ! Product N harvest index at maturity
            IF (cnamm.GT.0 .AND. gnamm.GT.0) hinmm=gnamm/cnamm
            
            ! Vegetative N concentration at maturity
            IF (vnpcmm.LE.0) THEN
             IF (vwamm.GT.0 .AND. vnamm.GT.0) vnpcmm = (vnamm/vwamm)*100
            ENDIF
            
            ! Product N concentration at maturity
            IF (gnpcmm.LE.0) THEN
             IF (gwamm.GT.0 .AND. gnamm.GT.0) gnpcmm = (gnamm/gwamm)*100
            ENDIF
            
            ! Leaf N concentration at maturity
            IF (lnpcam.LE.0 .AND. lnaam.GT.0 .AND. lwaam.GT.0.0)
     &        lnpcam = lnaam/lwaam
     
            ! END OF CHECKING DATA

!-----------------------------------------------------------------------

            ! T-FILE READS AND MEASURED.OUT WRITES
            
            IF ((IDETG.NE.'N'.AND.IDETL.NE.'0').OR.
     &           IDETL.EQ.'D'.OR.IDETL.EQ.'A') THEN
            
              ! T-FILE READS AND MEASURED.OUT WRITES
            
!              WRITE (fnumwrk,*)
!     &         'Trying to read T-file and write MEASURED.OUT'
            
              Fnametmp = ' '
              Fnametmp(1:12) = 'Measured.OUT'
              IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
               CALL Getlun ('FILET',FNUMT)
               OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,STATUS = 'UNKNOWN')
               WRITE (FNUMTMP,'(A22)') '$TIME_COURSE(MEASURED)'
               CLOSE(FNUMTMP)
              ENDIF
            
              STARNUMO = STARNUMO + 1  ! # datasets in sim output file
              
              IF (TVILENT(FILEADIR).GT.3) THEN
                IF (FILEADIR(TVILENT(FILEADIR):
     &            TVILENT(FILEADIR)).NE.SLASH)THEN
                  FILET = FILEADIR(1:TVILENT(FILEADIR))//
     &           SLASH //EXCODE(1:8)//'.'//EXCODE(9:10)//'T'
                ELSE
                  FILET = FILEADIR(1:TVILENT(FILEADIR))//
     &           EXCODE(1:8)//'.'//EXCODE(9:10)//'T'
                ENDIF
              ELSE
                FILET = FILENEW(1:FILELEN-12)//EXCODE(1:8)//'.'//
     &          EXCODE(9:10)//'T'
              ENDIF       
              FEXISTT  = .FALSE.
              INQUIRE (FILE = FILET,EXIST = FEXISTT)
            
              CFLTFILE = 'N'
              LAIXT = -99.0
              VALUER = -99.0
            
              IF (.not.FEXISTT) THEN
!                WRITE (fnumwrk,*) 'T-file not found: ',filet(1:60)
              ELSE
!                WRITE (fnumwrk,*) 'T-file found: ',filet(1:60)
                TLINENUM = 0
                OPEN (UNIT = FNUMT,FILE = FILET)
                OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,POSITION ='APPEND')
                COLNUM = 1
                L1 = 0
                DO
8453              CONTINUE                    
                  READ(FNUMT,'(A180)',END = 5555)LINET
                  IF (LINET(1:1).EQ.'!') GO TO 8453
                  TLINENUM = TLINENUM + 1  ! Only to check if file empty
                  IF (LINET(1:5).EQ.'*FLAG'.OR.LINET(1:5).EQ.'*CODE'.OR.
     &                LINET(1:5).EQ.'*GENE') THEN
                    DO
8454                  CONTINUE                    
                      READ(FNUMT,'(A180)',END = 5555)LINET
                      IF (LINET(1:1).EQ.'!') GO TO 8454
                      IF (LINET(1:1).EQ.'*') THEN
                        IF (LINET(1:7).EQ.'*DATA(T') EXIT
                        IF (LINET(1:7).EQ.'*EXP.DA') EXIT
                        IF (LINET(1:7).EQ.'*EXP. D') EXIT
                        IF (LINET(1:7).EQ.'*AVERAG') EXIT
                        IF (LINET(1:7).EQ.'*TIME_C') EXIT
                      ENDIF
                    ENDDO
                  ENDIF
                  L1 = 0
                  L2 = 0
                  IF (LINET(1:7).EQ.'*DATA(T' .OR.
     &             LINET(1:7).EQ.'*EXP.DA' .OR.
     &             LINET(1:7).EQ.'*EXP. D' .OR.
     &             LINET(1:7).EQ.'*TIME_C' .OR.
     &             LINET(1:7).EQ.'*AVERAG') THEN
                    TNCHAR = TL10FROMI(TN)
                    LENLINE = LEN(TRIM(LINET))
                    IF(LINET(1:7).EQ.'*EXP.DA'.OR.
     &                LINET(1:7).EQ.'*EXP. D')THEN
                      GROUP = 'A'
                      DO L = 1,30
                        IF (LINET(L:L+1).EQ.': ') L1 = L+2
                        IF (LINET(L:L).EQ.':' .AND. 
     &                    LINET(L+1:L+1).NE.' ')
     &                    L1 = L+1
                        IF (L1.GT.0.AND.L.GT.L1+9.AND.
     &                   LINET(L:L).NE.' ') THEN
                          L2 = L  ! Start of group information in t-file
                          EXIT
                        ENDIF
                      ENDDO
                      CALL LTRIM (TNAME)
                      LENTNAME = MIN(15,LEN(TRIM(TNAME)))
                      LENGROUP = MIN(L2+14,LENLINE)
                      IF (LEN(TRIM(TNCHAR)).EQ.1) THEN
                        LINESTAR = '*DATA(T):'//LINET(L1:L1+9)//
     &                  ' '//TNCHAR(1:1)//' C'//
     &                  CNCHAR2//TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ELSEIF (LEN(TRIM(TNCHAR)).EQ.2) THEN              
                        LINESTAR = '*DATA(T):'//LINET(L1:L1+9)//
     &                  ' '//TNCHAR(1:2)//' C'//CNCHAR2//
     &                  TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ELSEIF (LEN(TRIM(TNCHAR)).EQ.3) THEN
                        LINESTAR = '*DATA(T):'//LINET(L1:L1+9)//
     &                  ' '//TNCHAR(1:3)//' C'//CNCHAR2//
     &                  TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ENDIF
                    ENDIF
                    IF (LINET(1:7).EQ.'*DATA(T') THEN
                      GROUP = 'D'
                      DO L = 1,30
                        IF (LINET(L:L).EQ.' ') L1 = L-1
                        IF (L1.NE.0 .AND. LINET(L:L).NE.' ') THEN
                          L2 = L  ! Start of group information in t-file
                          EXIT
                        ENDIF
                      ENDDO
                      CALL LTRIM (TNAME)
                      LENTNAME = MIN(15,LEN(TRIM(TNAME)))
                      LENGROUP = MIN(L2+14,LENLINE)
                      IF (LEN(TRIM(TNCHAR)).EQ.1) THEN
                        LINESTAR = LINET(1:L1)//' TN:'//
     &                  TNCHAR(1:1)//' C'//CNCHAR2//TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ELSEIF (LEN(TRIM(TNCHAR)).EQ.2) THEN
                        LINESTAR = LINET(1:L1)//' TN:'//
     &                  TNCHAR(1:2)//' C'//CNCHAR2//TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ELSEIF (LEN(TRIM(TNCHAR)).EQ.3) THEN
                        LINESTAR = LINET(1:L1)//' TN:'//
     &                  TNCHAR(1:3)//' C'//CNCHAR2//TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ENDIF
                    ENDIF
                    IF (LINET(1:7).EQ.'*AVERAG' .OR.
     &                  LINET(1:7).EQ.'*TIME_C') THEN
                      GROUP = 'A'
                      DO L = 1,30
                        IF (LINET(L:L).EQ.' ') L1 = L-1
                        IF (L1.NE.0 .AND. LINET(L:L).NE.' ') THEN
                          L2 = L  ! Start of group information in t-file
                          EXIT
                        ENDIF
                      ENDDO
                      CALL LTRIM (TNAME)
                      LENTNAME = MIN(15,LEN(TRIM(TNAME)))
                      LENGROUP = MIN(L2+14,LENLINE)
                      IF (LEN(TRIM(TNCHAR)).EQ.1) THEN
                        LINESTAR = LINET(1:L1)//' TN:'//
     &                  TNCHAR(1:1)//' C'//CNCHAR2//TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ELSEIF (LEN(TRIM(TNCHAR)).EQ.2) THEN
                        LINESTAR = LINET(1:L1)//' TN:'//
     &                  TNCHAR(1:2)//' C'//CNCHAR2//TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ELSEIF (LEN(TRIM(TNCHAR)).EQ.3) THEN
                        LINESTAR = LINET(1:L1)//' TN:'//
     &                  TNCHAR(1:3)//' C'//CNCHAR2//TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ENDIF
                    ENDIF
                  ELSEIF (LINET(1:1).EQ.'@') THEN
                    DATECOL = Tvicolnm(linet,'DATE')
                    YEARCOL = Tvicolnm(linet,'YEAR')
                    DOYCOL = Tvicolnm(linet,'DOY')
                    IF (DOYCOL.LE.0) DOYCOL = Tvicolnm(linet,'DAY')
                    LENLINE = LEN(TRIM(LINET))
                    LINET(LENLINE+1:LENLINE+12) = '   DAP   DAS'
                    LINET(1:1) = '@'
                    WRITE (FNUMTMP,*) ' '
                    WRITE (FNUMTMP,'(A80)') LINESTAR(1:80)
                    WRITE (FNUMTMP,*) ' '
                    WRITE (FNUMTMP,'(A180)') LINET(1:180)
                    STARNUMM = STARNUMM + 1       ! Number of datasets  
                    CFLTFILE = 'Y'
                  ELSE
                    IF (LINET(1:1).NE.'!') THEN 
                      LENLINE = LEN(TRIM(LINET))
                      IF (LENLINE.GT.3)THEN
                        CALL Getstri (LINET,COLNUM,VALUEI)
                      ENDIF
                    ENDIF
                    IF (VALUEI.EQ.TN) THEN
                      IF (DATECOL.GT.0.OR.DOYCOL.GT.0) THEN
                        IF (DATECOL.GT.0) THEN
                          CALL Getstri (LINET,DATECOL,DATE)
                        ELSEIF (DATECOL.LE.0) THEN
                          CALL Getstri (LINET,DOYCOL,DOY)
                          CALL Getstri (LINET,YEARCOL,YEAR)
                          IF (YEAR.GT.2000) YEAR = YEAR-2000
                          IF (YEAR.GT.1900) YEAR = YEAR-1900
                          DATE = YEAR*1000+DOY
                        ENDIF
                        DAP = MAX(0,CSTIMDIF(YEARPLT,DATE))
                        DAS = MAX(0,CSTIMDIF(YEARSIM,DATE))
                        DAPCHAR = TL10FROMI(DAP)
                        IF (LEN(TRIM(DAPCHAR)).EQ.1) THEN
                          DAPWRITE = '     '//DAPCHAR(1:1)
                        ELSEIF (LEN(TRIM(DAPCHAR)).EQ.2) THEN
                          DAPWRITE = '    '//DAPCHAR(1:2)
                        ELSEIF (LEN(TRIM(DAPCHAR)).EQ.3) THEN
                          DAPWRITE = '   '//DAPCHAR(1:3)
                        ENDIF
                        LENLINE = LEN(TRIM(LINET))
                        LINET(LENLINE+1:LENLINE+6) = DAPWRITE(1:6)
                        DAPCHAR = TL10FROMI(DAS)
                        IF (LEN(TRIM(DAPCHAR)).EQ.1) THEN
                          DAPWRITE = '     '//DAPCHAR(1:1)
                        ELSEIF (LEN(TRIM(DAPCHAR)).EQ.2) THEN
                          DAPWRITE = '    '//DAPCHAR(1:2)
                        ELSEIF (LEN(TRIM(DAPCHAR)).EQ.3) THEN
                          DAPWRITE = '   '//DAPCHAR(1:3)
                        ENDIF
                        LENLINE = LEN(TRIM(LINET))
                        LINET(LENLINE+1:LENLINE+6) = DAPWRITE(1:6)
                      ENDIF
                      WRITE (FNUMTMP,'(A180)') LINET
                    ENDIF
                  ENDIF
                ENDDO
 5555           CONTINUE
                ! If T-file was empty
                IF (TLINENUM.LT.4) THEN
!                  WRITE (fnumwrk,*) 'T-file was empty!'
                ENDIF
              ENDIF
            
              CLOSE(FNUMT)
              CLOSE(FNUMTMP)
              
            ELSE
               OPEN (UNIT=FNUMTMP,FILE='MEASURED.OUT',STATUS='UNKNOWN')
               CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
            ENDIF ! END OF T-FILE READS AND MEASURED.OUT WRITES
          
            
!-----------------------------------------------------------------------
            
            ! Check that -99 not multiplied or divided 
            IF (hnumgmm.LT.0.0) hnumgmm = -99
            IF (hnumamm.LT.0.0) hnumamm = -99
          
            ! Create character equivalents for outputing
            IF (gwumm.LE.0) THEN
              gwummc = ' -99.0'
            ELSE
              gwummc = ' '
              WRITE (gwummc,'(F6.3)') gwumm
            ENDIF
            IF (gwum.LE.0) THEN
              gwumc = ' -99.0'
            ELSE
              gwumc = ' '
              WRITE (gwumc,'(F6.3)') gwum
            ENDIF
            IF (hiamm.LE.0) THEN
              hiammc = ' -99.0'
            ELSE
              hiammc = ' '
              WRITE (hiammc,'(F6.3)') hiamm
            ENDIF
            IF (hiam.LE.0) THEN
              hiamc = ' -99.0'
            ELSE
              hiamc = ' '
              WRITE (hiamc,'(F6.3)') hiam
            ENDIF
            IF (hinmm.LE.0) THEN
              hinmmc = ' -99.0'
            ELSE
              hinmmc = ' '
              WRITE (hinmmc,'(F6.3)') hinmm
            ENDIF
            IF (hinm.LE.0) THEN
              hinmc = ' -99.0'
            ELSE
              hinmc = ' '
              WRITE (hinmc,'(F6.3)') hinm
            ENDIF
            IF (vnpcmm.LE.0) THEN
              vnpcmmc = ' -99.0'
            ELSE
              vnpcmmc = ' '
              WRITE (vnpcmmc,'(F6.3)') vnpcmm
            ENDIF
            IF (vnpcm.LE.0) THEN
              vnpcmc = ' -99.0'
            ELSE
              vnpcmc = ' '
              WRITE (vnpcmc,'(F6.3)') vnpcm
            ENDIF
            IF (gnpcm.LE.0) THEN
              gnpcmc = ' -99.0'
            ELSE
              gnpcmc = ' '
              WRITE (gnpcmc,'(F6.3)') gnpcm
            ENDIF
            IF (gnpcmm.LE.0) THEN
              gnpcmmc = ' -99.0'
            ELSE
              gnpcmmc = ' '
              WRITE (gnpcmmc,'(F6.3)') gnpcmm
            ENDIF

            ! Calculate DAP data
            IF (stgdoy(9).LT.9999999) THEN 
              edap = Dapcalc(stgdoy(9),plyear,plday)
            ELSE
              EDAP = -99
              stgdoy(9) = -99
            ENDIF
            IF (edatm.GT.0) THEN
             edapm = edatm
            ELSE
             edapm = -99
            ENDIF 
            IF (drdat.GT.0) THEN 
              drdap = Dapcalc(drdat,plyear,plday)
            ELSE
             drdap = -99
            ENDIF
            IF (drdatm.GT.0) THEN
             drdapm = drdatm
            ELSE
             drdapm = -99
            ENDIF 
            IF (tsdat.GT.0) THEN 
              tsdap = Dapcalc(tsdat,plyear,plday)
            ELSE
             tsdap = -99
            ENDIF
            IF (tsdatm.GT.0) THEN
             tsdapm = tsdatm
            ELSE
             tsdapm = -99
            ENDIF 
            IF (jdat.GT.0) THEN 
              jdap = Dapcalc(jdat,plyear,plday)
            ELSE
             jdap = -99
            ENDIF
            IF (jdatm.GT.0) THEN
             jdapm = jdatm
            ELSE
             jdapm = -99
            ENDIF 
            IF (adat.LT.9999999) THEN 
              adap = Dapcalc(adat,plyear,plday)
            ELSE
              adap = -99
            ENDIF
            IF (adatm.GT.0) adapm = adatm
            IF (stgdoy(5).LT.9999999) THEN 
              mdap = Dapcalc(stgdoy(5),plyear,plday)
            ELSE 
              mdap = -99
              stgdoy(5) = -99
            ENDIF
            IF (mdatm.GT.0) THEN
              mdapm = mdatm
            ELSE
              mdapm = -99 
            ENDIF
!-----------------------------------------------------------------------
            
            ! SCREEN WRITES FOR DSSAT IN SENSITIVITY RNMODE
            
            IF (FILEIOT(1:3).EQ.'DS4' .AND. CN.EQ.1
     &                                .AND. RNMODE.EQ.'E') THEN         
            
              CALL CSCLEAR5
              WRITE(*,9589)
              WRITE(*,*) ' '
              WRITE(*,9588)
              WRITE(*,9600)
              DO L = 7, 9
!                CALL CSYR_DOY(STGDOY(L),YEAR,DOY)
                CALL YR_DOY(STGDOY(L),YEAR,DOY)
                CALL Calendar(year,doy,dom,month)
                CNCTMP = 0.0
                IF (CWADSTG(L).GT..0) CNCTMP = CNADSTG(L)/CWADSTG(L)*100
                WRITE (*,'(I8,I4,1X,A3,I4,1X,I1,1X,A10,I6,F6.2,
     &           F6.1,I6,F6.1,F6.2,F6.2)')
     &           STGDOY(L),DOM,MONTH,
     &           Dapcalc(stgdoy(L),plyear,plday),L,STNAME(L),
     &           NINT(CWADSTG(L)),LAISTG(L),LNUMSTG(L),
     &           NINT(CNADSTG(L)),CNCTMP,1.0-WFPAV(L),1.0-NFPAV(L)
              ENDDO
              DO L = 1, 6
!                CALL CSYR_DOY(STGDOY(L),YEAR,DOY)
                CALL YR_DOY(STGDOY(L),YEAR,DOY)
                CALL Calendar(year,doy,dom,month)
                CNCTMP = 0.0
                IF (CWADSTG(L).GT..0) CNCTMP = CNADSTG(L)/CWADSTG(L)*100
                IF (STGDOY(L).GT.0) THEN
                  WRITE (*,'(I8,I4,1X,A3,I4,1X,I1,1X,A10,I6,F6.2,
     &             F6.1,I6,F6.1,F6.2,F6.2)')
     &             STGDOY(L),DOM,MONTH,
     &             Dapcalc(stgdoy(L),plyear,plday),L,STNAME(L),
     &             NINT(CWADSTG(L)),LAISTG(L),LNUMSTG(L),
     &             NINT(CNADSTG(L)),CNCTMP,1.0-WFPAV(L),1.0-NFPAV(L)
                ENDIF
              ENDDO
            
              WRITE(*,*)' '
              WRITE(*,*)' Press ENTER to continue'
              PAUSE ' '
              CALL CSCLEAR5
           
              WRITE (*,206)
              WRITE (*,3051) MAX(-99,edap),MAX(-99,edapm),
     x         adap,adapm,
     x         mdap,mdapm,
     x         NINT(gwam),NINT(gwamm),
     x         gwumc,gwummc,
     x         NINT(hnumam),NINT(hnumamm),
     x         hnumgm,hnumgmm,
     x         hiam,hiamm,
     x         laix,laixm,
     x         lnumsm,lnumsmm,
     x         NINT(tnumam),NINT(tnumamm),
     x         NINT(cwam),NINT(cwamm),
     x         NINT(vwam),NINT(vwamm),
     x         NINT(rwam),NINT(rwamm),
     x         NINT(carboac),NINT(carboacm),
     x         NINT(senwatc),NINT(senwatcm),
     x         NINT(rswam),NINT(rswamm)
 3051         FORMAT (
     x        6X, 'Emergence (DAP)                 ',4X,I7,  6X,I7,  /,
     x        6X, 'Anthesis (DAP)                  ',4X,I7,  6X,I7,  /,
     x        6X, 'Maturity (DAP)                  ',4X,I7,  6X,I7,  /,
     x        6X, 'Product wt (kg dm/ha;no loss)   ',4X,I7, 6X,I7,  /,
     x        6X, 'Product unit weight (g dm)      ',5X,A6, 7X,A6,  /,
     x        6X, 'Product number (no/m2)          ',4X,I7,  6X,I7,  /,
     x        6X, 'Product number (no/group)       ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Product harvest index (ratio)   ',4X,F7.2,6X,F7.2,/,
     x        6X, 'Maximum leaf area index         ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Final leaf number (one axis)    ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Final shoot number (#/m2)       ',4X,I7  ,6X,I7  ,/,
     x        6X, 'Canopy (tops) wt (kg dm/ha)     ',4X,I7,  6X,I7,  /,
     x        6X, 'Vegetative wt (kg dm/ha)        ',4X,I7,  6X,I7,  /,
     x        6X, 'Root wt (kg dm/ha)              ',4X,I7,  6X,I7,  /,
     x        6X, 'Assimilate wt (kg dm/ha)        ',4X,I7,  6X,I7,  /,
     x        6X, 'Senesced wt (kg dm/ha)          ',4X,I7,  6X,I7,  /,
     x        6X, 'Reserves wt (kg dm/ha)          ',4X,I7,  6X,I7)

              WRITE(*,*)' '
              WRITE(*,*)' Press ENTER to continue'
              PAUSE ' '
              CALL CSCLEAR5
     
              WRITE (*,206)
              WRITE (*,3052)                              
     x         nuam,nuamm,
     x         sennatc,sennatcm,
     x         cnam,cnamm,
     x         rnam,rnamm,
     x         vnam,vnamm,
     x         gnam,gnamm,
     x         hinm,hinmm,
     x         gnpcm,gnpcmm,
     x         vnpcm,vnpcmm,
     x         NINT(cwaa),NINT(cwaam),
     x         vnaa,cnaam,
     x         lnpca,lnpcam
 3052         FORMAT (
     x        6X, 'N uptake (kg/ha)                 ',4X,F7.1,6X,F7.1,/,
     x        6X, 'N senesced (kg/ha)               ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Above-ground N (kg/ha)           ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Root N (kg/ha)                   ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Vegetative N (kg/ha)             ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Product N (kg/ha)                ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Product N harvest index (ratio)  ',4X,F7.2,6X,F7.2,/,
     x        6X, 'Product N (%)                    ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Vegetative N (%)                 ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Leaf+stem wt,anthesis (kg dm/ha) ',4X,  I7,6X,I7,  /,
     x        6X, 'Leaf+stem N,anthesis (kg/ha)     ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Leaf N,anthesis (%)              ',4X,F7.1,6X,F7.1)

              WRITE(*,*)' '
              WRITE(*,*)' Press ENTER to continue'
              PAUSE ' '
              CALL CSCLEAR5
              CALL CSCLEAR5
              CALL CSCLEAR5
              CALL CSCLEAR5
            
            ENDIF
            
            ! END OF SCREEN WRITES FOR DSSAT SENSITIVITY RNMODE
            
!-----------------------------------------------------------------------

            IF (IDETO.NE.'N'.OR.IDETL.EQ.'A') THEN
            
              ! PLANT EVALUATION (MEASURED - SIMULATED COMPARISONS)
C  FO - 07/16/2021 Added more characters for H#AMS and H#GMS because of GLUE error.
              
!              WRITE (fnumwrk,*) 'Writing EVALUATION'
              
              EVHEADER = ' '
              FNAMETMP = ' '
              EVHEADER(1:14) = '*EVALUATION : '
              FNAMETMP(1:12) = 'Evaluate.OUT'
              IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
               OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,STATUS = 'UNKNOWN')
               WRITE (FNUMTMP,'(A17)') '$PLANT EVALUATION'
               CLOSE(FNUMTMP)
               EVALOUT = 0
               EVHEADNM = 0
              ENDIF
              
              IF (EVHEADNM.EQ.0) THEN
                IF (EXCODE.NE.EXCODEP.AND.EVALOUT.GT.1 .OR.
     &              RUN.EQ.1.AND.RUNI.EQ.1) THEN
                 EVHEADNM = EVHEADNM + 1
                 OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,
     &            POSITION = 'APPEND')
                 WRITE (FNUMTMP,*) ' '
                 IF (EVHEADNM.EQ.1) THEN
                   WRITE (FNUMTMP,993) EVHEADER,EXCODE,
     &              ENAME(1:25),MODNAME
  993              FORMAT (A14,A10,'  ',A25,2X,A8,/)
                 ELSE
                   WRITE (FNUMTMP,1995) EVHEADER, 
     &             'MANY??????','  REMAINING EXPERIMENTS  ',MODNAME
 1995              FORMAT (A14,A10,A25,A8,/)
                 ENDIF
                 WRITE (FNUMTMP,994)
  994            FORMAT ('@RUN',
     x            ' EXCODE    ',
     x            '  TRNO RN',
     x            ' CR',
     x            ' EDAPS EDAPM',
     x            ' DRAPS DRAPM',
     x            ' TSAPS TSAPM',
     x            ' ADAPS ADAPM',
     x            ' MDAPS MDAPM',
     x            ' HWAMS HWAMM',
     x            ' HWUMS HWUMM',
     x            '    H#AMS H#AMM',
     x            '    H#GMS H#GMM',
     x            ' LAIXS LAIXM',
     x            ' L#SMS L#SMM',
     x            ' T#AMS T#AMM',
     x            ' CWAMS CWAMM',
     x            ' VWAMS VWAMM',
     x            ' HIAMS HIAMM',
     x            ' HN%MS HN%MM',
     x            ' VN%MS VN%MM',
     x            ' CNAMS CNAMM',
     x            ' HNAMS HNAMM',
     x            ' HINMS HINMM')
                 CLOSE(FNUMTMP)
                ENDIF
              ENDIF
                
              IF (EXCODE.NE.EXCODEP) EVALOUT = 0
              EVALOUT = EVALOUT + 1
              
              IF (STGDOY(5).GT.0 .AND. ADAT.GT.0) THEN
                AMDAYS = Dapcalc(stgdoy(5),plyear,plday) 
     X                 - Dapcalc(adat,plyear,plday)
              ELSE
                AMDAYS = -99
              ENDIF 
              IF (MDATM.GT.0 .AND. ADATM.GT.0) THEN
                AMDAYM = Dapcalc(mdatm,plyear,plday) 
     X                 - Dapcalc(adatm,plyear,plday)
              ELSE
                AMDAYM = -99
              ENDIF 
              
              OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,POSITION = 'APPEND') 
              
              WRITE (FNUMTMP,8404) RUN,EXCODE,TN,RN,CR,
     x        edap,edapm,
     x        drdap,drdapm,
     x        tsdap,tsdapm,
     x        adap,adapm,
     x        mdap,mdapm,         
     x        NINT(gwam),NINT(gwamm),
     x        gwumc,gwummc,
     x        NINT(hnumam),NINT(hnumamm),
     x        hnumgm,hnumgmm,
     x        laix,laixm,
     x        lnumsm,lnumsmm,
     x        NINT(tnumam),NINT(tnumamm),
     x        NINT(cwam),NINT(cwamm),
     x        NINT(vwam),NINT(vwamm),
     x        hiamc,hiammc,
     x        gnpcm,gnpcmm,
     x        vnpcmc,vnpcmmc,
     x        NINT(cnam),NINT(cnamm),
     x        NINT(gnam),NINT(gnamm),
     x        hinmc,hinmmc             
              
 8404         FORMAT (I4,1X,A10,I6,I3,1X,A2,
     x        I6,I6,
     x        I6,I6,
     x        I6,I6,
     x        I6,I6,
     x        I6,I6,
     x        I6,I6,
     x        A6,A6,
     x        1X,I8,I6,
     x        1X,F8.1,F6.1,
     x        F6.1,F6.1,
     x        F6.1,F6.1,
     x        I6,I6,
     x        I6,I6,
     x        I6,I6,
     x        A6,A6,
     x        F6.1,F6.1,
     x        A6,A6,
     x        I6,I6,
     x        I6,I6,
     x        A6,A6)
!     x        I7,I7,
!     x        I7,I7,
!     x        I6,I6)
              
              Close(FNUMTMP)       

!     VSH CSV output corresponding to Evaluate.OUT
      IF (FMOPT == 'C') then
         CALL CsvOutEvalCsCer(EXCODE, RUN, TN, RN,  REP, 
     &CR,Edap,Edapm,Drdap,Drdapm,Tsdap,Tsdapm,Adap,Adapm,Mdap,
     &Mdapm, Gwam, Gwamm, Gwumc, Gwummc, Hnumam, Hnumamm, Hnumgm,
     &Hnumgmm,Laix,Laixm, Lnumsm,Lnumsmm,Tnumam, Tnumamm, Cwam, 
     &Cwamm, Vwam, Vwamm, Hiamc,Hiammc,Gnpcm,Gnpcmm,Vnpcmc,Vnpcmmc,
     &Cnam, Cnamm, Gnam, Gnamm, Hinmc, Hinmmc,  
     &vCsvlineEvalCsCer, vpCsvlineEvalCsCer, vlngthEvalCsCer) 
     
         CALL LinklstEvalCsCer(vCsvlineEvalCsCer)
      END IF 
      
            ELSE
            
               OPEN (UNIT=FNUMTMP,FILE='EVALUATE.OUT',STATUS='UNKNOWN')
               CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
               
            ENDIF  ! END OF PLANT EVALUATION
            
!-----------------------------------------------------------------------
            
            ! OVERVIEW
            
            IF (IDETO.EQ.'Y'.OR.IDETL.EQ.'A') THEN

!              WRITE (fnumwrk,*) 'Writing OVERVIEW'
              
              FNAMETMP = ' '
              ! TF - Updated OVERVIEW.OUT name to avoid issues
              ! with case sensitive systems (07/27/2021) 
              FNAMETMP(1:12) = 'OVERVIEW.'//out
              
              IF (FILEIOT(1:2).EQ.'DS') THEN
                IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
                  OPEN (UNIT = FNUMTMP, FILE = FNAMETMP)
                  WRITE(FNUMTMP,'("*SIMULATION OVERVIEW FILE")')
                ELSE
                  INQUIRE (FILE = FNAMETMP, EXIST = FEXIST)
                  IF (FEXIST) THEN
                    INQUIRE (FILE = 'OVERVIEW.OUT',OPENED = fopen)
                    IF (.NOT.fopen) THEN
                      OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,
     &                 POSITION = 'APPEND')
                    ENDIF
                  ENDIF  
                ENDIF
                WRITE (FNUMTMP,*) ' '
                CALL HEADER(1, FNUMTMP, RUN)
              ELSE
                IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
                  OPEN (UNIT = FNUMTMP, FILE = FNAMETMP)
                  WRITE (FNUMTMP,'(A15)') '$OVERVIEW'
                ELSE
                 OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,POSITION='APPEND')
                ENDIF
              
                WRITE (FNUMTMP,'(/,A77,/)') OUTHED
                WRITE (FNUMTMP,103) MODEL
                WRITE (FNUMTMP,1031) MODNAME
                WRITE (FNUMTMP,1032) FILENEW
 1032           FORMAT (' FILE             ',A12)
                WRITE (FNUMTMP,104)
     &           EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
                WRITE (FNUMTMP,102) TN, TNAME
                WRITE (FNUMTMP,107) CR,VARNO,VRNAME
                CALL Calendar (plyear,plday,dom,month)
                WRITE (FNUMTMP,108)month,dom,NINT(pltpop),NINT(rowspc)
                WRITE (fnumtmp,109) tmaxx,tminn,NINT(co2max)
  109           FORMAT (' ENVIRONMENT      ','Tmax (max): ',F4.1,
     &           '  Tmin (min): ',F5.1,
     &           '  Co2 (max):',I4)
                WRITE (fnumtmp,110) iswwat, iswnit
  110           FORMAT (' MODEL SWITCHES   ','Water: ',A1,
     &           '  Nitrogen: ',A1)
              ENDIF
              
              !edap = Dapcalc(stgdoy(9),plyear,plday)
              !edapm = Dapcalc(edatm,plyear,plday)
              IF (edapm.GT.200) THEN
!                WRITE (Fnumwrk,*)' '
!                WRITE (Fnumwrk,'(A31,A31,A11)')
!     &           'Measured emergence over 200DAP ',
!     &           'Maybe reported before planting.',
!     &           'Check files'
              ENDIF
              !adap = Dapcalc(adat,plyear,plday)
              !adapm = Dapcalc(adatm,plyear,plday)
              !mdap = Dapcalc(stgdoy(5),plyear,plday)
              !mdapm = Dapcalc(mdatm,plyear,plday)
              
              WRITE(FNUMTMP,9589)
              WRITE(fnumtmp,*)' '
              WRITE(FNUMTMP,'(A11,I4,A3,A60)')
     &         ' RUN NO.   ',RUN,'  ',ENAME                             
              IF (DYNAMIC.EQ.SEASEND) THEN
                WRITE(fnumtmp,*)' '
                WRITE(fnumtmp,'(A50,A25)')
     &          ' NB. RUN TERMINATED PREMATURELY (PROBABLY BECAUSE ',
     &          'OF MISSING WEATHER DATA) '
              ENDIF
              WRITE(fnumtmp,9588)
              WRITE(fnumtmp,9600)
              DO L = 7, 9
!                 CALL CSYR_DOY(STGDOY(L),YEAR,DOY)
                 CALL YR_DOY(STGDOY(L),YEAR,DOY)
                 CALL Calendar(year,doy,dom,month)
                 CNCTMP = 0.0
                 IF (CWADSTG(L).GT.0.0) 
     &             CNCTMP = CNADSTG(L)/CWADSTG(L)*100
                 WRITE (fnumtmp,'(I8,I4,1X,A3,I4,1X,I1,1X,A10,I6,F6.2, 
     &            F6.1,I6,F6.1,F6.2,F6.2)')
     &            STGDOY(L),DOM,MONTH,
     &            Dapcalc(stgdoy(L),plyear,plday),L,STNAME(L),
     &            NINT(CWADSTG(L)),LAISTG(L),LNUMSTG(L),
     &            NINT(CNADSTG(L)),CNCTMP,1.0-WFPAV(L),1.0-NFPAV(L)
              ENDDO
              DO L = 1, 6
!                CALL CSYR_DOY(STGDOY(L),YEAR,DOY)
                CALL YR_DOY(STGDOY(L),YEAR,DOY)
                CALL Calendar(year,doy,dom,month)
                CNCTMP = 0.0
                IF (CWADSTG(L).GT.0.0)CNCTMP = CNADSTG(L)/CWADSTG(L)*100
                IF (STGDOY(L).GT.0.AND.STGDOY(L).LT.9999999) THEN
                  WRITE (fnumtmp,'(I8,I4,1X,A3,I4,1X,I1,1X,A10,I6,F6.2,
     &             F6.1,I6,F6.1,F6.2,F6.2)')
     &             STGDOY(L),DOM,MONTH,
     &             Dapcalc(stgdoy(L),plyear,plday),L,STNAME(L),
     &             NINT(CWADSTG(L)),LAISTG(L),LNUMSTG(L),
     &             NINT(CNADSTG(L)),CNCTMP,1.0-WFPAV(L),1.0-NFPAV(L)
                ENDIF
              ENDDO
              IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
              WRITE(fnumtmp,*)' '
              WRITE(fnumtmp,*)'BIOMASS = Above-ground dry wt (kg/ha)'
              WRITE(fnumtmp,*)'LEAF AREA  = Leaf area index (m2/m2)'
              WRITE(fnumtmp,*)
     &         'LEAF NUMBER  = Leaf number produced on main axis'
              WRITE(fnumtmp,*)'CROP N  = Above-ground N (kg/ha)'
              WRITE(fnumtmp,*)'CROP N% = Above-ground N conc (%)'
              WRITE(fnumtmp,*)
     &         'H2O STRESS = Photosynthesis stress,average (0-1,0=none)'
              WRITE(fnumtmp,*)
     &         'N STRESS = Photosynthesis stress,average (0-1,0=none)'
              ENDIF
              WRITE(fnumtmp,*)' '
              
              WRITE (FNUMTMP,206)
              WRITE (FNUMTMP,305) MAX(-99,edap),MAX(-99,edapm),
     x         adap,adapm,
     x         mdap,mdapm,
     x         NINT(gwam),NINT(gwamm),
     x         gwumc,gwummc,
     x         NINT(hnumam),NINT(hnumamm),
     x         hnumgm,hnumgmm,
     x         hiam,hiamm,
     x         laix,laixm,
     x         lnumsm,lnumsmm,
     x         NINT(tnumam),NINT(tnumamm),
     x         NINT(cwam),NINT(cwamm),
     x         NINT(vwam),NINT(vwamm),
     x         NINT(rwam),NINT(rwamm),
     x         NINT(carboac),NINT(carboacm),
     x         NINT(senwatc),NINT(senwatcm),
     x         NINT(rswam),NINT(rswamm),
     x         nuam,nuamm,
     x         sennatc,sennatcm,
     x         cnam,cnamm,
     x         rnam,rnamm,
     x         vnam,vnamm,
     x         gnam,gnamm,
     x         hinm,hinmm,
     x         gnpcm,gnpcmm,
     x         vnpcm,vnpcmm,
     x         NINT(cwaa),NINT(cwaam),
     x         vnaa,cnaam,
     x         lnpca,lnpcam
              
  305         FORMAT (
     x        6X, 'Emergence (DAP)                  ',4X,I7,  6X,I7,  /,
     x        6X, 'Anthesis (DAP)                   ',4X,I7,  6X,I7,  /,
     x        6X, 'Maturity (DAP)                   ',4X,I7,  6X,I7,  /,
     x        6X, 'Product wt (kg dm/ha;no loss)    ',4X,I7, 6X,I7,  /,
     x        6X, 'Product unit weight (g dm)       ',5X,A6, 7X,A6,  /,
     x        6X, 'Product number (no/m2)           ',4X,I7,  6X,I7,  /,
     x        6X, 'Product number (no/group)        ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Product harvest index (ratio)    ',4X,F7.2,6X,F7.2,/,
     x        6X, 'Maximum leaf area index          ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Final leaf number (one axis)     ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Final shoot number (#/m2)        ',4X,I7  ,6X,I7  ,/,
     x        6X, 'Canopy (tops) wt (kg dm/ha)      ',4X,I7,  6X,I7,  /,
     x        6X, 'Vegetative wt (kg dm/ha)         ',4X,I7,  6X,I7,  /,
     x        6X, 'Root wt (kg dm/ha)               ',4X,I7,  6X,I7,  /,
     x        6X, 'Assimilate wt (kg dm/ha)         ',4X,I7,  6X,I7,  /,
     x        6X, 'Senesced wt (kg dm/ha)           ',4X,I7,  6X,I7,  /,
     x        6X, 'Reserves wt (kg dm/ha)           ',4X,I7,  6X,I7,  /,
     x        6X, 'N uptake (kg/ha)                 ',4X,F7.1,6X,F7.1,/,
     x        6X, 'N senesced (kg/ha)               ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Above-ground N (kg/ha)           ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Root N (kg/ha)                   ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Vegetative N (kg/ha)             ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Product N (kg/ha)                ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Product N harvest index (ratio)  ',4X,F7.2,6X,F7.2,/,
     x        6X, 'Product N (%)                    ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Vegetative N (%)                 ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Leaf+stem wt,anthesis (kg dm/ha) ',4X,  I7,6X,I7,  /,
     x        6X, 'Leaf+stem N,anthesis (kg/ha)     ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Leaf N,anthesis (%)              ',4X,F7.1,6X,F7.1)
              
              IF (run.EQ.1.AND.runi.EQ.1) THEN
            
              WRITE (FNUMTMP,'(/,A55,/,A55)')
     x        '  Seed N must be added to N uptake to obtain a         ',
     x        '  balance with N in above-ground plus root material    '
              WRITE (FNUMTMP,'(/,17(A55,/))')
     x        '  Measured data are obtained from the A file,either    ',
     x        '  directly or by calculation from other other variables',
     x        '  using the expressions given below:                   ',
     x        '                                                       ',
     x        '  Product wt       Harvest wt (HWAH) /                 ',
     x        '                               (Harvest%/100) (HPC)    ',
     x        '  Canopy wt        Grain wt (HWAM)+Vegetative wt (VWAm)',
     x        '  Vegetative wt    Canopy wt (CWAM) - grain wt (HWAM)  ',
     x        '    = leaf+stem+retained dead material                 ',
     x        '  Product unit wt  Grain yield (HWAM)/Grain # (G#AM)   ',
     x        '  Product #/area   Product#/tiller (H#SM) *            ',
     x        '                                   tiller number (T#AM)',
     x        '  Product #/group  Product#/area (H#AM) /              ',
     x        '                                tiller number (T#AM)   ',
     x        '  Harvest index    Product wt (HWAM)/Canopy wt.(CWAM)  ',
     x        '                                                       ',
     x        '  The same procedure is followed for nitrogen aspects  '
     
              ENDIF
            
              WRITE(fnumtmp,5500)
            
              PFPAV = -99.0
              PFGAV = -99.0
              DASH = ' - '
              DO tvI1 = 1,5  
                IF (TVI1.EQ.1)THEN
                  WRITE(fnumtmp,600) stname(8), dash, stname(tvi1), 
     &             daysc(tvI1), TMAXav(tvI1),TMINav(tvI1),sRADav(tvI1),
     &             DAYLav(tvI1),RAINcp(tvI1),ETC(tvI1),1.0-wfpav(tvi1),
     &             1.0-wfgav(tvi1), 1.0-nfpav(tvi1), 1.0-nfgav(tvi1), 
     &             PFPAV(TVI1), PFGAV(TVI1)
                ELSE
                  IF (DAYSC(tvi1).GT.0) THEN
                    WRITE(fnumtmp,600) stname(tvi1-1),dash,stname(tvi1),
     &              daysc(tvI1), TMAXav(tvI1),TMINav(tvI1),sRADav(tvI1),
     &              DAYLav(tvI1),RAINcp(tvI1),ETC(tvI1),1.0-wfpav(tvi1),
     &              1.0-wfgav(tvi1), 1.0-nfpav(tvi1), 1.0-nfgav(tvi1), 
     &              PFPAV(TVI1), PFGAV(TVI1)
                  ENDIF
                ENDIF
  600           FORMAT(1X,A10,A3,A10,I5,3F6.1,F7.2,2F7.1,4F7.3,2F7.2)
  610           FORMAT(1X,A10,13X,I5,3F6.1,F7.2,2I7,6F7.3)
              ENDDO
              IF (daysc(5).gt.0) then
                WRITE(fnumtmp,*) ' ' 
                WRITE(fnumtmp,600) stname(8),dash,stname(5), 
     &           daysc(0), TMAXav(0),TMINav(0),sRADav(0), 
     &           DAYLav(0), RAINcp(0),ETC(0), 1.0-wfpav(0), 
     &           1.0-wfgav(0), 1.0-nfpav(0), 1.0-nfgav(0), 
     &           PFPAV(0), PFGAV(0)
              ENDIF     
            
              !Resource productivity calculations
   
              DMP_Rain = -99.
              GrP_Rain = -99.
              DMP_ET = -99.
              GrP_ET = -99.
              DMP_EP = -99.
              GrP_EP = -99.
              DMP_Irr = -99.    
              GrP_Irr = -99.
 
              DMP_NApp = -99.
              GrP_NApp = -99.
              DMP_NUpt = -99.
              GrP_NUpt = -99.
 
              IF (RAINCP(0) > 1.E-3) THEN
                DMP_Rain = CWAM / RAINCp(0) 
                GrP_Rain = GWAM  / RAINCp(0)
              ENDIF
            
              IF (ETC(0) > 1.E-3) THEN
                DMP_ET = CWAM / ETC(0) 
                GrP_ET = GWAM  / ETC(0) 
              ENDIF
            
              IF (EPC(0) > 1.E-3) THEN
                DMP_EP = CWAM / EPC(0) 
                GrP_EP = GWAM  / EPC(0) 
              ENDIF
 
              IF (TOTIR > 1.E-3) THEN
                DMP_Irr = CWAM / TOTIR 
                GrP_Irr = GWAM  / TOTIR
              ENDIF
 
              IF (ISWNIT.NE.'N') THEN
                IF (Amtnit > 1.E-3) THEN
                  DMP_NApp = CWAM / Amtnit
                  GrP_NApp = GWAM  / Amtnit
                ENDIF
            
                IF (NUAD > 1.E-3) THEN
                  DMP_NUpt = CWAM / NUAD
                  GrP_NUpt = GWAM  / NUAD
                ENDIF
              ENDIF !ISWNIT == 'Y'
                        
              ! Productiviity outputs not written if run aborted        
              IF (DYNAMIC.NE.SEASEND) THEN
                WRITE (FNUMTMP, 1200) daysc(0), 
     &           RAINCP(0), DMP_Rain*0.1,DMP_Rain,GrP_Rain*0.1,GrP_Rain,
     &           ETC(0),  DMP_ET*0.1,   DMP_ET,   GrP_ET*0.1,   GrP_ET, 
     &           EPc(0),  DMP_EP*0.1,   DMP_EP,   GrP_EP*0.1,   GrP_EP
 
                IF (TOTIR > 1.E-3) THEN
                 WRITE(FNUMTMP, 1210) 
     &            TOTIR, DMP_Irr*0.1, DMP_Irr, GrP_Irr*0.1, GrP_Irr
                ENDIF
 
                IF (ISWNIT.NE.'N') THEN
                  IF (Amtnit > 1.E-3) THEN
                   WRITE(FNUMTMP, 1220) Amtnit, DMP_NApp, GrP_NApp 
                  ENDIF
                          
                  IF (NUAD > 1.E-3) THEN
                   WRITE(FNUMTMP, 1230) NUAD, DMP_NUpt,GrP_NUpt
                  ENDIF
                ENDIF !ISWNIT == 'Y'
              ENDIF
            
              WRITE(FNUMTMP,270)
              IF (CR.EQ.'WH') THEN 
                WRITE(FNUMTMP,300) 'WHEAT', NINT(GWAM)
              ELSEIF (CR.EQ.'BA') THEN 
                WRITE(FNUMTMP,300) 'BARLEY', NINT(GWAM)
              ENDIF  
              WRITE(FNUMTMP,'(110("*"))')
            
              CLOSE(FNUMTMP)   

            ELSE
            
               OPEN (UNIT=FNUMTMP,FILE='OVERVIEW,OUT')
               CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
               
            ENDIF  ! END OF OVERVIEW

!-----------------------------------------------------------------------
            
            ! CSM SUMMARY ... not used in CROPSIM
            
            ! Store Summary labels and values in arrays to send to
            ! OPSUM routine for printing.  Integers are temporarily
            ! saved as real numbers for placement in real array.
            
            LABEL(1) = 'ADAT '; VALUE(1) = FLOAT(adat)
            LABEL(2) = 'MDAT '; VALUE(2) = FLOAT(stgdoy(5))
            LABEL(3) = 'DWAP '; VALUE(3) = sdrate
            LABEL(4) = 'CWAM '; VALUE(4) = cwam
            LABEL(5) = 'HWAM '; VALUE(5) = gwam
            LABEL(6) = 'HWAH '; VALUE(6) = gwam * hpc / 100.
            LABEL(7) = 'BWAH '; VALUE(7) = vwam * hbpc / 100. 
            
            LABEL(8)  = 'HWUM '; VALUE(8) = gwum
            LABEL(9)  = 'H#AM '; VALUE(9) = hnumam
            LABEL(10) = 'H#UM '; VALUE(10) = hnumgm
            LABEL(11) = 'NUCM '; VALUE(11) = nuad
            LABEL(12) = 'CNAM '; VALUE(12) = cnam
            LABEL(13) = 'GNAM '; VALUE(13) = gnam
            LABEL(14) = 'PWAM '; VALUE(14) = PWAM    
            LABEL(15) = 'LAIX '; VALUE(15) = LAIX    
            LABEL(16) = 'HIAM '; VALUE(16) = HIAM    
 
            LABEL(17) = 'DMPPM'; VALUE(17) = DMP_Rain 
            LABEL(18) = 'DMPEM'; VALUE(18) = DMP_ET                     
            LABEL(19) = 'DMPTM'; VALUE(19) = DMP_EP                     
            LABEL(20) = 'DMPIM'; VALUE(20) = DMP_Irr
            LABEL(21) = 'DPNAM'; VALUE(21) = DMP_NApp
            LABEL(22) = 'DPNUM'; VALUE(22) = DMP_NUpt
            
            LABEL(23) = 'YPPM ' ; VALUE(23) = GrP_Rain                  
            LABEL(24) = 'YPEM ' ; VALUE(24) = GrP_ET                   
            LABEL(25) = 'YPTM ' ; VALUE(25) = GrP_EP                    
            LABEL(26) = 'YPIM ' ; VALUE(26) = GrP_Irr
            LABEL(27) = 'YPNAM' ; VALUE(27) = GrP_NApp
            LABEL(28) = 'YPNUM' ; VALUE(28) = GrP_NUpt
 
            LABEL(29) = 'EDAT ' ; VALUE(29) = FLOAT(STGDOY(9))  
            
            LABEL(30) = 'NDCH ' ; VALUE(30) = FLOAT(DAYSC(0)) 
            LABEL(31) = 'TMINA' ; VALUE(31) = TMINAV(0)       
            LABEL(32) = 'TMAXA' ; VALUE(32) = TMAXAV(0)       
            LABEL(33) = 'SRADA' ; VALUE(33) = SRADAV(0)       
            LABEL(34) = 'DAYLA' ; VALUE(34) = DAYLAV(0)       
            LABEL(35) = 'CO2A ' ; VALUE(35) = CO2AV(0)        
            LABEL(36) = 'PRCP ' ; VALUE(36) = RAINCP(0)       
            LABEL(37) = 'ETCP ' ; VALUE(37) = ETC(0)      

            IF (FILEIOT(1:2).EQ.'DS') CALL SUMVALS (SUMNUM,LABEL,VALUE)
            
            ! END OF CSM SUMMARY
            
!-----------------------------------------------------------------------
            
            ! PLANT RESPONSES (SIMULATED)
            
            IF (IDETL.EQ.'D'.OR.IDETL.EQ.'A') THEN

!              WRITE (fnumwrk,*) 'Writing PLANT RESPONSES (SIMULATED)'
              
              FNAMETMP = ' '
              FNAMETMP(1:12) = 'Plantres.'//out
              IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
                OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,STATUS = 'UNKNOWN')
                WRITE (FNUMTMP,'(A28)') '$PLANT RESPONSES (SIMULATED)'
                CLOSE(FNUMTMP)
              ENDIF
              
              IF (EXCODE.NE.EXCODEP.OR.TNAME(1:1).EQ.'*') THEN
                OPEN (UNIT=FNUMTMP,FILE=FNAMETMP,POSITION='APPEND')
                WRITE (FNUMTMP,*) ' '
                WRITE (TLINETMP,9951) EXCODE,MODNAME
 9951           FORMAT ('*RESPONSES(S):',A10,'  ',A8)
                IF (TNAME(1:1).EQ.'*') THEN
                  WRITE (FNUMTMP,'(A180)') TLINETMP
                ELSE
                  WRITE (FNUMTMP,'(A180)') TLINETMP
                ENDIF
                PLDAYP = 0
                WRITE (FNUMTMP,97)
   97           FORMAT ('@  RUN',
     x          ' EXCODE    ',
     x          'TRNO RN',
     x          '    CR',
     x          '  PDOY  EDAP',
     x          ' TSDAP  ADAP  MDAP',
     x          '  HWAM  HWUM',
     x          '  H#AM  H#GM  LAIX  L#SM  T#AM',
     x          '  CWAM  VWAM  HIAM  RWAM',
     x          '  HN%M  TNAM',
     x          '  CNAM  HNAM',
     x          '  HINM PLPOP',
     x          '  NICM',
     x          ' SRADA TMAXA TMINA  PRCP')
              ELSE
                OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,POSITION ='APPEND')
              ENDIF
              
              IF (plday.LT.pldayp) THEN
                IF (varno.EQ.varnop) THEN
                  pldayp = plday + 365
                ELSE
                  pldayp = plday
                ENDIF
              ELSE
                pldayp = plday
              ENDIF
              varnop = varno
              
              WRITE (FNUMTMP,7401) RUN,EXCODE,TN,RN,CR,
     x         PLDAYP,
     x         edap,tsdap,adap,mdap,
     x         NINT(gwam),gwumc,
     x         NINT(hnumam),NINT(hnumgm),laix,lnumsm,tnumam,
     x         NINT(cwam),
     x         NINT(vwam),hiamc, NINT(rwam),
     x         gnpcmc,NINT(AMAX1(-99.0,cnam+rnam)),
     x         NINT(cnam),NINT(gnam),
     x         hinmc,pltpop,
     x         NINT(amtnit),
     &         sradav(0),tmaxav(0),tminav(0),NINT(raincp(0))
              
 7401          FORMAT (I6,1X,A10,1X,I3,I3,4X,A2,
     x         I6,
     x         I6,I6,I6,I6,
     x         I6,A6,
     x         I6,I6,F6.1,F6.1,F6.1,
     x         I6,
     x         I6,A6,I6,
     x         A6,I6,
     x         I6,I6,
     x         A6,F6.1,
     x         I6,
     x         3F6.1,I6)
              
              CLOSE(FNUMTMP)       ! END OF PLANT RESPONSES (SIMULATED)
            
            ELSE
               OPEN(UNIT=FNUMTMP,FILE='Plantres.out',STATUS = 'UNKNOWN')
               CLOSE(UNIT=FNUMTMP,STATUS='DELETE')
            ENDIF
            
!-----------------------------------------------------------------------
            
            ! PLANT RESPONSES (MEASURED)

            IF (IDETL.EQ.'D'.OR.IDETL.EQ.'A') THEN
            
!              WRITE (fnumwrk,*) 'Writing PLANT RESPONSES (MEASURED)'
              
              FNAMETMP = ' '
              FNAMETMP(1:12) = 'Plantrem.'//out
              IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
               OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,STATUS = 'UNKNOWN')
               WRITE (FNUMTMP,'(A27)') '$PLANT RESPONSES (MEASURED)'
               CLOSE(FNUMTMP)
              ENDIF
              
              IF (EXCODE.NE.EXCODEP .OR.TNAME(1:1).EQ.'*') THEN
                OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,POSITION='APPEND')
                WRITE (FNUMTMP,*) ' '
                TLINETMP = ' '
                WRITE (TLINETMP,99511) EXCODE,MODNAME
99511           FORMAT ('*RESPONSES(M):',A10,'  ',A8)
                IF (TNAME(1:1).EQ.'*') THEN
                  WRITE (FNUMTMP,'(A180)') TLINETMP
                ELSE
                  WRITE (FNUMTMP,'(A180)') TLINETMP
                ENDIF
                WRITE (FNUMTMP,97)
              ELSE
                OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,POSITION='APPEND')
              ENDIF
              
              IF (CNAMM.GT.0.0 .AND. RNAMM.GT.0.0) THEN
                tnamm = cnamm+rnamm
              ELSE
                tnamm = -99.0
              ENDIF
              
              WRITE (FNUMTMP,7402) RUN,EXCODE,TN,RN,CR,
     x         PLDAYP,
     x         MAX(-99,edapm),
     x         tsdapm,
     x         Dapcalc(adatm,plyear,plday),
     x         Dapcalc(mdatm,plyear,plday),
     x         NINT(gwamm),gwummc,
     x         NINT(hnumamm),NINT(hnumgmm),laixm,lnumsmm,tnumamm,
     x         NINT(cwamm),
     x         NINT(vwamm),hiammc, NINT(rwamm),
     x         gnpcmmc,NINT(tnamm),
     x         NINT(cnamm),NINT(gnamm),
     x         hinmmc,pltpop,
     x         NINT(amtnit),
     &         sradav(0),tmaxav(0),tminav(0),NINT(raincp(0))
              
 7402          FORMAT (I6,1X,A10,1X,I3,I3,4X,A2,
     x         I6,
     x         I6,I6,I6,I6,
     x         I6,A6,
     x         I6,I6,F6.1,F6.1,F6.1,
     x         I6,
     x         I6,A6,I6,
     x         A6,I6,
     x         I6,I6,
     x         A6,F6.1,
     x         I6,
     x         3F6.1,I6)
              
              CLOSE(FNUMTMP)       

            ELSE
               OPEN(UNIT=FNUMTMP,FILE='Plantrem.out',STATUS = 'UNKNOWN')
               CLOSE(UNIT=FNUMTMP,STATUS='DELETE')
            ENDIF    ! END OF PLANT RESPONSES (MEASURED)
            
!-----------------------------------------------------------------------

            IF (IDETL.EQ.'A') THEN
            
              ! PLANT ERRORS (A-file data)
              
!              WRITE (fnumwrk,*) 'Writing PLANT ERRORS (A)'
              
              FNAMETMP = ' '
              FNAMETMP(1:12) = 'Plantera.'//out
              IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
                OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,STATUS = 'UNKNOWN')
                WRITE (FNUMTMP,'(A21)') '$ERRORS (A-FILE DATA)'
                WRITE (FNUMTMP,501)
                CLOSE(FNUMTMP)
              ENDIF
              
              IF (EXCODE.NE.EXCODEP) THEN
                OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,POSITION='APPEND')
                WRITE (FNUMTMP,996) OUTHED(11:77)
  996           FORMAT (/,'*ERRORS(A):',A67,/)
                WRITE (FNUMTMP,896)
  896           FORMAT ('@  RUN',
     x          ' EXCODE    ',
     B          '   TRNO RN',
     C          '    CR',
     D          '    EDAP   EDAPE',
     E          '    ADAP   ADAPE',
     F          '    MDAP   MDAPE',
     G          '    HWAH   HWAHE',
     H          '    HWUM   HWUME',
     I          '    H#AM   H#AME',
     J          '    H#GM   H#GME',
     K          '    LAIX   LAIXE',
     L          '    L#SM   L#SME',
     M          '    S#AM   S#AME',
     N          '    CWAM   CWAME',
     O          '    VWAM   VWAME',
     P          '    HIAM   HIAME',
     Q          '    HN%M   HN%ME',
     R          '    CNAM   CNAME',
     S          '    HNAM   HNAME')
                CLOSE(FNUMTMP)
              ENDIF
              
              OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,POSITION = 'APPEND')
              
              IF (edatm.GT.0) THEN
               emdaterr = Dapcalc(stgdoy(9),plyear,plday)-
     X         Dapcalc(edatm,plyear,plday)
              ELSE
               emdaterr = -99
              Endif
              IF (adatm.GT.0) THEN
               adaterr = Dapcalc(adat,plyear,plday)-
     X         Dapcalc(adatm,plyear,plday)
              ELSE
               adaterr = -99
              Endif
              IF (mdatm.GT.0) THEN
               mdaterr = Dapcalc(stgdoy(5),plyear,plday)-
     X         Dapcalc(mdatm,plyear,plday)
              ELSE
               mdaterr = -99
              Endif
              IF (hwahm.GT.0 .AND. gwam.GT.0 .AND. hpc.GT.0) THEN
               hwaherr = 100.*(gwam*hpc/100.-hwahm)/(gwam*hpc/100.)
               IF (hwaherr.GT.99999.0) hwaherr = 99999.0
               IF (hwaherr.LT.-9999.0) hwaherr = -9999.0
              ELSE
               hwaherr = -99
              ENDIF
              IF (gwumm.GT.0 .AND. gwum.GT.0) THEN
               gwumerr = 100.0*(gwum-gwumm)/gwum
              ELSE
               gwumerr = -99
              ENDIF
              IF (hnumamm.GT.0 .AND. hnumam.GT.0) THEN
               hnumaerr = 100.0*(hnumam-hnumamm)/(hnumam)
              ELSE
               hnumaerr = -99
              ENDIF
              IF (hnumgmm.GT.0 .AND. hnumgm.GT.0) THEN
               hnumgerr = 100.0*((hnumgm-hnumgmm)/hnumgm)
              ELSE
               hnumgerr = -99
              ENDIF
              IF (laixm.GT.0 .AND. laix.GT.0) THEN
               laixerr = 100.0*((laix-laixm)/laix)
              ELSE
               laixerr = -99
              ENDIF
              IF (lnumsmm.GT.0 .AND. lnumsm.GT.0) THEN
               lnumserr = 100.0*((lnumsm-lnumsmm)/lnumsm)
              ELSE
               lnumserr = -99
              ENDIF
              IF (tnumamm.GT.0 .AND. tnumam.GT.0) THEN
               tnumaerr = 100.0*((tnumam-tnumamm)/tnumam)
              ELSE
               tnumaerr = -99
              Endif
              IF (cwamm.GT.0 .AND. cwam.GT.0) THEN
               cwamerr = 100.0*(cwam-cwamm)/cwam
              ELSE
               cwamerr = -99
              Endif
              IF (vwamm.GT.0 .AND. vwam.GT.0) THEN
               vwamerr = 100.0*(vwam-vwamm)/vwam
              ELSE
               vwamerr = -99
              Endif
              IF (hiamm.GT.0 .AND. hiam.GT.0) THEN
               hiamerr = 100.0*(hiam-hiamm)/hiam
              ELSE
               hiamerr = -99
              Endif
              IF (gnpcmm.GT.0 .AND. gnpcm.GT.0) THEN
               gnpcmerr = 100.0*(gnpcm-gnpcmm)/gnpcm
              ELSE
               gnpcmerr = -99
              Endif
              IF (cnamm.GT.0 .AND. cnam.GT.0) THEN
               cnamerr = 100.0*(cnam-cnamm)/cnam
              ELSE
               cnamerr = -99
              Endif
              IF (gnamm.GT.0 .AND. gnam.GT.0) THEN
               gnamerr = 100.0*(gnam-gnamm)/gnam
              ELSE
               gnamerr = -99
              Endif
              
              WRITE (FNUMTMP,8401) RUN,EXCODE,TN,RN,CR,
     x         edap,emdaterr,
     x         adap,adaterr,
     x         mdap,mdaterr,
     x         NINT(gwam),NINT(hwaherr),
     x         gwumc,NINT(gwumerr),
     x         NINT(hnumam),NINT(hnumaerr),
     x         hnumgm,NINT(hnumgerr),
     x         laix,NINT(laixerr),
     x         lnumsm,NINT(lnumserr),
     x         NINT(tnumam),NINT(tnumaerr),
     x         NINT(cwam),NINT(cwamerr),
     x         NINT(vwam),NINT(vwamerr),
     x         hiamc,NINT(hiamerr),
     x         gnpcm,NINT(gnpcmerr),
     x         NINT(cnam),NINT(cnamerr),
     x         NINT(gnam),NINT(gnamerr)

 8401         FORMAT (I6,1X,A10,1X,I6,I3,4X,A2,
     A         I8,  I8,
     B         I8,  I8,
     C         I8,  I8,
     D         I8,  I8,
     E      2X,A6,  I8,
     F         I8,  I8,
     G         F8.1,I8,
     H         F8.1,I8,
     I         F8.1,I8,
     J         I8  ,I8,
     K         I8,  I8,
     L         I8,  I8,
     M      2X,A6,  I8,
     N         F8.1,I8,
     O         I8,  I8,
     P         I8,  I8)
              
              CLOSE(FNUMTMP)       

            ELSE
               OPEN(UNIT=FNUMTMP,FILE='Plantera.out',STATUS = 'UNKNOWN')
               CLOSE(UNIT=FNUMTMP,STATUS='DELETE')
            ENDIF    ! END OF PLANT ERRORS (A)
           
!-----------------------------------------------------------------------
            
            ! PLANT ERRORS (TIME-COURSE)

            IF (IDETL.EQ.'A') THEN
            
              IF (CFLTFILE.NE.'Y' .OR. FROPADJ.GT.1) THEN
              
!                WRITE (fnumwrk,*) 'Cannot write PLANT ERRORS (T)'
!                IF (FROPADJ.GT.1)
!     &           WRITE (fnumwrk,*) 'Frequency of output > 1 day'  
                IF (RUN.EQ.1 .AND. RUNI.EQ.1) CFLHEAD = 'Y'
              
              ELSE
              
!                WRITE (fnumwrk,*) 'Writing PLANT ERRORS (T)'
              
                FNAMETMP = ' '
                FNAMETMP(1:12) = 'Plantert.'//out
                IF (RUN.EQ.1 .AND. RUNI.EQ.1 .OR. CFLHEAD.EQ.'Y') THEN
                 CFLHEAD = 'N'
                 OPEN (UNIT=FNUMTMP,FILE=FNAMETMP,STATUS='UNKNOWN')
                 WRITE (FNUMTMP,'(A21)') '$ERRORS (T-FILE DATA)'
                 WRITE (FNUMTMP,1501)
                 CLOSE(FNUMTMP)
                ENDIF
              
                INQUIRE (FILE = 'PlantGro.OUT',OPENED = fopen)
                IF (fopen) CLOSE (NOUTPG)
              
                STARNUM = 0
                OPEN (UNIT=FNUMT,FILE='Measured.out',STATUS='UNKNOWN')
                DO WHILE (TLINET(1:1).NE.'@')
                  TLINET = ' '
                  READ (FNUMT,1502,END=1600,ERR=1600) TLINET
 1502             FORMAT(A180)
                  IF (TLINET(1:1).EQ.'*') STARNUM = STARNUM + 1
                  IF (TLINET(1:1).EQ.'@') THEN
                    IF (STARNUM.NE.STARNUMM) THEN
                      TLINET = ' '
                      READ (FNUMT,1502,END=1600,ERR=1600) TLINET
                    ENDIF
                  ENDIF
                ENDDO
                tlinet(1:1) = ' '
                STARNUM = 0
              
                OPEN (UNIT=NOUTPG,FILE='PlantGro.OUT',STATUS='UNKNOWN')
              
                DO WHILE (TLINEGRO(1:1).NE.'@')
                  TLINEGRO = ' '
                  READ (NOUTPG,'(A180)') TLINEGRO
                  IF (TLINEGRO(1:4).EQ.'*RUN') STARNUM = STARNUM + 1
                  IF (TLINEGRO(1:1).EQ.'@') THEN
                    IF (STARNUM.NE.STARNUMO) THEN
                      TLINEGRO = ' '
                      READ (NOUTPG,'(A180)') TLINEGRO
                    ENDIF
                  ENDIF
                ENDDO
                tlinegro(1:1) = ' '
              
                ! Find headers from Measured file
                DO L = 1,20
                  CALL Getstr(tlinet,l,thead(l))
                  IF (THEAD(L)(1:3).EQ.'-99') EXIT
                  IF (THEAD(L)(1:3).EQ.'DAP') tfdapcol = l
                ENDDO
                TFCOLNUM = L-1
                IF (TFCOLNUM.LE.0) THEN
!                  WRITE (FNUMWRK,*) 'No columns found in T-file!'
                  GO TO 7777
                ENDIF
              
                ! Make new header line
                TLINETMP = ' '
                TLINETMP(1:1) = '@'
                DO L = 1, TFCOLNUM
                  TLPOS = (L-1)*6+1
                  IF (THEAD(L).EQ.'TRNO'.OR.THEAD(L).EQ.'YEAR'.OR.
     &              THEAD(L).EQ.'DATE') THEN
                    TLINETMP(TLPOS+2:TLPOS+5)=THEAD(L)(1:4)
                  ELSEIF(THEAD(L).EQ.'DOY'.OR.THEAD(L).EQ.'DAP' .OR.
     &              THEAD(L).EQ.'DAS'.OR.THEAD(L).EQ.'DAY') THEN
                    TLINETMP(TLPOS+3:TLPOS+5)=THEAD(L)(1:3)
                  ELSE
                    WRITE (TCHAR,'(I6)') NINT(ERRORVAL*100.0)
                    TLINETMP(TLPOS+1:TLPOS+4) = THEAD(L)(1:4)
                    TLINETMP(TLPOS+5:TLPOS+5) = 'E'
                  ENDIF
                ENDDO
              
                ! Find corresponding columns in PlantGro.OUT
                DO L = 1,TFCOLNUM
                  pgrocol(l) = Tvicolnm(tlinegro,thead(l))
                ENDDO
              
                OPEN (UNIT=FNUMTMP,FILE=FNAMETMP,POSITION='APPEND')
              
                 WRITE (FNUMTMP,2996) OUTHED(11:77)
 2996            FORMAT (/,'*ERRORS(T):',A67,/)
                 tlinet(1:1) = '@'
                 WRITE (FNUMTMP,'(A180)') TLINETMP
              
                ! Read data lines, match dates, calculate errors, write
                DO L1 = 1,200
                  TLINET = ' '
                  READ (FNUMT,7778,ERR=7777,END=7777) TLINET
 7778             FORMAT(A180)
                  IF (TLINET(1:1).EQ.'*') GO TO 7777
                  IF (TLINET(1:6).EQ.'      ') GO TO 7776
                  IF (TLINET(1:1).EQ.'!') GO TO 7776
                  CALL Getstri(tlinet,tfdapcol,tfdap) 
                  IF (TFDAP.LE.0) THEN
!                    WRITE (FNUMWRK,*) 'DAP in T-file <= 0!'
                    GO TO 7777
                  ENDIF
                  DO WHILE (tfdap.NE.pgdap)
                    TLINEGRO = ' '
                    READ (NOUTPG,7779,ERR=7777,END=7777) TLINEGRO
                    CALL Getstri(tlinegro,pgrocol(tfdapcol),pgdap)
                    IF (PGDAP.LT.0) THEN
!                      WRITE (FNUMWRK,*) 'DAP in Plantgro file < 0!'
                      GO TO 7777
                    ENDIF
                  ENDDO
 7779             FORMAT(A180)
                  TLINETMP = ' '
                  DO L = 1, TFCOLNUM
                    CALL Getstrr(tlinet,l,tfval)
                    CALL Getstrr(tlinegro,pgrocol(l),pgval)
                    ERRORVAL = 0.0
                    IF (TFVAL.GT.0.0 .AND. 
     &               PGVAL.NE.-99 .AND.PGVAL.NE.0.0) THEN
                      ERRORVAL = 100.0 * (PGVAL - TFVAL) / PGVAL
                    ELSE
                      ERRORVAL = -99.0
                    ENDIF
                    IF (THEAD(L).EQ.'TRNO'.OR.THEAD(L).EQ.'YEAR' .OR.
     &                THEAD(L).EQ.'DOY'.OR.THEAD(L).EQ.'DAP' .OR.
     &                THEAD(L).EQ.'DAY' .OR.
     &                THEAD(L).EQ.'DAS'.OR.THEAD(L).EQ.'DATE') THEN
                      CALL Getstri(tlinet,l,tvi1)
                      WRITE (TCHAR,'(I6)') TVI1
                    ELSE
                      WRITE (TCHAR,'(I6)') NINT(ERRORVAL)
                    ENDIF
                    TLPOS = (L-1)*6+1
                    TLINETMP(TLPOS:TLPOS+5)=TCHAR
                  ENDDO
                  WRITE (FNUMTMP,'(A180)') TLINETMP
 7776             CONTINUE
                ENDDO
              
 7777           CONTINUE
                GO TO 1601
              
 1600           CONTINUE
!                WRITE(fnumwrk,*)'End of file reading Measured.out'
!                WRITE(fnumwrk,*)'Starnum and starnumm were: ',         
!     &            starnum,starnumm
 1601           CONTINUE
              
                CLOSE (FNUMTMP)      
                CLOSE (FNUMT)
                CLOSE (NOUTPG)
                IF (FOPEN) THEN
                OPEN (UNIT=NOUTPG,FILE='PlantGro.OUT',POSITION='APPEND')
                ENDIF
              ENDIF

            ELSE
               OPEN(UNIT=FNUMTMP,FILE='Plantert.out',STATUS = 'UNKNOWN')
               CLOSE(UNIT=FNUMTMP,STATUS='DELETE')
            ENDIF      ! END OF PLANT ERRORS (TIME-COURSE)
            
!-----------------------------------------------------------------------
            
 8888       CONTINUE   ! Jump to here if cannot write outputs
            
!-----------------------------------------------------------------------
            
            EXCODEP = EXCODE
!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,*) 'END OF HARVEST DAY OUTPUTS'
!            WRITE (fnumwrk,*) 'WILL BEGIN NEW CYCLE (IF CALLED FOR)'
!            WRITE (fnumwrk,*) ' '
            
            SEASENDOUT = 'Y'
 
            ! END MAIN OUTPUTS

            ! Need to re-initialize here because of automatic
            ! fertilization routines in DSSAT
            NFG = 1.0
            NFP = 1.0
            NFT = 1.0
            WFG = 1.0
            WFP = 1.0
            WFT = 1.0

          
            IF (IDETO.EQ.'E'.OR.IDETO.EQ.'N') THEN
                      
            ! CSM SUMMARY ... not used in CROPSIM
            
            ! Store Summary labels and values in arrays to send to
            ! OPSUM routine for printing.  Integers are temporarily
            ! saved as real numbers for placement in real array.
            ! (Nnot done earlier because no Overview called for)
              !Resource productivity calculations
   
              DMP_Rain = -99.
              GrP_Rain = -99.
              DMP_ET = -99.
              GrP_ET = -99.
              DMP_EP = -99.
              GrP_EP = -99.
              DMP_Irr = -99.    
              GrP_Irr = -99.
 
              DMP_NApp = -99.
              GrP_NApp = -99.
              DMP_NUpt = -99.
              GrP_NUpt = -99.
 
              IF (RAINCP(0) > 1.E-3) THEN
                DMP_Rain = CWAM / RAINCp(0) 
                GrP_Rain = GWAM  / RAINCp(0)
              ENDIF
            
              IF (ETC(0) > 1.E-3) THEN
                DMP_ET = CWAM / ETC(0) 
                GrP_ET = GWAM  / ETC(0) 
              ENDIF
            
              IF (EPC(0) > 1.E-3) THEN
                DMP_EP = CWAM / EPC(0) 
                GrP_EP = GWAM  / EPC(0) 
              ENDIF
 
              IF (TOTIR > 1.E-3) THEN
                DMP_Irr = CWAM / TOTIR 
                GrP_Irr = GWAM  / TOTIR
              ENDIF
 
              IF (ISWNIT.NE.'N') THEN
                IF (Amtnit > 1.E-3) THEN
                  DMP_NApp = CWAM / Amtnit
                  GrP_NApp = GWAM  / Amtnit
                ENDIF
            
                IF (NUAD > 1.E-3) THEN
                  DMP_NUpt = CWAM / NUAD
                  GrP_NUpt = GWAM  / NUAD
                ENDIF
              ENDIF !ISWNIT == 'Y'
            
              LABEL(1) = 'ADAT '; VALUE(1) = FLOAT(adat)
              LABEL(2) = 'MDAT '; VALUE(2) = FLOAT(stgdoy(5))
              LABEL(3) = 'DWAP '; VALUE(3) = sdrate
              LABEL(4) = 'CWAM '; VALUE(4) = cwam
              LABEL(5) = 'HWAM '; VALUE(5) = gwam
              LABEL(6) = 'HWAH '; VALUE(6) = gwam * hpc / 100.
              LABEL(7) = 'BWAH '; VALUE(7) = vwam * hbpc / 100. 
            
              LABEL(8)  = 'HWUM '; VALUE(8) = gwum
              LABEL(9)  = 'H#AM '; VALUE(9) = hnumam
              LABEL(10) = 'H#UM '; VALUE(10) = hnumgm
              LABEL(11) = 'NUCM '; VALUE(11) = nuad
              LABEL(12) = 'CNAM '; VALUE(12) = cnam
              LABEL(13) = 'GNAM '; VALUE(13) = gnam
              LABEL(14) = 'PWAM '; VALUE(14) = PWAM    
              LABEL(15) = 'LAIX '; VALUE(15) = LAIX    
              LABEL(16) = 'HIAM '; VALUE(16) = HIAM    
 
              LABEL(17) = 'DMPPM'; VALUE(17) = DMP_Rain 
              LABEL(18) = 'DMPEM'; VALUE(18) = DMP_ET                   
              LABEL(19) = 'DMPTM'; VALUE(19) = DMP_EP                  
              LABEL(20) = 'DMPIM'; VALUE(20) = DMP_Irr
              LABEL(21) = 'DPNAM'; VALUE(21) = DMP_NApp
              LABEL(22) = 'DPNUM'; VALUE(22) = DMP_NUpt
            
              LABEL(23) = 'YPPM ' ; VALUE(23) = GrP_Rain                
              LABEL(24) = 'YPEM ' ; VALUE(24) = GrP_ET                 
              LABEL(25) = 'YPTM ' ; VALUE(25) = GrP_EP                  
              LABEL(26) = 'YPIM ' ; VALUE(26) = GrP_Irr
              LABEL(27) = 'YPNAM' ; VALUE(27) = GrP_NApp
              LABEL(28) = 'YPNUM' ; VALUE(28) = GrP_NUpt
 
              LABEL(29) = 'EDAP ' ; VALUE(29) = FLOAT(EDAP)     
              
              LABEL(30) = 'NDCH ' ; VALUE(30) = FLOAT(DAYSC(0)) 
              LABEL(31) = 'TMINA' ; VALUE(31) = TMINAV(0)       
              LABEL(32) = 'TMAXA' ; VALUE(32) = TMAXAV(0)       
              LABEL(33) = 'SRADA' ; VALUE(33) = SRADAV(0)       
              LABEL(34) = 'DAYLA' ; VALUE(34) = DAYLAV(0)       
              LABEL(35) = 'CO2A ' ; VALUE(35) = CO2AV(0)        
              LABEL(36) = 'PRCP ' ; VALUE(36) = RAINCP(0)       
              LABEL(37) = 'ETCP ' ; VALUE(37) = ETC(0)      

              IF (FILEIOT(1:2).EQ.'DS') CALL SUMVALS(SUMNUM,LABEL,VALUE)
            ENDIF

            ! To prevent massive Work.out files
!            IF (FILEIOT.EQ.'DS4') CLOSE(FNUMWRK)

          ENDIF

        ENDIF        ! YEARDOY >= YEARPLT


  206          FORMAT(
     &          /,"*MAIN GROWTH AND DEVELOPMENT VARIABLES",//,   
     &             "@",5X,"VARIABLE",T44,"SIMULATED     MEASURED",/,  
     &                 6X,"--------",T44,"---------     --------")  
  270 FORMAT(/,'------------------------------------------------------',
     &'--------------------------------------------------------')
  300       FORMAT(/,10X,A," YIELD : ",I8," kg/ha    [Dry weight] ",/)

  501  FORMAT(/,
     & '! This file presents the differences between simulated and',/,
     & '! single-time measured values (eg.date of anthesis,yield) for'/,
     & '! individual runs. The abbreviations are based on those',/,
     & '! listed in the DATA.CDE file, with the simple abbreviation',/,
     & '! indicating the simulated value,the basic abbreviation plus',/,
     & '! a final E the error. The units for the latter are days for',/,
     & '! time differences (EDAP,ADAP,MDAP) and percentages of',/,
     & '! simulated values for the remainder.,'/,
     & ' ',/,
     & '! As usual, a -99 indicates that no data were available.')
     
  502  FORMAT(/,
     & '! This file summarizes the error differences reported in',/,
     & '! the files PLANTERA.OUT and PLANTERT.OUT. The errors in',/,
     & '! these files have been averaged taking actual values first,'/,
     & '! then the absolute value of the original errors. The number',/,
     & '! of runs used is indicated in the RUNS column.',/,
     & ' ',/,
     & '! The abbreviations are based on those listed in the',/,
     & '! DATA.CDE file, with the abbreviation plus a final #',/,
     & '! indicating the number of values actually used for',/,
     & '! averaging, the basic abbreviation plus a final E the',/,
     & '! averaged error. The units for the latter are days for',/,
     & '! time differences (EDAP,ADAP,MDAP) and percentages of',/,
     & '! simulated values for the remainder.',/,
     & ' ',/,
     & '! The major heading (the * line) includes codes for model',/,
     & '! and plant module. The batch column, as generated, is',/,
     & '! filled with a 1, and the batchcode with the code for',/,
     & '! the last experiment in the PLANTERA/T.OUT files. The',/,
     & '! entries in these columns can (and should) be changed if',/,
     & '! an overall summary file is constructed by combining files',/,
     & '! from different model and module runs.',/,
     & ' ',/,
     & '! As usual, a -99 indicates that no data were available.')

 1501  FORMAT(/,
     & '! This file summarizes the differences between simulated',/,
     & '! and measured values for individual runs. Abbreviations',/,
     & '! are based on those listed in the DATA.CDE file, but with',/,
     & '! an E added to indicate error. The units for the errors',/,
     & '! are % of simulated values (ie.100*[SIM-MEAS]/SIM).'/,
     & ' ',/,
     & '! A -99 indicates that no data were available. Here, this'/,
     & '! could be simulated as well as measured data.')

 1200       FORMAT(
     &      '------------------------------------------------------',
     &      '--------------------------------------------------------',
     &///,'*RESOURCE PRODUCTIVITY',
     &//,' Growing season length:', I4,' days ',
     &//,' Precipitation during growth season',T42,F7.1,' mm[rain]',
     & /,'   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[rain]',
     &                           T75,'=',F7.1,' kg[DM]/ha per mm[rain]',
     & /,'   Yield Productivity',T42,F7.2,' kg[grain yield]/m3[rain]',
     &                       T75,'=',F7.1,' kg[yield]/ha per mm[rain]',
     &//,' Evapotranspiration during growth season',T42,F7.1,' mm[ET]',
     & /,'   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[ET]',
     &                            T75,'=',F7.1,' kg[DM]/ha per mm[ET]',
     & /,'   Yield Productivity',T42,F7.2,' kg[grain yield]/m3[ET]',
     &                       T75,'=',F7.1,' kg[yield]/ha per mm[ET]',
     &//,' Transpiration during growth season',T42,F7.1,' mm[EP]',
     & /,'   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[EP]',
     &                            T75,'=',F7.1,' kg[DM]/ha per mm[EP]',
     & /,'   Yield Productivity',T42,F7.2,' kg[grain yield]/m3[EP]',
     &                       T75,'=',F7.1,' kg[yield]/ha per mm[EP]')
 1210 FORMAT(
     & /,' Irrigation during growing season',T42,F7.1,' mm[irrig]',
     & /,'   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[irrig]',
     &                       T75,'=',F7.1,' kg[DM]/ha per mm[irrig]',
     & /,'   Yield Productivity',T42,F7.2,' kg[grain yield]/m3[irrig]',
     &                       T75,'=',F7.1,' kg[yield]/ha per mm[irrig]')
 1220 FORMAT(
     & /,' N applied during growing season',T42,F7.1,' kg[N applied]/ha'
     & /,'   Dry Matter Productivity',T42,F7.1,' kg[DM]/kg[N applied]',
     & /,'   Yield Productivity',T42,F7.1,' kg[yield]/kg[N applied]')
 1230 FORMAT(
     & /,' N uptake during growing season',T42,F7.1,' kg[N uptake]/ha'
     & /,'   Dry Matter Productivity',T42,F7.1,' kg[DM]/kg[N uptake]',
     & /,'   Yield Productivity',T42,F7.1,' kg[yield]/kg[N uptake]')

 5500  FORMAT(/,'*ENVIRONMENTAL AND STRESS FACTORS',//,
     &' |-----Development Phase------|-------------Environment--------',
     &'------|----------------Stress-----------------|',/,
     &30X,'|--------Average-------|---Cumulative--|         (0=Min, 1=',
     &'Max Stress)         |',/,
     &25X,'Time  Temp  Temp Solar Photop         Evapo |----Water---|-',
     &'-Nitrogen--|--Phosphorus-|',/,
     &25X,'Span   Max   Min   Rad  [day]   Rain  Trans  Photo',9X,'Pho',
     &'to         Photo',/,
     &25X,'days    °C    °C MJ/m2     hr     mm     mm  synth Growth ',
     &' synth Growth  synth Growth',/,110('-'))

 9588       FORMAT(
     &      /,' ...... DATE ......  GROWTH STAGE BIOMASS   LEAF  
     &     CROP N      STRESS')     
 9589         FORMAT(//,
     &     '*SIMULATED CROP AND SOIL STATUS AT MAIN DEVELOPMENT STAGES')
 9600       FORMAT(' YEARDOY DOM MON DAP ............. kg/ha AREA NUMBER
     &  kg/ha   %   H2O    N')

      

      END SUBROUTINE CER_Output