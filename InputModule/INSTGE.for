C=======================================================================
C  INSTGE, Subroutine
C
C  Initializes phenological stages
C-----------------------------------------------------------------------
C  Revision history
C
C  03/28/1993  GH  Written 
C  05/28/1993  PWW Header revision and minor changes     
C  09/23/2002  GH  Added Faba Bean
C  09/05/2020  JVJ Stages inclusion for Overview.    
C-----------------------------------------------------------------------
C  INPUT  : CROP
C
C  LOCAL  : I
C
C  OUTPUT : STNAME
C-----------------------------------------------------------------------
C  Called : SEHARV
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE INSTGE (STNAME,CROP)

      IMPLICIT     NONE

      CHARACTER*2  CROP
      CHARACTER*10 STNAME(20)

      INTEGER      I

C-----------------------------------------------------------------------
C     Define names of reproductive phases
C-----------------------------------------------------------------------

      DO I = 1, 20
         STNAME(I) = '          '
      END DO

      SELECT CASE (CROP)
!     IF (CROP .EQ. 'BA') THEN
      CASE('BA')
         STNAME( 1) = 'Max Prim  '
         STNAME( 2) = 'End Veg   '
         STNAME( 3) = 'End Ear Gr'
         STNAME( 4) = 'Ear Emerg '
         STNAME( 5) = 'End Gr Fil'
         STNAME( 6) = 'Maturity  '
         STNAME( 7) = 'Sowing    '
         STNAME( 8) = 'Germinate '
         STNAME( 9) = 'Emergence '

!      ELSE IF (CROP .EQ. 'BN') THEN
      CASE ('BN','CH','CP','CT','FB','LT','PE',
     &      'PN','PP','PR','SB','TM','VB')
         STNAME( 1) = 'Emergence '
         STNAME( 2) = 'Unifoliate'
         STNAME( 3) = 'End Juven.'
         STNAME( 4) = 'Flower Ind'
         STNAME( 5) = 'Flowering '
         STNAME( 6) = 'First Pod '
         STNAME( 7) = 'Full Pod  '
         STNAME( 8) = 'First Seed'
         STNAME( 9) = 'End Pod   '
         STNAME(10) = 'Phys. Mat '
         STNAME(11) = 'Harv. Mat '
         STNAME(12) = 'End Msnode'
         STNAME(13) = 'End Leaf  '
         STNAME(14) = 'Start Sim '
         STNAME(15) = 'Sowing    '
         STNAME(16) = 'Harvest   '
                  
!      ELSE IF (INDEX ('C3C4BHG1G2G3G4G5G6G7G8BR',CROP) .GT. 0) THEN
      CASE ('C3','C4','BH','G1','G2','G3','G4','G5','G6','G7','G8','BR')
         STNAME( 1) = 'Emergence '
         STNAME( 2) = 'First Leaf'
         STNAME( 3) = 'End Juven.'
         STNAME( 4) = 'Flower Ind'
         STNAME( 5) = 'Flowering '
         STNAME(10) = 'Phys. Mat '
         STNAME(11) = 'Harv. Mat '
         STNAME(12) = 'End Msnode'
         STNAME(13) = 'End Leaf  '
         STNAME(14) = 'Start Sim '
         STNAME(15) = 'Sowing    '
         STNAME(16) = 'Harvest   '
     
!      ELSE IF (CROP .EQ. 'CB') THEN
      CASE ('CB')
         STNAME( 1) = 'Emergence '
         STNAME( 2) = 'Unifoliate'
         STNAME( 3) = 'End Juven.'
         STNAME( 4) = '          '
         STNAME( 5) = '          '
         STNAME( 6) = 'First Head'
         STNAME( 7) = 'Full Head '
         STNAME( 8) = '          '
         STNAME( 9) = '          '
         STNAME(10) = 'Phys. Mat '
         STNAME(11) = 'Harv. Mat '
         STNAME(12) = 'End Msnode'
         STNAME(13) = 'End Leaf  '
         STNAME(14) = 'Start Sim '
         STNAME(15) = 'Sowing    '
         STNAME(16) = 'Harvest   '

!      ELSE IF (CROP .EQ. 'CO') THEN
      CASE ('CO')
         STNAME( 1) = 'Emergence '
         STNAME( 2) = 'First Leaf'
         STNAME( 3) = 'End Juven.'
         STNAME( 4) = 'Flower Ind'
         STNAME( 5) = 'Flowering '
         STNAME( 6) = 'Boll > 6mm'
         STNAME( 7) = 'End Flower'
         STNAME( 8) = 'First Seed'
         STNAME( 9) = 'Bolls>.5sz'
         STNAME(10) = 'Cracked Bl'
         STNAME(11) = '90%Open Bl'
         STNAME(12) = 'End Msnode'
         STNAME(13) = 'End Leaf  '
         STNAME(14) = 'Start Sim '
         STNAME(15) = 'Sowing    '
         STNAME(16) = 'Harvest   '
                
!      ELSE IF (CROP .EQ. 'FA') THEN
      CASE ('FA')
         STNAME(14) = 'Start Sim '
         STNAME(15) = 'End Sim   '

!      ELSE IF (CROP .EQ. 'ML') THEN
      CASE ('ML','SG')
         STNAME( 1) = 'End Juveni'  
         STNAME( 2) = 'Floral Ini'
         STNAME( 3) = 'End Lf Gro'
         STNAME( 4) = 'Anthesis  '
         STNAME( 5) = 'End Tlr Fl'
         STNAME( 6) = 'Maturity  '
         STNAME( 7) = 'Sowing    '
         STNAME( 8) = 'Germinate '
         STNAME( 9) = 'Emergence '

!      ELSE IF (CROP .EQ. 'MZ') THEN
      CASE ('MZ')
         STNAME( 1) = 'End Juveni'
         STNAME( 2) = 'Floral Ini'
         STNAME( 3) = '75% Silkin'
         STNAME( 4) = 'Beg Gr Fil'
         STNAME( 5) = 'End Gr Fil'
         STNAME( 6) = 'Maturity  '
         STNAME( 7) = 'Sowing    '
         STNAME( 8) = 'Germinate '
         STNAME( 9) = 'Emergence '

!      ELSE IF (CROP .EQ. 'PI') THEN
      CASE ('PI')
         STNAME( 1) = 'Foliar C1 '  !JVJ   Zero Stem is not a phenological stage, and it cannot be seen with the naked eye. The phyllotaxis of pineapple is 5/13
         STNAME( 2) = 'Foliar C2 '         !(leaves are produced around the axis of the stem, when 5 turns are completed, leaf number 13 is aligned with the first leaf.
         STNAME( 3) = 'Foliar C3 '         !In other words, a pineapple leaf cycle is fulfilled each time it is produce 13 leaves. 
         STNAME( 4) = 'Forcing   '         !It is a date entered by the user.
         STNAME( 5) = 'Open Heart'         !Calving is an important event for producers. But you can have different criteria to define the precise moment. I am writing an article where this will be well defined.
         STNAME( 6) = 'EarlyAnthe'         !Idem
         STNAME( 7) = 'LastAnthes'         !Idem
         STNAME( 8) = 'PhMaturity'         !For MD-2 producers, physiological maturity is important because it defines the timing of degreening the fruit
         STNAME( 9) = 'Fruit Harv'         !The concept of previous physiological maturity of the Aloha Pineapple model is very different, so the values have been modified.
         STNAME(10) = 'Planting  '
         STNAME(11) = 'WhRoottips'
         STNAME(12) = 'Leaf Emerg'
         STNAME(13) = '          '
         STNAME(14) = 'Start Sim '
         STNAME(15) = 'End Sim   '
         STNAME(16) = '          '
         STNAME(17) = '          '
         STNAME(18) = '          '
         STNAME(19) = '          '
         STNAME(20) = 'Harvest   '

!       ELSE IF (CROP .EQ. 'SC') THEN
      CASE ('SC')
         STNAME( 1) = '14th Leaf '
         STNAME( 2) = 'Stalk Beg.'
         STNAME( 3) = 'Full Canop'
         STNAME( 4) = 'Peak Pop  '
         STNAME( 5) = 'Stable Pop'
         STNAME( 6) = 'Maturity  '
         STNAME( 7) = 'Sowing    '
         STNAME( 8) = 'Germinate '
         STNAME( 9) = 'Emergence '

!       ELSE IF (CROP .EQ. 'WH') THEN
      CASE ('WH')
         STNAME( 1) = 'Term Spklt'
         STNAME( 2) = 'End Veg   '
         STNAME( 3) = 'End Ear Gr'
         STNAME( 4) = 'Ear Emerg '
         STNAME( 5) = 'End Gr Fil'
         STNAME( 6) = 'Maturity  '
         STNAME( 7) = 'Sowing    '
         STNAME( 8) = 'Germinate '
         STNAME( 9) = 'Emergence '

!      ELSEIF (CROP .EQ. 'CS') THEN
      CASE ('CS')
         STNAME( 1) = '1st Branch'
         STNAME( 2) = '2nd Branch'
         STNAME( 3) = '3rd Branch'
         STNAME( 4) = '4th Branch'
         STNAME( 5) = '5th Branch'
         STNAME( 6) = '6th Branch'
         STNAME( 7) = '7th Branch'
         STNAME( 8) = '8th Branch'
         STNAME( 9) = 'Maturity  '
         STNAME(10) = 'Sowing    '
         STNAME(11) = 'Germinate '
         STNAME(12) = 'Emergence '
         STNAME(13) = '          '
         STNAME(14) = 'Start Sim '

!       ELSEIF (CROP .EQ. 'RI') THEN
      CASE ('RI')
         STNAME( 1) = 'End Juveni'
         STNAME( 2) = 'Pan Init  '
         STNAME( 3) = 'Heading   '
         STNAME( 4) = 'Beg Gr Fil'
         STNAME( 5) = 'End Mn Fil'
         STNAME( 6) = 'Maturity  '
         STNAME( 7) = 'Sowing    '
         STNAME( 8) = 'Germinate '
         STNAME( 9) = 'Emergence '
         STNAME(10) = 'Prgerm Sow'
         STNAME(11) = 'Transplant'
         STNAME(12) = 'End Ti Fil'
         STNAME(13) = 'Start Sim '
         STNAME(14) = 'Harvest   '

!       ELSE IF (CROP .EQ. 'SU') THEN
      CASE ('SU')
         STNAME( 1) = 'End Juveni'
         STNAME( 2) = 'Floral Ini'
         STNAME( 3) = 'First Anth'
         STNAME( 4) = 'Beg Gr Fil'
         STNAME( 5) = 'End Gr Fil'
         STNAME( 6) = 'Maturity  '
         STNAME( 7) = 'Sowing    '
         STNAME( 8) = 'Germinate '
         STNAME( 9) = 'Emergence '
      END SELECT

      RETURN
      END SUBROUTINE INSTGE
