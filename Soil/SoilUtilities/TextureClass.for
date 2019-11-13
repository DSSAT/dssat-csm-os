!=======================================================================
!  TEXTURECLASS, Subroutine, A.J.Gijsman
!  Determins soil texture class
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  ...        AJG Written
!  04/13/2006 CHP Added COARSE variable and put into CSM
!=======================================================================
      SUBROUTINE TEXTURECLASS (
     &  CLAY, SAND, SILT,                                 !Input
     &  TEXTURE, COARSE)                                  !Output


      CHARACTER*12 TEXTURE
      LOGICAL COARSE
      LOGICAL FINE
      REAL CLAY, SAND, SILT

!-----------------------------------------------------------------------
!     Modified from the MUUF code (Baumer and Rice, 1988), which uses
!     very coarse/coarse/medium/fine/very fine sand. These have been
!     left out. The original MUUF code did not fully cover all texture
!     combinations, so a few minor changes have been made.
!                                             Previous
!     Soil texture     TEXTURE                  Code
!     Sand             'Sand        ' Coarse  '   S' 
!     Loamy Sand       'LoamySand   ' Coarse  '  LS'  
!     Sandy Loam       'SandyLoam   ' Coarse  '  SL' 
!     Loam             'Loam        ' Medium  '   L' 
!     Silty Loam       'SiltyLoam   ' Medium  ' SIL' 
!     Silt             'Silt        ' Medium  '  SI' 
!     Sandy Clay Loam  'SandClayLoam' Fine    ' SCL' 
!     Clay Loam        'ClayLoam    ' Fine    '  CL' 
!     Silty Clay Loam  'SiltClayLoam' Fine    'SICL' 
!     Sandy Clay       'SandyClay   ' Fine    '  SC' 
!     Silty Clay       'SiltyClay   ' Fine    ' SIC' 
!     Clay             'Clay        ' Fine    '   C' 

!-----------------------------------------------------------------------
!     According to the Glossary of Soil Science Terms of the Soil 
!     Science Society of America (1984), coarse texture is the 
!     texture exhibited by sands, loamy sands and sandy loams, 
!     except very fine sandy loam.
      COARSE = .FALSE.
      FINE   = .FALSE.

!     ******************** SAND ***********************
!     SAND: 85% OR MORE SAND, SILT + (1.5 * CLAY) <= 15
      IF (SAND .GE. 85. .AND. SILT + 1.5 * CLAY .LE. 15.) THEN
        TEXTURE = 'Sand        '
        COARSE = .TRUE.

!     ******************* LOAMY SAND ******************
      ELSEIF ((SAND .GE. 85. .AND. SAND .LT. 90. .AND.
!**  &  SILT + 1.5 * CLAY .GT. 15.) .OR.
     &  SILT + 1.5 * CLAY .GE. 15.) .OR.
     &  (SAND .GE. 70. .AND. SAND .LT. 85. .AND.
     &  SILT + 2.0 * CLAY .LE. 30.)) THEN
        TEXTURE = 'LoamySand   '
        COARSE = .TRUE.

!     ********************* SANDY LOAM ****************
!**   ELSEIF ((CLAY .LT. 20. .AND. SAND .GE. 52. .AND.
      ELSEIF ((CLAY .LE. 20. .AND. SAND .GE. 52. .AND.
     &  SILT + 2. * CLAY .GT. 30.) .OR.
     &  (CLAY .LT. 7. .AND. SILT .LT. 50. .AND. SAND .GT. 43. .AND.
     &  SAND .LT. 52.)) THEN
        TEXTURE = 'SandyLoam   '
        COARSE = .TRUE.

!     ************************ LOAM ********************
!**   ELSEIF (CLAY .GE. 7. .AND. CLAY .LT. 27. .AND.
      ELSEIF (CLAY .GE. 7. .AND. CLAY .LE. 27. .AND.
     &  SILT .GE. 28. .AND. SILT .LT. 50. .AND.
     &  SAND .GE. 23. .AND. SAND .LT. 52.) THEN
        TEXTURE = 'Loam        '

!     ********************** SILT LOAM *****************
!**   ELSEIF ((SILT .GE. 50. .AND. SILT .LT. 88. .AND.
      ELSEIF ((SILT .GE. 50. .AND. SILT .LE. 88. .AND.
!**  &  CLAY .GE. 12. .AND. CLAY .LT. 27.) .OR.
     &  CLAY .GE. 12. .AND. CLAY .LE. 27.) .OR.
     &  (SILT .GE. 50. .AND. SILT .LT. 80. .AND.
     &  CLAY .GE. 0. .AND. CLAY .LT. 12.)) THEN
        TEXTURE = 'SiltyLoam   '

!     ********************** SILT **********************
      ELSEIF (SILT .GE. 80. .AND. CLAY .LT. 12.) THEN
        TEXTURE = 'Silt        '

!    ****************** SANDY CLAY LOAM ***************
      ELSEIF (CLAY .GE. 20. .AND. CLAY .LT. 35. .AND.
     &  SILT .GE. 0.  .AND. SILT .LT. 28. .AND.
!    &  SAND .GE. 45. .AND. SAND .LT. 80.) THEN
     &  SAND .GE. 45.) THEN
        TEXTURE = 'SandClayLoam'
        FINE = .TRUE.

!     ********************** CLAY LOAM *****************
      ELSEIF (CLAY .GE. 27. .AND. CLAY .LT. 40. .AND.
     &    SAND .GE. 20. .AND. SAND .LT. 45.) THEN
        TEXTURE = 'ClayLoam    '
        FINE = .TRUE.

!     ******************* SILTY CLAY LOAM *************
      ELSEIF (CLAY .GE. 27. .AND. CLAY .LT. 40. .AND.
     &  SAND .GE. 0. .AND. SAND .LT. 20.) THEN
        TEXTURE = 'SiltClayLoam'
        FINE = .TRUE.

!     ********************** SANDY CLAY ***************
      ELSEIF (CLAY .GE. 35. .AND. SAND .GE. 45.) THEN
        TEXTURE = 'SandyClay   '
        FINE = .TRUE.

!     ********************** SILTY CLAY ***************
      ELSEIF (CLAY .GE. 40. .AND. SILT .GE. 40.) THEN
        TEXTURE = 'SiltyClay   '
        FINE = .TRUE.

!     ************************* CLAY ******************
      ELSEIF (CLAY .GE. 40. .AND. SAND .LT. 45. .AND.
     &  SILT .LT. 40.) THEN
        TEXTURE = 'Clay        '
        FINE = .TRUE.

!     ********* UNKNOWN SOIL TYPE (= ERROR) ***********
      ELSE
        TEXTURE = 'Unknown     '
        WRITE (6, 100)
100     FORMAT ('*** UNKNOWN SOIL TYPE ***',
     &    '*** PLEASE CHECK YOUR INPUT DATA ***')
          STOP

      ENDIF

      RETURN
      END SUBROUTINE TEXTURECLASS
!=======================================================================
