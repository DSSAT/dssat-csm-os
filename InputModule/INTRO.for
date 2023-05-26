C========================================================================
C  INTRO, Subroutine
C
C  Prints model opening screen
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1993 GH  Written
C  09/14/1993 PWW Header modifications and header changes
C  12/31/1997 GH  Change date to 12/31/97, DSSAT v3.1
C  05/06/1998 GH  Change release version to DSSAT v3.5
C  11/16/1998 GH  Change release date for DSSAT v3.5 patch
C  05/31/1999 GH  Change release date for DSSAT v3.51 patch
C  09/20/2000 GH  Change working version to DSSAT v3.7
C  11/03/2001 GH  Add CASUPRO model
C  11/04/2001 GH  Check for model name instead of crop name
C  12/13/2001 GH  Created new intro for CSM model
C  01/30/2001 GH  Modified introduction
C  09/20/2000 GH  Change working version to CSM v3.9
C  03/31/2004 GH  Changed to release version CSM v4.0
C  08/31/2005 GH  Changed to release version CSM Version 4.0.2
C  02/21/2006 GH  Change to working version CSM v4.5
C  02/06/2007 CHP Change to working version CSM v4.6
C  08/09/2017 CHP Change to working version CSM v4.7
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  : None
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : INPUT
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  None   :
C=======================================================================

      SUBROUTINE INTRO

      USE ModuleDefs
      IMPLICIT  NONE
      EXTERNAL HOME

      CHARACTER*1  ANS,BOX_SID
      CHARACTER*56 ZCG(15)
      CHARACTER*58 BOX_TOP,BOX_BOT,BOX_DAT
      INTEGER      I
     

      DATA BOX_TOP/
     & 'ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ'/

      DATA BOX_SID/'ณ'/

      DATA BOX_BOT/
     & 'ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ'/

      DATA BOX_DAT/
     & 'ณ                                    01 May 2023       ณ'/

      DATA ZCG/
     & '                    DSSAT FOUNDATION                    ',
     & '                                                        ',
     & '    G.Hoogenboom, J.W.Jones, C.H.Porter, K.J.Boote,     ',
     & '          L.A.Hunt, J.W. White, J.I.Lizaso,             ',
     & '    P.W.Wilkens, U.Singh, F.S. Royce, and R. Ogoshi     ',
     & '                                                        ',
     & ' The Cropping System Model (CSM) is a comprehensive     ',
     & ' simulation model that incorporates the features of the ',
     & ' generic grain legume model CROPGRO, the generic grain  ',
     & ' cereal model CERES, and the root model SUBSTOR. CSM    ',
     & ' simulates growth, development, and yield and soil and  ',
     & ' plant water, nitrogen and carbon dynamics for multiple ',
     & ' crops in response to weather and soil conditions, crop ',
     & ' management and cultivar characteristics.               ',
     & '                                                        '/
           
      CALL HOME
      WRITE (*,250) BOX_TOP
      WRITE (*,100) BOX_SID,BOX_SID
      WRITE (*,250) BOX_BOT

      WRITE(*,250) BOX_TOP
      WRITE(*,252) BOX_SID,Version,BOX_SID

      DO I = 1, 15
        WRITE (*,255) BOX_SID,ZCG(I),BOX_SID
       
      END DO
      WRITE (*,250) BOX_DAT
      WRITE (*,250) BOX_BOT
      WRITE (*,250) BOX_TOP
      WRITE (*,150) BOX_SID,BOX_SID
      WRITE (*,250) BOX_BOT
      READ  (5,'(A1)') ANS

      RETURN

  100 FORMAT (10X,A1,13X,'DSSAT Version 4.7.0 Generic Input',10X,A1)
  150 FORMAT (10X,A1,4X,
     &        'Please press < ENTER > key (ฤฤู) to continue',7X,A1)
  252 FORMAT (10X,A1,"  DSSAT Cropping System Model (CSM) Version ", 
     &     I1,".",I1,".",I1,".",I3.3,"   ",A1)

  250 FORMAT (10X,A58)
  255 FORMAT (10X,A1,A56,A1)
  410 FORMAT (10X,A2)
      END SUBROUTINE INTRO
