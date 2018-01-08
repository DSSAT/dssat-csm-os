      SUBROUTINE INDEXX(N,ARRIN,INDX)
      Integer, intent(in):: N   !added by TAOLI, 25Oct 2011
      Integer J, L, IR, INDXT, I, Q   !added by TAOLI, 25Oct 2011
      INTEGER, DIMENSION(N):: ARRIN,INDX  !modified by TAOLI, 25OCT 2011 from DIMENSION ARRIN(N), INDX(N)
      
      DO J=1,N
        INDX(J)=J
      END DO
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          INDXT=INDX(L)
          Q=ARRIN(INDXT)
        ELSE
          INDXT=INDX(IR)
          Q=ARRIN(INDXT)
          INDX(IR)=INDX(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            INDX(1)=INDXT
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(ARRIN(INDX(J)).LT.ARRIN(INDX(J+1)))J=J+1
          ENDIF
          IF(Q.LT.ARRIN(INDX(J)))THEN
            INDX(I)=INDX(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        INDX(I)=INDXT
      GO TO 10
      END
