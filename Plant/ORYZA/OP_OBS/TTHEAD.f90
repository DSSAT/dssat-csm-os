      SUBROUTINE TTHEAD (IUNIT,FILE,TITLE, &
                         XMIN,XMAX,XLAB,XTICK,XTXT, &
                         YMIN,YMAX,YLAB,YTICK,YTXT, &
                         XLEG,YLEG)
!     formal parameters
      IMPLICIT NONE
      INTEGER IUNIT
      CHARACTER (*) FILE,TITLE,XTXT,YTXT
      REAL          XMIN,XMAX,XLAB,XTICK
      REAL          YMIN,YMAX,YLAB,YTICK
      REAL          XLEG,YLEG

!     local parameters
      INTEGER IL
      SAVE

      CALL FOPENG (IUNIT,FILE,'NEW','FS',0,'DEL')
      IL = MAX (1, LEN_TRIM (TITLE))
      WRITE (IUNIT,'(A)') TITLE(1:IL)
      WRITE (IUNIT,'(A,4G12.5,A)') 'X 1 0',XMIN,XMAX,XLAB,XTICK,' 0 0'
      IL = MAX (1, LEN_TRIM (XTXT))
      WRITE (IUNIT,'(A)') XTXT(1:IL)
      WRITE (IUNIT,'(A,4G12.5,A)') 'Y 1 0',YMIN,YMAX,YLAB,YTICK,' 0 0'
      IL = MAX (1, LEN_TRIM (YTXT))
      WRITE (IUNIT,'(A)') YTXT(1:IL)
      WRITE (IUNIT,'(2G12.5)') XLEG,YLEG

      RETURN
      END
