      INTEGER FUNCTION ISMIN(N,SX,INCX)
      REAL SX(*)
      II=1
      AMIN=SX(1)
      DO I=INCX+1,N
         IF (SX(I).LT.AMIN) THEN
            AMIN=SX(I)
            II=I
         ENDIF
      ENDDO
      ISMIN=II
      END

*DECK LENSIG
      INTEGER FUNCTION LENSIG(STRING)
C
C     Determine length of significant part of character string
C     by searching for first non-blank character from end.
C
C     Mike Blackburn     UGAMP     05.04.90.
C
      CHARACTER STRING*(*),BLANK*1
      BLANK=' '
      ICLEN=LEN(STRING)
      IL=ICLEN+1
      DO 10 I=ICLEN,1,-1
      IL=IL-1
      IF (STRING(I:I).NE.BLANK) GOTO 20
   10 CONTINUE
      IL=IL-1
   20 LENSIG=IL
      RETURN
      END

c	The following are versions of some cray library routines
c
      SUBROUTINE SECOND(STIME)
      STIME=0.0
      END

c  section ssum
c
c  sum a vector v
c
      real function ssum(n,v,iv)
      implicit real (a-h,o-z)
c
      integer iv,j,n
      real v(n)
c
      ssum=0.
      do 10 j=1,n
	 ssum=ssum+v((j-1)*iv+1)
   10 continue
c
      return
      end
