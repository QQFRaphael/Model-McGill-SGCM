      program test
C*compile with f77 -r8 test.f -L /usr/people/hall/igcm/lib -lsgiblas1

      parameter (M=1,N=2,K=2)
      real C(M,N),A(M,K),B(K,N)

C*****illustrates the use of SGEMM
C     evaluates C(M,N)=A(M,K)*B(K,N)
C
C     remember, fortran runs down the first index first. 
C     so it does collumns before rows
C
C     the example sub below is therefore either:
C
C     (7,10) = (1,2) * (1,2)
C                      (3,4)
C
C     or
C
C     (7 ) = (1,3) * (1)
C     (10)   (2,4)   (2)
C
C     so you could think of this routine as using normal matrix 
C     notation and evaluating 
C
C     C = B' * A  where C and A are now collumn vectors and B has been 
C                 transposed (or more generally, where everything has 
C                 been trasnposed). This way may be easier for reconciling
C                 1 d vectors with matrices.

      A(1,1)=1.
      A(2,1)=2.

      B(1,1)=1.
      B(2,1)=3.
      B(1,2)=2.
      B(2,2)=4.

      C(1,1)=0.
      C(1,2)=0.

      PRINT*,'A = ',A
      PRINT*
      PRINT*,'B = ',B
      PRINT*

      CALL SGEMM('N','N',M,N,K,1.0,A,M,B,K,0.0,C,M)

      PRINT*,'C = ',C

      STOP
      END
