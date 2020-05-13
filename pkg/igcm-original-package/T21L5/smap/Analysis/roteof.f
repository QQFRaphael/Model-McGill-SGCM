c   LLAE -- points of space
c   idf  -- points of time
c   ss   -- standard deviation matrix.
c   diag -- matrix for eigenvalues
c   data -- matrix of anomaly data
c   um   -- matrix for time series of RPC
c   subd -- percentage after rotation
c   hhh, subf, xx, yy -- a matrix
c   resu -- matrix for eigenvectors
c   KEY2 -- number of vectors for rotation.
c   irot -- if irot=1, do rotation.
c  inputs -- data, irot, llae, idf
c

      parameter (LLAE=36*14, idf=51*3, irot=1,nm=idf)
      real work4(LLAE),work5(LLAE),work2(LLAE)
      real resu(LLAE,LLAE),ss(LLAE),fact(LLAE)
      real um(idf,LLAE),hhh(LLAE)
      real subd(LLAE),sube(LLAE),subf(LLAE)
      real xx(idf),yy(idf),data(LLAE,idf),diag(LLAE)
      integer indx(LLAE)
      real test(LLAE,4)


      Real t(72,37),tm(72,15),tz(73,37)

         open(15,file='Z500.dat',
     &   form='unformatted',status='old')
	open(22,file='roteof.z500',
     &   form='unformatted')
	open(23,file='roteof.perc')
	open(25,file='roteof.coef')
	open(30,file='roteof', form='unformatted')



        do 16 i=1,72
        do 16 j=1,15
16      tm(i,j)=0.
        do 61 k=1,nm
        read(15)tz
         do 12 i=1,72
         do 12 j=1,15
12       tm(i,j)=tm(i,j)+tz(i,j)/float(nm)
61      continue
        rewind (15)

        do 62 k=1,nm
        read(15)tz
          ii=0
CC 20-80N
        do 1 j=2,15
         phi=90.-(j-1)*5.
         phi=phi*3.1415926/180.
        do 1 i=1,36
          ii=ii+1
          i5=2*(i-1)+1
         data(ii,k)=(tz(i5,j)-tm(i5,j))
	 fact(ii)=sqrt(cos(phi))
1       continue
62      continue

CC
	do i=1,LLAE
	 s=0.
	 do k=1,nm
	  s=s+data(i,k)/float(nm)
	 end do

	 ss(i)=0.
	 do k=1,nm
	 ss(i)=ss(i)+(data(i,k)-s)**2
	 end do
	 ss(i)=sqrt(ss(i)/float(nm-1))	
	do k=1,nm
	data(i,k)=data(i,k)/ss(i)*fact(i)
	end do
	ss(i)=ss(i)*fact(i)
	end do


        call vari_rf(data,LLAE,LLAE,resu,um,xx,yy,ss,diag,subd,
     &                  hhh,sube,subf,KEY2,idf,iin,irot)
c
c   Save the eigenvectors corresponding to KEY2 largest eigenvalues
c
        KEY3=6
c       do 511 j=1,KEY3
c         do 502 ii=1,LLAE
c           work2(ii)=resu(ii,j)
c502      continue
c    write(22)work2
c511    continue

       do 511 j=1,4
          do 502 ii=1,LLAE
            test(ii,j)=resu(ii,j)
 502      continue
 511    continue
	  write(30)test




        write(23,*)   'percentage after rotation'
        write(23,*) (subd(j), j=1,KEY2)

        if(irot.eq.1) call rfa2(data,LLAE,LLAE,resu,um,
     &                   subf,indx,KEY2,idf)
 520    continue
	do kk=1,idf
        write(25,*) um(kk,1)
	enddo
CC recalculate percentage explained
	tot=0.
	do ii=1,LLAE
	tot=tot+ss(ii)*ss(ii)
	end do

	
	do j=1,6
	 r=0.
	 do ii=1,LLAE
	 do i=1,idf
	 r=r+(um(i,j)*resu(ii,j))**2/float(idf)
	 end do
	 end do
	print*,r/float(LLAE)
	end do
	

	stop
	end




cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        subroutine vari_rf(data,LLAF,LLAE,resu,um,xx,yy,ss,diag,
     &                  subd,hhh,sube,subf,KEY2,idf,it2,irot)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      real resu(LLAF,LLAF),data(LLAF,idf),um(idf,LLAF),hhh(LLAF)
      real diag(LLAF),subd(LLAF),sube(LLAF),subf(LLAF)
      real xx(idf),yy(idf),ss(LLAF)
c
c     Calculate the correlation matrix
c
        ridf1=float(idf-1)
	trace=float(LLAE)

	if(LLAE.lt.idf) goto 230
	do 224 ii=1,idf-1
	sxy=0.0
	do 210 kk=1,LLAE
        sxy=sxy+data(kk,ii)*data(kk,ii)
 210	continue
	resu(ii,ii)=sxy/ridf1
	do 224 jj=ii+1,idf
	sxy=0.0
	do 220 kk=1,LLAE
	sxy=sxy+data(kk,ii)*data(kk,jj)
 220	continue
        resu(ii,jj)=sxy/ridf1
	resu(jj,ii)=resu(ii,jj)
 224    continue
	sxy=0.0
	do 226 kk=1,LLAE
	sxy=sxy+data(kk,idf)*data(kk,idf)
 226    continue
        resu(idf,idf)=sxy/ridf1
c
c  Reduce the full symmetric matrix, resu, to tridiagonal form
c
        call tred2(idf,idf,resu,diag,subd,resu,LLAF,LLAF)
c
c  Finds the eigenvalues and eigenvectors of correlation matrix
c
        call tql2(idf,idf,diag,subd,resu,ierr,LLAF,LLAF,0)
	goto 330
 230	continue
        do 324 ii=1,LLAE-1
        sxy=0.0
        do 310 kk=1,idf
        sxy=sxy+data(ii,kk)*data(ii,kk)
 310    continue
         resu(ii,ii)=sxy/ridf1
          do 324 jj=ii+1,LLAE
          sxy=0.0
            do 320 kk=1,idf
              sxy=sxy+data(ii,kk)*data(jj,kk)
 320        continue
            resu(ii,jj)=sxy/ridf1
            resu(jj,ii)=resu(ii,jj)
 324    continue
        sxy=0.0
        do 326 kk=1,idf
        sxy=sxy+data(LLAE,kk)*data(LLAE,kk)
 326    continue
        resu(LLAE,LLAE)=sxy/ridf1
c
c  Reduce the full symmetric matrix, resu, to tridiagonal form
c
        call tred2(LLAE,LLAE,resu,diag,subd,resu,LLAF,LLAF)
c
c  Finds the eigenvalues and eigenvectors of correlation matrix
c
        call tql2(LLAE,LLAE,diag,subd,resu,ierr,LLAF,LLAF,1)
 330	continue
c
c   Find the lowest eigenvalue of interest
c
        KEY2=0
 332    KEY2=KEY2+1
        if(diag(KEY2).gt.1.0) goto 332
 333    KEY2=KEY2-1
        write(6,*) 'KEY2=',KEY2
        if(LLAE.lt.idf) goto 340
c
c   Calculate the associated eigenvectors
c
        do 338 ii=1,KEY2
        std=0.0
        do 336 jj=1,LLAE
        um(ii,jj)=0.0
        do 335 kk=1,idf
        um(ii,jj)=um(ii,jj)+data(jj,kk)*resu(kk,ii)
 335    continue
        std=std+um(ii,jj)*um(ii,jj)
 336    continue
        std=sqrt(std)
        do 337 jj=1,LLAE
        um(ii,jj)=um(ii,jj)*sqrt(diag(ii))/std
 337    continue
 338    continue
	do 339 ii=1,KEY2
	do 339 jj=1,LLAE
	resu(jj,ii)=um(ii,jj)
 339	continue
 340	continue
c
c   Calculate the modified communality.
c
        trace1=0.0
        do 344 i=1,LLAE
          a2=0.0
          do 342 j=1,KEY2
            a2=a2+resu(i,j)*resu(i,j)
 342      continue
          if(a2.gt.1.0) a2=1.0
          hhh(i)=a2
          trace1=trace1+a2
 344    continue
        write(it2,*) 'Eigenvalues:'
        write(it2,345) (diag(j),j=1,KEY2)
        write(it2,*) 'Principal component analysis:'
        write(it2,*) 'trace = ',trace
        write(it2,*) 'KEY2=',KEY2
        write(it2,*)   'percentage'
        write(it2,345) (diag(j)/trace*100., j=1,KEY2)
 345    format(6f13.5)
c
c   Calculate the KEY2 common factors
c
        do 350 j=1,KEY2
        do 348 i=1,idf
          diah=0.0
         do 346 k=1,LLAE
          diah=diah+resu(k,j)*data(k,i)
 346     continue
        um(i,j)=diah/diag(j)
 348    continue
 350    continue

        if(irot.eq.1) then
        do 360 i=1,KEY2
        do 360 j=1,KEY2
        data(i,j)=resu(i,j)
 360    continue
c
c   Rotating the common factors
c

        call varmo(resu,LLAE,KEY2,hhh,subd,sube,subf,LLAF)
        write(6,*) 'Common factors rotated'
c
c  Calculate the variance explained by the KEY2 common factors
c
       tot=0.0
	do 504 i=1,LLAE
	tot=tot+ss(i)*ss(i)
 504	continue

       do 506 j=1,KEY2
         subd(j)=0.0
         do 505 i=1,LLAE
           subd(j)=subd(j)+(resu(i,j)*ss(i))**2
 505     continue
         subd(j)=subd(j)*100.0/tot
 506   continue
c
c  Rearrange the the eigenvalues and eigenvectors
c
        do 510 ii=2,KEY2
          i=ii-1
          k=i
          p=subd(i)
          do 507 j=ii,KEY2
             if(subd(j).lt.p) goto 507
             k=j
             p=subd(j)
 507      continue
          if(k.eq.i) goto 509
          subd(k)=subd(i)
          subd(i)=p
          do 508 j=1,LLAE
             p=resu(j,i)
             resu(j,i)=resu(j,k)
             resu(j,k)=p
 508      continue
 509    continue
 510    continue
	endif
        return
        end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE TQL2(NM,N,D,E,Z,IERR,NMK,NK,MO)
C***BEGIN PROLOGUE  TQL2
C***DATE WRITTEN   760101   (YYMMDD)
C***REVISION DATE  830518   (YYMMDD)
C***CATEGORY NO.  D4A5,D4C2A
C***KEYWORDS  EIGENVALUES,EIGENVECTORS,EISPACK
C***AUTHOR  SMITH, B. T., ET AL.
C***PURPOSE  Compute eigenvalues and eigenvectors of symmetric
C            tridiagonal matrix.
C***DESCRIPTION
C
C     This subroutine is a translation of the ALGOL procedure TQL2,
C     NUM. MATH. 11, 293-306(1968) by Bowdler, Martin, Reinsch, and
C     Wilkinson.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 227-240(1971).
C
C     This subroutine finds the eigenvalues and eigenvectors
C     of a SYMMETRIC TRIDIAGONAL matrix by the QL method.
C     The eigenvectors of a FULL SYMMETRIC matrix can also
C     be found if  TRED2  has been used to reduce this
C     full matrix to tridiagonal form.
C
C     On Input
C
C        NM must be set to the row dimension of two-dimensional
C          array parameters as declared in the calling program
C          dimension statement (R.Mo set NMK to be the row dimension).
C
C        N is the order of the matrix (R.Mo set NK to be the order).
C
C        D contains the diagonal elements of the input matrix.
C
C        E contains the subdiagonal elements of the input matrix
C          in its last N-1 positions.  E(1) is arbitrary.
C
C        Z contains the transformation matrix produced in the
C          reduction by  TRED2, if performed.  If the eigenvectors
C          of the tridiagonal matrix are desired, Z must contain
C          the identity matrix.
C
C      On Output
C
C        D contains the eigenvalues in ascending order (NOTE: R. MO
C        has modified the program to put the eigenvalues in descending
C        order). If an error exit is made, the eigenvalues are correct
C        but unordered for indices 1,2,...,IERR-1.
C
C        E has been destroyed.
C
C        Z contains orthonormal eigenvectors of the symmetric
C        tridiagonal (or full) matrix.  Note: R. Mo has modified the
c        model to weight the eignvectors by the square root of the
c        corresponding eigenvalue.   If an error exit is made,
C          Z contains the eigenvectors associated with the stored
C          eigenvalues.
C
C        IERR is set to
C          Zero       for normal return,
C          J          if the J-th eigenvalue has not been
C                     determined after 30 iterations.
C
C     Calls PYTHAG(A,B) for sqrt(A**2 + B**2).
C
C     Questions and comments should be directed to B. S. Garbow,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C     ------------------------------------------------------------------
C***REFERENCES  B. T. SMITH, J. M. BOYLE, J. J. DONGARRA, B. S. GARBOW,
C                 Y. IKEBE, V. C. KLEMA, C. B. MOLER, *MATRIX EIGEN-
C                 SYSTEM ROUTINES - EISPACK GUIDE*, SPRINGER-VERLAG,
C                 1976.
C***ROUTINES CALLED  PYTHAG
C***END PROLOGUE  TQL2
C
      INTEGER I,J,K,L,M,N,II,L1,L2,NM,MML,IERR,NMK,NK
      REAL D(NK),E(NK),Z(NMK,NK)
      REAL B,C,C2,C3,DL1,EL1,F,G,H,P,R,S,S2
      REAL PYTHAG
C
C***FIRST EXECUTABLE STATEMENT  TQL2
      IERR = 0
      IF (N .EQ. 1) GO TO 1001
C
      DO 100 I = 2, N
  100 E(I-1) = E(I)
C
      F = 0.0E0
      B = 0.0E0
      E(N) = 0.0E0
C
      DO 240 L = 1, N
         J = 0
         H = ABS(D(L)) + ABS(E(L))
         IF (B .LT. H) B = H
C     .......... LOOK FOR SMALL SUB-DIAGONAL ELEMENT ..........
         DO 110 M = L, N
            IF (B + ABS(E(M)) .EQ. B) GO TO 120
C     .......... E(N) IS ALWAYS ZERO, SO THERE IS NO EXIT
C                THROUGH THE BOTTOM OF THE LOOP ..........
  110    CONTINUE
C
  120    IF (M .EQ. L) GO TO 220
  130    IF (J .EQ. 30) GO TO 1000
         J = J + 1
C     .......... FORM SHIFT ..........
         L1 = L + 1
         L2 = L1 + 1
         G = D(L)
         P = (D(L1) - G) / (2.0E0 * E(L))
         R = PYTHAG(P,1.0E0)
         D(L) = E(L) / (P + SIGN(R,P))
         D(L1) = E(L) * (P + SIGN(R,P))
         DL1 = D(L1)
         H = G - D(L)
         IF (L2 .GT. N) GO TO 145
C
         DO 140 I = L2, N
  140    D(I) = D(I) - H
C
  145    F = F + H
C     .......... QL TRANSFORMATION ..........
         P = D(M)
         C = 1.0E0
         C2 = C
         EL1 = E(L1)
         S = 0.0E0
         MML = M - L
C     .......... FOR I=M-1 STEP -1 UNTIL L DO -- ..........
         DO 200 II = 1, MML
            C3 = C2
            C2 = C
            S2 = S
            I = M - II
            G = C * E(I)
            H = C * P
            IF (ABS(P) .LT. ABS(E(I))) GO TO 150
            C = E(I) / P
            R = SQRT(C*C+1.0E0)
            E(I+1) = S * P * R
            S = C / R
            C = 1.0E0 / R
            GO TO 160
  150       C = P / E(I)
            R = SQRT(C*C+1.0E0)
            E(I+1) = S * E(I) * R
            S = 1.0E0 / R
            C = C * S
  160       P = C * D(I) - S * G
            D(I+1) = H + S * (C * G + S * D(I))
C     .......... FORM VECTOR ..........
            DO 180 K = 1, N
               H = Z(K,I+1)
               Z(K,I+1) = S * Z(K,I) + C * H
               Z(K,I) = C * Z(K,I) - S * H
  180       CONTINUE
C
  200    CONTINUE
C
         P = -S * S2 * C3 * EL1 * E(L) / DL1
         E(L) = S * P
         D(L) = C * P
         IF (B + ABS(E(L)) .GT. B) GO TO 130
  220    D(L) = D(L) + F
  240 CONTINUE
C     .......... ORDER EIGENVALUES AND EIGENVECTORS ..........
      DO 300 II = 2, N
         I = II - 1
         K = I
         P = D(I)
C
         DO 260 J = II, N
            IF (D(J) .LE. P) GO TO 260
            K = J
            P = D(J)
  260    CONTINUE
C
         IF (K .EQ. I) GO TO 300
         D(K) = D(I)
         D(I) = P
C
         DO 280 J = 1, N
            P = Z(J,I)
            Z(J,I) = Z(J,K)
            Z(J,K) = P
  280    CONTINUE
C
  300 CONTINUE
         if(MO.ne.1) goto 500
C
c  Weight the eignvectors by the square root of the
c  corresponding eigenvalue, added by R. Mo
c
      do 400 I=1,N
        P=sqrt(D(I))
        do 400 J=1,N
          Z(J,I)=Z(J,I)*P
 400    continue
 500  continue
c
      GO TO 1001
C     .......... SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS ..........
 1000 IERR = L
      write(6,*) 'NO CONVERGENCE TO AN EIGENVALUE AFTER 30 ITERATIONS'
      write(6,*) 'IERR=',L
 1001 RETURN
      END
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      REAL FUNCTION PYTHAG(A,B)
C***BEGIN PROLOGUE  PYTHAG
C***REFER TO  EISDOC
C
C     Finds sqrt(A**2+B**2) without overflow or destructive underflow
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  PYTHAG
      REAL A,B
C
      REAL P,Q,R,S,T
C***FIRST EXECUTABLE STATEMENT  PYTHAG
      P = AMAX1(ABS(A),ABS(B))
      Q = AMIN1(ABS(A),ABS(B))
      IF (Q .EQ. 0.0E0) GO TO 20
   10 CONTINUE
         R = (Q/P)**2
         T = 4.0E0 + R
         IF (T .EQ. 4.0E0) GO TO 20
         S = R/T
         P = P + 2.0E0*P*S
         Q = Q*S
      GO TO 10
   20 PYTHAG = P
      RETURN
      END
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE TRED2(NM,N,A,D,E,Z,NMK,NK)
C***BEGIN PROLOGUE  TRED2
C***DATE WRITTEN   760101   (YYMMDD)
C***REVISION DATE  830518   (YYMMDD)
C***CATEGORY NO.  D4C1B1
C***KEYWORDS  EIGENVALUES,EIGENVECTORS,EISPACK
C***AUTHOR  SMITH, B. T., ET AL.
C***PURPOSE  Reduce real symmetric matrix to symmetric tridiagonal
C            matrix using and accumulating orthogonal transformation
C***DESCRIPTION
C
C     This subroutine is a translation of the ALGOL procedure TRED2,
C     NUM. MATH. 11, 181-195(1968) by Martin, Reinsch, and Wilkinson.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
C
C     This subroutine reduces a REAL SYMMETRIC matrix to a
C     symmetric tridiagonal matrix using and accumulating
C     orthogonal similarity transformations.
C
C     On Input
C
C        NM must be set to the row dimension of two-dimensional
C          array parameters as declared in the calling program
C          dimension statement (R.Mo set NMK to be the row dimension).
C
C        N is the order of the matrix (R.Mo set NK to be the order).
C
C        A contains the real symmetric input matrix.  Only the
C          lower triangle of the matrix need be supplied.
C
C     On Output
C
C        D contains the diagonal elements of the tridiagonal matrix.
C
C        E contains the subdiagonal elements of the tridiagonal
C          matrix in its last N-1 positions.  E(1) is set to zero.
C
C        Z contains the orthogonal transformation matrix
C          produced in the reduction.
C
C        A and Z may coincide.  If distinct, A is unaltered.
C
C     Questions and comments should be directed to B. S. Garbow,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C     ------------------------------------------------------------------
C***REFERENCES  B. T. SMITH, J. M. BOYLE, J. J. DONGARRA, B. S. GARBOW,
C                 Y. IKEBE, V. C. KLEMA, C. B. MOLER, *MATRIX EIGEN-
C                 SYSTEM ROUTINES - EISPACK GUIDE*, SPRINGER-VERLAG,
C                 1976.
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  TRED2
C
      INTEGER I,J,K,L,N,II,NM,JP1,NMK,NK
      REAL A(NMK,NK),D(NK),E(NK),Z(NMK,NK)
      REAL F,G,H,HH,SCALE
C
C***FIRST EXECUTABLE STATEMENT  TRED2
      DO 100 I = 1, N
C
         DO 100 J = 1, I
            Z(I,J) = A(I,J)
  100 CONTINUE
C
      IF (N .EQ. 1) GO TO 320
C     .......... FOR I=N STEP -1 UNTIL 2 DO -- ..........
      DO 300 II = 2, N
         I = N + 2 - II
         L = I - 1
         H = 0.0E0
         SCALE = 0.0E0
         IF (L .LT. 2) GO TO 130
C     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) ..........
         DO 120 K = 1, L
  120    SCALE = SCALE + ABS(Z(I,K))
C
         IF (SCALE .NE. 0.0E0) GO TO 140
  130    E(I) = Z(I,L)
         GO TO 290
C
  140    DO 150 K = 1, L
            Z(I,K) = Z(I,K) / SCALE
            H = H + Z(I,K) * Z(I,K)
  150    CONTINUE
C
         F = Z(I,L)
         G = -SIGN(SQRT(H),F)
         E(I) = SCALE * G
         H = H - F * G
         Z(I,L) = F - G
         F = 0.0E0
C
         DO 240 J = 1, L
            Z(J,I) = Z(I,J) / H
            G = 0.0E0
C     .......... FORM ELEMENT OF A*U ..........
            DO 180 K = 1, J
  180       G = G + Z(J,K) * Z(I,K)
C
            JP1 = J + 1
            IF (L .LT. JP1) GO TO 220
C
            DO 200 K = JP1, L
  200       G = G + Z(K,J) * Z(I,K)
C     .......... FORM ELEMENT OF P ..........
  220       E(J) = G / H
            F = F + E(J) * Z(I,J)
  240    CONTINUE
C
         HH = F / (H + H)
C     .......... FORM REDUCED A ..........
         DO 260 J = 1, L
            F = Z(I,J)
            G = E(J) - HH * F
            E(J) = G
C
            DO 260 K = 1, J
               Z(J,K) = Z(J,K) - F * E(K) - G * Z(I,K)
  260    CONTINUE
C
  290    D(I) = H
  300 CONTINUE
C
  320 D(1) = 0.0E0
      E(1) = 0.0E0
C     .......... ACCUMULATION OF TRANSFORMATION MATRICES ..........
      DO 500 I = 1, N
         L = I - 1
         IF (D(I) .EQ. 0.0E0) GO TO 380
C
         DO 360 J = 1, L
            G = 0.0E0
C
            DO 340 K = 1, L
  340       G = G + Z(I,K) * Z(K,J)
C
            DO 360 K = 1, L
               Z(K,J) = Z(K,J) - G * Z(K,I)
  360    CONTINUE
C
  380    D(I) = Z(I,I)
         Z(I,I) = 1.0E0
         IF (L .LT. 1) GO TO 500
C
         DO 400 J = 1, L
            Z(I,J) = 0.0E0
            Z(J,I) = 0.0E0
  400    CONTINUE
C
  500 CONTINUE
C
      RETURN
      END

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  The following subroutine is a modified version of the               c
c  normalized-varimax-rotation subroutine given by R. J. Wherry, Sr.   c
c  in his book "Contributions to Correlational Analysis" (Academic     c
c  Press, pp463, 1984; see 442--446). The original subroutine can be   c
c  obtained by including all lines starting with "C" and deleting      c
c  those lines immediately preceding the lines that start with "CD".   c
c                                                                      c
c  The method of normalized varimax rotation was developed by          c
c  Kaiser (1959). It is used to undergo rotation of the principal      c
c  vectors. In this modified subroutine, we assume all communalities   c
c  of a given set of factor loadings are equal to unity (otherwise     c
c  the original subroutine should be used).                            c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        SUBROUTINE VARMO(F,NV,M,H2N,W,SA,SA2,NVK)
CD      SUBROUTINE VARJW(F,L1,L2,H2N,L3,W,L4,SA,L5,SA2,L6,NV,M)
        DIMENSION F(NVK,NVK),W(NVK),SA(NVK),SA2(NVK),H2N(NVK)
CD      DIMENSION F(L1,L2),H2N(L3),W(L4),SA(L5),SA2(L6)
 200    IF(M.EQ.1) GOTO 500
        NIV=M
CD      NIV=30
        EPS=0.00116
        CONS=1.0/SQRT(2.0)
        FM=M
        LM=M-1
        ERR=0.0001
        FNV=NV
        DO 210 I=1,NV
        H2N(I)=1.0/SQRT(H2N(I))
        DO 205 J=1,M
 205    F(I,J)=F(I,J)*H2N(I)
 210    CONTINUE
 220    SV=0.0
        DO 240 I=1,M
        SA(I)=0.0
        SA2(I)=0.0
        DO 230 J=1,NV
        SA(I)=SA(I)+F(J,I)**2
 230    SA2(I)=SA2(I)+F(J,I)**4
        W(I)=(FNV*SA2(I)-SA(I)**2)/(FNV**2)
 240    SV=SV+W(I)
        IF(NIV.EQ.0) GOTO 430
        NIV=NIV-1
	if(NIV.eq.LM) goto 242
        IF((SV-TV).LT.ERR) GOTO 430
 242    TV=SV
        DO 420 J=1,LM
        JJ=J+1
        DO 410 K=JJ,M
        AA=0.0
        BB=0.0
        CC=0.0
        DD=0.0
        DO 250 I=1,NV
        XX=F(I,J)
        YY=F(I,K)
        UU=(XX+YY)*(XX-YY)
        VV=2.0*XX*YY
        CC=CC+(UU+VV)*(UU-VV)
        DD=DD+2.0*UU*VV
        AA=AA+UU
 250    BB=BB+VV
        T=DD-2.0*AA*BB/FNV
        B=CC-(AA**2-BB**2)/FNV
	G=SQRT(T*T+B*B)
	if(G.lt.0.0001) then
	  COST=1.0
	  SINT=0.0
	else
          COS2T=SQRT((1.0+(B/G))/2.0)
	  COST=SQRT((1.0+COS2T)/2.0)
	  SINT=SQRT((1.0-COS2T)/2.0)
	  if(T.lt.0.0) SINT=-SINT
	endif
	  
 390    DO 400 I=1,NV
        AIJ=F(I,J)*COST+F(I,K)*SINT
        AIK=F(I,K)*COST-F(I,J)*SINT
        F(I,J)=AIJ
 400    F(I,K)=AIK
 410    CONTINUE

        SV=0.0
        DO 415 IS=1,M
        SA(IS)=0.0
        SA2(IS)=0.0
        DO 412 JS=1,NV
        SA(IS)=SA(IS)+F(JS,IS)**2
 412    SA2(IS)=SA2(IS)+F(JS,IS)**4
        W(IS)=(FNV*SA2(IS)-SA(IS)**2)/(FNV**2)
 415    SV=SV+W(IS)

 420    CONTINUE
        GOTO 220
 430    CONTINUE
        DO 450 I=1,NV
        AA=1.0/H2N(I)
        DO 440 J=1,M
  440   F(I,J)=F(I,J)*AA
  450   H2N(I)=AA**2
        DO 480 I=1,M
        SUMR=0.0
        DO 460 J=1,NV
 460    SUMR=SUMR+F(J,I)
        IF(SUMR.GE.0.0) GOTO 480
        DO 470 J=1,NV
 470    F(J,I)=-F(J,I)
 480    CONTINUE
 500    RETURN
        END

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        subroutine rfa2(data,LLAF,LLAE,resu,um,subf,indx,KEY2,idf)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      real resu(LLAF,LLAF),data(LLAF,idf),um(idf,LLAF)
      real subf(LLAF)
      integer indx(LLAF)

c
c   Calculate the first KEY2 rotating principal components
c
        call LUDCMP(resu,KEY2,LLAF,indx,ddd)
        do 515 i=1,idf
        do 513 j=1,KEY2
        subf(j)=0.0
        do 512 k=1,KEY2
        subf(j)=subf(j)+data(j,k)*um(i,k)
 512    continue
 513    continue
        call LUBKSB(resu,KEY2,LLAF,indx,subf)
        do 514 j=1,KEY2
        um(i,j)=subf(j)
 514    continue
 515    continue
 516    continue
        return
        end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        include "/diska/hlin/qgmodel/EOF/ludcmp.for"
        include "/diskc/hlin/recipes/lubksb.for"
