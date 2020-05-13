      INTEGER FUNCTION CVMGM(X1,X2,X3)
      INTEGER X1,X2
      IF (X3.LT.0.0) THEN
	 CVMGM=X1
      ELSE
	 CVMGM=X2
      ENDIF
      END
C
      REAL FUNCTION CVMGZ(X1,X2,X3)
      INTEGER X3
      IF (X3.EQ.0) THEN
	 CVMGZ=X1
      ELSE
	 CVMGZ=X2
      ENDIF
      END
C
c***********************************************************************
c
	subroutine minv(ab,n,nd,scratch,det,eps,m,mode)
       parameter(imax=400)
	 implicit real (a-h,o-z)
       dimension ab(nd,n+m),scratch(2*n),y(imax,imax)
       if (mode.ne.1) then
	 write(*,*) 'incompat mode ne 1'
	 stop
       else if (m.ne.0) then
	 write(*,*)' incompat m ne 0'
	 stop
      else
       det=0.
       call gaussj(ab,nd,nd,y,0,imax)

      endif
       return
       end


       subroutine mxma(sa,iac,iar,sb,ibc,ibr,sc,icc,icr,nrp,m,ncp)
       implicit none
       integer nrp,m,ncp ! array sizes
       integer iac,iar,ibc,ibr,icc,icr ! increemeents between rows
       real sa(1),sb(1),sc(1)
       integer k,j,i

C init product
       do 120 k=1,ncp
	 do 110 i=1,nrp
	   sc(1+(i-1)*icc+(k-1)*icr)=0.0
 110     continue
 120   continue

C multiply matrices
       do 230 k=1,ncp
	 do 220 j=1,m
	   do 210 i=1,nrp
	     sc(1+(i-1)*icc+(k-1)*icr)=
     &         sc(1+(i-1)*icc+(k-1)*icr)+
     &         sa(1+(i-1)*iac+(j-1)*iar)*
     &         sb(1+(j-1)*ibc+(k-1)*ibr)
 210       continue
 220     continue
 230   continue
       return
       end

c i'm sure this should be here 'coz it belongs to num recipes, so you
c should only use it if you've got num. recipies...
c simon
c
c
      subroutine gaussj(a,n,np,b,m,mp)
	 implicit real (a-h,o-z)
      parameter (nmax=50)
      dimension a(np,np),b(np,mp),ipiv(nmax),indxr(nmax),indxc(nmax)
      do 11 j=1,n
	ipiv(j)=0
11    continue
      do 22 i=1,n
	big=0.
	do 13 j=1,n
	  if(ipiv(j).ne.1)then
	    do 12 k=1,n
	      if (ipiv(k).eq.0) then
		if (abs(a(j,k)).ge.big)then
		  big=abs(a(j,k))
		  irow=j
		  icol=k
		endif
	      else if (ipiv(k).gt.1) then
		pause 'singular matrix'
	      endif
12          continue
	  endif
13      continue
	ipiv(icol)=ipiv(icol)+1
	if (irow.ne.icol) then
	  do 14 l=1,n
	    dum=a(irow,l)
	    a(irow,l)=a(icol,l)
	    a(icol,l)=dum
14        continue
	  do 15 l=1,m
	    dum=b(irow,l)
	    b(irow,l)=b(icol,l)
	    b(icol,l)=dum
15        continue
	endif
	indxr(i)=irow
	indxc(i)=icol
	if (a(icol,icol).eq.0.) pause 'singular matrix.'
	pivinv=1./a(icol,icol)
	a(icol,icol)=1.
	do 16 l=1,n
	  a(icol,l)=a(icol,l)*pivinv
16      continue
	do 17 l=1,m
	  b(icol,l)=b(icol,l)*pivinv
17      continue
	do 21 ll=1,n
	  if(ll.ne.icol)then
	    dum=a(ll,icol)
	    a(ll,icol)=0.
	    do 18 l=1,n
	      a(ll,l)=a(ll,l)-a(icol,l)*dum
18          continue
	    do 19 l=1,m
	      b(ll,l)=b(ll,l)-b(icol,l)*dum
19          continue
	  endif
21      continue
22    continue
      do 24 l=n,1,-1
	if(indxr(l).ne.indxc(l))then
	  do 23 k=1,n
	    dum=a(k,indxr(l))
	    a(k,indxr(l))=a(k,indxc(l))
	    a(k,indxc(l))=dum
23        continue
	endif
24    continue
      return
      end
**** decks fft991f, set99, rpassm, qpassm ****
*
*
*DECK FFT991F
C     SUBROUTINE 'FFT991' - MULTIPLE FAST REAL PERIODIC TRANSFORM
C
C     Minor modifications - Clive Temperton, January 1991:
C         modified vector-chopping for better performance
C
C     REAL TRANSFORM OF LENGTH N PERFORMED BY REMOVING REDUNDANT
C     OPERATIONS FROM COMPLEX TRANSFORM OF LENGTH N
C
C     A IS THE ARRAY CONTAINING INPUT & OUTPUT DATA
C     WORK IS AN AREA OF SIZE (N+1)*MIN(LOT,64)
C     TRIGS IS A PREVIOUSLY PREPARED LIST OF TRIG FUNCTION VALUES
C     IFAX IS A PREVIOUSLY PREPARED LIST OF FACTORS OF N
C     INC IS THE INCREMENT WITHIN EACH DATA 'VECTOR'
C         (E.G. INC=1 FOR CONSECUTIVELY STORED DATA)
C     JUMP IS THE INCREMENT BETWEEN THE START OF EACH DATA VECTOR
C     N IS THE LENGTH OF THE DATA VECTORS
C     LOT IS THE NUMBER OF DATA VECTORS
C     ISIGN = +1 FOR TRANSFORM FROM SPECTRAL TO GRIDPOINT
C           = -1 FOR TRANSFORM FROM GRIDPOINT TO SPECTRAL
C
C     ORDERING OF COEFFICIENTS:
C         A(0),B(0),A(1),B(1),A(2),B(2),...,A(N/2),B(N/2)
C         WHERE B(0)=B(N/2)=0; (N+2) LOCATIONS REQUIRED
C
C     ORDERING OF DATA:
C         X(0),X(1),X(2),...,X(N-1), 0 , 0 ; (N+2) LOCATIONS REQUIRED
C
C     VECTORIZATION IS ACHIEVED ON CRAY BY DOING THE TRANSFORMS
C     IN PARALLEL
C
C     N MUST BE COMPOSED OF FACTORS 2,3 & 5 BUT DOES NOT HAVE TO BE EVEN
C
C     DEFINITION OF TRANSFORMS:
C     -------------------------
C
C     ISIGN=+1: X(J)=SUM(K=0,...,N-1)(C(K)*EXP(2*I*J*K*PI/N))
C         WHERE C(K)=A(K)+I*B(K) AND C(N-K)=A(K)-I*B(K)
C
C     ISIGN=-1: A(K)=(1/N)*SUM(J=0,...,N-1)(X(J)*COS(2*J*K*PI/N))
C               B(K)=-(1/N)*SUM(J=0,...,N-1)(X(J)*SIN(2*J*K*PI/N))
C
      SUBROUTINE FFT991(A,WORK,TRIGS,IFAX,INC,JUMP,N,LOT,ISIGN)
      DIMENSION A(N),WORK(N),TRIGS(N),IFAX(1)
C
      NFAX=IFAX(1)
      NX=N+1
      IF (MOD(N,2).EQ.1) NX=N
      NBLOX=1+(LOT-1)/64
      LEFT=LOT
      IF (ISIGN.EQ.-1) GO TO 300
C
C     ISIGN=+1, SPECTRAL TO GRIDPOINT TRANSFORM
C     -----------------------------------------
  100 CONTINUE
      ISTART=1
      DO 220 NB=1,NBLOX
      IF (LEFT.LE.64) THEN
	 NVEX=LEFT
      ELSE IF (LEFT.LT.128) THEN
	 NVEX=LEFT/2
      ELSE
	 NVEX=64
      ENDIF
      LEFT=LEFT-NVEX
      IA=ISTART
      I=ISTART
CDIR$ IVDEP
      DO 110 J=1,NVEX
      A(I+INC)=0.5*A(I)
      I=I+JUMP
  110 CONTINUE
      IF (MOD(N,2).EQ.1) GO TO 130
      I=ISTART+N*INC
      DO 120 J=1,NVEX
      A(I)=0.5*A(I)
      I=I+JUMP
  120 CONTINUE
  130 CONTINUE
      IA=ISTART+INC
      LA=1
      IGO=+1
C
      DO 160 K=1,NFAX
      IFAC=IFAX(K+1)
      IERR=-1
      IF (IGO.EQ.-1) GO TO 140
      CALL RPASSM(A(IA),A(IA+LA*INC),WORK(1),WORK(IFAC*LA+1),TRIGS,
     *    INC,1,JUMP,NX,NVEX,N,IFAC,LA,IERR)
      GO TO 150
  140 CONTINUE
      CALL RPASSM(WORK(1),WORK(LA+1),A(IA),A(IA+IFAC*LA*INC),TRIGS,
     *    1,INC,NX,JUMP,NVEX,N,IFAC,LA,IERR)
  150 CONTINUE
      IF (IERR.NE.0) GO TO 500
      LA=IFAC*LA
      IGO=-IGO
      IA=ISTART
  160 CONTINUE
C
C     IF NECESSARY, COPY RESULTS BACK TO A
C     ------------------------------------
      IF (MOD(NFAX,2).EQ.0) GO TO 190
      IBASE=1
      JBASE=IA
      DO 180 JJ=1,NVEX
      I=IBASE
      J=JBASE
      DO 170 II=1,N
      A(J)=WORK(I)
      I=I+1
      J=J+INC
  170 CONTINUE
      IBASE=IBASE+NX
      JBASE=JBASE+JUMP
  180 CONTINUE
  190 CONTINUE
C
C     FILL IN ZEROS AT END
C     --------------------
      IX=ISTART+N*INC
CDIR$ IVDEP
      DO 210 J=1,NVEX
      A(IX)=0.0
      A(IX+INC)=0.0
      IX=IX+JUMP
  210 CONTINUE
C
      ISTART=ISTART+NVEX*JUMP
  220 CONTINUE
      RETURN
C
C     ISIGN=-1, GRIDPOINT TO SPECTRAL TRANSFORM
C     -----------------------------------------
  300 CONTINUE
      ISTART=1
      DO 410 NB=1,NBLOX
      IF (LEFT.LE.64) THEN
	 NVEX=LEFT
      ELSE IF (LEFT.LT.128) THEN
	 NVEX=LEFT/2
      ELSE
	 NVEX=64
      ENDIF
      LEFT=LEFT-NVEX
      IA=ISTART
      LA=N
      IGO=+1
C
      DO 340 K=1,NFAX
      IFAC=IFAX(NFAX+2-K)
      LA=LA/IFAC
      IERR=-1
      IF (IGO.EQ.-1) GO TO 320
      CALL QPASSM(A(IA),A(IA+IFAC*LA*INC),WORK(1),WORK(LA+1),TRIGS,
     *    INC,1,JUMP,NX,NVEX,N,IFAC,LA,IERR)
      GO TO 330
  320 CONTINUE
      CALL QPASSM(WORK(1),WORK(IFAC*LA+1),A(IA),A(IA+LA*INC),TRIGS,
     *    1,INC,NX,JUMP,NVEX,N,IFAC,LA,IERR)
  330 CONTINUE
      IF (IERR.NE.0) GO TO 500
      IGO=-IGO
      IA=ISTART+INC
  340 CONTINUE
C
C     IF NECESSARY, COPY RESULTS BACK TO A
C     ------------------------------------
      IF (MOD(NFAX,2).EQ.0) GO TO 370
      IBASE=1
      JBASE=IA
      DO 360 JJ=1,NVEX
      I=IBASE
      J=JBASE
      DO 350 II=1,N
      A(J)=WORK(I)
      I=I+1
      J=J+INC
  350 CONTINUE
      IBASE=IBASE+NX
      JBASE=JBASE+JUMP
  360 CONTINUE
  370 CONTINUE
C
C     SHIFT A(0) & FILL IN ZERO IMAG PARTS
C     ------------------------------------
      IX=ISTART
CDIR$ IVDEP
      DO 380 J=1,NVEX
      A(IX)=A(IX+INC)
      A(IX+INC)=0.0
      IX=IX+JUMP
  380 CONTINUE
      IF (MOD(N,2).EQ.1) GO TO 400
      IZ=ISTART+(N+1)*INC
      DO 390 J=1,NVEX
      A(IZ)=0.0
      IZ=IZ+JUMP
  390 CONTINUE
  400 CONTINUE
C
      ISTART=ISTART+NVEX*JUMP
  410 CONTINUE
      RETURN
C
C     ERROR MESSAGES
C     --------------
  500 CONTINUE
      GO TO (510,530,550) IERR
  510 CONTINUE
      WRITE(6,520) NVEX
  520 FORMAT(16H1VECTOR LENGTH =,I4,17H, GREATER THAN 64)
      GO TO 570
  530 CONTINUE
      WRITE(6,540) IFAC
  540 FORMAT( 9H1FACTOR =,I3,17H, NOT CATERED FOR)
      GO TO 570
  550 CONTINUE
      WRITE(6,560) IFAC
  560 FORMAT(9H1FACTOR =,I3,31H, ONLY CATERED FOR IF LA*IFAC=N)
  570 CONTINUE
      RETURN
      END
*DECK SET99
C     SUBROUTINE 'SET99' - COMPUTES FACTORS OF N & TRIGONOMETRIC
C     FUNCTINS REQUIRED BY FFT991(F)
C
      SUBROUTINE SET99(TRIGS,IFAX,N)
      DIMENSION TRIGS(N),IFAX(1),JFAX(10),LFAX(7)
      DATA LFAX/6,8,5,4,3,2,1/
C
      DEL=4.0*ASIN(1.0)/FLOAT(N)
      NIL=0
      NHL=(N/2)-1
      DO 10 K=NIL,NHL
      ANGLE=FLOAT(K)*DEL
      TRIGS(2*K+1)=COS(ANGLE)
      TRIGS(2*K+2)=SIN(ANGLE)
   10 CONTINUE
C
C     FIND FACTORS OF N (8,6,5,4,3,2; ONLY ONE 8 ALLOWED)
C     LOOK FOR SIXES FIRST, STORE FACTORS IN DESCENDING ORDER
      NU=N
      IFAC=6
      K=0
      L=1
   20 CONTINUE
      IF (MOD(NU,IFAC).NE.0) GO TO 30
      K=K+1
      JFAX(K)=IFAC
      IF (IFAC.NE.8) GO TO 25
      IF (K.EQ.1) GO TO 25
      JFAX(1)=8
      JFAX(K)=6
   25 CONTINUE
      NU=NU/IFAC
      IF (NU.EQ.1) GO TO 50
      IF (IFAC.NE.8) GO TO 20
   30 CONTINUE
      L=L+1
      IFAC=LFAX(L)
      IF (IFAC.GT.1) GO TO 20
C
      WRITE(6,40) N
   40 FORMAT(4H1N =,I4,27H - CONTAINS ILLEGAL FACTORS)
      RETURN
C
C     NOW REVERSE ORDER OF FACTORS
   50 CONTINUE
      NFAX=K
      IFAX(1)=NFAX
      DO 60 I=1,NFAX
      IFAX(NFAX+2-I)=JFAX(I)
   60 CONTINUE
      RETURN
      END
*DECK RPASSM
C     SUBROUTINE 'RPASSM' - PERFORMS ONE PASS THROUGH DATA AS PART
C     OF MULTIPLE REAL FFT (FOURIER SYNTHESIS) ROUTINE
C
C     A IS FIRST REAL INPUT VECTOR
C         EQUIVALENCE B(1) WITH A (LA*INC1+1)
C     C IS FIRST REAL OUTPUT VECTOR
C         EQUIVALENCE D(1) WITH C(IFAC*LA*INC2+1)
C     TRIGS IS A PRECALCULATED LIST OF SINES & COSINES
C     INC1 IS THE ADDRESSING INCREMENT FOR A
C     INC2 IS THE ADDRESSING INCREMENT FOR C
C     INC3 IS THE INCREMENT BETWEEN INPUT VECTORS A
C     INC4 IS THE INCREMENT BETWEEN OUTPUT VECTORS C
C     LOT IS THE NUMBER OF VECTORS
C     N IS THE LENGTH OF THE VECTORS
C     IFAC IS THE CURRENT FACTOR OF N
C     LA IS THE PRODUCT OF PREVIOUS FACTORS
C     IERR IS AN ERROR INDICATOR:
C              0 - PASS COMPLETED WITHOUT ERROR
C              1 - LOT GREATER THAN 64
C              2 - IFAC NOT CATERED FOR
C              3 - IFAC ONLY CATERED FOR IF LA=N/IFAC
C
C-----------------------------------------------------------------------
C
      SUBROUTINE RPASSM(A,B,C,D,TRIGS,INC1,INC2,INC3,INC4,LOT,N,IFAC,
     *    LA,IERR)
      DIMENSION A(N),B(N),C(N),D(N),TRIGS(N)
C
      DIMENSION A10(64),A11(64),A20(64),A21(64),
     *    B10(64),B11(64),B20(64),B21(64)
      DATA SIN36/0.587785252292473/,SIN72/0.951056516295154/,
     *    QRT5/0.559016994374947/,SIN60/0.866025403784437/
C
      M=N/IFAC
      IINK=LA*INC1
      JINK=LA*INC2
      JUMP=(IFAC-1)*JINK
      KSTOP=(N-IFAC)/(2*IFAC)
C
      IBAD=1
      IF (LOT.GT.64) GO TO 910
      IBASE=0
      JBASE=0
      IGO=IFAC-1
      IF (IGO.EQ.7) IGO=6
      IBAD=2
      IF (IGO.LT.1.OR.IGO.GT.6) GO TO 910
      GO TO (200,300,400,500,600,800),IGO
C
C     CODING FOR FACTOR 2
C     -------------------
  200 CONTINUE
      IA=1
      IB=IA+(2*M-LA)*INC1
      JA=1
      JB=JA+JINK
C
      IF (LA.EQ.M) GO TO 290
C
      DO 220 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 210 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      C(JB+J)=A(IA+I)-A(IB+I)
      I=I+INC3
      J=J+INC4
  210 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  220 CONTINUE
      IA=IA+IINK
      IINK=2*IINK
      IB=IB-IINK
      IBASE=0
      JBASE=JBASE+JUMP
      JUMP=2*JUMP+JINK
      IF (IA.EQ.IB) GO TO 260
      DO 250 K=LA,KSTOP,LA
      KB=K+K
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      IBASE=0
      DO 240 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 230 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      D(JA+J)=B(IA+I)-B(IB+I)
      C(JB+J)=C1*(A(IA+I)-A(IB+I))-S1*(B(IA+I)+B(IB+I))
      D(JB+J)=S1*(A(IA+I)-A(IB+I))+C1*(B(IA+I)+B(IB+I))
      I=I+INC3
      J=J+INC4
  230 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  240 CONTINUE
      IA=IA+IINK
      IB=IB-IINK
      JBASE=JBASE+JUMP
  250 CONTINUE
      IF (IA.GT.IB) GO TO 900
  260 CONTINUE
      IBASE=0
      DO 280 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 270 IJK=1,LOT
      C(JA+J)=A(IA+I)
      C(JB+J)=-B(IA+I)
      I=I+INC3
      J=J+INC4
  270 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  280 CONTINUE
      GO TO 900
C
  290 CONTINUE
      DO 294 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 292 IJK=1,LOT
      C(JA+J)=2.0*(A(IA+I)+A(IB+I))
      C(JB+J)=2.0*(A(IA+I)-A(IB+I))
      I=I+INC3
      J=J+INC4
  292 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  294 CONTINUE
      GO TO 900
C
C     CODING FOR FACTOR 3
C     -------------------
  300 CONTINUE
      IA=1
      IB=IA+(2*M-LA)*INC1
      IC=IB
      JA=1
      JB=JA+JINK
      JC=JB+JINK
C
      IF (LA.EQ.M) GO TO 390
C
      DO 320 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 310 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      C(JB+J)=(A(IA+I)-0.5*A(IB+I))-(SIN60*(B(IB+I)))
      C(JC+J)=(A(IA+I)-0.5*A(IB+I))+(SIN60*(B(IB+I)))
      I=I+INC3
      J=J+INC4
  310 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  320 CONTINUE
      IA=IA+IINK
      IINK=2*IINK
      IB=IB+IINK
      IC=IC-IINK
      JBASE=JBASE+JUMP
      JUMP=2*JUMP+JINK
      IF (IA.EQ.IC) GO TO 360
      DO 350 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      IBASE=0
      DO 340 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 330 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IC+I))
      D(JA+J)=B(IA+I)+(B(IB+I)-B(IC+I))
      C(JB+J)=
     *    C1*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))-(SIN60*(B(IB+I)+B(IC+I))))
     *   -S1*((B(IA+I)-0.5*(B(IB+I)-B(IC+I)))+(SIN60*(A(IB+I)-A(IC+I))))
      D(JB+J)=
     *    S1*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))-(SIN60*(B(IB+I)+B(IC+I))))
     *   +C1*((B(IA+I)-0.5*(B(IB+I)-B(IC+I)))+(SIN60*(A(IB+I)-A(IC+I))))
      C(JC+J)=
     *    C2*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))+(SIN60*(B(IB+I)+B(IC+I))))
     *   -S2*((B(IA+I)-0.5*(B(IB+I)-B(IC+I)))-(SIN60*(A(IB+I)-A(IC+I))))
      D(JC+J)=
     *    S2*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))+(SIN60*(B(IB+I)+B(IC+I))))
     *   +C2*((B(IA+I)-0.5*(B(IB+I)-B(IC+I)))-(SIN60*(A(IB+I)-A(IC+I))))
      I=I+INC3
      J=J+INC4
  330 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  340 CONTINUE
      IA=IA+IINK
      IB=IB+IINK
      IC=IC-IINK
      JBASE=JBASE+JUMP
  350 CONTINUE
      IF (IA.GT.IC) GO TO 900
  360 CONTINUE
      IBASE=0
      DO 380 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 370 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      C(JB+J)=(0.5*A(IA+I)-A(IB+I))-(SIN60*B(IA+I))
      C(JC+J)=-(0.5*A(IA+I)-A(IB+I))-(SIN60*B(IA+I))
      I=I+INC3
      J=J+INC4
  370 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  380 CONTINUE
      GO TO 900
C
  390 CONTINUE
      SSIN60=2.0*SIN60
      DO 394 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 392 IJK=1,LOT
      C(JA+J)=2.0*(A(IA+I)+A(IB+I))
      C(JB+J)=(2.0*A(IA+I)-A(IB+I))-(SSIN60*B(IB+I))
      C(JC+J)=(2.0*A(IA+I)-A(IB+I))+(SSIN60*B(IB+I))
      I=I+INC3
      J=J+INC4
  392 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  394 CONTINUE
      GO TO 900
C
C     CODING FOR FACTOR 4
C     -------------------
  400 CONTINUE
      IA=1
      IB=IA+(2*M-LA)*INC1
      IC=IB+2*M*INC1
      ID=IB
      JA=1
      JB=JA+JINK
      JC=JB+JINK
      JD=JC+JINK
C
      IF (LA.EQ.M) GO TO 490
C
      DO 420 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 410 IJK=1,LOT
      C(JA+J)=(A(IA+I)+A(IC+I))+A(IB+I)
      C(JB+J)=(A(IA+I)-A(IC+I))-B(IB+I)
      C(JC+J)=(A(IA+I)+A(IC+I))-A(IB+I)
      C(JD+J)=(A(IA+I)-A(IC+I))+B(IB+I)
      I=I+INC3
      J=J+INC4
  410 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  420 CONTINUE
      IA=IA+IINK
      IINK=2*IINK
      IB=IB+IINK
      IC=IC-IINK
      ID=ID-IINK
      JBASE=JBASE+JUMP
      JUMP=2*JUMP+JINK
      IF (IB.EQ.IC) GO TO 460
      DO 450 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      KD=KC+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      IBASE=0
      DO 440 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 430 IJK=1,LOT
      C(JA+J)=(A(IA+I)+A(IC+I))+(A(IB+I)+A(ID+I))
      D(JA+J)=(B(IA+I)-B(IC+I))+(B(IB+I)-B(ID+I))
      C(JC+J)=
     *    C2*((A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I)))
     *   -S2*((B(IA+I)-B(IC+I))-(B(IB+I)-B(ID+I)))
      D(JC+J)=
     *    S2*((A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I)))
     *   +C2*((B(IA+I)-B(IC+I))-(B(IB+I)-B(ID+I)))
      C(JB+J)=
     *    C1*((A(IA+I)-A(IC+I))-(B(IB+I)+B(ID+I)))
     *   -S1*((B(IA+I)+B(IC+I))+(A(IB+I)-A(ID+I)))
      D(JB+J)=
     *    S1*((A(IA+I)-A(IC+I))-(B(IB+I)+B(ID+I)))
     *   +C1*((B(IA+I)+B(IC+I))+(A(IB+I)-A(ID+I)))
      C(JD+J)=
     *    C3*((A(IA+I)-A(IC+I))+(B(IB+I)+B(ID+I)))
     *   -S3*((B(IA+I)+B(IC+I))-(A(IB+I)-A(ID+I)))
      D(JD+J)=
     *    S3*((A(IA+I)-A(IC+I))+(B(IB+I)+B(ID+I)))
     *   +C3*((B(IA+I)+B(IC+I))-(A(IB+I)-A(ID+I)))
      I=I+INC3
      J=J+INC4
  430 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  440 CONTINUE
      IA=IA+IINK
      IB=IB+IINK
      IC=IC-IINK
      ID=ID-IINK
      JBASE=JBASE+JUMP
  450 CONTINUE
      IF (IB.GT.IC) GO TO 900
  460 CONTINUE
      IBASE=0
      SIN45=SQRT(0.5)
      DO 480 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 470 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      C(JB+J)=SIN45*((A(IA+I)-A(IB+I))-(B(IA+I)+B(IB+I)))
      C(JC+J)=B(IB+I)-B(IA+I)
      C(JD+J)=-SIN45*((A(IA+I)-A(IB+I))+(B(IA+I)+B(IB+I)))
      I=I+INC3
      J=J+INC4
  470 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  480 CONTINUE
      GO TO 900
C
  490 CONTINUE
      DO 494 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 492 IJK=1,LOT
      C(JA+J)=2.0*((A(IA+I)+A(IC+I))+A(IB+I))
      C(JB+J)=2.0*((A(IA+I)-A(IC+I))-B(IB+I))
      C(JC+J)=2.0*((A(IA+I)+A(IC+I))-A(IB+I))
      C(JD+J)=2.0*((A(IA+I)-A(IC+I))+B(IB+I))
      I=I+INC3
      J=J+INC4
  492 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  494 CONTINUE
      GO TO 900
C
C     CODING FOR FACTOR 5
C     -------------------
  500 CONTINUE
      IA=1
      IB=IA+(2*M-LA)*INC1
      IC=IB+2*M*INC1
      ID=IC
      IE=IB
      JA=1
      JB=JA+JINK
      JC=JB+JINK
      JD=JC+JINK
      JE=JD+JINK
C
      IF (LA.EQ.M) GO TO 590
C
      DO 520 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 510 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IC+I))
      C(JB+J)=((A(IA+I)-0.25*(A(IB+I)+A(IC+I)))+QRT5*(A(IB+I)-A(IC+I)))
     *    -(SIN72*B(IB+I)+SIN36*B(IC+I))
      C(JC+J)=((A(IA+I)-0.25*(A(IB+I)+A(IC+I)))-QRT5*(A(IB+I)-A(IC+I)))
     *    -(SIN36*B(IB+I)-SIN72*B(IC+I))
      C(JD+J)=((A(IA+I)-0.25*(A(IB+I)+A(IC+I)))-QRT5*(A(IB+I)-A(IC+I)))
     *    +(SIN36*B(IB+I)-SIN72*B(IC+I))
      C(JE+J)=((A(IA+I)-0.25*(A(IB+I)+A(IC+I)))+QRT5*(A(IB+I)-A(IC+I)))
     *    +(SIN72*B(IB+I)+SIN36*B(IC+I))
      I=I+INC3
      J=J+INC4
  510 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  520 CONTINUE
      IA=IA+IINK
      IINK=2*IINK
      IB=IB+IINK
      IC=IC+IINK
      ID=ID-IINK
      IE=IE-IINK
      JBASE=JBASE+JUMP
      JUMP=2*JUMP+JINK
      IF (IB.EQ.ID) GO TO 560
      DO 550 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      KD=KC+KB
      KE=KD+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      C4=TRIGS(KE+1)
      S4=TRIGS(KE+2)
      IBASE=0
      DO 540 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 530 IJK=1,LOT
C
      A10(IJK)=(A(IA+I)-0.25*((A(IB+I)+A(IE+I))+(A(IC+I)+A(ID+I))))
     *    +QRT5*((A(IB+I)+A(IE+I))-(A(IC+I)+A(ID+I)))
      A20(IJK)=(A(IA+I)-0.25*((A(IB+I)+A(IE+I))+(A(IC+I)+A(ID+I))))
     *    -QRT5*((A(IB+I)+A(IE+I))-(A(IC+I)+A(ID+I)))
      B10(IJK)=(B(IA+I)-0.25*((B(IB+I)-B(IE+I))+(B(IC+I)-B(ID+I))))
     *    +QRT5*((B(IB+I)-B(IE+I))-(B(IC+I)-B(ID+I)))
      B20(IJK)=(B(IA+I)-0.25*((B(IB+I)-B(IE+I))+(B(IC+I)-B(ID+I))))
     *    -QRT5*((B(IB+I)-B(IE+I))-(B(IC+I)-B(ID+I)))
      A11(IJK)=SIN72*(B(IB+I)+B(IE+I))+SIN36*(B(IC+I)+B(ID+I))
      A21(IJK)=SIN36*(B(IB+I)+B(IE+I))-SIN72*(B(IC+I)+B(ID+I))
      B11(IJK)=SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I))
      B21(IJK)=SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I))
C
      C(JA+J)=A(IA+I)+((A(IB+I)+A(IE+I))+(A(IC+I)+A(ID+I)))
      D(JA+J)=B(IA+I)+((B(IB+I)-B(IE+I))+(B(IC+I)-B(ID+I)))
      C(JB+J)=C1*(A10(IJK)-A11(IJK))-S1*(B10(IJK)+B11(IJK))
      D(JB+J)=S1*(A10(IJK)-A11(IJK))+C1*(B10(IJK)+B11(IJK))
      C(JE+J)=C4*(A10(IJK)+A11(IJK))-S4*(B10(IJK)-B11(IJK))
      D(JE+J)=S4*(A10(IJK)+A11(IJK))+C4*(B10(IJK)-B11(IJK))
      C(JC+J)=C2*(A20(IJK)-A21(IJK))-S2*(B20(IJK)+B21(IJK))
      D(JC+J)=S2*(A20(IJK)-A21(IJK))+C2*(B20(IJK)+B21(IJK))
      C(JD+J)=C3*(A20(IJK)+A21(IJK))-S3*(B20(IJK)-B21(IJK))
      D(JD+J)=S3*(A20(IJK)+A21(IJK))+C3*(B20(IJK)-B21(IJK))
C
      I=I+INC3
      J=J+INC4
  530 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  540 CONTINUE
      IA=IA+IINK
      IB=IB+IINK
      IC=IC+IINK
      ID=ID-IINK
      IE=IE-IINK
      JBASE=JBASE+JUMP
  550 CONTINUE
      IF (IB.GT.ID) GO TO 900
  560 CONTINUE
      IBASE=0
      DO 580 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 570 IJK=1,LOT
      C(JA+J)=(A(IA+I)+A(IB+I))+A(IC+I)
      C(JB+J)=(QRT5*(A(IA+I)-A(IB+I))+(0.25*(A(IA+I)+A(IB+I))-A(IC+I)))
     *    -(SIN36*B(IA+I)+SIN72*B(IB+I))
      C(JE+J)=-(QRT5*(A(IA+I)-A(IB+I))+(0.25*(A(IA+I)+A(IB+I))-A(IC+I)))
     *    -(SIN36*B(IA+I)+SIN72*B(IB+I))
      C(JC+J)=(QRT5*(A(IA+I)-A(IB+I))-(0.25*(A(IA+I)+A(IB+I))-A(IC+I)))
     *    -(SIN72*B(IA+I)-SIN36*B(IB+I))
      C(JD+J)=-(QRT5*(A(IA+I)-A(IB+I))-(0.25*(A(IA+I)+A(IB+I))-A(IC+I)))
     *    -(SIN72*B(IA+I)-SIN36*B(IB+I))
      I=I+INC3
      J=J+INC4
  570 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  580 CONTINUE
      GO TO 900
C
  590 CONTINUE
      QQRT5=2.0*QRT5
      SSIN36=2.0*SIN36
      SSIN72=2.0*SIN72
      DO 594 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 592 IJK=1,LOT
      C(JA+J)=2.0*(A(IA+I)+(A(IB+I)+A(IC+I)))
      C(JB+J)=(2.0*(A(IA+I)-0.25*(A(IB+I)+A(IC+I)))
     *    +QQRT5*(A(IB+I)-A(IC+I)))-(SSIN72*B(IB+I)+SSIN36*B(IC+I))
      C(JC+J)=(2.0*(A(IA+I)-0.25*(A(IB+I)+A(IC+I)))
     *    -QQRT5*(A(IB+I)-A(IC+I)))-(SSIN36*B(IB+I)-SSIN72*B(IC+I))
      C(JD+J)=(2.0*(A(IA+I)-0.25*(A(IB+I)+A(IC+I)))
     *    -QQRT5*(A(IB+I)-A(IC+I)))+(SSIN36*B(IB+I)-SSIN72*B(IC+I))
      C(JE+J)=(2.0*(A(IA+I)-0.25*(A(IB+I)+A(IC+I)))
     *    +QQRT5*(A(IB+I)-A(IC+I)))+(SSIN72*B(IB+I)+SSIN36*B(IC+I))
      I=I+INC3
      J=J+INC4
  592 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  594 CONTINUE
      GO TO 900
C
C     CODING FOR FACTOR 6
C     -------------------
  600 CONTINUE
      IA=1
      IB=IA+(2*M-LA)*INC1
      IC=IB+2*M*INC1
      ID=IC+2*M*INC1
      IE=IC
      IF=IB
      JA=1
      JB=JA+JINK
      JC=JB+JINK
      JD=JC+JINK
      JE=JD+JINK
      JF=JE+JINK
C
      IF (LA.EQ.M) GO TO 690
C
      DO 620 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 610 IJK=1,LOT
      C(JA+J)=(A(IA+I)+A(ID+I))+(A(IB+I)+A(IC+I))
      C(JD+J)=(A(IA+I)-A(ID+I))-(A(IB+I)-A(IC+I))
      C(JB+J)=((A(IA+I)-A(ID+I))+0.5*(A(IB+I)-A(IC+I)))
     *    -(SIN60*(B(IB+I)+B(IC+I)))
      C(JF+J)=((A(IA+I)-A(ID+I))+0.5*(A(IB+I)-A(IC+I)))
     *    +(SIN60*(B(IB+I)+B(IC+I)))
      C(JC+J)=((A(IA+I)+A(ID+I))-0.5*(A(IB+I)+A(IC+I)))
     *    -(SIN60*(B(IB+I)-B(IC+I)))
      C(JE+J)=((A(IA+I)+A(ID+I))-0.5*(A(IB+I)+A(IC+I)))
     *    +(SIN60*(B(IB+I)-B(IC+I)))
      I=I+INC3
      J=J+INC4
  610 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  620 CONTINUE
      IA=IA+IINK
      IINK=2*IINK
      IB=IB+IINK
      IC=IC+IINK
      ID=ID-IINK
      IE=IE-IINK
      IF=IF-IINK
      JBASE=JBASE+JUMP
      JUMP=2*JUMP+JINK
      IF (IC.EQ.ID) GO TO 660
      DO 650 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      KD=KC+KB
      KE=KD+KB
      KF=KE+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      C4=TRIGS(KE+1)
      S4=TRIGS(KE+2)
      C5=TRIGS(KF+1)
      S5=TRIGS(KF+2)
      IBASE=0
      DO 640 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 630 IJK=1,LOT
C
      A11(IJK)= (A(IE+I)+A(IB+I))+(A(IC+I)+A(IF+I))
      A20(IJK)=(A(IA+I)+A(ID+I))-0.5*A11(IJK)
      A21(IJK)=SIN60*((A(IE+I)+A(IB+I))-(A(IC+I)+A(IF+I)))
      B11(IJK)= (B(IB+I)-B(IE+I))+(B(IC+I)-B(IF+I))
      B20(IJK)=(B(IA+I)-B(ID+I))-0.5*B11(IJK)
      B21(IJK)=SIN60*((B(IB+I)-B(IE+I))-(B(IC+I)-B(IF+I)))
C
      C(JA+J)=(A(IA+I)+A(ID+I))+A11(IJK)
      D(JA+J)=(B(IA+I)-B(ID+I))+B11(IJK)
      C(JC+J)=C2*(A20(IJK)-B21(IJK))-S2*(B20(IJK)+A21(IJK))
      D(JC+J)=S2*(A20(IJK)-B21(IJK))+C2*(B20(IJK)+A21(IJK))
      C(JE+J)=C4*(A20(IJK)+B21(IJK))-S4*(B20(IJK)-A21(IJK))
      D(JE+J)=S4*(A20(IJK)+B21(IJK))+C4*(B20(IJK)-A21(IJK))
C
      A11(IJK)=(A(IE+I)-A(IB+I))+(A(IC+I)-A(IF+I))
      B11(IJK)=(B(IE+I)+B(IB+I))-(B(IC+I)+B(IF+I))
      A20(IJK)=(A(IA+I)-A(ID+I))-0.5*A11(IJK)
      A21(IJK)=SIN60*((A(IE+I)-A(IB+I))-(A(IC+I)-A(IF+I)))
      B20(IJK)=(B(IA+I)+B(ID+I))+0.5*B11(IJK)
      B21(IJK)=SIN60*((B(IE+I)+B(IB+I))+(B(IC+I)+B(IF+I)))
C
      C(JD+J)=
     *  C3*((A(IA+I)-A(ID+I))+A11(IJK))-S3*((B(IA+I)+B(ID+I))-B11(IJK))
      D(JD+J)=
     *  S3*((A(IA+I)-A(ID+I))+A11(IJK))+C3*((B(IA+I)+B(ID+I))-B11(IJK))
      C(JB+J)=C1*(A20(IJK)-B21(IJK))-S1*(B20(IJK)-A21(IJK))
      D(JB+J)=S1*(A20(IJK)-B21(IJK))+C1*(B20(IJK)-A21(IJK))
      C(JF+J)=C5*(A20(IJK)+B21(IJK))-S5*(B20(IJK)+A21(IJK))
      D(JF+J)=S5*(A20(IJK)+B21(IJK))+C5*(B20(IJK)+A21(IJK))
C
      I=I+INC3
      J=J+INC4
  630 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  640 CONTINUE
      IA=IA+IINK
      IB=IB+IINK
      IC=IC+IINK
      ID=ID-IINK
      IE=IE-IINK
      IF=IF-IINK
      JBASE=JBASE+JUMP
  650 CONTINUE
      IF (IC.GT.ID) GO TO 900
  660 CONTINUE
      IBASE=0
      DO 680 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 670 IJK=1,LOT
      C(JA+J)=A(IB+I)+(A(IA+I)+A(IC+I))
      C(JD+J)=B(IB+I)-(B(IA+I)+B(IC+I))
      C(JB+J)=(SIN60*(A(IA+I)-A(IC+I)))-(0.5*(B(IA+I)+B(IC+I))+B(IB+I))
      C(JF+J)=-(SIN60*(A(IA+I)-A(IC+I)))-(0.5*(B(IA+I)+B(IC+I))+B(IB+I))
      C(JC+J)=SIN60*(B(IC+I)-B(IA+I))+(0.5*(A(IA+I)+A(IC+I))-A(IB+I))
      C(JE+J)=SIN60*(B(IC+I)-B(IA+I))-(0.5*(A(IA+I)+A(IC+I))-A(IB+I))
      I=I+INC3
      J=J+INC4
  670 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  680 CONTINUE
      GO TO 900
C
  690 CONTINUE
      SSIN60=2.0*SIN60
      DO 694 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 692 IJK=1,LOT
      C(JA+J)=(2.0*(A(IA+I)+A(ID+I)))+(2.0*(A(IB+I)+A(IC+I)))
      C(JD+J)=(2.0*(A(IA+I)-A(ID+I)))-(2.0*(A(IB+I)-A(IC+I)))
      C(JB+J)=(2.0*(A(IA+I)-A(ID+I))+(A(IB+I)-A(IC+I)))
     *    -(SSIN60*(B(IB+I)+B(IC+I)))
      C(JF+J)=(2.0*(A(IA+I)-A(ID+I))+(A(IB+I)-A(IC+I)))
     *    +(SSIN60*(B(IB+I)+B(IC+I)))
      C(JC+J)=(2.0*(A(IA+I)+A(ID+I))-(A(IB+I)+A(IC+I)))
     *    -(SSIN60*(B(IB+I)-B(IC+I)))
      C(JE+J)=(2.0*(A(IA+I)+A(ID+I))-(A(IB+I)+A(IC+I)))
     *    +(SSIN60*(B(IB+I)-B(IC+I)))
      I=I+INC3
      J=J+INC4
  692 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  694 CONTINUE
      GO TO 900
C
C     CODING FOR FACTOR 8
C     -------------------
  800 CONTINUE
      IBAD=3
      IF (LA.NE.M) GO TO 910
      IA=1
      IB=IA+LA*INC1
      IC=IB+2*LA*INC1
      ID=IC+2*LA*INC1
      IE=ID+2*LA*INC1
      JA=1
      JB=JA+JINK
      JC=JB+JINK
      JD=JC+JINK
      JE=JD+JINK
      JF=JE+JINK
      JG=JF+JINK
      JH=JG+JINK
      SSIN45=SQRT(2.0)
C
      DO 820 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 810 IJK=1,LOT
      C(JA+J)=2.0*(((A(IA+I)+A(IE+I))+A(IC+I))+(A(IB+I)+A(ID+I)))
      C(JE+J)=2.0*(((A(IA+I)+A(IE+I))+A(IC+I))-(A(IB+I)+A(ID+I)))
      C(JC+J)=2.0*(((A(IA+I)+A(IE+I))-A(IC+I))-(B(IB+I)-B(ID+I)))
      C(JG+J)=2.0*(((A(IA+I)+A(IE+I))-A(IC+I))+(B(IB+I)-B(ID+I)))
      C(JB+J)=2.0*((A(IA+I)-A(IE+I))-B(IC+I))
     *    +SSIN45*((A(IB+I)-A(ID+I))-(B(IB+I)+B(ID+I)))
      C(JF+J)=2.0*((A(IA+I)-A(IE+I))-B(IC+I))
     *    -SSIN45*((A(IB+I)-A(ID+I))-(B(IB+I)+B(ID+I)))
      C(JD+J)=2.0*((A(IA+I)-A(IE+I))+B(IC+I))
     *    -SSIN45*((A(IB+I)-A(ID+I))+(B(IB+I)+B(ID+I)))
      C(JH+J)=2.0*((A(IA+I)-A(IE+I))+B(IC+I))
     *    +SSIN45*((A(IB+I)-A(ID+I))+(B(IB+I)+B(ID+I)))
      I=I+INC3
      J=J+INC4
  810 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  820 CONTINUE
C
C     RETURN
C     ------
  900 CONTINUE
      IBAD=0
  910 CONTINUE
      IERR=IBAD
      RETURN
      END
*DECK QPASSM
C     SUBROUTINE 'QPASSM' - PERFORMS ONE PASS THROUGH DATA AS PART
C     OF MULTIPLE REAL FFT (FOURIER ANALYSIS) ROUTINE
C
C     A IS FIRST REAL INPUT VECTOR
C         EQUIVALENCE B(1) WITH A(IFAC*LA*INC1+1)
C     C IS FIRST REAL OUTPUT VECTOR
C         EQUIVALENCE D(1) WITH C(LA*INC2+1)
C     TRIGS IS A PRECALCULATED LIST OF SINES & COSINES
C     INC1 IS THE ADDRESSING INCREMENT FOR A
C     INC2 IS THE ADDRESSING INCREMENT FOR C
C     INC3 IS THE INCREMENT BETWEEN INPUT VECTORS A
C     INC4 IS THE INCREMENT BETWEEN OUTPUT VECTORS C
C     LOT IS THE NUMBER OF VECTORS
C     N IS THE LENGTH OF THE VECTORS
C     IFAC IS THE CURRENT FACTOR OF N
C     LA = N/(PRODUCT OF FACTORS USED SO FAR)
C     IERR IS AN ERROR INDICATOR:
C              0 - PASS COMPLETED WITHOUT ERROR
C              1 - LOT GREATER THAN 64
C              2 - IFAC NOT CATERED FOR
C              3 - IFAC ONLY CATERED FOR IF LA=N/IFAC
C
C-----------------------------------------------------------------------
C
      SUBROUTINE QPASSM(A,B,C,D,TRIGS,INC1,INC2,INC3,INC4,LOT,N,IFAC,
     *    LA,IERR)
      DIMENSION A(N),B(N),C(N),D(N),TRIGS(N)
C
      DATA SIN36/0.587785252292473/,SIN72/0.951056516295154/,
     *    QRT5/0.559016994374947/,SIN60/0.866025403784437/
C
      M=N/IFAC
      IINK=LA*INC1
      JINK=LA*INC2
      IJUMP=(IFAC-1)*IINK
      KSTOP=(N-IFAC)/(2*IFAC)
C
      IBAD=1
      IF (LOT.GT.64) GO TO 910
      IBASE=0
      JBASE=0
      IGO=IFAC-1
      IF (IGO.EQ.7) IGO=6
      IBAD=2
      IF (IGO.LT.1.OR.IGO.GT.6) GO TO 910
      GO TO (200,300,400,500,600,800),IGO
C
C     CODING FOR FACTOR 2
C     -------------------
  200 CONTINUE
      IA=1
      IB=IA+IINK
      JA=1
      JB=JA+(2*M-LA)*INC2
C
      IF (LA.EQ.M) GO TO 290
C
      DO 220 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 210 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      C(JB+J)=A(IA+I)-A(IB+I)
      I=I+INC3
      J=J+INC4
  210 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  220 CONTINUE
      JA=JA+JINK
      JINK=2*JINK
      JB=JB-JINK
      IBASE=IBASE+IJUMP
      IJUMP=2*IJUMP+IINK
      IF (JA.EQ.JB) GO TO 260
      DO 250 K=LA,KSTOP,LA
      KB=K+K
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      JBASE=0
      DO 240 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 230 IJK=1,LOT
      C(JA+J)=A(IA+I)+(C1*A(IB+I)+S1*B(IB+I))
      C(JB+J)=A(IA+I)-(C1*A(IB+I)+S1*B(IB+I))
      D(JA+J)=(C1*B(IB+I)-S1*A(IB+I))+B(IA+I)
      D(JB+J)=(C1*B(IB+I)-S1*A(IB+I))-B(IA+I)
      I=I+INC3
      J=J+INC4
  230 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  240 CONTINUE
      IBASE=IBASE+IJUMP
      JA=JA+JINK
      JB=JB-JINK
  250 CONTINUE
      IF (JA.GT.JB) GO TO 900
  260 CONTINUE
      JBASE=0
      DO 280 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 270 IJK=1,LOT
      C(JA+J)=A(IA+I)
      D(JA+J)=-A(IB+I)
      I=I+INC3
      J=J+INC4
  270 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  280 CONTINUE
      GO TO 900
C
  290 CONTINUE
      Z=1.0/FLOAT(N)
      DO 294 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 292 IJK=1,LOT
      C(JA+J)=Z*(A(IA+I)+A(IB+I))
      C(JB+J)=Z*(A(IA+I)-A(IB+I))
      I=I+INC3
      J=J+INC4
  292 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  294 CONTINUE
      GO TO 900
C
C     CODING FOR FACTOR 3
C     -------------------
  300 CONTINUE
      IA=1
      IB=IA+IINK
      IC=IB+IINK
      JA=1
      JB=JA+(2*M-LA)*INC2
      JC=JB
C
      IF (LA.EQ.M) GO TO 390
C
      DO 320 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 310 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IC+I))
      C(JB+J)=A(IA+I)-0.5*(A(IB+I)+A(IC+I))
      D(JB+J)=SIN60*(A(IC+I)-A(IB+I))
      I=I+INC3
      J=J+INC4
  310 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  320 CONTINUE
      JA=JA+JINK
      JINK=2*JINK
      JB=JB+JINK
      JC=JC-JINK
      IBASE=IBASE+IJUMP
      IJUMP=2*IJUMP+IINK
      IF (JA.EQ.JC) GO TO 360
      DO 350 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      JBASE=0
      DO 340 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 330 IJK=1,LOT
      A1=(C1*A(IB+I)+S1*B(IB+I))+(C2*A(IC+I)+S2*B(IC+I))
      B1=(C1*B(IB+I)-S1*A(IB+I))+(C2*B(IC+I)-S2*A(IC+I))
      A2=A(IA+I)-0.5*A1
      B2=B(IA+I)-0.5*B1
      A3=SIN60*((C1*A(IB+I)+S1*B(IB+I))-(C2*A(IC+I)+S2*B(IC+I)))
      B3=SIN60*((C1*B(IB+I)-S1*A(IB+I))-(C2*B(IC+I)-S2*A(IC+I)))
      C(JA+J)=A(IA+I)+A1
      D(JA+J)=B(IA+I)+B1
      C(JB+J)=A2+B3
      D(JB+J)=B2-A3
      C(JC+J)=A2-B3
      D(JC+J)=-(B2+A3)
      I=I+INC3
      J=J+INC4
  330 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  340 CONTINUE
      IBASE=IBASE+IJUMP
      JA=JA+JINK
      JB=JB+JINK
      JC=JC-JINK
  350 CONTINUE
      IF (JA.GT.JC) GO TO 900
  360 CONTINUE
      JBASE=0
      DO 380 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 370 IJK=1,LOT
      C(JA+J)=A(IA+I)+0.5*(A(IB+I)-A(IC+I))
      D(JA+J)=-SIN60*(A(IB+I)+A(IC+I))
      C(JB+J)=A(IA+I)-(A(IB+I)-A(IC+I))
      I=I+INC3
      J=J+INC4
  370 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  380 CONTINUE
      GO TO 900
C
  390 CONTINUE
      Z=1.0/FLOAT(N)
      ZSIN60=Z*SIN60
      DO 394 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 392 IJK=1,LOT
      C(JA+J)=Z*(A(IA+I)+(A(IB+I)+A(IC+I)))
      C(JB+J)=Z*(A(IA+I)-0.5*(A(IB+I)+A(IC+I)))
      D(JB+J)=ZSIN60*(A(IC+I)-A(IB+I))
      I=I+INC3
      J=J+INC4
  392 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  394 CONTINUE
      GO TO 900
C
C     CODING FOR FACTOR 4
C     -------------------
  400 CONTINUE
      IA=1
      IB=IA+IINK
      IC=IB+IINK
      ID=IC+IINK
      JA=1
      JB=JA+(2*M-LA)*INC2
      JC=JB+2*M*INC2
      JD=JB
C
      IF (LA.EQ.M) GO TO 490
C
      DO 420 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 410 IJK=1,LOT
      C(JA+J)=(A(IA+I)+A(IC+I))+(A(IB+I)+A(ID+I))
      C(JC+J)=(A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I))
      C(JB+J)=A(IA+I)-A(IC+I)
      D(JB+J)=A(ID+I)-A(IB+I)
      I=I+INC3
      J=J+INC4
  410 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  420 CONTINUE
      JA=JA+JINK
      JINK=2*JINK
      JB=JB+JINK
      JC=JC-JINK
      JD=JD-JINK
      IBASE=IBASE+IJUMP
      IJUMP=2*IJUMP+IINK
      IF (JB.EQ.JC) GO TO 460
      DO 450 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      KD=KC+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      JBASE=0
      DO 440 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 430 IJK=1,LOT
      A0=A(IA+I)+(C2*A(IC+I)+S2*B(IC+I))
      A2=A(IA+I)-(C2*A(IC+I)+S2*B(IC+I))
      A1=(C1*A(IB+I)+S1*B(IB+I))+(C3*A(ID+I)+S3*B(ID+I))
      A3=(C1*A(IB+I)+S1*B(IB+I))-(C3*A(ID+I)+S3*B(ID+I))
      B0=B(IA+I)+(C2*B(IC+I)-S2*A(IC+I))
      B2=B(IA+I)-(C2*B(IC+I)-S2*A(IC+I))
      B1=(C1*B(IB+I)-S1*A(IB+I))+(C3*B(ID+I)-S3*A(ID+I))
      B3=(C1*B(IB+I)-S1*A(IB+I))-(C3*B(ID+I)-S3*A(ID+I))
      C(JA+J)=A0+A1
      C(JC+J)=A0-A1
      D(JA+J)=B0+B1
      D(JC+J)=B1-B0
      C(JB+J)=A2+B3
      C(JD+J)=A2-B3
      D(JB+J)=B2-A3
      D(JD+J)=-(B2+A3)
      I=I+INC3
      J=J+INC4
  430 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  440 CONTINUE
      IBASE=IBASE+IJUMP
      JA=JA+JINK
      JB=JB+JINK
      JC=JC-JINK
      JD=JD-JINK
  450 CONTINUE
      IF (JB.GT.JC) GO TO 900
  460 CONTINUE
      SIN45=SQRT(0.5)
      JBASE=0
      DO 480 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 470 IJK=1,LOT
      C(JA+J)=A(IA+I)+SIN45*(A(IB+I)-A(ID+I))
      C(JB+J)=A(IA+I)-SIN45*(A(IB+I)-A(ID+I))
      D(JA+J)=-A(IC+I)-SIN45*(A(IB+I)+A(ID+I))
      D(JB+J)=A(IC+I)-SIN45*(A(IB+I)+A(ID+I))
      I=I+INC3
      J=J+INC4
  470 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  480 CONTINUE
      GO TO 900
C
  490 CONTINUE
      Z=1.0/FLOAT(N)
      DO 494 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 492 IJK=1,LOT
      C(JA+J)=Z*((A(IA+I)+A(IC+I))+(A(IB+I)+A(ID+I)))
      C(JC+J)=Z*((A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I)))
      C(JB+J)=Z*(A(IA+I)-A(IC+I))
      D(JB+J)=Z*(A(ID+I)-A(IB+I))
      I=I+INC3
      J=J+INC4
  492 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  494 CONTINUE
      GO TO 900
C
C     CODING FOR FACTOR 5
C     -------------------
  500 CONTINUE
      IA=1
      IB=IA+IINK
      IC=IB+IINK
      ID=IC+IINK
      IE=ID+IINK
      JA=1
      JB=JA+(2*M-LA)*INC2
      JC=JB+2*M*INC2
      JD=JC
      JE=JB
C
      IF (LA.EQ.M) GO TO 590
C
      DO 520 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 510 IJK=1,LOT
      A1=A(IB+I)+A(IE+I)
      A3=A(IB+I)-A(IE+I)
      A2=A(IC+I)+A(ID+I)
      A4=A(IC+I)-A(ID+I)
      A5=A(IA+I)-0.25*(A1+A2)
      A6=QRT5*(A1-A2)
      C(JA+J)=A(IA+I)+(A1+A2)
      C(JB+J)=A5+A6
      C(JC+J)=A5-A6
      D(JB+J)=-SIN72*A3-SIN36*A4
      D(JC+J)=-SIN36*A3+SIN72*A4
      I=I+INC3
      J=J+INC4
  510 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  520 CONTINUE
      JA=JA+JINK
      JINK=2*JINK
      JB=JB+JINK
      JC=JC+JINK
      JD=JD-JINK
      JE=JE-JINK
      IBASE=IBASE+IJUMP
      IJUMP=2*IJUMP+IINK
      IF (JB.EQ.JD) GO TO 560
      DO 550 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      KD=KC+KB
      KE=KD+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      C4=TRIGS(KE+1)
      S4=TRIGS(KE+2)
      JBASE=0
      DO 540 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 530 IJK=1,LOT
      A1=(C1*A(IB+I)+S1*B(IB+I))+(C4*A(IE+I)+S4*B(IE+I))
      A3=(C1*A(IB+I)+S1*B(IB+I))-(C4*A(IE+I)+S4*B(IE+I))
      A2=(C2*A(IC+I)+S2*B(IC+I))+(C3*A(ID+I)+S3*B(ID+I))
      A4=(C2*A(IC+I)+S2*B(IC+I))-(C3*A(ID+I)+S3*B(ID+I))
      B1=(C1*B(IB+I)-S1*A(IB+I))+(C4*B(IE+I)-S4*A(IE+I))
      B3=(C1*B(IB+I)-S1*A(IB+I))-(C4*B(IE+I)-S4*A(IE+I))
      B2=(C2*B(IC+I)-S2*A(IC+I))+(C3*B(ID+I)-S3*A(ID+I))
      B4=(C2*B(IC+I)-S2*A(IC+I))-(C3*B(ID+I)-S3*A(ID+I))
      A5=A(IA+I)-0.25*(A1+A2)
      A6=QRT5*(A1-A2)
      B5=B(IA+I)-0.25*(B1+B2)
      B6=QRT5*(B1-B2)
      A10=A5+A6
      A20=A5-A6
      B10=B5+B6
      B20=B5-B6
      A11=SIN72*B3+SIN36*B4
      A21=SIN36*B3-SIN72*B4
      B11=SIN72*A3+SIN36*A4
      B21=SIN36*A3-SIN72*A4
      C(JA+J)=A(IA+I)+(A1+A2)
      C(JB+J)=A10+A11
      C(JE+J)=A10-A11
      C(JC+J)=A20+A21
      C(JD+J)=A20-A21
      D(JA+J)=B(IA+I)+(B1+B2)
      D(JB+J)=B10-B11
      D(JE+J)=-(B10+B11)
      D(JC+J)=B20-B21
      D(JD+J)=-(B20+B21)
      I=I+INC3
      J=J+INC4
  530 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  540 CONTINUE
      IBASE=IBASE+IJUMP
      JA=JA+JINK
      JB=JB+JINK
      JC=JC+JINK
      JD=JD-JINK
      JE=JE-JINK
  550 CONTINUE
      IF (JB.GT.JD) GO TO 900
  560 CONTINUE
      JBASE=0
      DO 580 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 570 IJK=1,LOT
      A1=A(IB+I)+A(IE+I)
      A3=A(IB+I)-A(IE+I)
      A2=A(IC+I)+A(ID+I)
      A4=A(IC+I)-A(ID+I)
      A5=A(IA+I)+0.25*(A3-A4)
      A6=QRT5*(A3+A4)
      C(JA+J)=A5+A6
      C(JB+J)=A5-A6
      C(JC+J)=A(IA+I)-(A3-A4)
      D(JA+J)=-SIN36*A1-SIN72*A2
      D(JB+J)=-SIN72*A1+SIN36*A2
      I=I+INC3
      J=J+INC4
  570 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  580 CONTINUE
      GO TO 900
C
  590 CONTINUE
      Z=1.0/FLOAT(N)
      ZQRT5=Z*QRT5
      ZSIN36=Z*SIN36
      ZSIN72=Z*SIN72
      DO 594 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 592 IJK=1,LOT
      A1=A(IB+I)+A(IE+I)
      A3=A(IB+I)-A(IE+I)
      A2=A(IC+I)+A(ID+I)
      A4=A(IC+I)-A(ID+I)
      A5=Z*(A(IA+I)-0.25*(A1+A2))
      A6=ZQRT5*(A1-A2)
      C(JA+J)=Z*(A(IA+I)+(A1+A2))
      C(JB+J)=A5+A6
      C(JC+J)=A5-A6
      D(JB+J)=-ZSIN72*A3-ZSIN36*A4
      D(JC+J)=-ZSIN36*A3+ZSIN72*A4
      I=I+INC3
      J=J+INC4
  592 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  594 CONTINUE
      GO TO 900
C
C     CODING FOR FACTOR 6
C     -------------------
  600 CONTINUE
      IA=1
      IB=IA+IINK
      IC=IB+IINK
      ID=IC+IINK
      IE=ID+IINK
      IF=IE+IINK
      JA=1
      JB=JA+(2*M-LA)*INC2
      JC=JB+2*M*INC2
      JD=JC+2*M*INC2
      JE=JC
      JF=JB
C
      IF (LA.EQ.M) GO TO 690
C
      DO 620 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 610 IJK=1,LOT
      A11=(A(IC+I)+A(IF+I))+(A(IB+I)+A(IE+I))
      C(JA+J)=(A(IA+I)+A(ID+I))+A11
      C(JC+J)=(A(IA+I)+A(ID+I)-0.5*A11)
      D(JC+J)=SIN60*((A(IC+I)+A(IF+I))-(A(IB+I)+A(IE+I)))
      A11=(A(IC+I)-A(IF+I))+(A(IE+I)-A(IB+I))
      C(JB+J)=(A(IA+I)-A(ID+I))-0.5*A11
      D(JB+J)=SIN60*((A(IE+I)-A(IB+I))-(A(IC+I)-A(IF+I)))
      C(JD+J)=(A(IA+I)-A(ID+I))+A11
      I=I+INC3
      J=J+INC4
  610 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  620 CONTINUE
      JA=JA+JINK
      JINK=2*JINK
      JB=JB+JINK
      JC=JC+JINK
      JD=JD-JINK
      JE=JE-JINK
      JF=JF-JINK
      IBASE=IBASE+IJUMP
      IJUMP=2*IJUMP+IINK
      IF (JC.EQ.JD) GO TO 660
      DO 650 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      KD=KC+KB
      KE=KD+KB
      KF=KE+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      C4=TRIGS(KE+1)
      S4=TRIGS(KE+2)
      C5=TRIGS(KF+1)
      S5=TRIGS(KF+2)
      JBASE=0
      DO 640 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 630 IJK=1,LOT
      A1=C1*A(IB+I)+S1*B(IB+I)
      B1=C1*B(IB+I)-S1*A(IB+I)
      A2=C2*A(IC+I)+S2*B(IC+I)
      B2=C2*B(IC+I)-S2*A(IC+I)
      A3=C3*A(ID+I)+S3*B(ID+I)
      B3=C3*B(ID+I)-S3*A(ID+I)
      A4=C4*A(IE+I)+S4*B(IE+I)
      B4=C4*B(IE+I)-S4*A(IE+I)
      A5=C5*A(IF+I)+S5*B(IF+I)
      B5=C5*B(IF+I)-S5*A(IF+I)
      A11=(A2+A5)+(A1+A4)
      A20=(A(IA+I)+A3)-0.5*A11
      A21=SIN60*((A2+A5)-(A1+A4))
      B11=(B2+B5)+(B1+B4)
      B20=(B(IA+I)+B3)-0.5*B11
      B21=SIN60*((B2+B5)-(B1+B4))
      C(JA+J)=(A(IA+I)+A3)+A11
      D(JA+J)=(B(IA+I)+B3)+B11
      C(JC+J)=A20-B21
      D(JC+J)=A21+B20
      C(JE+J)=A20+B21
      D(JE+J)=A21-B20
      A11=(A2-A5)+(A4-A1)
      A20=(A(IA+I)-A3)-0.5*A11
      A21=SIN60*((A4-A1)-(A2-A5))
      B11=(B5-B2)-(B4-B1)
      B20=(B3-B(IA+I))-0.5*B11
      B21=SIN60*((B5-B2)+(B4-B1))
      C(JB+J)=A20-B21
      D(JB+J)=A21-B20
      C(JD+J)=A11+(A(IA+I)-A3)
      D(JD+J)=B11+(B3-B(IA+I))
      C(JF+J)=A20+B21
      D(JF+J)=A21+B20
      I=I+INC3
      J=J+INC4
  630 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  640 CONTINUE
      IBASE=IBASE+IJUMP
      JA=JA+JINK
      JB=JB+JINK
      JC=JC+JINK
      JD=JD-JINK
      JE=JE-JINK
      JF=JF-JINK
  650 CONTINUE
      IF (JC.GT.JD) GO TO 900
  660 CONTINUE
      JBASE=0
      DO 680 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 670 IJK=1,LOT
      C(JA+J)=(A(IA+I)+0.5*(A(IC+I)-A(IE+I)))+ SIN60*(A(IB+I)-A(IF+I))
      D(JA+J)=-(A(ID+I)+0.5*(A(IB+I)+A(IF+I)))-SIN60*(A(IC+I)+A(IE+I))
      C(JB+J)=A(IA+I)-(A(IC+I)-A(IE+I))
      D(JB+J)=A(ID+I)-(A(IB+I)+A(IF+I))
      C(JC+J)=(A(IA+I)+0.5*(A(IC+I)-A(IE+I)))-SIN60*(A(IB+I)-A(IF+I))
      D(JC+J)=-(A(ID+I)+0.5*(A(IB+I)+A(IF+I)))+SIN60*(A(IC+I)+A(IE+I))
      I=I+INC3
      J=J+INC4
  670 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  680 CONTINUE
      GO TO 900
C
  690 CONTINUE
      Z=1.0/FLOAT(N)
      ZSIN60=Z*SIN60
      DO 694 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 692 IJK=1,LOT
      A11=(A(IC+I)+A(IF+I))+(A(IB+I)+A(IE+I))
      C(JA+J)=Z*((A(IA+I)+A(ID+I))+A11)
      C(JC+J)=Z*((A(IA+I)+A(ID+I))-0.5*A11)
      D(JC+J)=ZSIN60*((A(IC+I)+A(IF+I))-(A(IB+I)+A(IE+I)))
      A11=(A(IC+I)-A(IF+I))+(A(IE+I)-A(IB+I))
      C(JB+J)=Z*((A(IA+I)-A(ID+I))-0.5*A11)
      D(JB+J)=ZSIN60*((A(IE+I)-A(IB+I))-(A(IC+I)-A(IF+I)))
      C(JD+J)=Z*((A(IA+I)-A(ID+I))+A11)
      I=I+INC3
      J=J+INC4
  692 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  694 CONTINUE
      GO TO 900
C
C     CODING FOR FACTOR 8
C     -------------------
  800 CONTINUE
      IBAD=3
      IF (LA.NE.M) GO TO 910
      IA=1
      IB=IA+IINK
      IC=IB+IINK
      ID=IC+IINK
      IE=ID+IINK
      IF=IE+IINK
      IG=IF+IINK
      IH=IG+IINK
      JA=1
      JB=JA+LA*INC2
      JC=JB+2*M*INC2
      JD=JC+2*M*INC2
      JE=JD+2*M*INC2
      Z=1.0/FLOAT(N)
      ZSIN45=Z*SQRT(0.5)
C
      DO 820 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 810 IJK=1,LOT
      C(JA+J)=Z*(((A(IA+I)+A(IE+I))+(A(IC+I)+A(IG+I)))+
     *    ((A(ID+I)+A(IH+I))+(A(IB+I)+A(IF+I))))
      C(JE+J)=Z*(((A(IA+I)+A(IE+I))+(A(IC+I)+A(IG+I)))-
     *    ((A(ID+I)+A(IH+I))+(A(IB+I)+A(IF+I))))
      C(JC+J)=Z*((A(IA+I)+A(IE+I))-(A(IC+I)+A(IG+I)))
      D(JC+J)=Z*((A(ID+I)+A(IH+I))-(A(IB+I)+A(IF+I)))
      C(JB+J)=Z*(A(IA+I)-A(IE+I))
     *    +ZSIN45*((A(IH+I)-A(ID+I))-(A(IF+I)-A(IB+I)))
      C(JD+J)=Z*(A(IA+I)-A(IE+I))
     *    -ZSIN45*((A(IH+I)-A(ID+I))-(A(IF+I)-A(IB+I)))
      D(JB+J)=ZSIN45*((A(IH+I)-A(ID+I))+(A(IF+I)-A(IB+I)))
     *    +Z*(A(IG+I)-A(IC+I))
      D(JD+J)=ZSIN45*((A(IH+I)-A(ID+I))+(A(IF+I)-A(IB+I)))
     *    -Z*(A(IG+I)-A(IC+I))
      I=I+INC3
      J=J+INC4
  810 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  820 CONTINUE
C
C     RETURN
C     ------
  900 CONTINUE
      IBAD=0
  910 CONTINUE
      IERR=IBAD
      RETURN
      END
      SUBROUTINE GWTLT(SIT,WEIGHT,J,JG)
C
C     NORMALLY A ROUTINE IN SUBLIB BUT INCLUDED HERE FOR PORTABLITY
C
      PARAMETER(PI=3.14159265359)
      DOUBLE PRECISION ACC,SA,SB,SC,D1,D2,D4,BN,GG,X,AMM,AMN,
     1 EM,ANN,RE,A,DD,DLT,DTH
      ACC=1.0D-16
      SA=DSQRT(.5D0)
      SB=DSQRT(1.5D0)
      SC=DSQRT(1.D0/3.D0)
      D1=1.D0
      D2=2.D0
      D4=4.D0
      NNN=JG+JG
      BN=NNN
      GG=D2*BN+D1
      HH=8.*BN*BN
      K=0
      AJ=J
      TH=PI*(2.*AJ-.5)/GG
      DTH=TH+(COS(TH)/SIN(TH))/HH
      X=DCOS(DTH)
54    CONTINUE
      AMM=SA
      AMN=X*SB
      EM=SC
      ANN=D1
      DO 51 N=2,NNN
      ANN=ANN+D1
      RE=DSQRT(D4*ANN*ANN-D1)/ANN
      A=RE*(X*AMN-EM*AMM)
      AMM=AMN
      AMN=A
      EM=D1/RE
51    CONTINUE
      DD=GG*EM*AMM-X*ANN*A
      K=K+1
      DLT=(D1-X*X)*A/DD
      IF (DABS(DLT).LT.ACC) GOTO 52
      IF (K.GT.50) GOTO 53
      X=X-DLT
      GOTO 54
53    CONTINUE
      WRITE (6,105)
105   FORMAT(15H NO CONVERGENCE)
52    CONTINUE
      WEIGHT=GG*(D1-X*X)/(DD*DD)
      SIT=X
      RETURN
      END
CERN      F200      VERSION    05/03/68 QREIG       217                F
C
      SUBROUTINE QREIG(A,IDIM1,M,IDIM2,ROOTR,ROOTI)
      DIMENSION A(IDIM1,IDIM2),ROOTR(M),ROOTI(M)
      CALL HESSEN(A,IDIM1,M,IDIM2,ROOTR)
      N=M
   81 ZERO=0.0
      JJ=1
  177 XNN=0.0
      XN2=0.0
      AA=0.0
      B=0.0
      C=0.0
      DD=0.0
      R=0.0
      SIG=0.0
      ITER=0
   17 IF(N-2)83,14,12
   83 ROOTR(1)=A(1,1)
      ROOTI(1)=0.0
    1 RETURN
   14 JJ=-1
   12 X=(A(N-1,N-1)-A(N,N))**2
      S=4.0*A(N,N-1)*A(N-1,N)
      ITER=ITER+1
      IF(X)124,15,124
  124 IF(ABS(S/X).GT.1.0E-8)GO TO 15
   16 IF(ABS(A(N-1,N-1))-ABS(A(N,N)))32,32,31
   31 E=A(N-1,N-1)
      G=A(N,N)
      GO TO 33
   32 G=A(N-1,N-1)
      E=A(N,N)
   33 F=0.0
      H=0.0
      GO TO 24
   15 S=X+S
      X=A(N-1,N-1)+A(N,N)
      IF(S)18,19,19
   19 SQ=SQRT(S)
      F=0.0
      H=0.0
      IF(X)21,21,22
   21 E=(X-SQ)/2.0
      G=(X+SQ)/2.0
      GO TO 24
   22 G=(X-SQ)/2.0
      E=(X+SQ)/2.0
      GO TO 24
   18 F=SQRT(-S)/2.0
      E=X/2.0
      G=E
      H=-F
   24 IF(JJ)85,70,70
   70 D=1.0E-10*(ABS(G)+F)
      IF(ABS(A(N-1,N-2)).GT.D)GO TO 26
   85 ROOTR(N)=E
      ROOTI(N)=F
      ROOTR(N-1)=G
      ROOTI(N-1)=H
      N=N-2
      IF(JJ)1,177,177
   26 IF(ABS(A(N,N-1)).GT.1.0E-10*ABS(A(N,N)))GO TO 50
   87 ROOTR(N)=A(N,N)
      ROOTI(N)=0.0
      N=N-1
      GO TO 177
   50 IF(ABS(ABS(XNN/A(N,N-1))-1.0)-1.0E-6)63,63,62
   62 IF(ABS(ABS(XN2/A(N-1,N-2))-1.0)-1.0E-6)63,63,700
   63 VQ=ABS(A(N,N-1))-ABS(A(N-1,N-2))
   64 IF(VQ)87,87,85
  700 IF(ITER.GT.50)GO TO 63
  701 Z1=(E-AA)**2+(F-B)**2-0.25*(E*E+F*F)
      Z2=(G-C)**2+(H-DD)**2-0.25*(G*G+H*H)
      IF(Z1)51,51,52
   51 IF(Z2)53,53,54
   53 R=E*G-F*H
      SIG=E+G
      GO TO 60
   54 R=E*E
      SIG=E+E
      GO TO 60
   52 IF(Z2)55,55,601
   55 R=G*G
      SIG=G+G
      GO TO 60
  601 R=0.0
      SIG=0.0
   60 XNN=A(N,N-1)
      XN2=A(N-1,N-2)
      CALL QRT(A,N,R,SIG,D,IDIM1,IDIM2)
      AA=E
      B=F
      C=G
      DD=H
      GO TO 12
      END
C
      SUBROUTINE HESSEN(A,IDIM1,M,IDIM2,B)
      DIMENSION A(IDIM1,IDIM2)
      DIMENSION B(M)
      DOUBLE PRECISION SUM
      IF(M-2)30,30,32
   32 DO 40 LC=3,M
      N=M-LC+3
      N1=N-1
      N2=N-2
      NI=N1
      DIV=ABS(A(N,N-1))
      DO 2 J=1,N2
      IF(ABS(A(N,J))-DIV)2,2,1
    1 NI=J
      DIV=ABS(A(N,J))
    2 CONTINUE
      IF(DIV)3,40,3
    3 IF(NI-N1)4,7,4
    4 DO 5 J=1,N
      DIV=A(J,NI)
      A(J,NI)=A(J,N1)
    5 A(J,N1)=DIV
      DO 6 J=1,M
      DIV=A(NI,J)
      A(NI,J)=A(N1,J)
    6 A(N1,J)=DIV
    7 DO 26 K=1,N1
   26 B(K)=A(N,K)/A(N,N-1)
      DO 45 J=1,M
      SUM=0.0
      IF(J-N1)46,43,43
   46 IF(B(J))41,43,41
   41 A(N,J)=0.0
      DO 42 K=1,N1
      A(K,J)=A(K,J)-A(K,N1)*B(J)
   42 SUM=SUM+A(K,J)*B(K)
      GO TO 45
   43 DO 44 K=1,N1
   44 SUM=SUM+A(K,J)*B(K)
   45 A(N1,J)=SUM
   40 CONTINUE
   30 RETURN
      END
      SUBROUTINE QRT(A,N,R,SIG,D,IDIM1,IDIM2)
      DIMENSION PSI(3),G(3),A(IDIM1,IDIM2)
      N1=N-1
      IA=N-2
      IP=IA
      IF(N-3)101,10,60
   60 DO 12 J=3,N1
      J1=N-J
      IF(ABS(A(J1+1,J1))-D)10,10,11
   11 IF(ABS(A(J1+1,J1)*A(J1+2,J1+1)*(ABS(A(J1+1,J1+1)+A(J1+2,J1+2)
     1-SIG)+ABS(A(J1+3,J1+2)))/(A(J1+1,J1+1)*(A(J1+1,J1+1)-SIG)+A(J1+1,
     2J1+2)*A(J1+2,J1+1)+R))-D)10,10,12
   12 IP=J1
   10 DO 14 J=1,IP
      J1=IP-J+1
      IF(ABS(A(J1+1,J1))-D)13,13,14
   14 IQ=J1
   13 DO 100 I=IP,N1
      IF(I-IP)16,15,16
   15 G(1)=A(IP,IP)*(A(IP,IP)-SIG)+A(IP,IP+1)*A(IP+1,IP)+R
      G(2)=A(IP+1,IP)*(A(IP,IP)+A(IP+1,IP+1)-SIG)
      G(3)=A(IP+1,IP)*A(IP+2,IP+1)
      A(IP+2,IP)=0.0
      GO TO 19
   16 G(1)=A(I,I-1)
      G(2)=A(I+1,I-1)
      IF(I-IA)17,17,18
   17 G(3)=A(I+2,I-1)
      GO TO 19
   18 G(3)=0.0
   19 XK=SIGN(SQRT(G(1)**2+G(2)**2+G(3)**2),G(1))
   22 IF(XK)23,24,23
   23 AL=G(1)/XK+1.0
      PSI(1)=G(2)/(G(1)+XK)
      PSI(2)=G(3)/(G(1)+XK)
      GO TO 25
   24 AL=2.0
      PSI(1)=0.0
      PSI(2)=0.0
   25 IF(I-IQ)26,27,26
   26 IF(I-IP)29,28,29
   28 A(I,I-1)=-A(I,I-1)
      GO TO 27
   29 A(I,I-1)=-XK
   27 DO 30 J=I,N
      IF(I-IA)31,31,32
   31 C=PSI(2)*A(I+2,J)
      GO TO 33
   32 C=0.0
   33 E=AL*(A(I,J)+PSI(1)*A(I+1,J)+C)
      A(I,J)=A(I,J)-E
      A(I+1,J)=A(I+1,J)-PSI(1)*E
      IF(I-IA)34,34,30
   34 A(I+2,J)=A(I+2,J)-PSI(2)*E
   30 CONTINUE
      IF(I-IA)35,35,36
   35 L=I+2
      GO TO 37
   36 L=N
   37 DO 40 J=IQ,L
      IF(I-IA)38,38,39
   38 C=PSI(2)*A(J,I+2)
      GO TO 41
   39 C=0.0
   41 E=AL*(A(J,I)+PSI(1)*A(J,I+1)+C)
      A(J,I)=A(J,I)-E
      A(J,I+1)=A(J,I+1)-PSI(1)*E
      IF(I-IA)42,42,40
   42 A(J,I+2)=A(J,I+2)-PSI(2)*E
   40 CONTINUE
      IF(I-N+3)43,43,100
   43 E=AL*PSI(2)*A(I+3,I+2)
      A(I+3,I)=-E
      A(I+3,I+1)=-PSI(1)*E
      A(I+3,I+2)=A(I+3,I+2)-PSI(2)*E
  100 CONTINUE
  101 RETURN
      END
      subroutine copyf( irtc, iin, iout)
* Append source file to the end of the target file
* If the files do not exist then they are created
* If the files are opened then they are not repositioned on return
* The source file or target file or both may be empty
* The files must both be form='FORMATTED'
* The number of lines transferred is returned or -1 if source does not exist
* Raymond Saktreger 19 Mar 1992
      integer irtc, iin, iout
      integer LENBUF
      parameter( LENBUF=256 )
      character buffer*(LENBUF)
      logical ok, there, inop, outop
      integer ios, k, n
      integer lengch
      external lengch
*
      print*,' source=', iin
      inquire( unit=iin, exist=ok, opened=inop )
      print*,'exist,opened,number',ok, inop
      if( ok )then
	 if( .not. inop )then
	    open( iin )
	 endif
      else
	 irtc = -1
	 return
      endif
      print*,'target=',iout
      inquire( unit=iout, exist=there,opened=outop)
      print*,'exist,opened,number',there,outop
      if( there )then
	 if( .not. outop )then
	    open( iout )
	 endif
*      print*,'Skip to end of target'
	 k = 0
	 ios =0
	 do while( ios .eq. 0 )
	    read( iout, '(A)', iostat=ios )
	    k=k+1
*            print*,k, ios
	 enddo
	 backspace iout
      else
	 open( iout)
      endif
      irtc=0
      buffer = ' '
      read( iin, '(A)', iostat=ios )buffer
      n = lengch( buffer )
      k = 0
      do while ( ios .eq. 0 )
	 write( iout, '(A)' )buffer(:n)
	 k = k+1
*         print*,k, ios, n, buffer(:n)
	 buffer = ' '
	 read( iin, '(A)', iostat=ios )buffer
	 n = lengch( buffer )
      enddo
      if( .not. inop )close( iin )
      if( .not. outop )close( iout )
      end
*
      integer function lengch( string )
      character string*(*)
      integer j
      intrinsic len
      do j= len(string), 1, -1
	 if(  string(j:j) .ne. ' ' )then
	    lengch = j
	    return
	 endif
      enddo
      lengch = 0
      end
*

