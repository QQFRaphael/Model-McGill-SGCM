CCcccccccccccccccccccccccccccccccccccccccccccccccc
CC  Fourier Transform and reverse Transform to
CC  retain the information with 1<wavenumber<18 (10day-180day)
CC
	subroutine lfilter(x,fx,nn)
	dimension WKP(nn),WK(2,0:nn),x(nn),
     *     fx(nn)
	do 12 i=1,nn
12	wkp(i)=x(i)
	PI=3.1415926
	TOTAL=float(nn)

ccccccccccccccc Get the coefficients A_k and B_k.
        DO 103 K=1,18
          S1=0.0
          S2=0.0
        DO 104 I=1,nn
        S1=S1+WKP(I)*COS(FLOAT(K)*FLOAT(I-1)*PI*2.0/TOTAL)
104     S2=S2+WKP(I)*SIN(FLOAT(K)*FLOAT(I-1)*PI*2.0/TOTAL)
        WK(1,K)=2.*S1/TOTAL
	 if(k.eq.0)WK(1,K)=S1/TOTAL
        WK(2,K)=2.*S2/TOTAL
103	continue

CCCCCC get the low-frequent flow.
c   Reverse transform
	do 31 i=1,nn
	ss=0.
ccccc  Control the frequency!!!
	do 33 k=1,18
	ss=ss+WK(1,k)*COS(FLOAT(K)*FLOAT(I-1)*PI*2.0/TOTAL)+
     *     WK(2,k)*SIN(FLOAT(K)*FLOAT(I-1)*PI*2.0/TOTAL)
33	continue
	fx(i)=ss
31	continue
	return
	end
	
CCcccccccccccccccccccccccccccccccccccccccccccccccc
CC  Fourier Transform and reverse Transform to
CC  retain the information with 19<wavenumber<91 (2day-10day)
CC
        subroutine hfilter(x,fx,nn)
        dimension WKP(nn),WK(2,0:nn),x(nn),
     *     fx(nn)
        do 12 i=1,nn
12      wkp(i)=x(i)
        PI=3.1415926
        TOTAL=float(nn)
        DO 103 K=19,91
          S1=0.0
          S2=0.0
        DO 104 I=1,nn
        S1=S1+WKP(I)*COS(FLOAT(K)*FLOAT(I-1)*PI*2.0/TOTAL)
104     S2=S2+WKP(I)*SIN(FLOAT(K)*FLOAT(I-1)*PI*2.0/TOTAL)
        WK(1,K)=2.*S1/TOTAL
	if(k.eq.0)WK(1,K)=S1/TOTAL
        WK(2,K)=2.*S2/TOTAL
103	continue
c   Reverse transform
        do 31 i=1,nn
        ss=0.
        do 33 k=19,91
          ss=ss+WK(1,k)*COS(FLOAT(K)*FLOAT(I-1)*PI*2.0/TOTAL)+
     *     WK(2,k)*SIN(FLOAT(K)*FLOAT(I-1)*PI*2.0/TOTAL)
33	continue
        fx(i)=ss
31      continue
        return
        end

