      PROGRAM GETFORCE
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA,nm=51)

      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      COMPLEX az(IGB),bz(IGB),ad(IGB),bd(IGB),at(IGB),
     *      bt(IGB),ap(IGA),bp(IGA)


CCCC here is spec.dat is not that used in the R2gettrospec.f
CCCC it is from the tro.dat that is generated from R2gettrospec.f
CCCC tro.dat is changed to spectral formate by the program in R5test.sh

	OPEN(15,file='spec.dat',form='UNFORMATTED')
	OPEN(16,file='regCoeff-AO.dat',form='UNFORMATTED')
	OPEN(17,file='FORCEanom_RegAO_T',form='UNFORMATTED')
	read(15)Z,D,T,SP
	read(16)az,bz,ad,bd,at,bt,ap,bp

	 DO I=1,IGB
	 ZA(I)=Z(I)+bz(I)
	 DA(I)=D(I)+bd(I)
	 TA(I)=T(I)+bt(I)
	 END DO
	 DO I=1,IGA
	 SPA(I)=SP(I)+bp(I)
	 END DO


	write(17)ZA,DA,TA,SPA


      STOP
      END               
