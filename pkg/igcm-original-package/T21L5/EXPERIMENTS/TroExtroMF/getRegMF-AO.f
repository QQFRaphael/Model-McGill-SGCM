	PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA,nm=51)

      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)


	COMPLEX a1,b1,a2,a3,b2,b3
	COMPLEX Zt(IGB,nm),Dt(IGB,nm),Tt(IGB,nm)
        COMPLEX SPt(IGA,nm), z1(nm),z2(nm),z3(nm)
	COMPLEX az(IGB),bz(IGB),ad(IGB),bd(IGB),at(IGB),
     *      bt(IGB),ap(IGA),bp(IGA)
	real y1(nm)

	open(12,file='/zemo2/jiaxj/force_diag/NCEP/AOindex')
	do k=1,nm
         read(12,*)y1(k)
	end do
	call getstd(y1,nm,std)
	print*, std

	OPEN(14,FILE=
     & '/mnt/climate/data/loach/jiaxj/Data/SGCM/'//
     &'Input/T21L5/FORCE_51DJF.dat_crtd'
     & ,form='UNFORMATTED',status='old')
	OPEN(15,FILE='FORCEanom_RegAO',form='UNFORMATTED')

	do iy=1,nm
	 READ(14)Z,D,T,SP
	
	do i=1,IGB
	Zt(I,iy)=Z(I)
        Dt(I,iy)=D(I)
        Tt(I,iy)=T(I)
        ENDDO
        DO I=1,IGA
        SPt(I,iy)=SP(I)
        ENDDO
	end do

CC
	do I=1,IGB
	 do k=1,nm
	  z1(k)=Zt(I,k)
          z2(k)=Dt(I,k)
          z3(k)=Tt(I,k)
         end do
	 call regress(y1,z1,nm,a1,b1)
	 call regress(y1,z2,nm,a2,b2)
	 call regress(y1,z3,nm,a3,b3)

CC without b, it's anomaly, b is z average
         ZA(I)=a1*std
	 DA(I)=a2*std
	 TA(I)=a3*std
	 az(I)=a1
	 ad(I)=a2
	 at(I)=a3
	 bz(I)=b1
	 bd(I)=b2
	 bt(I)=b3
	end do

	DO I=1,IGA
         do k=1,nm
          z1(k)=SPt(I,k)
         end do
         call regress(y1,z1,nm,a1,b1)
	 SPA(I)=a1*std
	 ap(I)=a1
	 bp(I)=b1
        END DO
CC
        write(15)ZA,DA,TA,SPA
        stop
        end



#include "/zemo2/jiaxj/SUBROUTINES/getstd.f"
#include "/zemo2/jiaxj/SUBROUTINES/regress.f"

