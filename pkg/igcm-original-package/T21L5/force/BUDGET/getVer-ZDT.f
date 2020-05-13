	PARAMETER (MG=128,JGG=64,NL=5,nm=51)


      REAL*8 Z(MG,JGG,NL),D(MG,JGG,NL),T(MG,JGG,NL),SP(MG,JGG)
      REAL ZA(MG,JGG),DA(MG,JGG),TA(MG,JGG),SPA(MG,JGG)
      REAL A1(MG,JGG),A2(MG,JGG),A3(MG,JGG),A4(MG,JGG)
      REAL A5(MG,JGG)
      real fll(73,37),bll(73,37),st(73,37,nm),y1(nm),y2(nm)
      real pot(73,37),a

      open(15,file='/diskc/jiaxj/ncarplot/SVD/fort.31')

        do k=1,nm
         read(15,*) y1(k)
        end do
	write(30,*)y1
cc detrend
c       call detrend(y1,nm,a1,b1)
c       do k=1,nm
c         y1(k)=y1(k)-a1*k-b1
c       end do

         s=0.
        do k=1,nm
         s=s+y1(k)/float(nm)
        end do

         std=0.
        do k=1,nm
         std=std+(y1(k)-s)**2/50.
        end do
        std=sqrt(std)
	print*,std

CCCCCCCCCCCCCC


	OPEN(14,FILE='grid_ZD.dat',
     &	form='UNFORMATTED',status='old')

	DO K=1,nm
CC Initial to zero
      DO  I=1,MG
      DO  J=1,JGG	
        ZA(I,J)=0.
      ENDDO
      ENDDO

      DO  I=1,MG
      DO  J=1,JGG 
        A1(I,J)=0.
        A2(I,J)=0.
        A3(I,J)=0.
        A4(I,J)=0.
        A5(I,J)=0.
      ENDDO
      ENDDO	

CC
	 
        READ(14)Z,D,T,SP

	DO I=1,MG
	DO J=1,JGG
         A1(i,j)=T(i,j,1)
         A2(i,j)=T(i,j,2)
         A3(i,j)=T(i,j,3)
         A4(i,j)=T(i,j,4)
         A5(i,j)=T(i,j,5)
	ENDDO
	ENDDO


CC

        DO I=1,MG
        DO J=1,JGG
	 ZA(I,J)=(A1(I,J)+A2(I,J)+A3(I,J)+A4(I,J)+A5(I,J))/5.
	ENDDO
	ENDDO
	call gautogrid(ZA,fll)
        do i=1,73
        do j=1,37
         st(i,j,k)=fll(i,j)
        end do
        end do

	END DO

CCCCCCC
        do i=1,73
        do j=1,37
         do k=1,nm
          y2(k)=st(i,j,k)
         end do

         call regress(y1,y2,nm,a,b)
         pot(i,j)=a*std
        end do
        end do
        write(16)pot




        end


#include "/zemo2/jiaxj/force/DHEOF3/gausstogrid128.f"



CCCCCCCCCC
        subroutine detrend(y,nm,a,b)
        real x(nm),y(nm)
CC use linear function y=ax+b
        do i=1,nm
        x(i)=i
        end do
CC
        x1=0.
        y1=0.
        do i=1,nm
        x1=x1+x(i)/float(nm)
        y1=y1+y(i)/float(nm)
        end do

        xy=0.
        do i=1,nm
        xy=xy+(x(i)-x1)*(y(i)-y1)
        end do

        x2=0.
        do i=1,nm
        x2=x2+(x(i)-x1)**2
        end do

        a=xy/x2
        b=y1-a*x1

        return
        end



CCCCCCCCCCCCCC
        subroutine regress(x,y,nm,a,b)
        real x(nm),y(nm)
CC use linear function y=ax+b
CC
        x1=0.
        y1=0.
        do i=1,nm
        x1=x1+x(i)/float(nm)
        y1=y1+y(i)/float(nm)
        end do

        xy=0.
        do i=1,nm
        xy=xy+(x(i)-x1)*(y(i)-y1)
        end do

        x2=0.

        do i=1,nm
        x2=x2+(x(i)-x1)**2
        end do

        a=xy/x2
        b=y1-a*x1

        return
        end



