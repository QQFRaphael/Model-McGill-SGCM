#include "/zemo2/jiaxj/force/DHEOF3/gausstogrid128.f"

	PARAMETER (MG=128,JGG=64,NL=5,nm=51)
        PARAMETER ( mmg=73,jg2=37)

      REAL*8 Z(MG,JGG,NL),D(MG,JGG,NL),T(MG,JGG,NL),SP(MG,JGG)
      REAL ZA(MG,JGG),DA(MG,JGG),TA(MG,JGG),SPA(MG,JGG)
      REAL A1(MG,JGG),A2(MG,JGG),A3(MG,JGG),A4(MG,JGG)
      REAL A5(MG,JGG)
      real fll(73,37),bll(73,37),st(73,37,nm),y1(nm),y2(nm)
        real pot1(mmg,jg2),pot2(mmg,jg2),t1(mmg,jg2,nm)
        real pott(mmg,jg2),pot(mmg,jg2),c(nm)



        open(12,file='/zemo2/jiaxj/force_diag/NCEP/AOindex'
     *       ,form='unformatted')
         read(12)c
        do k=1,nm
           y1(k)=c(k)
        end do


cc detrend
        call detrend(y1,nm,aa1,b1)
        do k=1,nm
          y1(k)=y1(k)-aa1*k-b1
        end do

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


	OPEN(14,FILE='DH.dat_crtd_grid',
     &	form='UNFORMATTED',status='old')

	DO K=1,nm
        READ(14)Z,D,T,SP
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
        DO I=1,MG
        DO J=1,JGG
         A1(i,j)=T(i,j,1)
         A2(i,j)=T(i,j,2)
         A3(i,j)=T(i,j,3)
         A4(i,j)=T(i,j,4)
         A5(i,j)=T(i,j,5)
        ENDDO
        ENDDO
        DO I=1,MG
        DO J=1,JGG
         ZA(I,J)=(A1(I,J)+A2(I,J)+A3(I,J)+A4(I,J)+A5(I,J))/5.
        ENDDO
        ENDDO

        call gautogrid(ZA,fll)
        do i=1,73
        do j=1,37
         t1(i,j,k)=fll(i,j)
        end do
        end do

	END DO

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCC
        do i=1,mmg
        do j=1,jg2
         pot(i,j)=0.
         pot1(i,j)=0.
         pot2(i,j)=0.
        end do
        end do





        n1=0
        n2=0

        do k=1,nm
        do i=1,mmg
        do j=1,jg2
         pot(i,j)=pot(i,j)+t1(i,j,k)
        end do
        end do

        if(y1(k).gt.1.0) then
         n1=n1+1
        do i=1,mmg
        do j=1,jg2
         pot1(i,j)=pot1(i,j)+t1(i,j,k)
        end do
        end do

        else if(y1(k).lt.-1.0) then
         n2=n2+1
        do i=1,mmg
        do j=1,jg2
         pot2(i,j)=pot2(i,j)+t1(i,j,k)
        end do
        end do
        end if
        end do



        do i=1,mmg
        do j=1,jg2
         pot(i,j)=pot(i,j)/real(nm)
         pot1(i,j)=pot1(i,j)/real(n1)
         pot2(i,j)=pot2(i,j)/real(n2)
        end do
        end do


        do i=1,mmg
        do j=1,jg2
c        pot1(i,j)=pot1(i,j)-pot(i,j)
         pot2(i,j)=pot2(i,j)-pot(i,j)
c        pott(i,j)=(pot1(i,j)-pot2(i,j))/2.
        end do
        end do

        write(16) pot2

	end



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



