      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=128,JG=32,NWJ2=121        G100.1
     &         ,NCRAY=64,JGG=JG*NHEM)     


      real*8 Ze(MG,JGG,NL),De(MG,JGG,NL),Te(MG,JGG,NL),SPe(MG,JGG)
      real*8 Zt(MG,JGG,NL),Dt(MG,JGG,NL),Tt(MG,JGG,NL),SPt(MG,JGG)
      real*8 ZA(MG,JGG,NL),DA(MG,JGG,NL),TA(MG,JGG,NL),SPA(MG,JGG)
      real*8 ZC(MG,JGG,NL),DC(MG,JGG,NL),TC(MG,JGG,NL),SPC(MG,JGG)


	OPEN(16,file='Trend.dat_gauss',form='UNFORMATTED',status='old')
	OPEN(19,file='haiclim_gauss',form='UNFORMATTED',status='old')

	OPEN(17,file='TroTrend.dat_gauss',form='UNFORMATTED')
	OPEN(18,file='ExtTrend.dat_gauss',form='UNFORMATTED')

	READ(16)ZA,DA,TA,SPA
	READ(19)ZC,DC,TC,SPC

      DO K=1,NL	
      DO I=1,MG
      DO J=20,46
        Ze(I,J,K)=ZA(I,J,K)+ZC(I,J,K)		
        De(I,J,K)=DA(I,J,K)+DC(I,J,K)		
        Te(I,J,K)=TA(I,J,K)+TC(I,J,K)		
      ENDDO
      ENDDO
      ENDDO

      DO I=1,MG
      DO J=20,46
        SPe(I,J)=SPA(I,J)+SPC(I,J)            
      ENDDO
      ENDDO

CC

      DO K=1,NL
      DO I=1,MG
      DO J=1,19
        Ze(I,J,K)=ZC(I,J,K)
        De(I,J,K)=DC(I,J,K)
        Te(I,J,K)=TC(I,J,K)
      ENDDO
      ENDDO
      ENDDO

      DO I=1,MG
      DO J=1,19
        SPe(I,J)=SPC(I,J)
      ENDDO
      ENDDO



      DO K=1,NL
      DO I=1,MG
      DO J=47,64
        Ze(I,J,K)=ZC(I,J,K)
        De(I,J,K)=DC(I,J,K)
        Te(I,J,K)=TC(I,J,K)
      ENDDO
      ENDDO
      ENDDO

      DO I=1,MG
      DO J=47,64
        SPe(I,J)=SPC(I,J)
      ENDDO
      ENDDO


      WRITE(17)Ze,De,Te,SPe

CCCCCCCCC




      DO K=1,NL
      DO I=1,MG
      DO J=20,46
        Zt(I,J,K)=ZC(I,J,K)
        Dt(I,J,K)=DC(I,J,K)
        Tt(I,J,K)=TC(I,J,K)
      ENDDO
      ENDDO
      ENDDO

      DO I=1,MG
      DO J=20,46
        SPt(I,J)=SPC(I,J)
      ENDDO
      ENDDO

CC

      DO K=1,NL
      DO I=1,MG
      DO J=1,19
        Zt(I,J,K)=ZA(I,J,K)+ZC(I,J,K)
        Dt(I,J,K)=DA(I,J,K)+DC(I,J,K)
        Tt(I,J,K)=TA(I,J,K)+TC(I,J,K)
      ENDDO
      ENDDO
      ENDDO

      DO I=1,MG
      DO J=1,19
        SPt(I,J)=SPA(I,J)+SPC(I,J)
      ENDDO
      ENDDO



      DO K=1,NL
      DO I=1,MG
      DO J=47,64
        Zt(I,J,K)=ZA(I,J,K)+ZC(I,J,K)
        Dt(I,J,K)=DA(I,J,K)+DC(I,J,K)
        Tt(I,J,K)=TA(I,J,K)+TC(I,J,K)
      ENDDO
      ENDDO
      ENDDO

      DO I=1,MG
      DO J=47,64
        SPt(I,J)=SPA(I,J)+SPC(I,J)
      ENDDO
      ENDDO


      WRITE(18)Zt,Dt,Tt,SPt





	
        stop
        end




CCCCCCCCCCC





        subroutine regress(x,y,nm,a,b)
        real x(nm)
	COMPLEX y(nm),a,b,y1,xy
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
CC
        subroutine getstd(y,nm,r)
        real y(nm)
        s=0.
        do k=1,nm
        s=s+y(k)/float(nm)
        end do

        r=0.
        do k=1,nm
        r=r+(y(k)-s)**2
        end do

        r=sqrt(r/float(nm-1))

        return
        end


C------------------------------
        subroutine detrend(y,nm,a,b)
        complex y(nm),a,b
	real x(nm)
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

