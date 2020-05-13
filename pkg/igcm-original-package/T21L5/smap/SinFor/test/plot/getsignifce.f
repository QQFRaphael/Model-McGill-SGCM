	parameter(NLON=73,NLAT=15,nm=50,mk=1)
	real st(NLON,NLAT),fll(73,37),
     *    x(73,15),x00(73,15),compp(NLON,NLAT,200)
	real pot(NLON,37),tt(73,15,nm),cp(73,15),cn(73,15)
	real phm(NLON,NLAT,mk),sa(nm),pht(NLON,NLAT,mk,200),
     *    phmon(NLON,NLAT,nm),sig(NLON,NLAT)
	real wwp(NLON,NLAT)
	real avem(NLON,NLAT),varm(NLON,NLAT)


        character*3 cy
CC---
        call getarg(1,cy)
cc convert from character to integer--
        read(cy,'(i3)')icount



       open(25,file='../fort.21',form='unformatted')
       open(26,file='../fort.20',form='unformatted')
       open(27,file='../avevar.data',form='unformatted')
	read(27)avem,varm

	do kk=1,1
	 read(26)pot
        do i=1,NLON
        do j=1,NLAT
         phm(i,j,kk)=pot(i,j)
	end do
	end do
	end do

	do k2=1,icount
        do k1=1,mk
         read(25)wwp

        do i=1,NLON
        do j=1,NLAT
	pht(i,j,k1,k2)=wwp(i,j)+avem(i,j)
	end do
	end do

   	end do
   	end do
	
CCCCCCCC

	 iic=0
	 iii=0
	do 999 k1=1,1
          do i=1,NLON
          do j=1,NLAT
            x(i,j)=phm(i,j,k1)
          end do
          end do

CC Do the t-test

	do i=1,NLON
        do j=1,NLAT
	do k2=1,icount
	 compp(i,j,k2)=pht(i,j,k1,k2)
	end do
	end do
	end do

        call stud(compp,avem,varm,sig,icount,9282)
c call stud(compp,avem,varm,sig,icount,10800)
          do j=1,15
          do i=1,73
           x00(i,j)=sig(i,j)
          end do
          end do
	  write(51) x00
	
CCCCC
999	continue 


        stop
        end


        subroutine stud(en,avem,varm,sig,ng,nl)
        parameter (L=73,M=15,MAMT=200)
        real en(L,M,ng),avem(L,M),varm(L,M),
     *   CLIM(L,M),DEV(L,M),T,TCRIT1,TCRIT5,SIG(L,M),
     *   ASM(L,M),ASMSQ(L,M), darray,data1(MAMT)
        do 91 i=1,L
        do 91 j=1,M
          do 92 k=1,ng
92          data1(k)=en(i,j,k)
	  AVE2=avem(i,j)
	  VAR2=varm(i,j)
          call TTESTmy(DATA1,ng,AVE2,VAR2,nl,TCRIT5,PROB)
               SIG(I,j) = PROB
91         CONTINUE
        return
      END

      SUBROUTINE TTESTmy(DATA1,N1,AVE2,VAR2,N2,T,PROB)
      DIMENSION DATA1(N1)
      CALL AVEVAR(DATA1,N1,AVE1,VAR1)
c      CALL AVEVAR(DATA2,N2,AVE2,VAR2)
      DF=N1+N2-2
      VAR=((N1-1)*VAR1+(N2-1)*VAR2)/DF
      T=(AVE1-AVE2)/SQRT(VAR*(1./N1+1./N2))
      PROB=BETAI(0.5*DF,0.5,DF/(DF+T**2))
      RETURN
      END

#include "/diskc/hlin/recipes/avevar.for"
#include "/diskc/hlin/recipes/betai.for"
#include "/diskc/hlin/recipes/gammln.for"
#include "/diskc/hlin/recipes/betacf.for"

#include "/diskb/hlin/amip/Pac/fill.f"
