	Parameter (mmg=72,nng=37,nnl=37)
	parameter(NLON=64,NLAT=32)
	real div(mmg,15),tend(mmg,15),tendave(mmg,15),tendave0(73,15)
	real fllu(73,nnl),fllv(73,nnl)
	real fllu0(mmg,nnl),fllv0(mmg,nnl)

	real uclim(NLON,NLAT),vclim(NLON,NLAT),uanom(NLON,NLAT),vanom(NLON,NLAT)
	real vortclim(NLON,NLAT),vortanom(NLON,NLAT)
	real uclimave(NLON,NLAT),vclimave(NLON,NLAT),vortclimave(NLON,NLAT)
	real uvortdev(NLON,NLAT,90),vvortdev(NLON,NLAT,90)
	real uvortdev2(NLON,NLAT),vvortdev2(NLON,NLAT)



      open (11,file=
     *'/zemo2/jiaxj/igcm/T21L5/EXPERIMENTS/IDEAL/Exp_eq/'//
     *'Clim/Enssum_UV_300.dat'
     * ,form='unformatted')
      open (12,file=
     *'/mnt/climate/data/loach/jiaxj/Data/SGCM/OutData/'//
     *'IDEAL/135E/Enssum_UV_300.dat'
c    *'IDEAL/145W/Enssum_UV_300.dat'
     *,form='unformatted')
      open (13,file='/zemo2/jiaxj/igcm/T21L5/EXPERIMENTS/'//
     *'IDEAL/Exp_eq/Clim/Enssum_Vort.dat'
     *,form='unformatted')
      open (14,file='/mnt/climate/data/loach/jiaxj/Data/SGCM/'//
     *'OutData/IDEAL/135E/Enssum_Vort.dat'
c    *'OutData/IDEAL/145W/Enssum_Vort.dat'
     *,form='unformatted')


        do kkk=1,30
        read(11)
        read(11)
        read(12)
        read(12)
        read(13)
        read(14)
        end do

CCC Get the climate U, V, Vort from the control runs

        do j=1,NLAT
        do i=1,NLON
          uclimave(i,j)=0.
          vclimave(i,j)=0.
          vortclimave(i,j)=0.
        end do
        end do


	do kkk=1,90
          read(11)uclim
          read(11)vclim
          read(13)vortclim
        do j=1,NLAT
        do i=1,NLON
          uclimave(i,j)=uclimave(i,j)+uclim(i,j)/real(90)
          vclimave(i,j)=vclimave(i,j)+vclim(i,j)/real(90)
          vortclimave(i,j)=vortclimave(i,j)+vortclim(i,j)/real(90)
        end do
        end do
	end do

CCC

CCC Get the transient U', V', Vort' 

        do kkk=1,90
          read(12)uanom
          read(12)vanom
          read(14)vortanom
        do j=1,NLAT
        do i=1,NLON
          uvortdev(i,j,kkk)=
     * (uanom(i,j)-uclimave(i,j))*(vortanom(i,j)-vortclimave(i,j))
          vvortdev(i,j,kkk)=
     * (vanom(i,j)-vclimave(i,j))*(vortanom(i,j)-vortclimave(i,j))
        end do
        end do
        end do



	   do kkk=1,90

	 do i=1,mmg
	 do j=1,nnl
	  tendave(i,j)=0
	 end do
	 end do


        do j=1,NLAT
        do i=1,NLON
          uvortdev2(i,j)=uvortdev(i,j,kkk)
          vvortdev2(i,j)=vvortdev(i,j,kkk)
        end do
        end do
	  call gautogrid(uvortdev2,fllu)
	  call gautogrid(vvortdev2,fllv)
CC
	 do i=1,mmg
	 do j=1,15
	  fllu0(i,j)=fllu(i,j)
	  fllv0(i,j)=fllv(i,j)
	 end do
	 end do

          call mydiv(fllu0,fllv0,div)
CC
          call relax(div,tend)

	 do i=1,mmg
	 do j=1,15
	  tendave(i,j)=tendave(i,j)+tend(i,j)/real(90)
	 end do
	 end do
	  end do

         do i=1,mmg
         do j=1,15
          tendave0(i,j)=tendave(i,j)
         end do
          tendave0(73,j)=tendave(1,j)
         end do



	  write(30) tendave0








CCCCCCCCCCC

	 stop 
	 end 





#include  "/zemo2/jiaxj/SUBROUTINES/diverg.h"
#include  "/zemo2/jiaxj/SUBROUTINES/relax.h"
#include  "/zemo2/jiaxj/SUBROUTINES/gausstogrid.f"


