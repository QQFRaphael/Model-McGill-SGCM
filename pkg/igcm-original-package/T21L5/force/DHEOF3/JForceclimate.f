
      PROGRAM GETFORCE
      real fll(73,37),rll(73,37)	

      OPEN(13,
     &FILE='FORCE_OtoM.dat_crtd_diabatic_grid_ave',form='UNFORMATTED') 
	
      OPEN(16,
     &FILE='FORCE_OtoM.dat_crtd_diabatic_grid_climate',
     & form='UNFORMATTED') 
	

	do 30 j=1,37
	do 30 i=1,73
	  rll(i,j)=0.
30	continue




	do 10 NREC=1,51

	read(13)fll
	read(13)fll

	do kk=1,3
	read(13)fll
	do j=1,37
	do i=1,73
c	  rll(i,j)=rll(i,j)+fll(i,j)/real(51*3)
 	  rll(i,j)=rll(i,j)+fll(i,j)
	enddo
	enddo	
	enddo
	read(13)fll

10	continue

	do j=1,37
	do i=1,73
 	  rll(i,j)=rll(i,j)/real(51*3)
	enddo
	enddo	



 	write(16)rll
 	print*,rll

      STOP
      END               
