	PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA,nm=51)

      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)

        character*100 outfile
        character*2 year(51)
        data year/'01','02','03','04','05','06','07','08',
     & '09','10','11','12','13','14','15','16','17','18','19',
     & '20','21','22','23','24','25','26','27','28','29','30',
     & '31','32','33','34','35','36','37','38','39','40','41',
     & '42','43','44','45','46','47','48','49','50','51'/


	OPEN(14,FILE='/mnt/climate/data/loach/jiaxj/'//
     &'Data/SGCM/Input/T21L5/FORCE_51DJF.dat_crtd'
     & ,form='UNFORMATTED',status='old')


	do iy=1,nm
	 print*, iy
	 READ(14)Z,D,T,SP

        outfile='FORCE'//year(iy)//'.dat'
        OPEN(15,FILE='/mnt/climate/data/loach/jiaxj/'//
     &'Data/SGCM/Input/T21L5/FORCE/TroExtroMF/MF51/'//outfile,
     & form='UNFORMATTED')

        write(15)Z,D,T,SP
	end do


	
        stop
        end










