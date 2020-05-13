      PROGRAM GETFORCE
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA,nm=51)

      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      COMPLEX az(IGB),bz(IGB),ad(IGB),bd(IGB),at(IGB),
     *      bt(IGB),ap(IGA),bp(IGA)

	character*15 outfile
	character*2 year(51)
	data year/'01','02','03','04','05','06','07','08',
     & '09','10','11','12','13','14','15','16','17','18','19',
     & '20','21','22','23','24','25','26','27','28','29','30',
     & '31','32','33','34','35','36','37','38','39','40','41',
     & '42','43','44','45','46','47','48','49','50','51'/

	real c(4,nm),y1(nm)
        open(12,file='sstPacDJFeffic.dat',
     *     form='unformatted')
        read(12)c
        do k=1,nm
         y1(k)=c(2,k)
        end do
CC 
	OPEN(16,file='regCoeff-E2-detrend.dat',form='UNFORMATTED')
	read(16)az,bz,ad,bd,at,bt,ap,bp

	do k=1,nm
	 DO I=1,IGB
	 ZA(I)=az(I)*y1(k)+bz(I)
	 DA(I)=ad(I)*y1(k)+bd(I)
	 TA(I)=at(I)*y1(k)+bt(I)
	 END DO
	 DO I=1,IGA
	 SPA(I)=ap(I)*y1(k)+bp(I)
	 END DO

	outfile='FORCE_DJF'//year(k)//'.dat'
      OPEN(15,FILE='/diskc/jiaxj/force/Haiforcedetrend/'//
     &  outfile,form='UNFORMATTED')

	write(15)ZA,DA,TA,SPA

	end do

      STOP
      END               
