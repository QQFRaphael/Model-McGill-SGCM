	parameter(nm=30,ns=nm,M=72,M1=M+1,N=37,Nrun=30)
	parameter(NLON=64,NLAT=32)
	real ttp(M1,N,nm),ss(M1,N),wwp(M1,N),tt(M1,N,nm*Nrun)

        character*15 outfile
        character*2 year(51)
        data year/'01','02','03','04','05','06','07','08',
     & '09','10','11','12','13','14','15','16','17','18','19',
     & '20','21','22','23','24','25','26','27','28','29','30',
     & '31','32','33','34','35','36','37','38','39','40','41',
     & '42','43','44','45','46','47','48','49','50','51'/



cc initialize array to hold ensemble mean climate value
	do k=1,nm
	do i=1,M1
	do j=1,N
	 ttp(i,j,k)=0.
	end do
	end do
	end do

	  do ie=1,Nrun
	  print*, ie
         outfile='Anom'//year(ie)//'.dat'
       open(21,file=
     &'/mnt/climate/data/loach/jiaxj/Data/SGCM/OutData/IDEAL/'//
     &'155W/First30/'//outfile,form='unformatted',status='old')

	  do kk=1,nm
          read(21)ss
	do i=1,M1
	do j=1,N
	 tt(i,j,kk+(ie-1)*nm)=ss(i,j)
	end do
	end do
	  end do

	  end do


CCCCCCCCDo averaged

          do l=1,Nrun
          do k=1,nm
           do i=1,M1
           do j=1,N
            ttp(i,j,k)=ttp(i,j,k)+tt(i,j,k+(l-1)*nm)/real(Nrun)
           end do
           end do
          end do
          end do



          do k=1,nm
           do i=1,M1
           do j=1,N
            wwp(i,j)=ttp(i,j,k)
           end do
           end do
            write(50) wwp

          end do















        stop
        end



#include "/diska/hlin/qgmodel/plot/gausstogrid.f"
