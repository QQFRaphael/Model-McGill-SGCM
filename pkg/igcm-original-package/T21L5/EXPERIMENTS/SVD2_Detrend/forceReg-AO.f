	PARAMETER (nm=51)


	real y1(nm),c(51,nm)

	open(12,file='/diska/hlin/NCEP/data52y/EOF/efficPmsl',
     *     form='unformatted')
        read(12)c
	do k=1,nm
	 y1(k)=c(2,k)
	end do
	
         s=0.
        do k=1,nm
         s=s+y1(k)/float(nm)
        end do

        std=0.
        do k=1,nm
         std=std+(y1(k)-s)**2/float(nm-1)
        end do
        std=sqrt(std)

        print*,std

	
	do k=1,nm
	 y1(k)=y1(k)/std
	end do

	write(34,*)y1

	end







