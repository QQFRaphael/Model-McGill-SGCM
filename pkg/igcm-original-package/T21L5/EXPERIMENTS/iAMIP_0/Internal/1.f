
	parameter(nn=51*30)

	real c3(nn)



        open(14,file='fort.86')
        read(14,*)c3


        ave3=0.
        do k=1,nn
        ave3=ave3+c3(k)
        end do
	ave3=ave3/nn
	print*,ave3

        stan3=0.
        do k=1,nn
        stan3=stan3+(c3(k)-ave3)**2
        end do
        stan3=stan3/nn
	print*,stan3
C****************fort.86 is the internal part***********************************



        write(80,*)'Internal for EOF1 is:',stan3
	




	stop
	end
