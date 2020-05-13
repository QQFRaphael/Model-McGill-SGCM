
	parameter(nn=51*30)

	real c3(nn)



        open(14,file='fort.86')
        read(14,*)c3


        ave3=0.
        do k=1,nn
        ave3=ave3+c3(k)/nn
        end do
	print*,ave3

        stan3=0.
        do k=1,nn
        stan3=stan3+(c3(k)-ave3)**2/nn
        end do
	print*,stan3
C****************fort.86 is the internal part***********************************





	stop
	end
