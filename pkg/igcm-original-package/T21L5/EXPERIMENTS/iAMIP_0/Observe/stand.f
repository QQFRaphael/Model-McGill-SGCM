
	parameter(nm=51*30,nn=51)

	real c1(nm),c2(nm)
	real c3(nn),c4(nn)


 	open(12,file='fort.87')
 	read(12,*)c1

        ave1=0.
        do k=1,nm
        ave1=ave1+c1(k)/nm
        end do

        stan1=0.
        do k=1,nm
        stan1=stan1+(c1(k)-ave1)**2
        end do
        stan1=stan1/nm
C*************fort.87 is the sum**********************************

        open(12,file='../Ensemble/Years/Eof/fort.86')
        read(12,*)c2

        ave2=0.
        do k=1,nm
        ave2=ave2+c2(k)/nm
        end do

        stan2=0.
        do k=1,nm
        stan2=stan2+(c2(k)-ave2)**2
        end do
        stan2=stan2/nm
C***************fort.86 is the internal part********************************


        open(14,file='../Season/Eof/fort.85')
        read(14,*)c3


        ave3=0.
        do k=1,nn
        ave3=ave3+c3(k)/nn
        end do

        stan3=0.
        do k=1,nn
        stan3=stan3+(c3(k)-ave3)**2
        end do
        stan3=stan3/nn
C****************fort.85 is the external part***********************************


	write(80,*)'Observed data for EOF1 is:',stan1
	write(80,*)
	sum=stan3+stan2
        write(80,*)'The sum of External and Internal is:',sum
	write(80,*) 

        write(80,*)'Internal for EOF1 is:',stan2
        write(80,*)

        write(80,*)'External for EOF1 is:',stan3
	write(80,*)
	

	write(80,*)

	perext=(stan3/stan1)*100

	perint=(stan2/stan1)*100


        write(80,*)'External part for EOF1 is:',perext
        write(80,*)'Internal part for EOF1 is:',perint
	write(80,*)


	stop
	end
