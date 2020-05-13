        parameter(ng=36*14, nMax=4)
        real x(37,14),evt(ng,nMax)



         open(17,file='../EOF',form='unformatted')
c        open(17,file='../roteof',form='unformatted')
	 read(17) evt

         do 99 k=1,2
         ii=0
          if (k.eq.1) then
         do 14 j=1,14
         do 14 i=1,36
         ii=ii+1
          x(i,j)=-evt(ii,k)
14      continue
          else
         do 15 j=1,14
         do 15 i=1,36
         ii=ii+1
          x(i,j)=-evt(ii,k)
15      continue
          end if

        do 41 j=1,14
41      x(37,j)=x(1,j)




         write(30)((x(i,j),i=1,37),j=1,14)

99	 continue

	end
