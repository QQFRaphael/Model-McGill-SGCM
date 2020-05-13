        parameter ( nm=180*45, ng=36*14,nt=180,np=45)
        parameter ( M=73, N=37,nday=180,L=15)
      Real t(M,N),tm(M,L,nday),e(ng,4),c(4,nm),yy1(nm),
     &      var(ng,nm), cct(ng,ng),fllp(M,N)
	real ttp(M,L,nm),t1(M,N),t2(M,L,nm)
	real y1(nm),dex(np,nt),dexx(np,nt)
	real ylf(nm),tt(nt),ts(M,L,nm),ti(M,L)
	real tr(nm),th(M,L),ty(M,L),tl(M,L)



      open (11, 
     * file='/zemo2/jiaxj/igcm/RESULT/CONTROLT31/SLP_daily73.dat'
     *,form='unformatted', status='old')

      open (12, file='anom.dat',form='unformatted')


CC get climatology for each grid
        do i=1,M
        do j=1,L
        do k=1,nday
         tm(i,j,k)=0.
        end do
        end do
        end do


        do k1=1,np
        do 61 k2=1,nday
	 read(11)t
         do 12 j=1,L
         do 12 i=1,M
12       tm(i,j,k2)=tm(i,j,k2)+t(i,j)/real(np)
61      continue
        end do
CC********tm is the climatology mean******
	rewind(11)


        do 66 k1=1,np
        do 65 k2=1,nday
	  read(11) t
         do i=1,M
         do j=1,L
          tl(i,j)=t(i,j)-tm(i,j,k2)
         end do
         end do
	 write(12) tl
65      continue
66      continue
CC substrace the climatology for all grids





	 stop 
	 end 





