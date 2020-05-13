	parameter(NT=180,nm=80)
	Parameter (mmg=73,nng=37,nnl=15)
        real tt1(mmg,nng),tt2(mmg,nng),tt3(mmg,nng),tt4(mmg,nng)
        real tt5(mmg,nng),tt6(mmg,nng),tt7(mmg,nng),tt8(mmg,nng)
        real tt9(mmg,nng),tt0(mmg,nng)
        real phm(mmg,nnl),pht(mmg,nnl,nm)
        real ave(mmg,nnl,nm),var(mmg,nnl,nm)
        real varm(mmg,nnl),avem(mmg,nnl)


C
	open (32, 
     & file='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/Z_daily73.dat
     & ',form='unformatted',status='old')
	 open(26,file='ave10.data',form='unformatted')

        do i=1,mmg
        do j=1,nnl
        do iy=1,nm
        ave(i,j,iy)=0.
	var(i,j,iy)=0.
	end do
	varm(i,j)=0.
	avem(i,j)=0.
	phm(i,j)=0.
	end do
	end do

        do 111 iy=1,nm
          print*,iy

ccc  Time Mean field-------------
        do 12 it=1,NT
         read(32)tt0
         read(32)tt1
         read(32)tt2
         read(32)tt3
         read(32)tt4
         read(32)tt5
         read(32)tt6
         read(32)tt7
         read(32)tt8
         read(32)tt9


        do 23 i=1,mmg
        do 23 j=1,nnl
      phm(i,j)=phm(i,j)+tt5(i,j)
      pht(i,j,it)=tt5(i,j)
23      continue
12      continue


        do 24 i=1,mmg
        do 24 j=1,nnl
         phm(i,j)=phm(i,j)/float(NT)
24      continue


cc  variance
        do 51 i=1,mmg
        do 51 j=1,nnl
          do 53 k=1,NT
	  s=pht(i,j,k)-phm(i,j)
53        var(i,j,iy)=var(i,j,iy)+s*s/float(NT-1)
	  ave(i,j,iy)=phm(i,j)
51      continue

111     continue


	do iy=1,nm
	 do i=1,mmg
	 do j=1,nnl
	  avem(i,j)=avem(i,j)+ave(i,j,iy)/real(nm)
	  varm(i,j)=varm(i,j)+var(i,j,iy)/real(nm)
	 end do
	 end do
	end do

	write(26)avem,varm
	stop
        end


