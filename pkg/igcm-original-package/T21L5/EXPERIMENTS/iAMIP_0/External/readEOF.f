	Parameter ( nm=51, ng=36*14, nEmax=10,Nrun=30)
        Real  eign(ng,nEmax)

      open(15,file=
     * 'Z500EOF_ASCII')

      open(23,file='Z500EOF',form='unformatted',
     & status='old')

        read(23)eign
	write(15,*)eign



      stop
      end


