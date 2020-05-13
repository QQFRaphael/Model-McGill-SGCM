CC get mean forcing field for 51 winters--
      PROGRAM GETFORCE
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA)
      COMPLEX Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      COMPLEX Z0(IGB),D0(IGB),T0(IGB),SP0(IGA)
      COMPLEX ZF(IGB),DF(IGB),TF(IGB),SPF(IGA)

       open(14,file='ASCII')

       OPEN(13,FILE='ForceDHEOF3_jcja',form='UNFORMATTED')

       NRECS=51*6

       DO 10 NREC=1,NRECS

        read(13)ZF,DF,TF,SPF
        write(14,*)ZF,DF,TF,SPF

10    CONTINUE

      STOP
      END

