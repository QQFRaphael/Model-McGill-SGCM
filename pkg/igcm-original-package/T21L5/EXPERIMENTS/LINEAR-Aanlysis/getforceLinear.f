
      PROGRAM GETFORCE
C*****compile with -r8
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA)
      COMPLEX Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      COMPLEX Z0(IGB),D0(IGB),T0(IGB),SP0(IGA)
      COMPLEX ZF(IGB),DF(IGB),TF(IGB),SPF(IGA)


      OPEN(10, FILE='FORCE+edd0/restart.12',
     & form='UNFORMATTED',status='old')
      OPEN(11,FILE=
c    &'/mnt/climate/data/loach/jiaxj/Data/SGCM/Input/'//
c    &'T21L5/UVTP4849after.dat.51DJFave',
c    &'/zemo2/jiaxj/igcm/T21L5/force/GetMF/UVTPEL-bs.dat.DJFave',
     &'/zemo2/jiaxj/igcm/T21L5/force/GetMF/UVTPLa-bs.dat.DJFave',
c    &'/zemo2/jiaxj/igcm/T21L5/force/GetMF/UVTP1998.dat.DJFave',
     & form='UNFORMATTED',status='old')
      OPEN(13,FILE='Force+edd.dat_crtd',form='UNFORMATTED') 

      PI=3.14159265359
      DELT2=4.*PI/48.
      NRECS=1

Cjxj
      READ(10)RKOUNT,RMTAPE,DAY,ZA,DA,TA,SPA,RLTAPE
      READ(11)RKOUNT,RMTAPE,DAY,Z0,D0,T0,SP0,RLTAPE


      DO I=1,IGB
      ZF(I)=(-ZA(I)+Z0(I))/DELT2
      DF(I)=(-DA(I)+D0(I))/DELT2
      TF(I)=(-TA(I)+T0(I))/DELT2
      ENDDO
      DO I=1,IGA
      SPF(I)=(-SPA(I)+SP0(I))/DELT2
      ENDDO

      WRITE(13)ZF,DF,TF,SPF


      STOP
      END               
