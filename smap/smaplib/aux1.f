      subroutine uvharm   
      common z(946),u(989),v(946),zt(946),zti(946),zi(946)
     @,salp(989,64),sdalp(946,64),zspfce(946)
      complex z,u,v,zt,zti,zi,zspfce
      common/grid/
     1zg(130),ug(130),vg(130),uzg(130),vzg(130),eprn(989),
     2sq(64),rsq(64),phi(130,64),zforce(130,64),tphi(130),
     3tzf(130),fo,ez,mf,mfp,aioct,weight,jg,moct,mh,mg,mgpp,jzf,nwj2
      complex ez,aioct
      common/adiv/
     @ div(946),ztot(946),diver(1892),divem(1892),ww,pi2,drag,azk
      complex div,ztot
      complex zm,zp,aim                                              
      complex zz                                                    
      complex dp,dz,dm
c     dp=(0.,0.)
c     dz=dp 
c     dm=dp 
      z(2)=z(2)-ez                                                 
      iz=1                                                        
      iu=0                                                       
      ip=0                                                      
      aim=-aioct                                               
      eprnp=0.                                                
      do 2 mp=1,mfp,moct                                     
      m=mp-1                                                
      ip=ip+1                                              
      aim=aim+aioct                                       
      zz=0.                                              
      zp=z(iz)                                          
      dz=0.
      dp=div(iz)
      in=mp   
      do 1  jp=mp,mfp,mh 
      iu=iu+1           
      iz=iz+1                                                           
      ip=ip+1                                                          
      zm=zz                                                           
      zz=zp                                                          
      dm=dz 
      dz=dp 
      if(jp-mfp)3,4,4                                               
    3 zp=z(iz)                                                     
      dp=div(iz)
      go to 5                                                     
    4 zp=0.                                                      
      dp=0. 
    5 eprnn=eprnp                                               
      eprnp=eprn(ip)                                           
c     u(iu)=-eprnn*zm+eprnp*zp                                
c     v(iz-1)=-aim*rsq(in)*zz                                
      u(iu)  =-eprnn*zm-aim*rsq(in)*dz+eprnp*zp 
      v(iz-1)= eprnn*dm-aim*rsq(in)*zz-eprnp*dp 
cc    v(iu)  = eprnn*dm-aim*rsq(in)*zz-eprnp*dp 
      in=in+1                                               
    1 continue                                             
      iu=iu+1                                             
      u(iu)=-eprnp*zz                                    
cc    v(iu)= eprnp*dz 
    2 continue                                          
      z(2)=z(2)+ez                                     
      return                                          
      end                                            
c///////////////////////////////////////////////////////////////////////
      subroutine hanal
      common z(946),u(989),v(946),zri(1892),zti(946),zi(946)
      complex z,u,v,zti,zi                                 
      common/grid/
     1zg(130),ug(130),vg(130),uzg(130),vzg(130),eprn(989),
     2sq(64),rsq(64),phi(130,64),zforce(130,64),tphi(130),
     3tzf(130),fo,ez,mf,mfp,aioct,weight,jg,moct,mh,mg,mgpp,jzf,nwj2
      complex ez,aioct
      common/legau/alp(989),dalp(946),gw(48)
      ip=1                                                          
      ifr=1                                                        
      ilr=1                                                       
      vzfr=vzg(ifr)                                              
      zri(1)=zri(1)+vzfr*dalp(1)                                
      lim=1+mh                                                 
      do 1 jp=lim,mfp,mh                                      
      ilr=ilr+2                                              
      ip=ip+1                                               
      zri(ilr)=zri(ilr)+vzfr*dalp(ip)                      
    1 continue                                            
      idp=ip                                            
      ip=ip+1                                          
      do 3 m=moct,mf,moct                             
      fm=m                                           
      ifr=ifr+2                                     
      ifi=ifr+1                                    
      vzfr=vzg(ifr)                               
      vzfi=vzg(ifi)                              
      fmuzfr=fm*uzg(ifr)                        
      fmuzfi=fm*uzg(ifi)                       
      do 2 jp=m,mf,mh                         
      ip=ip+1                                
      idp=idp+1                             
      ilr=ilr+2                            
      ili=ilr+1                           
      alpng=alp(ip)*weight               
      dalpn=dalp(idp)                   
      zri(ilr)=zri(ilr)+fmuzfi*alpng+vzfr*dalpn    
      zri(ili)=zri(ili)-fmuzfr*alpng+vzfi*dalpn   
    2 continue                                   
      ip=ip+1                                   
    3 continue                                                          
      return                                                           
      end                                                             
c///////////////////////////////////////////////////////////////////////
      subroutine hexp                                                
      common zri(1892),uri(1978),vri(1892),ztri(1892),zti(946),zi(946)
     @,salp(989,64),sdalp(946,64),zspfce(946)                        
      complex zti,zi,zspfce                                 
      common/grid/
     1zg(130),ug(130),vg(130),uzg(130),vzg(130),eprn(989),
     2sq(64),rsq(64),phi(130,64),zforce(130,64),tphi(130),
     3tzf(130),fo,ez,mf,mfp,aioct,weight,jg,moct,mh,mg,mgpp,jzf,nwj2
      complex ez,aioct
      common/legau/alp(989),dalp(946),gw(48)
      ip=0                                                         
      ivr=-1                                                      
      azfr=0.                                                    
      aufr=0.                                                   
      avfr=0.                                                  
      do 3 jp=1,mfp,mh                                       
      ip=ip+1                                               
      ivr=ivr+2                                            
      alpn=alp(ip)                                        
      azfr=azfr+alpn*zri(ivr)                            
      aufr=aufr+alp(ip)*uri(ivr)                        
      avfr=avfr+alpn*vri(ivr)                          
    3 continue                                        
      iur=ivr+2                                      
      ip=ip+1                                       
      ug(1)=aufr+alp(ip)*uri(iur)                  
      ug(2)=0.                                    
      vg(1)=avfr                                 
      vg(2)=0.                                  
      zg(1)=azfr                               
      zg(2)=0.                                
      imr=3                                  
      do 2 m=moct,mf,moct                   
      imi=imr+1                            
      azfr=0.                                               
      azfi=0.                                              
      aufr=0.                                             
      aufi=0.                                            
      avfr=0.                                           
      avfi=0.                                          
      do 1 jp=m,mf,mh                                 
      ip=ip+1                                        
      iur=iur+2                                     
      ivr=ivr+2                                    
      ivi=ivr+1                                   
      alpn=alp(ip)                               
      aufr=aufr+alpn*uri(iur)                   
      aufi=aufi+alpn*uri(iur+1)                
      azfr=azfr+alpn*zri(ivr)                 
      azfi=azfi+alpn*zri(ivi)                
      avfr=avfr+alpn*vri(ivr)               
      avfi=avfi+alpn*vri(ivi)              
    1 continue                            
      iur=iur+2                          
      ip=ip+1                           
      alpn=alp(ip)                                             
      ug(imr)=aufr+alpn*uri(iur)                              
      ug(imi)=aufi+alpn*uri(iur+1)                           
      vg(imr)=avfr                                          
      vg(imi)=avfi                                         
      zg(imr)=azfr                                        
      zg(imi)=azfi                                       
    2 imr=imr+2                                         
      do 4 j=1,jzf                                     
      ug(imr)=0.                                      
      vg(imr)=0.                                     
      zg(imr)=0.                                    
    4 imr=imr+1                                    
      return                                      
      end                                        
c///////////////////////////////////////////////////////////////////////
      subroutine lgndre(coa,weight,mfp,moct,alp,dalp) 
      dimension alp(78),dalp(72)                    
      sia=sqrt(1.-coa*coa)                         
      lm=2                                     
      lmd=1                                
      alp(1)=sqrt(.5)                  
      f1m=sqrt(1.5)                 
      alp(2)=f1m*coa              
      dalp(1)=0.                                                   
      do 1 m1=1,mfp                                              
      m=m1-1                                                  
      am=m                                                 
      a2m=m+m                                          
      re1=sqrt(a2m+3.)                             
      e1=1./re1                                   
      if(m.eq.0)goto 3                          
      f2m=-f1m*sia/sqrt(a2m)                                          
      f1m=f2m*re1                                                    
      if(m.ne.mmo)goto 1                                            
      lm=lm+1                                                      
      alp(lm)=f2m                                                 
      lm=lm+1                                                    
      alp(lm)=f1m*coa                                           
      lmd=lmd+1                                                
      dalp(lmd)=-am*e1*alp(lm)*weight                         
      if(m1.eq.mfp)go to 1
    3 m2=m+2                                                
      mmo=m+moct                                           
      jfm=mfp 
      do 4 n=m2,jfm                                      
      an=n                                              
      an2=n*n                                          
      e2=sqrt((4.*an2-1.)/(an2-am*am))                
      lm=lm+1                                        
      alp(lm )=e2*(coa*alp(lm -1)-e1*alp(lm -2))    
      e2=1./e2                                                          
      lmd=lmd+1                                                        
      dalp(lmd)=((1.-an)*e2*alp(lm)+an*e1*alp(lm-2))*weight           
      e1=e2                                                          
    4 continue                                                      
    1 continue                                                     
      return                                                      
      end                                                       
c///////////////////////////////////////////////////////////////////
      subroutine gauaw(a,w,k)
c      a and w are arrays of length k (at least), giving abscissas and
c      weights for the gaussian integration.                         
c      the zeros of the bessel functions j0 are used to get starting 
c      approximations for the abscissas.                            
      dimension a(k),w(k)                                               

c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

c.ps  eps=1.e-14                                                       
      eps=1.e-07                                                      
c      the value eps, used for convergence tests in the iterations, can 
c      be changed. Use the smaller valu in double precision.                                                     
c      newton iteration is used to find the abscissas.                
      c=(1.-(2./3.14159265358979)**2)*0.25                           
      fk=k                                                          
      kk=k/2                                                       
      call bsslzr(a,kk)                                           
      do 30 is=1,kk                                              
      xz=cos(a(is)/sqrt((fk+0.5)**2+c))                         
c      this is the first approximation to xz                   
      iter=0                                                  
   10 pkm2=1.                                                
      pkm1=xz                                               
      iter=iter+1                                          
      if(iter.gt.10) go to 70                             
c      computation of the legendre polynomial            
      do 20 n=2,k                                       
      fn=n                                             
      pk=((2.*fn-1.)*xz*pkm1-(fn-1.)*pkm2)/fn         
      pkm2=pkm1                                      
   20 pkm1=pk                                       
      pkm1=pkm2                                    
      pkmrk=(fk*(pkm1-xz*pk))/(1.-xz**2)          
      sp=pk/pkmrk                                
      xz=xz-sp                                  
      avsp=abs(sp)                             
      if(avsp.gt.eps) go to 10                
      a(is)=xz                               
      w(is)=(2.*(1.-xz**2))/(fk*pkm1)**2                           
   30 continue                                                    
      if(k.eq.kk*2) go to 50                                            
c      for odd k computation of weight at the equator                  
      a(kk+1)=0.                                                      
      pk=2./fk**2                                                    
      do 40 n=2,k,2                                                 
      fn=n                                                         
   40 pk=pk*fn**2/(fn-1.)**2                                      
      w(kk+1)=pk                                                 
   50 continue                                                  
c      complete the sets of abscissas and weights, using the symmetry.  
      do 60 n=1,kk                                                     
      l=k+1-n                                                         
      a(l)=-a(n)                                                     
   60 w(l)=w(n)                                                     
      return                                                       
c      error exit                                                 
   70 print 100                                                  
  100 format(//,5x,14herror in gauaw)                           
      stop                                                     
      end                                                     
c///////////////////////////////////////////////////////////////////////////
      subroutine bsslzr(bes,n)                                         
c     this routine returns n zeros, or if n>50 approximate zeros, of 
c     the bessel functions j0. the zeros are returned in the array bes.
c     the first 50 zeros will be given exactly, and the remaining zeros
c     will be computed by extrapolation, and therefore not exact.     
      dimension bes(n)                                                  
      dimension bz(50)                                                 
      data pi/3.14159265358979/                         
      data bz           / 2.4048255577,   5.5200781103,                
     x    8.6537279129,  11.7915344391,  14.9309177086,  18.0710639679,
     x   21.2116366299,  24.3524715308,  27.4934791320,  30.6346064684,
     x   33.7758202136,  36.9170983537,  40.0584257646,  43.1997917132, 
     x   46.3411883717,  49.4826098974,  52.6240518411,  55.7655107550,
     x   58.9069839261,  62.0484691902,  65.1899648002,  68.3314693299,
     x   71.4729816036,  74.6145006437,  77.7560256304,  80.8975558711,
     x   84.0390907769,  87.1806298436,  90.3221726372,  93.4637187819, 
     x   96.6052679510,  99.7468198587, 102.8883742542, 106.0299309165,
     x  109.1714896498, 112.3130502805, 115.4546126537, 118.5961766309,
     x  121.7377420880, 124.8793089132, 128.0208770059, 131.1624462752,
     x  134.3040166383, 137.4455880203, 140.5871603528, 143.7287335737,
     x  146.8703076258, 150.0118824570, 153.1534580192, 156.2950342685/ 
      nn=n                                                             
      if(n.le.50) go to 12                                            
      bes(50)=bz(50)                                                 
      do 5 j=51,n                                                   
    5 bes(j)=bes(j-1)+pi                                           
      nn=49                                                      
   12 do 15 j=1,nn                                              
   15 bes(j)=bz(j)                                             
      return                                                  
      end                                                    
c//////////////////////////////////////////////////////////////////////////////
C     SUBROUTINE 'FFT991' - MULTIPLE FAST REAL PERIODIC TRANSFORM      
C     SUPERSEDES PREVIOUS ROUTINE 'FFT991'                            
C                                                                    
C     REAL TRANSFORM OF LENGTH N PERFORMED BY REMOVING REDUNDANT    
C     OPERATIONS FROM COMPLEX TRANSFORM OF LENGTH N                
C                                                                 
C     A IS THE ARRAY CONTAINING INPUT & OUTPUT DATA              
C     WORK IS AN AREA OF SIZE (N+1)*MIN(LOT,64)                 
C     TRIGS IS A PREVIOUSLY PREPARED LIST OF TRIG FUNCTION VALUES      
C     IFAX IS A PREVIOUSLY PREPARED LIST OF FACTORS OF N              
C     INC IS THE INCREMENT WITHIN EACH DATA 'VECTOR'                 
C         (E.G. INC=1 FOR CONSECUTIVELY STORED DATA)                   
C     JUMP IS THE INCREMENT BETWEEN THE START OF EACH DATA VECTOR     
C     N IS THE LENGTH OF THE DATA VECTORS                            
C     LOT IS THE NUMBER OF DATA VECTORS                             
C     ISIGN = +1 FOR TRANSFORM FROM SPECTRAL TO GRIDPOINT          
C           = -1 FOR TRANSFORM FROM GRIDPOINT TO SPECTRAL         
C                                                                
C     ORDERING OF COEFFICIENTS:                                 
C         A(0),B(0),A(1),B(1),A(2),B(2),...,A(N/2),B(N/2)      
C         WHERE B(0)=B(N/2)=0; (N+2) LOCATIONS REQUIRED       
C                                                            
C     ORDERING OF DATA:                                     
C         X(0),X(1),X(2),...,X(N-1), 0 , 0 ; (N+2) LOCATIONS REQUIRED   
C                                                                      
C     VECTORIZATION IS ACHIEVED ON CRAY BY DOING THE TEANSFORMS       
C     IN PARALLEL                                                    
C                                                                   
C     N MUST BE COMPOSED OF FACTORS 2,3 & 5 BUT DOES NOT HAVE TO BE EVEN
C                                                                      
C     DEFINITION OF TRANSFORMS:                                       
C     -------------------------                                      
C                                                                   
C     ISIGN=+1: X(J)=SUM(K=0,...,N-1)(C(K)*EXP(2*I*J*K*PI/N))      
C         WHERE C(K)=A(K)+I*B(K) AND C(N-K)=A(K)-I*B(K)           
C                                                                
C     ISIGN=-1: A(K)=(1/N)*SUM(J=0,...,N-1)(X(J)*COS(2*J*K*PI/N))    
C               B(K)=-(1/N)*SUM(J=0,...,N-1)(X(J)*SIN(2*J*K*PI/N))  
C                                                                  
      SUBROUTINE FFT991(A,WORK,TRIGS,IFAX,INC,JUMP,N,LOT,ISIGN)   
      COMMON /QQQQFFT/ IXXX                                      
      DIMENSION A(N),WORK(N),TRIGS(N),IFAX(1)                   
C                                                              
      IF(IXXX.NE.1) CALL SET99(TRIGS,IFAX,N)                  
      NFAX=IFAX(1)                                           
      NX=N+1                                                
      IF (MOD(N,2).EQ.1) NX=N                              
      NBLOX=1+(LOT-1)/64                                  
      NVEX=LOT-(NBLOX-1)*64                              
      IF (ISIGN.EQ.-1) GO TO 300                                      
C                                                                    
C     ISIGN=+1, SPECTRAL TO GRIDPOINT TRANSFORM                     
C     -----------------------------------------                    
  100 CONTINUE                                                    
      ISTART=1                                                   
      DO 220 NB=1,NBLOX                                         
      IA=ISTART                                                
      I=ISTART                                                
      DO 110 J=1,NVEX                                        
      A(I+INC)=0.5*A(I)                                     
      I=I+JUMP                                             
  110 CONTINUE                                            
      IF (MOD(N,2).EQ.1) GO TO 130                       
      I=ISTART+N*INC                                    
      DO 120 J=1,NVEX                                  
      A(I)=0.5*A(I)                                   
      I=I+JUMP                                       
  120 CONTINUE                                      
  130 CONTINUE                                     
      IA=ISTART+INC                               
      LA=1                                                             
      IGO=+1                                                          
C                                                                    
      DO 160 K=1,NFAX                                               
      IFAC=IFAX(K+1)                                               
      IERR=-1                                                     
      IF (IGO.EQ.-1) GO TO 140                                   
      CALL RPASSM(A(IA),A(IA+LA*INC),WORK(1),WORK(IFAC*LA+1),TRIGS,     
     *    INC,1,JUMP,NX,NVEX,N,IFAC,LA,IERR)                           
      GO TO 150                                                       
  140 CONTINUE                                                       
      CALL RPASSM(WORK(1),WORK(LA+1),A(IA),A(IA+IFAC*LA*INC),TRIGS, 
     *    1,INC,NX,JUMP,NVEX,N,IFAC,LA,IERR)                       
  150 CONTINUE                                                    
      IF (IERR.NE.0) GO TO 500                                   
      LA=IFAC*LA                                                
      IGO=-IGO                                                 
      IA=ISTART                                               
  160 CONTINUE                                                         
C                                                                     
C     IF NECESSARY, COPY RESULTS BACK TO A                           
C     ------------------------------------                          
      IF (MOD(NFAX,2).EQ.0) GO TO 190                              
      IBASE=1                                                     
      JBASE=IA                                                   
      DO 180 JJ=1,NVEX                                          
      I=IBASE                                                  
      J=JBASE                                                 
      DO 170 II=1,N                                          
      A(J)=WORK(I)                                          
      I=I+1                                                
      J=J+INC                                             
  170 CONTINUE                                           
      IBASE=IBASE+NX                                    
      JBASE=JBASE+JUMP                                 
  180 CONTINUE                                        
  190 CONTINUE                                       
C                                                   
C     FILL IN ZEROS AT END                                           
C     --------------------                                          
      IX=ISTART+N*INC                                              
      DO 210 J=1,NVEX                                             
      A(IX)=0.0                                                  
      A(IX+INC)=0.0                                             
      IX=IX+JUMP                                               
  210 CONTINUE                                                
C                                                            
      ISTART=ISTART+NVEX*JUMP                               
      NVEX=64                                              
  220 CONTINUE                                            
      RETURN                                             
C                                                       
C     ISIGN=-1, GRIDPOINT TO SPECTRAL TRANSFORM        
C     -----------------------------------------       
  300 CONTINUE                                       
      ISTART=1                                      
      DO 410 NB=1,NBLOX                            
      IA=ISTART                                   
      LA=N                                       
      IGO=+1                                                          
C                                                                    
      DO 340 K=1,NFAX                                               
      IFAC=IFAX(NFAX+2-K)                                          
      LA=LA/IFAC                                                  
      IERR=-1                                                    
      IF (IGO.EQ.-1) GO TO 320                                       
      CALL QPASSM(A(IA),A(IA+IFAC*LA*INC),WORK(1),WORK(LA+1),TRIGS, 
     *    INC,1,JUMP,NX,NVEX,N,IFAC,LA,IERR)                       
      GO TO 330                                                   
  320 CONTINUE                                                        
      CALL QPASSM(WORK(1),WORK(IFAC*LA+1),A(IA),A(IA+LA*INC),TRIGS,  
     *    1,INC,NX,JUMP,NVEX,N,IFAC,LA,IERR)                        
  330 CONTINUE                                                     
      IF (IERR.NE.0) GO TO 500                                    
      IGO=-IGO                                                   
      IA=ISTART+INC                                             
  340 CONTINUE                                                 
C                                                                 
C     IF NECESSARY, COPY RESULTS BACK TO A                         
C     ------------------------------------                        
      IF (MOD(NFAX,2).EQ.0) GO TO 370                            
      IBASE=1                                                   
      JBASE=IA                                                 
      DO 360 JJ=1,NVEX                                        
      I=IBASE                                                
      J=JBASE                                               
      DO 350 II=1,N                                        
      A(J)=WORK(I)                                        
      I=I+1                                              
      J=J+INC                                           
  350 CONTINUE                                         
      IBASE=IBASE+NX                                  
      JBASE=JBASE+JUMP                               
  360 CONTINUE                                      
  370 CONTINUE                                     
C                                                 
C     SHIFT A(0) & FILL IN ZERO IMAG PARTS       
C     ------------------------------------      
      IX=ISTART                                                     
      DO 380 J=1,NVEX                                              
      A(IX)=A(IX+INC)                                             
      A(IX+INC)=0.0                                              
      IX=IX+JUMP                                                
  380 CONTINUE                                                 
      IF (MOD(N,2).EQ.1) GO TO 400                            
      IZ=ISTART+(N+1)*INC                                    
      DO 390 J=1,NVEX                                       
      A(IZ)=0.0                                            
      IZ=IZ+JUMP                                          
  390 CONTINUE                                           
  400 CONTINUE                                          
C                                                      
      ISTART=ISTART+NVEX*JUMP                         
      NVEX=64                                        
  410 CONTINUE                                      
      RETURN                                       
C                                                 
C     ERROR MESSAGES                                                
C     --------------                                               
  500 CONTINUE                                                    
      GO TO (510,530,550) IERR                                   
  510 CONTINUE                                                  
      WRITE(6,520) NVEX                                        
  520 FORMAT(16H1VECTOR LENGTH =,I4,17H, GREATER THAN 64)     
      GO TO 570                                              
  530 CONTINUE                                              
      WRITE(6,540) IFAC                                    
  540 FORMAT( 9H1FACTOR =,I3,17H, NOT CATERED FOR)        
      GO TO 570                                          
  550 CONTINUE                                          
      WRITE(6,560) IFAC                                
  560 FORMAT(9H1FACTOR =,I3,31H, ONLY CATERED FOR IF LA*IFAC=N)      
  570 CONTINUE                                                      
      RETURN                                                       
      END                                                         
C     SUBROUTINE 'RPASSM' - PERFORMS ONE PASS THROUGH DATA AS PART      
C     OF MULTIPLE REAL FFT (FOURIER SYNTHESIS) ROUTINE                 
C                                                                     
C     A IS FIRST REAL INPUT VECTOR                                   
C         EQUIVALENCE B(1) WITH A (LA*INC1+1)                       
C     C IS FIRST REAL OUTPUT VECTOR                                
C         EQUIVALENCE D(1) WITH C(IFAC*LA*INC2+1)                 
C     TRIGS IS A PRECALCULATED LIST OF SINES & COSINES           
C     INC1 IS THE ADDRESSING INCREMENT FOR A                    
C     INC2 IS THE ADDRESSING INCREMENT FOR C                   
C     INC3 IS THE INCREMENT BETWEEN INPUT VECTORS A           
C     INC4 IS THE INCREMENT BETWEEN OUTPUT VECTORS C         
C     LOT IS THE NUMBER OF VECTORS                          
C     N IS THE LENGTH OF THE VECTORS                       
C     IFAC IS THE CURRENT FACTOR OF N                     
C     LA IS THE PRODUCT OF PREVIOUS FACTORS              
C     IERR IS AN ERROR INDICATOR:                       
C              0 - PASS COMPLETED WITHOUT ERROR        
C              1 - LOT GREATER THAN 64                
C              2 - IFAC NOT CATERED FOR                                 
C              3 - IFAC ONLY CATERED FOR IF LA=N/IFAC                  
C                                                                     
C-----------------------------------------------------------------------
C                                                                      
      SUBROUTINE RPASSM(A,B,C,D,TRIGS,INC1,INC2,INC3,INC4,LOT,N,IFAC, 
     *    LA,IERR)                                                   
      DIMENSION A(N),B(N),C(N),D(N),TRIGS(N)                        
C                                                                  
      DIMENSION A10(64),A11(64),A20(64),A21(64),                  
     *    B10(64),B11(64),B20(64),B21(64)                        
      DATA SIN36/0.587785252292473/,SIN72/0.951056516295154/,   
     *    QRT5/0.559016994374947/,SIN60/0.866025403784437/     
C                                                             
      M=N/IFAC                                               
      IINK=LA*INC1                                          
      JINK=LA*INC2                                         
      JUMP=(IFAC-1)*JINK                                  
      KSTOP=(N-IFAC)/(2*IFAC)                            
C                                                                    
      IBAD=1                                                        
      IF (LOT.GT.64) GO TO 910                                     
      IBASE=0                                                     
      JBASE=0                                                    
      IGO=IFAC-1                                                
      IF (IGO.EQ.7) IGO=6                                      
      IBAD=2                                                  
      IF (IGO.LT.1.OR.IGO.GT.6) GO TO 910                    
      GO TO (200,300,400,500,600,800),IGO                   
C                                                          
C     CODING FOR FACTOR 2                                 
C     -------------------                                
  200 CONTINUE                                          
      IA=1                                             
      IB=IA+(2*M-LA)*INC1                             
      JA=1                                           
      JB=JA+JINK                                    
C                                                  
      IF (LA.EQ.M) GO TO 290                      
C                                                
      DO 220 L=1,LA                                                    
      I=IBASE                                                         
      J=JBASE                                                        
CDIR$ IVDEP                                                         
      DO 210 IJK=1,LOT                                             
      C(JA+J)=A(IA+I)+A(IB+I)                                     
      C(JB+J)=A(IA+I)-A(IB+I)                                    
      I=I+INC3                                                  
      J=J+INC4                                                 
  210 CONTINUE                                                
      IBASE=IBASE+INC1                                       
      JBASE=JBASE+INC2                                      
  220 CONTINUE                                             
      IA=IA+IINK                                          
      IINK=2*IINK                                        
      IB=IB-IINK                                        
      IBASE=0                                          
      JBASE=JBASE+JUMP                                
      JUMP=2*JUMP+JINK                               
      IF (IA.EQ.IB) GO TO 260                                       
      DO 250 K=LA,KSTOP,LA                                         
      KB=K+K                                                      
      C1=TRIGS(KB+1)                                             
      S1=TRIGS(KB+2)                                            
      IBASE=0                                                  
      DO 240 L=1,LA                                           
      I=IBASE                                                
      J=JBASE                                               
CDIR$ IVDEP                                                
      DO 230 IJK=1,LOT                                    
      C(JA+J)=A(IA+I)+A(IB+I)                            
      D(JA+J)=B(IA+I)-B(IB+I)                           
      C(JB+J)=C1*(A(IA+I)-A(IB+I))-S1*(B(IA+I)+B(IB+I))
      D(JB+J)=S1*(A(IA+I)-A(IB+I))+C1*(B(IA+I)+B(IB+I))              
      I=I+INC3                                                      
      J=J+INC4                                                     
  230 CONTINUE                                                    
      IBASE=IBASE+INC1                                           
      JBASE=JBASE+INC2                                          
  240 CONTINUE                                                       
      IA=IA+IINK                                                    
      IB=IB-IINK                                                   
      JBASE=JBASE+JUMP                                            
  250 CONTINUE                                                   
      IF (IA.GT.IB) GO TO 900                                   
  260 CONTINUE                                                 
      IBASE=0                                                 
      DO 280 L=1,LA                                          
      I=IBASE                                               
      J=JBASE                                              
CDIR$ IVDEP                                               
      DO 270 IJK=1,LOT                                   
      C(JA+J)=A(IA+I)                                   
      C(JB+J)=-B(IA+I)                                 
      I=I+INC3                                        
      J=J+INC4                                       
  270 CONTINUE                                      
      IBASE=IBASE+INC1                             
      JBASE=JBASE+INC2                                             
  280 CONTINUE                                                    
      GO TO 900                                                  
C                                                               
  290 CONTINUE                                                 
      DO 294 L=1,LA                                           
      I=IBASE                                                
      J=JBASE                                               
CDIR$ IVDEP                                                
      DO 292 IJK=1,LOT                                    
      C(JA+J)=2.0*(A(IA+I)+A(IB+I))                      
      C(JB+J)=2.0*(A(IA+I)-A(IB+I))                     
      I=I+INC3                                         
      J=J+INC4                                        
  292 CONTINUE                                       
      IBASE=IBASE+INC1                              
      JBASE=JBASE+INC2                             
  294 CONTINUE                                    
      GO TO 900                                  
C                                                                   
C     CODING FOR FACTOR 3                                          
C     -------------------                                         
  300 CONTINUE                                                   
      IA=1                                                      
      IB=IA+(2*M-LA)*INC1                                      
      IC=IB                                                   
      JA=1                                                   
      JB=JA+JINK                                            
      JC=JB+JINK                                           
C                                                         
      IF (LA.EQ.M) GO TO 390                             
C                                                       
      DO 320 L=1,LA                                    
      I=IBASE                                         
      J=JBASE                                        
CDIR$ IVDEP                                         
      DO 310 IJK=1,LOT                             
      C(JA+J)=A(IA+I)+A(IB+I)                                       
      C(JB+J)=(A(IA+I)-0.5*A(IB+I))-(SIN60*(B(IB+I)))              
      C(JC+J)=(A(IA+I)-0.5*A(IB+I))+(SIN60*(B(IB+I)))             
      I=I+INC3                                                   
      J=J+INC4                                                  
  310 CONTINUE                                                 
      IBASE=IBASE+INC1                                        
      JBASE=JBASE+INC2                                       
  320 CONTINUE                                              
      IA=IA+IINK                                           
      IINK=2*IINK                                         
      IB=IB+IINK                                         
      IC=IC-IINK                                        
      JBASE=JBASE+JUMP                                 
      JUMP=2*JUMP+JINK                                
      IF (IA.EQ.IC) GO TO 360                        
      DO 350 K=LA,KSTOP,LA                          
      KB=K+K                                       
      KC=KB+KB                                                      
      C1=TRIGS(KB+1)                                               
      S1=TRIGS(KB+2)                                              
      C2=TRIGS(KC+1)                                             
      S2=TRIGS(KC+2)                                            
      IBASE=0                                                  
      DO 340 L=1,LA                                           
      I=IBASE                                                
      J=JBASE                                               
CDIR$ IVDEP                                                
      DO 330 IJK=1,LOT                                    
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IC+I))                  
      D(JA+J)=B(IA+I)+(B(IB+I)-B(IC+I))                 
      C(JB+J)=                                         
     *    C1*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))-(SIN60*(B(IB+I)+B(IC+I))))
     *   -S1*((B(IA+I)-0.5*(B(IB+I)-B(IC+I)))+(SIN60*(A(IB+I)-A(IC+I))))
      D(JB+J)=                                                         
     *    S1*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))-(SIN60*(B(IB+I)+B(IC+I))))
     *   +C1*((B(IA+I)-0.5*(B(IB+I)-B(IC+I)))+(SIN60*(A(IB+I)-A(IC+I))))
      C(JC+J)=                                                         
     *    C2*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))+(SIN60*(B(IB+I)+B(IC+I))))
     *   -S2*((B(IA+I)-0.5*(B(IB+I)-B(IC+I)))-(SIN60*(A(IB+I)-A(IC+I))))
      D(JC+J)=                                                         
     *    S2*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))+(SIN60*(B(IB+I)+B(IC+I))))
     *   +C2*((B(IA+I)-0.5*(B(IB+I)-B(IC+I)))-(SIN60*(A(IB+I)-A(IC+I))))
      I=I+INC3                                                         
      J=J+INC4                                                        
  330 CONTINUE                                                       
      IBASE=IBASE+INC1                                              
      JBASE=JBASE+INC2                                             
  340 CONTINUE                                                    
      IA=IA+IINK                                                 
      IB=IB+IINK                                                
      IC=IC-IINK                                               
      JBASE=JBASE+JUMP                                        
  350 CONTINUE                                               
      IF (IA.GT.IC) GO TO 900                               
  360 CONTINUE                                                        
      IBASE=0                                                        
      DO 380 L=1,LA                                                 
      I=IBASE                                                      
      J=JBASE                                                     
CDIR$ IVDEP                                                      
      DO 370 IJK=1,LOT                                          
      C(JA+J)=A(IA+I)+A(IB+I)                                  
      C(JB+J)=(0.5*A(IA+I)-A(IB+I))-(SIN60*B(IA+I))           
      C(JC+J)=-(0.5*A(IA+I)-A(IB+I))-(SIN60*B(IA+I))         
      I=I+INC3                                              
      J=J+INC4                                             
  370 CONTINUE                                            
      IBASE=IBASE+INC1                                   
      JBASE=JBASE+INC2                                  
  380 CONTINUE                                         
      GO TO 900                                       
C                                                    
  390 CONTINUE                                      
      SSIN60=2.0*SIN60                             
      DO 394 L=1,LA                                                 
      I=IBASE                                                      
      J=JBASE                                                     
CDIR$ IVDEP                                                      
      DO 392 IJK=1,LOT                                          
      C(JA+J)=2.0*(A(IA+I)+A(IB+I))                            
      C(JB+J)=(2.0*A(IA+I)-A(IB+I))-(SSIN60*B(IB+I))          
      C(JC+J)=(2.0*A(IA+I)-A(IB+I))+(SSIN60*B(IB+I))         
      I=I+INC3                                              
      J=J+INC4                                             
  392 CONTINUE                                            
      IBASE=IBASE+INC1                                   
      JBASE=JBASE+INC2                                  
  394 CONTINUE                                         
      GO TO 900                                       
C                                                    
C     CODING FOR FACTOR 4                           
C     -------------------                          
  400 CONTINUE                                    
      IA=1                                       
      IB=IA+(2*M-LA)*INC1                                           
      IC=IB+2*M*INC1                                               
      ID=IB                                                       
      JA=1                                                       
      JB=JA+JINK                                                
      JC=JB+JINK                                               
      JD=JC+JINK                                              
C                                                            
      IF (LA.EQ.M) GO TO 490                                
C                                                          
      DO 420 L=1,LA                                       
      I=IBASE                                            
      J=JBASE                                           
CDIR$ IVDEP                                            
      DO 410 IJK=1,LOT                                
      C(JA+J)=(A(IA+I)+A(IC+I))+A(IB+I)              
      C(JB+J)=(A(IA+I)-A(IC+I))-B(IB+I)             
      C(JC+J)=(A(IA+I)+A(IC+I))-A(IB+I)            
      C(JD+J)=(A(IA+I)-A(IC+I))+B(IB+I)           
      I=I+INC3                                   
      J=J+INC4                                                      
  410 CONTINUE                                                     
      IBASE=IBASE+INC1                                            
      JBASE=JBASE+INC2                                           
  420 CONTINUE                                                  
      IA=IA+IINK                                               
      IINK=2*IINK                                             
      IB=IB+IINK                                             
      IC=IC-IINK                                            
      ID=ID-IINK                                           
      JBASE=JBASE+JUMP                                    
      JUMP=2*JUMP+JINK                                   
      IF (IB.EQ.IC) GO TO 460                           
      DO 450 K=LA,KSTOP,LA                             
      KB=K+K                                          
      KC=KB+KB                                       
      KD=KC+KB                                      
      C1=TRIGS(KB+1)                               
      S1=TRIGS(KB+2)                              
      C2=TRIGS(KC+1)                             
      S2=TRIGS(KC+2)                                             
      C3=TRIGS(KD+1)                                            
      S3=TRIGS(KD+2)                                           
      IBASE=0                                                 
      DO 440 L=1,LA                                          
      I=IBASE                                               
      J=JBASE                                              
CDIR$ IVDEP                                               
      DO 430 IJK=1,LOT                                   
      C(JA+J)=(A(IA+I)+A(IC+I))+(A(IB+I)+A(ID+I))       
      D(JA+J)=(B(IA+I)-B(IC+I))+(B(IB+I)-B(ID+I))      
      C(JC+J)=                                        
     *    C2*((A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I)))   
     *   -S2*((B(IA+I)-B(IC+I))-(B(IB+I)-B(ID+I)))  
      D(JC+J)=                                     
     *    S2*((A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I)))                  
     *   +C2*((B(IA+I)-B(IC+I))-(B(IB+I)-B(ID+I)))                 
      C(JB+J)=                                                    
     *    C1*((A(IA+I)-A(IC+I))-(B(IB+I)+B(ID+I)))               
     *   -S1*((B(IA+I)+B(IC+I))+(A(IB+I)-A(ID+I)))              
      D(JB+J)=                                                 
     *    S1*((A(IA+I)-A(IC+I))-(B(IB+I)+B(ID+I)))            
     *   +C1*((B(IA+I)+B(IC+I))+(A(IB+I)-A(ID+I)))           
      C(JD+J)=                                              
     *    C3*((A(IA+I)-A(IC+I))+(B(IB+I)+B(ID+I)))         
     *   -S3*((B(IA+I)+B(IC+I))-(A(IB+I)-A(ID+I)))        
      D(JD+J)=                                           
     *    S3*((A(IA+I)-A(IC+I))+(B(IB+I)+B(ID+I)))      
     *   +C3*((B(IA+I)+B(IC+I))-(A(IB+I)-A(ID+I)))     
      I=I+INC3                                        
      J=J+INC4                                       
  430 CONTINUE                                      
      IBASE=IBASE+INC1                             
      JBASE=JBASE+INC2                            
  440 CONTINUE                                   
      IA=IA+IINK                                
      IB=IB+IINK                               
      IC=IC-IINK                              
      ID=ID-IINK                             
      JBASE=JBASE+JUMP                                            
  450 CONTINUE                                                   
      IF (IB.GT.IC) GO TO 900                                   
  460 CONTINUE                                                 
      IBASE=0                                                 
      SIN45=SQRT(0.5)                                        
      DO 480 L=1,LA                                         
      I=IBASE                                              
      J=JBASE                                             
CDIR$ IVDEP                                              
      DO 470 IJK=1,LOT                                  
      C(JA+J)=A(IA+I)+A(IB+I)                          
      C(JB+J)=SIN45*((A(IA+I)-A(IB+I))-(B(IA+I)+B(IB+I)))            
      C(JC+J)=B(IB+I)-B(IA+I)                                       
      C(JD+J)=-SIN45*((A(IA+I)-A(IB+I))+(B(IA+I)+B(IB+I)))         
      I=I+INC3                                                    
      J=J+INC4                                                   
  470 CONTINUE                                                  
      IBASE=IBASE+INC1                                         
      JBASE=JBASE+INC2                                             
  480 CONTINUE                                                    
      GO TO 900                                                  
C                                                               
  490 CONTINUE                                                 
      DO 494 L=1,LA                                           
      I=IBASE                                                
      J=JBASE                                               
CDIR$ IVDEP                                                
      DO 492 IJK=1,LOT                                    
      C(JA+J)=2.0*((A(IA+I)+A(IC+I))+A(IB+I))            
      C(JB+J)=2.0*((A(IA+I)-A(IC+I))-B(IB+I))           
      C(JC+J)=2.0*((A(IA+I)+A(IC+I))-A(IB+I))          
      C(JD+J)=2.0*((A(IA+I)-A(IC+I))+B(IB+I))         
      I=I+INC3                                       
      J=J+INC4                                      
  492 CONTINUE                                     
      IBASE=IBASE+INC1                            
      JBASE=JBASE+INC2                           
  494 CONTINUE                                  
      GO TO 900                                                     
C                                                                  
C     CODING FOR FACTOR 5                                         
C     -------------------                                        
  500 CONTINUE                                                  
      IA=1                                                     
      IB=IA+(2*M-LA)*INC1                                     
      IC=IB+2*M*INC1                                         
      ID=IC                                                 
      IE=IB                                                
      JA=1                                                
      JB=JA+JINK                                         
      JC=JB+JINK                                        
      JD=JC+JINK                                       
      JE=JD+JINK                                      
C                                                    
      IF (LA.EQ.M) GO TO 590                        
C                                                  
      DO 520 L=1,LA                                                    
      I=IBASE                                                         
      J=JBASE                                                        
CDIR$ IVDEP                                                         
      DO 510 IJK=1,LOT                                             
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IC+I))                           
      C(JB+J)=((A(IA+I)-0.25*(A(IB+I)+A(IC+I)))+QRT5*(A(IB+I)-A(IC+I))) 
     *    -(SIN72*B(IB+I)+SIN36*B(IC+I))                               
      C(JC+J)=((A(IA+I)-0.25*(A(IB+I)+A(IC+I)))-QRT5*(A(IB+I)-A(IC+I))) 
     *    -(SIN36*B(IB+I)-SIN72*B(IC+I))                               
      C(JD+J)=((A(IA+I)-0.25*(A(IB+I)+A(IC+I)))-QRT5*(A(IB+I)-A(IC+I)))
     *    +(SIN36*B(IB+I)-SIN72*B(IC+I))                              
      C(JE+J)=((A(IA+I)-0.25*(A(IB+I)+A(IC+I)))+QRT5*(A(IB+I)-A(IC+I))) 
     *    +(SIN72*B(IB+I)+SIN36*B(IC+I))                               
      I=I+INC3                                                        
      J=J+INC4                                                       
  510 CONTINUE                                                      
      IBASE=IBASE+INC1                                             
      JBASE=JBASE+INC2                                            
  520 CONTINUE                                                         
      IA=IA+IINK                                                      
      IINK=2*IINK                                                    
      IB=IB+IINK                                                    
      IC=IC+IINK                                                   
      ID=ID-IINK                                                  
      IE=IE-IINK                                                 
      JBASE=JBASE+JUMP                                          
      JUMP=2*JUMP+JINK                                         
      IF (IB.EQ.ID) GO TO 560                                 
      DO 550 K=LA,KSTOP,LA                                   
      KB=K+K                                                
      KC=KB+KB                                             
      KD=KC+KB                                            
      KE=KD+KB                                           
      C1=TRIGS(KB+1)                                    
      S1=TRIGS(KB+2)                                   
      C2=TRIGS(KC+1)                                  
      S2=TRIGS(KC+2)                                 
      C3=TRIGS(KD+1)                                
      S3=TRIGS(KD+2)                                                 
      C4=TRIGS(KE+1)                                                
      S4=TRIGS(KE+2)                                               
      IBASE=0                                                     
      DO 540 L=1,LA                                              
      I=IBASE                                                   
      J=JBASE                                                  
CDIR$ IVDEP                                                   
      DO 530 IJK=1,LOT                                       
C                                                           
      A10(IJK)=(A(IA+I)-0.25*((A(IB+I)+A(IE+I))+(A(IC+I)+A(ID+I))))     
     *    +QRT5*((A(IB+I)+A(IE+I))-(A(IC+I)+A(ID+I)))                  
      A20(IJK)=(A(IA+I)-0.25*((A(IB+I)+A(IE+I))+(A(IC+I)+A(ID+I))))   
     *    -QRT5*((A(IB+I)+A(IE+I))-(A(IC+I)+A(ID+I)))                
      B10(IJK)=(B(IA+I)-0.25*((B(IB+I)-B(IE+I))+(B(IC+I)-B(ID+I)))) 
     *    +QRT5*((B(IB+I)-B(IE+I))-(B(IC+I)-B(ID+I)))              
      B20(IJK)=(B(IA+I)-0.25*((B(IB+I)-B(IE+I))+(B(IC+I)-B(ID+I))))   
     *    -QRT5*((B(IB+I)-B(IE+I))-(B(IC+I)-B(ID+I)))                
      A11(IJK)=SIN72*(B(IB+I)+B(IE+I))+SIN36*(B(IC+I)+B(ID+I))      
      A21(IJK)=SIN36*(B(IB+I)+B(IE+I))-SIN72*(B(IC+I)+B(ID+I))     
      B11(IJK)=SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I))       
      B21(IJK)=SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I))      
C                                                                  
      C(JA+J)=A(IA+I)+((A(IB+I)+A(IE+I))+(A(IC+I)+A(ID+I)))       
      D(JA+J)=B(IA+I)+((B(IB+I)-B(IE+I))+(B(IC+I)-B(ID+I)))      
      C(JB+J)=C1*(A10(IJK)-A11(IJK))-S1*(B10(IJK)+B11(IJK))     
      D(JB+J)=S1*(A10(IJK)-A11(IJK))+C1*(B10(IJK)+B11(IJK))    
      C(JE+J)=C4*(A10(IJK)+A11(IJK))-S4*(B10(IJK)-B11(IJK))   
      D(JE+J)=S4*(A10(IJK)+A11(IJK))+C4*(B10(IJK)-B11(IJK))  
      C(JC+J)=C2*(A20(IJK)-A21(IJK))-S2*(B20(IJK)+B21(IJK))            
      D(JC+J)=S2*(A20(IJK)-A21(IJK))+C2*(B20(IJK)+B21(IJK))           
      C(JD+J)=C3*(A20(IJK)+A21(IJK))-S3*(B20(IJK)-B21(IJK))          
      D(JD+J)=S3*(A20(IJK)+A21(IJK))+C3*(B20(IJK)-B21(IJK))         
C                                                                  
      I=I+INC3                                                    
      J=J+INC4                                                   
  530 CONTINUE                                                  
      IBASE=IBASE+INC1                                         
      JBASE=JBASE+INC2                                        
  540 CONTINUE                                               
      IA=IA+IINK                                                     
      IB=IB+IINK                                                    
      IC=IC+IINK                                                   
      ID=ID-IINK                                                  
      IE=IE-IINK                                                 
      JBASE=JBASE+JUMP                                          
  550 CONTINUE                                                 
      IF (IB.GT.ID) GO TO 900                                 
  560 CONTINUE                                               
      IBASE=0                                               
      DO 580 L=1,LA                                        
      I=IBASE                                             
      J=JBASE                                            
CDIR$ IVDEP                                             
      DO 570 IJK=1,LOT                                 
      C(JA+J)=(A(IA+I)+A(IB+I))+A(IC+I)               
      C(JB+J)=(QRT5*(A(IA+I)-A(IB+I))+(0.25*(A(IA+I)+A(IB+I))-A(IC+I)))
     *    -(SIN36*B(IA+I)+SIN72*B(IB+I))                              
      C(JE+J)=-(QRT5*(A(IA+I)-A(IB+I))+(0.25*(A(IA+I)+A(IB+I))-A(IC+I)))
     *    -(SIN36*B(IA+I)+SIN72*B(IB+I))                                
      C(JC+J)=(QRT5*(A(IA+I)-A(IB+I))-(0.25*(A(IA+I)+A(IB+I))-A(IC+I)))
     *    -(SIN72*B(IA+I)-SIN36*B(IB+I))                              
      C(JD+J)=-(QRT5*(A(IA+I)-A(IB+I))-(0.25*(A(IA+I)+A(IB+I))-A(IC+I)))
     *    -(SIN72*B(IA+I)-SIN36*B(IB+I))                               
      I=I+INC3                                                        
      J=J+INC4                                                       
  570 CONTINUE                                                      
      IBASE=IBASE+INC1                                             
      JBASE=JBASE+INC2                                            
  580 CONTINUE                                                   
      GO TO 900                                                 
C                                                              
  590 CONTINUE                                                
      QQRT5=2.0*QRT5                                         
      SSIN36=2.0*SIN36                                      
      SSIN72=2.0*SIN72                                     
      DO 594 L=1,LA                                       
      I=IBASE                                                          
      J=JBASE                                                         
CDIR$ IVDEP                                                          
      DO 592 IJK=1,LOT                                              
      C(JA+J)=2.0*(A(IA+I)+(A(IB+I)+A(IC+I)))                      
      C(JB+J)=(2.0*(A(IA+I)-0.25*(A(IB+I)+A(IC+I)))               
     *    +QQRT5*(A(IB+I)-A(IC+I)))-(SSIN72*B(IB+I)+SSIN36*B(IC+I))    
      C(JC+J)=(2.0*(A(IA+I)-0.25*(A(IB+I)+A(IC+I)))                   
     *    -QQRT5*(A(IB+I)-A(IC+I)))-(SSIN36*B(IB+I)-SSIN72*B(IC+I))  
      C(JD+J)=(2.0*(A(IA+I)-0.25*(A(IB+I)+A(IC+I)))                 
     *    -QQRT5*(A(IB+I)-A(IC+I)))+(SSIN36*B(IB+I)-SSIN72*B(IC+I))   
      C(JE+J)=(2.0*(A(IA+I)-0.25*(A(IB+I)+A(IC+I)))                  
     *    +QQRT5*(A(IB+I)-A(IC+I)))+(SSIN72*B(IB+I)+SSIN36*B(IC+I)) 
      I=I+INC3                                                     
      J=J+INC4                                                    
  592 CONTINUE                                                   
      IBASE=IBASE+INC1                                          
      JBASE=JBASE+INC2                                         
  594 CONTINUE                                                
      GO TO 900                                              
C                                                           
C     CODING FOR FACTOR 6                                             
C     -------------------                                            
  600 CONTINUE                                                      
      IA=1                                                         
      IB=IA+(2*M-LA)*INC1                                         
      IC=IB+2*M*INC1                                             
      ID=IC+2*M*INC1                                            
      IE=IC                                                    
      IF=IB                                                   
      JA=1                                                   
      JB=JA+JINK                                            
      JC=JB+JINK                                           
      JD=JC+JINK                                          
      JE=JD+JINK                                         
      JF=JE+JINK                                        
C                                                      
      IF (LA.EQ.M) GO TO 690                          
C                                                    
      DO 620 L=1,LA                                 
      I=IBASE                                      
      J=JBASE                                     
CDIR$ IVDEP                                                          
      DO 610 IJK=1,LOT                                              
      C(JA+J)=(A(IA+I)+A(ID+I))+(A(IB+I)+A(IC+I))                  
      C(JD+J)=(A(IA+I)-A(ID+I))-(A(IB+I)-A(IC+I))                 
      C(JB+J)=((A(IA+I)-A(ID+I))+0.5*(A(IB+I)-A(IC+I)))          
     *    -(SIN60*(B(IB+I)+B(IC+I)))                            
      C(JF+J)=((A(IA+I)-A(ID+I))+0.5*(A(IB+I)-A(IC+I)))        
     *    +(SIN60*(B(IB+I)+B(IC+I)))                          
      C(JC+J)=((A(IA+I)+A(ID+I))-0.5*(A(IB+I)+A(IC+I)))      
     *    -(SIN60*(B(IB+I)-B(IC+I)))                        
      C(JE+J)=((A(IA+I)+A(ID+I))-0.5*(A(IB+I)+A(IC+I)))    
     *    +(SIN60*(B(IB+I)-B(IC+I)))                      
      I=I+INC3                                           
      J=J+INC4                                          
  610 CONTINUE                                         
      IBASE=IBASE+INC1                                
      JBASE=JBASE+INC2                               
  620 CONTINUE                                      
      IA=IA+IINK                                   
      IINK=2*IINK                                 
      IB=IB+IINK                                                      
      IC=IC+IINK                                                     
      ID=ID-IINK                                                    
      IE=IE-IINK                                                   
      IF=IF-IINK                                                  
      JBASE=JBASE+JUMP                                           
      JUMP=2*JUMP+JINK                                          
      IF (IC.EQ.ID) GO TO 660                                  
      DO 650 K=LA,KSTOP,LA                                    
      KB=K+K                                                 
      KC=KB+KB                                              
      KD=KC+KB                                             
      KE=KD+KB                                            
      KF=KE+KB                                           
      C1=TRIGS(KB+1)                                    
      S1=TRIGS(KB+2)                                   
      C2=TRIGS(KC+1)                                  
      S2=TRIGS(KC+2)                                 
      C3=TRIGS(KD+1)                                
      S3=TRIGS(KD+2)                               
      C4=TRIGS(KE+1)                              
      S4=TRIGS(KE+2)                                               
      C5=TRIGS(KF+1)                                              
      S5=TRIGS(KF+2)                                             
      IBASE=0                                                   
      DO 640 L=1,LA                                            
      I=IBASE                                                 
      J=JBASE                                                
CDIR$ IVDEP                                                 
      DO 630 IJK=1,LOT                                     
C                                                         
      A11(IJK)= (A(IE+I)+A(IB+I))+(A(IC+I)+A(IF+I))      
      A20(IJK)=(A(IA+I)+A(ID+I))-0.5*A11(IJK)                        
      A21(IJK)=SIN60*((A(IE+I)+A(IB+I))-(A(IC+I)+A(IF+I)))          
      B11(IJK)= (B(IB+I)-B(IE+I))+(B(IC+I)-B(IF+I))                
      B20(IJK)=(B(IA+I)-B(ID+I))-0.5*B11(IJK)                     
      B21(IJK)=SIN60*((B(IB+I)-B(IE+I))-(B(IC+I)-B(IF+I)))       
C                                                               
      C(JA+J)=(A(IA+I)+A(ID+I))+A11(IJK)                       
      D(JA+J)=(B(IA+I)-B(ID+I))+B11(IJK)                      
      C(JC+J)=C2*(A20(IJK)-B21(IJK))-S2*(B20(IJK)+A21(IJK))  
      D(JC+J)=S2*(A20(IJK)-B21(IJK))+C2*(B20(IJK)+A21(IJK))           
      C(JE+J)=C4*(A20(IJK)+B21(IJK))-S4*(B20(IJK)-A21(IJK))          
      D(JE+J)=S4*(A20(IJK)+B21(IJK))+C4*(B20(IJK)-A21(IJK))         
C                                                                  
      A11(IJK)=(A(IE+I)-A(IB+I))+(A(IC+I)-A(IF+I))                
      B11(IJK)=(B(IE+I)+B(IB+I))-(B(IC+I)+B(IF+I))               
      A20(IJK)=(A(IA+I)-A(ID+I))-0.5*A11(IJK)                   
      A21(IJK)=SIN60*((A(IE+I)-A(IB+I))-(A(IC+I)-A(IF+I)))     
      B20(IJK)=(B(IA+I)+B(ID+I))+0.5*B11(IJK)                 
      B21(IJK)=SIN60*((B(IE+I)+B(IB+I))+(B(IC+I)+B(IF+I)))   
C                                                           
      C(JD+J)=                                             
     *  C3*((A(IA+I)-A(ID+I))+A11(IJK))-S3*((B(IA+I)+B(ID+I))-B11(IJK))
      D(JD+J)=                                                        
     *  S3*((A(IA+I)-A(ID+I))+A11(IJK))+C3*((B(IA+I)+B(ID+I))-B11(IJK)) 
      C(JB+J)=C1*(A20(IJK)-B21(IJK))-S1*(B20(IJK)-A21(IJK))            
      D(JB+J)=S1*(A20(IJK)-B21(IJK))+C1*(B20(IJK)-A21(IJK))           
      C(JF+J)=C5*(A20(IJK)+B21(IJK))-S5*(B20(IJK)+A21(IJK))          
      D(JF+J)=S5*(A20(IJK)+B21(IJK))+C5*(B20(IJK)+A21(IJK))         
C                                                                  
      I=I+INC3                                                        
      J=J+INC4                                                       
  630 CONTINUE                                                      
      IBASE=IBASE+INC1                                             
      JBASE=JBASE+INC2                                            
  640 CONTINUE                                                   
      IA=IA+IINK                                                
      IB=IB+IINK                                               
      IC=IC+IINK                                              
      ID=ID-IINK                                             
      IE=IE-IINK                                            
      IF=IF-IINK                                           
      JBASE=JBASE+JUMP                                    
  650 CONTINUE                                           
      IF (IC.GT.ID) GO TO 900                           
  660 CONTINUE                                         
      IBASE=0                                         
      DO 680 L=1,LA                                  
      I=IBASE                                                         
      J=JBASE                                                        
CDIR$ IVDEP                                                         
      DO 670 IJK=1,LOT                                             
      C(JA+J)=A(IB+I)+(A(IA+I)+A(IC+I))                           
      C(JD+J)=B(IB+I)-(B(IA+I)+B(IC+I))                          
      C(JB+J)=(SIN60*(A(IA+I)-A(IC+I)))-(0.5*(B(IA+I)+B(IC+I))+B(IB+I))
      C(JF+J)=-(SIN60*(A(IA+I)-A(IC+I)))-(0.5*(B(IA+I)+B(IC+I))+B(IB+I))
      C(JC+J)=SIN60*(B(IC+I)-B(IA+I))+(0.5*(A(IA+I)+A(IC+I))-A(IB+I))  
      C(JE+J)=SIN60*(B(IC+I)-B(IA+I))-(0.5*(A(IA+I)+A(IC+I))-A(IB+I)) 
      I=I+INC3                                                       
      J=J+INC4                                                      
  670 CONTINUE                                                     
      IBASE=IBASE+INC1                                            
      JBASE=JBASE+INC2                                           
  680 CONTINUE                                                  
      GO TO 900                                                
C                                                             
  690 CONTINUE                                               
      SSIN60=2.0*SIN60                                                 
      DO 694 L=1,LA                                                   
      I=IBASE                                                        
      J=JBASE                                                       
CDIR$ IVDEP                                                        
      DO 692 IJK=1,LOT                                            
      C(JA+J)=(2.0*(A(IA+I)+A(ID+I)))+(2.0*(A(IB+I)+A(IC+I)))    
      C(JD+J)=(2.0*(A(IA+I)-A(ID+I)))-(2.0*(A(IB+I)-A(IC+I)))   
      C(JB+J)=(2.0*(A(IA+I)-A(ID+I))+(A(IB+I)-A(IC+I)))        
     *    -(SSIN60*(B(IB+I)+B(IC+I)))                         
      C(JF+J)=(2.0*(A(IA+I)-A(ID+I))+(A(IB+I)-A(IC+I)))               
     *    +(SSIN60*(B(IB+I)+B(IC+I)))                                
      C(JC+J)=(2.0*(A(IA+I)+A(ID+I))-(A(IB+I)+A(IC+I)))             
     *    -(SSIN60*(B(IB+I)-B(IC+I)))                              
      C(JE+J)=(2.0*(A(IA+I)+A(ID+I))-(A(IB+I)+A(IC+I)))           
     *    +(SSIN60*(B(IB+I)-B(IC+I)))                            
      I=I+INC3                                                  
      J=J+INC4                                                 
  692 CONTINUE                                                
      IBASE=IBASE+INC1                                       
      JBASE=JBASE+INC2                                                 
  694 CONTINUE                                                        
      GO TO 900                                                      
C                                                                   
C     CODING FOR FACTOR 8                                          
C     -------------------                                         
  800 CONTINUE                                                   
      IBAD=3                                                    
      IF (LA.NE.M) GO TO 910                                   
      IA=1                                                    
      IB=IA+LA*INC1                                          
      IC=IB+2*LA*INC1                                       
      ID=IC+2*LA*INC1                                      
      IE=ID+2*LA*INC1                                     
      JA=1                                               
      JB=JA+JINK                                        
      JC=JB+JINK                                       
      JD=JC+JINK                                      
      JE=JD+JINK                                     
      JF=JE+JINK                                    
      JG=JF+JINK                                   
      JH=JG+JINK                                  
      SSIN45=SQRT(2.0)                                              
C                                                                  
      DO 820 L=1,LA                                               
      I=IBASE                                                    
      J=JBASE                                                   
CDIR$ IVDEP                                                    
      DO 810 IJK=1,LOT                                               
      C(JA+J)=2.0*(((A(IA+I)+A(IE+I))+A(IC+I))+(A(IB+I)+A(ID+I)))   
      C(JE+J)=2.0*(((A(IA+I)+A(IE+I))+A(IC+I))-(A(IB+I)+A(ID+I)))  
      C(JC+J)=2.0*(((A(IA+I)+A(IE+I))-A(IC+I))-(B(IB+I)-B(ID+I))) 
      C(JG+J)=2.0*(((A(IA+I)+A(IE+I))-A(IC+I))+(B(IB+I)-B(ID+I)))    
      C(JB+J)=2.0*((A(IA+I)-A(IE+I))-B(IC+I))                       
     *    +SSIN45*((A(IB+I)-A(ID+I))-(B(IB+I)+B(ID+I)))            
      C(JF+J)=2.0*((A(IA+I)-A(IE+I))-B(IC+I))                     
     *    -SSIN45*((A(IB+I)-A(ID+I))-(B(IB+I)+B(ID+I)))          
      C(JD+J)=2.0*((A(IA+I)-A(IE+I))+B(IC+I))                   
     *    -SSIN45*((A(IB+I)-A(ID+I))+(B(IB+I)+B(ID+I)))        
      C(JH+J)=2.0*((A(IA+I)-A(IE+I))+B(IC+I))                 
     *    +SSIN45*((A(IB+I)-A(ID+I))+(B(IB+I)+B(ID+I)))              
      I=I+INC3                                                      
      J=J+INC4                                                     
  810 CONTINUE                                                    
      IBASE=IBASE+INC1                                           
      JBASE=JBASE+INC2                                          
  820 CONTINUE                                                 
C                                                             
C     RETURN                                                 
C     ------                                                
  900 CONTINUE                                             
      IBAD=0                                              
  910 CONTINUE                                           
      IERR=IBAD                                         
      RETURN                                           
      END                                             
C     SUBROUTINE 'QPASSM' - PERFORMS ONE PASS THROUGH DATA AS PART     
C     OF MULTIPLE REAL FFT (FOURIER ANALYSIS) ROUTINE               
C                                                                  
C     A IS FIRST REAL INPUT VECTOR                                
C         EQUIVALENCE B(1) WITH A(IFAC*LA*INC1+1)                
C     C IS FIRST REAL OUTPUT VECTOR                             
C         EQUIVALENCE D(1) WITH C(LA*INC2+1)                   
C     TRIGS IS A PRECALCULATED LIST OF SINES & COSINES        
C     INC1 IS THE ADDRESSING INCREMENT FOR A                 
C     INC2 IS THE ADDRESSING INCREMENT FOR C                
C     INC3 IS THE INCREMENT BETWEEN INPUT VECTORS A        
C     INC4 IS THE INCREMENT BETWEEN OUTPUT VECTORS C      
C     LOT IS THE NUMBER OF VECTORS                       
C     N IS THE LENGTH OF THE VECTORS                    
C     IFAC IS THE CURRENT FACTOR OF N                  
C     LA = N/(PRODUCT OF FACTORS USED SO FAR)         
C     IERR IS AN ERROR INDICATOR:                                   
C              0 - PASS COMPLETED WITHOUT ERROR                    
C              1 - LOT GREATER THAN 64                            
C              2 - IFAC NOT CATERED FOR                          
C              3 - IFAC ONLY CATERED FOR IF LA=N/IFAC           
C                                                                       
C-----------------------------------------------------------------------
C                                                                      
      SUBROUTINE QPASSM(A,B,C,D,TRIGS,INC1,INC2,INC3,INC4,LOT,N,IFAC, 
     *    LA,IERR)                                                   
      DIMENSION A(N),B(N),C(N),D(N),TRIGS(N)                        
C                                                                  
      DATA SIN36/0.587785252292473/,SIN72/0.951056516295154/,     
     *    QRT5/0.559016994374947/,SIN60/0.866025403784437/       
C                                                               
      M=N/IFAC                                                 
      IINK=LA*INC1                                            
      JINK=LA*INC2                                           
      IJUMP=(IFAC-1)*IINK                                   
      KSTOP=(N-IFAC)/(2*IFAC)                              
C                                                         
      IBAD=1                                                          
      IF (LOT.GT.64) GO TO 910                                       
      IBASE=0                                                       
      JBASE=0                                                      
      IGO=IFAC-1                                                  
      IF (IGO.EQ.7) IGO=6                                        
      IBAD=2                                                    
      IF (IGO.LT.1.OR.IGO.GT.6) GO TO 910                      
      GO TO (200,300,400,500,600,800),IGO                     
C                                                            
C     CODING FOR FACTOR 2                                   
C     -------------------                                  
  200 CONTINUE                                            
      IA=1                                               
      IB=IA+IINK                                        
      JA=1                                             
      JB=JA+(2*M-LA)*INC2                             
C                                                    
      IF (LA.EQ.M) GO TO 290                        
C                                                                     
      DO 220 L=1,LA                                                  
      I=IBASE                                                       
      J=JBASE                                                      
CDIR$ IVDEP                                                       
      DO 210 IJK=1,LOT                                           
      C(JA+J)=A(IA+I)+A(IB+I)                                   
      C(JB+J)=A(IA+I)-A(IB+I)                                  
      I=I+INC3                                                
      J=J+INC4                                               
  210 CONTINUE                                              
      IBASE=IBASE+INC1                                     
      JBASE=JBASE+INC2                                    
  220 CONTINUE                                           
      JA=JA+JINK                                        
      JINK=2*JINK                                      
      JB=JB-JINK                                      
      IBASE=IBASE+IJUMP                              
      IJUMP=2*IJUMP+IINK                            
      IF (JA.EQ.JB) GO TO 260                      
      DO 250 K=LA,KSTOP,LA                                           
      KB=K+K                                                        
      C1=TRIGS(KB+1)                                               
      S1=TRIGS(KB+2)                                              
      JBASE=0                                                    
      DO 240 L=1,LA                                             
      I=IBASE                                                  
      J=JBASE                                                 
CDIR$ IVDEP                                                  
      DO 230 IJK=1,LOT                                      
      C(JA+J)=A(IA+I)+(C1*A(IB+I)+S1*B(IB+I))              
      C(JB+J)=A(IA+I)-(C1*A(IB+I)+S1*B(IB+I))             
      D(JA+J)=(C1*B(IB+I)-S1*A(IB+I))+B(IA+I)            
      D(JB+J)=(C1*B(IB+I)-S1*A(IB+I))-B(IA+I)           
      I=I+INC3                                         
      J=J+INC4                                        
  230 CONTINUE                                       
      IBASE=IBASE+INC1                              
      JBASE=JBASE+INC2                             
  240 CONTINUE                                    
      IBASE=IBASE+IJUMP                          
      JA=JA+JINK                                                    
      JB=JB-JINK                                                   
  250 CONTINUE                                                    
      IF (JA.GT.JB) GO TO 900                                    
  260 CONTINUE                                                  
      JBASE=0                                                  
      DO 280 L=1,LA                                           
      I=IBASE                                                
      J=JBASE                                               
CDIR$ IVDEP                                                
      DO 270 IJK=1,LOT                                    
      C(JA+J)=A(IA+I)                                    
      D(JA+J)=-A(IB+I)                                  
      I=I+INC3                                         
      J=J+INC4                                        
  270 CONTINUE                                       
      IBASE=IBASE+INC1                              
      JBASE=JBASE+INC2                             
  280 CONTINUE                                    
      GO TO 900                                  
C                                                                     
  290 CONTINUE                                                       
      Z=1.0/FLOAT(N)                                                
      DO 294 L=1,LA                                                
      I=IBASE                                                     
      J=JBASE                                                    
CDIR$ IVDEP                                                     
      DO 292 IJK=1,LOT                                         
      C(JA+J)=Z*(A(IA+I)+A(IB+I))                             
      C(JB+J)=Z*(A(IA+I)-A(IB+I))                            
      I=I+INC3                                              
      J=J+INC4                                             
  292 CONTINUE                                            
      IBASE=IBASE+INC1                                   
      JBASE=JBASE+INC2                                  
  294 CONTINUE                                         
      GO TO 900                                       
C                                                    
C     CODING FOR FACTOR 3                           
C     -------------------                          
  300 CONTINUE                                                      
      IA=1                                                         
      IB=IA+IINK                                                  
      IC=IB+IINK                                                 
      JA=1                                                      
      JB=JA+(2*M-LA)*INC2                                      
      JC=JB                                                   
C                                                            
      IF (LA.EQ.M) GO TO 390                                
C                                                          
      DO 320 L=1,LA                                       
      I=IBASE                                            
      J=JBASE                                           
CDIR$ IVDEP                                            
      DO 310 IJK=1,LOT                                
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IC+I))              
      C(JB+J)=A(IA+I)-0.5*(A(IB+I)+A(IC+I))         
      D(JB+J)=SIN60*(A(IC+I)-A(IB+I))              
      I=I+INC3                                    
      J=J+INC4                                   
  310 CONTINUE                                                       
      IBASE=IBASE+INC1                                              
      JBASE=JBASE+INC2                                             
  320 CONTINUE                                                    
      JA=JA+JINK                                                 
      JINK=2*JINK                                               
      JB=JB+JINK                                               
      JC=JC-JINK                                              
      IBASE=IBASE+IJUMP                                      
      IJUMP=2*IJUMP+IINK                                    
      IF (JA.EQ.JC) GO TO 360                              
      DO 350 K=LA,KSTOP,LA                                
      KB=K+K                                             
      KC=KB+KB                                          
      C1=TRIGS(KB+1)                                   
      S1=TRIGS(KB+2)                                  
      C2=TRIGS(KC+1)                                 
      S2=TRIGS(KC+2)                                
      JBASE=0                                      
      DO 340 L=1,LA                               
      I=IBASE                                                         
      J=JBASE                                                        
CDIR$ IVDEP                                                         
      DO 330 IJK=1,LOT                                             
      A1=(C1*A(IB+I)+S1*B(IB+I))+(C2*A(IC+I)+S2*B(IC+I))          
      B1=(C1*B(IB+I)-S1*A(IB+I))+(C2*B(IC+I)-S2*A(IC+I))         
      A2=A(IA+I)-0.5*A1                                         
      B2=B(IA+I)-0.5*B1                                               
      A3=SIN60*((C1*A(IB+I)+S1*B(IB+I))-(C2*A(IC+I)+S2*B(IC+I)))     
      B3=SIN60*((C1*B(IB+I)-S1*A(IB+I))-(C2*B(IC+I)-S2*A(IC+I)))    
      C(JA+J)=A(IA+I)+A1                                           
      D(JA+J)=B(IA+I)+B1                                          
      C(JB+J)=A2+B3                                              
      D(JB+J)=B2-A3                                             
      C(JC+J)=A2-B3                                            
      D(JC+J)=-(B2+A3)                                        
      I=I+INC3                                               
      J=J+INC4                                                       
  330 CONTINUE                                                      
      IBASE=IBASE+INC1                                             
      JBASE=JBASE+INC2                                            
  340 CONTINUE                                                   
      IBASE=IBASE+IJUMP                                         
      JA=JA+JINK                                               
      JB=JB+JINK                                              
      JC=JC-JINK                                             
  350 CONTINUE                                              
      IF (JA.GT.JC) GO TO 900                              
  360 CONTINUE                                            
      JBASE=0                                            
      DO 380 L=1,LA                                     
      I=IBASE                                          
      J=JBASE                                         
CDIR$ IVDEP                                          
      DO 370 IJK=1,LOT                              
      C(JA+J)=A(IA+I)+0.5*(A(IB+I)-A(IC+I))        
      D(JA+J)=-SIN60*(A(IB+I)+A(IC+I))            
      C(JB+J)=A(IA+I)-(A(IB+I)-A(IC+I))                                
      I=I+INC3                                                        
      J=J+INC4                                                       
  370 CONTINUE                                                      
      IBASE=IBASE+INC1                                             
      JBASE=JBASE+INC2                                            
  380 CONTINUE                                                   
      GO TO 900                                                 
C                                                              
  390 CONTINUE                                                
      Z=1.0/FLOAT(N)                                         
      ZSIN60=Z*SIN60                                        
      DO 394 L=1,LA                                        
      I=IBASE                                             
      J=JBASE                                            
CDIR$ IVDEP                                             
      DO 392 IJK=1,LOT                                 
      C(JA+J)=Z*(A(IA+I)+(A(IB+I)+A(IC+I)))           
      C(JB+J)=Z*(A(IA+I)-0.5*(A(IB+I)+A(IC+I)))                      
      D(JB+J)=ZSIN60*(A(IC+I)-A(IB+I))                              
      I=I+INC3                                                     
      J=J+INC4                                                    
  392 CONTINUE                                                   
      IBASE=IBASE+INC1                                          
      JBASE=JBASE+INC2                                         
  394 CONTINUE                                                
      GO TO 900                                              
C                                                           
C     CODING FOR FACTOR 4                                  
C     -------------------                                 
  400 CONTINUE                                           
      IA=1                                              
      IB=IA+IINK                                       
      IC=IB+IINK                                      
      ID=IC+IINK                                     
      JA=1                                          
      JB=JA+(2*M-LA)*INC2                          
      JC=JB+2*M*INC2                              
      JD=JB                                                            
C                                                                     
      IF (LA.EQ.M) GO TO 490                                         
C                                                                   
      DO 420 L=1,LA                                                
      I=IBASE                                                     
      J=JBASE                                                    
CDIR$ IVDEP                                                     
      DO 410 IJK=1,LOT                                         
      C(JA+J)=(A(IA+I)+A(IC+I))+(A(IB+I)+A(ID+I))             
      C(JC+J)=(A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I))            
      C(JB+J)=A(IA+I)-A(IC+I)                               
      D(JB+J)=A(ID+I)-A(IB+I)                              
      I=I+INC3                                            
      J=J+INC4                                           
  410 CONTINUE                                          
      IBASE=IBASE+INC1                                 
      JBASE=JBASE+INC2                                
  420 CONTINUE                                       
      JA=JA+JINK                                    
      JINK=2*JINK                                  
      JB=JB+JINK                                                    
      JC=JC-JINK                                                   
      JD=JD-JINK                                                  
      IBASE=IBASE+IJUMP                                          
      IJUMP=2*IJUMP+IINK                                        
      IF (JB.EQ.JC) GO TO 460                                  
      DO 450 K=LA,KSTOP,LA                                    
      KB=K+K                                                 
      KC=KB+KB                                              
      KD=KC+KB                                             
      C1=TRIGS(KB+1)                                      
      S1=TRIGS(KB+2)                                     
      C2=TRIGS(KC+1)                                    
      S2=TRIGS(KC+2)                                   
      C3=TRIGS(KD+1)                                  
      S3=TRIGS(KD+2)                                 
      JBASE=0                                       
      DO 440 L=1,LA                                
      I=IBASE                                     
      J=JBASE                                    
CDIR$ IVDEP                                                          
      DO 430 IJK=1,LOT                                              
      A0=A(IA+I)+(C2*A(IC+I)+S2*B(IC+I))                           
      A2=A(IA+I)-(C2*A(IC+I)+S2*B(IC+I))                          
      A1=(C1*A(IB+I)+S1*B(IB+I))+(C3*A(ID+I)+S3*B(ID+I))         
      A3=(C1*A(IB+I)+S1*B(IB+I))-(C3*A(ID+I)+S3*B(ID+I))        
      B0=B(IA+I)+(C2*B(IC+I)-S2*A(IC+I))                       
      B2=B(IA+I)-(C2*B(IC+I)-S2*A(IC+I))                      
      B1=(C1*B(IB+I)-S1*A(IB+I))+(C3*B(ID+I)-S3*A(ID+I))     
      B3=(C1*B(IB+I)-S1*A(IB+I))-(C3*B(ID+I)-S3*A(ID+I))    
      C(JA+J)=A0+A1                                        
      C(JC+J)=A0-A1                                       
      D(JA+J)=B0+B1                                      
      D(JC+J)=B1-B0                                     
      C(JB+J)=A2+B3                                    
      C(JD+J)=A2-B3                                   
      D(JB+J)=B2-A3                                  
      D(JD+J)=-(B2+A3)                              
      I=I+INC3                                                     
      J=J+INC4                                                    
  430 CONTINUE                                                   
      IBASE=IBASE+INC1                                          
      JBASE=JBASE+INC2                                         
  440 CONTINUE                                                
      IBASE=IBASE+IJUMP                                      
      JA=JA+JINK                                            
      JB=JB+JINK                                           
      JC=JC-JINK                                          
      JD=JD-JINK                                         
  450 CONTINUE                                          
      IF (JB.GT.JC) GO TO 900                          
  460 CONTINUE                                        
      SIN45=SQRT(0.5)                                
      JBASE=0                                       
      DO 480 L=1,LA                                
      I=IBASE                                     
      J=JBASE                                    
CDIR$ IVDEP                                     
      DO 470 IJK=1,LOT                                                  
      C(JA+J)=A(IA+I)+SIN45*(A(IB+I)-A(ID+I))                          
      C(JB+J)=A(IA+I)-SIN45*(A(IB+I)-A(ID+I))                         
      D(JA+J)=-A(IC+I)-SIN45*(A(IB+I)+A(ID+I))                       
      D(JB+J)=A(IC+I)-SIN45*(A(IB+I)+A(ID+I))                       
      I=I+INC3                                                     
      J=J+INC4                                                    
  470 CONTINUE                                                   
      IBASE=IBASE+INC1                                          
      JBASE=JBASE+INC2                                         
  480 CONTINUE                                                
      GO TO 900                                              
C                                                           
  490 CONTINUE                                             
      Z=1.0/FLOAT(N)                                      
      DO 494 L=1,LA                                      
      I=IBASE                                                         
      J=JBASE                                                        
CDIR$ IVDEP                                                         
      DO 492 IJK=1,LOT                                             
      C(JA+J)=Z*((A(IA+I)+A(IC+I))+(A(IB+I)+A(ID+I)))             
      C(JC+J)=Z*((A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I)))            
      C(JB+J)=Z*(A(IA+I)-A(IC+I))                               
      D(JB+J)=Z*(A(ID+I)-A(IB+I))                              
      I=I+INC3                                                
      J=J+INC4                                               
  492 CONTINUE                                              
      IBASE=IBASE+INC1                                     
      JBASE=JBASE+INC2                                    
  494 CONTINUE                                           
      GO TO 900                                         
C                                                      
C     CODING FOR FACTOR 5                             
C     -------------------                            
  500 CONTINUE                                      
      IA=1                                                          
      IB=IA+IINK                                                   
      IC=IB+IINK                                                  
      ID=IC+IINK                                                 
      IE=ID+IINK                                                
      JA=1                                                     
      JB=JA+(2*M-LA)*INC2                                     
      JC=JB+2*M*INC2                                         
      JD=JC                                                 
      JE=JB                                                
C                                                         
      IF (LA.EQ.M) GO TO 590                             
C                                                       
      DO 520 L=1,LA                                    
      I=IBASE                                         
      J=JBASE                                        
CDIR$ IVDEP                                         
      DO 510 IJK=1,LOT                             
      A1=A(IB+I)+A(IE+I)                          
      A3=A(IB+I)-A(IE+I)                                             
      A2=A(IC+I)+A(ID+I)                                            
      A4=A(IC+I)-A(ID+I)                                           
      A5=A(IA+I)-0.25*(A1+A2)                                     
      A6=QRT5*(A1-A2)                                            
      C(JA+J)=A(IA+I)+(A1+A2)                                   
      C(JB+J)=A5+A6                                            
      C(JC+J)=A5-A6                                           
      D(JB+J)=-SIN72*A3-SIN36*A4                             
      D(JC+J)=-SIN36*A3+SIN72*A4                            
      I=I+INC3                                             
      J=J+INC4                                            
  510 CONTINUE                                           
      IBASE=IBASE+INC1                                  
      JBASE=JBASE+INC2                                 
  520 CONTINUE                                        
      JA=JA+JINK                                     
      JINK=2*JINK                                   
      JB=JB+JINK                                   
      JC=JC+JINK                                 
      JD=JD-JINK                                
      JE=JE-JINK                                                       
      IBASE=IBASE+IJUMP                                               
      IJUMP=2*IJUMP+IINK                                             
      IF (JB.EQ.JD) GO TO 560                                       
      DO 550 K=LA,KSTOP,LA                                         
      KB=K+K                                                      
      KC=KB+KB                                                   
      KD=KC+KB                                                  
      KE=KD+KB                                                 
      C1=TRIGS(KB+1)                                          
      S1=TRIGS(KB+2)                                         
      C2=TRIGS(KC+1)                                        
      S2=TRIGS(KC+2)                                       
      C3=TRIGS(KD+1)                                      
      S3=TRIGS(KD+2)                                     
      C4=TRIGS(KE+1)                                    
      S4=TRIGS(KE+2)                                   
      JBASE=0                                         
      DO 540 L=1,LA                                  
      I=IBASE                                       
      J=JBASE                                                        
CDIR$ IVDEP                                                         
      DO 530 IJK=1,LOT                                             
      A1=(C1*A(IB+I)+S1*B(IB+I))+(C4*A(IE+I)+S4*B(IE+I))          
      A3=(C1*A(IB+I)+S1*B(IB+I))-(C4*A(IE+I)+S4*B(IE+I))         
      A2=(C2*A(IC+I)+S2*B(IC+I))+(C3*A(ID+I)+S3*B(ID+I))        
      A4=(C2*A(IC+I)+S2*B(IC+I))-(C3*A(ID+I)+S3*B(ID+I))       
      B1=(C1*B(IB+I)-S1*A(IB+I))+(C4*B(IE+I)-S4*A(IE+I))      
      B3=(C1*B(IB+I)-S1*A(IB+I))-(C4*B(IE+I)-S4*A(IE+I))     
      B2=(C2*B(IC+I)-S2*A(IC+I))+(C3*B(ID+I)-S3*A(ID+I))    
      B4=(C2*B(IC+I)-S2*A(IC+I))-(C3*B(ID+I)-S3*A(ID+I))   
      A5=A(IA+I)-0.25*(A1+A2)                             
      A6=QRT5*(A1-A2)                                    
      B5=B(IA+I)-0.25*(B1+B2)                           
      B6=QRT5*(B1-B2)                                  
      A10=A5+A6                                       
      A20=A5-A6                                      
      B10=B5+B6                                     
      B20=B5-B6                                    
      A11=SIN72*B3+SIN36*B4                       
      A21=SIN36*B3-SIN72*B4                      
      B11=SIN72*A3+SIN36*A4                     
      B21=SIN36*A3-SIN72*A4                    
      C(JA+J)=A(IA+I)+(A1+A2)                 
      C(JB+J)=A10+A11                                                
      C(JE+J)=A10-A11                                               
      C(JC+J)=A20+A21                                              
      C(JD+J)=A20-A21                                             
      D(JA+J)=B(IA+I)+(B1+B2)                                    
      D(JB+J)=B10-B11                                           
      D(JE+J)=-(B10+B11)                                       
      D(JC+J)=B20-B21                                         
      D(JD+J)=-(B20+B21)                                     
      I=I+INC3                                              
      J=J+INC4                                             
  530 CONTINUE                                            
      IBASE=IBASE+INC1                                   
      JBASE=JBASE+INC2                                  
  540 CONTINUE                                         
      IBASE=IBASE+IJUMP                               
      JA=JA+JINK                                     
      JB=JB+JINK                                    
      JC=JC+JINK                                   
      JD=JD-JINK                                  
      JE=JE-JINK                                 
  550 CONTINUE                                  
      IF (JB.GT.JD) GO TO 900                                         
  560 CONTINUE                                                       
      JBASE=0                                                       
      DO 580 L=1,LA                                                
      I=IBASE                                                     
      J=JBASE                                                    
CDIR$ IVDEP                                                     
      DO 570 IJK=1,LOT                                         
      A1=A(IB+I)+A(IE+I)                                      
      A3=A(IB+I)-A(IE+I)                                     
      A2=A(IC+I)+A(ID+I)                                    
      A4=A(IC+I)-A(ID+I)                                   
      A5=A(IA+I)+0.25*(A3-A4)                             
      A6=QRT5*(A3+A4)                                    
      C(JA+J)=A5+A6                                     
      C(JB+J)=A5-A6                                    
      C(JC+J)=A(IA+I)-(A3-A4)                         
      D(JA+J)=-SIN36*A1-SIN72*A2                     
      D(JB+J)=-SIN72*A1+SIN36*A2                    
      I=I+INC3                                     
      J=J+INC4                                                        
  570 CONTINUE                                                       
      IBASE=IBASE+INC1                                              
      JBASE=JBASE+INC2                                             
  580 CONTINUE                                                    
      GO TO 900                                                  
C                                                               
  590 CONTINUE                                                 
      Z=1.0/FLOAT(N)                                          
      ZQRT5=Z*QRT5                                           
      ZSIN36=Z*SIN36                                        
      ZSIN72=Z*SIN72                                       
      DO 594 L=1,LA                                       
      I=IBASE                                            
      J=JBASE                                           
CDIR$ IVDEP                                            
      DO 592 IJK=1,LOT                                
      A1=A(IB+I)+A(IE+I)                             
      A3=A(IB+I)-A(IE+I)                            
      A2=A(IC+I)+A(ID+I)                           
      A4=A(IC+I)-A(ID+I)                                             
      A5=Z*(A(IA+I)-0.25*(A1+A2))                                   
      A6=ZQRT5*(A1-A2)                                             
      C(JA+J)=Z*(A(IA+I)+(A1+A2))                                 
      C(JB+J)=A5+A6                                              
      C(JC+J)=A5-A6                                             
      D(JB+J)=-ZSIN72*A3-ZSIN36*A4                             
      D(JC+J)=-ZSIN36*A3+ZSIN72*A4                            
      I=I+INC3                                               
      J=J+INC4                                              
  592 CONTINUE                                             
      IBASE=IBASE+INC1                                    
      JBASE=JBASE+INC2                                   
  594 CONTINUE                                          
      GO TO 900                                        
C                                                     
C     CODING FOR FACTOR 6                            
C     -------------------                           
  600 CONTINUE                                     
      IA=1                                        
      IB=IA+IINK                                                      
      IC=IB+IINK                                                     
      ID=IC+IINK                                                    
      IE=ID+IINK                                                   
      IF=IE+IINK                                                  
      JA=1                                                       
      JB=JA+(2*M-LA)*INC2                                       
      JC=JB+2*M*INC2                                           
      JD=JC+2*M*INC2                                          
      JE=JC                                                  
      JF=JB                                                 
C                                                          
      IF (LA.EQ.M) GO TO 690                              
C                                                        
      DO 620 L=1,LA                                     
      I=IBASE                                          
      J=JBASE                                         
CDIR$ IVDEP                                          
      DO 610 IJK=1,LOT                              
      A11=(A(IC+I)+A(IF+I))+(A(IB+I)+A(IE+I))      
      C(JA+J)=(A(IA+I)+A(ID+I))+A11                                   
      C(JC+J)=(A(IA+I)+A(ID+I)-0.5*A11)                              
      D(JC+J)=SIN60*((A(IC+I)+A(IF+I))-(A(IB+I)+A(IE+I)))           
      A11=(A(IC+I)-A(IF+I))+(A(IE+I)-A(IB+I))                      
      C(JB+J)=(A(IA+I)-A(ID+I))-0.5*A11                           
      D(JB+J)=SIN60*((A(IE+I)-A(IB+I))-(A(IC+I)-A(IF+I)))        
      C(JD+J)=(A(IA+I)-A(ID+I))+A11                             
      I=I+INC3                                                 
      J=J+INC4                                                
  610 CONTINUE                                               
      IBASE=IBASE+INC1                                      
      JBASE=JBASE+INC2                                     
  620 CONTINUE                                            
      JA=JA+JINK                                         
      JINK=2*JINK                                       
      JB=JB+JINK                                       
      JC=JC+JINK                                      
      JD=JD-JINK                                     
      JE=JE-JINK                                                      
      JF=JF-JINK                                                     
      IBASE=IBASE+IJUMP                                             
      IJUMP=2*IJUMP+IINK                                           
      IF (JC.EQ.JD) GO TO 660                                     
      DO 650 K=LA,KSTOP,LA                                       
      KB=K+K                                                    
      KC=KB+KB                                                 
      KD=KC+KB                                                
      KE=KD+KB                                               
      KF=KE+KB                                              
      C1=TRIGS(KB+1)                                       
      S1=TRIGS(KB+2)                                      
      C2=TRIGS(KC+1)                                     
      S2=TRIGS(KC+2)                                    
      C3=TRIGS(KD+1)                                   
      S3=TRIGS(KD+2)                                  
      C4=TRIGS(KE+1)                                 
      S4=TRIGS(KE+2)                                
      C5=TRIGS(KF+1)                               
      S5=TRIGS(KF+2)                                               
      JBASE=0                                                     
      DO 640 L=1,LA                                              
      I=IBASE                                                   
      J=JBASE                                                  
CDIR$ IVDEP                                                   
      DO 630 IJK=1,LOT                                       
      A1=C1*A(IB+I)+S1*B(IB+I)                              
      B1=C1*B(IB+I)-S1*A(IB+I)                             
      A2=C2*A(IC+I)+S2*B(IC+I)                            
      B2=C2*B(IC+I)-S2*A(IC+I)                           
      A3=C3*A(ID+I)+S3*B(ID+I)                          
      B3=C3*B(ID+I)-S3*A(ID+I)                         
      A4=C4*A(IE+I)+S4*B(IE+I)                        
      B4=C4*B(IE+I)-S4*A(IE+I)                       
      A5=C5*A(IF+I)+S5*B(IF+I)                      
      B5=C5*B(IF+I)-S5*A(IF+I)                     
      A11=(A2+A5)+(A1+A4)                         
      A20=(A(IA+I)+A3)-0.5*A11                   
      A21=SIN60*((A2+A5)-(A1+A4))               
      B11=(B2+B5)+(B1+B4)                      
      B20=(B(IA+I)+B3)-0.5*B11                                       
      B21=SIN60*((B2+B5)-(B1+B4))                                   
      C(JA+J)=(A(IA+I)+A3)+A11                                     
      D(JA+J)=(B(IA+I)+B3)+B11                                    
      C(JC+J)=A20-B21                                            
      D(JC+J)=A21+B20                                           
      C(JE+J)=A20+B21                                          
      D(JE+J)=A21-B20                                         
      A11=(A2-A5)+(A4-A1)                                    
      A20=(A(IA+I)-A3)-0.5*A11                              
      A21=SIN60*((A4-A1)-(A2-A5))                          
      B11=(B5-B2)-(B4-B1)                                 
      B20=(B3-B(IA+I))-0.5*B11                           
      B21=SIN60*((B5-B2)+(B4-B1))                       
      C(JB+J)=A20-B21                                  
      D(JB+J)=A21-B20                                 
      C(JD+J)=A11+(A(IA+I)-A3)                       
      D(JD+J)=B11+(B3-B(IA+I))                      
      C(JF+J)=A20+B21                              
      D(JF+J)=A21+B20                             
      I=I+INC3                                   
      J=J+INC4                                                      
  630 CONTINUE                                                     
      IBASE=IBASE+INC1                                            
      JBASE=JBASE+INC2                                           
  640 CONTINUE                                                  
      IBASE=IBASE+IJUMP                                        
      JA=JA+JINK                                              
      JB=JB+JINK                                             
      JC=JC+JINK                                            
      JD=JD-JINK                                           
      JE=JE-JINK                                          
      JF=JF-JINK                                         
  650 CONTINUE                                          
      IF (JC.GT.JD) GO TO 900                          
  660 CONTINUE                                        
      JBASE=0                                        
      DO 680 L=1,LA                                 
      I=IBASE                                      
      J=JBASE                                     
CDIR$ IVDEP                                                            
      DO 670 IJK=1,LOT                                                
      C(JA+J)=(A(IA+I)+0.5*(A(IC+I)-A(IE+I)))+ SIN60*(A(IB+I)-A(IF+I))  
      D(JA+J)=-(A(ID+I)+0.5*(A(IB+I)+A(IF+I)))-SIN60*(A(IC+I)+A(IE+I)) 
      C(JB+J)=A(IA+I)-(A(IC+I)-A(IE+I))                               
      D(JB+J)=A(ID+I)-(A(IB+I)+A(IF+I))                              
      C(JC+J)=(A(IA+I)+0.5*(A(IC+I)-A(IE+I)))-SIN60*(A(IB+I)-A(IF+I))   
      D(JC+J)=-(A(ID+I)+0.5*(A(IB+I)+A(IF+I)))+SIN60*(A(IC+I)+A(IE+I)) 
      I=I+INC3                                                        
      J=J+INC4                                                       
  670 CONTINUE                                                      
      IBASE=IBASE+INC1                                             
      JBASE=JBASE+INC2                                            
  680 CONTINUE                                                   
      GO TO 900                                                 
C                                                              
  690 CONTINUE                                                
      Z=1.0/FLOAT(N)                                         
      ZSIN60=Z*SIN60                                                  
      DO 694 L=1,LA                                                  
      I=IBASE                                                       
      J=JBASE                                                      
CDIR$ IVDEP                                                       
      DO 692 IJK=1,LOT                                           
      A11=(A(IC+I)+A(IF+I))+(A(IB+I)+A(IE+I))                   
      C(JA+J)=Z*((A(IA+I)+A(ID+I))+A11)                        
      C(JC+J)=Z*((A(IA+I)+A(ID+I))-0.5*A11)                   
      D(JC+J)=ZSIN60*((A(IC+I)+A(IF+I))-(A(IB+I)+A(IE+I)))            
      A11=(A(IC+I)-A(IF+I))+(A(IE+I)-A(IB+I))                        
      C(JB+J)=Z*((A(IA+I)-A(ID+I))-0.5*A11)                         
      D(JB+J)=ZSIN60*((A(IE+I)-A(IB+I))-(A(IC+I)-A(IF+I)))         
      C(JD+J)=Z*((A(IA+I)-A(ID+I))+A11)                           
      I=I+INC3                                                   
      J=J+INC4                                                  
  692 CONTINUE                                                 
      IBASE=IBASE+INC1                                        
      JBASE=JBASE+INC2                                       
  694 CONTINUE                                              
      GO TO 900                                                       
C                                                                    
C     CODING FOR FACTOR 8                                           
C     -------------------                                          
  800 CONTINUE                                                    
      IBAD=3                                                     
      IF (LA.NE.M) GO TO 910                                    
      IA=1                                                     
      IB=IA+IINK                                              
      IC=IB+IINK                                             
      ID=IC+IINK                                            
      IE=ID+IINK                                           
      IF=IE+IINK                                          
      IG=IF+IINK                                         
      IH=IG+IINK                                        
      JA=1                                             
      JB=JA+LA*INC2                                   
      JC=JB+2*M*INC2                                 
      JD=JC+2*M*INC2                                
      JE=JD+2*M*INC2                               
      Z=1.0/FLOAT(N)                              
      ZSIN45=Z*SQRT(0.5)                                             
C                                                                   
      DO 820 L=1,LA                                                
      I=IBASE                                                     
      J=JBASE                                                    
CDIR$ IVDEP                                                     
      DO 810 IJK=1,LOT                                         
      C(JA+J)=Z*(((A(IA+I)+A(IE+I))+(A(IC+I)+A(IG+I)))+       
     *    ((A(ID+I)+A(IH+I))+(A(IB+I)+A(IF+I))))             
      C(JE+J)=Z*(((A(IA+I)+A(IE+I))+(A(IC+I)+A(IG+I)))-     
     *    ((A(ID+I)+A(IH+I))+(A(IB+I)+A(IF+I))))           
      C(JC+J)=Z*((A(IA+I)+A(IE+I))-(A(IC+I)+A(IG+I)))     
      D(JC+J)=Z*((A(ID+I)+A(IH+I))-(A(IB+I)+A(IF+I)))    
      C(JB+J)=Z*(A(IA+I)-A(IE+I))                                   
     *    +ZSIN45*((A(IH+I)-A(ID+I))-(A(IF+I)-A(IB+I)))            
      C(JD+J)=Z*(A(IA+I)-A(IE+I))                                 
     *    -ZSIN45*((A(IH+I)-A(ID+I))-(A(IF+I)-A(IB+I)))          
      D(JB+J)=ZSIN45*((A(IH+I)-A(ID+I))+(A(IF+I)-A(IB+I)))      
     *    +Z*(A(IG+I)-A(IC+I))                                 
      D(JD+J)=ZSIN45*((A(IH+I)-A(ID+I))+(A(IF+I)-A(IB+I)))    
     *    -Z*(A(IG+I)-A(IC+I))                                        
      I=I+INC3                                                       
      J=J+INC4                                                      
  810 CONTINUE                                                     
      IBASE=IBASE+INC1                                            
      JBASE=JBASE+INC2                                           
  820 CONTINUE                                                  
C                                                              
C     RETURN                                                  
C     ------                                                 
  900 CONTINUE                                              
      IBAD=0                                               
  910 CONTINUE                                            
      IERR=IBAD                                          
      RETURN                                            
      END                                              
C     SUBROUTINE 'SET99' - COMPUTES FACTORS OF N & TRIGONOMETRIC      
C     FUNCTINS REQUIRED BY FFT99 & FFT991                            
C                                                                     
      SUBROUTINE SET99(TRIGS,IFAX,N)                                 
      COMMON /QQQQFFT/ IXXX                                         
      DIMENSION TRIGS(N),IFAX(1),JFAX(10),LFAX(7)                  
      DATA LFAX/6,8,5,4,3,2,1/                                    
      IXXX=1                                                     
C                                                               
      DEL=4.0*ASIN(1.0)/FLOAT(N)                               
      NIL=0                                                   
      NHL=(N/2)-1                                            
      DO 10 K=NIL,NHL                                       
      ANGLE=FLOAT(K)*DEL                                   
      TRIGS(2*K+1)=COS(ANGLE)                             
      TRIGS(2*K+2)=SIN(ANGLE)                            
   10 CONTINUE                                          
C                                                                      
C     FIND FACTORS OF N (8,6,5,4,3,2; ONLY ONE 8 ALLOWED)             
C     LOOK FOR SIXES FIRST, STORE FACTORS IN DESCENDING ORDER        
      NU=N                                                          
      IFAC=6                                                          
      K=0                                                            
      L=1                                                           
   20 CONTINUE                                                     
      IF (MOD(NU,IFAC).NE.0) GO TO 30                             
      K=K+1                                                      
      JFAX(K)=IFAC                                              
      IF (IFAC.NE.8) GO TO 25                                  
      IF (K.EQ.1) GO TO 25                                    
      JFAX(1)=8                                              
      JFAX(K)=6                                             
   25 CONTINUE                                             
      NU=NU/IFAC                                          
      IF (NU.EQ.1) GO TO 50                              
      IF (IFAC.NE.8) GO TO 20                           
   30 CONTINUE                                         
      L=L+1                                           
      IFAC=LFAX(L)                                   
      IF (IFAC.GT.1) GO TO 20                       
C                                                  
      WRITE(6,40) N                                                   
   40 FORMAT(4H1N =,I4,27H - CONTAINS ILLEGAL FACTORS)               
      RETURN                                                        
C                                                                  
C     NOW REVERSE ORDER OF FACTORS                                
   50 CONTINUE                                                   
      NFAX=K                                                    
      IFAX(1)=NFAX                                             
      DO 60 I=1,NFAX                                          
      IFAX(NFAX+2-I)=JFAX(I)                                 
   60 CONTINUE                                              
      RETURN                                               
      END                                                 
      SUBROUTINE FAX(IFAX,N,MODE)                        
      DIMENSION IFAX(10)                                
      COMMON /QQQQFFT/ IXXX                            
C                                                     
C     MARK INITIALISATION AS 'OLD' TYPE              
C                                                   
      IXXX=0                                       
C                                                                     
      NN=N                                                           
      IF (IABS(MODE).EQ.1) GO TO 10                                 
      IF (IABS(MODE).EQ.8) GO TO 10                                
      NN=N/2                                                      
      IF ((NN+NN).EQ.N) GO TO 10                                 
      IFAX(1)=-99                                               
      RETURN                                                   
   10 K=1                                                     
C     TEST FOR FACTORS OF 4                                  
   20 IF (MOD(NN,4).NE.0) GO TO 30                          
      K=K+1                                                
      IFAX(K)=4                                           
      NN=NN/4                                            
      IF (NN.EQ.1) GO TO 80                             
      GO TO 20                                         
C     TEST FOR EXTRA FACTOR OF 2                      
   30 IF (MOD(NN,2).NE.0) GO TO 40                   
      K=K+1                                         
      IFAX(K)=2                                    
      NN=NN/2                                     
      IF (NN.EQ.1) GO TO 80                                            
C     TEST FOR FACTORS OF 3                                           
   40 IF (MOD(NN,3).NE.0) GO TO 50                                   
      K=K+1                                                         
      IFAX(K)=3                                                    
      NN=NN/3                                                     
      IF (NN.EQ.1) GO TO 80                                      
      GO TO 40                                                  
C     NOW FIND REMAINING FACTORS                               
   50 L=5                                                     
      INC=2                                                  
C     INC ALTERNATELY TAKES ON VALUES 2 AND 4               
   60 IF (MOD(NN,L).NE.0) GO TO 70                         
      K=K+1                                               
      IFAX(K)=L                                          
      NN=NN/L                                           
      IF (NN.EQ.1) GO TO 80                            
      GO TO 60                                        
   70 L=L+INC                                        
      INC=6-INC                                     
      GO TO 60                                                       
   80 IFAX(1)=K-1                                                   
C     IFAX(1) CONTAINS NUMBER OF FACTORS                           
C     IFAX(1) CONTAINS NUMBER OF FACTORS                          
      NFAX=IFAX(1)                                               
C     SORT FACTORS INTO ASCENDING ORDER                         
      IF (NFAX.EQ.1) GO TO 110                                 
      DO 100 II=2,NFAX                                        
      ISTOP=NFAX+2-II                                        
      DO 90 I=2,ISTOP                                       
      IF (IFAX(I+1).GE.IFAX(I)) GO TO 90                   
      ITEM=IFAX(I)                                        
      IFAX(I)=IFAX(I+1)                                  
      IFAX(I+1)=ITEM                                    
   90 CONTINUE                                         
  100 CONTINUE                                        
  110 CONTINUE                                       
      RETURN                                   
      END                                       
      SUBROUTINE FFTRIG(TRIGS,N,MODE)            
      DIMENSION TRIGS(N)                                              
      COMMON /QQQQFFT/ IXXX                                           
C                                                                    
C     MARK INITIALISATION AS 'OLD' TYPE                             
C                                                                  
      IXXX=0                                                      
C                                                                
      PI=2.0*ASIN(1.0)                                          
      IMODE=IABS(MODE)                                         
      NN=N                                                    
      IF (IMODE.GT.1.AND.IMODE.LT.6) NN=N/2                  
      DEL=(PI+PI)/FLOAT(NN)                                 
      L=NN+NN                                              
      DO 10 I=1,L,2                                       
      ANGLE=0.5*FLOAT(I-1)*DEL                           
      TRIGS(I)=COS(ANGLE)                               
      TRIGS(I+1)=SIN(ANGLE)                            
   10 CONTINUE                                        
      IF (IMODE.EQ.1) RETURN                                        
      IF (IMODE.EQ.8) RETURN                                       
      DEL=0.5*DEL                                                 
      NH=(NN+1)/2                                                
      L=NH+NH                                                   
      LA=NN+NN                                                 
      DO 20 I=1,L,2                                           
      ANGLE=0.5*FLOAT(I-1)*DEL                               
      TRIGS(LA+I)=COS(ANGLE)                                
      TRIGS(LA+I+1)=SIN(ANGLE)                             
   20 CONTINUE                                            
      IF (IMODE.LE.3) RETURN                             
      DEL=0.5*DEL                                       
      LA=LA+NN                                         
      IF (MODE.EQ.5) GO TO 40                         
      DO 30 I=2,NN                                   
      ANGLE=FLOAT(I-1)*DEL                          
      TRIGS(LA+I)=2.0*SIN(ANGLE)                   
   30 CONTINUE                                    
      RETURN                                                          
   40 CONTINUE                                                       
      DEL=0.5*DEL                                                   
      DO 50 I=2,N                                                  
      ANGLE=FLOAT(I-1)*DEL                                        
      TRIGS(LA+I)=SIN(ANGLE)                                     
   50 CONTINUE                                                  
      RETURN                                                   
      END                                                    
