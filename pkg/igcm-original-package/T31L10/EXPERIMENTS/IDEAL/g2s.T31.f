c###############################################################################
c#                                                                             #
c#    Prashant D. Sardeshmukh  CRD/CIRES   January 1992                        #
c#                                                                             #
c#    Program for generating the T42 spectral coefficients of fields on a      #
c#    T42 gaussian 128x64 grid.                                                #
c#                                                                             #
c#    The program can be run by using the commands                             #
c#         : f77 g2s.f aux1.o -o g2s.out                                       #
c#         : g2s.out                                                           #
c#                                                                             #
c#    Input is from file T42grids.b                                            #
c#    Output is to file  T42coeffs.b                                           #
c#                                                                             #
c#    Any number N of grid-point fields may be transformed.                    #
c#        The program will prompt the user interactively for the value of N.   #
c#                                                                             #
c#    The input file contains 2N records,ihd(10),field1(128,64),               #
c#        ihd(10),field2(128,64), ..... ,ihd(10),fieldN(128,64),               #
c#        where ihd(10) is a descriptor record. The program ignores ihd.       #
c#    The output file contains N records, each 1892 real words long,           #
c#        containing the T42 spectral coefficients of the N grid-point fields. #
c#                                                                             #
c#    Fields on grids other than a T42 gaussian grid may also be transformed,  #
c#        by using a different parameter statement in main and gausqd below.   #
c#                                                                             #
c###############################################################################
c#                                                                             #
c#    The program may also be used to generate the spectral coefficients of    #
c#    an arbitrary field force(128,64) defined by the user in the first        #
c#    loop over gaussian latitudes below.                                      #
c#                                                                             #
c#    An example of a circular sine-squared vorticity source is given.         #
c#    To enable this option type 0 when the program prompts you for a          #
c#    value of N.                                                              #
c#                                                                             #
c###############################################################################
                                                                        
      parameter(mfx=31,jgx=32,mgx=128)

      parameter(mfpx=mfx+1,mtx=mfpx+(mfpx*mfx/2),mtpx=mtx+mfpx)
      parameter(mtrx=mtx+mtx,mtsx=mtpx+mtpx)
      parameter(jg2x=jgx+jgx,mgppx=mgx+2)

      common/grip/ zg(mgppx),mf,mfp,moct,mh
      common/legs/ alp(mtpx),dalp(mtx),weight

      dimension abgw(192),wtgw(192),alat(192),gw(96)
      dimension work(1158),ifax(10),trigs(576)         
      dimension salp(mtpx,jg2x),sdalp(mtx,jg2x)
      dimension ihd(10),orog(mgx,jg2x),force(mgx,jg2x),divet(mtrx)

c -------------------------------------------------------------------------- c

C     open(12,file='/diskb/hall/T42grids.b'
C    .,form='unformatted')
C     open(13,file='/diskb/hall/T31coeffs.b'
C    .,form='unformatted')

      open(12,file='gridanom'
     .,form='unformatted')
      open(13,file='T31.flatcoeffs'
     .,form='unformatted')
c -------------------------------------------------------------------------- c

      nrec = 0
      mrec = 1

      write(*,*) ''
      write(*,*) 'Type N, the number of fields in your input file'
      read (*,*)  nrec 

      mh     =  1                                                         
      moct   =  1                                                      
      mf     =  mfx                                                      
      mg     =  mgx                                                    
      jg     =  jgx                                                    
      pi     =  2.*asin(1.)
      mfp    =  mf+1                                                          
      mfpp   =  mfp+1                                                       
      pi2    =  pi+pi
      mgp    =  mg+1
      mgpp   =  mgp+1
      rmg    =  1./mg
      jg2     = jg+jg   

      call gauaw  (abgw,wtgw,jg2)                                   
      call fax    (ifax,mg,3)                                        
      call fftrig (trigs,mg,3)                                   
 
      do 8 j    = 1,jg2            
         sit    = abgw(j)            
         weight = wtgw(j)      
         gwt    = weight/(1.-sit*sit)
         alat(j)= atan(sit/sqrt(1.-sit*sit))*57.29578  
         if (j.gt.jg) go to 19                        
         gw(j)  = gwt                                
   19    call lgndre(sit,gwt,mfp,moct,salp(1,j),sdalp(1,j)) 
         amp    = 1.
         alt1   = 20.                                    
         alt2   =-20.                                   
         aln1   = 100.                                  
         aln2   = 140.                                                  
         fct    = 0.                                                  
         alt    = alat(j)                                            
         if(alt.lt.alt1.and.alt.gt.alt2)
     @      fct = sin(pi*(alt-alt1)/(alt1-alt2))
            fct = fct*fct   
         do 11 i  = 1,mg  
            flam  = (i-1)*360.*rmg                  
            if (flam.ge.aln2.or.flam.le.aln1) go to 11         
            sn    = sin(pi*(flam-aln1)/(aln2-aln1))
            force(i,j) = amp*fct*sn*sn                               
   11    continue                                                 
    8 continue                                                      

c===============================================================================

      if (nrec.ne.0) mrec = nrec 

      do irec   = 1,mrec

       if (nrec.ne.0) then
          read (12) ihd
          write(*,*)(ihd(ihdi),ihdi=1,3)
          read (12) orog
       endif

       do jj         = 1,mtrx
          divet(jj)  = 0.0
       enddo

       sum      = 0.
       do 9 j   = 1,jg2

         sit    = abgw(j)            
         weight = wtgw(j)      
         ia     = 0
         id     = 0
         do mp  = 1,mfp,moct    
            do jp = mp,mfp,mh
               ia = ia+1
               id = id+1
               alp(ia)  = salp(ia,j)
               dalp(id) = sdalp(id,j)
            enddo
         ia = ia+1
         alp(ia) = salp(ia,j)
         enddo

         if (nrec.ne.0) then
            do i     = 1,mg
               zg(i) = orog(i,j)
               sum   = sum + zg(i)*zg(i)
            enddo
         else
            do i     = 1,mg
               zg(i) = force(i,j)
               sum   = sum + zg(i)*zg(i)
            enddo
         endif
         zg(mgp)     =  0.
         zg(mgpp)    =  0.

         call fft991(zg(1),work,trigs,ifax,1,mgpp,mg,1,-1)
         weight      =  wtgw(j)
         call gausqd (divet)

    9  continue

       write (13) divet

       sum  = sqrt (sum/(float(jg2)*float(mg)))
       write (*,*) 'Global rms value of grid-point field = ',sum

       ig   = 0
       do mp= 1,mfp,moct
         m = mp-1
         n = mp-1
         do jp = mp,mfp,mh
            ig = ig+1
            igr = 2*ig-1
C           write(2,781)m,n,divet(igr),divet(igr+1)
            n  = n+1
         enddo
       enddo
C      write (2,782) irec

      enddo

c============================================================================

C     write (2,202)                                                 
      do j       = 1,jg2                                               
         abgw(j) = alat(j)                                            
      enddo
C     write (2,201) (abgw(j),j=1,jg2)                            

  201 format(8(2x,f6.2))
  202 format(1h ,18hgaussian latitudes)
  781  format(1x,5h  m =,i3,5h  n =,i3
     * ,             15h   real part = ,e9.3,15h   imag part = ,e9.3)
  782  format(1x,45h Processing completed for field number ------,i3)

   84 stop
      end                                                    
c//////////////////////////////////////////////////////////////////////////////

      subroutine gausqd (zri)

      parameter(mfx=31,jgx=32,mgx=128)

      parameter(mfpx=mfx+1,mtx=mfpx+(mfpx*mfx/2),mtpx=mtx+mfpx)
      parameter(mtrx=mtx+mtx,mtsx=mtpx+mtpx)
      parameter(jg2x=jgx+jgx,mgppx=mgx+2)

      common/grip/ zg(mgppx),mf,mfp,moct,mh
      common/legs/ alp(mtpx),dalp(mtx),weight

      dimension zri(mtrx)

      ip     =  1
      ifr    =  1
      ilr    =  1
      vzfr   =  zg(ifr)
      zri(1) =  zri(1)+vzfr*alp(1)*weight
      lim    =  1+mh

      do 1 jp     =  lim,mfp,mh
         ilr      =  ilr+2
         ip       =  ip+1
         zri(ilr) =  zri(ilr)+vzfr*alp(ip)*weight
    1 continue
      ip          =  ip+1

      do 3 m      =  moct,mf,moct                            
         ifr      =  ifr+2
         ifi      =  ifr+1
         vzfr     =  zg(ifr)
         vzfi     =  zg(ifi)
         do 2 jp  =  m,mf,mh
            ip    =  ip+1
            ilr   =  ilr+2
            ili   =  ilr+1
            zri(ilr) =  zri(ilr)+vzfr*alp(ip)*weight
            zri(ili) =  zri(ili)+vzfi*alp(ip)*weight
    2    continue
         ip       =  ip+1
    3 continue

      return
      end
c//////////////////////////////////////////////////////////////////////////////
