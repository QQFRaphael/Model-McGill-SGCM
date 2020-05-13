load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin
	cyear1=getenv("YEAR1")

        iyear1=stringtoint(cyear1)
        root = "/home/qqf/Data/NCEP2-DAILY"

        au1 = addfile(root+"/uwnd/uwnd."+cyear1+".nc","r")
        av1 = addfile(root+"/vwnd/vwnd."+cyear1+".nc","r")
        at1 = addfile(root+"/air/air."+cyear1+".nc","r")
        az1 = addfile(root+"/hgt/hgt."+cyear1+".nc","r")

        u1 = au1->uwnd(:,0:13,:,:)
        uu1 = u1

        v1 = av1->vwnd(:,0:13,:,:)
        vv1 = v1

        t1 = at1->air(:,0:13,:,:)
        tt1 = short2flt(t1) 

        z1 = az1->hgt(:,0,:,:)
        zz1 = z1

;;;output
        filo1 = "./data/testu.dat"            ; output file
        filo2 = "./data/testv.dat"            ; output file
        filo3 = "./data/testt.dat"            ; output file
        filo4 = "./data/testz.dat"            ; output file

 istart=243

if(mod(iyear1,4).eq.0)then
  istart=istart+1
end if

  iend=istart+91

	npts = dimsizes(zz1(:,0,0))         ; number of time points
;setfileoption("bin","WriteByteOrder","BigEndian")
do it=istart,iend-1
   u3=uu1(it,:,:,:)
   v3=vv1(it,:,:,:)
   t3=tt1(it,:,:,:)
	
   fbinrecwrite (filo1, -1, u3)
   fbinrecwrite (filo2, -1, v3)
   fbinrecwrite (filo3, -1, t3)

   z3=zz1(it,:,:)
   fbinrecwrite (filo4, -1, z3)
end do

end

