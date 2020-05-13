#script to prepare a forcing anomaly field for the global model
#first create a global gridpoint array with forcing 
#anomalies for all model fields (set anom to zero 
#if no forcing anomaly required for a particular field)
#YOU MUST EDIT makeanom.f FOR THE ANOMALY YOU WANT
#produces a gridpoint file caled gridanom.
#f77 -r8 makeanomMJO.f
f77 -r8 makeanomIDEAL.f
#f77 -r8  makeanomDipoe.f
#f77 -r8 0.f
a.out
#spectrally analyse at flat T31 (read gridanom, write T31.flatcoeffs)
f77 -r8 g2s.T31.f /zemo2/jiaxj/SUBROUTINES/aux1.f
echo running g2s
cat << /EOF | a.out
31
/EOF
#then change to reading format (reads T31.flatcoeffs, writes data.fan)
f77 -r8 flat2jaggedFANOM.f
a.out
