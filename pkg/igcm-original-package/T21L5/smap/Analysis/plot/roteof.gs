'clear'
'reinit'
'reset'
'open gradsEOF.ctl'
'enable print resp.met'


'set vpage 1 8 5.5 10.5'
'set lat 20 90'
'set mproj nps'
'set mpvals  -270 90 20 90'
'set grid on 8 1'
'set map 15 1 3'
'set ylint 70'
'set xlint 90'
'set csmooth on'
'set grads off'
'set clab off'

'set gxout contour'
*'set ccols 29 28 27 26 25 24 23 22 21 0 31 32 33 34 35 36'
*'d resp'
*'basemap L 60 1 M'
'set t 1'
'set cthick 11'
'set cmax 0'
'set cstyle 4'
'set clevs -0.1 -0.2 -0.3 -0.4 -0.5 -0.6 -0.7 -0.8 '
'set ccols 4 4 4 4 4 4 4'
'd resp'
'set cmin 0'
'set clevs 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1 '
'set ccols 2 2 2 2 2 2 2 2 2 '
'set cstyle 1'
'd resp'
'draw title EOF1 (SGCM) 36% '
**********



'set vpage 1 8 0.5 5.5 '
'set lat 20 90'
'set mproj nps'
'set mpvals  -270 90 20 90'
'set grid on 8 1'
'set map 15 1 3'
'set ylint 70'
'set xlint 90'
'set csmooth on'
'set grads off'
'set clab off'

'set gxout contour'
'set t 2'
'set cthick 11'
'set cmax 0'
'set cstyle 4'
*'set clevs -0.1 -0.2 -0.3 -0.4 -0.5 -0.6 -0.7 -0.8 '
'set clevs -0.02 -0.04 -0.06 -0.08 '
'set ccols 4 4 4 4 4 4 4'
'd resp'
'set cmin 0'
*'set clevs 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1 '
'set clevs 0.02 0.04 0.06 0.08 0.1 '
'set ccols 2 2 2 2 2 2 2 2 2 '
'set cstyle 1'
'd resp'

'draw title EOF1 (SGCM) 11% '
**********








'print'

