'clear'
'reinit'
'reset'
'open gradsEOF.ctl'
'enable print resp.met'
'set vpage 2 7 5.5 11 '

'set lat 20 90'
'set mproj nps'
'set mpvals  -270 90 20 90'
'set grid on 8 1'
'set map 15 1 3'
'set ylint 70'
'set xlint 90'
'set csmooth on'
*'set poli off'
'set grads off'
*'set frame off'
'set clab off'



**
*'set gxout shaded'
'set gxout contour'
'set rgb 29 60 60 255'
'set rgb 28 80  80 255'
'set rgb 27 100 100 255'
'set rgb 26 120 120 255'
'set rgb 25 140 140 255'
'set rgb 24 160 160 255'
'set rgb 23 180 180 255'
'set rgb 22 200 200 255'
'set rgb 21 220 220 255'
* These are the RED shades
'set rgb 31 255 240 240'
'set rgb 32 255 220 220'
'set rgb 33 255 165 165'
'set rgb 34 255 110 110'
'set rgb 35 255  55  55'
'set rgb 36 255   0   0'
'set rgb 60 10   10  10'
'set clevs 0.02 '
*'set ccols 29 28 27 26 25 24 23 22 21 0 31 32 33 34 35 36'
*'d resp'
*'basemap L 60 1 M'

** set for the lines
'set gxout contour'
'set cthick 11'
'set cmax 0'
'set cstyle 4'
'set clevs -0.18 -0.16 -0.14 -0.12 -0.1 -0.08 -0.06 -0.04 -0.02 '
'set ccols 26 26 26 26 26 26 26 26 26 '
'd resp'
'set cmin 0'
'set clevs 0.02 0.04 0.06 0.08 0.1 0.12 0.14 0.16 0.19'
'set ccols 34 34 34 34 34 34 34 34 34'
'set cstyle 1'
'd resp'
*******************

'set vpage 2 7 0.5 5.5 '

'set lat 20 90'
'set mproj nps'
'set mpvals  -270 90 20 90'
'set grid on 8 1'
'set map 15 1 3'
'set ylint 70'
'set xlint 90'
'set csmooth on'
*'set poli off'
'set grads off'
*'set frame off'
'set clab off'

'set gxout contour'
'set t 2'
'set cthick 11'
'set cmax 0'
'set cstyle 4'
'set clevs -0.18  -0.15 -0.12 -0.09 -0.06 -0.03 '
*'set clevs -0.18 -0.16 -0.14 -0.12 -0.1 -0.08 -0.06 -0.04 -0.02 '
'set ccols 26 26 26 26 26 26 26 26 26 '
'd resp'
'set cmin 0'
*'set clevs 0.02 0.04 0.06 0.08 0.1 0.12 0.14 0.16 0.18 0.2'
'set clevs 0.03 0.06 0.09 0.12 0.15 0.18 0.21'
'set ccols 34 34 34 34 34 34 34 34 34 34'
'set cstyle 1'
'd resp'













*'draw title Atmospheric response to equator forcing\ (-5c/day)'
'print'

