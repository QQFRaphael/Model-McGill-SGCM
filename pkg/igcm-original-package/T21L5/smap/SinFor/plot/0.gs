'clear'
'reinit'
'reset'

'open epglobal.ctl'
    'set xlint 20'
    'set ylint 20'
*'set lat 0 90'
*'set lon -270 90'
*'set mpvals  -180 90 -30 30'
'set mpdset mres'
'set poli off'
'set grid on 1'
'set grads off'
    'set map 15 1 1'
    'set grid on 8 1'
    'set csmooth on'
****
'set gxout contour'
'set cmax 0'
'set clevs -15 -30 -45 -60 -75 -90 -105 '
'set ccols 4 4 4 4 4 4 4 '
'set cstyle 4'
'set cthick 6'
'd z'
'set cmin 0'
'set clevs 15 30 45 60 75 90 105 '
'set rgb 24 255 80 80'
'set ccols 24 24 24 24 24 24 24 24 24 24'
'set cstyle 1'
'set cthick 4'
'd z'
***

*********************
    'set gxout vector'
    'set strmden 40'
    'set digsiz 0.685 '
    'set arrscl '0.5' ' 100.
    'set arrowhead 0.04 '
    'set cthick 6'
    'd u;v'
*********************

     'draw title 135E '

'enable print resp.met'
'print'
