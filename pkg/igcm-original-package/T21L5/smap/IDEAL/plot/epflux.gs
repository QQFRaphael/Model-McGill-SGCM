*function panels(args)
*
* Get arguments
*  if (args='')
*    say 'panels requires two arguments: the # of rows and # of columns'
*    return
*  else
*    nrows = subwrd(args,1)
*    ncols = subwrd(args,2)
*  endif
* Get dimensions of the real page
'clear'
'reinit'
'reset'


  nrows = 1
  ncols = 1

  'query gxinfo'
  rec2  = sublin(result,2)
  xsize = subwrd(rec2,4)
  ysize = subwrd(rec2,6)

* Calculate coordinates of each vpage
  width  = xsize/ncols
  height = ysize/nrows
  row = 1
  col = 1
  panel = 1


   while (row <= nrows)
    yhi = ysize - (height * (row - 1))
    if (row = nrows)
      ylo = 0
    else
      ylo = yhi - height
    endif
 
    while (col <= ncols)
      xlo = width * (col - 1)
      xhi = xlo + width
      _vpg.panel = 'set vpage 'xlo'  'xhi'  'ylo'  'yhi
      panel = panel + 1
      col = col + 1
    endwhile
    col = 1
    row = row + 1

  endwhile

'open epflux.ctl'
  p = 1
  q = -2
  r=2
  s=1
  ptot = nrows * ncols

  while (p <= ptot)
   _vpg.p
***
'set mproj nps'
    'set xlint 60'
    'set ylint 60'
'set mpvals  -270 90 20 90'
'set mpdset mres'
'set poli off'
'set grid on 1'
'set grads off'
****
'set gxout contour'
'set cmax 0'
*'set clevs -160 -140 -120 -100 -80 -60 -40 -20 '
*'set clevs -105 -90 -75 -60 -45 -30 -15 '
*'set rgb 60 10   10   10'
*'set clevs  -100 100 '
*'d z'
*'basemap L 60 1 M'
*'set clevs -1 -2 -3 -4 -5 -6 -7 '
'set clevs -160 -140 -120 -100 -80 -60 -40 -20 '
'set ccols 4 4 4 4 4 4 4 '
'set cstyle 4'
'set cthick 9'
'd z'
'set cmin 0'
'set clevs 20 40 60 80 100 120 140 160'
*'set clevs 15 30 45 60 '
*'set clevs 1 2 3 4 5 6 7 8  '
'set rgb 24 255 80 80'
'set ccols 24 24 24 24 24 24 24 24'
'set cstyle 1'
'set cthick 9'
'd z'
***

    'set mproj nps'
    'set mpdset mres'
    'set poli off'
*    'set frame off' 
    'set mpvals  -270 90 20 90'
    'set grads off'
    'set map 15 1 3'
    'set xlint 60'
    'set ylint 60'
    'set grid on 8 1'
    'set csmooth on'
*********************
    'set t ' p
    'set gxout vector'
    'set strmden 40'
    'set digsiz 0.285 '
    'set arrscl '1.0' ' 80.
    'set arrowhead 0.08 '
    'set cthick 7'
    'd u;v'
*********************

    'draw title  '

    p = p + 1
    q = q + 1
    r = r + 2
    s = s + 2
  endwhile

'enable print resp.met'
'print'
