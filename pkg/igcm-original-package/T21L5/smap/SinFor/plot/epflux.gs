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


  nrows = 2
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
  q = -6
  r=4
  s=1
  ptot = nrows * ncols

  while (p <= ptot)
   _vpg.p
***

   'set mproj nps'
    'set mpdset mres'
    'set poli off'
*    'set frame off' 
   'set mpvals  -270 90 20 90'
    'set grads off'
    'set map 15 1 3'
    'set xlint 60'
    'set ylint 30'
    'set grid on 8 1'
    'set csmooth on'
    'set lat 0 90'
*********************
    'set t ' s
'set gxout contour'
'set cmax 0'
'set clevs -1 -2 -3 -4 -5 -6 -7 -8 -9 '
'set ccols 4 4 4 4 4 4 4 '
'set cstyle 4'
'set cthick 11'
'd z'
'set cmin 0'
'set clevs 1 2 3 4 5 6 7 8 9 10'
'set cstyle 1'
'set cthick 11'
'set rgb 24 255 80 80'
'set ccols 24 24 24 24 24 24 24 24'
'd z'
*********************
    'set gxout vector'
    'set strmden 10'
    'set digsiz 0.085 '
    'set arrscl '0.5' ' 6.
    'set arrowhead 0.08 '
    'set cthick 7'
    'd u;v'
*********************
*********************
    if (p = 1)
     'draw title (a)'
    else
    endif
    if (p = 2)
     'draw title (b)'
    else
    endif


    p = p + 1
    q = q + 1
    r = r + 2
    s = s + 1
  endwhile

'enable print resp.met'
'print'
