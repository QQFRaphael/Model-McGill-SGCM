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

'open evo.ctl'
  p = 1
  q = 4
  r=1
  s=1
  ptot = nrows * ncols

  while (p <= ptot)
    _vpg.p
    'set mproj nps'
    'set mpdset mres'
    'set poli off'
*    'set frame off' 
    'set mpvals  -270 90 20 90'
    'set grads off'
    'set map 15 1 3'
    'set xlint 30'
    'set ylint 30'
    'set grid on 1'
    'set csmooth on'
*********************
    'set rbcols auto'
    'set t 'r
    'set cint 1'
    'set cthick 8'
    'set gxout contour'
    'set csmooth on'
    'set cstyle 4'
    'set cmax 0'
'set clevs -6 -5.5 -5 -4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -0.5'
*'set clevs -14 -13 -12 -11 -10 -9 -8 -4 -3 -2 -1 '
'set ccols 4 4 4 4 4 4 4 4 4 4 4 4 4'
     'd sha'
    'set cmin 0'
'set clevs 0.5 1 1.5 2 2.5 3 3.5 4 4.5 5'
*'set clevs 1 2 3 4 5 6 7 8 9 10'
'set ccols 2 2 2 2 2 2 2 2 2 2 2'
    'd sha'
*********************

    'draw title S'q''

    p = p + 1
    q = q + 2
    r = r + 1
    s = s + 4
  endwhile

'enable print resp.met'
'print'
