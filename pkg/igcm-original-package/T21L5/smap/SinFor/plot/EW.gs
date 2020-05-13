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

'open 0.ctl'
  p = 1
  q = -7
  r=1
  s=1
  ptot = nrows * ncols

  while (p <= ptot)
    _vpg.p
*    'set mproj nps'
    'set mpdset mres'
    'set poli off'
    'set clab off'
*    'set frame off' 
    'set mpvals  -270 90 20 90'
    'set lat -20 90'
    'set lon 60 360'
    'set grads off'
    'set map 15 1 1'
    'set xlint 20'
    'set ylint 20'
    'set grid on 1'
    'set csmooth on'
*********************
    'set rbcols auto'
    'set t 'r
    'set cint 1'
    'set cthick 3'
    'set gxout contour'
'set rgb 60 2 2 2'
'set clevs  -100 100 '
'd sha'
'basemap L 60 1 M'

    'set gxout contour'
    'set csmooth on'
    'set cstyle 4'
    'set cmax 0'
*'set clevs -4 -8 -12 -16 -20 -24 -28'
'set clevs -8 -16 -24 -32 -36 -40'
'set ccols 4 4 4 4 4 4 4 4 4 4 4 4 4'
     'd sha'
    'set cstyle 1'
    'set cmin 0'
'set clevs 8 16 24 32 40 48 56'
*'set clevs 1 2 3 4 5 6 7 8 9 10'
'set ccols 2 2 2 2 2 2 2 2 2 2 2'
    'd sha'
*********************
    if (p = 2)
     'draw title (b)'
    else
    endif
    if (p = 3)
     'draw title (c)'
    else
    endif
    if (p = 4)
     'draw title (d)'
    else
    endif

'set string 4 c 6 0'
'set strsiz 0.1 0.15'
'draw string 3.58 5.58 W'
'draw string 4.29 5.95 E'



    p = p + 1
    q = q + 2
    r = r + 1
    s = s + 4
  endwhile

'enable print resp.met'
'print'
