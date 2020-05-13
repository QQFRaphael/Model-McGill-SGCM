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
  q = -7
  p=1
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
    'set map 15 1 1'
    'set xlint 30'
    'set ylint 30'
    'set grid on 1'
    'set csmooth on'
*********************

    'set rbcols auto'
    'set cint 100'
    'set cthick 8'
    'set gxout contour'
    'set csmooth on'
     'd sha'
*********************

    'draw title S'q''

    q = q + 1
    s = s + 1
    p = p + 1
  endwhile

'enable print resp.met'
'print'
