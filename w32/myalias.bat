@echo off
doskey m = bldmake bldfiles $*
doskey b = abld build winscw udeb $* 
doskey t = abld target winscw udeb $* 
doskey c = abld clean $* 
doskey cx = abld cleanexport $* 
doskey x = abld export $* 

doskey av = cd "\sf\mw\classicui\uifw\avkon\group"
doskey cu = cd "\sf\mw\classicui\group"
doskey tf = cd "\sf\mw\hapticsservices\group"
doskey ht = cd "\sf\os\devicesrv\hwrmhaptics\group"

doskey av5 = cd "\s60\mw\classicui\uifw\avkon\group"
doskey cu5 = cd "\s60\mw\classicui\group"

doskey ls = ls -x --color=always $*

set EDITOR=emacsclient
