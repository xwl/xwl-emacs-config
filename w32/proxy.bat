@rem desproxy="%home%/usr/desproxy/desproxy.exe"
@set proxy=%http_proxy%
@set port=8080

start /B desproxy irc.debian.org        6669   %proxy% %port% 16669 > NUL
start /B desproxy irc.freenode.net      6667   %proxy% %port% 16667 > NUL

start /B desproxy news.gmane.org        119    %proxy% %port% 10119 > NUL
start /B desproxy nntp.aioe.org         119    %proxy% %port% 11119 > NUL

start /B desproxy dict.org              2628   %proxy% %port% 12628 > NUL
start /B desproxy github.com            22     %proxy% %port% 10022 > NUL
start /B desproxy git.savannah.gnu.org  9418   %proxy% %port% 19418 > NUL
