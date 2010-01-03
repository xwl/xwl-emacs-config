## external reply editor for qterm

## Author: William Xu <william.xwl@gmail.com>
## Version: 0.4

## Commentary
## ----------
# To use, the most important thing you should keep in mind is knowing
# the *right* time to trigger this script. i.e., running this script
# when you are reading some article(note: not in reply mode ! ).
#
# - How about support for editing an old post or starting a new post?
#
#   After some try, i plan not to support editing an old post or
#   starting a new post, since the coding seems quite complicated, and
#   not worthwhile(i was thinking about using some regexp match to find
#   out whether we are editing an old post, starting a new post or
#   whatever).
#
#   So, if this is not supported, what should i do? I ususally would
#   write down the texts inside Emacs(with wubi.el), and cut it then
#   paste it to qterm. A bit weired? Not so much to me. ;-)

## Depend
## ------ 
# - emacs, emacsclient
# - ansit.el (http://williamxu.net9.org/ref/ansit.el, modified version)
# - qterm.el (http://williamxu.net9.org/ref/qterm.el)

## Changelog
## ---------
# 0.31 - Rename editor.py to reply.py

# 0.3  - split emacs configs into a seprated file, qterm.el
#      - now we support highlighting qmd ! 

# 0.22 - show author as:  author (nickname)
#      - make qmd with fortune-zh be optional

# 0.21 - commentary slightly updates
#      - add (require 'ansi-color) to emacs config part

import qterm
import sys
import subprocess

# user customizable

log = "/tmp/qterm.log"
editor = "emacsclient"


lp = long(sys.argv[0])

# if __name__ == '__main__':
def main():
    # copy article to reply
    f = file(log, "w")

    f.write(qterm.toUTF8(qterm.copyArticle(lp), "gbk"))

    f.close()

    qterm.sendString(lp, "r\n")
    for i in range(0, 12):
        qterm.sendString(lp, "")

    # call external editor
    subprocess.call([editor, log])

    # post back
    f = file(log, "r")

    content = ""
    for i in f.readlines():
        content += i

    f.close()

    qterm.sendString(lp, qterm.fromUTF8(content, "gbk"))
    qterm.sendString(lp, "")

main()

## reply.py ends here
