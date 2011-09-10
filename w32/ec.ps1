emacsclient.exe --server-file $home/.emacs.d/server/server -n $args
Set-foregroundWindow (Get-process Emacs).Mainwindowhandle

