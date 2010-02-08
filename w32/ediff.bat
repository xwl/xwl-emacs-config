@set f1=%1
@set f2=%2

@rem replace `\' with `/' 
@set f1=%f1:\=/% 
@set f2=%f2:\=/% 

emacsclient.exe -n -e "(progn (ediff-files \"%f1%\" \"%f2%\") (raise-frame))" 
rem bcomp %f1% %f2%
