IF NOT EXIST htm MKDIR htm
cd htm
del *.gif
del *.htm
omhelp ..\main.omh -debug -xml
cd ..
