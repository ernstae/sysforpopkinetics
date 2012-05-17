INSTALL_DIR = \
	/usr/local/bin/spktest/

all :  jobDriver midDriver

jobDriver :
	g++ -I/usr/share/pvm3/include -L/usr/share/pvm3/lib/LINUXX86_64 -L/usr/share/pvm3/lib/LINUX -I/usr/local/include/spktest -L/usr/local/lib/spktest jobDriver.cpp -o jobDriver -lpvm3 -lspk

midDriver :
	g++ -I/usr/share/pvm3/include -L/usr/share/pvm3/lib/LINUXX86_64 -L/usr/share/pvm3/lib/LINUX -I/usr/local/include/spktest -L/usr/local/lib/spktest midDriver.cpp -o midDriver -lpvm3 -lspk

install : jobDriver midDriver
	cp jobDriver midDriver $(INSTALL_DIR)

clean : jobDriver midDriver
	rm jobDriver midDriver
