FLAGS := -std=c++11 -arch i386 -arch x86_64 -Wall -lz -framework CoreFoundation -framework Foundation -dynamiclib
all: dropbox

setuid: set_uid.mm
	rm -f set_uid
	clang++ -framework Foundation -arch i386 -o set_uid set_uid.mm
	chmod 06755 set_uid
	sudo chown root:wheel set_uid

dropbox: dropbox_inj.mm
	clang++ $(FLAGS) -o dropbox_inj.dylib dropbox_inj.mm

dbfs: dbfs_inj.mm
	clang++ $(FLAGS) -o dbfs_inj.dylib dbfs_inj.mm
