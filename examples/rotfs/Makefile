SYSTEM := $(shell uname -sm)
export SYSTEM

# targets for filesystem
#------------------------------

all:
	gprbuild -p -f -Protfs

clean:
	gprclean -Protfs >/dev/null
	rm obj/* 2>/dev/null || true

# targets for tests
#------------------------------

test:
	gprbuild -p -f -Punittest
	./test_main
clean_test: 
	gprclean -Punittest >/dev/null
	rm -r unittest/obj/* 2>/dev/null || true
