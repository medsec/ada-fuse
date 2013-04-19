SYSTEM := $(shell uname -sm)
FUSE_LIBS := $(shell pkg-config --libs fuse)
export SYSTEM
export FUSE_LIBS

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

clean_test: 
	gprclean -Punittest >/dev/null
	rm -r unittest/obj/* 2>/dev/null || true
