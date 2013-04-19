SYSTEM := $(shell uname -sm)
FUSE_LIBS := $(shell pkg-config --libs fuse)
export SYSTEM
export FUSE_LIBS

all:
	gprbuild -p -f -Phello_world

clean:
	gprclean -Phello_world >/dev/null
	rm obj/* 2>/dev/null || true
