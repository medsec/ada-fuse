SYSTEM := $(shell uname -sm)
export SYSTEM

all:
	gprbuild -p -f -Phello_world

clean:
	gprclean -Phello_world >/dev/null
	rm obj/* 2>/dev/null || true
