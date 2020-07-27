ADA_FUSE_SYSTEM := $(shell uname -sm)
export ADA_FUSE_SYSTEM

ifeq ($(ADA_FUSE_SYSTEM),Linux x86_64)
	SYS_DIR=system/linux64
endif

ifeq ($(ADA_FUSE_SYSTEM),Linux i686)
	SYS_DIR=system/linux32
endif

ifeq ($(ADA_FUSE_SYSTEM),Darwin i386)
	SYS_DIR=system/macos
endif

ifeq ($(ADA_FUSE_SYSTEM),Darwin x86_64)
	SYS_DIR=system/macos
endif


all:
	gprbuild -p -f -Pada_fuse

clean:
	gprclean -Pada_fuse >/dev/null
	rm obj/* lib/* 2>/dev/null || true

# Install directory
prefix ?= $(shell which gprbuild 2> /dev/null | sed -e 's/\/bin\/gprbuild.*//')

install: lib/libada_fuse.a
	gprinstall -p -Pada_fuse --prefix=$(prefix)

uninstall:
	gprinstall -Pada_fuse --prefix=$(prefix) --uninstall
