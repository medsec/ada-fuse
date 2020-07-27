SYSTEM := $(shell uname -sm)
export SYSTEM

ifeq ($(SYSTEM),Linux x86_64)
	SYS_DIR=system/linux64
endif

ifeq ($(SYSTEM),Linux i686)
	SYS_DIR=system/linux32
endif

ifeq ($(SYSTEM),Darwin i386)
	SYS_DIR=system/macos
endif

ifeq ($(SYSTEM),Darwin x86_64)
	SYS_DIR=system/macos
endif


all:
	gprbuild -p -f -Pada_fuse

clean:
	gprclean -Pada_fuse >/dev/null
	rm obj/* lib/* 2>/dev/null || true


# Install directories

INSTALL = $(shell which gprbuild 2> /dev/null | sed -e 's/\/bin\/gprbuild.*//')
I_INC   = $(INSTALL)/include/ada_fuse
I_LIB   = $(INSTALL)/lib/ada_fuse
I_GPR   = $(INSTALL)/lib/gnat

install: lib/libada_fuse.a
	install -d $(I_INC)
	install -d $(I_LIB)
	install src/fuse*.ads src/fuse-*.adb src/$(SYS_DIR)/* $(I_INC)
	install lib/* $(I_LIB)
	install ada_fuse_install.gpr $(I_GPR)/ada_fuse.gpr

uninstall:
	rm -r $(I_INC)
	rm -r $(I_LIB)
	rm $(I_GPR)/ada_fuse.gpr

lib/libada_fuse.a:
	@echo 'run "make all" first' && false
