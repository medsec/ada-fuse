#!/usr/bin/env python3
import subprocess
from os.path import *
import shutil

TEMPLATE="""abstract project Libfuse is
   package Linker is
      for Linker_Options use (%(libs)s);
   end Linker;
end Libfuse;"""

make_dir = dirname(shutil.which("make"))
pkg_config_dir = dirname(shutil.which("pkg-config"))
gprbuild_dir = dirname(shutil.which("gprbuild"))
if gprbuild_dir == make_dir:
    pkg_config = "pkg-config"
else:
    pkg_config = join(make_dir,"pkg-config")
libs=[]
for i in subprocess.check_output([pkg_config,"--libs","fuse"]).decode("utf-8").strip().split():
    libs.append('"%s"' % i)
    
with open("libfuse.gpr","w") as outf:
    outf.write (TEMPLATE % {"libs":",".join(libs)})
