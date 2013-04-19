-- This is a clone of the hello world in C found at
-- http://fuse.sourceforge.net/helloworld.html

with Hello_World;
with Fuse;

procedure Hello_World_Main is

   -- we don't need any user data
   User_Data : Fuse.Null_Data;

begin

   -- call the main procedure, which will call fuse
   Hello_World.Fuse_Hello.Main (User_Data => User_Data);

end Hello_World_Main;

-- vim: ts=3 sw=3 et
