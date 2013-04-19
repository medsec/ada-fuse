-- A simple filesystem, which will pass through most operations to a source
-- directory. However it stores everything rot13 encrypted.
--
-- call with
-- ./rotfs_main source_directory mount_point -s

with Ada.Directories;
with Ada.Command_Line;

with Fuse;
with RotFS;


procedure RotFS_Main is

   User_Data : RotFS.User_Data_Type;
   Arguments : Fuse.Arguments_Type;

begin

   -- Catch the first argument given and use it as source directory...
   User_Data.Root_Dir :=
      new String'(Ada.Directories.Full_Name (Ada.Command_Line.Argument(1)));

   -- ...and pass the rest to Fuse.
   for I in 2 .. Ada.Command_Line.Argument_Count loop
      Arguments.Append (Ada.Command_Line.Argument (I));
   end loop;

   RotFS.Fuse_Rot.General.Put_Message("Root Dir: " & User_Data.Root_Dir.all);

   -- call Fuse
   RotFS.Fuse_Rot.Main (Arguments, User_Data);

end RotFS_Main;

-- vim: ts=3 sw=3 et
