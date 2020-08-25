-- In order to create a filesystem instantiate Fuse.Main.
-- Fuse.General and Fuse.System will be instatiated through Main.
-- If you want to build a passthrough filesystem, you can use Fuse.Aux.

with Ada.Containers.Indefinite_Vectors;


package Fuse is

   Fuse_Error : exception;

   type Log_Level_Type is new Integer range 0..9;
   -- see Fuse.Main

   package String_Vectors is
      new Ada.Containers.Indefinite_Vectors (Positive, String);
   subtype Arguments_Type is String_Vectors.Vector;
   -- This is used to pass command line arguments to Fuse.
   -- Fill the Vector and pass it to Fuse.Main.Main.

   function Get_Arguments return Arguments_Type;
   -- Passes every given command line argument.

   type Null_Data is null record;
   -- Use for User_Data if not needed

end Fuse;

-- vim: ts=3 sw=3 et
