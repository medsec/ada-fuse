with Fuse.Main;

package Hello_World is

   Hello_Path   : constant String := "/hello";
   Hello_String : constant String := "Hello World!" & Standard.ASCII.LF;

   -- suppress warnings about Fuse not using every byte of its data structures
   pragma Warnings (Off, "* bits of * unused");

   -- instantiate Fuse.Main where we will register our functions
   -- and actually call fuse
   package Fuse_Hello is new Fuse.Main
     (Element_Type => Character,
      Element_Array => String,
      User_Data_Type => Fuse.Null_Data);

   pragma Warnings (On, "* bits of * unused");

   use Fuse_Hello;


   -- declare necessary functions
   function GetAttr
     (Path   : in String;
      St_Buf : access System.Stat_Type)
      return System.Error_Type;

   function Open
     (Path   : in String;
      Fi     : access System.File_Info_Type)
      return System.Error_Type;

   function Read
     (Path   : in String;
      Buffer : access Buffer_Type;
      Size   : in out Natural;
      Offset : in Natural;
      Fi     : access System.File_Info_Type)
      return System.Error_Type;

   function ReadDir
     (Path   : in String;
      Filler : access procedure
                 (Name     : String;
                  St_Buf   : System.Stat_Access;
                  Offset   : Natural);
      Offset : in Natural;
      Fi     : access System.File_Info_Type)
      return System.Error_Type;


   -- register functions
   package Hello_GetAttr is new Fuse_Hello.GetAttr;
   package Hello_Open is new Fuse_Hello.Open;
   package Hello_Read is new Fuse_Hello.Read;
   package Hello_ReadDir is new Fuse_Hello.ReadDir;

end Hello_World;

-- vim: ts=3 sw=3 et
