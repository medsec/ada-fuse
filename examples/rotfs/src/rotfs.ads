with Fuse.Main;
with Fuse.Aux;

package RotFS is

   RotFS_Error : exception;

   -- use user data to store the path to the source directory
   type User_Data_Type is
      record
         Root_Dir : access String;
      end record;

   -- see hello_world for the rest
   pragma Warnings (Off, "* bits of * unused");

   package Fuse_Rot is new Fuse.Main
     (Element_Type => Character,
      Element_Array => String,
      User_Data_Type => User_Data_Type);

   pragma Warnings (On, "* bits of * unused");

   use Fuse_Rot;


   function GetAttr
     (Path   : in String;
      St_Buf : access System.Stat_Type)
      return System.Error_Type;

   package Rot_GetAttr is new Fuse_Rot.GetAttr;


   function ReadLink
     (Path    : in String;
      Link    : out String;
      Size    : out Natural)
      return System.Error_Type;

   package Rot_ReadLink is new Fuse_Rot.ReadLink;


   function MkNod
     (Path   : in String;
      Mode   : in System.St_Mode_Type;
      Dev    : in System.Dev_T)
      return System.Error_Type;

   package Rot_MkNod is new Fuse_Rot.MkNod;


   function MkDir
     (Path   : in String;
      Mode   : in System.St_Mode_Type)
      return System.Error_Type;

   package Rot_MkDir is new Fuse_Rot.MkDir;


   function Unlink
     (Path   : in String)
      return System.Error_Type;

   package Rot_Unlink is new Fuse_Rot.Unlink;


   function RmDir
     (Path   : in String)
      return System.Error_Type;

   package Rot_RmDir is new Fuse_Rot.RmDir;


   function SymLink
     (Path    : in String;
      Link    : in String)
      return System.Error_Type;

   package Rot_SymLink is new Fuse_Rot.SymLink;


   function Rename
     (Path_Old : in String;
      Path_New : in String)
      return System.Error_Type;

   package Rot_Rename is new Fuse_Rot.Rename;


   function Link
     (Path     : in String;
      New_Path : in String)
      return System.Error_Type;

   package Rot_Link is new Fuse_Rot.Link;


   function ChMod
     (Path    : in String;
      Mode    : in System.St_Mode_Type)
      return System.Error_Type;

   package Rot_ChMod is new Fuse_Rot.ChMod;


   function ChOwn
     (Path   : in String;
      UID    : in System.UID_T;
      GID    : in System.GID_T)
      return System.Error_Type;

   package Rot_ChOwn is new Fuse_Rot.ChOwn;


   function Truncate
     (Path   : in String;
      Size   : in Natural)
      return System.Error_Type;

   package Rot_Truncate is new Fuse_Rot.Truncate;


   function UTime
     (Path   : in String;
      Buffer : access System.UTimeBuffer_Type)
      return System.Error_Type;

   package Rot_UTime is new Fuse_Rot.UTime;


   function Create
     (Path   : in String;
      Mode   : in System.St_Mode_Type;
      Fi     : access System.File_Info_Type)
      return System.Error_Type;

   -- package Rot_Create is new Fuse_Rot.Create;
      --  either mknod or create is sufficient


   function Open
     (Path   : in String;
      Fi     : access System.File_Info_Type)
      return System.Error_Type;

   package Rot_Open is new Fuse_Rot.Open;


   function Release
     (Path   : in String;
      Fi     : access System.File_Info_Type)
      return System.Error_Type;

   package Rot_Release is new Fuse_Rot.Release;


   function Read
     (Path   : in String;
      Buffer : access Buffer_Type;
      Size   : in out Natural;
      Offset : in Natural;
      Fi     : access System.File_Info_Type)
      return System.Error_Type;

   package Rot_Read is new Fuse_Rot.Read;


   function Write
     (Path   : in String;
      Buffer : access Buffer_Type;
      Size   : in out Natural;
      Offset : in Natural;
      Fi     : access System.File_Info_Type)
      return System.Error_Type;

   package Rot_Write is new Fuse_Rot.Write;


   function ReadDir
     (Path   : in String;
      Filler : access procedure
                 (Name     : String;
                  St_Buf   : System.Stat_Access;
                  Offset   : Natural);
      Offset : in Natural;
      Fi     : access System.File_Info_Type)
      return System.Error_Type;

   package Rot_ReadDir is new Fuse_Rot.ReadDir;


   function StatFS
     (Path   : in String;
      Buffer : access System.StatVFS_Type)
      return System.Error_Type;

   package Rot_StatFS is new Fuse_Rot.StatFS;

private

   package Aux is new Fuse.Aux (Fuse_Rot);

end RotFS;

-- vim: ts=3 sw=3 et
