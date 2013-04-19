-- Auxiliary functions for passthrough filesystems.
--
-- Just call these in your filesystem implementation or copy them if
-- you want to modify the behaviour.
--
-- We cannot read errno yet. So we just catch the most common errors
-- by hand. If anything goes wrong an exception is raised and we
-- return an IO error (EIO).

with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Unchecked_Deallocation;

with Fuse.Main;

generic

   with package Main is new Fuse.Main (<>);

package Fuse.Aux is

   use Main;

   package C renames Interfaces.C;

   function GetAttr
     (Path   : in String;
      Buffer : access System.Stat_Type)
      return System.Error_Type;

   -- GetAttr uses C's lstat which doesn't work on every OS. If you run into
   -- problems you can use Simple_GetAttr which uses only Ada function. However
   -- it provides only basic functionality.
   function Simple_GetAttr
     (Path   : in String;
      Buffer : access System.Stat_Type)
      return System.Error_Type;

   function ReadLink
     (Path    : in String;
      Link    : out String;
      Size    : out Natural)
      return System.Error_Type;

   function MkNod
     (Path   : in String;
      Mode   : in System.St_Mode_Type;
      Dev    : in System.Dev_T)
      return System.Error_Type;

   function MkDir
     (Path   : in String)
      return System.Error_Type;

   function Unlink
     (Path   : in String)
      return System.Error_Type;

   function RmDir
     (Path   : in String)
      return System.Error_Type;

   function SymLink
     (Path    : in String;
      Link    : in String)
      return System.Error_Type;

   function Rename
     (Path_Old : in String;
      Path_New : in String)
      return System.Error_Type;

   function Link
     (Path     : in String;
      New_Path : in String)
      return System.Error_Type;

   function ChMod
     (Path    : in String;
      Mode    : in System.St_Mode_Type)
      return System.Error_Type;

   function ChOwn
     (Path   : in String;
      UID    : in System.UID_T;
      GID    : in System.GID_T)
      return System.Error_Type;

   function Truncate
     (Path   : in String;
      Length : in Natural)
      return System.Error_Type;

   function UTime
     (Path   : in String;
      Buffer : access System.UTimeBuffer_Type)
      return System.Error_Type;

   -- provides basic functionality by Ada functions
   function Create
     (Path   : in String;
      Fi     : access System.File_Info_Type)
      return System.Error_Type;

   function Open
     (Path   : in String;
      Fi     : access System.File_Info_Type)
      return System.Error_Type;

   -- can be used for Read
   function Read_File
     (File   : in IO.File_Type;
      Offset : in Natural;
      Size   : in Natural)
      return Element_Array;

   function Read
     (Path   : String;
      Buffer : access Buffer_Type;
      Size   : in out Natural;
      Offset : in Natural;
      Fi     : access System.File_Info_Type)
      return System.Error_Type;

   -- can be used for Write
   procedure Write_File
     (File   : in IO.File_Type;
      Data   : in Element_Array);

   function Write
     (Path   : String;
      Buffer : access Buffer_Type;
      Size   : in out Natural;
      Offset : in Natural;
      Fi     : access System.File_Info_Type)
      return System.Error_Type;

   function StatFS
     (Path   : in String;
      Buffer : access System.StatVFS_Type)
      return System.Error_Type;

   function Release
     (Fi     : access System.File_Info_Type)
      return System.Error_Type;

   function ReadDir
     (Path   : in String;
      Filler : access procedure
                 (Name     : String;
                  St_Buf   : System.Stat_Access;
                  Offset   : Natural))
      return System.Error_Type;



------------------------------------------------------------------------------

-- The following functions are needed for the wrapped functions above. Take a
-- look at those first, but in case one needs them, we didn't declare them
-- private.

   procedure Delete_File_Type is new
      Ada.Unchecked_Deallocation (IO.File_Type, File_Access);

   function ReadLink_C
     (Path   : C.Strings.chars_ptr;
      Buf    : C.Strings.char_array_access;
      BufSiz : C.Size_T) return System.SSize_T;
   pragma Import (C, ReadLink_C, "readlink");

   function MkNod_C
     (pathname : C.Strings.chars_ptr;
      mode     : System.Mode_T;
      dev      : System.Dev_T) return C.int;
   pragma Import (C, MkNod_C, "mknod");

   function LStat_C
     (Path   : C.Strings.chars_ptr;
      Buf    : System.Stat_Access) return C.int;
   pragma Import (C, LStat_C, "lstat");

   function Unlink_C
     (pathname : C.Strings.chars_ptr) return C.int;
   pragma Import (C, Unlink_C, "unlink");

   function SymLink_C
     (oldpath : C.Strings.chars_ptr;
      newpath : C.Strings.chars_ptr) return C.int;
   pragma Import (C, SymLink_C, "symlink");

   function Link_C
     (oldpath : C.Strings.chars_ptr;
      newpath : C.Strings.chars_ptr) return C.int;
   pragma Import (C, Link_C, "link");

   function ChMod_C
     (path   : C.Strings.chars_ptr;
      mode   : System.Mode_T) return C.int;
   pragma Import (C, ChMod_C, "chmod");

   function ChOwn_C
     (path   : C.Strings.chars_ptr;
      uid    : System.UID_T;
      gid    : System.GID_T) return C.int;
   pragma Import (C, ChOwn_C, "chown");

   function Truncate_C
     (Path   : C.Strings.chars_ptr;
      Length : System.Off_T) return C.int;
   pragma Import (C, Truncate_C, "truncate");

   function UTime_C
     (filename : C.Strings.chars_ptr;
      times    : System.UTimeBuffer_Access) return C.int;
   pragma Import (C, UTime_C, "utime");

   function StatVFS_C
     (Path   : Interfaces.C.Strings.chars_ptr;
      Statv  : System.StatVFS_Access) return C.int;
   pragma Import(C, StatVFS_C, "statvfs");

   function Get_UID return System.UID_T;
   pragma Import(C, Get_UID, "getuid");

   function Get_GID return System.GID_T;
   pragma Import(C, Get_GID, "getgid");

--   Errno : Interfaces.C.int;
--   pragma Import (C, Errno, "errno");
--
--   function Get_Error is
--      new Ada.Unchecked_Conversion (Interfaces.C.int, System.Error_Type);

--   doesn't work because of TLS issues

end Fuse.Aux;

-- vim: ts=3 sw=3 et
