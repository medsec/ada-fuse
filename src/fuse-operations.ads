-- Only needed because Ada wants to know everything.
-- Ignore.

with Interfaces.C;
with Interfaces.C.Strings;

with Fuse.System;
with Fuse.General;

generic

   with package System is new Fuse.System (<>);
   with package General is new Fuse.General (<>);
   type Buffer_Access is private;
   type User_Data_Access is private;

package Fuse.Operations is

   --------------------------
   --  Get Attributes
   --------------------------
   type GetAttr_C_Type is access function
     (Path     : Interfaces.C.Strings.chars_ptr;
      St_Buf   : System.Stat_Access)
      return Interfaces.C.int;

   --------------------------
   --  Read Link
   --------------------------
   type ReadLink_C_Type is access function
     (Path    : Interfaces.C.Strings.chars_ptr;
      Link    : Interfaces.C.Strings.chars_ptr;
      Size    : Interfaces.C.Size_T)
      return Interfaces.C.int;

   --------------------------
   --  Make Node    
   --------------------------
   type MkNod_C_Type is access function
     (Path  : Interfaces.C.Strings.chars_ptr;
      Mode  : System.Mode_T;
      Dev   : System.Dev_T)
      return Interfaces.C.int;

   --------------------------
   --  Make Directory
   --------------------------
    type MkDir_C_Type is access function
      (Path : Interfaces.C.Strings.chars_ptr;
       Mode : System.Mode_T)
       return Interfaces.C.int;

   --------------------------
   --  Unlink
   --------------------------
   type Unlink_C_Type is access function
     (Path    : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.int;

   --------------------------
   --  Remove Directory
   --------------------------
   type RmDir_C_Type is access function
       (Path : Interfaces.C.Strings.chars_ptr)
       return Interfaces.C.int;

   --------------------------
   --  Symlink
   --------------------------
   type SymLink_C_Type is access function
     (Path    : Interfaces.C.Strings.chars_ptr;
      Link    : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.int;

   --------------------------
   --  Rename
   --------------------------
   type Rename_C_Type is access function
     (Path_Old : Interfaces.C.Strings.chars_ptr;
      Path_New : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.int;

   --------------------------
   --  Link
   --------------------------
   type Link_C_Type is access function
     (Path    : Interfaces.C.Strings.chars_ptr;
      NewPath : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.int;

   --------------------------
   --  Change Mode
   --------------------------
   type Chmod_C_Type is access function
     (Path  : Interfaces.C.Strings.chars_ptr;
      Mode  : System.Mode_T)
      return Interfaces.C.int;

   --------------------------
   --  Change Owner
   --------------------------
   type ChOwn_C_Type is access function
     (Path  : Interfaces.C.Strings.chars_ptr;
      UID   : System.UID_T;
      GID   : System.GID_T)
      return Interfaces.C.int;

   --------------------------
   --  Truncate
   --------------------------
   type Truncate_C_Type is access function
     (Path    : Interfaces.C.Strings.chars_ptr;
      Off_T   : System.Off_T)
      return Interfaces.C.int;

   --------------------------
   --  U Time
   --------------------------
   type UTime_C_Type is access function
     (Path  : Interfaces.C.Strings.chars_ptr;
      UBuf  : System.UTimeBuffer_Access)
      return Interfaces.C.int;

   --------------------------
   --  Open
   --------------------------
   type Open_C_Type is access function
     (Path     : Interfaces.C.Strings.chars_ptr;
      Fi       : System.File_Info_Access)
      return Interfaces.C.int;

   --------------------------
   --  Read
   --------------------------
   type Read_C_Type is access function
     (Path     : Interfaces.C.Strings.chars_ptr;
      Buf      : Buffer_Access;
      Size     : Interfaces.C.Size_T;
      Off      : System.Off_T;
      Fi       : System.File_Info_Access)
      return Interfaces.C.int;

   --------------------------
   --  Write
   --------------------------
   type Write_C_Type is access function
     (Path     : Interfaces.C.Strings.chars_ptr;
      Buf      : Buffer_Access;
      Size     : Interfaces.C.Size_T;
      Off      : System.Off_T;
      Fi       : System.File_Info_Access)
      return Interfaces.C.int;

   --------------------------
   --  Statfs
   --------------------------
   type StatFS_C_Type is access function
     (Path   : Interfaces.C.Strings.chars_ptr;
      Statv  : System.StatVFS_Access)
      return Interfaces.C.int;

   --------------------------
   --  Flush
   --------------------------
   type Flush_C_Type is access function
     (Path    : Interfaces.C.Strings.chars_ptr;
      Fi      : System.File_Info_Access)
      return Interfaces.C.int;

   --------------------------
   --  Release
   --------------------------
   type Release_C_Type is access function
     (Path    : Interfaces.C.Strings.chars_ptr;
      Fi      : System.File_Info_Access)
      return Interfaces.C.int;

   --------------------------
   --  FSync
   --------------------------
   type FSync_C_Type is access function
     (Path     : Interfaces.C.Strings.chars_ptr;
      DataSync : Interfaces.C.int;
      Fi       : System.File_Info_Access)
      return Interfaces.C.int;

   --------------------------
   --  SetXAttribute
   --------------------------
  type SetXAttr_C_Type is access function
    (Path    : Interfaces.C.Strings.chars_ptr;
     Name    : Interfaces.C.Strings.chars_ptr;
     Value   : Interfaces.C.Strings.chars_ptr;
     Size    : Interfaces.C.size_t;
     Flags   : Interfaces.C.int)
     return Interfaces.C.int;

   --------------------------
   --  GetXAttribute
   --------------------------
  type GetXAttr_C_Type is access function
    (Path    : Interfaces.C.Strings.chars_ptr;
     Name    : Interfaces.C.Strings.chars_ptr;
     Value   : Interfaces.C.Strings.chars_ptr;
     Size    : Interfaces.C.size_t)
     return Interfaces.C.int;

   --------------------------
   --  ListXAttribute
   --------------------------
  type ListXAttr_C_Type is access function
    (Path    : Interfaces.C.Strings.chars_ptr;
     List    : Interfaces.C.Strings.chars_ptr;
     Size    : Interfaces.C.size_t)
     return Interfaces.C.int;

   --------------------------
   --  RemoveXAttribute
   --------------------------
  type RemoveXAttr_C_Type is access function
    (Path    : Interfaces.C.Strings.chars_ptr;
     Name    : Interfaces.C.Strings.chars_ptr)
     return Interfaces.C.int;

   --------------------------
   --  Open Directory
   --------------------------
   type OpenDir_C_Type is access function
     (Path  : Interfaces.C.Strings.chars_ptr;
      Fi     : System.File_Info_Access)
      return Interfaces.C.int;

   --------------------------
   --  Read Directory
   --------------------------
   type Fill_Dir_T is access function
     (Buf      : General.Dir_Buffer_Type;
      Name     : Interfaces.C.Strings.chars_ptr;
      Stbuf    : System.Stat_Access;
      Off      : System.Off_T )
      return Interfaces.C.int;

   pragma Convention (C, Fill_Dir_T);

   type ReadDir_C_Type is access function
     (Path     : Interfaces.C.Strings.chars_ptr;
      Buf      : General.Dir_Buffer_Type;
      Filler   : Fill_Dir_T;
      Off      : System.Off_T;
      Fi       : System.File_Info_Access)
      return Interfaces.C.int;

   --------------------------
   --  Release Directory
   --------------------------
   type ReleaseDir_C_Type is access function
     (Path  : Interfaces.C.Strings.chars_ptr;
      Fi     : System.File_Info_Access)
      return Interfaces.C.int;

   --------------------------
   --  FSync Directory
   --------------------------
   type FSyncDir_C_Type is access function
     (Path     : Interfaces.C.Strings.chars_ptr;
      DataSync : Interfaces.C.int;
      Fi       : System.File_Info_Access)
      return Interfaces.C.int;

   --------------------------
   --  Init
   --------------------------
   type Init_C_Type is access function
     (Conn : System.Fuse_Conn_Info_Access)
     return Interfaces.C.int;

   --------------------------
   --  Destroy
   --------------------------
   type Destroy_C_Type is access function
     (Userdata : User_Data_Access)
     return Interfaces.C.int;

   --------------------------
   --  Access
   --------------------------
   type Access_C_Type is access function
     (Path  : Interfaces.C.Strings.chars_ptr;
      Mask  : Interfaces.C.int)
      return Interfaces.C.int;

   --------------------------
   --  Create
   --------------------------
   type Create_C_Type is access function
     (Path    : Interfaces.C.Strings.chars_ptr;
      Mode    : System.Mode_T;
      Fi      : System.File_Info_Access)
      return Interfaces.C.int;


   --------------------------
   --  FTruncate
   --------------------------
   type FTruncate_C_Type is access function
     (Path    : Interfaces.C.Strings.chars_ptr;
      Off_T   : System.Off_T;
      Fi      : System.File_Info_Access)
      return Interfaces.C.int;

   --------------------------
   --  FGet Attributes
   --------------------------
   type FGetAttr_C_Type is access function
     (Path     : Interfaces.C.Strings.chars_ptr;
      St_Buf   : System.Stat_Access;
      Fi       : System.File_Info_Access)
      return Interfaces.C.int;

   --------------------------
   --  Lock
   --------------------------

   --------------------------
   --  U-Time Nanoseconds
   --------------------------

   --------------------------
   --  BMap
   --------------------------

   --------------------------
   --  IO Control
   --------------------------

   --------------------------
   --  Poll
   --------------------------

end Fuse.Operations;

-- vim: ts=3 sw=3 et
