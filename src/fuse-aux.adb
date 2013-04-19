with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Calendar.Conversions;

package body Fuse.Aux is

   package Dir renames Ada.Directories;

   --------------------------
   --        GetAttr       --
   --------------------------
   function GetAttr
     (Path   : in String;
      Buffer : access System.Stat_Type)
      return System.Error_Type
   is
      Path_Ptr : C.Strings.chars_ptr;
      Ret      : C.int;
      use type C.int;

   begin

      Path_Ptr := C.Strings.New_String (Path);
      Ret := LStat_C (Path_Ptr, System.Stat_Access(Buffer));
      C.Strings.Free (Path_Ptr);

      if Ret = 0 then
         return System.EXIT_SUCCESS;

      else
         if not Dir.Exists (Path) then
            return System.ENOENT;
         end if;

         raise Fuse.Fuse_Error with "LStat failed";

      end if;

   end GetAttr;


   --------------------------
   --    Simple GetAttr    --
   --------------------------
   function Simple_GetAttr
     (Path   : in String;
      Buffer : access System.Stat_Type)
      return System.Error_Type
   is
      use type Dir.File_Kind;
      use type System.St_Mode_Type;

   begin

      if not Dir.Exists (Path) then
         return System.ENOENT;

      elsif Dir.Kind (Path) = Dir.Directory then
         Buffer.St_Mode := System.S_IFDIR or 8#755#;

      else
         Buffer.St_Mode := System.S_IFREG or 8#644#;
         Buffer.St_Size := System.Off_T (Dir.Size (Path));

      end if;

      Buffer.St_UID := Get_UID;
      Buffer.St_GID := Get_GID;
      Buffer.St_Mtime := System.Time_T
                           (Ada.Calendar.Conversions.To_Unix_Time
                              (Dir.Modification_Time (Path)));

      return System.EXIT_SUCCESS;

   end Simple_GetAttr;


   --------------------------
   --      Read Link       --
   --------------------------
   function ReadLink
     (Path    : in String;
      Link    : out String;
      Size    : out Natural)
      return System.Error_Type
   is
      procedure Free is new
         Ada.Unchecked_Deallocation (C.char_array, C.Strings.char_array_access);

      Path_Ptr : C.Strings.chars_ptr := C.Strings.New_String (Path);
      Buf : C.Strings.char_array_access := new C.char_array (1 .. Link'Length);

   begin

      Size := Natural
        (ReadLink_C
           (Path_Ptr,
            Buf,
            C.Size_T (Link'Length)));

      if Size > Link'Length then Size := Link'Length; end if;

      Link := C.To_Ada (Buf.all, Trim_Nul => False);

      Free (Buf);
      C.Strings.Free (Path_Ptr);

      return System.EXIT_SUCCESS;

   end ReadLink;


   --------------------------
   --         MkNod        --
   --------------------------
   function MkNod
     (Path   : in String;
      Mode   : in System.St_Mode_Type;
      Dev    : in System.Dev_T)
      return System.Error_Type
   is
      Path_Ptr : C.Strings.chars_ptr;
      Ret      : C.int;
      use type C.int;

   begin

      Path_Ptr := C.Strings.New_String (Path);
      Ret := MkNod_C (Path_Ptr, System.St_Mode_To_Mode_T (Mode), Dev);
      C.Strings.Free (Path_Ptr);

      if Ret = 0 then
         return System.EXIT_SUCCESS;

      else
         raise Fuse.Fuse_Error with "MkNod failed";

      end if;

   end MkNod;


   --------------------------
   --         MkDir        --
   --------------------------
   function MkDir
     (Path   : in String)
      return System.Error_Type

   is begin

      Dir.Create_Directory (Path);
      return System.EXIT_SUCCESS;

   end MkDir;


   --------------------------
   --         Unlink       --
   --------------------------
   function Unlink
     (Path   : in String)
      return System.Error_Type

   is
      Path_Ptr : C.Strings.chars_ptr;
      Ret      : C.int;
      use type C.int;

   begin

      Path_Ptr := C.Strings.New_String (Path);
      Ret := Unlink_C (Path_Ptr);
      C.Strings.Free (Path_Ptr);

      if Ret = 0 then
         return System.EXIT_SUCCESS;

      else
         raise Fuse.Fuse_Error with "Unlink failed";

      end if;

      -- Dir.Delete_File (Path);
      -- can be used, too, but cannot remove broken symlinks

   end Unlink;


   --------------------------
   --         RmDir        --
   --------------------------
   function RmDir
     (Path   : in String)
      return System.Error_Type

   is begin

      begin
         Dir.Delete_Directory (Path);
      exception when Dir.USE_ERROR =>
         return System.ENOTEMPTY;
      end;

      return System.EXIT_SUCCESS;

   end RmDir;


   --------------------------
   --        Symlink       --
   --------------------------
   function SymLink
     (Path    : in String;
      Link    : in String)
      return System.Error_Type

   is
      use type C.int;
      oldpath : C.Strings.chars_ptr;
      newpath : C.Strings.chars_ptr;
      Ret : C.int;

   begin

      if Path = "" then
         return System.ENOENT;

      elsif Dir.Exists (Link) then
         return System.EEXIST;

      elsif not Dir.Exists (Dir.Containing_Directory (Link)) then
         return System.ENOENT;

      end if;

      oldpath := C.Strings.New_String (Path);
      newpath := C.Strings.New_String (Link);

      Ret := SymLink_C (oldpath, newpath);

      C.Strings.Free (oldpath);
      C.Strings.Free (newpath);

      if Ret = 0 then
         return System.EXIT_SUCCESS;

      else
         raise Fuse.Fuse_Error with "SymLink failed";

      end if;

   end SymLink;


   --------------------------
   --         Link         --
   --------------------------
   function Link
     (Path     : in String;
      New_Path : in String)
      return System.Error_Type
   is
      use type C.int;
      use type Dir.File_Kind;
      oldpath : C.Strings.chars_ptr;
      newpath : C.Strings.chars_ptr;
      Ret : C.int;

   begin

      if not Dir.Exists (Path) then
         return System.ENOENT;

      elsif Dir.Kind (Path) = Dir.Directory then
         return System.EPERM;

      elsif Dir.Exists (New_Path) then
         return System.EEXIST;

      elsif not Dir.Exists (Dir.Containing_Directory (New_Path)) then
         return System.ENOENT;

      end if;

      oldpath := C.Strings.New_String (Path);
      newpath := C.Strings.New_String (New_Path);

      Ret := Link_C (oldpath, newpath);

      C.Strings.Free (oldpath);
      C.Strings.Free (newpath);

      if Ret = 0 then
         return System.EXIT_SUCCESS;

      else
         raise Fuse.Fuse_Error with "Link failed";

      end if;

   end Link;


   --------------------------
   --        Rename        --
   --------------------------
   function Rename
     (Path_Old : in String;
      Path_New : in String)
      return System.Error_Type
   is
      use type Dir.File_Kind;

   begin

      if not Dir.Exists (Path_Old) then
         return System.ENOENT;

      elsif Dir.Exists (Path_New) then

         if Dir.Kind (Path_Old) = Dir.Ordinary_File
         and Dir.Kind (Path_New) = Dir.Ordinary_File then
            Dir.Delete_File (Path_New);
            -- TODO check for special files

         elsif Dir.Kind (Path_Old) = Dir.Ordinary_File
         and Dir.Kind (Path_New) = Dir.Directory then
            return System.EISDIR;

         elsif Dir.Kind (Path_Old) = Dir.Directory
         and Dir.Kind (Path_New) = Dir.Ordinary_File then
            return System.ENOTDIR;

         end if;

      end if;

      Dir.Rename (Path_Old, Path_New);

      return System.EXIT_SUCCESS;

   end Rename;

   --------------------------
   --         ChMod        --
   --------------------------
   function ChMod
     (Path    : in String;
      Mode    : in System.St_Mode_Type)
      return System.Error_Type
   is
      Path_Ptr : C.Strings.chars_ptr;
      Ret      : C.int;
      use type C.int;

   begin

      Path_Ptr := C.Strings.New_String (Path);
      Ret := ChMod_C (Path_Ptr, System.St_Mode_To_Mode_T (Mode));
      C.Strings.Free (Path_Ptr);

      if Ret = 0 then
         return System.EXIT_SUCCESS;

      else
         raise Fuse.Fuse_Error with "ChMod failed";

      end if;

   end ChMod;

   --------------------------
   --         ChOwn        --
   --------------------------
   function ChOwn
     (Path   : in String;
      UID    : in System.UID_T;
      GID    : in System.GID_T)
      return System.Error_Type
   is
      Path_Ptr : C.Strings.chars_ptr;
      Ret      : C.int;
      use type C.int;

   begin

      Path_Ptr := C.Strings.New_String (Path);
      Ret := ChOwn_C (Path_Ptr, UID, GID);
      C.Strings.Free (Path_Ptr);

      if Ret = 0 then
         return System.EXIT_SUCCESS;

      else
         return System.EPERM;
         -- Fuse should take care of most other errors that can occur
         -- and since we can't read errno yet, this should be best for now

      end if;

   end ChOwn;

   --------------------------
   --       Truncate       --
   --------------------------
   function Truncate
     (Path   : in String;
      Length : in Natural)
      return System.Error_Type
   is
      Path_Ptr : C.Strings.chars_ptr;
      Ret      : C.int;
      use type C.int;

   begin

      if not Dir.Exists (Path) then
         return System.ENOENT;
      end if;

      Path_Ptr := C.Strings.New_String (Path);
      Ret := Truncate_C (Path_Ptr, System.Off_T(Length));
      C.Strings.Free (Path_Ptr);

      if Ret = 0 then
         return System.EXIT_SUCCESS;

      else
         raise Fuse.Fuse_Error with "Truncate failed";

      end if;

   end Truncate;


   --------------------------
   --        UTime         --
   --------------------------
   function UTime
     (Path   : in String;
      Buffer : access System.UTimeBuffer_Type)
      return System.Error_Type
   is
      Path_Ptr : C.Strings.chars_ptr;
      Ret      : C.int;
      use type C.int;

   begin

      Path_Ptr := C.Strings.New_String (Path);
      Ret := UTime_C (Path_Ptr, System.UTimeBuffer_Access (Buffer));
      C.Strings.Free (Path_Ptr);

      if Ret = 0 then
         return System.EXIT_SUCCESS;

      else
         raise Fuse.Fuse_Error with "UTime failed";

      end if;

   end UTime;


   --------------------------
   --        Create        --
   --------------------------
   function Create
     (Path   : in String;
      Fi     : access System.File_Info_Type)
      return System.Error_Type
   is
      use type System.RW_Type;

   begin

      if Dir.Exists (Path) then
         return System.EEXIST;
      end if;

      Fi.Fh := new IO.File_Type;

      if Fi.Flags.RW = System.O_RDONLY then
         IO.Create (Fi.Fh.all, IO.In_File, Path);

      elsif Fi.Flags.RW = System.O_WRONLY then
         IO.Create (Fi.Fh.all, IO.Out_File, Path);

      elsif Fi.Flags.RW = System.O_RDWR then
         IO.Create (Fi.Fh.all, IO.InOut_File, Path);

      else
         Delete_File_Type (Fi.Fh);
         raise Fuse.Fuse_Error with "Create: mode not supported";

      end if;

      return System.EXIT_SUCCESS;

   end Create;


   --------------------------
   --         Open         --
   --------------------------
   function Open
     (Path   : in String;
      Fi     : access System.File_Info_Type)
      return System.Error_Type
   is
      use type System.RW_Type;

   begin

      if not Dir.Exists (Path) then
         return System.ENOENT;
      end if;

      Fi.Fh := new IO.File_Type;

      begin

         if Fi.Flags.RW = System.O_RDONLY then
            IO.Open (Fi.Fh.all, IO.In_File, Path);

         elsif Fi.Flags.RW = System.O_WRONLY then
            IO.Open (Fi.Fh.all, IO.Out_File, Path);

         elsif Fi.Flags.RW = System.O_RDWR then
            IO.Open (Fi.Fh.all, IO.InOut_File, Path);

         else
            Delete_File_Type (Fi.Fh);
            raise Fuse.Fuse_Error with "Open: mode not supported";

         end if;

      exception

         when Ada.IO_Exceptions.Use_Error =>
            Delete_File_Type (Fi.Fh);
            return System.EEXIST;

      end;

      return System.EXIT_SUCCESS;

   end Open;


   --------------------------
   --          Read        --
   --------------------------
   function Read_File
     (File   : in IO.File_Type;
      Offset : in Natural;
      Size   : in Natural)
      return Element_Array
   is
      Data : Element_Array (Offset + 1 .. Offset + Size);

   begin

      for I in Data'Range loop
         IO.Read (File, Data (I), IO.Count (I));
      end loop;

      return Data;

   end Read_File;


   function Read
     (Path   : in String;
      Buffer : access Buffer_Type;
      Size   : in out Natural;
      Offset : in Natural;
      Fi     : access System.File_Info_Type)
      return System.Error_Type
   is
      use type Dir.File_Kind;
      Length : constant Natural := Natural (Dir.Size (Path));

   begin

      if not Dir.Exists (Path) or else Dir.Kind (Path) /= Dir.Ordinary_File then
         -- TODO check if working with special files, see write too
         return System.ENOENT;
      end if;

      if Offset >= Length then
         Size := 0;

      elsif Offset + Size > Length then
         Size := Length - Offset;

      end if;

      Buffer(1 .. Size) := Read_File (Fi.Fh.all, Offset, Size);

      return System.EXIT_SUCCESS;

   end Read;


   --------------------------
   --         Write        --
   --------------------------
   procedure Write_File
     (File   : in IO.File_Type;
      Data   : in Element_Array)

   is begin

      for I in Data'Range loop
         IO.Write (File, Data (I), IO.Count (I));
      end loop;

   end Write_File;


   function Write
     (Path   : String;
      Buffer : access Buffer_Type;
      Size   : in out Natural;
      Offset : in Natural;
      Fi     : access System.File_Info_Type)
      return System.Error_Type
   is
      pragma Unmodified (Size);
      -- according to the Fuse docu Size should not be modified, however in C
      -- it can, so we made it 'in out'

      use type Dir.File_Kind;
      Data : constant Element_Array (Offset + 1 .. Offset + Size)
                        := Buffer (1 .. Size);

   begin

      if not Dir.Exists (Path) or else Dir.Kind (Path) /= Dir.Ordinary_File then
         return System.ENOENT;
      end if;

      Write_File (Fi.Fh.all, Data);

      return System.EXIT_SUCCESS;

   end Write;


   --------------------------
   --        StatFS       --
   --------------------------
   function StatFS
     (Path   : in String;
      Buffer : access System.StatVFS_Type)
      return System.Error_Type
   is
      Path_Ptr : C.Strings.chars_ptr;
      Ret      : C.int;
      use type C.int;

   begin

      if not Dir.Exists (Path) then
         return System.ENOENT;
      end if;

      Path_Ptr := C.Strings.New_String (Path);
      Ret := StatVFS_C (Path_Ptr, System.StatVFS_Access(Buffer));
      C.Strings.Free (Path_Ptr);

      if Ret = 0 then
         return System.EXIT_SUCCESS;

      else
         raise Fuse.Fuse_Error with "StatVFS failed";

      end if;

   end StatFS;


   --------------------------
   --        Release       --
   --------------------------
   function Release
     (Fi     : access System.File_Info_Type)
      return System.Error_Type
   is begin

      if Fi.Fh = null or else not IO.Is_Open (Fi.Fh.all) then
         return System.ENOENT;
      end if;

      IO.Close (Fi.Fh.all);
      Delete_File_Type (Fi.Fh);

      return System.EXIT_SUCCESS;

   end Release;


   --------------------------
   --       Read Dir       --
   --------------------------
   function ReadDir
     (Path   : in String;
      Filler : access procedure
                 (Name     : String;
                  St_Buf   : System.Stat_Access;
                  Offset   : Natural))
      return System.Error_Type
   is
      use type Dir.File_Kind;

      procedure Fill_Dir (Ent : Dir.Directory_Entry_Type)
      is begin
         Filler.all (Dir.Simple_Name (Ent), null, 0);
         -- according to the Fuse docu we can just pass 0 as Offset every time
      end Fill_Dir;

      -- TODO check for directories with many files

   begin

      if not Dir.Exists (Path) or else Dir.Kind (Path) /= Dir.Directory then
         return System.ENOENT;

      else
         Dir.Search (Path, "", Process => Fill_Dir'Access);
         return System.EXIT_SUCCESS;

      end if;

      -- This doesn't show broken symlinks, but we havn't found a solution to
      -- this yet.
      --
      -- Also Dir.Search seems to die quite ugly when indexing directories with
      -- absolute symlinks in it.

   end ReadDir;

end Fuse.Aux;

-- vim: ts=3 sw=3 et
