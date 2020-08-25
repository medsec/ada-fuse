with Ada.Directories;

with Rot_13;

package body RotFS is

   function Get_Real_Path (Path : String) return String
   is begin
      -- we have to use this callback function, as we call this from inside Fuse
      return General.Get_User_Data.Root_Dir.all & Path;
   end Get_Real_Path;


   --------------------------
   --    Get Attributes    --
   --------------------------
   function GetAttr
     (Path   : in String;
      St_Buf : access System.Stat_Type)
      return System.Error_Type
   is
      Real_Path : constant String := Get_Real_Path (Path);

   begin

      --return Aux.GetAttr (Real_Path, St_Buf);
      return Aux.Simple_GetAttr (Real_Path, St_Buf);

   end GetAttr;


   -------------------------
   --      Read Link      --
   -------------------------
   function ReadLink
     (Path    : in String;
      Link    : out String;
      Size    : out Natural)
      return System.Error_Type
   is
      Real_Path : constant String := Get_Real_Path (Path);

   begin

      return Aux.ReadLink (Real_Path, Link, Size);

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
      Real_Path : constant String := Get_Real_Path (Path);

   begin

      return Aux.Mknod (Real_Path, Mode, Dev);

   end MkNod;


   --------------------------
   --         MkDir        --
   --------------------------
   function MkDir
     (Path   : in String;
      Mode   : in System.St_Mode_Type)
      return System.Error_Type
   is
      pragma Unreferenced (Mode);
      Real_Path : constant String := Get_Real_Path (Path);

   begin

      return Aux.MkDir (Real_Path);

   end MkDir;


   --------------------------
   --         Unlink       --
   --------------------------
   function Unlink
     (Path   : in String)
      return System.Error_Type
   is
      Real_Path : constant String := Get_Real_Path (Path);

   begin

      return Aux.Unlink (Real_Path);

   end Unlink;

   --------------------------
   --          RmDir       --
   --------------------------
   function RmDir
     (Path   : in String)
      return System.Error_Type
   is
      Real_Path : constant String := Get_Real_Path (Path);

   begin

      return Aux.RmDir (Real_Path);

   end RmDir;


   --------------------------
   --       Symlink        --
   --------------------------
   function SymLink
     (Path    : in String;
      Link    : in String)
      return System.Error_Type
   is
      Real_Link : constant String := Get_Real_Path (Link);

   begin

      return Aux.SymLink (Path, Real_Link);

   end SymLink;


   --------------------------
   --         Rename       --
   --------------------------
   function Rename
     (Path_Old : in String;
      Path_New : in String)
      return System.Error_Type
   is
      Real_Path_Old : constant String := Get_Real_Path (Path_Old);
      Real_Path_New : constant String := Get_Real_Path (Path_New);

   begin

      return Aux.Rename (Real_Path_Old, Real_Path_New);

   end Rename;


   --------------------------
   --         Link         --
   --------------------------
   function Link
     (Path     : in String;
      New_Path : in String)
      return System.Error_Type
   is
      Real_Path     : constant String := Get_Real_Path (Path);
      Real_New_Path : constant String := Get_Real_Path (New_Path);

   begin

      return Aux.Link (Real_Path, Real_New_Path);

   end Link;

   --------------------------
   --         ChMod        --
   --------------------------
   function ChMod
     (Path    : in String;
      Mode    : in System.St_Mode_Type)
      return System.Error_Type
   is
      Real_Path : constant String := Get_Real_Path (Path);

   begin

      return Aux.ChMod (Real_Path, Mode);

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
      Real_Path : constant String := Get_Real_Path (Path);

   begin

      return Aux.ChOwn (Real_Path, UID, GID);

   end ChOwn;

   --------------------------
   --        Truncate      --
   --------------------------
   function Truncate
     (Path   : in String;
      Size   : in Natural)
      return System.Error_Type
   is
      Real_Path : constant String := Get_Real_Path (Path);

   begin

      return Aux.Truncate (Real_Path, Size);

   end Truncate;


   --------------------------
   --        UTime         --
   --------------------------
   function UTime
     (Path   : in String;
      Buffer : access System.UTimeBuffer_Type)
      return System.Error_Type
   is
      Real_Path : constant String := Get_Real_Path (Path);

   begin

      return Aux.UTime (Real_Path, Buffer);

   end UTime;


   --------------------------
   --        Create        --
   --------------------------
   function Create
     (Path   : in String;
      Mode   : in System.St_Mode_Type;
      Fi     : access System.File_Info_Type)
      return System.Error_Type
   is
      pragma Unreferenced (Mode);
      Real_Path : constant String := Get_Real_Path (Path);

   begin

      return Aux.Create (Real_Path, Fi);

   end Create;

   --------------------------
   --         Open         --
   --------------------------
   function Open
     (Path   : in String;
      Fi     : access System.File_Info_Type)
      return System.Error_Type
   is
      Real_Path : constant String := Get_Real_Path (Path);

   begin

      return Aux.Open (Real_Path, Fi);

   end Open;


   --------------------------
   --        Release       --
   --------------------------
   function Release
     (Path   : in String;
      Fi     : access System.File_Info_Type)
      return System.Error_Type
   is
      pragma Unreferenced (Path);

   begin

      return Aux.Release (Fi);

   end Release;


   --------------------------
   --          Read        --
   --------------------------
   function Read
     (Path   : in String;
      Buffer : access Buffer_Type;
      Size   : in out Natural;
      Offset : in Natural;
      Fi     : access System.File_Info_Type)
      return System.Error_Type
   is
      use type Ada.Directories.File_Kind;
      Real_Path : constant String := Get_Real_Path (Path);
      Length : constant Natural := Natural (Ada.Directories.Size (Real_Path));

   begin

      if Offset >= Length then
         Size := 0;

      elsif Offset + Size > Length then
         Size := Length - Offset;

      end if;

      Buffer(1 .. Size) :=
         Rot_13.Encrypt (Aux.Read_File (Fi.Fh.all, Offset, Size));

      return System.EXIT_SUCCESS;

   end Read;


   --------------------------
   --         Write        --
   --------------------------
   function Write
     (Path   : in String;
      Buffer : access Buffer_Type;
      Size   : in out Natural;
      Offset : in Natural;
      Fi     : access System.File_Info_Type)
      return System.Error_Type
   is
      pragma Unreferenced (Path);
      pragma Unmodified (Size);
      use type Ada.Directories.File_Kind;
      Data : constant String (Offset + 1 .. Offset + Size)
                        := Rot_13.Encrypt (Buffer (1 .. Size));

   begin

      Aux.Write_File (Fi.Fh.all, Data);

      return System.EXIT_SUCCESS;

   end Write;


   --------------------------
   --       Read Dir       --
   --------------------------
   function ReadDir
     (Path   : in String;
      Filler : access procedure
                 (Name     : String;
                  St_Buf   : System.Stat_Access;
                  Offset   : Natural);
      Offset : in Natural;
      Fi     : access System.File_Info_Type)
      return System.Error_Type
   is
      pragma Unreferenced (Offset, Fi);
      Real_Path : constant String := Get_Real_Path (Path);

   begin

      return Aux.ReadDir (Real_Path, Filler);

   end ReadDir;


   --------------------------
   --        StatFS        --
   --------------------------
   function StatFS
     (Path   : in String;
      Buffer : access System.StatVFS_Type)
      return System.Error_Type
   is
      Real_Path : constant String := Get_Real_Path (Path);

   begin

      return Aux.StatFS (Real_Path, Buffer);

   end StatFS;

end RotFS;

-- vim: ts=3 sw=3 et
