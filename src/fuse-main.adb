with Ada.Command_Line;

package body Fuse.Main is

   --------------------------
   --  Get Attributes
   --------------------------
   package body GetAttr is

      function GetAttr_C
        (Path     : Interfaces.C.Strings.chars_ptr;
         St_Buf   : System.Stat_Access)
         return Interfaces.c.int
      is begin
         return General.Error_Code (GetAttr
            (Interfaces.C.Strings.Value (Path), St_Buf));
      exception when Error : others =>
            return General.Handle_Exception (Error);
      end GetAttr_C;

   begin

      Operations_C.GetAttr_C := GetAttr_C_Access;

   end GetAttr;


   --------------------------
   --  Read Link
   --------------------------
   package body ReadLink is

       function ReadLink_C
           (Path    : Interfaces.C.Strings.chars_ptr;
            Link    : Interfaces.C.Strings.chars_ptr;
            Size    : Interfaces.C.Size_T)
            return Interfaces.C.int
       is
          use type System.Error_Type;
          Link_String : String (1 .. Integer (Size) - 1);
          String_Size : Natural;
          Ret : System.Error_Type;

       begin

          Ret := (ReadLink
                    (Interfaces.C.Strings.Value(Path),
                     Link_String,
                     String_Size));

          Interfaces.C.Strings.Update
            (Item   => Link,
             Offset => 0,
             Chars  => Interfaces.C.To_C
                         (Link_String (1 .. String_Size),
                          Append_Nul => True),
             Check  => False); -- overwriting a buffer

          return General.Error_Code (Ret);

       exception when Error : others =>

             return General.Handle_Exception (Error);

       end ReadLink_C;

   begin

       Operations_C.ReadLink_C := ReadLink_C_Access;

   end ReadLink;


   --------------------------
   --  Make Node
   --------------------------
   package body MkNod is

      function MkNod_C
        (Path    : Interfaces.C.Strings.chars_ptr;
         Mode    : System.Mode_T;
         Dev     : System.Dev_T)
         return Interfaces.C.int

      is begin

         return General.Error_Code
           (MkNod
              (Interfaces.C.Strings.Value(Path),
               System.Mode_T_To_St_Mode (Mode),
               Dev));

      exception when Error : others =>

         return General.Handle_Exception (Error);

      end MkNod_C;

   begin

      Operations_C.MkNod_C := MkNod_C_Access;

   end MkNod;

   --------------------------
   --  Make Directory
   --------------------------
   package body MkDir is

       function MkDir_C
           (Path : Interfaces.C.Strings.chars_ptr;
            Mode : System.Mode_T)
            return Interfaces.C.int
       is begin
           return General.Error_Code (MkDir
           (Interfaces.C.Strings.Value(Path), System.Mode_T_To_St_Mode (Mode)));
       exception when Error : others =>
               return General.Handle_Exception (Error);
       end MkDir_C;

   begin

       Operations_C.MkDir_C := MkDir_C_Access;

   end MkDir;


   --------------------------
   --  Unlink
   --------------------------
   package body Unlink is

      function Unlink_C
         (Path    : Interfaces.C.Strings.chars_ptr)
         return Interfaces.C.int
      is begin
         return General.Error_Code (Unlink
            (Interfaces.C.Strings.Value (Path)));
      exception when Error : others =>
            return General.Handle_Exception (Error);
      end Unlink_C;

   begin

      Operations_C.Unlink_C := Unlink_C_Access;

   end Unlink;


   --------------------------
   --  Remove Dir
   --------------------------
   package body RmDir is

      function RmDir_C
        (Path: Interfaces.C.Strings.chars_ptr)
         return Interfaces.C.int
      is begin
         return General.Error_Code (RmDir
           (Interfaces.C.Strings.Value(Path)));
      exception when Error : others =>
         return General.Handle_Exception (Error);
      end RmDir_C;

   begin

      Operations_C.RmDir_C := RmDir_C_Access;

   end RmDir;


   --------------------------
   --  Symlink
   --------------------------
   package body SymLink is

       function SymLink_C
           (Path    : Interfaces.C.Strings.chars_ptr;
            Link    : Interfaces.C.Strings.chars_ptr)
            return Interfaces.C.int
       is begin
           return General.Error_Code (SymLink
           (Interfaces.C.Strings.Value(Path), Interfaces.C.Strings.Value(Link)));
       exception when Error : others =>
             return General.Handle_Exception (Error);
       end SymLink_C;

   begin

       Operations_C.SymLink_C := SymLink_C_Access;

   end SymLink;


   --------------------------
   --  Rename
   --------------------------
   package body Rename is

      function Rename_C
         (Path_Old   : Interfaces.C.Strings.chars_ptr;
          Path_New   : Interfaces.C.Strings.chars_ptr)
          return Interfaces.C.int
      is begin
         return General.Error_Code
            (Rename
              (Interfaces.C.Strings.Value(Path_Old),
               Interfaces.C.Strings.Value(Path_New)));
      exception when Error : others =>
            return General.Handle_Exception (Error);
      end Rename_C;

   begin

      Operations_C.Rename_C  := Rename_C_Access;

   end Rename;


   --------------------------
   --  Link
   --------------------------
   package body Link is

      function Link_C
        (Path    : Interfaces.C.Strings.chars_ptr;
         NewPath : Interfaces.C.Strings.chars_ptr)
         return Interfaces.C.int
      is begin

         return General.Error_Code
           (Link
             (Interfaces.C.Strings.Value(Path),
              Interfaces.C.Strings.Value(NewPath)));

      exception when Error : others =>

         return General.Handle_Exception (Error);

      end Link_C;

   begin

      Operations_C.Link_C := Link_C_Access;

   end Link;


   --------------------------
   --  Change Mode
   --------------------------
   package body ChMod is

       function ChMod_C
         (Path  : Interfaces.C.Strings.chars_ptr;
          Mode  : System.Mode_T)
          return Interfaces.C.int

       is begin

          return General.Error_Code
            (ChMod
               (Interfaces.C.Strings.Value(Path),
                System.Mode_T_To_St_Mode (Mode)));

       exception when Error : others =>

               return General.Handle_Exception (Error);

       end ChMod_C;

   begin

       Operations_C.Chmod_C := Chmod_C_Access;

   end ChMod;


   --------------------------
   --  Change Owner
   --------------------------
   package body ChOwn is

       function ChOwn_C
         (Path  : Interfaces.C.Strings.chars_ptr;
          UID   : System.UID_T;
          GID   : System.GID_T)
          return Interfaces.C.int

       is begin

          return General.Error_Code
            (ChOwn
               (Interfaces.C.Strings.Value(Path),
                UID,
                GID));

       exception when Error : others =>

               return General.Handle_Exception (Error);

       end ChOwn_C;

   begin

       Operations_C.ChOwn_C := ChOwn_C_Access;

   end ChOwn;

   --------------------------
   --  Truncate
   --------------------------
   package body Truncate is

      function Truncate_C
        (Path    : Interfaces.C.Strings.chars_ptr;
         Off_T   : System.Off_T)
         return Interfaces.C.int

      is begin

         return General.Error_Code (Truncate
            (Interfaces.C.Strings.Value(Path), Natural(Off_T)));

      exception when Error : others =>

         return General.Handle_Exception (Error);

      end Truncate_C;

   begin

      Operations_C.Truncate_C := Truncate_C_Access;

   end Truncate;


   --------------------------
   --  U Time
   --------------------------
   package body UTime is

       function UTime_C
         (Path  : Interfaces.C.Strings.chars_ptr;
          UBuf  : System.UTimeBuffer_Access)
          return Interfaces.C.int
       is begin
           return General.Error_Code (UTime
              (Interfaces.C.Strings.Value(Path), UBuf));
       exception when Error : others =>
               return General.Handle_Exception (Error);
       end UTime_C;

   begin

       Operations_C.UTime_C := UTime_C_Access;

   end UTime;


   --------------------------
   --  Open
   --------------------------
   package body Open is

      function Open_C
        (Path     : Interfaces.C.Strings.chars_ptr;
         Fi       : System.File_Info_Access)
         return Interfaces.C.int
      is begin
         return General.Error_Code (Open
            (Interfaces.C.Strings.Value (Path), Fi));
      exception when Error : others =>
            return General.Handle_Exception (Error);
      end Open_C;

   begin

      Operations_C.Open_C := Open_C_Access;

   end Open;


   --------------------------
   --  Read
   --------------------------
   package body Read is

      function Read_C
        (Path     : Interfaces.C.Strings.chars_ptr;
         Buf      : Buffer_Access;
         Size     : Interfaces.C.Size_T;
         Off      : System.Off_T;
         Fi       : System.File_Info_Access)
         return Interfaces.C.int
      is
         Ret       : System.Error_Type;
         Size_Temp : Natural := Natural (Size);
         use type System.Error_Type;
      begin
         Ret := Read
                  (Interfaces.C.Strings.Value (Path),
                   Buf,
                   Size_Temp,
                   Natural (Off),
                   Fi);
         if Ret = System.EXIT_SUCCESS then
            return Interfaces.C.int (Size_Temp);
         else
            return General.Error_Code (Ret);
         end if;
      exception when Error : others =>
            return General.Handle_Exception (Error);
      end Read_C;

   begin

      Operations_C.Read_C := Read_C_Access;

   end Read;


   --------------------------
   --  Write
   --------------------------
   package body Write is

      function Write_C
        (Path     : Interfaces.C.Strings.chars_ptr;
         Buf      : Buffer_Access;
         Size     : Interfaces.C.Size_T;
         Off      : System.Off_T;
         Fi       : System.File_Info_Access)
         return Interfaces.C.int
      is
         Ret       : System.Error_Type;
         Size_Temp : Natural := Natural (Size);
         use type System.Error_Type;
      begin
         Ret := Write
                  (Interfaces.C.Strings.Value (Path),
                   Buf,
                   Size_Temp,
                   Natural (Off),
                   Fi);
         if Ret = System.EXIT_SUCCESS then
            return Interfaces.C.int (Size_Temp);
         else
            return General.Error_Code (Ret);
         end if;
      exception when Error : others =>
            return General.Handle_Exception (Error);
      end Write_C;

   begin

      Operations_C.Write_C := Write_C_Access;

   end Write;


   --------------------------
   --  Stat FS
   --------------------------
   package body StatFS is

      function StatFS_C
        (Path   : Interfaces.C.Strings.chars_ptr;
         Statv  : System.StatVFS_Access)
         return Interfaces.C.int

      is begin

         return General.Error_Code (StatFS
            (Interfaces.C.Strings.Value (Path), Statv));

      exception when Error : others =>

         return General.Handle_Exception (Error);
end StatFS_C;

   begin

      Operations_C.StatFS_C := StatFS_C_Access;

   end StatFS;


   --------------------------
   --  Flush
   --------------------------
   package body Flush is

      function Flush_C
         (Path    : Interfaces.C.Strings.chars_ptr;
          Fi      : System.File_Info_Access)
          return Interfaces.C.int
      is begin
         return General.Error_Code (Flush
         (Interfaces.C.Strings.Value(Path), Fi));
      exception when Error : others =>
            return General.Handle_Exception (Error);
      end Flush_C;

   begin

      Operations_C.Flush_C := Flush_C_Access;

   end Flush;


   --------------------------
   --  Release
   --------------------------
   package body Release is

      function Release_C
         (Path    : Interfaces.C.Strings.chars_ptr;
          Fi      : System.File_Info_Access)
          return Interfaces.C.int
      is begin
         return General.Error_Code (Release
         (Interfaces.C.Strings.Value(Path), Fi));
      exception when Error : others =>
            return General.Handle_Exception (Error);
      end Release_C;

   begin

      Operations_C.Release_C := Release_C_Access;

   end Release;


   --------------------------
   --  F Sync
   --------------------------
   package body FSync is

       function FSync_C
           (Path     : Interfaces.C.Strings.chars_ptr;
            DataSync : Interfaces.C.int;
            Fi       : System.File_Info_Access)
            return Interfaces.C.int
       is begin
           return General.Error_Code (FSync
           (Interfaces.C.Strings.Value(Path), Natural (DataSync), Fi));
       exception when Error : others =>
               return General.Handle_Exception (Error);
       end FSync_C;

   begin

       Operations_C.FSync_C := FSync_C_Access;

   end FSync;


   --------------------------
   --  Set X Attributes
   --------------------------
   package body SetXAttr is

      function SetXAttr_C
        (Path    : Interfaces.C.Strings.chars_ptr;
         Name    : Interfaces.C.Strings.chars_ptr;
         Value   : Interfaces.C.Strings.chars_ptr;
         Size    : Interfaces.C.size_t;
         Flags   : Interfaces.C.int)
         return Interfaces.C.int

      is begin

         return General.Error_Code
           (SetXAttr
              (Interfaces.C.Strings.Value(Path),
               Interfaces.C.Strings.Value(Name),
               Interfaces.C.Strings.Value(Value, Size),
               Integer(Flags)));

      exception when Error : others =>

         return General.Handle_Exception (Error);

      end SetXAttr_C;

   begin

      Operations_C.SetXAttr_C := SetXAttr_C_Access;

   end SetXAttr;


   --------------------------
   --  Get X Attributes
   --------------------------
   package body GetXAttr is

      function GetXAttr_C
        (Path    : Interfaces.C.Strings.chars_ptr;
         Name    : Interfaces.C.Strings.chars_ptr;
         Value   : Interfaces.C.Strings.chars_ptr;
         Size    : Interfaces.C.size_t)
         return Interfaces.C.int
      is
         use type System.Error_Type;

         Ret       : System.Error_Type;
         Size_Ada  : Natural := Natural (Size);
         Value_Ada : String (1 .. Size_Ada);

      begin

         Ret := (GetXAttr
                   (Interfaces.C.Strings.Value(Path),
                    Interfaces.C.Strings.Value(Name),
                    Value_Ada,
                    Size_Ada));

         if Ret = System.EXIT_SUCCESS then

            Interfaces.C.Strings.Update
              (Item     => Value,
               Offset   => 0,
               Str      => Value_Ada,
               Check    => False); -- we overwrite Value and know the size

         end if;

         return General.Error_Code (Ret);

      exception when Error : others =>

         return General.Handle_Exception (Error);

      end GetXAttr_C;

   begin

       Operations_C.GetXAttr_C := GetXAttr_C_Access;

   end GetXAttr;


   --------------------------
   --  List X Attributes
   --------------------------
   package body ListXAttr is

      function ListXAttr_C
        (Path    : Interfaces.C.Strings.chars_ptr;
         List    : Interfaces.C.Strings.chars_ptr;
         Size    : Interfaces.C.size_t)
         return Interfaces.C.int
      is
         use type System.Error_Type;
         use type Interfaces.C.size_t;

         Ret      : System.Error_Type;
         List_Ada : Attributes_Type;
         Offset   : Interfaces.C.size_t := 0;

         procedure Update_List (Position : Fuse.String_Vectors.Cursor)
         is
            package Vec renames Fuse.String_Vectors;

         begin

            if Vec.Element(Position)'Length + Offset < Size then
               Interfaces.C.Strings.Update
                 (Item    => List,
                  Offset  => Offset,
                  Chars   => Interfaces.C.To_C (Vec.Element(Position)),
                  Check   => False);

               Offset := Offset + Vec.Element(Position)'Length + 1;

            else
               Offset := 0;
               return;

            end if;

         end Update_List;

      begin

         Ret := (ListXAttr
                   (Interfaces.C.Strings.Value (Path),
                    List_Ada));

         if Ret /= System.EXIT_SUCCESS then
            return General.Error_Code (Ret);
         end if;

         Fuse.String_Vectors.Iterate (List_Ada, Update_List'Access);

         return Interfaces.C.int (Offset);

      exception when Error : others =>

         return General.Handle_Exception (Error);

      end ListXAttr_C;

   begin

      Operations_C.ListXAttr_C := ListXAttr_C_Access;

   end ListXAttr;


   --------------------------
   --  Remove X Attributes
   --------------------------
   package body RemoveXAttr is

       function RemoveXAttr_C
           (Path    : Interfaces.C.Strings.chars_ptr;
            Name    : Interfaces.C.Strings.chars_ptr)
            return Interfaces.C.int
       is begin
           return General.Error_Code (RemoveXAttr
           (Interfaces.C.Strings.Value(Path),
            Interfaces.C.Strings.Value(Name)));
       exception when Error : others =>
               return General.Handle_Exception (Error);
       end RemoveXAttr_C;

   begin

       Operations_C.RemoveXAttr_C := RemoveXAttr_C_Access;

   end RemoveXAttr;


   --------------------------
   --  Open Directory
   --------------------------
   package body OpenDir is

       function OpenDir_C
           (Path    : Interfaces.C.Strings.chars_ptr;
            Fi      : System.File_Info_Access)
            return Interfaces.C.int
       is begin
           return General.Error_Code (OpenDir
           (Interfaces.C.Strings.Value(Path),
           Fi));
       exception when Error : others =>
               return General.Handle_Exception (Error);
       end OpenDir_C;

   begin

       Operations_C.OpenDir_C := OpenDir_C_Access;

   end OpenDir;


   --------------------------
   --  Read Directory
   --------------------------
   package body ReadDir is

      function ReadDir_C
        (Path     : Interfaces.C.Strings.chars_ptr;
         Buf      : General.Dir_Buffer_Type;
         Filler   : Operations.Fill_Dir_T;
         Off      : System.Off_T;
         Fi       : System.File_Info_Access)
         return Interfaces.C.int
      is

         procedure Fill_Dir is new General.Fill_Dir (Filler.all, Buf);

      begin
         return General.Error_Code (ReadDir
           (Interfaces.C.Strings.Value (Path),
            Fill_Dir'Access,
            Natural (Off),
            Fi));
      exception when Error : others =>
            return General.Handle_Exception (Error);
      end ReadDir_C;


   begin

      Operations_C.ReadDir_C := ReadDir_C_Access;

   end ReadDir;


   --------------------------
   --  Release Directory
   --------------------------
   package body ReleaseDir is

       function ReleaseDir_C
           (Path    : Interfaces.C.Strings.chars_ptr;
            Fi      : System.File_Info_Access)
            return Interfaces.C.int
       is begin
           return General.Error_Code (ReleaseDir
           (Interfaces.C.Strings.Value(Path),
           Fi));
       exception when Error : others =>
               return General.Handle_Exception (Error);
       end ReleaseDir_C;

   begin

       Operations_C.ReleaseDir_C := ReleaseDir_C_Access;

   end ReleaseDir;


   --------------------------
   --  F Sync Directory
   --------------------------
   package body FSyncDir is

       function FSyncDir_C
           (Path     : Interfaces.C.Strings.chars_ptr;
            Datasync : Interfaces.C.int;
            Fi       : System.File_Info_Access)
            return Interfaces.C.int
       is begin
           return General.Error_Code (FSyncDir
           (Interfaces.C.Strings.Value(Path),
            Natural(Datasync),
            Fi));
       exception when Error : others =>
               return General.Handle_Exception (Error);
       end FSyncDir_C;

   begin

       Operations_C.FSyncDir_C := FSyncDir_C_Access;

   end FSyncDir;


   --------------------------
   --  Init
   --------------------------
   package body Init is

       function Init_C
           (Conn : System.Fuse_Conn_Info_Access)
           return Interfaces.C.int
       is begin
           return General.Error_Code (Init(Conn));
       exception when Error : others =>
               return General.Handle_Exception (Error);
       end Init_C;

   begin

       Operations_C.Init_C := Init_C_Access;

   end Init;


   --------------------------
   --  Destroy
   --------------------------
   package body Destroy is

       function Destroy_C
           (Userdata : User_Data_Access)
           return Interfaces.C.int
       is begin
           return General.Error_Code (Destroy(Userdata));
       exception when Error : others =>
               return General.Handle_Exception (Error);
       end Destroy_C;

   begin

       Operations_C.Destroy_C := Destroy_C_Access;

   end Destroy;


   --------------------------
   --  Access
   --------------------------
   package body Access_Ada is

       function Access_C
           (Path    : Interfaces.C.Strings.chars_ptr;
            Mask    : Interfaces.C.int)
            return Interfaces.C.int

       is begin
           return General.Error_Code (Access_Ada
           (Interfaces.C.Strings.Value(Path),
            Natural(Mask)));
       exception when Error : others =>
               return General.Handle_Exception (Error);
       end Access_C;

   begin

       Operations_C.Access_C := Access_C_Access;

   end Access_Ada;

   --------------------------
   --  Create
   --------------------------
   package body Create is

      function Create_C
        (Path    : Interfaces.C.Strings.chars_ptr;
         Mode    : System.Mode_T;
         Fi      : System.File_Info_Access)
         return Interfaces.C.int

      is begin

         return General.Error_Code (Create
           (Interfaces.C.Strings.Value (Path),
            System.Mode_T_To_St_Mode (Mode),
            Fi));

      exception when Error : others =>

         return General.Handle_Exception (Error);

      end Create_C;

   begin

      Operations_C.Create_C := Create_C_Access;

   end Create;


   --------------------------
   --  F Truncate
   --------------------------
   package body FTruncate is

       function FTruncate_C
          (Path    : Interfaces.C.Strings.chars_ptr;
           Off_T   : System.Off_T;
           Fi      : System.File_Info_Access)
           return Interfaces.C.int
       is begin
           return General.Error_Code (FTruncate
           (Interfaces.C.Strings.Value(Path), Natural(Off_T), Fi));
      exception when Error : others =>
              return General.Handle_Exception (Error);
      end FTruncate_C;

   begin

       Operations_C.FTruncate_C := FTruncate_C_Access;

   end FTruncate;


   --------------------------
   --  F Get Attributes
   --------------------------
   package body FGetAttr is

      function FGetAttr_C
        (Path     : Interfaces.C.Strings.chars_ptr;
         St_Buf   : System.Stat_Access;
         Fi       : System.File_Info_Access)
         return Interfaces.c.int
      is begin
         return General.Error_Code (FGetAttr
            (Interfaces.C.Strings.Value (Path), St_Buf, Fi));
      exception when Error : others =>
            return General.Handle_Exception (Error);
      end FGetAttr_C;

   begin

      Operations_C.FGetAttr_C := FGetAttr_C_Access;

   end FGetAttr;


   --------------------------
   --  Lock
   --------------------------

   --------------------------
   --  U Time NS
   --------------------------

   --------------------------
   --  B Map
   --------------------------

   --------------------------
   --  IO Control
   --------------------------

   --------------------------
   --  Poll
   --------------------------

---------------------------------------------------------------------


   -------------------------------
   ------------ Main -------------
   -------------------------------
   procedure Main
     (Arguments : Fuse.Arguments_Type := Fuse.Get_Arguments;
      User_Data : User_Data_Type)
   is

      Arguments_Length : constant Positive
         := Positive (Fuse.String_Vectors.Length (Arguments));

      type Argv_Type is array (0 .. Arguments_Length)
         of Interfaces.C.Strings.chars_ptr;
      type Argv_Access is access Argv_Type;

      function Fuse_Main_C
        (argc      : Interfaces.C.int;
         argv      : Argv_Access;
         op        : Sys_Operations.Operations_C_Access;
         op_size   : Interfaces.C.int;
         user_data : User_Data_Access)
         return      Interfaces.C.int;

      pragma Import (C, Fuse_Main_C, "fuse_main_real");

      Argv : Argv_Access := new Argv_Type;
      Data : constant User_Data_Access := new User_Data_Type'(User_Data);

      use type Interfaces.C.int;

   begin

      Argv(0) := Interfaces.C.Strings.New_String
         (Ada.Command_Line.Command_Name);
      for I in 1 .. Arguments_Length loop
         Argv(I) := Interfaces.C.Strings.New_String (Arguments.Element(I));
      end loop;

      if Fuse_Main_C
        (argc     => Interfaces.C.int (Arguments_Length + 1),
         argv     => Argv,
         op       => Operations_C'Access,
         op_size  => Sys_Operations.Operations_C_Record'Size,
         user_data=> Data) /= 0
      then
         raise Fuse.Fuse_Error;
      end if;

   end Main;

begin

   Operations_C.Flag_Nullpath_Ok := Nullpath_Ok;

end Fuse.Main;

-- vim: ts=3 sw=3 et
