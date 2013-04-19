with Ada.Calendar;
with Ada.Directories;
with GNAT.Calendar.Time_IO;

package body Fuse.General is

   -- Context

   function Get_Context return System.Context_Type
   is begin
      return fuse_get_context.all;
   end Get_context;


   function Get_User_Data return System.User_Data_Type
   is begin
      return fuse_get_context.all.Data.all;
   end Get_User_Data;


   -- Log, Errors

   procedure Put_Message
     (Message : String;
      Level   : Fuse.Log_Level_Type := 3)
   is
      Msg : constant String := "MESSAGE: " & Message;
   begin
      if Log_Level >= Level then
         Ada.Text_IO.Put_Line (Msg);
         Write_Log (Msg);
      end if;
   end Put_Message;


   function Error_Code
     (Error   : System.Error_Type) return Interfaces.C.Int
   is
      use type Interfaces.C.Int;
   begin
      return -System.Error_Type'Enum_Rep (Error);
   end Error_Code;


   function Handle_Exception
     (Exc     : Ada.Exceptions.Exception_Occurrence) return Interfaces.C.Int
   is
      Msg : constant String := "Exception occurred: "
         & Ada.Exceptions.Exception_Information (Exc);
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Msg);
      if Log_level >= 1 then
         Write_Log (Msg);
      end if;
      return Error_Code (System.EIO);
   end Handle_Exception;


   procedure Write_Log
     (Message : String)
   is begin
      Ada.Text_IO.Put_Line (Log_File,
         GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock, "%b %d %T ")
         & Message );
      Ada.Text_IO.Flush (Log_File);
   end Write_Log;


   procedure Open_Log_File
   is begin
      if Ada.Directories.Exists (Log_File_Name) then
         Ada.Text_IO.Open (Log_File, Ada.Text_IO.Append_File, Log_File_Name);
      else
         Ada.Text_IO.Create (Log_File, Name => Log_File_Name);
      end if;
   end Open_Log_File;


   -- Fill Dir
   procedure Fill_Dir
     (Name     : String;
      St_Buf   : System.Stat_Access;
      Offset   : Natural)
   is
      use type Interfaces.C.int;
      Fill_Dir_Error : exception;
      Name_Ptr : Interfaces.C.Strings.chars_ptr :=
         Interfaces.C.Strings.New_String (Name);
      Ret      : Interfaces.C.int;
   begin
      Ret := Fill_Dir_C
        (Buf    => Buffer,
         Name   => Name_Ptr,
         Stbuf  => St_Buf,
         Off    => System.Off_T (Offset));
      Interfaces.C.Strings.Free (Name_Ptr);
      if Ret /= 0 then
         raise Fill_Dir_Error;
      end if;
   end Fill_Dir;

begin

   if Log_Level > 0 then
      Open_Log_File;
   end if;

end Fuse.General;

-- vim: ts=3 sw=3 et
