-- Architecture independent functions and types
--
-- Use Put_Message to write custom messages to the log file and the
-- output when executing with '-o debug'.

with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Exceptions;
with Ada.Text_IO;

with Fuse.System;

generic

   with package System is new Fuse.System (<>);
   Log_Level : Fuse.Log_Level_Type;
   Log_File_Name : String;

package Fuse.General is

   -- Log, Errors
   procedure Put_Message
     (Message : String;
      Level   : Fuse.Log_Level_Type := 3);

   function Error_Code
     (Error   : System.Error_Type) return Interfaces.C.Int;

   function Handle_Exception
     (Exc     : Ada.Exceptions.Exception_Occurrence) return Interfaces.C.Int;

   -- Context
   function fuse_get_context return System.Context_Access;
   pragma import (C, fuse_get_context);
   function Get_Context return System.Context_Type;
   function Get_User_Data return System.User_Data_Type;

   -- Fill Dir
   type Dir_Buffer_Type is private;

   type Fill_Dir_Access is access procedure
     (Buffer   : Dir_Buffer_Type;
      Name     : String;
      St_Buf   : System.Stat_Access;
      Offset   : Natural);

   generic
      with function Fill_Dir_C
        (Buf      : General.Dir_Buffer_Type;
         Name     : Interfaces.C.Strings.chars_ptr;
         Stbuf    : System.Stat_Access;
         Off      : System.Off_T)
         return Interfaces.C.int;
      Buffer : Dir_Buffer_Type;
   procedure Fill_Dir
     (Name     : String;
      St_Buf   : System.Stat_Access;
      Offset   : Natural);

private

   -- Log
   Log_File : Ada.Text_IO.File_Type;

   procedure Write_Log
     (Message : String);

   procedure Open_Log_File;

   -- Fill Dir
   type Void is null record;
   pragma Convention (C, Void);

   type Void_Ptr is access all Void;
   pragma Convention (C, Void_Ptr);

   type Dir_Buffer_Type is new Void_Ptr;

end Fuse.General;

-- vim: ts=3 sw=3 et
