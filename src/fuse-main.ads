-- First instantiate this package. Then start implementing the functions
-- for the generic nested packages. Instantiate those in order to register
-- your functions. Call the Main function to mount the filesystem

with Ada.Direct_IO;
with Ada.Environment_Variables;
with Interfaces.C;
with Interfaces.C.Strings;

with Fuse.System;
with Fuse.General;
with Fuse.Operations;

generic
   -- Element Type for File IO, e.g. Character
   type Element_Type is private;

   -- Array Type, e.g. String
   type Element_Array is array (Positive range <>) of Element_Type;

   -- see Main procedure
   type User_Data_Type is private;

   -- Exception and message logging; Log level of Messages can be specified.
   -- 0: no logging at all, 1: only exceptions, 3: standard messages,
   -- 5: default, 9: excessive logging
   Log_Level : Fuse.Log_Level_Type := 5;

   -- Log File will be created if not present.
   Log_File_Name : String
      := Ada.Environment_Variables.Value ("HOME") & "/fuse.log";

   -- see Fuse docu: flag_nullpath_ok in fuse_operations
   Nullpath_Ok : Boolean := True;

package Fuse.Main is

   package IO is
      new Ada.Direct_IO
            (Element_Type);

   type File_Access is access IO.File_Type;
   subtype Buffer_Type is Element_Array (Positive);
   type Buffer_Access is access Buffer_Type;
   type User_Data_Access is access User_Data_Type;


   package System is
      new Fuse.System
            (User_Data_Type,
             IO.File_Type,
             File_Access);

   package General is
      new Fuse.General
            (System,
             Log_Level,
             Log_File_Name);

   package Operations is
      new Fuse.Operations
            (System  => System,
             General => General,
             Buffer_Access => Buffer_Access,
             User_Data_Access => User_Data_Access);


   -- Fire up Fuse.
   -- This is not threadsave yet, so call with '-s' (singlethreaded).
   -- To see some output use '-o debug'.
   --
   -- Arguments is used to pass command line arguments to Fuse.
   -- See Fuse parent package.
   --
   -- Store to User_Data whatever you want. You can access it through
   -- General.Get_User_Data. This is the only way to pass your general
   -- data to fuse functions.
   procedure Main
     (Arguments : Fuse.Arguments_Type := Fuse.Get_Arguments;
      User_Data : User_Data_Type);

---------------------------------------------------------------------

   --------------------------
   --  Get Attributes
   --------------------------
   generic

   -- Equivalent to stat / lstat
   -- takes files or directories
      with function GetAttr
        (Path      :     in String;
         St_Buf    : access System.Stat_Type)
         return System.Error_Type is <>;

   package GetAttr is private

      function GetAttr_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         St_Buf    : System.Stat_Access)
         return Interfaces.C.int;

      GetAttr_C_Access : Operations.GetAttr_C_Type := GetAttr_C'Access;

   end GetAttr;


   --------------------------
   --  Read Link
   --------------------------
   generic

   -- Link (1 .. Size) gives the destination of the symlink Path.
      with function ReadLink
        (Path      :     in String;
         Link      :    out String;
         Size      :    out Natural)
         return System.Error_Type is <>;

   package ReadLink is private

      function ReadLink_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         Link      : Interfaces.C.Strings.chars_ptr;
         Size      : Interfaces.C.Size_T)
         return Interfaces.C.int;

      ReadLink_C_Access : Operations.ReadLink_C_Type := ReadLink_C'Access;

   end ReadLink;


   --------------------------
   --  Make Node
   --------------------------
   generic

   -- implement either this or Create
      with function MkNod
        (Path      :     in String;
         Mode      :     in System.St_Mode_Type;
         Dev       :     in System.Dev_T)
         return System.Error_Type is <>;

   package MkNod is private

      function MkNod_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         Mode      : System.Mode_T;
         Dev       : System.Dev_T)
         return Interfaces.C.int;

      MkNod_C_Access : Operations.MkNod_C_Type := MkNod_C'Access;

   end MkNod;


   --------------------------
   --  Make Directory
   --------------------------
   generic

      with function MkDir
        (Path      :     in String;
         Mode      :     in System.St_Mode_Type)
         return System.Error_Type is <>;

   package MkDir is private

      function MkDir_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         Mode      : System.Mode_T)
         return Interfaces.C.int;

      MkDir_C_Access : Operations.MkDir_C_Type := MkDir_C'Access;

   end MkDir;


   --------------------------
   --  Unlink
   --------------------------
   generic

   -- delete files
      with function Unlink
        (Path      :     in String)
         return System.Error_Type is <>;

   package Unlink is private

      function Unlink_C
        (Path      : Interfaces.C.Strings.chars_ptr)
         return Interfaces.C.int;

      Unlink_C_Access : Operations.Unlink_C_Type := Unlink_C'Access;

   end Unlink;


   --------------------------
   --  Remove Directory
   --------------------------
   generic

      with function RmDir
        (Path      :     in String)
         return System.Error_Type is <>;

   package RmDir is private

      function RmDir_C
        (Path      : Interfaces.C.Strings.chars_ptr)
         return Interfaces.C.int;

      RmDir_C_Access : Operations.RmDir_C_Type := RmDir_C'Access;

   end RmDir;


   --------------------------
   --  Symlink
   --------------------------
   generic

      with function SymLink
        (Path      :     in String;
         Link      :     in String)
         return System.Error_Type is <>;

   package SymLink is private

      function SymLink_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         Link      : Interfaces.C.Strings.chars_ptr)
         return Interfaces.C.int;

      SymLink_C_Access : Operations.SymLink_C_Type := SymLink_C'Access;

   end SymLink;


   --------------------------
   --  Rename
   --------------------------
   generic

   -- rename files and directories
      with function Rename
        (Path_Old  :     in String;
         Path_New  :     in String)
         return System.Error_Type is <>;

   package Rename is private

      function Rename_C
        (Path_Old  : Interfaces.C.Strings.chars_ptr;
         Path_New  : Interfaces.C.Strings.chars_ptr)
         return Interfaces.C.int;

      Rename_C_Access : Operations.Rename_C_Type := Rename_C'Access;

   end Rename;


   --------------------------
   --  Hard Link
   --------------------------
   generic

      with function Link
        (Path      :     in String;
         New_Path  :     in String)
         return System.Error_Type is <>;

   package Link is private

      function Link_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         NewPath   : Interfaces.C.Strings.chars_ptr)
         return Interfaces.C.int;

      Link_C_Access : Operations.Link_C_Type := Link_C'Access;

   end Link;


   --------------------------
   --  Change Mode
   --------------------------
   generic

      with function ChMod
        (Path      :     in String;
         Mode      :     in System.St_Mode_Type)
         return System.Error_Type is <>;

   package ChMod is private

      function Chmod_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         Mode      : System.Mode_T)
         return Interfaces.C.int;

      ChMod_C_Access : Operations.ChMod_C_Type := ChMod_C'Access;

   end ChMod;


   --------------------------
   --  Change Owner
   --------------------------
   generic

      with function ChOwn
        (Path      :     in String;
         UID       :     in System.UID_T;
         GID       :     in System.GID_T)
         return System.Error_Type is <>;

   package ChOwn is private

       function ChOwn_C
         (Path     : Interfaces.C.Strings.chars_ptr;
          UID      : System.UID_T;
          GID      : System.GID_T)
          return Interfaces.C.int;

       ChOwn_C_Access : Operations.ChOwn_C_Type := ChOwn_C'Access;

    end ChOwn;


   --------------------------
   --  Truncate
   --------------------------
   generic

      with function Truncate
        (Path      :     in String;
         Size      :     in Natural)
         return System.Error_Type is <>;

   package Truncate is private

      function Truncate_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         Off_T     : System.Off_T)
         return Interfaces.C.int;

      Truncate_C_Access : Operations.Truncate_C_Type := Truncate_C'Access;

   end Truncate;


   --------------------------
   --  U Time
   --------------------------
   generic

      with function UTime
        (Path      :     in String;
         Buffer    : access System.UTimeBuffer_Type)
         return System.Error_Type is <>;

   package UTime is private

      function UTime_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         UBuf      : System.UTimeBuffer_Access)
         return Interfaces.C.int;

      UTime_C_Access : Operations.UTime_C_Type := UTime_C'Access;

   end UTime;


   --------------------------
   --  Open
   --------------------------
   generic

   -- store an own file handler in Fi.Fh if needed
      with function Open
        (Path      :     in String;
         Fi        : access System.File_Info_Type)
         return System.Error_Type is <>;

   package Open is private

      function Open_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         Fi        : System.File_Info_Access)
         return Interfaces.C.int;

      Open_C_Access : Operations.Open_C_Type := Open_C'Access;

   end Open;


   --------------------------
   --  Read
   --------------------------
   generic

   -- if less than 'Size' bytes are read, modify Size accordingly
      with function Read
        (Path      :     in String;
         Buffer    : access Buffer_Type; -- use as out parameter
         Size      : in out Natural;
         Offset    :     in Natural;
         Fi        : access System.File_Info_Type)
         return System.Error_Type is <>;

   package Read is private

      function Read_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         Buf       : Buffer_Access;
         Size      : Interfaces.C.Size_T;
         Off       : System.Off_T;
         Fi        : System.File_Info_Access)
         return Interfaces.C.int;

      Read_C_Access : Operations.Read_C_Type := Read_C'Access;

   end Read;


   --------------------------
   --  Write
   --------------------------
   generic

      with function Write
        (Path      :     in String;
         Buffer    : access Buffer_Type; -- use as in parameter
         Size      : in out Natural;
         Offset    :     in Natural;
         Fi        : access System.File_Info_Type)
         return System.Error_Type is <>;

   package Write is private

      function Write_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         Buf       : Buffer_Access;
         Size      : Interfaces.C.Size_T;
         Off       : System.Off_T;
         Fi        : System.File_Info_Access)
         return Interfaces.C.int;

      Write_C_Access : Operations.Write_C_Type := Write_C'Access;

   end Write;


   --------------------------
   --  Stat FS
   --------------------------
   generic

   -- gives information about the filesystem
   -- use statvfs
      with function StatFS
        (Path      :     in String;
         Buffer    : access System.StatVFS_Type)
         return System.Error_Type is <>;


   package StatFS is private

      function StatFS_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         Statv     : System.StatVFS_Access)
         return Interfaces.C.int;

      StatFS_C_Access : Operations.StatFS_C_Type := StatFS_C'Access;

   end StatFS;


   --------------------------
   --  Flush
   --------------------------
   generic

      with function Flush
        (Path      :     in String;
         Fi        : access System.File_Info_Type)
         return System.Error_Type is <>;

   package Flush is private

      function Flush_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         Fi        : System.File_Info_Access)
         return Interfaces.C.int;

      Flush_C_Access : Operations.Flush_C_Type := Flush_C'Access;

   end Flush;


   --------------------------
   --  Release
   --------------------------
   generic

      with function Release
        (Path      :     in String;
         Fi        : access System.File_Info_Type)
         return System.Error_Type is <>;

   package Release is private

      function Release_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         Fi        : System.File_Info_Access)
         return Interfaces.C.int;

      Release_C_Access : Operations.Release_C_Type := Release_C'Access;

   end Release;


   --------------------------
   --  F Sync
   --------------------------
   generic

   -- UNTESTED
      with function FSync
        (Path      :     in String;
         DataSync  :     in Natural;
         Fi        : access System.File_Info_Type)
         return System.Error_Type is <>;

   package FSync is private

      function FSync_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         Datasync  : Interfaces.C.int;
         Fi        : System.File_Info_Access)
         return Interfaces.C.int;

      FSync_C_Access : Operations.FSync_C_Type := FSync_C'Access;

   end FSync;


   --------------------------
   --  Set X Attributes
   --------------------------
   generic

   -- UNTESTED
      with function SetXAttr
        (Path      :     in String;
         Name      :     in String;
         Value     :     in String;
         Flags     :     in Integer := 0)
         return System.Error_Type is <>;

   package SetXAttr is private

      function SetXAttr_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         Name      : Interfaces.C.Strings.chars_ptr;
         Value     : Interfaces.C.Strings.chars_ptr;
         Size      : Interfaces.C.size_t;
         Flags     : Interfaces.C.int)
         return Interfaces.C.int;

      SetXAttr_C_Access : Operations.SetXAttr_C_Type := SetXAttr_C'Access;

   end SetXAttr;


   --------------------------
   --  Get X Attributes
   --------------------------
   generic

   -- UNTESTED
      with function GetXAttr
        (Path      :     in String;
         Name      :     in String;
         Value     :    out String;
         Size      : in out Natural)
         return System.Error_Type is <>;

   package GetXAttr is private

      function GetXAttr_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         Name      : Interfaces.C.Strings.chars_ptr;
         Value     : Interfaces.C.Strings.chars_ptr;
         Size      : Interfaces.C.size_t)
         return Interfaces.C.int;

      GetXAttr_C_Access : Operations.GetXAttr_C_Type := GetXAttr_C'Access;

   end GetXAttr;


   --------------------------
   --  List X Attributes
   --------------------------
   subtype Attributes_Type is Fuse.String_Vectors.Vector;
   generic

   -- UNTESTED
      with function ListXAttr
        (Path      :     in String;
         List      :    out Attributes_Type)
         return System.Error_Type is <>;

   package ListXAttr is private

      function ListXAttr_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         List      : Interfaces.C.Strings.chars_ptr;
         Size      : Interfaces.C.size_t)
         return Interfaces.C.int;

      ListXAttr_C_Access : Operations.ListXAttr_C_Type := ListXAttr_C'Access;

   end ListXAttr;


   --------------------------
   --  Remove X Attributes
   --------------------------
   generic

   -- UNTESTED
      with function RemoveXAttr
        (Path      :     in String;
         Name      :     in String)
         return System.Error_Type is <>;

   package RemoveXAttr is private

      function RemoveXAttr_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         Name      : Interfaces.C.Strings.chars_ptr)
         return Interfaces.C.int;

      RemoveXAttr_C_Access : Operations.RemoveXAttr_C_Type := RemoveXAttr_C'Access;

   end RemoveXAttr;


   --------------------------
   --  Open Directory
   --------------------------
   generic

   -- UNTESTED
      with function OpenDir
        (Path      :     in String;
         Fi        : access System.File_Info_Type)
         return System.Error_Type is <>;

   package OpenDir is private

      function OpenDir_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         Fi        : System.File_Info_Access)
         return Interfaces.C.int;

      OpenDir_C_Access : Operations.OpenDir_C_Type := OpenDir_C'Access;

   end OpenDir;


   --------------------------
   --  Read Directory
   --------------------------
   generic

   -- register each directory entry using Filler
      with function ReadDir
        (Path      :     in String;
         Filler    : access procedure
                              (Name     : String;
                               St_Buf   : System.Stat_Access;
                               Offset   : Natural);
         Offset    :     in Natural;
         Fi        : access System.File_Info_Type)
         return System.Error_Type is <>;

   package ReadDir is private

      function ReadDir_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         Buf       : General.Dir_Buffer_Type;
         Filler    : Operations.Fill_Dir_T;
         Off       : System.Off_T;
         Fi        : System.File_Info_Access)
         return Interfaces.C.int;

      ReadDir_C_Access : Operations.ReadDir_C_Type := ReadDir_C'Access;

   end ReadDir;


   --------------------------
   --  Release Directory
   --------------------------
   generic

   -- UNTESTED
      with function ReleaseDir
        (Path      :     in String;
         Fi        : access System.File_Info_Type)
         return System.Error_Type is <>;

   package ReleaseDir is private

      function ReleaseDir_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         Fi        : System.File_Info_Access)
         return Interfaces.C.int;

      ReleaseDir_C_Access : Operations.ReleaseDir_C_Type := ReleaseDir_C'Access;

   end ReleaseDir;


   --------------------------
   --  F Sync Directory
   --------------------------
   generic

   -- UNTESTED
      with function FSyncDir
        (Path      :     in String;
         DataSync  :     in Natural;
         Fi        : access System.File_Info_Type)
         return System.Error_Type is <>;

   package FSyncDir is private

      function FSyncDir_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         Datasync  : Interfaces.C.int;
         Fi        : System.File_Info_Access)
         return Interfaces.C.int;

      FSyncDir_C_Access : Operations.FSyncDir_C_Type := FSyncDir_C'Access;

   end FSyncDir;


   --------------------------
   --  Init
   --------------------------
    generic

    -- UNTESTED
        with function Init
          (Conn : access System.Fuse_Conn_Info_Type)
          return System.Error_Type is <>;

    package Init is private

        function Init_C
            (Conn : System.Fuse_Conn_Info_Access)
            return Interfaces.C.int;

        Init_C_Access : Operations.Init_C_Type := Init_C'Access;

    end Init;


   --------------------------
   --  Destroy
   --------------------------
   generic

   -- UNTESTED
       with function Destroy
         (Userdata : access User_Data_Type)
         return System.Error_Type is <>;

   package Destroy is private

       function Destroy_C
           (Userdata : User_Data_Access)
           return Interfaces.C.int;

       Destroy_C_Access : Operations.Destroy_C_Type := Destroy_C'Access;

   end Destroy;


   --------------------------
   --  Access
   --------------------------
   generic

   -- UNTESTED
      with function Access_Ada
        (Path      :     in String;
         Mask      :     in Natural)
         return System.Error_Type is <>;

   package Access_Ada is private

      function Access_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         Mask      : Interfaces.C.int)
         return Interfaces.C.int;

      Access_C_Access : Operations.Access_C_Type := Access_C'Access;

   end Access_Ada;


   --------------------------
   --  Create
   --------------------------
   generic

   -- same as mknod + open
      with function Create
        (Path      :     in String;
         Mode      :     in System.St_Mode_Type;
         Fi        : access System.File_Info_Type)
         return System.Error_Type is <>;

   package Create is private

      function Create_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         Mode      : System.Mode_T;
         Fi        : System.File_Info_Access)
         return Interfaces.C.int;

      Create_C_Access : Operations.Create_C_Type := Create_C'Access;

   end Create;


   --------------------------
   --  F Truncate
   --------------------------
   generic

   -- UNTESTED
      with function FTruncate
        (Path      :     in String;
         Off_T     :     in Natural;
         Fi        : access System.File_Info_Type)
         return System.Error_Type is <>;

   package FTruncate is private

      function FTruncate_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         Off_T     : System.Off_T;
         Fi        : System.File_Info_Access)
         return Interfaces.C.int;

      FTruncate_C_Access : Operations.FTruncate_C_Type := FTruncate_C'Access;

   end FTruncate;


   --------------------------
   --  F Get Attributes
   --------------------------
   generic

   -- UNTESTED
      with function FGetAttr
        (Path      :     in String;
         St_Buf    : access System.Stat_Type;
         Fi        : access System.File_Info_Type)
         return System.Error_Type is <>;

   package FGetAttr is private

      function FGetAttr_C
        (Path      : Interfaces.C.Strings.chars_ptr;
         St_Buf    : System.Stat_Access;
         Fi        : System.File_Info_Access)
         return Interfaces.C.int;

      FGetAttr_C_Access : Operations.FGetAttr_C_Type := FGetAttr_C'Access;

   end FGetAttr;


   --------------------------
   --  Lock
   --------------------------
   -- TODO implement

   --------------------------
   --  U Time NS
   --------------------------
   -- TODO implement

   --------------------------
   --  B Map
   --------------------------
   -- TODO implement

   --------------------------
   --  IO Control
   --------------------------
   -- TODO implement

   --------------------------
   --  Poll
   --------------------------
   -- TODO implement

---------------------------------------------------------------------

private

   package Sys_Operations is new System.Operations
     (GetAttr_C_Type     => Operations.GetAttr_C_Type,
      ReadLink_C_Type    => Operations.ReadLink_C_Type,
      MkNod_C_Type       => Operations.MkNod_C_Type,
      MkDir_C_Type       => Operations.MkDir_C_Type,
      Unlink_C_Type      => Operations.Unlink_C_Type,
      RmDir_C_Type       => Operations.RmDir_C_Type,
      SymLink_C_Type     => Operations.SymLink_C_Type,
      Rename_C_Type      => Operations.Rename_C_Type,
      Link_C_Type        => Operations.Link_C_Type,
      ChMod_C_Type       => Operations.ChMod_C_Type,
      ChOwn_C_Type       => Operations.ChOwn_C_Type,
      Truncate_C_Type    => Operations.Truncate_C_Type,
      UTime_C_Type       => Operations.UTime_C_Type,
      Open_C_Type        => Operations.Open_C_Type,
      Read_C_Type        => Operations.Read_C_Type,
      Write_C_Type       => Operations.Write_C_Type,
      StatFS_C_Type      => Operations.StatFS_C_Type,
      Flush_C_Type       => Operations.Flush_C_Type,
      Release_C_Type     => Operations.Release_C_Type,
      FSync_C_Type       => Operations.FSync_C_Type,
      SetXAttr_C_Type    => Operations.SetXAttr_C_Type,
      GetXAttr_C_Type    => Operations.GetXAttr_C_Type,
      ListXAttr_C_Type   => Operations.ListXAttr_C_Type,
      RemoveXAttr_C_Type => Operations.RemoveXAttr_C_Type,
      OpenDir_C_Type     => Operations.OpenDir_C_Type,
      ReadDir_C_Type     => Operations.ReadDir_C_Type,
      ReleaseDir_C_Type  => Operations.ReleaseDir_C_Type,
      FSyncDir_C_Type    => Operations.FSyncDir_C_Type,
      Init_C_Type        => Operations.Init_C_Type,
      Destroy_C_Type     => Operations.Destroy_C_Type,
      Access_C_Type      => Operations.Access_C_Type,
      Create_C_Type      => Operations.Create_C_Type,
      FTruncate_C_Type   => Operations.FTruncate_C_Type,
      FGetAttr_C_Type    => Operations.FGetAttr_C_Type);
--      Lock_C_Type        => Operations.Lock_C_Type,
--      UTimeNS_C_Type     => Operations.UTimeNS_C_Type,
--      BMap_C_Type        => Operations.BMap_C_Type,
--      IOCtl_C_Type       => Operations.IOCtl_C_Type,
--      Poll_C_Type        => Operations.Poll_C_Type);

   Operations_C : aliased Sys_Operations.Operations_C_Record;


end Fuse.Main;

-- vim: ts=3 sw=3 et
