--
--
--


-- used packages


with Ada.Text_IO;
with AUnit.Assertions;
with Ada.Directories;
with Ada.IO_Exceptions;

with Command_Line_Calls;


-- package implementation


package body Test_Setup is
   
   
   ----------------------------------------------------------------------------
   --                                                                        --
   -- the scaffolding for the testcases                                      --
   --                                                                        --
   ----------------------------------------------------------------------------
   
   
   -- place for used variables and constants used for testing
   -- e.g.
   -- I1, I2, I3 : Int;
   -- Expected1, Expexted2, Expexted3 : Int;
   
   
   Framework_Test     : Integer;
   Framework_Value    : Integer;
   Framework_Expected : Integer;
   
   
   -- implementation of necessary functions
   
   
   procedure Set_Up_Case(T: in out TC) is
      pragma Unreferenced(T);
      Current_Dir       : constant String := Ada.Directories.Current_Directory;
      Folder_Created    : Boolean := False;
      FS_Mounted        : Boolean := False;
      Mount_Exception   : Exception;
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("Run Set_Up_Case ...");
      Ada.Text_IO.New_Line;
   
      -- Here are the basic setup for testcases.
      -- Especially the expected values of the tests are writen here.
      -- e.g.
      -- Expected1 := 5;
      -- Expected2 := 9;
      -- Expected3 := 1;
   
      Framework_Test       := 2;
      Framework_Expected   := 4;

      -- create folders
      while not Folder_Created loop
         Ada.Directories.Create_Path(Current_Dir & "/from");
         Folder_Created := Ada.Directories.Exists(Current_Dir & "/from");
      end loop;

      Folder_Created := False;

      while not Folder_Created loop
         Ada.Directories.Create_Path(Current_Dir & "/to");
         Folder_Created := Ada.Directories.Exists(Current_Dir & "/to");
      end loop;

      -- mount the filesystem
      FS_Mounted := Command_Line_Calls.Mount_FS;
      if FS_Mounted then
         Ada.Text_IO.Put_Line("-- ok");
      else
         Ada.Text_IO.Put_Line("Filesystem could not be mounted.");
         raise Mount_Exception;
      end if;
   end Set_Up_Case;
   
   
   procedure Set_Up(T: in out TC) is
      pragma Unreferenced(T);
   begin
      -- Procedure not used
      -- May useful for later use.
      null;
   end Set_Up;
   
   
   procedure Tear_Down(T: in out TC) is
      pragma Unreferenced(T);
   begin
      -- Procedure not used
      -- May useful for later use.
      null;
   end Tear_Down;
   
   
   procedure Tear_Down_Case(T: in out TC) is
      pragma Unreferenced(T);
      Current_Dir : constant String := Ada.Directories.Current_Directory;
      Success     : Boolean := False;
      Mount_Exception   : Exception;
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("Run Tear_Down_Case ...");

      -- unmount filesystem
      Success := Command_Line_Calls.Unmount_FS(3, "1");

      -- delete folders
      if Success then
         Ada.Text_IO.Put_Line("-- ok");
         Ada.Directories.Delete_Tree(Current_Dir & "/from");
         Ada.Directories.Delete_Tree(Current_Dir & "/to");
      else
         Ada.Text_IO.Put_Line("The filesystem could not be unmounted.");
         Ada.Text_IO.Put_Line("Folders are not deleted.");
         raise Mount_Exception;
      end if;
   end Tear_Down_Case;
   
   
   ----------------------------------------------------------------------------
   --                                                                        --
   -- the testcases which should be tested                                   --
   --                                                                        --
   ----------------------------------------------------------------------------
   
   
   ---------------------------------------
   -- T1: simple framework test
   ---------------------------------------
   
   
   procedure Simple_Framework_Test(CWTC: in out AUnit.Test_Cases.Test_Case'Class) is
      -- not in example, but compiler warning
      pragma Unreferenced(CWTC);
   begin
      Framework_Value := Framework_Test + Framework_Test; -- should be 4
      AUnit.Assertions.Assert(Condition => (Framework_Value = Framework_Expected),
                              Message => "Incorrect value for a simple addition.");
   end Simple_Framework_Test;
   
   
   ---------------------------------------
   -- T2: create a folder
   ---------------------------------------
   
   
   procedure Create_A_Folder(CWTC: in out AUnit.Test_Cases.Test_Case'Class) is
      -- not in example, but compiler warning
      pragma Unreferenced(CWTC);
      Current_Dir    : constant String := Ada.Directories.Current_Directory;
      New_Dir_Path   : constant String := "/to/";
      Test_Dir       : constant String := "test_dir/";
      New_Dir_Name   : constant String := "ordner";
      Search_Pattern : constant String := "";
      Search         : Ada.Directories.Search_Type;
      Dir_Entry      : Ada.Directories.Directory_Entry_Type;
      Filter         : constant Ada.Directories.Filter_Type := (
         Ada.Directories.Ordinary_File => False,
         Ada.Directories.Special_File  => False,
         Ada.Directories.Directory     => True
      );
      Dir_Found      : Boolean := False;
   begin
      -- part 1: create dir
      Ada.Directories.Create_Path(Current_Dir & New_Dir_Path & Test_Dir &
                                  New_Dir_Name);

      -- part 2: check
      Ada.Directories.Start_Search(Search, Current_Dir & New_Dir_Path &
                                   Test_Dir, Search_Pattern, Filter);

      while Ada.Directories.More_Entries(Search) loop
         Ada.Directories.Get_Next_Entry(Search, Dir_Entry);
         if Ada.Directories.Simple_Name(Dir_Entry) = New_Dir_Name then
            Dir_Found := True;
            Ada.Directories.End_Search (Search);
         end if;
      end loop;

      Ada.Directories.End_Search (Search);
      
      AUnit.Assertions.Assert(Condition => Dir_Found,
                              Message => "The created directory could not be found.");
   end Create_A_Folder;
   
   
   ---------------------------------------
   -- T3: create a folder 2
   ---------------------------------------
   
   
   --#
   --# Should be removed, if there is a solution for te problem.
   pragma Warnings(Off);
   --#
   --#
   procedure Create_A_Folder2(CWTC: in out AUnit.Test_Cases.Test_Case'Class) is
      -- not in example, but compiler warning
      pragma Unreferenced(CWTC);
      Current_Dir    : constant String := Ada.Directories.Current_Directory;
      New_Dir_Path   : constant String := "/to/";
      Test_Dir       : constant String := "test_dir/";
      New_Dir_Name   : constant String := "ordner";
      Dir_Found      : Boolean := False;
   begin
      -- part 1: create dir
      Ada.Directories.Create_Path(Current_Dir & New_Dir_Path & Test_Dir &
                                  New_Dir_Name);

      -- part 2: catch exception
      -- exception should be raised?
      -- Nothing happens. So this test will allways fail.
      -- Because no exception is raised, some variables aren't used.
      -- So some warnings are printed.
      --
      -- Added pragma Warnings, to avoid warnings, see top.

      AUnit.Assertions.Assert(Condition => Dir_Found,
                              Message => "The directory creation raised no exception");
   end Create_A_Folder2;
   --#
   --# Should be removed, if there is a solution for te problem.
   pragma Warnings(On);
   --#
   --#


   ---------------------------------------
   -- T4: remove a folder
   ---------------------------------------
   
   
   procedure Remove_A_Folder(CWTC: in out AUnit.Test_Cases.Test_Case'Class) is
      -- not in example, but compiler warning
      pragma Unreferenced(CWTC);
      Current_Dir    : constant String := Ada.Directories.Current_Directory;
      Dir_Path       : constant String := "/to/";
      Test_Dir       : constant String := "test_dir/";
      Dir_Name       : constant String := "ordner";
      Search_Pattern : constant String := "";
      Search         : Ada.Directories.Search_Type;
      Dir_Entry      : Ada.Directories.Directory_Entry_Type;
      Filter         : constant Ada.Directories.Filter_Type := (
         Ada.Directories.Ordinary_File => False,
         Ada.Directories.Special_File  => False,
         Ada.Directories.Directory     => True
      );
      Dir_Found      : Boolean := False;
   begin
      -- part 1: create dir
      Ada.Directories.Delete_Directory(Current_Dir & Dir_Path & Test_Dir &
                                       Dir_Name);

      -- part 2: check
      Ada.Directories.Start_Search( Search, Current_Dir & Dir_Path & Test_Dir,
                                    Search_Pattern, Filter);

      while Ada.Directories.More_Entries(Search) loop
         Ada.Directories.Get_Next_Entry(Search, Dir_Entry);
         if Ada.Directories.Simple_Name(Dir_Entry) = Dir_Name then
            Dir_Found := True;
            Ada.Directories.End_Search (Search);
         end if;
      end loop;

      Ada.Directories.End_Search (Search);
      
      AUnit.Assertions.Assert(Condition => not Dir_Found,
                              Message => "The directory was not removed.");
   end Remove_A_Folder;


   ---------------------------------------
   -- T5: remove a folder 2
   ---------------------------------------
   
   
   procedure Remove_A_Folder2(CWTC: in out AUnit.Test_Cases.Test_Case'Class) is
      -- not in example, but compiler warning
      pragma Unreferenced(CWTC);
      Current_Dir       : constant String := Ada.Directories.Current_Directory;
      Dir_Path          : constant String := "/to/";
      Test_Dir          : constant String := "test_dir/";
      Dir_Name          : constant String := "ordner";
      Exception_Raised  : Boolean := False;
   begin
      -- part 1: create dir
      Ada.Directories.Delete_Directory(Current_Dir & Dir_Path & Test_Dir & Dir_Name);

      -- part 2: catch exception
      exception
         when Ada.IO_Exceptions.Name_Error =>
            Exception_Raised := True;
      
     AUnit.Assertions.Assert(Condition => Exception_Raised,
                              Message => "The directory was not removed.");
   end Remove_A_Folder2;


   ---------------------------------------
   -- T6: rename a folder
   ---------------------------------------
   
   
   procedure Rename_A_Folder(CWTC: in out AUnit.Test_Cases.Test_Case'Class) is
      -- not in example, but compiler warning
      pragma Unreferenced(CWTC);
      Current_Dir    : constant String := Ada.Directories.Current_Directory;
      Dir_Path       : constant String := "/to/";
      Test_Dir       : constant String := "test_dir/";
      Dir_Name       : constant String := "ordner9";
      New_Dir_Name   : constant String := "ordner10";
      Dir_Found      : Boolean := False;
   begin
      -- part 1: create dir
      Ada.Directories.Create_Path(Current_Dir & Dir_Path & Test_Dir & Dir_Name);

      -- part 2: check if it was created
      Dir_Found := Ada.Directories.Exists(Current_Dir & Dir_Path & Test_Dir &
                                          Dir_Name);
      AUnit.Assertions.Assert(Condition => Dir_Found,
                              Message => "The created directory could not be found.");

      -- part3: rename
      Ada.Directories.Rename(Current_Dir & Dir_Path & Test_Dir & Dir_Name,
                             Current_Dir & Dir_Path & Test_Dir & New_Dir_Name);

      -- part4: check if it was renamed
      Dir_Found := Ada.Directories.Exists(Current_Dir & Dir_Path & Test_Dir &
                                          New_Dir_Name);
      AUnit.Assertions.Assert(Condition => Dir_Found,
                              Message => "The renamed directory could not be found.");
   end Rename_A_Folder;
  

   ---------------------------------------
   -- T7: rename a folder 2
   ---------------------------------------
   
   
   procedure Rename_A_Folder2(CWTC: in out AUnit.Test_Cases.Test_Case'Class) is
      -- not in example, but compiler warning
      pragma Unreferenced(CWTC);
      Current_Dir       : constant String := Ada.Directories.Current_Directory;
      Dir_Path          : constant String := "/to/";
      Test_Dir          : constant String := "test_dir/";
      Dir_Name          : constant String := "ordner9";
      New_Dir_Name      : constant String := "ordner10";
      Exception_Raised  : Boolean := False;
   begin
      -- part 1: rename
      Ada.Directories.Rename(Current_Dir & Dir_Path & Test_Dir & Dir_Name,
                             Current_Dir & Dir_Path & Test_Dir & New_Dir_Name);

      -- part 2: catch exception
      exception
         when Ada.IO_Exceptions.Name_Error =>
            Exception_Raised := True;

      AUnit.Assertions.Assert(Condition => Exception_Raised,
                              Message => "The renamed directory could not be found.");
   end Rename_A_Folder2;
  

   ---------------------------------------
   -- T8: create a file
   ---------------------------------------
   
   
   procedure Create_A_File(CWTC: in out AUnit.Test_Cases.Test_Case'Class) is
      -- not in example, but compiler warning
      pragma Unreferenced(CWTC);
      File           : Ada.Text_IO.File_Type;
      File_Mode      : constant Ada.Text_IO.File_Mode := Ada.Text_IO.Out_File;
      File_Name      : constant String := "datei";
      Current_Dir    : constant String := Ada.Directories.Current_Directory;
      Dir_Path       : constant String := "/to/";
      Test_Dir       : constant String := "test_dir/";
      File_Created   : Boolean := False;
   begin
      -- part 1: create file, stream content
      Ada.Text_IO.Create(File, File_Mode, Current_Dir & Dir_Path & Test_Dir &
                         File_Name, "");
      Ada.Text_IO.Close(File);

      -- part 2: check if file exists
      File_Created := Ada.Directories.Exists(Current_Dir & Dir_Path & Test_Dir &
                                             File_Name);

      AUnit.Assertions.Assert(Condition => File_Created,
                              Message => "The file was not created.");
   end Create_A_File;


   ---------------------------------------
   -- T9: write a file
   ---------------------------------------
   
   
   procedure Write_A_File(CWTC: in out AUnit.Test_Cases.Test_Case'Class) is
      -- not in example, but compiler warning
      pragma Unreferenced(CWTC);
      File           : Ada.Text_IO.File_Type;
      File_Mode      : constant Ada.Text_IO.File_Mode := Ada.Text_IO.Out_File;
      File_Name      : constant String := "datei2";
      Current_Dir    : constant String := Ada.Directories.Current_Directory;
      Dir_Path       : constant String := "/to/";
      Test_Dir       : constant String := "test_dir/";
      Content        : constant String := "Hallo Welt!";
      Size_Before    : Ada.Directories.File_Size;
      Size_After     : Ada.Directories.File_Size;
   begin
      -- part 1: create file, write content
      Ada.Text_IO.Create(File, File_Mode, Current_Dir & Dir_Path & Test_Dir &
                         File_Name, "");
      Size_Before := Ada.Directories.Size(Current_Dir & Dir_Path & Test_Dir &
                                          File_Name);
      Ada.Text_IO.Put(File, Content);
      Ada.Text_IO.Close(File);
      Size_After := Ada.Directories.Size(Current_Dir & Dir_Path & Test_Dir &
                                         File_Name);

      -- part 2: check if file exists
      AUnit.Assertions.Assert(Condition => (Ada.Directories.">"(Size_After, Size_Before)),
                              Message => "The file was not writen.");
   end Write_A_File;


   ---------------------------------------
   -- T10: read a file
   ---------------------------------------
   
   
   procedure Read_A_File(CWTC: in out AUnit.Test_Cases.Test_Case'Class) is
      -- not in example, but compiler warning
      pragma Unreferenced(CWTC);
      File           : Ada.Text_IO.File_Type;
      File_Mode_Out  : constant Ada.Text_IO.File_Mode := Ada.Text_IO.Out_File;
      File_Mode_In   : constant Ada.Text_IO.File_Mode := Ada.Text_IO.In_File;
      File_Name      : constant String := "datei3";
      Current_Dir    : constant String := Ada.Directories.Current_Directory;
      Dir_Path       : constant String := "/to/";
      Test_Dir       : constant String := "test_dir/";
      Content        : constant String := "Hallo Welt! Und noch was ...";
      Input          : String(Content'Range);
   begin
      -- part 1: create file, write content
      Ada.Text_IO.Create(File, File_Mode_Out, Current_Dir & Dir_Path & Test_Dir
                         & File_Name, "");
      Ada.Text_IO.Put(File, Content);
      Ada.Text_IO.Close(File);

      -- part 2: read file and check content
      Ada.Text_IO.Open(File, File_Mode_In, Current_Dir & Dir_Path & Test_Dir &
                       File_Name, "");
      Ada.Text_IO.Get(File, Input);
      Ada.Text_IO.Close(File);

      AUnit.Assertions.Assert(Condition => (Content = Input),
                              Message => "The file could not be read.");
   end Read_A_File;


   ---------------------------------------
   -- T11: delete a file
   ---------------------------------------
   
   
   procedure Delete_A_File(CWTC: in out AUnit.Test_Cases.Test_Case'Class) is
      -- not in example, but compiler warning
      pragma Unreferenced(CWTC);
      File           : Ada.Text_IO.File_Type;
      File_Mode_Out  : constant Ada.Text_IO.File_Mode := Ada.Text_IO.Out_File;
      File_Name      : constant String := "datei4";
      Current_Dir    : constant String := Ada.Directories.Current_Directory;
      Dir_Path       : constant String := "/to/";
      Test_Dir       : constant String := "test_dir/";
      File_Exists    : Boolean := False;
   begin
      -- part 1: create file
      Ada.Text_IO.Create(File, File_Mode_Out, Current_Dir & Dir_Path & Test_Dir
                         & File_Name, "");
      Ada.Text_IO.Close(File);

      -- part 2: check if file was created
      File_Exists := Ada.Directories.Exists(Current_Dir & Dir_Path & Test_Dir &
                                            File_Name);
      AUnit.Assertions.Assert(Condition => File_Exists,
                              Message => "The file was not created.");

      -- part 3: delete file
      Ada.Directories.Delete_File(Current_Dir & Dir_Path & Test_Dir & File_Name);

      -- part 4: check if file was deleted
      File_Exists := Ada.Directories.Exists(Current_Dir & Dir_Path & Test_Dir &
                                            File_Name);
      AUnit.Assertions.Assert(Condition => not File_Exists,
                              Message => "The file was not deleted.");
   end Delete_A_File;


   ---------------------------------------
   -- T12: read a file 2
   ---------------------------------------
   
   
   procedure Read_A_File2(CWTC: in out AUnit.Test_Cases.Test_Case'Class) is
      -- not in example, but compiler warning
      pragma Unreferenced(CWTC);
      File              : Ada.Text_IO.File_Type;
      File_Mode_In      : constant Ada.Text_IO.File_Mode := Ada.Text_IO.In_File;
      File_Name         : constant String := "datei5";
      Current_Dir       : constant String := Ada.Directories.Current_Directory;
      Dir_Path          : constant String := "/to/";
      Test_Dir          : constant String := "test_dir/";
      Exception_Raised  : Boolean := False;
   begin
      -- part 1: try to open file that not exists
      Ada.Text_IO.Open(File, File_Mode_In, Current_Dir & Dir_Path & Test_Dir &
                       File_Name, "");

      -- part 2: catch exception
      exception
         when ADA.IO_Exceptions.NAME_ERROR =>
            Exception_Raised := True;
            
      AUnit.Assertions.Assert(Condition => Exception_Raised,
                              Message => "A not existing file was opened.");
   end Read_A_File2;


   ---------------------------------------
   -- T13: delete a file 2
   ---------------------------------------
   
   
   procedure Delete_A_File2(CWTC: in out AUnit.Test_Cases.Test_Case'Class) is
      -- not in example, but compiler warning
      pragma Unreferenced(CWTC);
      File_Name         : constant String := "datei5";
      Current_Dir       : constant String := Ada.Directories.Current_Directory;
      Dir_Path          : constant String := "/to/";
      Test_Dir          : constant String := "test_dir/";
      Exception_Raised  : Boolean := False;
   begin
      -- part 1: delete file that not exists
      Ada.Directories.Delete_File(Current_Dir & Dir_Path & Test_Dir & File_Name);

      -- part 2: catch exception
      exception
         when ADA.IO_Exceptions.NAME_ERROR =>
            Exception_Raised := True;
            
      AUnit.Assertions.Assert(Condition => Exception_Raised,
                              Message => "The file was not deleted.");
   end Delete_A_File2;


   ---------------------------------------
   -- T14: move a file
   ---------------------------------------
   
   
   procedure Move_A_File(CWTC: in out AUnit.Test_Cases.Test_Case'Class) is
      -- not in example, but compiler warning
      pragma Unreferenced(CWTC);
      File           : Ada.Text_IO.File_Type;
      File_Mode_Out  : constant Ada.Text_IO.File_Mode := Ada.Text_IO.Out_File;
      File_Mode_In   : constant Ada.Text_IO.File_Mode := Ada.Text_IO.In_File;
      File_Name      : constant String := "datei6";
      Current_Dir    : constant String := Ada.Directories.Current_Directory;
      Dir_Path       : constant String := "/to/";
      Test_Dir       : constant String := "test_dir/";
      Source         : constant String := "ordner2/";
      Target         : constant String := "ordner3/";
      Content        : constant String := "Eine Datei zum Verschieben.";
      Input          : String(Content'Range);
      File_Exists    : Boolean := False;
      Dir_Exists     : Boolean := False;
   begin
      -- part 1: create folders and check creation, create file
      Ada.Directories.Create_Path(Current_Dir & Dir_Path & Test_Dir & Source);
      Ada.Directories.Create_Path(Current_Dir & Dir_Path & Test_Dir & Target);

      Dir_Exists := Ada.Directories.Exists(Current_Dir & Dir_Path & Test_Dir &
                                           Source);
      AUnit.Assertions.Assert(Condition => Dir_Exists,
                              Message => "The source directory was not created.");
      Dir_Exists := Ada.Directories.Exists(Current_Dir & Dir_Path & Test_Dir &
                                           Target);
      AUnit.Assertions.Assert(Condition => Dir_Exists,
                              Message => "The target directory was not created.");

      -- part 2: create file, write content
      Ada.Text_IO.Create(File, File_Mode_Out, Current_Dir & Dir_Path & Test_Dir
                         & Source & File_Name, "");
      Ada.Text_IO.Put(File, Content);
      Ada.Text_IO.Close(File);

      -- part 2: copy file, delete source
      Ada.Directories.Copy_File(Current_Dir & Dir_Path & Test_Dir & Source &
                                File_Name,
                                Current_Dir & Dir_Path & Test_Dir & Target &
                                File_Name, "");
      Ada.Directories.Delete_File(Current_Dir & Dir_Path & Test_Dir & Source &
                                  File_Name);
      
      -- part 3: check if source was deleted and if content is correct
      File_Exists := Ada.Directories.Exists(Current_Dir & Dir_Path & Test_Dir &
                                            Source & File_Name);
      AUnit.Assertions.Assert(Condition => not File_Exists,
                              Message => "The source file was not deleted.");
      Ada.Text_IO.Open(File, File_Mode_In, Current_Dir & Dir_Path & Test_Dir &
                       Target & File_Name, "");
      Ada.Text_IO.Get(File, Input);
      Ada.Text_IO.Close(File);

      AUnit.Assertions.Assert(Condition => (Content = Input),
                              Message => "The content of moved file is not equal.");
   end Move_A_File;


   ---------------------------------------
   -- T15: rename a file
   ---------------------------------------
   
   
   procedure Rename_A_File(CWTC: in out AUnit.Test_Cases.Test_Case'Class) is
      -- not in example, but compiler warning
      pragma Unreferenced(CWTC);
      File           : Ada.Text_IO.File_Type;
      File_Mode_Out  : constant Ada.Text_IO.File_Mode := Ada.Text_IO.Out_File;
      File_Mode_In   : constant Ada.Text_IO.File_Mode := Ada.Text_IO.In_File;
      File_Name_Old  : constant String := "datei7";
      File_Name_New  : constant String := "datei8";
      Current_Dir    : constant String := Ada.Directories.Current_Directory;
      Dir_Path       : constant String := "/to/";
      Test_Dir       : constant String := "test_dir/";
      Content        : constant String := "Eine Datei zum Umbenennen.";
      Input          : String(Content'Range);
   begin
      -- part 1: create file, write content
      Ada.Text_IO.Create(File, File_Mode_Out, Current_Dir & Dir_Path & Test_Dir
                         & File_Name_Old, "");
      Ada.Text_IO.Put(File, Content);
      Ada.Text_IO.Close(File);

      -- part 2: rename file
      Ada.Directories.Rename(Current_Dir & Dir_Path & Test_Dir & File_Name_Old,
                             Current_Dir & Dir_Path & Test_Dir & File_Name_New);
      
      -- part 3: read file and check content
      Ada.Text_IO.Open(File, File_Mode_In, Current_Dir & Dir_Path & Test_Dir &
                       File_Name_New, "");
      Ada.Text_IO.Get(File, Input);
      Ada.Text_IO.Close(File);

      AUnit.Assertions.Assert(Condition => (Content = Input),
                              Message => "The content of renamed file is not equal.");
   end Rename_A_File;


   ---------------------------------------
   -- T16: delete a not empty directory
   ---------------------------------------


   procedure Delete_A_Not_Empty_Directory
      (CWTC: in out AUnit.Test_Cases.Test_Case'Class)
   is
      -- not in example, but compiler warning
      pragma Unreferenced(CWTC);
      File              : Ada.Text_IO.File_Type;
      File_Mode_Out     : constant Ada.Text_IO.File_Mode := Ada.Text_IO.Out_File;
      File_Name         : constant String := "datei11";
      Current_Dir       : constant String := Ada.Directories.Current_Directory;
      Dir_Path          : constant String := "/to/";
      Test_Dir          : constant String := "test_dir/";
      Not_Empty_Dir     : constant String := "not_empty/";
      Content           : constant String := "Eine Datei für rmdir.";
      Exception_Raised  : Boolean := False;
   begin
      -- part 1: create folder
      Ada.Directories.Create_Path(Current_Dir & Dir_Path & Test_Dir &
                                  Not_Empty_Dir);
      
      -- part 2: create file, write content
      Ada.Text_IO.Create(File, File_Mode_Out, Current_Dir & Dir_Path & Test_Dir
                         & Not_Empty_Dir & File_Name, "");
      Ada.Text_IO.Put(File, Content);
      Ada.Text_IO.Close(File);

      -- part 3: try to delete directory
      Ada.Directories.Delete_Directory(Current_Dir & Dir_Path & Test_Dir &
                                       Not_Empty_Dir);

      -- part 4: catch exception
      exception
         when ADA.IO_EXCEPTIONS.USE_ERROR =>
            Exception_Raised := True;

      AUnit.Assertions.Assert(Condition => Exception_Raised,
                              Message => "Seemingly a non empty folder was"
                              & " deleted with Delete_Directory().");
   end Delete_A_Not_Empty_Directory;


   ---------------------------------------
   -- T17: copy folder with content
   ---------------------------------------
   -- test not implemented
   -- This test should be equal to cp -r folder1 folder2.
   -- It's not necessary to implement such a test, because it's only a
   -- loop of already tested parts open(), read(), write(), mkdir().


   ---------------------------------------
   -- T18: Read_Without_Permission
   ---------------------------------------
   -- To test read permission of a file, the file should be "locked" via:
   -- chmod 200 file
   -- There is no procedure changing permissions, in the needed way so this
   -- test depents on the chmod function from own package Command_Line_Calls.


   procedure Read_Without_Permission
      (CWTC: in out AUnit.Test_Cases.Test_Case'Class)
   is
      -- not in example, but compiler warning
      pragma Unreferenced(CWTC);
      File              : Ada.Text_IO.File_Type;
      File_Mode_Out     : constant Ada.Text_IO.File_Mode := Ada.Text_IO.Out_File;
      File_Mode_In      : constant Ada.Text_IO.File_Mode := Ada.Text_IO.In_File;
      File_Name         : constant String := "datei12";
      Current_Dir       : constant String := Ada.Directories.Current_Directory;
      Dir_Path          : constant String := "/to/";
      Test_Dir          : constant String := "test_dir/";
      Changed_Mode      : Boolean := False;
      Content           : constant String := "Eine Datei für rmdir.";
      Exception_Raised  : Boolean := False;
   begin
      -- part 1: create file, write content
      Ada.Text_IO.Create(File, File_Mode_Out, Current_Dir & Dir_Path & Test_Dir
                         & File_Name, "");
      Ada.Text_IO.Put(File, Content);
      Ada.Text_IO.Close(File);

      -- part 2: chmod 200 datei
      Changed_Mode := Command_Line_Calls.Chmod(Current_Dir & Dir_Path & Test_Dir
                                               & File_Name, "200");
      AUnit.Assertions.Assert(Condition => Changed_Mode,
                              Message => "Seemingly the permissions could not"
                              & " be changed.");

      -- part 3: try to read
      Ada.Text_IO.Open(File, File_Mode_In, Current_Dir & Dir_Path & Test_Dir &
                       File_Name,"");
      Ada.Text_IO.Close(File);

      -- part 4: catch exception
      exception
         when ADA.IO_EXCEPTIONS.USE_ERROR =>
            Exception_Raised := True;

      AUnit.Assertions.Assert(Condition => Exception_Raised,
                              Message => "Seemingly a non empty folder was"
                              & " deleted with Delete_Directory().");
   end Read_Without_Permission;


   ---------------------------------------
   -- T19: Write_Without_Permission
   ---------------------------------------
   -- Same as above, only with write permissions.


   procedure Write_Without_Permission
      (CWTC: in out AUnit.Test_Cases.Test_Case'Class)
   is
      -- not in example, but compiler warning
      pragma Unreferenced(CWTC);
      File              : Ada.Text_IO.File_Type;
      File_Mode_Out     : constant Ada.Text_IO.File_Mode := Ada.Text_IO.Out_File;
      File_Mode_In      : constant Ada.Text_IO.File_Mode := Ada.Text_IO.In_File;
      File_Name         : constant String := "datei12";
      Current_Dir       : constant String := Ada.Directories.Current_Directory;
      Dir_Path          : constant String := "/to/";
      Test_Dir          : constant String := "test_dir/";
      Changed_Mode      : Boolean := False;
      Content           : constant String := "Eine Datei für rmdir.";
      Exception_Raised  : Boolean := False;
   begin
      -- part 1: create file, write content
      Ada.Text_IO.Create(File, File_Mode_Out, Current_Dir & Dir_Path & Test_Dir
                         & File_Name, "");
      Ada.Text_IO.Put(File, Content);
      Ada.Text_IO.Close(File);

      -- part 2: chmod 200 datei
      Changed_Mode := Command_Line_Calls.Chmod(Current_Dir & Dir_Path & Test_Dir
                                               & File_Name, "400");
      AUnit.Assertions.Assert(Condition => Changed_Mode,
                              Message => "Seemingly the permissions could not"
                              & " be changed.");

      -- part 3: try to write again
      Ada.Text_IO.Open(File, File_Mode_In, Current_Dir & Dir_Path & Test_Dir &
                       File_Name,"");
      Ada.Text_IO.Put(File, Content);
      Ada.Text_IO.Close(File);

      -- part 4: catch exception
      exception
         when ADA.IO_EXCEPTIONS.USE_ERROR =>
            Exception_Raised := True;

      AUnit.Assertions.Assert(Condition => Exception_Raised,
                              Message => "Seemingly a non empty folder was"
                              & " deleted with Delete_Directory().");
   end Write_Without_Permission;


   ---------------------------------------
   -- T20: delete folder with content
   ---------------------------------------
   
   
   procedure Delete_Folder_Content(CWTC: in out AUnit.Test_Cases.Test_Case'Class) is
      -- not in example, but compiler warning
      pragma Unreferenced(CWTC);
      Current_Dir       : constant String := Ada.Directories.Current_Directory;
      Dir_Path          : constant String := "/to/";
      Test_Dir          : constant String := "test_dir";
      Dir_Found         : Boolean := False;
      Exception_Raised  : Boolean := False;
   begin
      -- part 1: delete folder
      Ada.Directories.Delete_Tree(Current_Dir & Dir_Path & Test_Dir);

      -- part 2: check
      exception
         when Ada.IO_Exceptions.Use_Error =>
            Exception_Raised := True;

      AUnit.Assertions.Assert(Condition => not Exception_Raised,
                              Message => "No permission for deleting.");
            
      Dir_Found := Ada.Directories.Exists(Current_Dir & Dir_Path & Test_Dir);
      AUnit.Assertions.Assert(Condition => not Dir_Found,
                              Message => "The directory was not (completly) deleted.");
   end Delete_Folder_Content;


   ----------------------------------------------------------------------------
   --                                                                        --
   -- registration and name of the testcases                                 --
   --                                                                        --
   ----------------------------------------------------------------------------
   
   
   procedure Register_Tests(T: in out TC) is
      -- AUnit.Test_Cases.Registration
      package AU_TC_R renames AUnit.Test_Cases.Registration;
   begin
   
      AU_TC_R.Register_Routine(  Test => T,
                                 Routine => Simple_Framework_Test'Access,
                                 Name => "Simple_Framework_Test");
   
      AU_TC_R.Register_Routine(  Test => T,
                                 Routine => Create_A_Folder'Access,
                                 Name => "Create_A_Folder");
   
      AU_TC_R.Register_Routine(  Test => T,
                                 Routine => Create_A_Folder2'Access,
                                 Name => "Create_A_Folder2");

      AU_TC_R.Register_Routine(  Test => T,
                                 Routine => Remove_A_Folder'Access,
                                 Name => "Delete_A_Folder");
   
      AU_TC_R.Register_Routine(  Test => T,
                                 Routine => Remove_A_Folder2'Access,
                                 Name => "Delete_A_Folder2");
   
      AU_TC_R.Register_Routine(  Test => T,
                                 Routine => Rename_A_Folder'Access,
                                 Name => "Rename_A_Folder");
   
      AU_TC_R.Register_Routine(  Test => T,
                                 Routine => Rename_A_Folder2'Access,
                                 Name => "Rename_A_Folder2");
   
      AU_TC_R.Register_Routine(  Test => T,
                                 Routine => Create_A_File'Access,
                                 Name => "Create_A_File");
   
      AU_TC_R.Register_Routine(  Test => T,
                                 Routine => Write_A_File'Access,
                                 Name => "Write_A_File");
   
      AU_TC_R.Register_Routine(  Test => T,
                                 Routine => Read_A_File'Access,
                                 Name => "Read_A_File");
   
      AU_TC_R.Register_Routine(  Test => T,
                                 Routine => Delete_A_File'Access,
                                 Name => "Delete_A_File");
   
      AU_TC_R.Register_Routine(  Test => T,
                                 Routine => Read_A_File2'Access,
                                 Name => "Read_A_File2");
   
      AU_TC_R.Register_Routine(  Test => T,
                                 Routine => Delete_A_File2'Access,
                                 Name => "Read_A_File2");
   
      AU_TC_R.Register_Routine(  Test => T,
                                 Routine => Move_A_File'Access,
                                 Name => "Move_A_File");
   
      AU_TC_R.Register_Routine(  Test => T,
                                 Routine => Rename_A_File'Access,
                                 Name => "Rename_A_File");

      AU_TC_R.Register_Routine(  Test => T,
                                 Routine => Delete_A_Not_Empty_Directory'Access,
                                 Name => "Delete_A_Not_Empty_Directory");

      AU_TC_R.Register_Routine(  Test => T,
                                 Routine => Read_Without_Permission'Access,
                                 Name => "Read_Without_Permission");

      AU_TC_R.Register_Routine(  Test => T,
                                 Routine => Write_Without_Permission'Access,
                                 Name => "Write_Without_Permission");

      AU_TC_R.Register_Routine(  Test => T,
                                 Routine => Delete_Folder_Content'Access,
                                 Name => "Delete_Folder_Content");
   
   end Register_Tests;
   
   
   function Name(T: TC) return AUnit.Message_String is
      pragma Unreferenced(T);
   begin
      return AUnit.Format("Test-case: Standard");
   end Name;
   
   
end Test_Setup;
