-----------------------------------------------
-- package with needed calls to command-line --
-----------------------------------------------

-- used packages
--

with Gnat.OS_Lib;
with Ada.Text_IO;
with Ada.Directories;

-- package
--

package body Command_Line_Calls is


   -- Simple_Exec
   -- 
   -- This Function is used to run external programs.
   -- For example you can run the program sleep like below:
   --
   -- Simple_Exec("/bin/sleep", "3");


   function Simple_Exec
     (Program_Name      : String;
      Arguments_String  : String) return Boolean
   is
      Arguments      : Gnat.OS_Lib.String_List_Access;
      Success        : Boolean := False;
   begin
      Arguments := Gnat.OS_Lib.Argument_String_To_List(Arguments_String);
      Gnat.OS_Lib.Spawn(Program_Name, Arguments.all, Success);

      return Success;
   end Simple_Exec;


   procedure Simple_Exec_No_Return
     (Program_Name      : String;
      Arguments_String  : String)
   is
      Arguments      : Gnat.OS_Lib.String_List_Access;
      Success        : Boolean;
      pragma Unreferenced(Success); 
   begin
      Arguments := Gnat.OS_Lib.Argument_String_To_List(Arguments_String);
      Gnat.OS_Lib.Spawn(Program_Name, Arguments.all, Success);
   end Simple_Exec_No_Return;


   -- Mount_FS


   function Mount_FS return Boolean is
      Success  : Boolean := False;
   begin
      Ada.Text_IO.Put_Line("Try to mount filesystem ...");
      Success := Simple_Exec("./rotfs_main", "from to");
      return Success;
   end Mount_FS;


   -- Unmount_FS


   function Unmount_FS
     (Max_Tries: Integer;
      Interval_In_S: String) return Boolean
   is
      Tries                : Integer := 1;
      Current_Dir          : constant String := Ada.Directories.Current_Directory;
      Fusermount           : constant String := "/bin/fusermount";
      Fusermount_Arguments : constant String := "-u " & Current_Dir & "/to";
      Sleep                : constant String := "/bin/sleep";
      Sleep_Arguments      : constant String := Interval_In_S;
      Success              : Boolean := False;
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("Try to unmount filesystem ...");
      
      while not Success and Tries < Max_Tries loop
         Ada.Text_IO.Put_Line(Tries'Img & ". try ...");
         Ada.Text_IO.Put_Line("Wait " & Sleep_Arguments &
         " second before ...");
         Simple_Exec_No_Return(Sleep, Sleep_Arguments);
         Success := Simple_Exec(Fusermount, Fusermount_Arguments);
         tries := tries + 1;
      end loop;

      return Success;
   end Unmount_FS;


   -- Chmod


   function Chmod(Path_To_File : String; Mode : String) return Boolean is
      Chmod       : constant String := "/bin/chmod";
      Arguments   : constant String := Mode & " " & Path_To_File;
      Success     : Boolean := False;
   begin
      Success := Simple_Exec(Chmod, Arguments);
      return Success;
   end Chmod;


end Command_Line_Calls;
