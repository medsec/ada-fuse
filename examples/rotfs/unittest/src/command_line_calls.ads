-----------------------------------------------
-- package with needed calls to command-line --
-----------------------------------------------

package Command_Line_Calls is


   -- Simple_Exec


   function Simple_Exec
     (Program_Name      : String;
      Arguments_String  : String) return Boolean;

   procedure Simple_Exec_No_Return
     (Program_Name      : String;
      Arguments_String  : String);


   -- Mount_FS


   function Mount_FS return Boolean;


   -- Unmount_FS


   function Unmount_FS
     (Max_Tries      : Integer;
      Interval_In_S  : String) return Boolean;


   -- Chmod


   function Chmod(Path_To_File : String; Mode : String) return Boolean;


end Command_Line_Calls;
