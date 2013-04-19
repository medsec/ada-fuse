with Ada.Command_Line;

package body Fuse is

   function Get_Arguments return Arguments_Type
   is
      Arguments : Arguments_Type;
   begin
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         Arguments.Append (Ada.Command_Line.Argument (I));
      end loop;
      return Arguments;
   end Get_Arguments;

end Fuse;

-- vim: ts=3 sw=3 et
