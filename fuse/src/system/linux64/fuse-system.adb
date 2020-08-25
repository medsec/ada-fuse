package body Fuse.System is

   function "or"
     (Left   : St_Mode_Type;
      Right  : St_Mode_Type) return St_Mode_Type
   is
      use type Mode_T;
   begin
      return Mode_T_to_St_Mode
         (St_Mode_to_Mode_T (Left) or St_Mode_to_Mode_T (Right));
   end "or";


   function "or"
     (Left   : St_Mode_Type;
      Right  : Mode_T) return St_Mode_Type
   is
      use type Mode_T;
   begin
      return Mode_T_to_St_Mode
         (St_Mode_to_Mode_T (Left) or Right);
   end "or";


   function "or"
     (Left   : Mode_T;
      Right  : St_Mode_Type) return St_Mode_Type
   is
      use type Mode_T;
   begin
      return Mode_T_to_St_Mode
         (Left or St_Mode_to_Mode_T(Right));
   end "or";


   pragma inline("or");

end Fuse.System;

-- vim: ts=3 sw=3 et
