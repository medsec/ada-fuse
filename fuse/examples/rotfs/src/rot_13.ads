with Interfaces.C.Strings;

package Rot_13 is

   function Encrypt (Plaintext : String)
      return String;

   function Encrypt (Plaintext : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;

end Rot_13;
