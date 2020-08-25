package body Rot_13 is

   function Encrypt (Plaintext : String) return String
   is
      Cyphertext : String := Plaintext;
      C : Integer;

   begin

      for I in Plaintext'Range loop

         C := Character'Pos(Plaintext(I));

         case C is
            --lowercase letter
            when 97..122 =>
               Cyphertext(I) := Character'Val((((C-97)+13) mod 26)+97);

            --uppercase letter
            when 65..90 =>
               Cyphertext(I) := Character'Val((((C-65)+13) mod 26)+65);

            when others =>
               Cyphertext(I) := Plaintext(I);

         end case;

      end loop;

      return Cyphertext;

   end Encrypt;

   function Encrypt (Plaintext : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr
   is
      use Interfaces.C.Strings;

   begin

      return New_String(Encrypt(Value(Plaintext)));

   end Encrypt;

end Rot_13;
