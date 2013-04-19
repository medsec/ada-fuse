package body Hello_World is

   -- Get Attributes
   --
   -- will be called to determine if the file / directory actually exists
   -- see 'man 2 stat'
   function GetAttr
     (Path   : in String;
      St_Buf : access System.Stat_Type)
      return System.Error_Type
   is
      use type System.St_Mode_Type;

   begin

      if Path = "/" then
         St_Buf.St_Mode := System.S_IFDIR or 8#755#;
         St_Buf.St_Nlink := 2;

      elsif Path = Hello_Path then
         St_Buf.St_Mode := System.S_IFREG or 8#444#;
         St_Buf.St_Nlink := 1;
         St_Buf.St_Size := Hello_String'Length;

      else
         return System.ENOENT;

      end if;

      return System.EXIT_SUCCESS;

   end GetAttr;


   -- Open
   --
   -- here we don't need to actually open anything,
   -- we just check the permissions
   function Open
     (Path   : in String;
      Fi     : access System.File_Info_Type)
      return System.Error_Type
   is
      use type System.RW_Type;

   begin

      if Path /= Hello_Path then
         return System.ENOENT;

      elsif Fi.all.Flags.RW /= System.O_RDONLY then
         return System.EACCES;

      else

         return System.EXIT_SUCCESS;

      end if;

   end Open;


   -- Read
   --
   -- reads the example file into Buffer
   --
   -- Size gives how much should be read and we need to modify it to tell how
   -- much actually is.
   function Read
     (Path   : in String;
      Buffer : access Buffer_Type;
      Size   : in out Natural;
      Offset : in Natural;
      Fi     : access System.File_Info_Type)
      return System.Error_Type
   is
      pragma Unreferenced (Fi);
      Length : constant  Natural := Hello_String'Length;

   begin

      if Path /= Hello_Path then
         return System.ENOENT;
      end if;

      if Offset <= Length then

         if Offset + Size > Length then
            Size := Length - Offset;
         end if;

         Buffer(1 .. Size) := Hello_String
            (Hello_String'Last - Size + 1 .. Hello_String'Last);

      else
         Size := 0;

      end if;

      return System.EXIT_SUCCESS;

   end Read;


   -- Read Directory
   --
   -- calls Filler for each directory entry
   function ReadDir
     (Path   : in String;
      Filler : access procedure
                 (Name     : String;
                  St_Buf   : System.Stat_Access;
                  Offset   : Natural);
      Offset : in Natural;
      Fi     : access System.File_Info_Type)
      return System.Error_Type
   is
      pragma Unreferenced (Offset, Fi);

   begin

      if Path /= "/" then
         return System.ENOENT;
      end if;

      Filler (".", null, 0);
      Filler ("..", null, 0);
      Filler (Hello_Path(Hello_Path'First +1 .. Hello_Path'Last), null, 0);

      return System.EXIT_SUCCESS;

   end ReadDir;

end Hello_World;

-- vim: ts=3 sw=3 et
