with Ada.Environment_Variables;
with System.Storage_Elements;
with Ada.Direct_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with CuBit.Block_Devices; use CuBit.Block_Devices;

package body CuBit.Messages is
   type Sector is array (Natural range 0 .. 511) of Unsigned_8;
   package IO is new Ada.Direct_IO (Sector);
   Image : IO.File_Type;
   Image_Path : Ada.Strings.Unbounded.Unbounded_String;
   Image_Size : Unsigned_64;

   procedure Open_Image (Path : String) is
   begin
      Image_Path := Ada.Strings.Unbounded.To_Unbounded_String (Path);
      IO.Open (Image, IO.Inout_File, Path);
      Image_Size := Unsigned_64 (IO.Size (Image)) * 512;
   end Open_Image;

   procedure Close_Image is
   begin
      if IO.Is_Open (Image) then IO.Close (Image); end if;
   end Close_Image;

   procedure debugPrint (value : String) is
   begin
      Ada.Text_IO.Put (value);
   end debugPrint;

   function capCall (slot : Unsigned_64; msg : in out Message) return MessageTag is
      pragma Unreferenced (slot);
      Op : constant Unsigned_32 := msg.tag.label;
      Offset : constant Unsigned_64 := msg.words (0) * 512;
      Count : constant Unsigned_64 := msg.words (2) * 512;
   begin
      if Op = OP_DESCRIBE_DEVICE then
         msg := ((REPLY_OK, 4, 0, 0), 0,
                 [Image_Size / 512, Pack_Sizes (512, 512), 8,
                  Pack_Properties (FEATURE_FLUSH or FEATURE_VOLATILE_CACHE, Fixed_Media)]);
      elsif Op = OP_FLUSH_DEVICE then
         --  Flush host buffering; no power-loss durability claim is made.
         IO.Close (Image);
         IO.Open (Image, IO.Inout_File, Ada.Strings.Unbounded.To_String (Image_Path));
         msg := ((REPLY_OK, 1, 0, 0), 0, [others => 0]);
      else
         pragma Assert (Op in OP_READ_BLOCKS | OP_WRITE_BLOCKS);
         pragma Assert (Count in 1 .. Grant_Buffer'Length);
         pragma Assert (Offset <= Image_Size and then Count <= Image_Size - Offset);
         for Index in 0 .. Natural (Count / 512) - 1 loop
            declare
               Data : Sector with Import,
                 Address => Grant_Buffer (Index * 512)'Address;
               Position : constant IO.Positive_Count :=
                 IO.Positive_Count (Offset / 512 + Unsigned_64 (Index) + 1);
            begin
               if Op = OP_READ_BLOCKS then
                  IO.Read (Image, Data, Position);
               else
                  IO.Write (Image, Data, Position);
               end if;
            end;
         end loop;
         msg := ((REPLY_OK, 1, 0, 0), 0, [0 => Count, others => 0]);
      end if;
      return msg.tag;
   end capCall;
   function syscall
     (call : Unsigned_64; arg0 : Unsigned_64 := 0; arg1 : Unsigned_64 := 0;
      arg2 : Unsigned_64 := 0; arg3 : Unsigned_64 := 0;
      arg4 : Unsigned_64 := 0; arg5 : Unsigned_64 := 0) return Unsigned_64
   is
      pragma Unreferenced (arg1, arg2, arg3, arg4, arg5);
      Page : constant := 4096;
      type Region is array (Natural range <>) of Unsigned_8;
      type Region_Access is access Region;
   begin
      if call = SYSCALL_GETTIME then
         return 0;
      elsif call = SYSCALL_INFO and then arg0 = SYSINFO_WALL_CLOCK_OFFSET then
         declare
            Clock : constant String :=
              Ada.Environment_Variables.Value ("CUBIT_TEST_WALL_CLOCK", "");
         begin
            return (if Clock = "" then Unsigned_64'Last
                    else Unsigned_64'Value (Clock) * 1_000);
         end;
      end if;
      if call /= SYSCALL_ALLOCATE_OWNED_MEMORY or else arg0 = 0 or else
        arg0 > 16 * 1024 * 1024
      then
         return Unsigned_64'Last;
      end if;
      declare
         Area : constant Region_Access :=
           new Region'(0 .. Natural (arg0) + Page - 1 => 0);
         Base : constant Unsigned_64 := Unsigned_64
           (System.Storage_Elements.To_Integer (Area.all (0)'Address));
      begin
         return (Base + Page - 1) and not (Page - 1);
      end;
   end syscall;
end CuBit.Messages;
