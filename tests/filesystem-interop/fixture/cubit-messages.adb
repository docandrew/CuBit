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
                  Pack_Properties (FEATURE_FLUSH, Fixed_Media)]);
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
end CuBit.Messages;
