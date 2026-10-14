with System.Storage_Elements;
with Ada.Direct_IO;
with Ada.Directories;
with CuBit.Block_Devices; use CuBit.Block_Devices;

--  Hosted benchmark device: the whole image held in memory, so timings
--  measure the Ext2 code, not host I/O. Nothing is written back.
package body CuBit.Messages is
   type Image_Bytes is array (Unsigned_64 range <>) of Unsigned_8;
   type Image_Access is access Image_Bytes;
   Image : Image_Access;
   Sector_Bytes : constant := 512;

   procedure Open_Image (Path : String) is
      Size : constant Unsigned_64 := Unsigned_64 (Ada.Directories.Size (Path));
      subtype Whole is Image_Bytes (0 .. Size - 1);
      package IO is new Ada.Direct_IO (Whole);
      File : IO.File_Type;
   begin
      Image := new Image_Bytes (0 .. Size - 1);
      IO.Open (File, IO.In_File, Path);
      IO.Read (File, Image.all);
      IO.Close (File);
   end Open_Image;

   procedure Close_Image is null;

   procedure debugPrint (value : String) is null;

   function capCall (slot : CuBit.Handles.Endpoint; msg : in out Message; Deadline : Unsigned_64)
     return MessageTag
   is
      pragma Unreferenced (slot, Deadline);
      Op : constant Unsigned_32 := msg.tag.label;
      Offset : constant Unsigned_64 := msg.words (0) * Sector_Bytes;
      Count : constant Unsigned_64 := msg.words (2) * Sector_Bytes;
   begin
      Commands := Commands + 1;
      if Op = OP_DESCRIBE_DEVICE then
         msg := ((label => REPLY_OK, length => 4, protocol => 0, ipc => <>, reserved => 0), 0,
                 [Image'Length / Sector_Bytes, Pack_Sizes (Sector_Bytes, Sector_Bytes), 1024,
                  Pack_Properties (FEATURE_FLUSH or FEATURE_VOLATILE_CACHE, Fixed_Media)]);
      elsif Op = OP_FLUSH_DEVICE then
         msg := ((label => REPLY_OK, length => 1, protocol => 0, ipc => <>, reserved => 0), 0, [others => 0]);
      else
         pragma Assert (Op in OP_READ_BLOCKS | OP_WRITE_BLOCKS);
         pragma Assert (Count in 1 .. Grant_Buffer'Length);
         pragma Assert (Offset <= Image'Length and then Count <= Image'Length - Offset);
         declare
            Device : Image_Bytes (0 .. Count - 1)
              with Import, Address => Image (Offset)'Address;
            Buffer : Image_Bytes (0 .. Count - 1)
              with Import, Address => Grant_Buffer (0)'Address;
         begin
            if Op = OP_READ_BLOCKS then
               Buffer := Device;
            else
               Device := Buffer;
            end if;
         end;
         msg := ((label => REPLY_OK, length => 1, protocol => 0, ipc => <>, reserved => 0), 0, [0 => Count, others => 0]);
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
