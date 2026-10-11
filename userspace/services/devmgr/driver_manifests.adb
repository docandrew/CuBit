------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  What a driver's manifest asks of its device - implementation
------------------------------------------------------------------------------
package body Driver_Manifests with SPARK_Mode => On is

   --  ELF64 (System V gABI 4.1): header fields and section header fields
   --  this reader uses, by byte offset.
   ELF_HEADER_BYTES       : constant := 64;
   HEADER_SECTION_TABLE   : constant := 40;   --  e_shoff
   HEADER_ENTRY_SIZE      : constant := 58;   --  e_shentsize
   HEADER_SECTION_COUNT   : constant := 60;   --  e_shnum
   SECTION_HEADER_BYTES   : constant := 64;
   SECTION_TYPE           : constant := 4;    --  sh_type
   SECTION_OFFSET         : constant := 24;   --  sh_offset
   SECTION_SIZE           : constant := 32;   --  sh_size
   SHT_PROGBITS           : constant := 1;
   MAGIC_BYTES            : constant := 4;
   MAX_FIELD_BYTES        : constant := 8;
   BITS_PER_BYTE          : constant := 8;

   --  Little-endian field of Bytes bytes at 0-based Offset.
   function Field (Image : Byte_Array; Offset, Bytes : Unsigned_64) return Unsigned_64
   with Pre => Image'First = 1 and then Bytes in 1 .. MAX_FIELD_BYTES
     and then Offset <= Unsigned_64 (Image'Length)
     and then Bytes <= Unsigned_64 (Image'Length) - Offset
   is
      Value : Unsigned_64 := 0;
   begin
      for I in reverse 0 .. Bytes - 1 loop
         Value := Shift_Left (Value, BITS_PER_BYTE) or
           Unsigned_64 (Image (Positive (Offset + I + 1)));
      end loop;
      return Value;
   end Field;

   procedure Find_Resources
     (Image : Byte_Array; Offset, Size : out Unsigned_64; Found : out Boolean)
   with Pre  => Image'First = 1 and then Image'Last < Natural'Last,
        Post => (if Found then Size in HEADER_BYTES .. MAX_SECTION_BYTES
                   and then Offset <= Unsigned_64 (Image'Length)
                   and then Size <= Unsigned_64 (Image'Length) - Offset)
   is
      Length : constant Unsigned_64 := Unsigned_64 (Image'Length);
      Table, Count, Header, Kind, At_Offset, Bytes : Unsigned_64;
   begin
      Offset := 0;
      Size := 0;
      Found := False;
      if Length < ELF_HEADER_BYTES then
         return;
      end if;
      Table := Field (Image, HEADER_SECTION_TABLE, 8);
      Count := Field (Image, HEADER_SECTION_COUNT, 2);
      if Field (Image, HEADER_ENTRY_SIZE, 2) /= SECTION_HEADER_BYTES
        or else Table > Length
        or else Count > (Length - Table) / SECTION_HEADER_BYTES
      then
         return;
      end if;
      for Index in 1 .. Count loop
         pragma Loop_Invariant (not Found);
         Header := Table + (Index - 1) * SECTION_HEADER_BYTES;
         Kind := Field (Image, Header + SECTION_TYPE, 4);
         At_Offset := Field (Image, Header + SECTION_OFFSET, 8);
         Bytes := Field (Image, Header + SECTION_SIZE, 8);
         if Kind = SHT_PROGBITS
           and then Bytes in HEADER_BYTES .. MAX_SECTION_BYTES
           and then At_Offset <= Length
           and then Bytes <= Length - At_Offset
           and then Field (Image, At_Offset, MAGIC_BYTES) = MAGIC
         then
            Offset := At_Offset;
            Size := Bytes;
            Found := True;
            return;
         end if;
      end loop;
   end Find_Resources;

   function Interrupt_Of (Image : Byte_Array) return Interrupt_Request is
      Offset, Size : Unsigned_64;
      Found : Boolean;
      Plan : Section_Plan;
      Status : Decode_Status;
   begin
      Find_Resources (Image, Offset, Size, Found);
      if not Found then
         return (others => <>);
      end if;
      declare
         Data : Byte_Array (1 .. Positive (Size)) := [others => 0];
      begin
         for I in Data'Range loop
            Data (I) := Image (Positive (Offset + Unsigned_64 (I)));
         end loop;
         Decode (Data, Plan, Status);
      end;
      if Status /= Decoded then
         return (others => <>);
      end if;
      for I in 1 .. Plan.Count loop
         declare
            Item : constant Resource := Plan.Entries (I);
         begin
            if Item.Kind = Interrupt
              and then Item.Extra in
                Unsigned_64 (Interrupt_Mode'Enum_Rep (MSI_X)) |
                Unsigned_64 (Interrupt_Mode'Enum_Rep (MSI))
              and then Item.Amount in 1 .. MAX_INTERRUPT_VECTORS
            then
               return (Present => True,
                       Slot    => Item.Slot,
                       Mode    => (if Item.Extra = Unsigned_64 (Interrupt_Mode'Enum_Rep (MSI_X))
                                   then MSI_X else MSI),
                       Vectors => Positive (Item.Amount));
            end if;
         end;
      end loop;
      return (others => <>);
   end Interrupt_Of;

end Driver_Manifests;
