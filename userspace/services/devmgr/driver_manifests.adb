------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  What a program's manifest asks for - implementation
------------------------------------------------------------------------------
pragma Ada_2022;
package body Driver_Manifests with SPARK_Mode => On is
   use type ST.Table;

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

   --  .cubit.caps field offsets.
   CAPS_VERSION_OFFSET    : constant := 4;
   CAPS_COUNT_OFFSET      : constant := 6;
   CAPS_KIND_OFFSET       : constant := 0;
   CAPS_RIGHTS_OFFSET     : constant := 1;
   CAPS_SERVICE_OFFSET    : constant := 4;
   CAPS_DESCRIPTOR_OFFSET : constant := 8;

   --  Little-endian field of Bytes bytes at 0-based Offset.
   function Field (Image : Byte_Array; Offset, Bytes : Unsigned_64) return Unsigned_64
   with Pre => Image'First = 1 and then Image'Last < Natural'Last
     and then Bytes in 1 .. MAX_FIELD_BYTES
     and then Offset <= Unsigned_64 (Image'Length)
     and then Bytes <= Unsigned_64 (Image'Length) - Offset
   is
      Value : Unsigned_64 := 0;
   begin
      for I in reverse 0 .. Bytes - 1 loop
         pragma Assert (Offset + I + 1 <= Unsigned_64 (Image'Last));
         declare
            Index : constant Positive := Positive (Offset + I + 1);
         begin
            pragma Assert (Unsigned_64 (Index) = Offset + I + 1);
            pragma Assert (Unsigned_64 (Index) <= Unsigned_64 (Image'Last));
            pragma Assert (Index <= Image'Last);
            Value := Shift_Left (Value, BITS_PER_BYTE) or Unsigned_64 (Image (Index));
         end;
      end loop;
      return Value;
   end Field;

   --  The first PROGBITS section of Minimum .. Maximum bytes that starts
   --  with Magic.
   procedure Find_Section
     (Image : Byte_Array; Magic : Unsigned_64; Minimum, Maximum : Unsigned_64;
      Offset, Size : out Unsigned_64; Found : out Boolean)
   with Pre  => Image'First = 1 and then Image'Last < Natural'Last
                and then Minimum >= MAGIC_BYTES and then Minimum <= Maximum,
        Post => (if Found then Size in Minimum .. Maximum
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
           and then Bytes in Minimum .. Maximum
           and then At_Offset <= Length
           and then Bytes <= Length - At_Offset
           and then Field (Image, At_Offset, MAGIC_BYTES) = Magic
         then
            Offset := At_Offset;
            Size := Bytes;
            Found := True;
            return;
         end if;
      end loop;
   end Find_Section;

   procedure Read_Startup (Image : Byte_Array; Into : in out Manifest)
   with Pre  => Image'First = 1 and then Image'Last < Natural'Last,
        Post => Into.Capabilities = Into.Capabilities'Old
                and then Into.Capability_Total = Into.Capability_Total'Old
                and then (if Into.Startup_Found
                          then ST.Valid_Table (Into.Declared)
                               and then ST.Declaration (Into.Declared))
   is
      Offset, Size : Unsigned_64;
      Found : Boolean;
      Outcome : ST.Decode_Result;
      use type ST.Decode_Result;
   begin
      Into.Startup_Found := False;
      Into.Declared := ST.Empty_Table;
      Find_Section (Image, Unsigned_64 (ST.Magic), ST.Header_Bytes, ST.Maximum_Table_Bytes,
                    Offset, Size, Found);
      if not Found then
         return;
      end if;
      declare
         Raw : ST.Bytes (1 .. ST.Maximum_Table_Bytes) := [others => 0];
         Last : constant Positive := Positive (Size);
      begin
         for J in 1 .. Size loop
            pragma Assert (Offset + J <= Unsigned_64 (Image'Last));
            declare
               Index : constant Positive := Positive (Offset + J);
            begin
               pragma Assert (Unsigned_64 (Index) = Offset + J);
               pragma Assert (Unsigned_64 (Index) <= Unsigned_64 (Image'Last));
               pragma Assert (Index <= Image'Last);
               Raw (Positive (J)) := Image (Index);
            end;
         end loop;
         ST.Decode (Raw (1 .. Last), Into.Declared, Outcome);
      end;
      if Outcome = ST.Decoded and then ST.Declaration (Into.Declared) then
         Into.Startup_Found := True;
      else
         Into.Declared := ST.Empty_Table;
      end if;
   end Read_Startup;

   procedure Read_Capabilities (Image : Byte_Array; Into : in out Manifest)
   with Pre  => Image'First = 1 and then Image'Last < Natural'Last,
        Post => Into.Startup_Found = Into.Startup_Found'Old
                and then Into.Declared = Into.Declared'Old
   is
      Offset, Size, Count, Base, Rights : Unsigned_64;
      Found : Boolean;
   begin
      Into.Capability_Total := 0;
      Find_Section (Image, CAPS_MAGIC, CAPS_HEADER_BYTES, MAX_CAPS_BYTES, Offset, Size, Found);
      if not Found
        or else Field (Image, Offset + CAPS_VERSION_OFFSET, 2) /= CAPS_VERSION
      then
         return;
      end if;
      Count := Field (Image, Offset + CAPS_COUNT_OFFSET, 2);
      if Count > MAX_CAPABILITIES
        or else Size /= CAPS_HEADER_BYTES + Count * CAPS_ENTRY_BYTES
      then
         return;
      end if;
      for I in 1 .. Natural (Count) loop
         pragma Assert (Unsigned_64 (I) <= Count);
         pragma Assert (Unsigned_64 (I - 1) * CAPS_ENTRY_BYTES + CAPS_ENTRY_BYTES
                        <= Count * CAPS_ENTRY_BYTES);
         Base := Offset + CAPS_HEADER_BYTES + Unsigned_64 (I - 1) * CAPS_ENTRY_BYTES;
         pragma Assert (Base + CAPS_ENTRY_BYTES <= Unsigned_64 (Image'Length));
         Rights := Field (Image, Base + CAPS_RIGHTS_OFFSET, 1);
         if Rights not in Unsigned_64 (Rights_Kind'Enum_Rep (Read_Only))
                        .. Unsigned_64 (Rights_Kind'Enum_Rep (Read_Write))
         then
            Into.Capability_Total := 0;
            return;
         end if;
         Into.Capabilities (I) :=
           (Kind       => Unsigned_8 (Field (Image, Base + CAPS_KIND_OFFSET, 1) and 16#FF#),
            Rights     => (if Rights = Unsigned_64 (Rights_Kind'Enum_Rep (Read_Only)) then Read_Only
                           elsif Rights = Unsigned_64 (Rights_Kind'Enum_Rep (Write_Only)) then Write_Only
                           else Read_Write),
            Service    => Unsigned_32 (Field (Image, Base + CAPS_SERVICE_OFFSET, 4) and 16#FFFF_FFFF#),
            Descriptor => Field (Image, Base + CAPS_DESCRIPTOR_OFFSET, 8));
      end loop;
      Into.Capability_Total := Natural (Count);
   end Read_Capabilities;

   procedure Read_Resources (Image : Byte_Array; Into : in out Manifest)
   with Pre  => Image'First = 1 and then Image'Last < Natural'Last,
        Post => Into.Startup_Found = Into.Startup_Found'Old
                and then Into.Declared = Into.Declared'Old
                and then Into.Capabilities = Into.Capabilities'Old
                and then Into.Capability_Total = Into.Capability_Total'Old
   is
      Offset, Size : Unsigned_64;
      Found : Boolean;
      Status : Decode_Status;
   begin
      Into.Resources_Found := False;
      Find_Section (Image, MAGIC, HEADER_BYTES, MAX_SECTION_BYTES, Offset, Size, Found);
      if not Found then
         return;
      end if;
      declare
         Data : Byte_Array (1 .. MAX_SECTION_BYTES) := [others => 0];
         Last : constant Positive := Positive (Size);
      begin
         for J in 1 .. Size loop
            pragma Assert (Offset + J <= Unsigned_64 (Image'Last));
            declare
               Index : constant Positive := Positive (Offset + J);
            begin
               pragma Assert (Unsigned_64 (Index) = Offset + J);
               pragma Assert (Unsigned_64 (Index) <= Unsigned_64 (Image'Last));
               pragma Assert (Index <= Image'Last);
               Data (Positive (J)) := Image (Index);
            end;
         end loop;
         Decode (Data (1 .. Last), Into.Resources, Status);
      end;
      Into.Resources_Found := Status = Decoded;
   end Read_Resources;

   function Read (Image : Byte_Array) return Manifest is
      Result : Manifest;
   begin
      Read_Capabilities (Image, Result);
      Read_Resources (Image, Result);
      Read_Startup (Image, Result);
      return Result;
   end Read;

   function Source_Of (M : Manifest; Index : ST.Entry_Index) return Source is
      Capabilities, Resources : Natural := 0;
   begin
      if not M.Startup_Found or else Index > M.Declared.Count then
         return (others => <>);
      end if;
      for I in 1 .. Index loop
         pragma Loop_Invariant (Capabilities + Resources = I - 1);
         if Resource_Entry (M.Declared.Entries (I).Kind) then
            Resources := Resources + 1;
         else
            Capabilities := Capabilities + 1;
         end if;
      end loop;
      if Resource_Entry (M.Declared.Entries (Index).Kind) then
         if M.Resources_Found and then Resources <= M.Resources.Count then
            return (Resource_Source, Resources);
         end if;
      elsif Capabilities <= M.Capability_Total then
         return (Capability_Source, Capabilities);
      end if;
      return (others => <>);
   end Source_Of;

   function Interrupt_Of (M : Manifest) return Interrupt_Request is
   begin
      if not M.Resources_Found then
         return (others => <>);
      end if;
      for I in 1 .. M.Resources.Count loop
         declare
            Item : constant Resource := M.Resources.Entries (I);
         begin
            if Item.Kind = Interrupt
              and then Item.Extra in
                Unsigned_64 (Interrupt_Mode'Enum_Rep (MSI_X)) |
                Unsigned_64 (Interrupt_Mode'Enum_Rep (MSI))
              and then Item.Amount in 1 .. MAX_INTERRUPT_VECTORS
            then
               return (Present => True,
                       Mode    => (if Item.Extra = Unsigned_64 (Interrupt_Mode'Enum_Rep (MSI_X))
                                   then MSI_X else MSI),
                       Vectors => Positive (Item.Amount));
            end if;
         end;
      end loop;
      return (others => <>);
   end Interrupt_Of;

end Driver_Manifests;
