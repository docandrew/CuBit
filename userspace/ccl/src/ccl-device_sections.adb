package body CCL.Device_Sections with SPARK_Mode => On is

   --  Byte offsets inside an entry.
   KIND_OFFSET    : constant := 0;
   LENGTH_OFFSET  : constant := 1;
   PAYLOAD_OFFSET : constant := ENTRY_PREFIX_BYTES;
   PAYLOAD_BYTES  : constant := ENTRY_BYTES - ENTRY_PREFIX_BYTES;
   PCI_IDS_BYTES  : constant := 6;
   CLASS_BYTES    : constant := 3;
   PLATFORM_BYTES : constant := 1;
   BYTE_BITS      : constant := 8;

   subtype Payload_Length is Natural range 0 .. PAYLOAD_BYTES;

   function Payload_Of (M : Device_Match) return Payload_Length is
     (case M.Kind is
         when PCI_IDs => PCI_IDS_BYTES,
         when PCI_Class | USB_Class => CLASS_BYTES,
         when ACPI_ID => M.ID_Length,
         when Platform => PLATFORM_BYTES);

   procedure Encode (T : Match_Table; Bytes : out Section_Bytes; Length : out Section_Length) is
      procedure Put16 (At_Byte : Positive; Value : Unsigned_16)
      with Pre => At_Byte < MAX_SECTION_BYTES
      is
      begin
         Bytes (At_Byte) := Unsigned_8 (Value and 16#FF#);
         Bytes (At_Byte + 1) := Unsigned_8 (Shift_Right (Value, BYTE_BITS));
      end Put16;
      Base : Positive;
   begin
      Bytes := [others => 0];
      Put16 (1, Unsigned_16 (MAGIC mod 2 ** 16));
      Put16 (3, Unsigned_16 (MAGIC / 2 ** 16));
      Put16 (5, VERSION);
      Put16 (7, Unsigned_16 (T.Count));
      for I in 1 .. T.Count loop
         pragma Loop_Invariant (True);
         Base := HEADER_BYTES + (I - 1) * ENTRY_BYTES + 1;
         declare
            M : constant Device_Match := T.Entries (I);
            P : constant Positive := Base + PAYLOAD_OFFSET;
         begin
            Bytes (Base + KIND_OFFSET) := Unsigned_8 (Match_Kind'Enum_Rep (M.Kind));
            Bytes (Base + LENGTH_OFFSET) := Unsigned_8 (Payload_Of (M));
            case M.Kind is
               when PCI_IDs =>
                  Put16 (P, M.Vendor);
                  Put16 (P + 2, M.First);
                  Put16 (P + 4, M.Last);
               when PCI_Class | USB_Class =>
                  Bytes (P) := M.Class;
                  Bytes (P + 1) := M.Subclass;
                  Bytes (P + 2) := M.Program;
               when ACPI_ID =>
                  for C in 1 .. M.ID_Length loop
                     Bytes (P + C - 1) := Character'Pos (M.ID (C));
                  end loop;
               when Platform =>
                  Bytes (P) := Unsigned_8 (Platform_Device'Enum_Rep (M.Device));
            end case;
         end;
      end loop;
      Length := HEADER_BYTES + T.Count * ENTRY_BYTES;
   end Encode;

   procedure Decode (Data : Byte_Array; T : out Match_Table; Status : out Decode_Status) is
      function U16 (First : Positive) return Unsigned_16 is
        (Unsigned_16 (Data (First)) or Shift_Left (Unsigned_16 (Data (First + 1)), BYTE_BITS))
      with Pre => First in Data'Range and then First < Data'Last;

      Count : Natural;
      Base, P : Positive;
      M : Device_Match;
      Known : Boolean;
      Length : Natural;
   begin
      T := (others => <>);
      Status := Invalid_Length;
      if Data'Length < HEADER_BYTES then
         return;
      end if;
      if U16 (1) /= Unsigned_16 (MAGIC mod 2 ** 16)
        or else U16 (3) /= Unsigned_16 (MAGIC / 2 ** 16)
        or else U16 (5) /= VERSION
        or else U16 (7) not in 1 .. MAX_ENTRIES
      then
         Status := Invalid_Header;
         return;
      end if;
      Count := Natural (U16 (7));
      if Data'Length /= HEADER_BYTES + Count * ENTRY_BYTES then
         return;
      end if;
      for I in 1 .. Count loop
         pragma Loop_Invariant (T.Count = I - 1);
         pragma Loop_Invariant (for all J in 1 .. T.Count => Valid_Match (T.Entries (J)));
         Base := HEADER_BYTES + (I - 1) * ENTRY_BYTES + 1;
         P := Base + PAYLOAD_OFFSET;
         M := (others => <>);
         Known := False;
         for K in Match_Kind loop
            if Unsigned_8 (Match_Kind'Enum_Rep (K)) = Data (Base + KIND_OFFSET) then
               M.Kind := K;
               Known := True;
            end if;
         end loop;
         Length := Natural (Data (Base + LENGTH_OFFSET));
         if not Known or else Length > PAYLOAD_BYTES
           or else Data (Base + 2) /= 0 or else Data (Base + 3) /= 0
         then
            Status := Invalid_Entry;
            return;
         end if;
         --  Bytes past the payload are zero.
         for B in P + Length .. Base + ENTRY_BYTES - 1 loop
            if Data (B) /= 0 then
               Status := Invalid_Entry;
               return;
            end if;
         end loop;
         case M.Kind is
            when PCI_IDs =>
               M.Vendor := U16 (P);
               M.First := U16 (P + 2);
               M.Last := U16 (P + 4);
               Known := Length = PCI_IDS_BYTES;
            when PCI_Class | USB_Class =>
               M.Class := Data (P);
               M.Subclass := Data (P + 1);
               M.Program := Data (P + 2);
               Known := Length = CLASS_BYTES;
            when ACPI_ID =>
               Known := Length in ACPI_ID_MIN .. ACPI_ID_MAX;
               if Known then
                  M.ID_Length := Length;
                  for C in 1 .. Length loop
                     M.ID (C) := Character'Val (Data (P + C - 1));
                  end loop;
               end if;
            when Platform =>
               Known := False;
               for D in Platform_Device loop
                  if Unsigned_8 (Platform_Device'Enum_Rep (D)) = Data (P) then
                     M.Device := D;
                     Known := True;
                  end if;
               end loop;
               Known := Known and then Length = PLATFORM_BYTES;
         end case;
         if not Known or else not Valid_Match (M) then
            Status := Invalid_Entry;
            return;
         end if;
         T.Count := I;
         T.Entries (I) := M;
      end loop;
      Status := Decoded;
   end Decode;

   procedure Best (T : Match_Table; F : PCI_Function; Best_Rank : out Rank; At_Entry : out Entry_Count) is
      R : Rank;
   begin
      Best_Rank := No_Rank;
      At_Entry := 0;
      for I in 1 .. T.Count loop
         pragma Loop_Invariant
           (if At_Entry = 0 then Best_Rank = No_Rank
            else At_Entry < I and then Best_Rank = Rank_Of (T.Entries (At_Entry), F)
                 and then Best_Rank /= No_Rank);
         pragma Loop_Invariant (for all J in 1 .. I - 1 => Rank_Of (T.Entries (J), F) <= Best_Rank);
         R := Rank_Of (T.Entries (I), F);
         if R > Best_Rank then
            pragma Assert (for all J in 1 .. I - 1 => Rank_Of (T.Entries (J), F) < R);
            Best_Rank := R;
            At_Entry := I;
         end if;
         pragma Assert (Rank_Of (T.Entries (I), F) <= Best_Rank);
         pragma Assert (for all J in 1 .. I => Rank_Of (T.Entries (J), F) <= Best_Rank);
      end loop;
   end Best;

end CCL.Device_Sections;
