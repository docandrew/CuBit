with CCL.Scheduling_Limits;
package body CCL.Resource_Sections with SPARK_Mode => On is

   MATCH_FIRST : constant := HEADER_BYTES + 1;
   ENTRIES_FIRST : constant := HEADER_BYTES + MATCH_BYTES + 1;

   function Valid_Index (Match : Match_Info; Index : Unsigned_32) return Boolean is
     (case Match.Kind is
         when PCI_Class_Match | PCI_ID_Match => Index < PCI_BAR_COUNT,
         when Platform_Match => Index < MAX_PLATFORM_RESOURCES,
         when No_Match => False);

   function Page_Sized (Amount, Limit : Unsigned_64) return Boolean is
     (Amount in PAGE_BYTES .. Limit and then Amount mod PAGE_BYTES = 0);

   function Code (Mode : Interrupt_Mode) return Unsigned_64 is
     (Unsigned_64 (Interrupt_Mode'Enum_Rep (Mode)));

   --  Ordinary bodies, so proofs use these predicates as opaque facts rather
   --  than unfolding them inside quantifiers.
   function Valid_Match (Match : Match_Info) return Boolean is
   begin
      return
        (case Match.Kind is
           when No_Match =>
              (for all Value of Match.Values => Value = 0),
           when PCI_Class_Match =>
              (for all Value of Match.Values => Value <= PCI_CODE_LAST),
           --  0xFFFF is "no device" in PCI configuration space.
           when PCI_ID_Match =>
              Match.Values (1) /= PCI_ID_LAST and then Match.Values (3) = 0,
           when Platform_Match =>
              Match.Values (1) in
                Unsigned_16 (Platform_Device'Enum_Rep (Platform_Device'First)) ..
                Unsigned_16 (Platform_Device'Enum_Rep (Platform_Device'Last))
              and then Match.Values (2) = 0 and then Match.Values (3) = 0);
   end Valid_Match;

   function Valid_Resource (Match : Match_Info; Item : Resource) return Boolean is
   begin
      return
        (case Item.Kind is
           when Device_Memory =>
              Valid_Index (Match, Item.Index)
              and then Page_Sized (Item.Amount, MAX_DEVICE_MEMORY_BYTES)
              and then Item.Rights in Read_Only | Read_Write
              and then Item.Extra = 0,
           when IO_Ports =>
              Valid_Index (Match, Item.Index)
              and then Item.Amount in 1 .. IO_PORT_SPACE
              and then Item.Rights = Read_Write
              and then Item.Extra = 0,
           when Interrupt =>
              Item.Rights = Read_Only
              and then
                (case Match.Kind is
                    when No_Match => False,
                    when Platform_Match =>
                       Item.Extra = Code (Platform_Line)
                       and then Valid_Index (Match, Item.Index)
                       and then Item.Amount = 1,
                    when PCI_Class_Match | PCI_ID_Match =>
                       Item.Index = 0
                       and then
                         ((Item.Extra = Code (Line) and then Item.Amount = 1)
                          or else
                          ((Item.Extra = Code (MSI_X) or else Item.Extra = Code (MSI))
                           and then Item.Amount in 1 .. MAX_INTERRUPT_VECTORS))),
           when DMA =>
              Match.Kind /= No_Match
              and then Item.Index = 0
              and then Page_Sized (Item.Amount, MAX_DMA_BYTES)
              and then Item.Rights = Read_Write
              and then Item.Extra = 0,
           when Scheduling =>
              Item.Rights = Read_Only
              and then Item.Index = 0
              and then Item.Amount in 1 .. CCL.Scheduling_Limits.MAX_MICROSECONDS
              and then Item.Extra in 1 .. CCL.Scheduling_Limits.MAX_MICROSECONDS
              and then CCL.Scheduling_Limits.Admissible
                         (Integer_64 (Item.Amount), Integer_64 (Item.Extra)));
   end Valid_Resource;

   procedure Decode
     (Data : Byte_Array; Plan : out Section_Plan; Status : out Decode_Status)
   is
      --  Little-endian fields.
      function U8 (First : Positive) return Unsigned_8 is (Data (First))
      with Pre => First in Data'Range;
      function U16 (First : Positive) return Unsigned_16 is
        (Unsigned_16 (Data (First)) or Shift_Left (Unsigned_16 (Data (First + 1)), 8))
      with Pre => First in Data'Range and then First < Data'Last;
      function U32 (First : Positive) return Unsigned_32 is
        (Unsigned_32 (U16 (First)) or Shift_Left (Unsigned_32 (U16 (First + 2)), 16))
      with Pre => First in Data'Range and then Data'Last - First >= 3;
      function U64 (First : Positive) return Unsigned_64 is
        (Unsigned_64 (U32 (First)) or Shift_Left (Unsigned_64 (U32 (First + 4)), 32))
      with Pre => First in Data'Range and then Data'Last - First >= 7;

      Count : Unsigned_16;
      Kind_Code, Rights_Code : Unsigned_8;
      Slot_Code : Unsigned_16;
      Item : Resource;
      Known : Boolean;
      Base : Positive;
      Has_Device : Boolean := False;
   begin
      Plan := (others => <>);
      Status := Invalid_Length;
      if Data'Length < HEADER_BYTES + MATCH_BYTES then
         return;
      end if;
      Count := U16 (7);
      if U32 (1) /= MAGIC or else U16 (5) /= VERSION
        or else Count not in 1 .. MAX_ENTRIES
      then
         Status := Invalid_Header;
         return;
      end if;
      if Data'Length /= HEADER_BYTES + MATCH_BYTES + Natural (Count) * ENTRY_BYTES then
         return;
      end if;

      --  The match: kind, a reserved byte, three values, 8 reserved bytes.
      Known := False;
      for Kind in Match_Kind loop
         if Unsigned_8 (Match_Kind'Enum_Rep (Kind)) = U8 (MATCH_FIRST) then
            Plan.Match.Kind := Kind;
            Known := True;
         end if;
      end loop;
      for Value in Match_Value_Index loop
         Plan.Match.Values (Value) := U16 (MATCH_FIRST + 2 * Value);
      end loop;
      if not Known or else U8 (MATCH_FIRST + 1) /= 0
        or else U64 (MATCH_FIRST + 8) /= 0
        or else not Valid_Match (Plan.Match)
      then
         Status := Invalid_Match;
         return;
      end if;

      for Number in 1 .. Natural (Count) loop
         pragma Loop_Invariant (Plan.Count = Number - 1);
         pragma Loop_Invariant (Valid_Match (Plan.Match));
         pragma Loop_Invariant
           (for all I in 1 .. Plan.Count =>
              Valid_Resource (Plan.Match, Plan.Entries (I)));
         pragma Loop_Invariant
           (for all I in 1 .. Plan.Count =>
              (for all J in 1 .. Plan.Count =>
                 I = J or else Plan.Entries (I).Slot /= Plan.Entries (J).Slot));
         Base := ENTRIES_FIRST + (Number - 1) * ENTRY_BYTES;
         Kind_Code := U8 (Base);
         Rights_Code := U8 (Base + 1);
         Slot_Code := U16 (Base + 2);
         Item := (others => <>);
         Known := False;
         for Kind in Resource_Kind loop
            if Unsigned_8 (Resource_Kind'Enum_Rep (Kind)) = Kind_Code then
               Item.Kind := Kind;
               Known := True;
            end if;
         end loop;
         if not Known then
            Status := Invalid_Entry;
            return;
         end if;
         Known := False;
         for Rights in Rights_Kind loop
            if Unsigned_8 (Rights_Kind'Enum_Rep (Rights)) = Rights_Code then
               Item.Rights := Rights;
               Known := True;
            end if;
         end loop;
         if not Known or else Slot_Code not in 1 .. Unsigned_16 (Slot_Number'Last) then
            Status := Invalid_Entry;
            return;
         end if;
         Item.Slot := Slot_Number (Slot_Code);
         Item.Index := U32 (Base + 4);
         Item.Amount := U64 (Base + 8);
         Item.Extra := U64 (Base + 16);
         if Item.Kind in Device_Resource and then Plan.Match.Kind = No_Match then
            Status := Device_Without_Match;
            return;
         end if;
         if not Valid_Resource (Plan.Match, Item) then
            Status := Invalid_Entry;
            return;
         end if;
         for I in 1 .. Plan.Count loop
            if Plan.Entries (I).Slot = Item.Slot then
               Status := Duplicate_Slot;
               return;
            end if;
            pragma Loop_Invariant
              (for all J in 1 .. I => Plan.Entries (J).Slot /= Item.Slot);
         end loop;
         Plan.Count := Number;
         Plan.Entries (Number) := Item;
      end loop;

      for I in 1 .. Plan.Count loop
         if Plan.Entries (I).Kind in Device_Resource then
            Has_Device := True;
         end if;
         pragma Loop_Invariant
           (Has_Device =
              (for some J in 1 .. I => Plan.Entries (J).Kind in Device_Resource));
      end loop;

      if (Plan.Match.Kind /= No_Match) /= Has_Device then
         Status := Match_Without_Device;
         return;
      end if;
      Status := Decoded;
   end Decode;
end CCL.Resource_Sections;
