pragma Ada_2022;

package body CuBit.ACPI_SCI_Route with SPARK_Mode is

   ---------------------------------------------------------------------------
   --  Little-endian fields, read only inside the buffer.
   ---------------------------------------------------------------------------
   function U8 (Item : Byte_Array; Offset : Natural) return Unsigned_8 is
     (Item (Offset + 1))
   with Pre => Item'First = 1 and then Item'Length <= Maximum_Table_Bytes
               and then Offset < Item'Length;

   function U16 (Item : Byte_Array; Offset : Natural) return Unsigned_16 is
     (Unsigned_16 (Item (Offset + 1)) or Shift_Left (Unsigned_16 (Item (Offset + 2)), 8))
   with Pre => Item'First = 1 and then Item'Length <= Maximum_Table_Bytes
               and then Item'Length >= 2 and then Offset <= Item'Length - 2;

   function U32 (Item : Byte_Array; Offset : Natural) return Unsigned_32 is
     (Unsigned_32 (Item (Offset + 1))
      or Shift_Left (Unsigned_32 (Item (Offset + 2)), 8)
      or Shift_Left (Unsigned_32 (Item (Offset + 3)), 16)
      or Shift_Left (Unsigned_32 (Item (Offset + 4)), 24))
   with Pre => Item'First = 1 and then Item'Length <= Maximum_Table_Bytes
               and then Item'Length >= 4 and then Offset <= Item'Length - 4;

   ---------------------------------------------------------------------------
   --  A table's extent: Whole when the buffer holds all of it (Length is
   --  its own Length field); Malformed when it states less than a header
   --  or more than the buffer holds.
   ---------------------------------------------------------------------------
   type Table_Kind is (Absent, Whole, Malformed, Oversized);
   type Table_View is record
      Kind   : Table_Kind := Absent;
      Length : Natural := 0;
   end record;

   function View_Of (Item : Byte_Array; Signature : String) return Table_View
   with Pre  => Item'First = 1 and then Item'Length <= Maximum_Table_Bytes
                and then Signature'Length = Signature_Bytes,
        Post => (if View_Of'Result.Kind = Whole then
                   View_Of'Result.Length in Header_Bytes .. Item'Length);

   function View_Of (Item : Byte_Array; Signature : String) return Table_View is
      Stated : Unsigned_32;
   begin
      if Item'Length < Header_Bytes then
         return (Kind => Absent, Length => 0);
      end if;
      for I in 0 .. Signature_Bytes - 1 loop
         if Item (1 + I) /= Character'Pos (Signature (Signature'First + I)) then
            return (Kind => Absent, Length => 0);
         end if;
      end loop;
      Stated := U32 (Item, Length_Offset);
      if Stated > Maximum_Table_Bytes then
         return (Kind => Oversized, Length => 0);
      elsif Natural (Stated) > Item'Length or else Stated < Header_Bytes then
         return (Kind => Malformed, Length => 0);
      end if;
      return (Kind => Whole, Length => Natural (Stated));
   end View_Of;

   ---------------------------------------------------------------------------
   --  Decode_Flags
   ---------------------------------------------------------------------------
   function Decode_Flags (Flags : Unsigned_16; SCI : Boolean) return Flag_Decoding is
      Reserved : constant Unsigned_16 := 2#10#;
      P : constant Unsigned_16 := Flags and Polarity_Mask;
      T : constant Unsigned_16 := Shift_Right (Flags, Trigger_Shift) and Trigger_Mask;
   begin
      if P = Reserved or else T = Reserved then
         return (Defined => False);
      end if;
      return
        (Defined  => True,
         Trigger  => (if T = Flags_Level or else (SCI and then T = Flags_Conforming)
                      then LA.Level else LA.Edge),
         Polarity => (if P = Flags_Active_Low or else (SCI and then P = Flags_Conforming)
                      then LA.Active_Low else LA.Active_High));
   end Decode_Flags;

   ---------------------------------------------------------------------------
   --  SCI_Of
   ---------------------------------------------------------------------------
   function SCI_Of (FADT : Byte_Array) return SCI_Field is
      V : constant Table_View := View_Of (FADT, FADT_Signature);
      Source : Unsigned_16;
   begin
      if V.Kind = Oversized then
         return (Present => False, Why => Too_Large);
      elsif V.Kind /= Whole or else V.Length < SCI_INT_Offset + SCI_INT_Bytes then
         return (Present => False, Why => No_FADT);
      end if;
      if V.Length >= FADT_Flags_Offset + FADT_Flags_Bytes
        and then (Shift_Right (U32 (FADT, FADT_Flags_Offset), HW_Reduced_ACPI_Bit) and 1) = 1
      then
         return (Present => False, Why => Hardware_Reduced);
      end if;
      Source := U16 (FADT, SCI_INT_Offset);
      if Source = 0 then
         return (Present => False, Why => No_SCI_Line);
      end if;
      return (Present => True, Source => Source);
   end SCI_Of;

   ---------------------------------------------------------------------------
   --  One pass over the MADT's entries.
   ---------------------------------------------------------------------------
   subtype Saturating_Count is Natural range 0 .. 2;   --  none, one, more

   type Scan_Result is record
      Well_Formed    : Boolean := True;
      --  Overrides for the source asked about.
      Overrides      : Saturating_Count := 0;
      Override_GSI   : GSI_Number := 0;
      Override_Flags : Unsigned_16 := 0;
      --  I/O APICs: the first's GSI base, and the lowest of the others'.
      IO_APICs       : Saturating_Count := 0;
      First_Base     : GSI_Number := 0;
      Lowest_Other   : GSI_Number := GSI_Number'Last;
   end record;

   procedure Scan
     (MADT : Byte_Array; Limit : Natural; Source : ISA_IRQ; R : out Scan_Result)
   with Pre => MADT'First = 1 and then MADT'Length <= Maximum_Table_Bytes
               and then Limit <= MADT'Length
               and then Limit >= MADT_Entries_Offset;

   --  Whether an override for a source other than Source names GSI.
   function Taken_By_Other
     (MADT : Byte_Array; Limit : Natural; GSI : GSI_Number; Source : ISA_IRQ) return Boolean
   with Pre => MADT'First = 1 and then MADT'Length <= Maximum_Table_Bytes
               and then Limit <= MADT'Length
               and then Limit >= MADT_Entries_Offset;

   procedure Scan
     (MADT : Byte_Array; Limit : Natural; Source : ISA_IRQ; R : out Scan_Result)
   is
      Here : Natural := MADT_Entries_Offset;
      Kind, Length : Natural;
      Base : GSI_Number;
   begin
      R := (others => <>);
      while Here < Limit loop
         pragma Loop_Invariant (Here in MADT_Entries_Offset .. Limit - 1);
         pragma Loop_Variant (Increases => Here);
         if Limit - Here < Entry_Header_Bytes then
            R.Well_Formed := False;
            return;
         end if;
         Kind   := Natural (U8 (MADT, Here + Entry_Type_Offset));
         Length := Natural (U8 (MADT, Here + Entry_Length_Offset));
         if Length < Entry_Header_Bytes or else Length > Limit - Here then
            R.Well_Formed := False;
            return;
         end if;
         if Kind = IO_APIC_Type then
            if Length < IO_APIC_Bytes then
               R.Well_Formed := False;
               return;
            end if;
            Base := U32 (MADT, Here + IO_APIC_GSI_Offset);
            if R.IO_APICs = 0 then
               R.First_Base := Base;
               R.IO_APICs := 1;
            else
               R.IO_APICs := 2;
               R.Lowest_Other := GSI_Number'Min (R.Lowest_Other, Base);
            end if;
         elsif Kind = Override_Type then
            if Length < Override_Bytes
              or else U8 (MADT, Here + Override_Bus_Offset) /= ISA_Bus
            then
               R.Well_Formed := False;
               return;
            end if;
            if Natural (U8 (MADT, Here + Override_Source_Offset)) = Source then
               if R.Overrides = 0 then
                  R.Overrides := 1;
                  R.Override_GSI := U32 (MADT, Here + Override_GSI_Offset);
                  R.Override_Flags := U16 (MADT, Here + Override_Flags_Offset);
               else
                  R.Overrides := 2;
               end if;
            end if;
         end if;
         Here := Here + Length;
      end loop;
   end Scan;

   function Taken_By_Other
     (MADT : Byte_Array; Limit : Natural; GSI : GSI_Number; Source : ISA_IRQ) return Boolean
   is
      Here : Natural := MADT_Entries_Offset;
      Kind, Length : Natural;
   begin
      while Here < Limit loop
         pragma Loop_Invariant (Here in MADT_Entries_Offset .. Limit - 1);
         pragma Loop_Variant (Increases => Here);
         exit when Limit - Here < Entry_Header_Bytes;
         Kind   := Natural (U8 (MADT, Here + Entry_Type_Offset));
         Length := Natural (U8 (MADT, Here + Entry_Length_Offset));
         exit when Length < Entry_Header_Bytes or else Length > Limit - Here;
         if Kind = Override_Type and then Length >= Override_Bytes
           and then Natural (U8 (MADT, Here + Override_Source_Offset)) /= Source
           and then U32 (MADT, Here + Override_GSI_Offset) = GSI
         then
            return True;
         end if;
         Here := Here + Length;
      end loop;
      return False;
   end Taken_By_Other;

   ---------------------------------------------------------------------------
   --  Resolve_ISA
   ---------------------------------------------------------------------------
   function Resolve_ISA
     (MADT : Byte_Array; Source : ISA_IRQ; SCI : Boolean) return Resolution
   is
      V : constant Table_View := View_Of (MADT, MADT_Signature);
      S : Scan_Result;
      Route : Line_Route;
   begin
      case V.Kind is
         when Absent    => return (Routed => False, Why => No_MADT, Seen => <>);
         when Malformed => return (Routed => False, Why => Malformed_MADT, Seen => <>);
         when Oversized => return (Routed => False, Why => Too_Large, Seen => <>);
         when Whole     => null;
      end case;
      if V.Length < MADT_Entries_Offset then
         return (Routed => False, Why => Malformed_MADT, Seen => <>);
      end if;

      Scan (MADT, V.Length, Source, S);
      if not S.Well_Formed then
         return (Routed => False, Why => Malformed_MADT, Seen => <>);
      elsif S.Overrides > 1 then
         return (Routed => False, Why => Ambiguous_Override, Seen => <>);
      end if;

      if S.Overrides = 1 then
         declare
            D : constant Flag_Decoding := Decode_Flags (S.Override_Flags, SCI);
         begin
            if not D.Defined then
               return (Routed => False, Why => Reserved_Override_Flags,
                       Seen => (GSI => S.Override_GSI, Overridden => True, others => <>));
            end if;
            Route := (GSI => S.Override_GSI, Trigger => D.Trigger,
                      Polarity => D.Polarity, Overridden => True);
         end;
      else
         --  No override: GSI = the IRQ, in its bus's conforming mode.
         Route := (GSI        => GSI_Number (Source),
                   Trigger    => (if SCI then LA.Level else LA.Edge),
                   Polarity   => (if SCI then LA.Active_Low else LA.Active_High),
                   Overridden => False);
      end if;

      if Taken_By_Other (MADT, V.Length, Route.GSI, Source) then
         return (Routed => False, Why => GSI_Overridden_Away, Seen => Route);
      elsif S.IO_APICs = 0 then
         return (Routed => False, Why => No_IO_APIC, Seen => Route);
      elsif S.First_Base /= 0 then
         return (Routed => False, Why => First_IO_APIC_Not_GSI_0, Seen => Route);
      elsif S.IO_APICs > 1 and then Route.GSI >= S.Lowest_Other then
         return (Routed => False, Why => Other_IO_APIC, Seen => Route);
      elsif Route.GSI = LA.Timer_Input then
         return (Routed => False, Why => Timer_Input, Seen => Route);
      elsif Route.GSI > LA.Last_Input then
         return (Routed => False, Why => Beyond_Legacy_Lines, Seen => Route);
      end if;
      return (Routed => True, Route => Route,
              Vector => LA.Vector_Of (LA.Line_Input (Route.GSI)));
   end Resolve_ISA;

   ---------------------------------------------------------------------------
   --  Resolve_SCI
   ---------------------------------------------------------------------------
   function Resolve_SCI (FADT, MADT : Byte_Array) return Resolution is
      F : constant SCI_Field := SCI_Of (FADT);
   begin
      if not F.Present then
         return (Routed => False, Why => F.Why, Seen => <>);
      elsif F.Source > Unsigned_16 (ISA_IRQ'Last) then
         --  No override can name it: it is the GSI itself, past the lines.
         return (Routed => False, Why => Beyond_Legacy_Lines,
                 Seen => (GSI => GSI_Number (F.Source), Trigger => LA.Level,
                          Polarity => LA.Active_Low, Overridden => False));
      end if;
      return Resolve_ISA (MADT, ISA_IRQ (F.Source), SCI => True);
   end Resolve_SCI;

end CuBit.ACPI_SCI_Route;
