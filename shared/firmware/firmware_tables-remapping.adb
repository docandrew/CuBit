pragma Ada_2022;
with Interfaces; use Interfaces;

package body Firmware_Tables.Remapping with SPARK_Mode is

   Bits_Per_Byte : constant := 8;
   Byte_Width : constant := 1;
   Word_Width : constant := 2;
   QWord_Width : constant := 8;

   --  The fixed part after the SDT header.
   Host_Width_Offset : constant := Table_Header_Size;
   Flags_Offset : constant := Table_Header_Size + 1;
   Interrupt_Remapping_Flag : constant Byte := 1;
   X2APIC_Opt_Out_Flag : constant Byte := 2;
   DMA_Control_Opt_In_Flag : constant Byte := 4;

   --  Remapping structure header and types.
   Structure_Length_Offset : constant := 2;
   Unit_Type : constant Unsigned_16 := 0;
   Region_Type : constant Unsigned_16 := 1;
   ATS_Root_Type : constant Unsigned_16 := 2;
   Affinity_Type : constant Unsigned_16 := 3;
   Namespace_Type : constant Unsigned_16 := 4;
   SATC_Type : constant Unsigned_16 := 5;
   SIDP_Type : constant Unsigned_16 := 6;
   ATS_Root_Fixed_Size : constant := 8;
   Affinity_Fixed_Size : constant := 20;
   Namespace_Fixed_Size : constant := 9;
   SATC_Fixed_Size : constant := 8;
   SIDP_Fixed_Size : constant := 8;

   --  DRHD fields.
   Unit_Flags_Offset : constant := 4;
   Unit_Size_Offset : constant := 5;
   Unit_Segment_Offset : constant := 6;
   Unit_Base_Offset : constant := 8;
   Include_All_Flag : constant Byte := 1;
   Size_Field_Mask : constant Byte := 16#0F#;

   --  RMRR fields.
   Region_Segment_Offset : constant := 6;
   Region_Base_Offset : constant := 8;
   Region_Limit_Offset : constant := 16;

   --  Device scope fields and types.
   Scope_Length_Offset : constant := 1;
   Scope_Enumeration_Offset : constant := 4;
   Scope_Bus_Offset : constant := 5;
   Endpoint_Code : constant Byte := 1;
   Bridge_Code : constant Byte := 2;
   IOAPIC_Code : constant Byte := 3;
   HPET_Code : constant Byte := 4;
   Namespace_Code : constant Byte := 5;

   function U16 (Data : Bytes; Offset : Natural) return Natural is
     (Natural (Data (Data'First + Offset))
      + Natural (Data (Data'First + Offset + 1)) * 2 ** Bits_Per_Byte)
   with Pre => Data'Length >= Word_Width and then Offset <= Data'Length - Word_Width,
        Post => U16'Result <= 2 ** (2 * Bits_Per_Byte) - 1;

   function U64 (Data : Bytes; Offset : Natural) return Unsigned_64
     with Pre => Data'Length >= QWord_Width and then Offset <= Data'Length - QWord_Width
   is
      V : Unsigned_64 := 0;
   begin
      for I in 0 .. QWord_Width - 1 loop
         V := V or Shift_Left (Unsigned_64 (Data (Data'First + Offset + I)),
                               Bits_Per_Byte * I);
      end loop;
      return V;
   end U64;

   Register_Pages_Of : constant array (Register_Size_Field) of Positive :=
     [1, 2, 4, 8, 16];

   function At_Byte (Data : Bytes; Offset : Natural) return Byte is
     (Data (Data'First + Offset))
   with Pre => Offset < Data'Length;

   function Known_Scope (Code : Byte) return Boolean is
     (Code in Endpoint_Code | Bridge_Code | IOAPIC_Code | HPET_Code | Namespace_Code);

   function Kind_Of (Code : Byte) return Scope_Kind is
     (case Code is
        when Endpoint_Code => PCI_Endpoint,
        when Bridge_Code => PCI_Bridge,
        when IOAPIC_Code => IOAPIC,
        when HPET_Code => HPET,
        when others => Namespace_Device)
   with Pre => Known_Scope (Code);

   --  Scopes already stored (1 .. F.Scope_Total) and the owners are left
   --  alone; the new ones go to slots after Scope_Total, uncounted until the
   --  caller adds their owner.
   function Unchanged_Except_New_Scopes (Old, F : Facts) return Boolean is
     (F.Unit_Total = Old.Unit_Total and then F.Units = Old.Units
      and then F.Region_Total = Old.Region_Total and then F.Regions = Old.Regions
      and then F.Scope_Total = Old.Scope_Total
      and then F.Width = Old.Width and then F.Revision = Old.Revision
      and then F.Interrupt_Remapping = Old.Interrupt_Remapping
      and then F.X2APIC_Opt_Out = Old.X2APIC_Opt_Out
      and then F.DMA_Control_Opt_In = Old.DMA_Control_Opt_In
      and then (for all S in 1 .. F.Scope_Total => F.Scopes (S) = Old.Scopes (S)));

   function New_Scopes_Owned
     (F : Facts; Count : Scope_Count; Owner : Scope_Owner; Owner_Index : Positive)
     return Boolean is
     (Count <= Maximum_Scopes - F.Scope_Total
      and then (for all S in F.Scope_Total + 1 .. F.Scope_Total + Count =>
                  F.Scopes (S).Owner = Owner
                  and then F.Scopes (S).Owner_Index = Owner_Index
                  and then (if F.Scopes (S).Kind in PCI_Endpoint | PCI_Bridge
                            then F.Scopes (S).Hops >= 1)))
   with Pre => F.Scope_Total <= Maximum_Scopes;

   --  Parse the device scopes in Data [Start, Start + Size): validate every
   --  one and store the known kinds after F.Scope_Total.
   procedure Parse_Scopes
     (Data        : Bytes;
      Start, Size : Natural;
      Owner       : Scope_Owner;
      Owner_Index : Positive;
      No_PCI      : Boolean;          --  an INCLUDE_PCI_ALL unit's scopes
      F           : in out Facts;
      Count       : out Scope_Count;
      Failed      : out Boolean;
      Why         : out Rejection;
      Offset      : out Natural)
   with
     Pre  => Start <= Data'Length and then Size <= Data'Length - Start,
     Post => Unchanged_Except_New_Scopes (F'Old, F)
       and then (if not Failed then New_Scopes_Owned (F, Count, Owner, Owner_Index))
   is
      Used : Natural := 0;
   begin
      Count := 0;
      Failed := False;
      Why := Scope_Truncated;
      Offset := 0;
      while Used < Size loop
         pragma Loop_Invariant (Used < Size);
         pragma Loop_Invariant (not Failed);
         pragma Loop_Invariant (Unchanged_Except_New_Scopes (F'Loop_Entry, F));
         pragma Loop_Invariant (New_Scopes_Owned (F, Count, Owner, Owner_Index));
         pragma Loop_Variant (Increases => Used);
         declare
            Here : constant Natural := Start + Used;
         begin
            if Size - Used < Scope_Header_Size then
               Failed := True; Why := Scope_Truncated; Offset := Here;
               return;
            end if;
            declare
               Code   : constant Byte := At_Byte (Data, Here);
               Length : constant Natural :=
                 Natural (At_Byte (Data, Here + Scope_Length_Offset));
            begin
               if Length < Scope_Header_Size then
                  Failed := True; Why := Scope_Too_Short; Offset := Here;
                  return;
               elsif Length > Size - Used then
                  Failed := True; Why := Scope_Truncated; Offset := Here;
                  return;
               elsif (Length - Scope_Header_Size) mod Path_Pair_Size /= 0 then
                  Failed := True; Why := Scope_Path_Invalid; Offset := Here;
                  return;
               end if;
               if Known_Scope (Code) then
                  declare
                     Kind  : constant Scope_Kind := Kind_Of (Code);
                     Pairs : constant Natural :=
                       (Length - Scope_Header_Size) / Path_Pair_Size;
                     Item  : Scope;
                  begin
                     if Kind in PCI_Endpoint | PCI_Bridge and then Pairs = 0 then
                        Failed := True; Why := Scope_Path_Invalid; Offset := Here;
                        return;
                     elsif Kind in PCI_Endpoint | PCI_Bridge and then No_PCI then
                        Failed := True; Why := Include_All_Invalid; Offset := Here;
                        return;
                     elsif Pairs > Maximum_Path_Hops then
                        Failed := True; Why := Path_Too_Long; Offset := Here;
                        return;
                     elsif Count >= Maximum_Scopes - F.Scope_Total then
                        Failed := True; Why := Too_Many_Scopes; Offset := Here;
                        return;
                     end if;
                     Item.Kind := Kind;
                     Item.Owner := Owner;
                     Item.Owner_Index := Owner_Index;
                     Item.Enumeration_ID := At_Byte (Data, Here + Scope_Enumeration_Offset);
                     Item.Start_Bus := At_Byte (Data, Here + Scope_Bus_Offset);
                     Item.Hops := Pairs;
                     for P in 1 .. Pairs loop
                        pragma Loop_Invariant (not Failed);
                        declare
                           Pair : constant Natural :=
                             Here + Scope_Header_Size + (P - 1) * Path_Pair_Size;
                           Device : constant Byte := At_Byte (Data, Pair);
                           Func   : constant Byte := At_Byte (Data, Pair + Byte_Width);
                        begin
                           if Device > Device_Number'Last
                             or else Func > Function_Number'Last
                           then
                              Failed := True; Why := Scope_Path_Invalid; Offset := Pair;
                              return;
                           end if;
                           Item.Path (P) := (Device => Device, Func => Func);
                        end;
                     end loop;
                     Count := Count + 1;
                     F.Scopes (F.Scope_Total + Count) := Item;
                  end;
               end if;
               Used := Used + Length;
            end;
         end;
      end loop;
   end Parse_Scopes;

   procedure Decode
     (Data   : Bytes;
      F      : out Facts;
      Status : out Outcome;
      Why    : out Rejection;
      Offset : out Natural)
   is
      Header : constant Table_Result := Read_Table (Data, "DMAR");
      Cursor : Natural;

      procedure Reject (Reason : Rejection; At_Byte_Offset : Natural)
      with Post => Status = Rejected
      is
      begin
         Status := Rejected;
         Why := Reason;
         Offset := At_Byte_Offset;
      end Reject;
   begin
      F := Empty_Facts;
      Status := Rejected;
      Why := Header_Rejected;
      Offset := 0;
      if Header.Status /= Accepted or else Header.Extent /= Data'Length then
         Reject (Header_Rejected, 0);
            return;
      elsif Data'Length < Fixed_Size then
         Reject (Fixed_Part_Truncated, Data'Length);
            return;
      end if;
      declare
         Width : constant Natural := Natural (At_Byte (Data, Host_Width_Offset)) + 1;
         Flags : constant Byte := At_Byte (Data, Flags_Offset);
      begin
         if Width not in Host_Width then
            Reject (Host_Width_Invalid, Host_Width_Offset);
            return;
         end if;
         F.Width := Width;
         F.Revision := Header.Revision;
         F.Interrupt_Remapping := (Flags and Interrupt_Remapping_Flag) /= 0;
         F.X2APIC_Opt_Out := (Flags and X2APIC_Opt_Out_Flag) /= 0;
         F.DMA_Control_Opt_In := (Flags and DMA_Control_Opt_In_Flag) /= 0;
      end;
      pragma Assert (Well_Formed (F));

      Cursor := Fixed_Size;
      while Cursor < Data'Length loop
         pragma Loop_Invariant (Cursor < Data'Length);
         pragma Loop_Invariant (Well_Formed (F));
         pragma Loop_Variant (Increases => Cursor);
         if Data'Length - Cursor < Structure_Header_Size then
            Reject (Structure_Truncated, Cursor);
            return;
         end if;
         declare
            Kind   : constant Unsigned_16 :=
              Unsigned_16 (U16 (Data, Cursor));
            Length : constant Natural :=
              U16 (Data, Cursor + Structure_Length_Offset);
            Minimum : constant Natural :=
              (case Kind is
                 when Unit_Type => Unit_Fixed_Size,
                 when Region_Type => Region_Fixed_Size,
                 when ATS_Root_Type => ATS_Root_Fixed_Size,
                 when Affinity_Type => Affinity_Fixed_Size,
                 when Namespace_Type => Namespace_Fixed_Size,
                 when SATC_Type => SATC_Fixed_Size,
                 when SIDP_Type => SIDP_Fixed_Size,
                 when others => Structure_Header_Size);
            Count  : Scope_Count;
            Failed : Boolean;
            Scope_Why    : Rejection;
            Scope_Offset : Natural;
         begin
            if Length > Data'Length - Cursor then
               Reject (Structure_Truncated, Cursor);
            return;
            elsif Length < Minimum then
               Reject (Structure_Too_Short, Cursor);
            return;
            end if;

            if Kind = Unit_Type then
               declare
                  Unit_Flags : constant Byte := At_Byte (Data, Cursor + Unit_Flags_Offset);
                  Size_Field : constant Natural :=
                    Natural (At_Byte (Data, Cursor + Unit_Size_Offset) and Size_Field_Mask);
                  Segment : constant Segment_Number :=
                    Segment_Number (U16 (Data, Cursor + Unit_Segment_Offset));
                  Base : constant Address_Value :=
                    U64 (Data, Cursor + Unit_Base_Offset);
                  Include_All : constant Boolean := (Unit_Flags and Include_All_Flag) /= 0;
                  Pages : Positive;
               begin
                  if Size_Field > Maximum_Register_Size_Field then
                     Reject (Unit_Size_Unsupported, Cursor + Unit_Size_Offset);
            return;
                  end if;
                  Pages := Register_Pages_Of (Size_Field);
                  if Base = 0 or else Base mod Page_Bytes /= 0
                    or else Base > Address_Value'Last - Address_Value (Pages) * Page_Bytes + 1
                  then
                     Reject (Unit_Base_Invalid, Cursor + Unit_Base_Offset);
            return;
                  elsif F.Unit_Total = Maximum_Units then
                     Reject (Too_Many_Units, Cursor);
            return;
                  end if;
                  if Include_All then
                     for U in 1 .. F.Unit_Total loop
                        if F.Units (U).Include_All and then F.Units (U).Segment = Segment then
                           Reject (Include_All_Invalid, Cursor);
            return;
                        end if;
                     end loop;
                  end if;
                  Parse_Scopes (Data, Cursor + Unit_Fixed_Size, Length - Unit_Fixed_Size,
                                Unit_Scope, F.Unit_Total + 1, Include_All,
                                F, Count, Failed, Scope_Why, Scope_Offset);
                  if Failed then
                     Reject (Scope_Why, Scope_Offset);
            return;
                  end if;
                  F.Units (F.Unit_Total + 1) :=
                    (Segment => Segment, Register_Base => Base,
                     Register_Pages => Pages, Include_All => Include_All,
                     First_Scope => F.Scope_Total + 1, Scopes => Count);
                  F.Unit_Total := F.Unit_Total + 1;
                  F.Scope_Total := F.Scope_Total + Count;
               end;
            elsif Kind = Region_Type then
               declare
                  Segment : constant Segment_Number :=
                    Segment_Number (U16 (Data, Cursor + Region_Segment_Offset));
                  Base : constant Address_Value :=
                    U64 (Data, Cursor + Region_Base_Offset);
                  Limit : constant Address_Value :=
                    U64 (Data, Cursor + Region_Limit_Offset);
               begin
                  if Base mod Page_Bytes /= 0 or else Limit mod Page_Bytes /= Page_Bytes - 1
                    or else Base > Limit
                  then
                     Reject (Region_Range_Invalid, Cursor + Region_Base_Offset);
            return;
                  elsif F.Region_Total = Maximum_Regions then
                     Reject (Too_Many_Regions, Cursor);
            return;
                  end if;
                  Parse_Scopes (Data, Cursor + Region_Fixed_Size, Length - Region_Fixed_Size,
                                Region_Scope, F.Region_Total + 1, False,
                                F, Count, Failed, Scope_Why, Scope_Offset);
                  if Failed then
                     Reject (Scope_Why, Scope_Offset);
            return;
                  elsif Count = 0 then
                     Reject (Region_Without_Scope, Cursor);
            return;
                  end if;
                  F.Regions (F.Region_Total + 1) :=
                    (Segment => Segment, Base => Base, Limit => Limit,
                     First_Scope => F.Scope_Total + 1, Scopes => Count);
                  F.Region_Total := F.Region_Total + 1;
                  F.Scope_Total := F.Scope_Total + Count;
               end;
            end if;
            --  Other known structures (ATSR, RHSA, ANDD, SATC, SIDP) and
            --  reserved types carry nothing the kernel uses: their length
            --  was checked, their content is skipped.
            pragma Assert (Well_Formed (F));
            Cursor := Cursor + Length;
            exit when Cursor = Data'Length;
         end;
      end loop;

      pragma Assert (Well_Formed (F));
      if F.Unit_Total = 0 then
         Reject (No_Units, Data'Length);
            return;
      end if;
      Status := Accepted;
   end Decode;
end Firmware_Tables.Remapping;
