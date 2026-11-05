pragma Ada_2022;
with Interfaces;
with AML_Field_Data;
package body AML_Table_Readers with SPARK_Mode is
   function Describe
     (Boot : aliased AML_Table_Backing.State; Copies : AML_Copied_Tables.State;
      Item : Sources.Source) return Table_Info is
      Limit : constant Sources.Quotas :=
        (Boot => Boot.Table_Capacity, Copied => Copies.Table_Capacity);
      Count : constant Sources.Table_Counts :=
        (Boot => Boot.Count, Copied => AML_Copied_Tables.Count (Copies));
   begin
      if not Sources.Fits (Limit) or else Boot.Count > Boot.Table_Capacity
        or else not AML_Copied_Tables.Valid (Copies)
      then return (Status => Invalid_Inventory); end if;
      if not Sources.Present (Limit, Count, Item) then return (Status => Source_Missing); end if;
      case Item.Kind is
         when Sources.No_Source => return (Status => Source_Missing);
         when Sources.Boot_Source =>
            if not AML_Table_Backing.Valid_Span (Boot, Positive (Item.Boot)) then
               return (Status => Invalid_Inventory);
            end if;
            return (Readable, Boot.Tables (Positive (Item.Boot)).Extent);
         when Sources.Copied_Source =>
            return (Readable, AML_Copied_Tables.Length (Copies,
              AML_Copied_Tables.Table_ID (Item.Copied)));
      end case;
   end Describe;
   function Find_Table
     (Boot : aliased AML_Table_Backing.State; Copies : AML_Copied_Tables.State;
      Requested : Firmware_Tables.Identifiers.Selection) return Selection_Result
   is
      Limit : constant Sources.Quotas :=
        (Boot => Boot.Table_Capacity, Copied => Copies.Table_Capacity);
      function Probe (Item : Sources.Source) return Selection_Result
        with Post => (if Probe'Result.Status = Table_Selected then
          Probe'Result.Item.Kind /= Sources.No_Source
          and then Describe (Boot, Copies, Probe'Result.Item).Status = Readable
          and then Describe (Boot, Copies, Probe'Result.Item).Extent = Probe'Result.Extent)
      is
         Info : constant Table_Info := Describe (Boot, Copies, Item);
         Header : F.Bytes (1 .. F.Table_Header_Size);
         Status : Read_Status;
      begin
         if Info.Status /= Readable then
            return (Status => Invalid_Selection_Inventory);
         end if;
         Read_Window (Boot, Copies, Item, 0, Header, Status);
         if Status /= Readable then return (Status => Invalid_Selection_Inventory); end if;
         if Firmware_Tables.Identifiers.Matches
           (Firmware_Tables.Identifiers.Read_Identity (Header), Requested)
         then return (Table_Selected, Item, Info.Extent); end if;
         return (Status => No_Matching_Table);
      end Probe;
   begin
      if not Sources.Fits (Limit) or else Boot.Count > Boot.Table_Capacity
        or else not AML_Copied_Tables.Valid (Copies)
      then return (Status => Invalid_Selection_Inventory); end if;
      for I in 1 .. Boot.Count loop
         if not AML_Table_Backing.Valid_Span (Boot, I) then
            return (Status => Invalid_Selection_Inventory);
         end if;
      end loop;
      for I in 1 .. Boot.Count loop
         declare
            Result : constant Selection_Result := Probe
              ((Sources.Boot_Source, Sources.Boot_Table_ID (I)));
         begin
            if Result.Status /= No_Matching_Table then return Result; end if;
         end;
      end loop;
      for I in 1 .. AML_Copied_Tables.Count (Copies) loop
         declare
            Result : constant Selection_Result := Probe
              ((Sources.Copied_Source, Sources.Copied_Table_ID (I)));
         begin
            if Result.Status /= No_Matching_Table then return Result; end if;
         end;
      end loop;
      return (Status => No_Matching_Table);
   end Find_Table;
   procedure Read_Window
     (Boot : aliased AML_Table_Backing.State; Copies : AML_Copied_Tables.State;
      Item : Sources.Source; Offset : Natural; Data : out F.Bytes;
      Status : out Read_Status) is
      Info : constant Table_Info := Describe (Boot, Copies, Item);
      Copied : Boolean;
   begin
      Data := [others => 0];
      Status := Info.Status;
      if Info.Status /= Readable then return; end if;
      if Offset > Info.Extent or else Data'Length > Info.Extent - Offset then
         Status := Invalid_Window; return;
      end if;
      case Item.Kind is
         when Sources.No_Source => Status := Source_Missing;
         when Sources.Boot_Source =>
            for I in Data'Range loop
               Data (I) := Boot.Data.Bytes
                 (Boot.Tables (Positive (Item.Boot)).Offset + Offset + (I - Data'First) + 1);
            end loop;
            Status := Readable;
         when Sources.Copied_Source =>
            AML_Copied_Tables.Copy_Window (Copies,
              AML_Copied_Tables.Table_ID (Item.Copied), Offset, Data, Copied);
            if Copied then Status := Readable;
            else Data := [others => 0]; Status := Invalid_Inventory;
            end if;
      end case;
   end Read_Window;
   function Valid_Field
     (Boot : aliased AML_Table_Backing.State; Copies : AML_Copied_Tables.State;
      View : Field_View) return Boolean
   is
      Info : constant Table_Info := Describe (Boot, Copies, View.Item);
   begin
      return Info.Status = Readable and then Info.Extent = View.Extent
        and then AML_Field_Data.Fits (View.Extent, View.Offset, View.Bits);
   end Valid_Field;
   procedure Read_Field_Byte
     (Boot : aliased AML_Table_Backing.State; Copies : AML_Copied_Tables.State;
      View : Field_View; Index : Natural; Value : out F.Byte;
      Status : out Read_Status)
   is
      Info : constant Table_Info := Describe (Boot, Copies, View.Item);
   begin
      Value := 0; Status := Info.Status;
      if Info.Status /= Readable then return; end if;
      Status := Invalid_Window;
      if Info.Extent /= View.Extent
        or else not AML_Field_Data.Fits (View.Extent, View.Offset, View.Bits)
        or else Index >= Field_Length (View)
      then return; end if;
      declare
         First : constant Natural := View.Offset / Bits_Per_Octet + Index;
         Shift : constant Natural := View.Offset mod Bits_Per_Octet;
         Available : constant Positive :=
           (if Index = Field_Length (View) - 1
            then (View.Bits - 1) mod Bits_Per_Octet + 1 else Bits_Per_Octet);
         Count : constant Positive :=
           (if Shift /= 0 and then Available > Bits_Per_Octet - Shift then 2 else 1);
         Data : F.Bytes (1 .. Count);
      begin
         Read_Window (Boot, Copies, View.Item, First, Data, Status);
         if Status /= Readable then return; end if;
         Value := Interfaces.Shift_Right (Data (1), Shift);
         if Count = 2 then
            Value := Value or Interfaces.Shift_Left (Data (2), Bits_Per_Octet - Shift);
         end if;
         if Available < Bits_Per_Octet then
            Value := Value and Interfaces.Shift_Right (F.Byte'Last, Bits_Per_Octet - Available);
         end if;
      end;
   end Read_Field_Byte;
end AML_Table_Readers;
