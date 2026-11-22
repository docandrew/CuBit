pragma Ada_2022;
package body AML_Copied_Tables with SPARK_Mode is
   use type F.Admission;
   use type Interfaces.Unsigned_32;
   Header_Length_Offset : constant := 4;
   Encoded_Length_Bytes : constant := 4;
   Bits_Per_Byte : constant := 8;
   function Model (Store : State) return Store_Model is
     (Table_Capacity => Store.Table_Capacity, Byte_Capacity => Store.Byte_Capacity,
      Tables => Store.Tables, Data => Store.Data, Stored => Store.Stored, Used => Store.Used);
   function Valid (Store : State) return Boolean is
     (Store.Stored <= Store.Table_Capacity and then Store.Used <= Store.Byte_Capacity
      and then (if Store.Stored = 0 then Store.Used = 0)
      and then (for all I in 1 .. Store.Stored =>
        Store.Tables (I).Extent >= F.Table_Header_Size
        and then Store.Tables (I).Offset <= Store.Used
        and then Store.Tables (I).Extent <= Store.Used - Store.Tables (I).Offset
        and then (if I = 1 then Store.Tables (I).Offset = 0 else
          Store.Tables (I - 1).Offset <= Store.Used
          and then Store.Tables (I - 1).Extent <= Store.Used - Store.Tables (I - 1).Offset
          and then Store.Tables (I).Offset = Store.Tables (I - 1).Offset + Store.Tables (I - 1).Extent))
      and then (if Store.Stored > 0 then
        Store.Tables (Store.Stored).Offset + Store.Tables (Store.Stored).Extent = Store.Used));
   function Byte_At (Store : State; ID : Table_ID; Offset : Natural) return F.Byte is
     (Store.Data (Store.Tables (Positive (ID)).Offset + 1 + Offset));
   procedure Retain
     (Store : in out State; Source : F.Bytes; Fuel : Natural; Result : out Report) is
      Name : F.Signature;
      Header : F.Table_Result;
      Allowed, Equal : Boolean;
      procedure Spend (Amount : Natural)
        with Pre => Result.Charged <= Fuel,
          Post => Allowed = (Amount <= Fuel - Result.Charged'Old)
            and then Result.Charged = (if Allowed then Result.Charged'Old + Amount else Fuel)
            and then Result.ID = Result.ID'Old
            and then Result.Raw_Length = Result.Raw_Length'Old
            and then Result.Extent = Result.Extent'Old
            and then Result.Admission = Result.Admission'Old
            and then Result.Status = (if Allowed then Result.Status'Old else Budget_Exceeded)
      is
      begin
         Allowed := Amount <= Fuel - Result.Charged;
         if Allowed then Result.Charged := Result.Charged + Amount;
         else Result.Charged := Fuel; Result.Status := Budget_Exceeded;
         end if;
      end Spend;
   begin
      Result := (Status => Invalid_Table, others => <>);
      if Source'Length < F.Table_Header_Size then return; end if;
      Spend (F.Table_Header_Size); if not Allowed then return; end if;
      for I in Name'Range loop Name (I) := Character'Val (Source (Source'First + (I - 1))); end loop;
      for I in 0 .. Encoded_Length_Bytes - 1 loop
         Result.Raw_Length := Result.Raw_Length or Interfaces.Shift_Left
           (Interfaces.Unsigned_32 (Source (Source'First + Header_Length_Offset + I)), Bits_Per_Byte * I);
      end loop;
      if Name = "FACS" then Result.Status := Non_SDT; return; end if;
      if Result.Raw_Length < F.Table_Header_Size then Result.Admission := F.Invalid_Length; return; end if;
      if Long_Long_Integer (Result.Raw_Length) > Long_Long_Integer (Source'Length) then return; end if;
      if Long_Long_Integer (Result.Raw_Length) > Long_Long_Integer (Natural'Last) then return; end if;
      Result.Extent := Natural (Long_Long_Integer (Result.Raw_Length));
      Spend (Result.Extent); if not Allowed then return; end if;
      Header := F.Read_Table (Source (Source'First .. Source'First + (Result.Extent - 1)), Name);
      Result.Admission := Header.Status;
      if Header.Status /= F.Accepted or else Header.Extent /= Result.Extent then return; end if;
      for I in 1 .. Store.Stored loop
         pragma Loop_Invariant (Valid (Store));
         pragma Loop_Invariant (Result.Charged <= Fuel);
         pragma Loop_Invariant (Result.Status = Invalid_Table and then Result.ID = No_Table);
         pragma Loop_Invariant (Result.Extent in F.Table_Header_Size .. Source'Length);
         Spend (1); if not Allowed then return; end if;
         if Store.Tables (I).Extent = Result.Extent then
            Spend (Result.Extent); if not Allowed then return; end if;
            Equal := True;
            for J in 1 .. Result.Extent loop
               if Byte_At (Store, Table_ID (I), J - 1) /= Source (Source'First + (J - 1)) then
                  Equal := False; exit;
               end if;
            end loop;
            if Equal then Result.Status := Already_Retained; Result.ID := Table_ID (I); return; end if;
         end if;
      end loop;
      if Store.Stored = Store.Table_Capacity then Result.Status := Table_Limit; return; end if;
      if Result.Extent > Store.Byte_Capacity - Store.Used then Result.Status := Byte_Limit; return; end if;
      Spend (Result.Extent); if not Allowed then return; end if;
      for I in 1 .. Result.Extent loop
         Store.Data (Store.Used + I) := Source (Source'First + (I - 1));
         pragma Loop_Invariant (Valid (Store));
         pragma Loop_Invariant (Store.Stored = Store.Stored'Loop_Entry);
         pragma Loop_Invariant (Store.Used = Store.Used'Loop_Entry);
         pragma Loop_Invariant (Store.Tables = Store.Tables'Loop_Entry);
         pragma Loop_Invariant (Result.Extent in F.Table_Header_Size .. Source'Length);
         pragma Loop_Invariant (Result.Charged <= Fuel);
      end loop;
      declare
         Next : constant Positive := Store.Stored + 1;
         Prior_Used : constant Natural := Store.Used;
         Copied_Extent : constant Natural := Result.Extent;
      begin
         Store.Tables (Next) := (Prior_Used, Copied_Extent);
         Store.Used := Prior_Used + Copied_Extent;
         Store.Stored := Next;
         Result.ID := Table_ID (Next);
         pragma Assert (Store.Tables (Next).Extent = Copied_Extent);
         Result.Status := Added;
      end;
   end Retain;
   procedure Copy_Window
     (Store : State; ID : Table_ID; Offset : Natural; Data : out F.Bytes; Copied : out Boolean) is
   begin
      Data := [others => 0]; Copied := False;
      if not Present (Store, ID) then return; end if;
      if Offset > Length (Store, ID) or else Data'Length > Length (Store, ID) - Offset then return; end if;
      for I in Data'Range loop
         Data (I) := Byte_At (Store, ID, Offset + (I - Data'First));
         pragma Loop_Invariant
           (for all J in Data'First .. I =>
             Data (J) = Byte_At (Store, ID, Offset + (J - Data'First)));
      end loop;
      Copied := True;
   end Copy_Window;
end AML_Copied_Tables;
