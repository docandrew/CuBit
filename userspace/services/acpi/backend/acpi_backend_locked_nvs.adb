pragma Ada_2022;
package body ACPI_Backend_Locked_NVS with SPARK_Mode is
   package P renames ACPI_Locked_NVS_Protocol;
   use type Interfaces.Unsigned_64, Records.Outcome, Records.Record_ID, Records.Epoch_Number;
   package body Callbacks is
      procedure Describe_Datum
        (Scope : Scope_Tag; ID : P.Record_ID; Datum : P.Geometry;
         Epoch : out Interfaces.Unsigned_64; Accepted : out Boolean) is
         Header, Final : Records.Header_Report;
         Item : Records.Description_Report;
         Bytes : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (P.T.Octets (Datum.Width));
      begin
         Epoch := 0; Accepted := False;
         if not P.Valid (Datum) then return; end if;
         Records.Header (Record_State, Interfaces.Unsigned_64 (Scope), Header);
         if Header.Status /= Records.Ready then return; end if;
         for I in 1 .. Header.Count loop
            Records.Describe (Record_State, Interfaces.Unsigned_64 (Scope), Header.Epoch, I, Item);
            if Item.Status /= Records.Ready then return; end if;
            if Item.ID = Records.Record_ID (ID) then
               if Datum.Offset >= Item.Length or else Bytes > Item.Length - Datum.Offset then return; end if;
               Records.Header (Record_State, Interfaces.Unsigned_64 (Scope), Final);
               if Final.Status = Records.Ready and then Final.Epoch /= Records.No_Epoch
                 and then Final.Epoch = Header.Epoch and then Final.Count = Header.Count
               then Epoch := Interfaces.Unsigned_64 (Final.Epoch); Accepted := True; end if;
               return;
            end if;
         end loop;
      end Describe_Datum;
      procedure Read_Datum
        (Scope : Scope_Tag; Epoch : Interfaces.Unsigned_64; ID : P.Record_ID; Datum : P.Geometry;
         Value : out Interfaces.Unsigned_64; Completed : out Boolean) is
         Result : Records.Outcome;
      begin
         Records.Read (Record_State, Interfaces.Unsigned_64 (Scope), Records.Epoch_Number (Epoch),
           Records.Record_ID (ID), Datum.Offset, Datum.Width, Value, Result);
         Completed := Result = Records.Done;
      end Read_Datum;
   end Callbacks;
end ACPI_Backend_Locked_NVS;
