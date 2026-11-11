pragma Ada_2022;
with Interfaces;
with System;
with AML_Decode;
with ACPI_Clock_Source;
with ACPI_Fallible_Heap;
with CuBit.Firmware_Snapshot;
with Startup;
with Firmware_Tables.MCFG;
with Firmware_Tables.PCI_Windows;
with Firmware_Tables.PCI_Aliases;
package body ACPI_Native_PCI_Aliases is
   package K renames CuBit.Firmware_Snapshot;
   package F renames Firmware_Tables;
   package M renames F.MCFG;
   package W renames F.PCI_Windows;
   package A renames F.PCI_Aliases;
   use type K.Outcome, K.Table_ID, K.Byte_Count, System.Address;
   use type R.Tick, R.Outcome, A.Outcome, F.Byte, Interfaces.Unsigned_64;
   Saved : Report;
   Block : System.Address := System.Null_Address;
   Last : R.Tick := 0;
   Seen : Boolean := False;
   function Name (Why : Failure) return String is
     (case Why is
        when None => "NONE",
        when Already_Consumed => "ALREADY_CONSUMED",
        when Invalid_Deadline => "INVALID_DEADLINE",
        when Clock_Unavailable => "CLOCK_UNAVAILABLE",
        when Clock_Reversed => "CLOCK_REVERSED",
        when Deadline_Expired => "DEADLINE_EXPIRED",
        when Source_Refused => "SOURCE_REFUSED",
        when Duplicate_Table => "DUPLICATE_TABLE",
        when Invalid_Extent => "INVALID_EXTENT",
        when Allocation_Refused => "ALLOCATION_REFUSED",
        when Copy_Refused => "COPY_REFUSED",
        when Invalid_Table => "INVALID_TABLE");
   function In_Time (Deadline : R.Tick) return Boolean is
      Raw : AML_Decode.Integer_Value;
      Available : Boolean;
      Now : R.Tick;
   begin
      ACPI_Clock_Source.Read (Raw, Available); Now := R.Tick (Raw);
      if not Available then Saved.Why := Clock_Unavailable; return False;
      elsif Seen and then Now < Last then Saved.Why := Clock_Reversed; return False;
      end if;
      Seen := True; Last := Now;
      if Now >= Deadline then Saved.Why := Deadline_Expired; return False; end if;
      return True;
   end In_Time;
   procedure Validate (Good : in out Boolean; Item : M.Allocation) is
   begin Good := Good and then W.Valid (Item, F.Address_Value'Last); end Validate;
   procedure Scan is new M.Scan (Boolean, Validate);
   procedure Initialize (Deadline : R.Tick; Result : out Report) is
      Catalog : K.Catalog_Info;
      Description, Selected : K.Table_Description;
      ID : K.Table_ID := 0;
      Status : K.Outcome;
      Name : constant String := "MCFG";
      Good : Boolean := True;
      Header : M.Table_Metadata;
      procedure Attempt is
      begin
         if Deadline = R.Tick'Last then Saved.Why := Invalid_Deadline; return; end if;
         if not In_Time (Deadline) then return; end if;
         K.Info (Startup.Tables, Catalog, Status);
         if Status /= K.Ok or else not Catalog.Ready then Saved.Why := Source_Refused; return; end if;
         for Index in K.Table_ID range 1 .. K.Table_ID (Catalog.Table_Count) loop
            if not In_Time (Deadline) then return; end if;
            K.Describe (Startup.Tables, Index, Description, Status);
            if Status /= K.Ok then Saved.Why := Source_Refused; return; end if;
            if (for all J in K.Signature_Index => Description.Signature (J) = Character'Pos (Name (J))) then
               if ID /= 0 then Saved.Why := Duplicate_Table; return; end if;
               ID := Index; Selected := Description;
            end if;
         end loop;
         if not In_Time (Deadline) then return; end if;
         if ID = 0 then Saved.Stage := Absent; return; end if;
         if Selected.Extent < M.Fixed_Size or else Selected.Extent > K.Byte_Count (Natural'Last)
           or else ID > K.Table_ID (Positive'Last) then Saved.Why := Invalid_Extent; return;
         end if;
         Saved.Bytes := Natural (Selected.Extent);
         Block := ACPI_Fallible_Heap.Reserve
           (Interfaces.Unsigned_64 (Saved.Bytes), Interfaces.Unsigned_64 (F.Byte'Alignment));
         if Block = System.Null_Address then Saved.Why := Allocation_Refused; return; end if;
         declare
            Data : F.Bytes (1 .. Saved.Bytes) with Import, Address => Block;
         begin
            ACPI_Native_Snapshot_Read.Read
              ((ID => R.S.Table_ID (ID), Name => Name, Revision => F.Byte (Selected.Revision),
                Extent => Saved.Bytes), Deadline, Data, Saved.Copy);
            if Saved.Copy.Copy.Status /= R.Copied_Table then Saved.Why := Copy_Refused; return; end if;
            if not In_Time (Deadline) then return; end if;
            Scan (Data, Good, Header);
            if not Header.Valid or else Header.Revision /= 1 or else Header.Reserved /= 0
              or else Header.Count = 0 or else not Good then Saved.Why := Invalid_Table; return; end if;
            if not In_Time (Deadline) then return; end if;
            Saved.Allocations := Header.Count;
            Saved.Stage := Ready;
         end;
      end Attempt;
   begin
      if Saved.Stage /= Fresh then Result := Saved; Result.Why := Already_Consumed; return; end if;
      Saved.Stage := Preparing;
      Attempt;
      if Saved.Stage = Preparing then Saved.Stage := Failed; end if;
      if Saved.Stage /= Ready and then Block /= System.Null_Address then
         ACPI_Fallible_Heap.Release (Block); Block := System.Null_Address;
      end if;
      Result := Saved;
   end Initialize;
   procedure Resolve
     (Geometry : AML_Field_Layout.Register_Geometry;
      Key : in out ACPI_PCI_Protocol.P.Bus_Key; Accepted : in out Boolean) is
   begin
      Accepted := False;
      if Saved.Stage /= Ready or else Geometry.Region.Space /= AML_Field_Layout.Memory_Space then return; end if;
      declare
         Data : F.Bytes (1 .. Saved.Bytes) with Import, Address => Block;
         Choice : constant A.Selection := A.Resolve (Data, Geometry.Region.Base, Geometry.Region.Length);
      begin
         if Choice.Status = A.Resolved then
            Key := (ACPI_PCI_Protocol.P.Segment_Number (Choice.Segment),
                    ACPI_PCI_Protocol.P.Bus_Number (Choice.Bus));
            Accepted := True;
         end if;
      end;
   end Resolve;
end ACPI_Native_PCI_Aliases;
