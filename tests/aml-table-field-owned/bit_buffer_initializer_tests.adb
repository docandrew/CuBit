with Ada.Text_IO;
with AML_Decode;
with AML_Objects;
with AML_Objects.Reclamation;
with AML_Metadata_Kinds;
with AML_Object_Identifiers;
with AML_Identity;
with AML_Identity.Issuer;
with Firmware_Tables;
procedure Bit_Buffer_Initializer_Tests is
   package O renames AML_Objects;
   package R renames AML_Objects.Reclamation;
   use type R.Reclaim_Status;
   use type AML_Metadata_Kinds.Snapshot_Description;
   use type AML_Object_Identifiers.Slot_Incarnation;
   type Fixture_Source (First, Last : Positive) is record
      Data : Firmware_Tables.Bytes (First .. Last);
   end record;
   function Source_Length (Source : aliased Fixture_Source) return Natural is
     (Source.Data'Length) with Global => null;
   function Source_Byte (Source : aliased Fixture_Source; Index : Natural)
      return AML_Decode.Byte is (Source.Data (Source.Data'First + Index))
      with Global => null, Pre => Index < Source.Data'Length;
   package B is new O.Bit_Buffer_Initializers (Fixture_Source, Source_Length, Source_Byte);
   use type O.State;
   use type O.Allocation_Status;
   use type AML_Decode.Byte;
   use type AML_Decode.Integer_Value;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   declare
      Store : O.State;
      Source : aliased constant Fixture_Source (Positive'Last - 1, Positive'Last) :=
        (Positive'Last - 1, Positive'Last, [16#80#, 1]);
      Empty : aliased constant Fixture_Source (17, 16) := (17, 16, [others => 0]);
      ID, Empty_ID : O.Object_ID;
      Status : O.Allocation_Status;
   begin
      Check (B.Fits_Source (Source, 0, 2, 7, 2));
      B.Allocate (Store, Source, 0, 2, 7, 2, ID, Status);
      Check (Status = O.Allocated and then O.Length (Store, ID) = 1
        and then O.Stored_Byte (Store, ID, 0) = 3);
      B.Allocate (Store, Empty, 0, 0, 0, 0, Empty_ID, Status);
      Check (Status = O.Allocated and then O.Length (Store, Empty_ID) = 0);
      Check (O.Stored_Byte (Store, ID, 0) = 3);
      Check (not B.Fits_Source (Source, Natural'Last, 1, 0, 0));
      Check (not B.Fits_Source (Source, 1, Natural'Last, 0, 0));
      Check (not B.Fits_Source (Source, 0, 2, Natural'Last, 1));
   end;
   declare
      Store, Prior : O.State;
      Source : aliased constant Fixture_Source (7, 6 + O.Max_Bytes) :=
        (7, 6 + O.Max_Bytes, [others => 16#A5#]);
      ID, Sentinel, Existing : O.Object_ID;
      Status : O.Allocation_Status;
      Existing_Length : constant := 3;
      Remaining : constant := O.Max_Bytes - Existing_Length;
   begin
      O.New_Integer (Store, 16#1234#, Sentinel, Status);
      Check (Status = O.Allocated);
      O.New_Buffer (Store, Existing_Length, [16#D3#, 16#7E#, 16#19#], Existing, Status);
      Check (Status = O.Allocated);
      Prior := Store;
      B.Allocate (Store, Source, 0, Source_Length (Source), 0, O.Max_Bytes * 8, ID, Status);
      Check (Status = O.Byte_Limit and then ID = O.No_Object and then Store = Prior);
      B.Allocate (Store, Source, 0, Source_Length (Source), 0, Remaining * 8, ID, Status);
      Check (Status = O.Allocated and then O.Length (Store, ID) = Remaining);
      Check (O.Integer_Data (Store, Sentinel) = 16#1234#);
      Check (O.Stored_Byte (Store, Existing, 0) = 16#D3#
        and then O.Stored_Byte (Store, Existing, 1) = 16#7E#
        and then O.Stored_Byte (Store, Existing, 2) = 16#19#);
      Check (O.Stored_Byte (Store, ID, 0) = 16#A5#
        and then O.Stored_Byte (Store, ID, Remaining - 1) = 16#A5#);
      Prior := Store;
      B.Allocate (Store, Source, 0, Source_Length (Source), 0, 1, ID, Status);
      Check (Status = O.Byte_Limit and then ID = O.No_Object and then Store = Prior);
   end;
   declare
      Store, Prior : O.State;
      Source : aliased constant Fixture_Source (1, O.Max_Bytes) :=
        (1, O.Max_Bytes, [others => 16#A5#]);
      ID, Existing, Snapshot : O.Object_ID;
      Status : O.Allocation_Status;
      Device : constant AML_Metadata_Kinds.Snapshot_Description :=
        (Object_Type => AML_Metadata_Kinds.Device_Metadata);
   begin
      O.New_Buffer (Store, 3, [16#D3#, 16#7E#, 16#19#], Existing, Status);
      Check (Status = O.Allocated);
      O.New_Metadata_Snapshot (Store, Device, Snapshot, Status);
      Check (Status = O.Allocated);
      for I in 1 .. O.Max_Objects - 2 loop
         O.New_Buffer (Store, 0, [], ID, Status);
         Check (Status = O.Allocated);
      end loop;
      Prior := Store;
      -- Both slot and byte quotas would fail; Select_Slot has precedence.
      B.Allocate (Store, Source, 0, Source_Length (Source), 0, O.Max_Bytes * 8, ID, Status);
      Check (Status = O.Object_Limit and then ID = O.No_Object and then Store = Prior);
      B.Allocate (Store, Source, 0, Source_Length (Source), 0, 0, ID, Status);
      Check (Status = O.Object_Limit and then ID = O.No_Object and then Store = Prior);
      Check (O.Stored_Byte (Store, Existing, 0) = 16#D3#
        and then O.Stored_Byte (Store, Existing, 1) = 16#7E#
        and then O.Stored_Byte (Store, Existing, 2) = 16#19#
        and then O.Metadata_Snapshot_Data (Store, Snapshot) = Device);
   end;
   declare
      Store : O.State;
      Source : aliased constant Fixture_Source (7, 8) := (7, 8, [16#80#, 1]);
      ID, Discarded, Existing, Snapshot : O.Object_ID;
      Status : O.Allocation_Status;
      Device : constant AML_Metadata_Kinds.Snapshot_Description :=
        (Object_Type => AML_Metadata_Kinds.Device_Metadata);
      Keep : R.Keep_Set := [others => False];
      Scratch : R.Workspace;
      Reclaimed : R.Reclaim_Status;
      Owner : AML_Identity.Identity;
      Issued : Boolean;
      Stamp : AML_Object_Identifiers.Slot_Incarnation;
   begin
      AML_Identity.Issuer.Issue (Owner, Issued); Check (Issued);
      O.New_Buffer (Store, 3, [16#A6#, 16#B7#, 16#C8#], Discarded, Status);
      Check (Status = O.Allocated);
      Stamp := O.Last_Incarnation (Store, Discarded);
      O.New_Metadata_Snapshot (Store, Device, Snapshot, Status); Check (Status = O.Allocated);
      O.New_Buffer (Store, 2, [16#D3#, 16#7E#], Existing, Status); Check (Status = O.Allocated);
      Keep (Snapshot) := True; Keep (Existing) := True;
      R.Reclaim (Store, Owner, Keep, Scratch, Reclaimed);
      Check (Reclaimed = R.Reclaimed and then not O.Is_Live (Store, Discarded)
        and then O.Byte_Count (Store) = 2);
      B.Allocate (Store, Source, 0, Source_Length (Source), 7, 9, ID, Status);
      Check (Status = O.Allocated and then ID = Discarded
        and then O.Last_Incarnation (Store, ID) = Stamp + 1
        and then O.Length (Store, ID) = 2
        and then O.Stored_Byte (Store, ID, 0) = 3
        and then O.Stored_Byte (Store, ID, 1) = 0);
      Check (O.Stored_Byte (Store, Existing, 0) = 16#D3#
        and then O.Stored_Byte (Store, Existing, 1) = 16#7E#
        and then O.Metadata_Snapshot_Data (Store, Snapshot) = Device);
   end;
   declare
      Store : O.State := O.Empty (Max_Incarnation => 1);
      Prior : O.State;
      Source : aliased constant Fixture_Source (1, O.Max_Bytes) :=
        (1, O.Max_Bytes, [others => 16#5A#]);
      ID, Existing, Snapshot : O.Object_ID;
      Status : O.Allocation_Status;
      Device : constant AML_Metadata_Kinds.Snapshot_Description :=
        (Object_Type => AML_Metadata_Kinds.Device_Metadata);
      Keep : R.Keep_Set := [others => False];
      Scratch : R.Workspace;
      Reclaimed : R.Reclaim_Status;
      Owner : AML_Identity.Identity;
      Issued : Boolean;
   begin
      AML_Identity.Issuer.Issue (Owner, Issued); Check (Issued);
      O.New_Buffer (Store, 3, [16#91#, 16#82#, 16#73#], Existing, Status);
      Check (Status = O.Allocated);
      O.New_Metadata_Snapshot (Store, Device, Snapshot, Status); Check (Status = O.Allocated);
      for I in 1 .. O.Max_Objects - 2 loop
         O.New_Buffer (Store, 0, [], ID, Status);
         Check (Status = O.Allocated);
      end loop;
      Keep (Existing) := True; Keep (Snapshot) := True;
      R.Reclaim (Store, Owner, Keep, Scratch, Reclaimed);
      Check (Reclaimed = R.Reclaimed and then O.Live_Count (Store) = 2);
      Prior := Store;
      B.Allocate (Store, Source, 0, Source_Length (Source), 0, 0, ID, Status);
      Check (Status = O.Generation_Limit and then ID = O.No_Object and then Store = Prior);
      -- The exhausted incarnation supply precedes insufficient remaining bytes.
      B.Allocate (Store, Source, 0, Source_Length (Source), 0, O.Max_Bytes * 8, ID, Status);
      Check (Status = O.Generation_Limit and then ID = O.No_Object and then Store = Prior);
      Check (O.Stored_Byte (Store, Existing, 0) = 16#91#
        and then O.Stored_Byte (Store, Existing, 1) = 16#82#
        and then O.Stored_Byte (Store, Existing, 2) = 16#73#
        and then O.Metadata_Snapshot_Data (Store, Snapshot) = Device);
   end;
   Ada.Text_IO.Put_Line ("Bit buffer initializer checks" & Checks'Image);
end Bit_Buffer_Initializer_Tests;
