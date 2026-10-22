with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Objects;
with AML_Objects.Reclamation;
with AML_Metadata_Kinds;
with AML_Object_Identifiers;
with AML_Identity;
with AML_Identity.Issuer;
procedure Buffer_Allocation_Tests is
   package O renames AML_Objects;
   package R renames AML_Objects.Reclamation;
   use type O.State;
   use type O.Allocation_Status;
   use type O.Object_Kind;
   use type R.Reclaim_Status;
   use type AML_Metadata_Kinds.Snapshot_Description;
   use type Integer_Value;
   use type Byte;
   use type AML_Object_Identifiers.Slot_Incarnation;
   Store, Prior : O.State;
   ID, Snapshot, Existing, Reused : O.Object_ID;
   Prior_Stamp : AML_Object_Identifiers.Slot_Incarnation;
   Keep : R.Keep_Set := [others => False];
   Status : O.Allocation_Status;
   Scratch : R.Workspace;
   Reclaimed : R.Reclaim_Status;
   Owner : AML_Identity.Identity;
   Issued : Boolean;
   Checks : Natural := 0;
   Empty_Data : constant Bytes (1 .. 0) := [others => 0];
   Shifted_Empty : constant Bytes (7 .. 6) := [others => 0];
   Initializer : constant Bytes (7 .. 9) := [16#A5#, 16#5A#, 16#D3#];
   Large : constant Bytes (1 .. 1025) := [others => 16#7E#];
   Overlong : constant Bytes (1 .. O.Max_Bytes + 1) := [others => 16#CC#];
   Device : constant AML_Metadata_Kinds.Snapshot_Description :=
     (Object_Type => AML_Metadata_Kinds.Device_Metadata);
   procedure Check (Good : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Message; end if;
   end Check;
   procedure Reject (Count : Integer_Value; Data : Bytes; Expected : O.Allocation_Status) is
   begin
      Prior := Store;
      O.New_Buffer (Store, Count, Data, ID, Status);
      Check (Status = Expected and then ID = O.No_Object and then Store = Prior,
        "failure leaves every object/backing byte/incarnation unchanged");
   end Reject;
   procedure Allocate (Count : Integer_Value; Data : Bytes) is
      Size : constant Natural := Natural (Integer_Value'Max (Count, Integer_Value (Data'Length)));
      Bytes_Correct : Boolean := True;
   begin
      O.New_Buffer (Store, Count, Data, ID, Status);
      Check (Status = O.Allocated and then O.Is_Live (Store, ID)
        and then O.Kind (Store, ID) = O.Buffer_Object and then O.Length (Store, ID) = Size,
        "buffer allocation dimensions");
      for I in 1 .. Size loop
         Bytes_Correct := Bytes_Correct and then O.Stored_Byte (Store, ID, I - 1) =
           (if I <= Data'Length then Data (Data'First + I - 1) else 0);
      end loop;
      Check (Bytes_Correct, "initializer followed by zero-filled tail");
   end Allocate;
   Lengths : constant array (Positive range <>) of Integer_Value := [0, 1, 1024, 1025, 8192, O.Max_Bytes];
begin
   for Count of Lengths loop
      Store := O.Empty; Allocate (Count, Empty_Data);
   end loop;
   Store := O.Empty; Allocate (0, Initializer);
   Store := O.Empty; Allocate (3, Initializer);
   Store := O.Empty; Allocate (8192, Initializer);
   Store := O.Empty; Allocate (0, Large);
   Store := O.Empty;
   O.New_Metadata_Snapshot (Store, Device, Snapshot, Status);
   Check (Status = O.Allocated, "live captured metadata sentinel");
   Reject (O.Max_Bytes + 1, Empty_Data, O.Byte_Limit);
   Reject (0, Overlong, O.Byte_Limit);
   Reject (16#1_0000_0000#, Empty_Data, O.Byte_Limit);
   Reject (Integer_Value'Last, Empty_Data, O.Byte_Limit);
   Check (O.Metadata_Snapshot_Data (Store, Snapshot) = Device, "metadata snapshot unaffected by rejected buffer");
   Allocate (O.Max_Bytes, Initializer);
   Reject (1, Empty_Data, O.Byte_Limit);
   Reject (0, Initializer, O.Byte_Limit);
   Allocate (0, Empty_Data);
   Store := O.Empty;
   Allocate (0, Shifted_Empty);
   Allocate (1, Shifted_Empty);
   Store := O.Empty;
   Allocate (3, Initializer); Existing := ID;
   O.New_Metadata_Snapshot (Store, Device, Snapshot, Status);
   Check (Status = O.Allocated, "metadata beside preexisting buffer");
   Allocate (5, [16#19#, 16#E7#]);
   Check (O.Length (Store, Existing) = Initializer'Length
     and then O.Stored_Byte (Store, Existing, 0) = 16#A5#
     and then O.Stored_Byte (Store, Existing, 1) = 16#5A#
     and then O.Stored_Byte (Store, Existing, 2) = 16#D3#
     and then O.Metadata_Snapshot_Data (Store, Snapshot) = Device,
     "successful nonempty append preserves old buffer and metadata");
   Reject (Integer_Value (O.Max_Bytes - O.Byte_Count (Store) + 1), Empty_Data, O.Byte_Limit);
   Allocate (Integer_Value (O.Max_Bytes - O.Byte_Count (Store)), [16#B6#]);
   Check (O.Byte_Count (Store) = O.Max_Bytes
     and then O.Stored_Byte (Store, Existing, 0) = 16#A5#
     and then O.Stored_Byte (Store, Existing, 1) = 16#5A#
     and then O.Stored_Byte (Store, Existing, 2) = 16#D3#
     and then O.Metadata_Snapshot_Data (Store, Snapshot) = Device,
     "exact remaining capacity succeeds without altering earlier objects");
   Reject (1, Empty_Data, O.Byte_Limit);
   Store := O.Empty;
   for I in 1 .. O.Max_Objects loop
      O.New_Buffer (Store, 0, Empty_Data, ID, Status);
      Check (Status = O.Allocated, "fill object quota with empty buffers");
   end loop;
   Reject (Integer_Value'Last, Empty_Data, O.Object_Limit);
   AML_Identity.Issuer.Issue (Owner, Issued); Check (Issued, "owner for reclamation");
   Store := O.Empty;
   Allocate (3, Initializer); Existing := ID;
   O.New_Metadata_Snapshot (Store, Device, Snapshot, Status);
   Check (Status = O.Allocated, "metadata retained across reusable-slot reclamation");
   Prior_Stamp := O.Last_Incarnation (Store, Existing);
   Keep := [others => False]; Keep (Snapshot) := True;
   R.Reclaim (Store, Owner, Keep, Scratch, Reclaimed);
   Check (Reclaimed = R.Reclaimed and then not O.Is_Live (Store, Existing)
     and then O.Byte_Count (Store) = 0, "buffer backing reclaimed while metadata survives");
   Allocate (4, [16#42#]); Reused := ID;
   Check (Reused = Existing and then O.Last_Incarnation (Store, Reused) = Prior_Stamp + 1
     and then O.Metadata_Snapshot_Data (Store, Snapshot) = Device,
     "reclaimed slot reused with incremented incarnation and preserved metadata");
   Store := O.Empty (Max_Incarnation => 1);
   for I in 1 .. O.Max_Objects loop
      O.New_Buffer (Store, 0, Empty_Data, ID, Status);
      Check (Status = O.Allocated, "consume slot incarnation");
   end loop;
   R.Reclaim (Store, Owner, [others => False], Scratch, Reclaimed);
   Check (Reclaimed = R.Reclaimed and then O.Live_Count (Store) = 0, "retire exhausted slots");
   Reject (0, Empty_Data, O.Generation_Limit);
   Reject (Integer_Value'Last, Empty_Data, O.Generation_Limit);
   Ada.Text_IO.Put_Line ("Buffer allocation checks" & Checks'Image);
end Buffer_Allocation_Tests;
