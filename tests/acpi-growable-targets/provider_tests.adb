pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with ACPI_Capacity_Diagnostics;
with ACPI_Field_NVS_Provider;
with ACPI_NVS_Protocol;
with ACPI_Target_Heap_Test_Control;
with AML_Field_Access;
with AML_Field_Layout;
procedure Provider_Tests is
   package P renames ACPI_NVS_Protocol;
   package W renames P.Wire;
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   package D renames ACPI_Capacity_Diagnostics;
   type Transport is null record;
   type Peer is new Interfaces.Unsigned_64;
   Destination : constant Peer := 16#8000_0000_0000_1234#;
   Last : D.Report;
   Checks, Failures : Natural := 0;
   use type W.Time_Instant, A.Target, L.Region_Geometry;
   use type D.Failure_Kind, D.Cache_Kind, D.Entry_Count, D.Byte_Count;
   use type Interfaces.Unsigned_64, Interfaces.Unsigned_32;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
   procedure Clock (Now : out W.Time_Instant; Available : out Boolean) is
   begin Now := 1; Available := True; end Clock;
   procedure Deadline (Until_Time : out W.Time_Instant) is
   begin Until_Time := 1_000; end Deadline;
   procedure Exchange
     (Channel : in out Transport; Target : Peer; Until_Time : W.Time_Instant;
      Item : P.Packet; Reply : out P.Packet; Delivered : out W.Delivery) is
      pragma Unreferenced (Channel);
   begin
      Check (Target = Destination and then Until_Time = 1_000);
      Delivered := W.Reply_Received;
      if Item.Label = P.Header_Operation then Reply := P.Header_Result (1, 1);
      elsif Item.Label = P.Describe_Operation then Reply := P.Describe_Result (1, 16#1000#, 16#4000#);
      else raise Program_Error with "metadata admission performed IO"; end if;
   end Exchange;
   procedure Record_Failure (Item : D.Report) is
   begin Last := Item; Failures := Failures + 1; end Record_Failure;
   package Memory is new ACPI_Field_NVS_Provider
     (Transport, Peer, 0, Clock, Exchange, Deadline,
      Initial_Target_Capacity => 2, Record_Capacity_Failure => Record_Failure);
   IO, Other : Memory.Context;
   Saved : array (Positive range 1 .. 129) of A.Target;
   Geometry : L.Register_Geometry := ((L.Memory_Space, 16#1000#, 16#4000#), 0, P.T.Byte_Access);
   Target : A.Target;
   Described : L.Register_Geometry;
   Good : Boolean;
   Before : Natural;
   procedure Refuse (Item : L.Register_Geometry) is
      Was : constant Natural := ACPI_Target_Heap_Test_Control.Attempts;
   begin
      Memory.Install (IO, Item, Target, Good);
      Check (not Good and then Target = A.No_Target);
      Check (ACPI_Target_Heap_Test_Control.Attempts = Was);
   end Refuse;
begin
   Memory.Initialize (IO, Destination, 1, 1_000, Good); Check (Good);
   Memory.Initialize (Other, Destination, 1, 1_000, Good); Check (Good);
   for I in Saved'Range loop
      Geometry.Byte_Offset := Interfaces.Unsigned_64 (I - 1);
      Memory.Install (IO, Geometry, Saved (I), Good); Check (Good);
      Memory.Authorize (IO, Saved (I), False, Good); Check (Good);
      Memory.Authorize (IO, Saved (I), True, Good); Check (not Good);
      if I in 1 | 3 | 5 | 33 | 65 | 129 then
         for J in 1 .. I loop
            Memory.Describe (IO, Saved (J), Described, Good);
            Check (Good and then Described.Region = Geometry.Region
              and then Described.Byte_Offset = Interfaces.Unsigned_64 (J - 1));
         end loop;
      end if;
   end loop;
   Before := ACPI_Target_Heap_Test_Control.Attempts;
   for I in Saved'Range loop
      Geometry.Byte_Offset := Interfaces.Unsigned_64 (I - 1);
      Memory.Install (IO, Geometry, Target, Good);
      Check (Good and then Target = Saved (I));
      Memory.Authorize (Other, Saved (I), False, Good); Check (not Good);
   end loop;
   Check (ACPI_Target_Heap_Test_Control.Attempts = Before);
   Refuse (((L.Memory_Space, 16#5000#, 1), 0, P.T.Byte_Access));
   Refuse (((L.Memory_Space, 16#4FFF#, 2), 0, P.T.Byte_Access));
   Refuse (((L.Memory_Space, Interfaces.Unsigned_64'Last, 2), 0, P.T.Byte_Access));
   Refuse (((L.IO_Space, 16#1000#, 16#4000#), 0, P.T.Byte_Access));
   Refuse (((L.Memory_Space, 16#1000#, 0), 0, P.T.Byte_Access));
   Refuse (((L.Memory_Space, 16#1000#, 16#4000#), 16#4000#, P.T.Byte_Access));
   Refuse (((L.Memory_Space, 16#1000#, 16#4000#), 1, P.T.Word_Access));
   Refuse (((L.Memory_Space, 16#1000#, 16#4000#), 16#3FFF#, P.T.Word_Access));
   Check (Failures = 0);
   ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := 0;
   for I in 130 .. 257 loop
      Geometry.Byte_Offset := Interfaces.Unsigned_64 (I - 1);
      Memory.Install (IO, Geometry, Target, Good);
      Check (Good = (I < 257));
      Check (Good or else Target = A.No_Target);
   end loop;
   Check (Failures = 1 and then Last.Cache = D.NVS_Targets and then Last.Failure = D.Process_Heap);
   Check (Last.Demand = 257 and then Last.Capacity = 256 and then Last.Requested_Bytes > 0);
   for I in Saved'Range loop
      Memory.Describe (IO, Saved (I), Described, Good);
      Check (Good and then Described.Byte_Offset = Interfaces.Unsigned_64 (I - 1));
      Geometry.Byte_Offset := Interfaces.Unsigned_64 (I - 1);
      Memory.Install (IO, Geometry, Target, Good); Check (Good and then Target = Saved (I));
   end loop;
   Check (Failures = 1);
   Ada.Text_IO.Put_Line ("PASS NVS provider checks=" & Checks'Image & " " & D.Format (Last));
end Provider_Tests;
