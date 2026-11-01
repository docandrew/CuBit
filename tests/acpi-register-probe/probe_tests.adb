pragma Ada_2022;
with Interfaces;
with Ada.Text_IO;
with ACPI_Timer_Probe;
with ACPI_Timer_Probe_Types;
with ACPI_Inventory_Protocol;
with ACPI_Region_Protocol;
with ACPI_Backend_Register_IDs;
procedure Probe_Tests is
   use Interfaces;
   use ACPI_Timer_Probe_Types;
   package P renames ACPI_Inventory_Protocol;
   package W renames ACPI_Region_Protocol;
   use type W.Time_Instant;
   use type W.Register_ID;
   use type W.Epoch_Number;
   Checks : Natural := 0;
   procedure Check (B : Boolean) is
   begin
      Checks := Checks + 1;
      if not B then raise Program_Error with "check" & Checks'Image; end if;
   end Check;
   type Mode is
     (Changing, Wrapping, Upper_Noise, Frozen, Incomplete, Missing, Duplicate,
      Invalid_Record, Too_Many, Writable, Wrong_Width, Read_Denied,
      Read_Fault, Read_Uncertain, Oversized, Unavailable, Regressed, Expired,
      Invalid_Limit, Reenter, Late_Uncertain);
   Current : Mode;
   Calls, Loads, Clocks : Natural;
   Stop_At : Natural := 0;
   procedure Load
     (Deadline : Time_Instant; Complete : out Boolean;
      Count : out Natural; Items : out Descriptions)
   is
   begin
      Check (Deadline = 100);
      Loads := Loads + 1;
      Complete := Current /= Incomplete;
      Count := 1;
      Items := [others => (others => <>)];
      Items (1) :=
        (ID => ACPI_Backend_Register_IDs.PM_Timer, Epoch => 17,
         Base => 16#408#, Length => 4, Offset => 0,
         IO_Space => True, Readable => True, Width => P.T.Dword_Access,
         Widths => [P.T.Dword_Access => True, others => False], others => <>);
      case Current is
         when Missing => Count := 0;
         when Duplicate => Count := 2; Items (2) := Items (1);
         when Invalid_Record => Items (1).Epoch := 0;
         when Too_Many => Count := Items'Length + 1;
         when Writable =>
            Items (1).Writable := True;
            Items (1).Exact_Write_Safe := True;
         when Wrong_Width =>
            Items (1).Width := P.T.Word_Access;
            Items (1).Widths (P.T.Word_Access) := True;
         when others => null;
      end case;
   end Load;
   procedure Read_Clock (Now : out Time_Instant; Available : out Boolean) is
   begin
      Clocks := Clocks + 1;
      Now := Time_Instant (Clocks);
      Available := not (Current = Unavailable and Clocks = 2);
      if Current = Regressed and Clocks = 2 then Now := 0; end if;
      if (Current = Expired and Clocks = 2)
        or else (Stop_At /= 0 and Clocks = Stop_At)
      then Now := 100; end if;
   end Read_Clock;
   -- A held context can be called again by a callback. It must not send twice.
   procedure Nested;
   procedure Read
     (Deadline : Time_Instant; Item : W.Register_Request;
      Status : out Read_Status; Value : out Unsigned_64)
   is
   begin
      Calls := Calls + 1;
      Check (Deadline = 100 and then not Item.For_Write and then Item.Value = 0
        and then Item.Epoch = 17
        and then Item.Register = W.Register_ID (ACPI_Backend_Register_IDs.PM_Timer));
      Status := Completed;
      Value := Unsigned_64 (Calls);
      case Current is
         when Wrapping => Value := (if Calls = 1 then 16#FFFFFF# else 0);
         when Upper_Noise => Value := Unsigned_64 (Calls) * 16#1000000#;
         when Frozen => Value := 42;
         when Read_Denied => Status := Rejected;
         when Read_Fault => Status := Failed;
         when Read_Uncertain => Status := Unknown;
         when Late_Uncertain =>
            if Calls = 2 then Status := Unknown; end if;
         when Oversized => Value := 16#1_0000_0000#;
         when Reenter => Nested;
         when others => null;
      end case;
   end Read;
   package Probe is new ACPI_Timer_Probe (2, Load, Read_Clock, Read);
   type Context_Access is access all Probe.Context;
   Active : Context_Access;
   procedure Nested is
      R : Report;
   begin
      Probe.Run (Active.all, 100, R);
      Check (R.Status = Running);
   end Nested;
   R, Again : Report;
   Expected : Outcome;
begin
   for M in Mode loop
      declare
         State : aliased Probe.Context;
         Saved_Calls, Saved_Clocks, Saved_Loads : Natural;
      begin
         Current := M; Calls := 0; Loads := 0; Clocks := 0;
         Active := State'Unchecked_Access;
         Probe.Run (State, (if M = Invalid_Limit then W.No_Finite_Deadline else 100), R);
         Expected := (case M is
           when Changing | Wrapping | Reenter => Counter_Changed,
           when Upper_Noise | Frozen => Counter_Unchanged,
           when Incomplete => Metadata_Failed,
           when Missing => Timer_Missing,
           when Duplicate | Invalid_Record | Too_Many => Invalid_Inventory,
           when Writable | Wrong_Width => Unsafe_Timer,
           when Read_Denied => Read_Rejected, when Read_Fault => Read_Failed,
           when Read_Uncertain | Late_Uncertain => Read_Unknown,
           when Oversized => Invalid_Value,
           when Unavailable => Clock_Unavailable, when Regressed => Clock_Regressed,
           when Expired => Deadline_Expired, when Invalid_Limit => Invalid_Deadline);
         Check (R.Status = Expected);
         Check (R.Attempts = Calls and then R.Completed_Reads <= R.Attempts);
         if M in Changing | Wrapping | Reenter then Check (Calls = 2); end if;
         if M in Read_Denied | Read_Fault | Read_Uncertain then
            Check (Calls = 1 and R.Completed_Reads = 0);
         end if;
         if M = Late_Uncertain then
            Check (Calls = 2 and R.Completed_Reads = 1);
         end if;
         if M in Upper_Noise | Frozen then Check (Calls = 8); end if;
         Saved_Calls := Calls; Saved_Clocks := Clocks; Saved_Loads := Loads;
         Probe.Run (State, 999, Again);
         Check (Again = R and then Calls = Saved_Calls
           and then Clocks = Saved_Clocks and then Loads = Saved_Loads);
      end;
   end loop;
   -- Expire at every callback boundary, including after a successful read.
   for Boundary in 1 .. 18 loop
      declare
         State : Probe.Context;
      begin
         Current := Frozen; Calls := 0; Loads := 0; Clocks := 0;
         Stop_At := Boundary;
         Probe.Run (State, 100, R);
         Check (R.Status = Deadline_Expired);
         Check (Clocks = Boundary and R.Attempts = Calls);
         Check (Calls = (if Boundary <= 3 then 0 else (Boundary - 2) / 2));
      end;
   end loop;
   Ada.Text_IO.Put_Line ("PASS timer probe checks=" & Checks'Image);
end Probe_Tests;
