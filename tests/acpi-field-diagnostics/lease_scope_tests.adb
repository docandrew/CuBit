pragma Ada_2022;
with Ada.Text_IO;
with ACPI_Field_Diagnostics;
with Interfaces;
with ACPI_Global_Lock_Owner;
with ACPI_Global_Lock_Scoped_Sessions;
with ACPI_Global_Lock_Requests;
with ACPI_Global_Lock_Dispatch;
with ACPI_Global_Lock_Client;
with ACPI_Global_Lock_Protocol;
with ACPI_Field_Lease_Scope;
with ACPI_Locked_NVS;
with ACPI_Locked_NVS_Client;
with ACPI_Locked_NVS_Protocol;
with ACPI_Region_Protocol;
procedure Lease_Scope_Tests is
   use Interfaces;
   package D renames ACPI_Field_Diagnostics;
   use type D.Cause, D.Point;
   package W renames ACPI_Region_Protocol;
   package P renames ACPI_Locked_NVS_Protocol;
   type Scope_Tag is new Unsigned_64;
   type Principal is new Unsigned_64;
   type Peer is new Unsigned_64;
   Tag : constant Scope_Tag := 17;
   Caller : constant Principal := 16#9000_0000_0000_002A#;
   Destination : constant Peer := 16#8000_0000_0000_0077#;
   Now : Unsigned_64 := 1;
   Word : Unsigned_32 := 0;
   Checks, Loads, Changes, Reads, Selects, Queries : Natural := 0;
   Late_Load : Boolean := False;
   IPC_Now : W.Time_Instant := 500;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
   type Atomic_Result is (Exchanged, Different, Refused, Unknown);
   type Signal_Result is (Complete, Not_Sent, Uncertain);
   procedure Clock (Value : out Unsigned_64; Available : out Boolean) is
   begin Value := Now; Available := True; end Clock;
   procedure IPC_Clock (Value : out W.Time_Instant; Available : out Boolean) is
   begin Value := IPC_Now; Available := True; end IPC_Clock;
   procedure Read_Word (Value : out Unsigned_32; Available : out Boolean) is
   begin Value := Word; Available := True; end Read_Word;
   procedure Compare (Expected, Replacement : Unsigned_32; Observed : out Unsigned_32; Result : out Atomic_Result) is
   begin Check (Expected = Word); Changes := Changes + 1; Observed := Word; Word := Replacement; Result := Exchanged; end Compare;
   procedure Signal (Result : out Signal_Result) is
   begin Result := Complete; end Signal;
   package O is new ACPI_Global_Lock_Owner
     (Scope_Tag, 0, Clock, Read_Word, Atomic_Result, Exchanged, Different, Refused, Unknown,
      Compare, Signal_Result, Complete, Not_Sent, Uncertain, Signal);
   package S is new ACPI_Global_Lock_Scoped_Sessions (Principal, 0, Scope_Tag, 0, O);
   package H is new ACPI_Global_Lock_Requests (Principal, 0, Scope_Tag, 0, O, S);
   package G is new ACPI_Global_Lock_Dispatch (Principal, 0, Scope_Tag, 0, O, S, H, Clock);
   procedure Describe (Scope : Scope_Tag; ID : P.Record_ID; Datum : P.Geometry; Epoch : out Unsigned_64; Accepted : out Boolean) is
      use type P.Record_ID;
   begin Epoch := 1; Accepted := Scope = Tag and then ID = 1 and then P.Valid (Datum) and then Datum.Offset < 32; end Describe;
   procedure Load (Scope : Scope_Tag; Epoch : Unsigned_64; ID : P.Record_ID; Datum : P.Geometry;
     Value : out Unsigned_64; Completed : out Boolean) is
      use type P.Record_ID;
   begin
      Check (Scope = Tag and then Epoch = 1 and then ID = 1 and then P.Valid (Datum));
      Loads := Loads + 1; Value := 16#A5#; Completed := True;
      if Late_Load then Now := 100; end if;
   end Load;
   package B is new ACPI_Locked_NVS (Principal, 0, Scope_Tag, 0, O, S, H, Describe, Load);
   use type O.Outcome, W.Delivery, P.Operation;
begin
   for Mode in 0 .. 12 loop
      declare
         package LP renames ACPI_Global_Lock_Protocol;
         Backend : H.Context;
         Data : B.Context;
         type Channel_State is null record;
         Channel : Channel_State;
         Acquires, Releases, Lock_Queries : Natural := 0;
         procedure Exchange (Channel : in out Channel_State; Target : Peer; Deadline : W.Time_Instant;
           Request : W.Packet; Reply : out W.Packet; Delivered : out W.Delivery) is
            pragma Unreferenced (Channel);
            Handled : Boolean;
            Input : constant P.Request := P.Decode_Request (Request);
            Lock_Input : constant LP.Request := LP.Decode_Request (Request);
            use type LP.Operation;
         begin
            Check (Target = Destination and then Deadline in 501 | 777 | 888);
            Delivered := W.Reply_Received;
            if Lock_Input.Valid then
               if Lock_Input.Kind = LP.Acquire then Acquires := Acquires + 1;
               elsif Lock_Input.Kind = LP.Release then
                  Releases := Releases + 1;
                  if Mode = 8 and then Releases = 1 then Delivered := W.Not_Dispatched; return; end if;
               elsif Lock_Input.Kind in LP.Query_Acquisition | LP.Query_Termination then
                  Lock_Queries := Lock_Queries + 1;
               end if;
            end if;
            G.Handle (Backend, Tag, Caller, Request, Reply, Handled);
            if not Handled then
               if Input.Kind = P.Select_Datum then Selects := Selects + 1;
               elsif Input.Kind = P.Read_Datum then Reads := Reads + 1;
               else Queries := Queries + 1;
               end if;
               B.Dispatch (Data, Backend, Tag, Caller, Request, Reply, Handled); Check (Handled);
            end if;
            if (Mode = 2 and then Lock_Input.Valid and then Lock_Input.Kind = LP.Acquire)
              or else (Mode = 4 and then Lock_Input.Valid and then Lock_Input.Kind = LP.Release)
              or else (Mode = 3 and then Input.Valid and then Input.Kind = P.Read_Datum and then Reads = 1)
            then Delivered := W.Completion_Unknown; end if;
         end Exchange;
         package Lease is new ACPI_Global_Lock_Client (Channel_State, Peer, 0, Clock, IPC_Clock, Exchange);
         package Client is new ACPI_Locked_NVS_Client (Channel_State, Peer, 0, IPC_Clock, Exchange, Lease);
         package Scope is new ACPI_Field_Lease_Scope (Channel_State, Peer, 0, IPC_Clock, Exchange, Lease, Client, 3);
         State : Scope.Context;
         Good : Boolean;
         Result : Lease.Outcome;
         Owner_Result : O.Outcome;
         Value : Unsigned_64;
         Before : Natural;
         use type Lease.Outcome, Scope.Depth_Count;
         procedure Start (Required : Boolean := True; Deadline : Unsigned_64 := 100) is
         begin Scope.Begin_Field (State, Channel, Required, False, Deadline, 501, 1, Good); end Start;
         procedure Stop (Deadline : W.Time_Instant := 777) is
         begin Scope.End_Field (State, Channel, Deadline, Good); end Stop;
         procedure Read is
         begin Scope.Read (State, Channel, 1, (8, P.T.Byte_Access), 501, Value, Good); end Read;
      begin
         Now := 1; Word := 0; Loads := 0; Changes := 0; Reads := 0; Selects := 0; Queries := 0;
         Late_Load := False; IPC_Now := 500;
         H.Configure (Backend, Tag, 9, Owner_Result); Check (Owner_Result = O.Configured);
         Scope.Initialize (State, Destination, Good); Check (Good);
         Scope.Initialize (State, Peer'Last, Good); Check (not Good and then Scope.Observe (State).Initialized);
         if Mode = 7 then Word := 2; end if;
         Start (Required => Mode not in 1 | 10);
         if Mode in 2 | 7 then
            Check (not Good and then Scope.Observe (State).Depth = 0 and then Scope.Pending (State));
            Check (Scope.Observe (State).Cleanup_Required = (Mode = 2));
            if Mode = 2 then
               Scope.Retire (State, Good); Check (not Good);
               Before := Acquires; Start; Check (not Good and then Acquires = Before);
               Scope.Cleanup (State, Channel, 777, Result); Check (Result = Lease.Released);
            end if;
         elsif Mode = 10 then
            Check (Good and then Acquires = 0);
            Read; Check (not Good and then Value = 0 and then Loads = 0);
            Stop; Check (not Good and then Releases = 0);
         else
            Check (Good);
            if Mode = 1 then Start; Check (Good); end if;
            if Mode in 0 | 1 | 6 | 11 then
               Start (Deadline => 0); Check (Good and then Acquires = 1);
            end if;
            if Mode = 6 then
               Start; Check (Good and then Scope.Observe (State).Depth = 3);
               Start; Check (not Good and then Scope.Observe (State).Depth = 3);
            elsif Mode = 11 then
               Now := 100; Stop; Check (not Good and then Releases = 0);
            else
               Read;
               Check (Good = (Mode /= 3) and then Value = (if Mode = 3 then 0 else 16#A5#) and then Loads = 1);
               if Mode = 0 then
                  Read; Check (Good and then Loads = 2 and then Acquires = 1 and then Releases = 0);
               elsif Mode = 3 then
                  Before := Reads; Read; Check (not Good and then Value = 0 and then Reads = Before);
                  Start; Check (not Good and then Scope.Observe (State).Depth = 1);
               elsif Mode = 12 then
                  Now := 100;
               elsif Mode = 5 then
                  Now := 100; Start; Check (not Good and then Scope.Observe (State).Depth = 1 and then Acquires = 1);
               end if;
            end if;
            if Mode = 9 then Stop (0); Check (not Good and then Scope.Observe (State).Cleanup_Required);
            else
               while Scope.Observe (State).Depth > 0 loop
                  Stop;
               end loop;
            end if;
            if Mode in 4 | 8 | 9 then
               Check (Scope.Observe (State).Depth = 0 and then Scope.Observe (State).Cleanup_Required);
               Scope.Retire (State, Good); Check (not Good);
               Scope.Cleanup (State, Channel, 888, Result); Check (Result = Lease.Released);
               Check (Releases = (if Mode = 8 then 2 else 1));
               if Mode = 4 then Check (Lock_Queries = 2); end if;
            end if;
            if Mode = 12 then
               Check (not Good and then Scope.Pending (State));
               Check (Scope.First_Failure (State).Reason = D.Lease_Deadline
                 and then Scope.First_Failure (State).At_Point = D.End_Lease);
            end if;
            Check (Acquires = 1 and then Changes = 2 and then not Scope.Observe (State).Cleanup_Required);
         end if;
         if Mode = 2 then
            Check (Scope.First_Failure (State).Reason = D.Transport_Unknown
              and then Scope.First_Failure (State).At_Point = D.Acquire_Lease);
         end if;
         Check (Scope.Observe (State).Depth = 0);
         Scope.Retire (State, Good); Check (Good and then not Scope.Pending (State));
         if Mode = 3 then
            Start; Check (Good and then Acquires = 2);
            Read; Check (Good and then Loads = 2 and then Reads = 2);
            Stop; Check (Good and then Changes = 4);
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Lease scope checks" & Checks'Image);
end Lease_Scope_Tests;
