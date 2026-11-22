pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with ACPI_Global_Lock_Owner;
with ACPI_Global_Lock_Scoped_Sessions;
with ACPI_Global_Lock_Requests;
with ACPI_Global_Lock_Dispatch;
with ACPI_Global_Lock_Client;
with ACPI_Locked_NVS;
with ACPI_Locked_NVS_Client;
with ACPI_Locked_NVS_Protocol;
with ACPI_Region_Protocol;
procedure Scoped_Client_PU2C_Tests is
   use Interfaces;
   package W renames ACPI_Region_Protocol;
   package P renames ACPI_Locked_NVS_Protocol;
   type Scope_Tag is new Unsigned_64;
   type Principal is new Unsigned_64;
   type Peer is new Unsigned_64;
   Tag : constant Scope_Tag := 17;
   Caller : constant Principal := 16#9000_0000_0000_002A#;
   Destination : constant Peer := 16#8000_0000_0000_0077#;
   NVS_First : constant Unsigned_64 := 16#3EB7_F000#;
   NVS_Limit : constant Unsigned_64 := 16#41B2_F000#;
   PNVA_First : constant Unsigned_64 := 16#41AE_3018#;
   PU2C_In_PNVA : constant Unsigned_64 := 735;
   PU2C_Record_Offset : constant Unsigned_64 := PNVA_First - NVS_First + PU2C_In_PNVA;
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
   begin Epoch := 1; Accepted := Scope = Tag and then ID = 1 and then P.Valid (Datum) and then Datum.Offset < NVS_Limit - NVS_First
      and then Unsigned_64 (P.T.Octets (Datum.Width)) <= NVS_Limit - NVS_First - Datum.Offset; end Describe;
   procedure Load (Scope : Scope_Tag; Epoch : Unsigned_64; ID : P.Record_ID; Datum : P.Geometry;
     Value : out Unsigned_64; Completed : out Boolean) is
      use type P.Record_ID;
   begin
      Check (Scope = Tag and then Epoch = 1 and then ID = 1 and then P.Valid (Datum));
      Check (NVS_First + Datum.Offset in PNVA_First + PU2C_In_PNVA | PNVA_First + PU2C_In_PNVA + 8);
      Loads := Loads + 1; Value := 16#A5#; Completed := True;
      if Late_Load then Now := 100; end if;
   end Load;
   package B is new ACPI_Locked_NVS (Principal, 0, Scope_Tag, 0, O, S, H, Describe, Load);
   use type O.Outcome, W.Delivery, P.Operation, P.Descriptor_ID, P.Lease_Ticket;
begin
   for Mode in 0 .. 10 loop
      declare
         Backend : H.Context;
         Data : B.Context;
         type Channel_State is null record;
         Channel : Channel_State;
         First_Descriptor : P.Descriptor_ID := P.No_Descriptor;
         First_Lease : P.Lease_Ticket := (0, 0);
         procedure Exchange (Channel : in out Channel_State; Target : Peer; Deadline : W.Time_Instant;
           Request : W.Packet; Reply : out W.Packet; Delivered : out W.Delivery) is
            pragma Unreferenced (Channel);
            Handled : Boolean;
            Input : constant P.Request := P.Decode_Request (Request);
         begin
            Check (Target = Destination and then Deadline in 501 | 777 | 888);
            G.Handle (Backend, Tag, Caller, Request, Reply, Handled);
            if not Handled then
               if Input.Kind = P.Select_Datum then Selects := Selects + 1;
               elsif Input.Kind = P.Read_Datum then
                  Reads := Reads + 1;
                  if Reads = 1 then First_Descriptor := Input.Descriptor; First_Lease := Input.Lease;
                  elsif Mode = 8 then Check (Input.Descriptor /= First_Descriptor and then Input.Lease /= First_Lease);
                  end if;
               else Queries := Queries + 1;
               end if;
               if Mode = 4 and then Input.Kind = P.Read_Datum then
                  Delivered := W.Not_Dispatched; return;
               end if;
               B.Dispatch (Data, Backend, Tag, Caller, Request, Reply, Handled); Check (Handled);
               if Mode in 1 | 8 and then Input.Kind = P.Read_Datum and then Reads = 1 then Delivered := W.Completion_Unknown; return;
               elsif Mode = 2 and then Input.Kind = P.Read_Datum then Reply.Length := 3;
               elsif Mode = 3 and then Input.Kind = P.Select_Datum then Delivered := W.Completion_Unknown; return;
               elsif Mode = 5 and then Input.Kind = P.Select_Datum then Reply.Data (2) := 2;
               elsif Mode = 6 and then Input.Kind = P.Read_Datum then Reply.Data (1) := Reply.Data (1) + 1;
               elsif Mode = 9 and then Input.Kind = P.Read_Datum then IPC_Now := 501;
               elsif Mode = 10 and then Input.Kind = P.Read_Datum then Reply.Data (3) := 16#100#;
               end if;
            end if;
            Delivered := W.Reply_Received;
         end Exchange;
         package Lease is new ACPI_Global_Lock_Client (Channel_State, Peer, 0, Clock, IPC_Clock, Exchange);
         package Client is new ACPI_Locked_NVS_Client (Channel_State, Peer, 0, IPC_Clock, Exchange, Lease);
         Held : Lease.Context;
         State : Client.Context;
         Result : Client.Outcome;
         Lease_Result : Lease.Outcome;
         Owner_Result : O.Outcome;
         Value : Unsigned_64;
         Before : Natural;
         Saved_Diagnostic : Client.Diagnostic;
         use type Lease.Outcome, Client.Outcome, Client.Phase, P.Read_Outcome;
         use type Client.Diagnostic, Client.Read_Point;
         use type P.Record_ID, P.Geometry, W.Time_Instant;
      begin
         Now := 1; Word := 0; Loads := 0; Changes := 0; Reads := 0; Selects := 0; Queries := 0; Late_Load := Mode = 7;
         IPC_Now := 500;
         H.Configure (Backend, Tag, 9, Owner_Result); Check (Owner_Result = O.Configured);
         Lease.Initialize (Held, Destination, Lease_Result); Check (Lease_Result = Lease.Initialized);
         Lease.Acquire (Held, Channel, 100, 501, 1, Lease_Result); Check (Lease_Result = Lease.Acquired);
         Client.Read (State, Held, Channel, 1, (PU2C_Record_Offset, P.T.Byte_Access), 501, Value, Result);
         Saved_Diagnostic := Client.First_Failure (State);
         Check (Saved_Diagnostic.Present = (Mode /= 0));
         if Mode /= 0 then
            Check (Saved_Diagnostic.Result = Result and then Saved_Diagnostic.ID = 1
              and then Saved_Diagnostic.Datum = P.Geometry'(PU2C_Record_Offset, P.T.Byte_Access)
              and then Saved_Diagnostic.IPC_Deadline = 501);
            if Mode = 3 then Check (Saved_Diagnostic.At_Point = Client.Select_Exchange);
            elsif Mode = 4 then Check (Saved_Diagnostic.At_Point = Client.Read_Exchange);
            elsif Mode = 5 then Check (Saved_Diagnostic.At_Point = Client.Select_Reply);
            elsif Mode in 2 | 6 | 7 | 10 then Check (Saved_Diagnostic.At_Point = Client.Read_Reply);
            elsif Mode = 9 then Check (Saved_Diagnostic.At_Point = Client.After_Read);
            end if;
         end if;
         if Mode = 0 then
            Check (Result = Client.Read_Complete and then Value = 16#A5# and then Loads = 1);
            Client.Read (State, Held, Channel, 1, (PU2C_Record_Offset, P.T.Byte_Access), 501, Value, Result);
            Check (Result = Client.Read_Complete and then Loads = 2);
            Client.Read (State, Held, Channel, 1, (PU2C_Record_Offset + 8, P.T.Byte_Access), 501, Value, Result);
            Check (Result = Client.Read_Complete and then Loads = 3);
            Client.Retire (State, Held, Result); Check (Result = Client.Not_Ready);
         else
            Check (Result /= Client.Read_Complete and then Value = 0 and then not Lease.Observe (Held).Usable);
            Check (Loads = (if Mode in 3 .. 5 then 0 else 1));
            Before := Reads;
            Client.Read (State, Held, Channel, 1, (PU2C_Record_Offset, P.T.Byte_Access), 501, Value, Result);
            Check (Result = Client.Not_Ready and then Value = 0 and then Reads = Before);
            if Mode /= 8 then
               Client.Query_Evidence (State, Held, Channel, 888, Result);
               if Mode in 1 | 2 | 6 | 10 then
                  Check (Result = Client.Evidence_Received and then Client.Observe (State).Remote = P.Done and then Loads = 1);
               else Check (Result = Client.No_Evidence); end if;
            end if;
            Check (not Lease.Observe (Held).Usable);
            Client.Retire (State, Held, Result); Check (Result = Client.Not_Ready);
         end if;
         Check (Client.First_Failure (State) = Saved_Diagnostic);
         Lease.Release (Held, Channel, 777, Lease_Result); Check (Lease_Result = Lease.Released and then Changes = 2);
         Client.Retire (State, Held, Result); Check (Result = Client.Retired and then Client.Observe (State).Stage = Client.Ready);
         Check (Client.First_Failure (State) = Saved_Diagnostic);
         if Mode = 8 then
            Check (Queries = 0);
            Lease.Acquire (Held, Channel, 100, 501, 1, Lease_Result); Check (Lease_Result = Lease.Acquired);
            Client.Read (State, Held, Channel, 1, (PU2C_Record_Offset, P.T.Byte_Access), 501, Value, Result);
            Check (Result = Client.Read_Complete and then Loads = 2 and then Reads = 2);
            Lease.Release (Held, Channel, 777, Lease_Result); Check (Lease_Result = Lease.Released and then Changes = 4);
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("PU2C geometry scoped client checks" & Checks'Image);
end Scoped_Client_PU2C_Tests;
