pragma Ada_2022;
with ACPI_Global_Lock_Owner;
with ACPI_Global_Lock_Scoped_Sessions;
with ACPI_Global_Lock_Requests;
with ACPI_Global_Lock_Dispatch;
with ACPI_Global_Lock_Protocol;
with ACPI_Locked_NVS;
with ACPI_Locked_NVS_Protocol;
with ACPI_Inventory_Protocol;
with ACPI_NVS_Protocol;
package body Locked_Owner_Model is
   use Interfaces;
   package P renames ACPI_Locked_NVS_Protocol;
   type Scope_Tag is new Unsigned_64;
   type Principal is new Unsigned_64;
   Tag : constant Scope_Tag := 17;
   Caller : constant Principal := 16#9000_0000_0000_002A#;
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
   use type O.Outcome, P.Operation;
   package LP renames ACPI_Global_Lock_Protocol;
   package I renames ACPI_Inventory_Protocol;
   package N renames ACPI_NVS_Protocol;
   Backend : H.Context;
   Data : B.Context;
         procedure Lease_Clock (Value : out Unsigned_64; Available : out Boolean) is
         begin Value := Now; Available := Mode /= 1; end Lease_Clock;
         procedure Exchange (Channel : in out Channel_State; Target : Peer; Deadline : W.Time_Instant;
           Request : W.Packet; Reply : out W.Packet; Delivered : out W.Delivery) is
            pragma Unreferenced (Channel);
            Handled : Boolean;
            Input : constant P.Request := P.Decode_Request (Request);
            Lock_Input : constant LP.Request := LP.Decode_Request (Request);
            use type LP.Operation;
         begin
            Check (Target = Destination and then Deadline in 1_000 | 600 | 525);
            Delivered := W.Reply_Received;
            if Request.Label = I.Header_Operation then
               Reply := (Label => I.Header_Reply, Data => [I.Outcome'Pos (I.Ready), 1, 1, I.Version], others => <>); return;
            elsif Request.Label = I.Part_Operation then
               declare
                  Description : constant I.Description := (ID => 11, Epoch => 9, Base => 16#608#, Length => 4,
                    Offset => 0, Space => I.IO_Space, Readable => True, Writable => False,
                    Delegable => False, Exact_Write_Safe => False,
                    Widths => [P.T.Dword_Access => True, others => False], Width => P.T.Dword_Access, Write_Mask => 0);
                  Parts : constant I.Parts := I.Encode (Description);
                  Part : constant I.Payload := Parts (I.Part_Kind'Val (Request.Data (2)));
               begin Reply := (Label => I.Part_Reply, Data => [I.Outcome'Pos (I.Ready), Part (0), Part (1), Part (2)], others => <>); return; end;
            elsif Request.Label = N.Header_Operation then Reply := N.Header_Result (1, 1); return;
            elsif Request.Label = N.Describe_Operation then Reply := N.Describe_Result (1, 16#1000#, 4096); return;
            elsif Request.Label = N.Read_Operation or else Request.Label = W.Read_Operation then
               Legacy_Reads := Legacy_Reads + 1; raise Program_Error with "Lock field reached legacy IO";
            end if;
            if Lock_Input.Valid then
               if Lock_Input.Kind = LP.Acquire then Acquires := Acquires + 1;
               elsif Lock_Input.Kind = LP.Release then Releases := Releases + 1;
               elsif Lock_Input.Kind in LP.Query_Acquisition | LP.Query_Termination then Queries_Lock := Queries_Lock + 1;
               end if;
            end if;
            G.Handle (Backend, Tag, Caller, Request, Reply, Handled);
            if not Handled then
               Check (Input.Valid);
               B.Dispatch (Data, Backend, Tag, Caller, Request, Reply, Handled); Check (Handled);
               if Mode in 4 | 10 and then Input.Kind = P.Read_Datum then Now := 100_001; end if;
            end if;
            if (Mode = 2 and then Lock_Input.Valid and then Lock_Input.Kind = LP.Acquire)
              or else (Mode = 8 and then Lock_Input.Valid and then Lock_Input.Kind = LP.Release)
              or else (Mode = 3 and then Input.Valid and then Input.Kind = P.Read_Datum)
            then Delivered := W.Completion_Unknown; end if;
         end Exchange;
   procedure Configure is
      Result : O.Outcome;
   begin H.Configure (Backend, Tag, 9, Result); Check (Result = O.Configured); end Configure;
end Locked_Owner_Model;
