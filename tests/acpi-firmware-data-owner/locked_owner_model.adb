pragma Ada_2022;
with ACPI_Firmware_Data_Frames;
with ACPI_Firmware_Data_Replies;
with ACPI_Firmware_Data_Lease_Check;
with ACPI_Firmware_Data_Scopes;
with ACPI_Firmware_Data_History;
with ACPI_Firmware_Data_Dispatch;
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
   use type W.Time_Instant;
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
              or else (Mode in 8 | 11 and then Lock_Input.Valid and then Lock_Input.Kind = LP.Release)
              or else (Drop_Termination_Replies and then Lock_Input.Valid
                and then Lock_Input.Kind = LP.Query_Termination)
              or else (Mode = 3 and then Input.Valid and then Input.Kind = P.Read_Datum)
            then Delivered := W.Completion_Unknown; end if;
         end Exchange;
   package Data_Frames renames ACPI_Firmware_Data_Frames;
   package Data_Wire renames ACPI_Firmware_Data_Replies;
   use type Data_Frames.Identifier, Data_Frames.Operation, Data_Wire.Outcome, Data_Wire.Effect_Kind;
   function Data_Authorized (Who : Unsigned_64; For_Write : Boolean) return Boolean is
      (Who = Unsigned_64 (Caller) and then not For_Write);
   function Convert_Caller (Who : Unsigned_64) return Principal is (Principal (Who));
   package Data_Lease is new ACPI_Firmware_Data_Lease_Check
      (Principal,0,Convert_Caller,Scope_Tag,0,O,S,H,Backend,Tag);
   procedure Data_Select (Who : Unsigned_64; ID : Data_Frames.Identifier; Offset : Unsigned_64;
      Width : Data_Frames.T.Access_Width; Accepted : out Boolean) is
      use type Data_Frames.T.Access_Width;
   begin
      Accepted := Who = Unsigned_64 (Caller) and then ID = 1 and then Offset = 0
         and then Width = Data_Frames.T.Byte_Access;
   end Data_Select;
   procedure Data_Transfer (Who : Unsigned_64; ID : Data_Frames.Identifier; Offset : Unsigned_64;
      Width : Data_Frames.T.Access_Width; For_Write : Boolean; Input : Unsigned_64;
      Deadline : Data_Frames.IPC.Time_Instant; Value : out Unsigned_64;
      Status : out Data_Frames.IPC.Outcome);
   package Data_Scopes is new ACPI_Firmware_Data_Scopes
      (IPC_Clock,Data_Authorized,Data_Lease.Check,Data_Select,Data_Transfer);
   Data_State : Data_Scopes.Context;
   procedure Data_Transfer (Who : Unsigned_64; ID : Data_Frames.Identifier; Offset : Unsigned_64;
      Width : Data_Frames.T.Access_Width; For_Write : Boolean; Input : Unsigned_64;
      Deadline : Data_Frames.IPC.Time_Instant; Value : out Unsigned_64;
      Status : out Data_Frames.IPC.Outcome) is
      Allowed : Boolean;
      use type Data_Frames.T.Access_Width;
   begin
      Check (Who = Unsigned_64 (Caller) and then ID = 1 and then Offset = 0
         and then Width = Data_Frames.T.Byte_Access and then not For_Write and then Input = 0);
      Check (Deadline = (if Data_Mode = 5 then 1_000 else 600));
      Data_Scopes.Check_Active (Data_State,Who,For_Write,Allowed); Check (Allowed);
      Check (if Data_Mode = 5 then Acquires = 0 and then Releases = 0 else Acquires = Releases + 1);
      Data_Reads := Data_Reads + 1; Value := 16#5A#; Status := Data_Frames.IPC.Done;
   end Data_Transfer;
   procedure Data_Effect (Who : Unsigned_64; Descriptor : Data_Frames.Identifier; For_Write : Boolean;
      Input : Unsigned_64; Value : out Unsigned_64; Status : out Data_Frames.IPC.Outcome) is
   begin Data_Scopes.Perform (Data_State,Who,Descriptor,For_Write,Input,Value,Status); end Data_Effect;
   package Data_History is new ACPI_Firmware_Data_History (Data_Effect);
   History : Data_History.Context;
   function Data_Allowed (Who : Unsigned_64) return Boolean is (Who = Unsigned_64 (Caller));
   function Scope_Status (Item : Data_Scopes.Outcome) return Data_Wire.Outcome is
      (case Item is when Data_Scopes.Ready => Data_Wire.Ready,
       when Data_Scopes.Closed => Data_Wire.Closed, when Data_Scopes.Stopped => Data_Wire.Stopped,
       when Data_Scopes.Not_Found => Data_Wire.Not_Found,
       when Data_Scopes.Request_Changed => Data_Wire.Request_Changed,
       when Data_Scopes.Invalid_Request => Data_Wire.Invalid_Request,
       when Data_Scopes.Denied => Data_Wire.Denied,
       when Data_Scopes.Clock_Unavailable => Data_Wire.Clock_Unavailable,
       when Data_Scopes.Clock_Reversed => Data_Wire.Clock_Reversed,
       when Data_Scopes.Expired => Data_Wire.Expired,
       when Data_Scopes.Lease_Refused => Data_Wire.Lease_Refused,
       when Data_Scopes.Selection_Refused => Data_Wire.Selection_Refused,
       when Data_Scopes.Allocation_Refused => Data_Wire.Capacity_Refused,
       when Data_Scopes.Reentered => Data_Wire.Busy);
   Published_Client : Data_Frames.Identifier := 0;
   procedure Data_Handle (Who : Unsigned_64; Item : Data_Frames.Request;
      Status : out Data_Wire.Outcome; Effect : out Data_Wire.Effect_Kind; Value : out Unsigned_64) is
      R : Data_Scopes.Reply;
      Saved : Data_History.Report;
      use type Data_History.Outcome, Data_Frames.IPC.Outcome;
   begin
      Status := Data_Wire.Denied; Effect := Data_Wire.No_Effect; Value := 0;
      case Item.Kind is
         when Data_Frames.Publish =>
            Check (Item.Region.Address = 16#5000# and then Item.Region.Length = 2 and then Item.Publication_Deadline = 1_000);
            Data_Publishes := Data_Publishes + 1; Published_Client := Item.ID;
            Status := Data_Wire.Ready; Value := 1;
         when Data_Frames.Describe_Publication | Data_Frames.Recover_Publication =>
            if Item.ID = Published_Client then Status := Data_Wire.Ready; Value := 1;
            else Status := Data_Wire.Not_Found; end if;
         when Data_Frames.Begin_Scope =>
            Data_Begins := Data_Begins + 1;
            Check (Item.Scope_Deadline = (if Data_Mode = 5 then 1_000 else 600));
            Check (if Data_Mode = 5 then Item.Lock_Domain = 0 and then Item.Lock_Sequence = 0
               else Item.Lock_Domain /= 0 and then Item.Lock_Sequence /= 0);
            if Data_Mode = 1 then Status := Data_Wire.Refused; return; end if;
            Data_Scopes.Begin_Scope (Data_State,Who,Item.ID,Item.Scope_Deadline,Item.Lock_Domain,Item.Lock_Sequence,R);
            Status := Scope_Status (R.Status); if Status = Data_Wire.Ready then Value := Unsigned_64 (R.ID); end if;
         when Data_Frames.Select_Datum =>
            Data_Scopes.Select_Datum (Data_State,Who,Item.ID,Item.Client_Descriptor_ID,Item.Record_ID,Item.Offset,Item.Width,R);
            Status := Scope_Status (R.Status); if Status = Data_Wire.Ready then Value := Unsigned_64 (R.ID); end if;
         when Data_Frames.Close_Scope =>
            Data_Ends := Data_Ends + 1; Data_Scopes.Close_Scope (Data_State,Who,Item.ID,R); Status := Scope_Status (R.Status);
         when Data_Frames.Describe_Scope =>
            R := Data_Scopes.Describe_Scope (Data_State,Who,Item.ID);
            Status := Scope_Status (R.Status); if Status = Data_Wire.Ready then Value := Unsigned_64 (R.ID); end if;
         when Data_Frames.Read_Datum | Data_Frames.Write_Datum | Data_Frames.Query_Operation =>
            if Item.Kind = Data_Frames.Query_Operation then
               Saved := Data_History.Query (History,Who,Item.ID,Item.Operation_ID);
            else
               Data_History.Submit (History,Who,Item.ID,
                  (if Item.Kind = Data_Frames.Write_Datum then Item.Write_Operation_ID else Item.Operation_ID),
                  Item.Kind = Data_Frames.Write_Datum,
                  (if Item.Kind = Data_Frames.Write_Datum then Item.Value else 0),Saved);
            end if;
            Effect := Saved.Effect;
            case Saved.State is
               when Data_History.No_Evidence => Status := Data_Wire.No_Evidence;
               when Data_History.Running => Status := Data_Wire.Running;
               when Data_History.Identity_Conflict => Status := Data_Wire.Request_Changed;
               when Data_History.Allocation_Refused => Status := Data_Wire.Capacity_Refused;
               when Data_History.Invalid_Identity => Status := Data_Wire.Invalid_Request;
               when Data_History.Reentered => Status := Data_Wire.Busy;
               when Data_History.Finished =>
                  Status := (case Saved.Status is when Data_Frames.IPC.Done => Data_Wire.Done,
                     when Data_Frames.IPC.Denied => Data_Wire.Denied,
                     when Data_Frames.IPC.Indeterminate => Data_Wire.Indeterminate,
                     when Data_Frames.IPC.Malformed | Data_Frames.IPC.Backend_Fault => Data_Wire.Backend_Fault);
            end case;
            if Status = Data_Wire.Done and then Effect = Data_Wire.Read_Effect then Value := Saved.Value; end if;
      end case;
   end Data_Handle;
   package Data_Dispatch is new ACPI_Firmware_Data_Dispatch (Data_Allowed,Data_Handle);
   Data_Dispatcher : Data_Dispatch.Context;
   procedure Data_Exchange (Channel : in out Channel_State; Target : Peer; Deadline : W.Time_Instant;
      Request : W.Packet; Reply : out W.Packet; Delivered : out W.Delivery) is
      pragma Unreferenced (Channel);
      Decoded : constant Data_Frames.Decoded := Data_Frames.Decode (Request,False);
      Handled : Boolean;
   begin
      Check (Target = Destination and then Deadline > IPC_Now and then Decoded.Valid);
      Data_Dispatch.Dispatch (Data_Dispatcher,Unsigned_64 (Caller),Request,False,Reply,Handled); Check (Handled);
      Delivered := W.Reply_Received;
      if (Data_Mode = 4 and then Decoded.Item.Kind = Data_Frames.Publish)
        or else (Data_Mode = 2 and then Decoded.Item.Kind = Data_Frames.Read_Datum)
        or else (Data_Mode = 3 and then Decoded.Item.Kind = Data_Frames.Close_Scope)
      then Delivered := W.Completion_Unknown; end if;
   end Data_Exchange;

   procedure Configure is
      Result : O.Outcome;
   begin H.Configure (Backend, Tag, 9, Result); Check (Result = O.Configured); end Configure;
end Locked_Owner_Model;
