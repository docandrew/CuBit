pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Region_Protocol;
with ACPI_Region_Policy;
with ACPI_Region_IO;
with ACPI_Region_Client;
with ACPI_FADT.Transactions;
procedure Region_Client_Tests is
   package P renames ACPI_Region_Protocol;
   package R renames ACPI_Region_Policy;
   package T renames ACPI_FADT.Transactions;
   use type P.Words;
   use type P.Outcome;
   use type P.Register_ID;
   use type P.Register_Request;
   use type P.Time_Instant;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "client check" & Checks'Image; end if;
   end Check;
   type Hardware is record
      Value : Unsigned_64 := 0;
      Reads, Writes : Natural := 0;
      Complete : Boolean := True;
   end record;
   procedure Transact
     (H : in out Hardware; Space : R.Address_Space; Address : Unsigned_64;
      Width : R.Access_Width; For_Write : Boolean; Input : Unsigned_64;
      Output : out Unsigned_64; Completed : out Boolean) is
      pragma Unreferenced (Width);
      use type R.Address_Space;
   begin
      Check (Space = R.Memory_Space and then Address = 16#8000#);
      if For_Write then H.Writes := H.Writes + 1; H.Value := Input;
      else H.Reads := H.Reads + 1; end if;
      Output := H.Value; Completed := H.Complete;
   end Transact;
   package Backend is new ACPI_Region_IO (Hardware, Transact);
   type Endpoint is new Unsigned_64;
   Endpoint_Value : constant Endpoint := 16#FEDC_BA98_7654_3210#;
   Stamp : constant Unsigned_64 := 16#9876_5432_1020_3040#;
   Register_Value : constant P.Register_ID := 16#FEDC_1111_8765_4321#;
   type Mode is (Normal, Not_Sent, Lost_Reply, Corrupt_Reply, Unknown_Hardware);
   type Transport_State is record
      Region : R.State;
      H : Hardware;
      Behavior : Mode := Normal;
      Calls : Natural := 0;
      Ticket : Unsigned_64 := 0;
      Last_Request : P.Packet;
      Authority : Unsigned_64 := Stamp;
   end record;
   procedure Exchange
     (Transport : in out Transport_State; Destination : Endpoint;
      Deadline : P.Time_Instant; Item : P.Packet; Reply : out P.Packet;
      Delivered : out P.Delivery) is
   begin
      Check (Destination = Endpoint_Value and then Deadline = 99);
      Transport.Calls := Transport.Calls + 1;
      Transport.Last_Request := Item;
      Reply := (others => <>);
      if Transport.Behavior = Not_Sent then Delivered := P.Not_Dispatched; return; end if;
      Transport.H.Complete := Transport.Behavior /= Unknown_Hardware;
      Backend.Dispatch (Transport.Region, Transport.H, Transport.Authority,
         Item, Reply, Transport.Ticket);
      Delivered := P.Reply_Received;
      if Transport.Behavior = Lost_Reply then Delivered := P.Completion_Unknown;
      elsif Transport.Behavior = Corrupt_Reply then Reply.Flags := 1; end if;
   end Exchange;
   package Client is new ACPI_Region_Client (Transport_State, Endpoint, 0, Exchange);
   use type Client.Completion;
   use type Client.Request_ID;
   use type Client.Observation;
   function Configuration return R.Configuration is
     ((Tag => Stamp, Base => 16#8000#, Length => 8, Space => R.Memory_Space,
      Readable => True, Writable => True, Widths => [others => True],
      Register_Authority => (Resource_ID => Unsigned_64 (Register_Value),
        Readable => True, Writable => True, Delegable => False, Write_Mask => Unsigned_64'Last),
      Register_Offset => 0, Register_Width => T.Qword_Access));
   Request : P.Register_Request :=
     (Epoch => 1, Register => Register_Value, For_Write => False, Value => 0);
   Sample_Values : constant array (Positive range 1 .. 8) of Unsigned_64 :=
     [0, 1, 16#FFFF_FFFF#, 16#1_0000_0000#, 16#8000_0000_0000_0000#,
      16#FEDC_BA98_7654_3210#, Unsigned_64'Last - 1, Unsigned_64'Last];
   Raw : P.Packet;
   Decoded : P.Decoded_Reply;
begin
   -- The codec preserves opaque IDs, epochs and every value bit.
   for Writing in Boolean loop
      for Value of Sample_Values loop
         Request.Epoch := P.Epoch_Number (Unsigned_64'Last);
         Request.For_Write := Writing;
         Request.Value := (if Writing then Value else 0);
         Check (P.Valid_Request (Request));
         Raw := P.Encode (Request);
         Check (Raw.Data = P.Words'[Unsigned_64'Last, Unsigned_64 (Register_Value), Request.Value, 0]);
         Check (Raw.Label = (if Writing then P.Write_Operation else P.Read_Operation));
         for Status in P.Outcome loop
            Raw := (Label => P.Operation_Reply,
               Data => [P.Outcome'Pos (Status), 0, 0, 0], others => <>);
            if Status = P.Done and then not Writing then
               Raw.Data (1) := Value and P.Half_Mask;
               Raw.Data (2) := Shift_Right (Value, P.Half_Bits);
            end if;
            Decoded := P.Decode (Raw, Writing);
            Check (Decoded.Valid and then Decoded.Status = Status);
            Check (Decoded.Value = (if Status = P.Done and then not Writing then Value else 0));
         end loop;
      end loop;
   end loop;
   Request := (Epoch => 1, Register => Register_Value, For_Write => False, Value => 0);
   for Writing in Boolean loop
      for Bad in 1 .. 11 loop
         Raw := (Label => P.Operation_Reply, Data => [P.Outcome'Pos (P.Done), 0, 0, 0], others => <>);
         case Bad is
            when 1 => Raw.Label := P.Read_Operation;
            when 2 => Raw.Length := 3;
            when 3 => Raw.Length := 5;
            when 4 => Raw.Flags := 1;
            when 5 => Raw.Reserved := 1;
            when 6 => Raw.Data (0) := P.Outcome'Pos (P.Outcome'Last) + 1;
            when 7 => Raw.Data (0) := Unsigned_64'Last;
            when 8 => Raw.Data (1) := P.Half_Mask + 1;
            when 9 => Raw.Data (2) := P.Half_Mask + 1;
            when 10 => Raw.Data (3) := 1;
            when 11 => Raw.Data := [P.Outcome'Pos (P.Denied), 1, 0, 0];
         end case;
         Check (not P.Decode (Raw, Writing).Valid);
      end loop;
   end loop;
   Raw := (Label => P.Operation_Reply, Data => [P.Outcome'Pos (P.Done), 1, 0, 0], others => <>);
   Check (not P.Decode (Raw, True).Valid);
   declare
      IO : Client.Context;
      Transport : Transport_State;
      Reply : Client.Result;
      Accepted : Boolean;
      Before : Natural;
      Previous : Client.Request_ID := Client.No_Request;
   begin
      R.Install (Transport.Region, Configuration, Accepted); Check (Accepted);
      Client.Dispatch (IO, Transport, 10, 99, Request, Reply);
      Check (Reply.Status = Client.Rejected and then Transport.Calls = 0);
      Client.Initialize (IO, 0, Accepted); Check (not Accepted);
      Client.Initialize (IO, Endpoint_Value, Accepted); Check (Accepted);
      Client.Initialize (IO, Endpoint_Value + 1, Accepted); Check (not Accepted);
      for Bad in 1 .. 6 loop
         declare
            Item : P.Register_Request := Request;
            Deadline : P.Time_Instant := 99;
         begin
            case Bad is
               when 1 => Item.Epoch := P.No_Epoch;
               when 2 => Item.Register := P.No_Register;
               when 3 => Item.Value := 1;
               when 4 => Deadline := 10;
               when 5 => Deadline := 9;
               when 6 => Deadline := P.No_Finite_Deadline;
            end case;
            Client.Dispatch (IO, Transport, 10, Deadline, Item, Reply);
            Check (Reply.Status = Client.Rejected and then Transport.Calls = 0
              and then Reply.Operation = Client.No_Request and then not Client.Pending (IO));
         end;
      end loop;
      for Value of Sample_Values loop
         Request.For_Write := True; Request.Value := Value;
         Client.Dispatch (IO, Transport, 10, 99, Request, Reply);
         Check (Reply.Status = Client.Completed and then Reply.Value = 0
           and then Reply.Operation > Previous);
         Previous := Reply.Operation;
         Request.For_Write := False; Request.Value := 0;
         Client.Dispatch (IO, Transport, 10, 99, Request, Reply);
         Check (Reply.Status = Client.Completed and then Reply.Value = Value
           and then Reply.Operation > Previous and then not Client.Pending (IO));
         Previous := Reply.Operation;
      end loop;
      Transport.Behavior := Not_Sent; Before := Transport.H.Writes;
      Client.Dispatch (IO, Transport, 10, 99, Request, Reply);
      Check (Reply.Status = Client.Rejected and then not Client.Pending (IO)
        and then Transport.H.Writes = Before);
      Transport.Behavior := Normal; Transport.Authority := 0;
      Before := Transport.H.Reads;
      Client.Dispatch (IO, Transport, 10, 99, Request, Reply);
      Check (Reply.Status = Client.Rejected and then not Client.Pending (IO)
        and then Transport.H.Reads = Before);
   end;
   for Behavior in Lost_Reply .. Unknown_Hardware loop
      declare
         IO : Client.Context;
         Transport : Transport_State;
         Reply : Client.Result;
         Saved, Wrong : Client.Observation;
         Accepted : Boolean;
         Before : Natural;
      begin
         R.Install (Transport.Region, Configuration, Accepted); Check (Accepted);
         Client.Initialize (IO, Endpoint_Value, Accepted); Check (Accepted);
         Transport.Behavior := Behavior;
         Request.For_Write := True; Request.Value := Unsigned_64'Last;
         Client.Dispatch (IO, Transport, 10, 99, Request, Reply);
         Saved := Client.Uncertain (IO);
         Check (Reply.Status = Client.Indeterminate and then Client.Pending (IO)
           and then Saved.Present and then Saved.Operation = Reply.Operation
           and then Saved.Item = Request and then Transport.H.Writes = 1);
         Before := Transport.Calls;
         Client.Dispatch (IO, Transport, 10, 99, Request, Reply);
         Check (Reply.Status = Client.Rejected and then Transport.Calls = Before
           and then Client.Uncertain (IO) = Saved);
         Wrong := Saved; Wrong.Operation := Wrong.Operation + 1;
         Client.Confirm_Completion (IO, Wrong, True, Accepted); Check (not Accepted);
         Wrong := Saved; Wrong.Item.Register := Wrong.Item.Register + 1;
         Client.Confirm_Completion (IO, Wrong, True, Accepted); Check (not Accepted);
         Client.Confirm_Completion (IO, Saved, False, Accepted); Check (not Accepted);
         Client.Retire_Confirmation (IO, Saved, True, Accepted); Check (not Accepted);
         if Behavior = Unknown_Hardware then
            Check (R.Busy (Transport.Region) and then Transport.Ticket /= 0);
            R.Finish_Access (Transport.Region, Transport.Ticket, Accepted); Check (Accepted);
         end if;
         Client.Confirm_Completion (IO, Saved, True, Accepted);
         Check (Accepted and then not Client.Pending (IO) and then Client.Reconciled (IO, Saved));
         Client.Dispatch (IO, Transport, 10, 99, Request, Reply);
         Check (Reply.Status = Client.Rejected and then Transport.Calls = Before);
         Client.Retire_Confirmation (IO, Saved, False, Accepted); Check (not Accepted);
         Client.Retire_Confirmation (IO, Wrong, True, Accepted); Check (not Accepted);
         Client.Retire_Confirmation (IO, Saved, True, Accepted);
         Check (Accepted and then not Client.Reconciled (IO, Saved)
           and then Transport.H.Writes = 1 and then Transport.Calls = Before);
         Transport.Behavior := Normal;
         Request.For_Write := False; Request.Value := 0;
         Client.Dispatch (IO, Transport, 10, 99, Request, Reply);
         Check (Reply.Operation > Saved.Operation);
         Check ((if Behavior = Unknown_Hardware then Reply.Status = Client.Rejected
            else Reply.Status = Client.Completed and then Reply.Value = Unsigned_64'Last));
         Check (Transport.H.Writes = 1);
      end;
   end loop;
   declare
      IO : Client.Context;
      Transport : Transport_State;
      C : R.Configuration := Configuration;
      Reply : Client.Result;
      Accepted : Boolean;
   begin
      C.Register_Width := T.Byte_Access;
      C.Register_Authority.Write_Mask := 16#FF#;
      R.Install (Transport.Region, C, Accepted); Check (Accepted);
      Client.Initialize (IO, Endpoint_Value, Accepted); Check (Accepted);
      Transport.H.Value := Unsigned_64'Last;
      Client.Dispatch (IO, Transport, 10, 99, Request, Reply);
      Check (Reply.Status = Client.Failed_Completed and then not Client.Pending (IO)
        and then not R.Active (Transport.Region) and then not R.Busy (Transport.Region));
   end;
   declare
      First, Second : Client.Context;
      One, Two : Transport_State;
      Reply : Client.Result;
      Saved_One, Saved_Two : Client.Observation;
      Accepted : Boolean;
   begin
      R.Install (One.Region, Configuration, Accepted); Check (Accepted);
      R.Install (Two.Region, Configuration, Accepted); Check (Accepted);
      Client.Initialize (First, Endpoint_Value, Accepted); Check (Accepted);
      Client.Initialize (Second, Endpoint_Value, Accepted); Check (Accepted);
      One.Behavior := Lost_Reply; Two.Behavior := Lost_Reply;
      Client.Dispatch (First, One, 10, 99, Request, Reply);
      Saved_One := Client.Uncertain (First);
      Client.Dispatch (Second, Two, 10, 99, Request, Reply);
      Saved_Two := Client.Uncertain (Second);
      Check (Saved_One.Operation = Saved_Two.Operation and then Saved_One.Item = Saved_Two.Item
        and then Saved_One /= Saved_Two);
      Client.Confirm_Completion (Second, Saved_One, True, Accepted);
      Check (not Accepted and then Client.Pending (Second));
      Client.Confirm_Completion (First, Saved_Two, True, Accepted);
      Check (not Accepted and then Client.Pending (First));
      Client.Confirm_Completion (First, Saved_One, True, Accepted); Check (Accepted);
      Client.Confirm_Completion (Second, Saved_Two, True, Accepted); Check (Accepted);
   end;
   Ada.Text_IO.Put_Line ("Region client checks" & Checks'Image);
end Region_Client_Tests;
