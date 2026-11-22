pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Handles;
with CuBit.Messages;
with ACPI_Register_Transport;
with ACPI_Region_Protocol;
with ACPI_Region_Policy;
with Transport_Model;
procedure Register_Transport_Tests is
   package Native renames ACPI_Register_Transport;
   package Client renames Native.Client;
   package P renames ACPI_Region_Protocol;
   package M renames Transport_Model;
   package R renames ACPI_Region_Policy;
   use type Client.Completion;
   use type Client.Request_ID;
   use type Client.Observation;
   use type P.Register_Request;
   use type P.Delivery;
   use type M.Mode;
   use type CuBit.Messages.MessageWords;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "native transport check" & Checks'Image; end if;
   end Check;
   Request : P.Register_Request :=
     (Epoch => 1, Register => M.Register_ID, For_Write => False, Value => 0);
   Values : constant array (Positive range 1 .. 8) of Unsigned_64 :=
     [0, 1, 16#FFFF_FFFF#, 16#1_0000_0000#, 16#8000_0000_0000_0000#,
      16#FEDC_BA98_7654_3210#, Unsigned_64'Last - 1, Unsigned_64'Last];
begin
   declare
      Transport : Native.State;
      Packet, Reply : P.Packet;
      Delivered : P.Delivery;
   begin
      M.Reset;
      for Bad in 1 .. 9 loop
         Packet := P.Encode (Request);
         case Bad is
            when 1 => Packet.Label := 2;
            when 2 => Packet.Length := 3;
            when 3 => Packet.Length := 5;
            when 4 => Packet.Flags := 1;
            when 5 => Packet.Reserved := 1;
            when 6 => Packet.Data (3) := 1;
            when 7 => Packet.Data (0) := 0;
            when 8 => Packet.Data (1) := 0;
            when 9 => Packet.Data (2) := 1;
         end case;
         Native.Exchange (Transport, CuBit.Handles.Test_Endpoint, 99, Packet, Reply, Delivered);
         Check (Delivered = P.Not_Dispatched and then M.Calls = 0 and then M.Writes = 0);
      end loop;
      Packet := P.Encode (Request);
      Native.Exchange (Transport, CuBit.Handles.No_Endpoint, 99, Packet, Reply, Delivered);
      Check (Delivered = P.Not_Dispatched and then M.Calls = 0);
      Native.Exchange (Transport, CuBit.Handles.Test_Endpoint,
        P.No_Finite_Deadline, Packet, Reply, Delivered);
      Check (Delivered = P.Not_Dispatched and then M.Calls = 0);
      Native.Exchange (Transport, CuBit.Handles.Test_Endpoint, 10, Packet, Reply, Delivered);
      Check (Delivered = P.Not_Dispatched and then M.Calls = 0);
   end;
   declare
      IO : Client.Context;
      Reply : Client.Result;
      Accepted : Boolean;
      Previous : Client.Request_ID := Client.No_Request;
      Before : Natural;
   begin
      M.Reset;
      Native.Dispatch (IO, 99, Request, Reply);
      Check (Reply.Status = Client.Rejected and then M.Calls = 0);
      Client.Initialize (IO, CuBit.Handles.No_Endpoint, Accepted); Check (not Accepted);
      Client.Initialize (IO, CuBit.Handles.Test_Endpoint, Accepted); Check (Accepted);
      Client.Initialize (IO, CuBit.Handles.Other_Endpoint, Accepted); Check (not Accepted);
      for Value of Values loop
         Request.For_Write := True; Request.Value := Value;
         Native.Dispatch (IO, 99, Request, Reply);
         Check (Reply.Status = Client.Completed and then Reply.Value = 0
           and then Reply.Operation > Previous and then M.Cell = Value);
         Previous := Reply.Operation;
         Check (M.Last_Message.words = CuBit.Messages.MessageWords'
           [1, Unsigned_64 (M.Register_ID), Value, 0]);
         Check (M.Last_Message.authorityTag = 0 and then M.Last_Deadline = 99);
         Request.For_Write := False; Request.Value := 0;
         Native.Dispatch (IO, 99, Request, Reply);
         Check (Reply.Status = Client.Completed and then Reply.Value = Value
           and then Reply.Operation > Previous and then not Client.Pending (IO));
         Previous := Reply.Operation;
      end loop;
      Before := M.Calls;
      Native.Dispatch (IO, P.No_Finite_Deadline, Request, Reply);
      Check (Reply.Status = Client.Rejected and then M.Calls = Before);
      Native.Dispatch (IO, 10, Request, Reply);
      Check (Reply.Status = Client.Rejected and then M.Calls = Before);
      M.Now := Unsigned_64'Last;
      Native.Dispatch (IO, 99, Request, Reply);
      Check (Reply.Status = Client.Rejected and then M.Calls = Before);
      M.Now := 10; M.Advance_Clock := True; M.Next_Time := 100;
      Native.Dispatch (IO, 99, Request, Reply);
      Check (Reply.Status = Client.Rejected and then M.Calls = Before
        and then not Client.Pending (IO));
      M.Now := Unsigned_64'Last - 10;
      Native.Dispatch (IO, P.Time_Instant (Unsigned_64'Last - 1), Request, Reply);
      Check (Reply.Status = Client.Completed and then M.Last_Deadline = Unsigned_64'Last - 1);
      M.Now := 10; Request.Epoch := P.Epoch_Number (Unsigned_64'Last);
      Before := M.Reads;
      Native.Dispatch (IO, 99, Request, Reply);
      Check (Reply.Status = Client.Rejected and then M.Reads = Before
        and then M.Last_Message.words (0) = Unsigned_64'Last and then not Client.Pending (IO));
      Request.Epoch := 1;
   end;
   for Behavior in M.Timeout_After_Effect .. M.Unknown_Hardware loop
      declare
         IO : Client.Context;
         Reply : Client.Result;
         Saved, Wrong : Client.Observation;
         Accepted : Boolean;
         Before : Natural;
      begin
         M.Reset; M.Behavior := Behavior;
         Client.Initialize (IO, CuBit.Handles.Test_Endpoint, Accepted); Check (Accepted);
         Request.For_Write := True; Request.Value := Unsigned_64'Last;
         Native.Dispatch (IO, 99, Request, Reply);
         Saved := Client.Uncertain (IO);
         Check (Reply.Status = Client.Indeterminate and then Client.Pending (IO)
           and then Saved.Present and then Saved.Item = Request
           and then Saved.Operation = Reply.Operation and then M.Calls = 1
           and then M.Writes = 1 and then M.Cell = Unsigned_64'Last);
         Before := M.Calls;
         Native.Dispatch (IO, 99, Request, Reply);
         Check (Reply.Status = Client.Rejected and then M.Calls = Before
           and then Client.Uncertain (IO) = Saved);
         Wrong := Saved; Wrong.Operation := Wrong.Operation + 1;
         Client.Confirm_Completion (IO, Wrong, True, Accepted); Check (not Accepted);
         Client.Confirm_Completion (IO, Saved, False, Accepted); Check (not Accepted);
         if Behavior = M.Unknown_Hardware then
            Check (R.Busy (M.Region) and then not R.Active (M.Region) and then M.Ticket /= 0);
            M.Finish (Accepted); Check (Accepted);
         else
            Check (not R.Busy (M.Region) and then M.Ticket = 0);
         end if;
         Client.Confirm_Completion (IO, Saved, True, Accepted);
         Check (Accepted and then Client.Reconciled (IO, Saved) and then not Client.Pending (IO));
         Native.Dispatch (IO, 99, Request, Reply);
         Check (Reply.Status = Client.Rejected and then M.Calls = Before);
         Client.Retire_Confirmation (IO, Saved, False, Accepted); Check (not Accepted);
         Client.Retire_Confirmation (IO, Saved, True, Accepted);
         Check (Accepted and then M.Calls = Before and then M.Writes = 1);
         M.Behavior := M.Normal;
         Request.For_Write := False; Request.Value := 0;
         Native.Dispatch (IO, 99, Request, Reply);
         Check (Reply.Operation > Saved.Operation and then M.Writes = 1);
         Check ((if Behavior = M.Unknown_Hardware then Reply.Status = Client.Rejected
            else Reply.Status = Client.Completed and then Reply.Value = Unsigned_64'Last));
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Register transport checks" & Checks'Image);
end Register_Transport_Tests;
