pragma Ada_2022;
with Ada.Text_IO;
with CuBit.Process_IDs;
with GPIO_Control_Wire;
with GPIO_Request_Dispatch;
procedure Dispatcher_Tests is
   package W renames GPIO_Control_Wire;
   package P renames CuBit.Process_IDs;
   use type W.Word, W.Operation, W.Completion_Status, W.Control_Kind, P.Process_ID;
   Owner : constant P.Process_ID := P.From_Word (16#F123_4567_0000_1234#);
   Stranger : constant P.Process_ID := P.From_Word (16#F123_4568_0000_1234#);
   Bound_Badge : constant W.Word := 16#4750_1000_0000_00A0#;
   Checks, Saves, Offers, Saved_Answers, Current_Answers, Auth_Calls : Natural := 0;
   Save_OK, Take_OK, Deliver_OK : Boolean := True;
   Last_Reply, Retained_Reply : Natural := 0;
   Last_Item : W.Request;
   Last_Completion : W.Completion := (W.Arm, W.Refused, 0, 0);
   Last_Label : W.Operation := 0;
   Consumed : array (Positive range 1 .. 64) of Boolean := [others => False];
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
   function Authorized (Caller : P.Process_ID; Badge : W.Word) return Boolean is
   begin Auth_Calls := Auth_Calls + 1; return Caller = Owner and then Badge = Bound_Badge; end Authorized;
   procedure Save (Reply : out Natural; Saved : out Boolean) is
   begin Saves := Saves + 1; Reply := Saves; Last_Reply := Reply; Saved := Save_OK; end Save;
   procedure Answer_Saved (Reply : Natural; Label : W.Operation; Value : W.Completion; Delivered : out Boolean) is
   begin
      Check (Reply in Consumed'Range and then not Consumed (Reply)); Check (W.Valid (Value));
      Consumed (Reply) := True; Saved_Answers := Saved_Answers + 1; Last_Label := Label;
      Last_Completion := Value; Delivered := Deliver_OK;
   end Answer_Saved;
   procedure Answer_Current (Caller : P.Process_ID; Label : W.Operation; Value : W.Completion; Delivered : out Boolean) is
   begin
      Check (Caller = Owner); Check (W.Valid (Value)); Current_Answers := Current_Answers + 1;
      Last_Label := Label; Last_Completion := Value; Delivered := Deliver_OK;
   end Answer_Current;
   procedure Offer (Caller : P.Process_ID; Badge : W.Word; Item : W.Request; Reply : Natural; Taken : out Boolean) is
   begin
      Check (Caller = Owner and then Badge = Bound_Badge and then Save_OK);
      Check (Reply = Last_Reply and then not Consumed (Reply)); Offers := Offers + 1; Taken := Take_OK;
      if Taken then Last_Item := Item; Retained_Reply := Reply; end if;
   end Offer;
   package Dispatch is new GPIO_Request_Dispatch (Natural, Authorized, Save, Answer_Saved, Answer_Current, Offer);
   use type Dispatch.Outcome;
   R : Dispatch.Report;
   procedure Reject (Label : W.Operation; Protocol : W.Word; Words : W.Message_Words; Frame : Boolean := True) is
      Before : constant Natural := Offers;
   begin
      Dispatch.Handle (Owner, Bound_Badge, Label, Protocol, Words, Frame, R);
      Check (R.Result = Dispatch.Rejected_Delivered and then R.Status = W.Invalid_Frame);
      Check (Offers = Before and then Consumed (Last_Reply));
   end Reject;
begin
   Dispatch.Handle (Owner, Bound_Badge, W.OP_GPIO_ARM, 1, [0, 1000], True, R);
   Check (R.Result = Dispatch.Control_Retained and then Offers = 1 and then Saved_Answers = 0);
   Check (Retained_Reply = 1 and then not Consumed (Retained_Reply));
   Check (Last_Item.Kind = W.Arm and then W.Encode_Deadline (Last_Item.Limit) = 1000);
   Take_OK := False;
   Dispatch.Handle (Owner, Bound_Badge, W.OP_GPIO_ACK, 1, [W.Word'Last, W.Word'Last, W.Wait_Forever_Word], True, R);
   Check (R.Result = Dispatch.Rejected_Delivered and then R.Status = W.Busy);
   Check (Last_Completion.Kind = W.Acknowledge and then Last_Completion.Lease = W.Word'Last and then Last_Completion.Generation = W.Word'Last);
   Check (Consumed (2) and then not Consumed (1) and then Retained_Reply = 1);
   Answer_Saved (Retained_Reply, W.OP_GPIO_ARM, (W.Arm, W.Completed, W.Word'Last, 0), Deliver_OK);
   Check (Consumed (1));
   declare Before : constant Natural := Offers; begin
      Save_OK := False; Dispatch.Handle (Owner, Bound_Badge, W.OP_GPIO_ARM, 1, [0, 1000], True, R);
      Check (Offers = Before and then Current_Answers = 1 and then R.Status = W.Busy);
      Check (Saved_Answers = 2 and then not Consumed (Last_Reply));
   end;
   Save_OK := True; Take_OK := True;
   declare Before : constant Natural := Offers; begin
      Dispatch.Handle (Stranger, Bound_Badge, W.OP_GPIO_ARM, 1, [0, 1000], True, R);
      Check (R.Status = W.Unauthorized and then Offers = Before);
      Dispatch.Handle (Owner, Bound_Badge + 1, W.OP_GPIO_ARM, 1, [0, 1000], True, R);
      Check (R.Status = W.Unauthorized and then Offers = Before);
      declare Previous_Auth : constant Natural := Auth_Calls; begin
         Dispatch.Handle (P.No_Process, Bound_Badge, W.OP_GPIO_ARM, 1, [0, 1000], True, R);
         Check (R.Status = W.Unauthorized and then Auth_Calls = Previous_Auth and then Offers = Before);
      end;
   end;
   Reject (W.OP_GPIO_ARM, 1, [0, 1000], False);
   Reject (W.OP_GPIO_ARM, 2, [0, 1000]); Reject (W.OP_GPIO_ARM, 1, [0]);
   Reject (W.OP_GPIO_ARM, 1, [1, 1000]); Reject (W.OP_GPIO_EVENT, 1, [1, 2]);
   Reject (W.OP_GPIO_RELEASE, 1, [0, 1000]); Reject (W.OP_GPIO_ACK, 1, [42, 0, 1000]);
   Deliver_OK := False; Take_OK := False;
   Dispatch.Handle (Owner, Bound_Badge, W.OP_GPIO_RELEASE, 1, [W.Word'Last, 1000], True, R);
   Check (R.Result = Dispatch.Rejected_Lost and then R.Status = W.Busy and then Consumed (Last_Reply));
   Check (Last_Completion.Lease = W.Word'Last and then Last_Completion.Generation = 0);
   Save_OK := False; declare Before : constant Natural := Offers; begin
      Dispatch.Handle (Owner, Bound_Badge, W.OP_GPIO_ARM, 1, [0, 1000], True, R);
      Check (R.Result = Dispatch.Rejected_Lost and then Offers = Before and then Current_Answers = 2);
   end;
   Check (Last_Label = W.OP_GPIO_ARM);
   Ada.Text_IO.Put_Line ("GPIO dispatcher checks" & Checks'Image);
end Dispatcher_Tests;
