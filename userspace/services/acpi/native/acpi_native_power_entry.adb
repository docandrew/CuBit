pragma Ada_2022;
with CuBit.Handles; with CuBit.Handles.IPC; with CuBit.Monotonic;
with ACPI_One_Service; with ACPI_One_Service.Power_Preparation;
with ACPI_Native_Power_Preparation; with ACPI_Power_Entry; with ACPI_Sleep_Entry;
package body ACPI_Native_Power_Entry is
   package Q renames ACPI_Requests; package One renames ACPI_One_Service;
   package P renames ACPI_One_Service.Power_Preparation;
   package H renames CuBit.Handles; package IPC renames CuBit.Handles.IPC;
   package M renames CuBit.Messages; package S renames ACPI_Sleep_Entry; package E renames S.E;
   use type Interfaces.Unsigned_64, Interfaces.Unsigned_32, Q.Phase, One.Runtime.Status.Phase;
   Runtime_Mode : Boolean := False;
   procedure Read_Prepared (Service : Q.State; Available : out Boolean;
      Registers : out S.Register_Model; A, B : out E.Sleep_Type) is
      Copy : constant P.Controller.Copied_Types := ACPI_Native_Power_Preparation.Current_Types (Service);
   begin
      Available := Copy.Available;
      Registers := (case Copy.Registers is when P.Objects.PM1A_Only => S.PM1A_Only,
         when P.Objects.PM1A_And_B => S.PM1A_And_B, when P.Objects.Hardware_Reduced => S.Hardware_Reduced,
         when P.Objects.No_Sleep_Registers => S.No_Registers);
      A := E.Sleep_Type (Copy.Type_A); B := E.Sleep_Type (Copy.Type_B);
   end Read_Prepared;
   procedure Consume_Prepared is
   begin ACPI_Native_Power_Preparation.Retire; end Consume_Prepared;
   function Owner_Ready (Service : Q.State) return Boolean is
     (Runtime_Mode and then Q.Current (Service) = Q.Complete
      and then not Q.Notifications_Pending (Service) and then not Q.Retained_Results_Pending (Service)
      and then One.Runtime.Current = One.Runtime.Status.Idle and then One.Root.Hardware.Admitting
      and then One.Root.Fields.Control_Idle and then One.Root.Fields.EC_Commands.Quiescent);
   procedure Read_Wait_Clock (Milliseconds : out Interfaces.Unsigned_64; Available : out Boolean) is
   begin Milliseconds := CuBit.Monotonic.Milliseconds; Available := Milliseconds /= M.Wait_Forever; end Read_Wait_Clock;
   procedure Save_Current (Token : out H.Reply; Saved : out Boolean) is
      Result : constant IPC.Saved_Reply := IPC.Save_Reply;
   begin Saved := Result.Saved; Token := (if Result.Saved then Result.Handle else H.No_Reply); end Save_Current;
   procedure Encode (ID : Interfaces.Unsigned_64; Status : E.Enter_Status; Reply : out M.Message) is
   begin
      Reply := M.NULL_MESSAGE; Reply.tag.label := E.Enter_Sleep_State; Reply.tag.length := E.Enter_Reply_Words;
      Reply.words (E.Reply_Request_Word) := ID;
      Reply.words (E.Enter_Status_Word) := Interfaces.Unsigned_64 (E.Enter_Status'Enum_Rep (Status));
      -- Detail and every unused word remain zero.
   end Encode;
   procedure Answer_Saved (Token : in out H.Reply; ID : Interfaces.Unsigned_64;
      Status : E.Enter_Status; Delivered : out Boolean) is
      Reply : M.Message;
   begin Encode (ID, Status, Reply); Delivered := IPC.Answer (Token, Reply); Token := H.No_Reply; end Answer_Saved;
   package Owner is new ACPI_Power_Entry
     (Q.State, H.Reply, H.No_Reply, Read_Prepared, Consume_Prepared, Owner_Ready,
      P.Read_Clock, Read_Wait_Clock, Save_Current, Answer_Saved, One.Root.Hardware.Sleep_Action);
   use type Owner.Delivery;
   Item : Owner.State;
   procedure Dispatch (Server : Q.State; Request : M.Message;
      Runtime_Selected : Boolean; Reply : out M.Message; Handled, Reply_Now : out Boolean) is
      Response : Owner.Response; Deferred : Boolean;
   begin
      Reply := M.NULL_MESSAGE; Reply_Now := True; Handled := Request.tag.label = E.Enter_Sleep_State;
      if not Handled then return; end if;
      Runtime_Mode := Runtime_Selected;
      Owner.Enter (Item, Server,
        (Stamp => Request.authorityTag, Length => Interfaces.Unsigned_64 (Request.tag.length),
         ID => Request.words (E.Request_Word), State => Request.words (E.State_Word),
         Confirm => Request.words (E.Confirm_Word), Unused => Request.words (3),
         Flags => M.Flag_Byte (Request.tag), Reserved => Request.tag.reserved), Response, Deferred);
      Reply_Now := not Deferred; Encode (Response.ID, Response.Status, Reply);
   end Dispatch;
   procedure Advance (Server : Q.State; Result : out Step_Outcome) is
      Delivery : Owner.Delivery;
      Was_Runnable : constant Boolean := Owner.Runnable (Item);
   begin
      Owner.Advance (Item, Server, Delivery);
      Result := (if Delivery = Owner.Answered then Replied elsif Delivery = Owner.Caller_Gone then Caller_Gone
        elsif Was_Runnable or else Owner.Runnable (Item) then Progress else Idle);
   end Advance;
   procedure Begin_Stop is
   begin ACPI_Native_Power_Preparation.Retire; Owner.Begin_Stop (Item); end Begin_Stop;
   function Runnable return Boolean is (Owner.Runnable (Item));
   function Blocks_Runtime return Boolean is (Owner.Blocks_Runtime (Item));
   function Quiet return Boolean is (Owner.Quiet (Item));
   function Wake_Deadline_Milliseconds return Interfaces.Unsigned_64 is
      Deadline : constant Interfaces.Unsigned_64 := Owner.Wake_Deadline_Milliseconds (Item);
   begin return (if Deadline = Owner.No_Deadline then M.Wait_Forever else Deadline); end Wake_Deadline_Milliseconds;
end ACPI_Native_Power_Entry;
