pragma Ada_2022;
with CuBit.Grant_References;with ACPI_Query_Retry;
with ACPI_Protocol;with ACPI_Observer_Loans;with ACPI_Query_Sessions;
with ACPI_Native_Query_Codec;with ACPI_Native_Query_Input;
package body ACPI_Native_Query_Entry is
   package T renames ACPI_Query_Transport;
   package P renames ACPI_Passive_Query.Pages;
   package Refs renames CuBit.Grant_References;
   package Input is new ACPI_Native_Query_Input (ACPI_Native_Query_Codec.Decode);
   use type T.Word;
   use type Input.Admission.Outcome;
   procedure Acquire
     (Owner : in out Context; Loan : Refs.Reference; Expected_Owner : CuBit.Messages.Process_ID;
      Byte_Length : T.Word; Acquired : out Byte_IO.Acquisition; Accepted : out Boolean) is
      pragma Unreferenced(Owner);
   begin Byte_IO.Acquire(Loan,Expected_Owner,Byte_Length,Acquired,Accepted);end Acquire;
   procedure Copy_In(Owner:in out Context;Acquired:Byte_IO.Acquisition;Data:out P.Byte_Array;Accepted:out Boolean) is
      pragma Unreferenced(Owner);
   begin Byte_IO.Copy_In(Acquired,Data,Accepted);end Copy_In;
   procedure Copy_Out(Owner:in out Context;Acquired:Byte_IO.Acquisition;Data:P.Byte_Array;Accepted:out Boolean) is
      pragma Unreferenced(Owner);
   begin Byte_IO.Copy_Out(Acquired,Data,Accepted);end Copy_Out;
   procedure Return_One(Owner:in out Context;Loan:Refs.Reference;Acquired:Byte_IO.Acquisition;Accepted:out Boolean) is
      pragma Unreferenced(Owner);
      Held:Byte_IO.Acquisition:=Acquired;
   begin Byte_IO.Release(Loan,Held,Accepted);end Return_One;
   Unused_Reference:constant Refs.Reference:=(others=><>);
   package Loans is new ACPI_Observer_Loans
     (Context,CuBit.Messages.Process_ID,Refs.Reference,Unused_Reference,
      Byte_IO.Acquisition,Byte_IO.No_Acquisition,Acquire,Copy_In,Evaluate,Copy_Out,Return_One);
   function Details(Value:Loans.Report) return T.Reply_Details is
      Status:T.Call_Status;
   begin
      case Value.Status is
         when Loans.Completed=>Status:=T.Completed;
         when Loans.Denied=>Status:=T.Denied;
         when Loans.Invalid_Extent=>Status:=T.Invalid_Extent;
         when Loans.Busy=>Status:=T.Busy;
         when Loans.Acquire_Refused=>Status:=T.Acquire_Refused;
         when Loans.Copy_In_Failed=>Status:=T.Copy_In_Failed;
         when Loans.Evaluation_Refused=>Status:=T.Evaluation_Refused;
         when Loans.Invalid_Response=>Status:=T.Invalid_Response;
         when Loans.Copy_Out_Failed=>Status:=T.Copy_Out_Failed;
      end case;
      return(Status,Value.Written,Value.Required_Bytes);
   end Details;
   procedure Save(Owner:in out Context;Handle:out Reply_Handle;Accepted:out Boolean) is
      pragma Unreferenced(Owner);
   begin Save_Reply(Handle,Accepted);end Save;
   procedure Deliver(Owner:in out Context;Handle:Reply_Handle;Operation:Loans.Report;Delivered:out Boolean) is
      pragma Unreferenced(Owner);
   begin Answer(Handle,Details(Operation),Delivered);end Deliver;
   package Sessions is new ACPI_Query_Sessions(Loans,Reply_Handle,No_Reply,Save,Deliver);
   use type Sessions.Outcome;
   State:Sessions.State;
   Request_Data,Response_Data:P.Byte_Array(0..Buffer_Bytes-1);
   Stopping:Boolean:=False;
   Retry_State:ACPI_Query_Retry.State;
   function Quiet return Boolean is(not Sessions.Has_Saved_Reply(State));
   function Healthy return Boolean is(ACPI_Query_Retry.Healthy(Retry_State));
   function Wake_Deadline return T.Word is
     (if Quiet then CuBit.Messages.Wait_Forever else T.Word(ACPI_Query_Retry.Deadline(Retry_State)));
   procedure Observe_Time(Now:out T.Word;Accepted:out Boolean) is
      Available:Boolean;
   begin
      Read_Clock(Now,Available);
      ACPI_Query_Retry.Observe(Retry_State,ACPI_Query_Retry.Tick(Now),Available,Accepted);
   end Observe_Time;
   procedure Schedule is
   begin ACPI_Query_Retry.Schedule(Retry_State,ACPI_Query_Retry.Interval(Retry_Milliseconds));end Schedule;
   procedure Dispatch
     (Owner:in out Context;Sender:CuBit.Messages.Process_ID;Packet:CuBit.Messages.Message;
      Admission_Open:Boolean;Reply:out CuBit.Messages.Message;Handled,Reply_Now:out Boolean) is
      Decoded:constant Input.Classification:=Input.Classify(Sender,Packet,T.Word(Buffer_Bytes),T.Word(Buffer_Bytes));
      Result:Sessions.Report;Value:T.Reply_Details:=(Status=>T.Malformed,others=><>);
      Now:T.Word;Accepted:Boolean;
   begin
      Handled:=Decoded.Selected;Reply_Now:=True;Reply:=CuBit.Messages.NULL_MESSAGE;
      if not Handled then return;end if;
      case Decoded.Result.Status is
         when Input.Admission.Denied=>Value.Status:=T.Denied;
         when Input.Admission.Malformed=>Value.Status:=T.Malformed;
         when Input.Admission.Invalid_Sender=>Value.Status:=T.Invalid_Sender;
         when Input.Admission.Invalid_Reference=>Value.Status:=T.Invalid_Reference;
         when Input.Admission.Invalid_Extent=>Value.Status:=T.Invalid_Extent;
         when Input.Admission.Admitted=>
            if Stopping or else not Healthy or else not Admission_Open then Value.Status:=T.Busy;
            else
               Sessions.Begin_Request(State,Owner,ACPI_Protocol.Role_Tag(Packet.authorityTag),
                 Decoded.Result.Value.Sender,Decoded.Result.Value.Loan,Decoded.Result.Value.Length,
                 Request_Data,Response_Data,Result);
               Reply_Now:=Result.Immediate_Reply;
               if Result.Immediate_Reply then
                  Value:=(case Result.Status is
                    when Sessions.Denied=>(Status=>T.Denied,others=><>),
                    when Sessions.Busy=>(Status=>T.Busy,others=><>),
                    when Sessions.Invalid_Extent=>(Status=>T.Invalid_Extent,others=><>),
                    when others=>(Status=>T.Save_Refused,others=><>));
               else
                  if not Quiet then Observe_Time(Now,Accepted);if Accepted then Schedule;end if;end if;
                  return;
               end if;
            end if;
      end case;
      Reply:=ACPI_Native_Query_Codec.Reply(Value);
   end Dispatch;
   procedure Advance(Owner:in out Context;Did_Work:out Boolean) is
      Now:T.Word;Accepted:Boolean;Result:Sessions.Report;
   begin
      Did_Work:=False;if Quiet or else not Healthy then return;end if;
      Observe_Time(Now,Accepted);if not Accepted or else not ACPI_Query_Retry.Due(Retry_State,ACPI_Query_Retry.Tick(Now)) then return;end if;
      Sessions.Progress(State,Owner,Result);
      Did_Work:=not Sessions.Has_Saved_Reply(State);
      if not Did_Work then Observe_Time(Now,Accepted);if Accepted then Schedule;end if;end if;
   end Advance;
   procedure Begin_Stop is begin Stopping:=True;end Begin_Stop;
   procedure Drain(Owner:in out Context;Complete:out Boolean) is
      Result:Sessions.Report;
   begin
      if not Quiet then Sessions.Progress(State,Owner,Result);end if;
      Complete:=Quiet;
   end Drain;
end ACPI_Native_Query_Entry;
