pragma Ada_2022;
with ACPI_Service;with ACPI_Requests.Passive_Queries;with ACPI_Protocol;
with ACPI_Native_Query_Entry;with ACPI_Native_Query_Codec;
with ACPI_Runtime_Query_Grants;with ACPI_Runtime_Query_Replies;
with ACPI_Query_Transport;with CuBit.Handles;with CuBit.Process_IDs;with CuBit.Monotonic;
with Firmware_Tables;
package body ACPI_Native_Passive_Queries is
   package Q renames ACPI_Requests;package Owner_Queries renames Q.Passive_Queries;
   package D renames Owner_Queries.Data;package T renames ACPI_Query_Transport;
   use type D.Dispatcher.Outcome,T.Word;
   Buffer_Bytes:constant Positive:=4096;
   Maximum_Node_Visits:constant Positive:=256;
   Maximum_Resource_Items:constant D.Dispatcher.Resources.Data_IO.Item_Count:=64;
   Maximum_Atoms:constant Positive:=64;
   Nodes:D.Search.Node_Array(0..Maximum_Node_Visits-1);
   Parents:D.Paths.Node_Array(0..ACPI_Service.Max_Namespace_Nodes-1);
   Reader_Data,Text_Source:Firmware_Tables.Bytes(1..Buffer_Bytes);
   Reader_Atoms:D.Reader.Atoms(0..Maximum_Atoms-1);
   Filter,Payload:D.Pages.Byte_Array(0..Buffer_Bytes-1);
   procedure Evaluate(Server:in out Q.State;Request_Data:D.Pages.Byte_Array;
      Response_Data:out D.Pages.Byte_Array;Written,Required_Bytes:out T.Word;Accepted:out Boolean) is
      Self:constant CuBit.Process_IDs.Process_ID:=CuBit.Messages.Own_Process;
      Result:D.Dispatcher.Report;
   begin
      Written:=0;Required_Bytes:=0;Accepted:=False;Response_Data:=[others=>0];
      if not CuBit.Process_IDs.Is_Process(Self) then return;end if;
      -- Private callback, reached only after the observer stamp was checked
      -- by native admission and the loan/session owner. No request supplies it.
      Owner_Queries.Handle(Server,D.Wire.Instance_ID(CuBit.Process_IDs.To_Word(Self)),
        ACPI_Protocol.Observer_Tag,Request_Data,Maximum_Node_Visits,Maximum_Resource_Items,
        Nodes,Parents,Reader_Data,Reader_Atoms,Text_Source,Filter,Payload,Response_Data,Result);
      Accepted:=Result.Status=D.Dispatcher.Reply_Ready;
      Written:=Result.Written;Required_Bytes:=Result.Required_Bytes;
   end Evaluate;
   procedure Clock(Now:out T.Word;Available:out Boolean) is
   begin
      -- GETTIME/receive-deadline epoch, not the fallback HPET microsecond epoch.
      Now:=CuBit.Monotonic.Milliseconds;Available:=Now/=CuBit.Messages.Wait_Forever;
   end Clock;
   package Replies is new ACPI_Runtime_Query_Replies(T.Reply_Details,ACPI_Native_Query_Codec.Reply);
   package Native_Entry is new ACPI_Native_Query_Entry(Q.State,ACPI_Runtime_Query_Grants.IO,
     CuBit.Handles.Reply,CuBit.Handles.No_Reply,Replies.Save,Replies.Answer,Evaluate,Clock,Buffer_Bytes);
   procedure Dispatch(Server:in out Q.State;Sender:CuBit.Messages.Process_ID;
      Packet:CuBit.Messages.Message;Admission_Open:Boolean;
      Reply:out CuBit.Messages.Message;Handled,Reply_Now:out Boolean) renames Native_Entry.Dispatch;
   procedure Advance(Server:in out Q.State;Did_Work:out Boolean) renames Native_Entry.Advance;
   procedure Maintain(Deadline:out Interfaces.Unsigned_64;May_Continue:out Boolean) is
   begin Deadline:=Native_Entry.Wake_Deadline;May_Continue:=Native_Entry.Healthy;end Maintain;
   procedure Begin_Stop renames Native_Entry.Begin_Stop;
   procedure Drain(Server:in out Q.State;Complete:out Boolean) renames Native_Entry.Drain;
   function Quiet return Boolean renames Native_Entry.Quiet;
end ACPI_Native_Passive_Queries;
