pragma Ada_2022;
with Bridge_Trace; use Bridge_Trace;
with ACPI_Requests;
with ACPI_Native_Blocks;
with Server_Model;
package body CuBit.Handles.IPC is
 function Call(Target : Endpoint; Msg : in out CuBit.Messages.Message;
   Deadline : Unsigned_64) return CuBit.Messages.MessageTag is
  Request : constant ACPI_Requests.Packet := (Msg.tag.label,Msg.tag.length,
    CuBit.Messages.Flag_Byte(Msg.tag),Msg.tag.reserved,ACPI_Requests.Words(Msg.words));
  Reply : ACPI_Requests.Packet;
  Tag : CuBit.Messages.MessageTag;
 begin
  if not Is_Service(Target) or else Deadline=Unsigned_64'Last or else Deadline<=Now then raise Program_Error; end if;
  Note(IPC_Call); Calls:=Calls+1; Last_Message:=Msg; Last_Deadline:=Deadline;
  if Request.Label=ACPI_Requests.Configure_Notifications then Interest_Calls:=Interest_Calls+1; end if;
  if Request.Label=ACPI_Requests.Claim_Notification then Claim_Calls:=Claim_Calls+1; end if;
  if Request.Label=ACPI_Requests.Acknowledge_Notification then
   Ack_Calls:=Ack_Calls+1; Last_Ack:=Msg.words;
  end if;
  Server_Model.Route(Request,Reply);
  Tag:=(label=>Reply.Label,length=>Reply.Length,protocol=>Reply.Flags,ipc=><>,reserved=>Reply.Reserved);
  Msg.words:=CuBit.Messages.MessageWords(Reply.Data);
  -- Faults replace only the transport tag AFTER actual server execution.
  if (Mode=Lost_Start and then Request.Label=ACPI_Requests.Start_Snapshot) or else
     (Mode=Lost_Import and then Request.Label=ACPI_Native_Blocks.Import_Table_Grant) or else
     (Mode=Lost_Finish and then Request.Label=ACPI_Requests.Finish_Snapshot)
  then Tag.label:=CuBit.Messages.REPLY_TIMEOUT; end if;
  if (Fault=Lose_Query and then Request.Label=ACPI_Requests.Read_Metrics) or else
     (Fault=Lose_Interest and then Request.Label=ACPI_Requests.Configure_Notifications) or else
     (Fault=Lose_Claim and then Request.Label=ACPI_Requests.Claim_Notification) or else
     (Fault=Lose_Ack and then Request.Label=ACPI_Requests.Acknowledge_Notification)
  then Tag.label:=CuBit.Messages.REPLY_TIMEOUT; Fault:=No_Notify_Fault;
  elsif (Fault=Malform_Interest and then Request.Label=ACPI_Requests.Configure_Notifications) or else
        (Fault=Malform_Finish and then Request.Label=ACPI_Requests.Finish_Snapshot) or else
        (Fault=Malform_Receipt and then Request.Label=ACPI_Requests.Read_Notification)
  then Tag.length:=3; Fault:=No_Notify_Fault;
  end if;
  return Tag;
 end Call;
end CuBit.Handles.IPC;
