pragma Ada_2022;
with Interfaces;with CuBit.Messages;with CuBit.Handles.IPC;with CuBit.Memory_Grants;
with CuBit.Grant_References;with ACPI_Query_Client_Session;
package body ACPI_Native_Query_Client is
   package T renames ACPI_Query_Transport;
   package P renames ACPI_Passive_Query.Pages;
   package G renames CuBit.Memory_Grants;
   package Refs renames CuBit.Grant_References;
   package M renames CuBit.Messages;
   use type M.Call_Ending;
   type Aligned_Page is array(Natural range 0..Page_Bytes-1) of P.Byte with Alignment=>Page_Bytes,Volatile_Components;
   Page:Aligned_Page:=[others=>0];
   procedure Store(Data:P.Byte_Array;Accepted:out Boolean) is
   begin
      Accepted:=Data'Length=Page_Bytes and CuBit.Handles.Valid(Endpoint);if not Accepted then return;end if;
      for I in Page'Range loop Page(I):=Data(Data'First+I);end loop;
   end Store;
   procedure Create(Loan:out Refs.Reference;Accepted:out Boolean) is
   begin G.Create_Via_Capability(Endpoint,Page'Address,1,True,Loan,Accepted);end Create;
   procedure Call(Loan:Refs.Reference;Extent,Deadline:T.Word;Reply:out T.Packet;Answered:out Boolean) is
      Request:constant T.Packet:=T.Encode_Request(Refs.Encode(Loan),Extent);
      Message:M.Message:=M.NULL_MESSAGE;
      Tag:M.MessageTag;
   begin
      Message.tag.label:=Interfaces.Unsigned_32(Request.Label);
      Message.tag.length:=Interfaces.Unsigned_8(Request.Length);
      Message.words:=M.MessageWords(Request.Data);
      Tag:=CuBit.Handles.IPC.Call(Endpoint,Message,Deadline);
      Answered:=M.Ending_Of(Tag)=M.Call_Answered;
      Reply:=(Label=>T.Message_Label(Tag.label),Length=>T.Header_Byte(Tag.length),
        Flags=>T.Header_Byte(M.Flag_Byte(Tag)),Reserved=>T.Reserved_Field(Tag.reserved),
        Data=>T.Words(Message.words));
   end Call;
   procedure Revoke(Loan:Refs.Reference;Accepted:out Boolean) is
   begin G.Revoke(Loan,Accepted);end Revoke;
   function Confirmed(Loan:Refs.Reference)return Boolean is(G.Retirement_Confirmed(Loan));
   procedure Copy(Data:out P.Byte_Array;Accepted:out Boolean) is
   begin
      Accepted:=Data'Length=Page_Bytes;Data:=[others=>0];if not Accepted then return;end if;
      for I in Page'Range loop Data(Data'First+I):=Page(I);end loop;
   end Copy;
   No_Reference:constant Refs.Reference:=(others=><>);
   package Session is new ACPI_Query_Client_Session(Page_Bytes,Refs.Reference,No_Reference,
      Store,Create,Call,Revoke,Confirmed,Copy);
   procedure Start(Request_Data:P.Byte_Array;Deadline:T.Word;Accepted:out Boolean) renames Session.Start;
   procedure Progress(Did_Work:out Boolean) renames Session.Progress;
   function Status return ACPI_Query_Client_Lifetime.Phase renames Session.Status;
   function Cleanup_Pending return Boolean renames Session.Cleanup_Pending;
   procedure Take(Value:out Report;Data:out Buffer;Taken:out Boolean) renames Session.Take;
end ACPI_Native_Query_Client;
