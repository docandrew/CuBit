pragma Ada_2022;
with Interfaces;with ACPI_Protocol;with ACPI_Endpoint;
with ACPI_Power_Identity_Protocol;with ACPI_Source_Paths;with ACPI_Cached_Power_Values;
with ACPI_Native_Event_Domain;
package body ACPI_Native_Power_Identities is
   package M renames CuBit.Messages;package P renames ACPI_Power_Identity_Protocol;
   package Paths renames ACPI_Source_Paths;package V renames ACPI_Cached_Power_Values;
   use type Interfaces.Unsigned_32,Interfaces.Unsigned_64,Interfaces.Unsigned_8,Interfaces.Unsigned_16,P.Result_Code;
   pragma Compile_Time_Error(P.Maximum_Path_Characters/=Interfaces.Unsigned_64(Paths.Maximum_Characters),"identity path bounds differ");
   procedure Dispatch(Server:ACPI_Requests.State;Request:M.Message;Read_Allowed:Boolean;
      Reply:out M.Message;Handled:out Boolean) is
      Code:P.Result_Code:=P.Not_Ready;Current:V.Generation:=0;Value:Paths.Path;
      Result:ACPI_Requests.Response:=(Status=>ACPI_Requests.OK,others=><>);Packet:ACPI_Requests.Packet;
   begin
      Reply:=M.NULL_MESSAGE;Handled:=Request.tag.label=P.Read_Metrics_Label and then Request.words(0)=P.Identity_Page;
      if not Handled then return;end if;
      if not ACPI_Protocol.Is_Observer(ACPI_Protocol.Role_Tag(Request.authorityTag)) then Result.Status:=ACPI_Requests.Denied;
      elsif Request.tag.length/=4 or M.Flag_Byte(Request.tag)/=0 or Request.tag.reserved/=0
        or Request.words(1)>P.Maximum_Battery_Index or Request.words(2)>P.Maximum_Path_Characters
        or Request.words(3)=0 or Request.words(3)>Interfaces.Unsigned_64(V.Generation'Last) then Result.Status:=ACPI_Requests.Malformed;
      else
         if Read_Allowed then ACPI_Native_Event_Domain.Read_Identity(Server,Natural(Request.words(1)),V.Generation(Request.words(3)),Current,Code,Value);end if;
         if Code=P.OK then
            if Request.words(2)>Interfaces.Unsigned_64(Value.Length) then Code:=P.Bad_Offset;
            else Result.Data(2):=ACPI_Requests.Revision_Number(Value.Length);Result.Data(3):=ACPI_Requests.Revision_Number(Paths.Chunk(Value,Request.words(2)));end if;
         end if;
         Result.Data(0):=ACPI_Requests.Revision_Number(Current);Result.Data(1):=P.Result_Code'Enum_Rep(Code);
      end if;
      Packet:=ACPI_Endpoint.Encode(Result);Reply.tag.label:=Packet.Label;Reply.tag.length:=Packet.Length;Reply.words:=M.MessageWords(Packet.Data);
   end Dispatch;
end ACPI_Native_Power_Identities;
