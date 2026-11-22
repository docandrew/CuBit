pragma Ada_2022;
with CuBit.Handles.Test_Broker;
package body CuBit.Handles.IPC is
 function Describe(Item:Device_Memory) return Description is
 pragma Unreferenced(Item); begin return Inspected;end;
 function Reply_With(Requester:CuBit.Messages.Process_ID;Msg:CuBit.Messages.Message;Copy:Derivation) return Reply_Result is
 pragma Unreferenced(Requester);begin Transfers:=Transfers+1; Last_Rights:=Test_Broker.Kept(Copy); CuBit.Messages.Last_Reply:=Msg;
 case Next_Outcome is when Transferred=>return (Outcome=>Transferred);when Delivered_Refused=>return(Outcome=>Delivered_Refused,Why=>ABI.Not_Accepted);when Refused=>return(Outcome=>Refused,Why=>ABI.Not_Accepted);end case;end;
end CuBit.Handles.IPC;
