pragma Ada_2022;
with CuBit.Messages;
package CuBit.Handles.IPC is
 type Description(Current : Boolean := False) is record case Current is when True=>Kind:ABI.Kind:=ABI.No_Capability;Held:Rights;Authority_Tag,Object,Parameter:Unsigned_64:=0;when False=>Why:Refusal:=ABI.Invalid_Handle;end case;end record;
 Inspected : Description;
 function Describe(Item:Device_Memory) return Description;
 type Reply_Outcome is (Transferred,Delivered_Refused,Refused);
 type Reply_Result(Outcome:Reply_Outcome:=Refused) is record case Outcome is when Transferred=>null;when others=>Why:Refusal:=ABI.Not_Accepted;end case;end record;
 Next_Outcome : Reply_Outcome := Transferred;
 Transfers : Natural := 0; Last_Rights : Unsigned_64 := 0;
 function Reply_With(Requester:CuBit.Messages.Process_ID;Msg:CuBit.Messages.Message;Copy:Derivation) return Reply_Result;
end CuBit.Handles.IPC;
