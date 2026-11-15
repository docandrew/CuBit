with Sender_Model;with CuBit.Firmware_Data_Exchange;
package body CuBit.Handles.IPC is
 use Interfaces;
 function Describe(Item:Endpoint) return Description is begin
 Sender_Model.Assert(Item=77);if not Sender_Model.Current then return(Current=>False);end if;
 return(Current=>True,Kind=>(if Sender_Model.Good_Kind then ABI.Endpoint else ABI.No_Capability),Held=>(Read=>Sender_Model.Read_Right),Authority_Tag=>(if Sender_Model.Good_Tag then CuBit.Firmware_Data_Exchange.Authority_Tag else 0),Object=>(if Sender_Model.Good_Peer then 16#F234_5678_0000_0009# else 0));end;
 function Call(Target:Endpoint;Msg:in out CuBit.Messages.Message;Deadline:Unsigned_64) return CuBit.Messages.MessageTag is
 Request:constant Unsigned_64:=Msg.words(0);
 Phase:constant Unsigned_64:=(if Sender_Model.Mode in 41|52 then 2 elsif Sender_Model.Mode in 40|42|50 then 3 elsif Sender_Model.Mode in 43|51 then 4 else 1);
 Answer:constant Unsigned_64:=(if Sender_Model.Mode in 1..5 then Unsigned_64(Sender_Model.Mode) elsif Sender_Model.Mode=53 then 5 elsif Sender_Model.Mode=54 then 3 else 1);
 begin
 Sender_Model.Calls:=Sender_Model.Calls+1;
 Sender_Model.Assert(Target=77 and Deadline=(if Sender_Model.Mode=25 then 150 elsif Sender_Model.Mode=44 then 250 elsif Sender_Model.Mode=46 then Unsigned_64'Last-1 else (if Sender_Model.Calls=1 then 1100 else 1200)));
 Sender_Model.Assert(Msg.tag.label=16#253# and Msg.tag.length=3 and Msg.tag.protocol=0 and Msg.tag.ipc=0 and Msg.tag.reserved=0);
 Sender_Model.Assert(Request=(if Sender_Model.Mode=35 then Unsigned_64'Last else 16#F000_0000_0000_0001#) and Msg.words(1)=Sender_Model.Own_ID and Msg.words(2)=Phase and Msg.words(3)=0);
 if Sender_Model.Mode in 22..23|26..27|36..37|40|44|55..56 then
 if Sender_Model.Calls=1 or else Sender_Model.Mode=23 then Msg.tag.label:=CuBit.Messages.Kernel_Reply_First;return Msg.tag;end if;end if;
 Msg.tag:=(label=>16#253#,length=>3,others=><>);Msg.words:=[Request,Answer,0,0];
 case Sender_Model.Mode is
 when 6=>Msg.tag.label:=0;when 7=>Msg.tag.length:=2;when 8=>Msg.tag.protocol:=1;when 9=>Msg.tag.ipc:=1;
 when 10=>Msg.tag.reserved:=1;when 11=>Msg.words(0):=Request+1;when 12=>Msg.words(1):=0;when 13=>Msg.words(1):=6;
 when 14=>Msg.words(1):=2;Msg.words(2):=1;when 15=>Msg.words(3):=1;
 when 38=>Msg.words(1):=Unsigned_64'Last;when 39=>Msg.words(0):=Request+16#1_0000_0000#;
 when 47|50|52|53|55|58=>Msg.words(2):=1;
 when 48|51|54=>Msg.words(2):=Unsigned_64'Last;
 when 49=>Msg.words(2):=16#1_0000_0000#;
 when 57=>if Sender_Model.Calls=1 then Msg.tag.length:=2;end if;
 when others=>null;end case;return Msg.tag;
 end Call;
end CuBit.Handles.IPC;
