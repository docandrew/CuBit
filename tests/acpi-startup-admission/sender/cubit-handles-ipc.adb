-- Hosted test model only; never a native runtime/provider implementation.
with Sender_Model; with GPIO_Bootstrap_Payload; with CuBit.Grant_References;
package body CuBit.Handles.IPC is
 use Interfaces; use type GPIO_Bootstrap_Payload.Decode_Status;
 function Describe(Item : Endpoint) return Description is begin
 Sender_Model.Assert(Item=77);
 if not Sender_Model.Current then return (Current=>False); end if;
 return (Current=>True,Kind=>(if Sender_Model.Good_Kind then ABI.Endpoint else ABI.No_Capability),
 Held=>(Read=>Sender_Model.Read_Right),Authority_Tag=>(if Sender_Model.Good_Tag then 16#4657# else 0),Object=>(if Sender_Model.Good_Peer then 16#2_0000_0009# else 0));
 end Describe;
 function Call(Target : Endpoint; Msg : in out CuBit.Messages.Message; Deadline : Unsigned_64) return CuBit.Messages.MessageTag is
 package P renames GPIO_Bootstrap_Payload;
 R : P.Request; Decoded : P.Decode_Result; Request : constant Unsigned_64 := Msg.words(0);
 Answer : constant Unsigned_64 := (if Sender_Model.Mode in 1 .. 17 then Unsigned_64(Sender_Model.Mode) elsif Sender_Model.Mode=51 then 6 else 1);
 begin
 Sender_Model.Calls:=Sender_Model.Calls+1;
 Sender_Model.Assert(Target=77 and Deadline=(if Sender_Model.Calls=1 then Sender_Model.Times(1)+1000 else Sender_Model.Times(3)+1000));
 Sender_Model.Assert(Msg.tag.label=16#251# and Msg.tag.length=3 and Msg.tag.protocol=0 and Msg.tag.reserved=0);
 Sender_Model.Assert(Request=16#1_0000_0011# and Msg.words(1)=CuBit.Grant_References.Encode((slot=>5,generation=>16#1234_5678#)) and Msg.words(2)=120 and Msg.words(3)=0);
 P.Decode(Sender_Model.Buffer(0 .. 119),R,Decoded);
 Sender_Model.Assert(Decoded.Status=P.Decoded and then R.Epoch=16#1_0000_0007# and then R.Spans=1 and then R.Path(1)="_SB_" and then R.Path(2)="GPI0");
 Sender_Model.Assert((for all B of Sender_Model.Buffer(120 .. Sender_Model.Buffer'Last)=>B=0));
 if (Sender_Model.Calls=1 and Sender_Model.Mode in 33 | 34 | 46 | 47 | 48 | 49 | 51) or else (Sender_Model.Mode=34 and Sender_Model.Calls=2) then Msg.tag.label:=CuBit.Messages.Kernel_Reply_First; return Msg.tag; end if;
 Msg.tag:=(label=>16#251#,length=>4,others=><>);
 Msg.words:=[Request,Answer,(if Answer=1 then 1 else 0),4];
 case Sender_Model.Mode is
 when 35 | 49 => Msg.tag.label:=0;
 when 36 => Msg.tag.length:=3;
 when 37 => Msg.tag.protocol:=1;
 when 38 => Msg.tag.reserved:=1;
 when 39 => Msg.words(0):=Request+1;
 when 40 => Msg.words(1):=0;
 when 41 => Msg.words(2):=0;
 when 42 => Msg.words(3):=0;
 when 50 => if Sender_Model.Calls=1 then Msg.tag.label:=0; end if;
 when 52 => Msg.words(1):=18;
 when 53 => Msg.words(1):=Unsigned_64'Last;
 when others => null;
 end case;
 return Msg.tag;
 end Call;
end CuBit.Handles.IPC;