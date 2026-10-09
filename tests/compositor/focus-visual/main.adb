with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Desktop_Protocol; use CuBit.Desktop_Protocol;
with CuBit.Desktop_Messages; use CuBit.Desktop_Messages;
procedure Main is
 function Send (Wire : Wire_Message) return Wire_Message is
  Msg : Message := From_Wire(Wire);
 begin Msg.tag:=capCall(CAP_SLOT_DESKTOP,Msg,Wait_Forever);return To_Wire(Msg);end;
 A,B : Creation_Result;
 Ignore : Wire_Message;
 Slept : Unsigned_64;
begin
 A:=Decode_Creation_Result(Send(Encode_Create((300,200,Window_Surface))));
 B:=Decode_Creation_Result(Send(Encode_Create((300,200,Window_Surface))));
 if A.Status/=Success or B.Status/=Success then
  debugPrint("TEST: FAIL focus windows" & ASCII.LF);return;
 end if;
 Ignore:=Send(Encode_Title((A.Surface,Make_Title("Focus One"))));
 Ignore:=Send(Encode_Title((B.Surface,Make_Title("Focus Two"))));
 debugPrint("TEST: focus windows ready" & ASCII.LF);
 loop Slept:=syscall(SYSCALL_SLEEP,1000);end loop;
end Main;
