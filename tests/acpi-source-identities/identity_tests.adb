pragma Ada_2022;
with Ada.Text_IO;with Interfaces;with ACPI_Source_Paths;with ACPI_Protocol;with ACPI_Endpoint;
with ACPI_Native_Power_Identities;with ACPI_Power_Identity_Protocol;with ACPI_Requests;with CuBit.Messages;with Identity_Test_Control;
procedure Identity_Tests is
 package P renames ACPI_Source_Paths;package W renames ACPI_Power_Identity_Protocol;package M renames CuBit.Messages;package T renames Identity_Test_Control;
 use type Interfaces.Unsigned_64,Interfaces.Unsigned_32,M.MessageWords;
 Checks:Natural:=0;Value:P.Path;Server:ACPI_Requests.State;Q,Reply:M.Message:=M.NULL_MESSAGE;Handled:Boolean;
 procedure Check(B:Boolean;Why:String) is begin Checks:=Checks+1;if not B then raise Program_Error with Why;end if;end;
 procedure Packet is begin Q:=M.NULL_MESSAGE;Q.tag.length:=4;Q.authorityTag:=Interfaces.Unsigned_64(ACPI_Protocol.Observer_Tag);Q.words:=[23,1,0,6];end;
 procedure Read(Allowed:Boolean:=True) is begin ACPI_Native_Power_Identities.Dispatch(Server,Q,Allowed,Reply,Handled);end;
 Expected:Interfaces.Unsigned_64;
 Text:constant String:="\_SB_.I2C1.BAT0";
 Calls:Natural;
 begin
  for Len in 0..P.Maximum_Characters loop
   Value:=(State=>P.Known,Length=>Len,others=><>);
   for I in Value.Text'Range loop Value.Text(I):=Character'Val((I*17)mod 256);end loop;
   for Offset in 0..Len loop
    Expected:=0;
    for I in 0..6 loop
     if Offset+I<Len then Expected:=Expected+Interfaces.Unsigned_64(Character'Pos(Value.Text(Offset+I+1)))*256**I;end if;
    end loop;
    Check(P.Chunk(Value,Interfaces.Unsigned_64(Offset))=Expected,"complete chunk encoding/bounds");
   end loop;
   Check(P.Chunk(Value,Interfaces.Unsigned_64'Last)=0,"wide offset cannot wrap");
  end loop;
  Value.State:=P.Too_Long;Check(P.Chunk(Value,0)=0,"overlong names yield no prefix");Value.State:=P.Unknown;Check(P.Chunk(Value,0)=0,"unknown names empty");
  T.Value:=(State=>P.Known,Length=>Text'Length,others=><>);T.Value.Text(1..Text'Length):=Text;
  Packet;Read;Check(Handled and Reply.tag.label=ACPI_Endpoint.Reply_OK and Reply.words(0)=6 and Reply.words(1)=0 and Reply.words(2)=Text'Length,"native identity reply");
  Check(Reply.words(3)=P.Chunk(T.Value,0),"native exact first chunk");
  for Offset in 0..Text'Length loop Q.words(2):=Interfaces.Unsigned_64(Offset);Read;Check(Reply.words(1)=0 and Reply.words(3)=P.Chunk(T.Value,Q.words(2)),"all native offsets including end");end loop;
  Q.words(2):=Text'Length+1;Read;Check(Reply.words=[6,5,0,0],"offset past complete path refused");
  Packet;Q.words(3):=5;Read;Check(Reply.words=[6,2,0,0],"family generation mismatch is stale");
  Packet;Calls:=T.Calls;Read(False);Check(Reply.words=[0,1,0,0] and T.Calls=Calls,"closed admission has no metadata callback");
  Packet;Q.authorityTag:=0;Read;Check(Reply.tag.label=ACPI_Endpoint.Reply_Error and T.Calls=Calls,"forged observer denied without callback");
  for Variant in 1..7 loop
   Packet;
   case Variant is when 1=>Q.tag.length:=3;when 2=>Q.tag.reserved:=1;when 3=>Q.tag.protocol:=1;
    when 4=>Q.words(1):=Interfaces.Unsigned_64'Last;when 5=>Q.words(2):=Interfaces.Unsigned_64'Last;
    when 6=>Q.words(3):=0;when 7=>Q.words(3):=Interfaces.Unsigned_64'Last;end case;
   Read;Check(Reply.tag.label=ACPI_Endpoint.Reply_Error and T.Calls=Calls,"full-width shape refused before callback");
  end loop;
  Packet;T.Code:=W.Too_Long;Read;Check(Reply.words=[6,4,0,0],"native overlong path has no data");
  Q.words(0):=22;Read;Check(not Handled,"other native metric untouched");
  Ada.Text_IO.Put_Line("Source identity checks"&Checks'Image);
end Identity_Tests;
