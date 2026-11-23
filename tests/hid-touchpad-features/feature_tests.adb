with Ada.Text_IO;
with HID_Report_Schema;
with HID_Field_Usages;
with HID_Touchpad_Features;
with HID_Feature_Commands;
with Laptop_Report;
procedure Feature_Tests is
 package S is new HID_Report_Schema(8,16);
 package N is new HID_Field_Usages(S);
 package F is new HID_Touchpad_Features(S,N);
 package W renames HID_Feature_Commands;
 package L is new Laptop_Report(S);
 use type S.Status,S.Report_Kind,S.Report_ID,S.Word,W.D.Bytes,W.D.Byte;
 Fields:S.Field_Table(1..256);Usages:S.Usage_Table(1..512);Collections:S.Collection_Table(1..64);
 Layout:S.Result;Plan:F.Plan;C:W.Command;
 Checks:Natural:=0;
 procedure Check(Good:Boolean) is
 begin Checks:=Checks+1;if not Good then raise Program_Error with "feature check" & Checks'Image;end if;end;
begin
 S.Parse(L.Data,Fields,Usages,Collections,Layout);Check(Layout.Outcome=S.Decoded);
 F.Build(Layout,Fields,Usages,16#22#,16#23#,Plan);
 Check(Plan.Count=2 and then Plan.Mode_Advertised and then Plan.Mode_Supported and then not Plan.Refused);
 Check(Plan.Commands(1).Mode and then not Plan.Commands(1).Surface and then Plan.Commands(1).ID=3);
 Check(Plan.Commands(1).Length=11 and then Plan.Commands(1).Data(1..11)=[16#22#,0,16#33#,3,16#23#,0,5,0,3,3,0]);
 Check(Plan.Commands(2).Surface and then Plan.Commands(2).Buttons and then not Plan.Commands(2).Mode);
 Check(Plan.Commands(2).Length=11 and then Plan.Commands(2).Data(1..11)=[16#22#,0,16#35#,3,16#23#,0,5,0,5,3,0]);
 declare Saved : constant S.Field_Table := Fields; begin
  for Index in 1 .. Natural(Layout.Fields_Required) loop
   if Fields(Index).Kind=S.Feature and then Fields(Index).ID=3 then
    Fields(Index).Size:=1;exit;
   end if;
  end loop;
  F.Build(Layout,Fields,Usages,16#22#,16#23#,Plan);
  Check(Plan.Mode_Advertised and then not Plan.Mode_Supported and then Plan.Refused and then Plan.Count=1);
  Fields:=Saved;
  for Index in 1 .. Natural(Layout.Fields_Required) loop
   if Fields(Index).Kind=S.Feature and then Fields(Index).ID=5 and then (Fields(Index).Flags and 1)/=0 then
    Fields(Index).Flags:=2;exit;
   end if;
  end loop;
  F.Build(Layout,Fields,Usages,16#22#,16#23#,Plan);
  Check(Plan.Mode_Supported and then Plan.Refused and then Plan.Count=1 and then Plan.Commands(1).Mode);
  Fields:=Saved;
 end;
 for ID in W.D.Byte loop
   C:=W.Encode(16#ABCD#,16#1234#,ID,[16#A5#,16#5A#],True,False,False);
   Check(C.Data(1..2)=[16#CD#,16#AB#] and then C.Data(4)=3);
   case ID is
     when 0 => Check(C.Data(3)=16#30# and then C.Length=10);
     when 1 .. 14 => Check(C.Data(3)=16#30# + ID and then C.Length=11);
     when others => Check(C.Data(3)=16#3F# and then C.Data(5)=ID and then C.Length=12);
   end case;
   Check(C.Data(C.Length-1..C.Length)=[16#A5#,16#5A#]);
 end loop;
 Ada.Text_IO.Put_Line("feature checks" & Checks'Image);
end Feature_Tests;
