with Ada.Text_IO;
with HID_Report_Schema;
with HID_Field_Usages;
with HID_Input_Values;
with HID_Mouse_Fallback;
with Pointer_Pending;
with Laptop_Report;
procedure Mouse_Tests is
 package S is new HID_Report_Schema(8,16);
 package N is new HID_Field_Usages(S);
 package V is new HID_Input_Values(S);
 package M is new HID_Mouse_Fallback(S,N,V);
 package L is new Laptop_Report(S);
 use type S.Status,Pointer_Pending.Button_State;
 Fields:S.Field_Table(1..256);Usages:S.Usage_Table(1..512);Collections:S.Collection_Table(1..64);
 Layout:S.Result;Plan:M.Model;Report:Pointer_Pending.Report;
 Data:S.Bytes(1..9):=[1,3,10,251,0,0,0,0,0];
 Matched,Accepted:Boolean;Checks:Natural:=0;
 procedure Check(Good:Boolean) is
 begin Checks:=Checks+1;if not Good then raise Program_Error with "mouse check" & Checks'Image;end if;end;
begin
 S.Parse(L.Data,Fields,Usages,Collections,Layout);Check(Layout.Outcome=S.Decoded);
 M.Compile(Layout,Fields,Usages,Collections,Plan);Check(Plan.Ready);
 M.Decode(Plan,Layout,Data,Matched,Accepted,Report);Check(Matched and then Accepted);
 Check(Report.X=10 and then Report.Y=5 and then Report.Buttons=3);
 for X in Integer range -127..127 loop
  for Buttons in Integer range 0..3 loop
   Data(2):=S.Byte(Buttons);Data(3):=S.Byte(X mod 256);Data(4):=S.Byte((-X)mod 256);
   M.Decode(Plan,Layout,Data,Matched,Accepted,Report);Check(Matched and then Accepted);
   Check(Report.X=X and then Report.Y=X
    and then Report.Buttons=Pointer_Pending.Button_State(Buttons));
  end loop;
 end loop;
 Data(3):=128;M.Decode(Plan,Layout,Data,Matched,Accepted,Report);Check(Matched and then not Accepted);
 M.Decode(Plan,Layout,Data(1..8),Matched,Accepted,Report);Check(Matched and then not Accepted);
 Data(1):=16#54#;M.Decode(Plan,Layout,Data,Matched,Accepted,Report);Check(not Matched and then not Accepted);
 Ada.Text_IO.Put_Line("mouse checks" & Checks'Image);
end Mouse_Tests;
