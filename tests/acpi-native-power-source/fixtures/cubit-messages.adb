with CuBit.Power_Event_Exchange;
package body CuBit.Messages is
 package E renames CuBit.Power_Event_Exchange;
 use type Interfaces.Unsigned_8;
 function Flag_Byte(Tag:MessageTag) return Interfaces.Unsigned_8 is
 (Interfaces.Unsigned_8(Tag.protocol) or Interfaces.Shift_Left(Interfaces.Unsigned_8(Tag.ipc),4));
 Mode:Scenario:=Default_Case;
 function Test_Word(Item:Process_ID) return Word is (Item.Value);
 procedure Reset(Kind:Scenario) is
 begin Mode:=Kind;Turn:=0;Replies:=0;Waits:=0;Current_Reply_Authority:=False;Last_Replied_PID:=0;end;
 function Poll_Completion(Into:System.Address) return Word is
 pragma Unreferenced(Into);begin return 0;end;
 function Poll_Event(Item:out Message;Publisher:out Process_ID) return Boolean is
 begin
  Turn:=Turn+1;Item:=(others=><>);Publisher:=(Value=>Test_PID_Word);
  if Turn=3 then Item.tag.label:=Stop_Label;return True;
  elsif Turn=2 and Mode=Press_Wakeup then Item.tag.label:=Press_Label;return True;
  end if;return False;
 end;
 procedure Poll_Service_Request(From:out Process_ID;Item:out Message;Found:out Boolean) is
 begin
  Found:=Turn<=2;From:=(Value=>Test_PID_Word);Item:=(others=><>);
  if not Found then return;end if;
  if Current_Reply_Authority then raise Program_Error with "previous current reply not consumed";end if;
  Current_Reply_Authority:=True;Item.words(0):=Word(Turn);
  if Turn=1 then Item.tag:=(label=>E.Button_Watch,length=>E.Watch_Request_Words,others=><>);Item.authorityTag:=E.Authority_Tag;
  else Item.tag.label:=Observer_Label;end if;
 end;
 function reply(To_Process:Process_ID;Item:Message) return Word is
 pragma Unreferenced(Item);
 begin
  if not Current_Reply_Authority then raise Program_Error with "immediate reply after saved authority";end if;
  Current_Reply_Authority:=False;Replies:=Replies+1;Last_Replied_PID:=To_Process.Value;return 1;
 end;
 function Wait_For_Activity_Until(Deadline:Word) return Wait_Result is
 pragma Unreferenced(Deadline);begin Waits:=Waits+1;return Unavailable;end;
end CuBit.Messages;
