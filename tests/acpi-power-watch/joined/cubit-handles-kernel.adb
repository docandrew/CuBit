with Owner_Test_Control;with Joined_Startup_Boundary;
package body CuBit.Handles.Kernel is
 function Call(Number:Unsigned_64;A0,A1,A2,A3,A4,A5,A6:Unsigned_64:=0) return Unsigned_64 is
  pragma Unreferenced(A1,A2,A4,A6);
  package B renames Joined_Startup_Boundary;
 begin
  if Number=Move_Reply and B.Allow_Watch_Save then
   if not B.Current_Watch_Reply then raise Program_Error with "save outside modeled receive";end if;
   B.Current_Watch_Reply:=False;
   for I in B.Watch_Live'Range loop
    if not B.Watch_Live(I) then B.Watch_Live(I):=True;B.Saved_Watches:=B.Saved_Watches+1;
     return 16#1_0000_0000#+Unsigned_64(I);end if;
   end loop;
   raise Program_Error with "modeled watch table full";
  end if;
  if Number=Reply_And_Consume and B.Allow_Watch_Save then
   if A0<=16#1_0000_0000# or A0>16#1_0000_0010# then raise Program_Error with "unknown saved reply";end if;
   declare I:constant Positive:=Positive(A0-16#1_0000_0000#);begin
    if not B.Watch_Live(I) then raise Program_Error with "saved reply consumed twice";end if;
    B.Watch_Live(I):=False;B.Answered_Watches:=B.Answered_Watches+1;
    B.Watch_Stopping_Only:=B.Watch_Stopping_Only and A3=3 and A5=1;return 1;
   end;
  end if;
  -- Inspecting the absent optional endpoint is a normal typed refusal.
  -- No inspection buffer is read or written; all other calls still fail.
  if Number=Inspect and A0=ABI.No_Handle then
   Owner_Test_Control.Null_Inspections:=Owner_Test_Control.Null_Inspections+1;
   return ABI.Refusal_Word(ABI.Invalid_Handle);
  end if;
  return raise Program_Error with "unexpected typed guest kernel call";
 end Call;
end CuBit.Handles.Kernel;
