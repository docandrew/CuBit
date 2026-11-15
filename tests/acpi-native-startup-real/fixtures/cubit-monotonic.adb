with Owner_Test_Control;
package body CuBit.Monotonic is
 package T renames Owner_Test_Control;
 use type T.Scenario;
 Hardware_Microseconds:constant Interfaces.Unsigned_64:=10;
 IPC_Milliseconds:constant Interfaces.Unsigned_64:=7_000_000;
 function Read return Reading is
 begin
  if T.Selected in T.Clock_Unavailable|T.Clock_Overflow then
   T.Native_Reads:=T.Native_Reads+1;return raise Program_Error with "early clock refusal must not read hardware clock";
  end if;
  T.Hardware_Clock_Reads:=T.Hardware_Clock_Reads+1;return (True,Hardware_Microseconds);
 end Read;
 function Milliseconds return Interfaces.Unsigned_64 is
 begin
  if T.Selected in T.Clock_Unavailable|T.Clock_Overflow then
   T.Native_Reads:=T.Native_Reads+1;return raise Program_Error with "early clock refusal must not read IPC clock";
  end if;
  T.IPC_Clock_Reads:=T.IPC_Clock_Reads+1;return IPC_Milliseconds;
 end Milliseconds;
end CuBit.Monotonic;
