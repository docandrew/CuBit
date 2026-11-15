with Owner_Test_Control;
package body CuBit.Handles.Kernel is
 function Call(Number:Unsigned_64;A0,A1,A2,A3,A4,A5,A6:Unsigned_64:=0) return Unsigned_64 is
  pragma Unreferenced(A1,A2,A3,A4,A5,A6);
 begin
  -- Inspecting the absent optional endpoint is a normal typed refusal.
  -- No inspection buffer is read or written; all other calls still fail.
  if Number=Inspect and A0=ABI.No_Handle then
   Owner_Test_Control.Null_Inspections:=Owner_Test_Control.Null_Inspections+1;
   return ABI.Refusal_Word(ABI.Invalid_Handle);
  end if;
  return raise Program_Error with "unexpected typed guest kernel call";
 end Call;
end CuBit.Handles.Kernel;
