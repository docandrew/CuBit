with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;
with Intel_GPU_Combo_PHY;
with Intel_GPU_Combo_Restore;
with Intel_GPU_Native_Combo_State;
with Intel_GPU_PHY_Pages;
package body Intel_GPU_Native_Combo_Restore is
   Active, Local_Owner, Succeeded : Boolean := False;
   function Last_Succeeded return Boolean is (Succeeded);
   function Available return Boolean is
     (Local_Owner and then PW1_Held and then DC_Disabled and then Pages_Ready);
   function Held return Boolean is (Active and then Available);
   function Begin_Scope return Boolean is
   begin
      if Active or else not Available then return False; end if;
      Active := True; return True;
   end Begin_Scope;
   procedure End_Scope is
   begin Active := False; end End_Scope;
   package Reader is new Intel_GPU_Native_Combo_State (Held);
   procedure Read_State (Port : Intel_GPU_Combo_PHY.PHY;
                         State : out Intel_GPU_Combo_PHY.Snapshot;
                         Success : out Boolean) is
      use type Reader.Outcome;
      Sample : constant Reader.Observation := Reader.Capture (Local_Owner, Port);
   begin
      State := Sample.Values; Success := Sample.Status = Reader.Collected;
   end Read_State;
   procedure Write_Register (Offset, Value : Unsigned_32; Success : out Boolean) is
      Target : constant Unsigned_64 := Intel_GPU_PHY_Pages.Write_Address (Offset);
   begin
      Success := False;
      if Target = 0 or else not Held then return; end if;
      declare
         Register_Value : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Target));
      begin Register_Value := Value; end;
      Success := True;
   end Write_Register;
   procedure Finish_Writes (Success : out Boolean) is
      Posting : Unsigned_32;
   begin
      Success := False;
      if not Held then return; end if;
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      declare
         -- Read from the same device BAR to drain posted writes. Avoid reading
         -- a group-write alias: use PHY A's readable MISC register instead.
         Value : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (16#60064C00#);
      begin Posting := Value; end;
      Success := Posting /= Unsigned_32'Last and then Held;
      -- This is ordering/readback, not GPU engine completion. The executor
      -- separately captures and verifies each restored PHY's register state.
   end Finish_Writes;
   package Restore is new Intel_GPU_Combo_Restore
     (Begin_Scope, End_Scope, Held, Read_State, Write_Register, Finish_Writes);
   function Execute (Owner : Boolean) return String is
      Result : Restore.Report;
      use type Restore.Outcome;
   begin
      if Active then return "REJECTED"; end if;
      Local_Owner := Owner;
      Restore.Execute (Owner, Result);
      Succeeded := Result.Status = Restore.Ready;
      return Restore.Outcome'Image (Result.Status);
   end Execute;
end Intel_GPU_Native_Combo_Restore;
