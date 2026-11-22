with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Inventory; use Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Engine_Settings;
with Intel_GPU_Engine_Configure;
procedure Engine_Configure_Tests is
   package S renames Intel_GPU_ADLN_Engine_Settings;
   Inventory : constant Intel_GPU_ADLN_Inventory.Inventory :=
     (True, [others => True], [others => True]);
   Plan : S.Settings_Plan;
   Registers : array (S.Settings_Array'Range) of Unsigned_32;
   Fault_Index : Positive := 10;
   Owned : Boolean := True;
   Writes, Reads, Scenario : Natural := 0;
   function Owner return Boolean is (Owned);
   function Find (Offset : Unsigned_32; MCR : Boolean) return Positive is
   begin
      for I in 1 .. Plan.Count loop
         if Plan.Entries (I).Offset = Offset then
            pragma Assert (MCR = Plan.Entries (I).CPU_Steered);
            return I;
         end if;
      end loop;
      raise Program_Error;
   end Find;
   function Read32 (Offset : Unsigned_32; MCR : Boolean) return Unsigned_32 is
      I : constant Positive := Find (Offset, MCR);
   begin
      Reads := Reads + 1;
      if Scenario = 5 and I = Fault_Index then return Unsigned_32'Last; end if;
      if Scenario = 7 and I = Fault_Index and Writes = I then
         return Registers (I) xor 1;
      end if;
      if Scenario = 2 and I = 2 then return Unsigned_32'Last; end if;
      if Scenario = 4 and I = 1 then return Registers (I) xor 1; end if;
      return Registers (I);
   end Read32;
   procedure Write32 (Offset, Value : Unsigned_32; MCR : Boolean; Success : out Boolean) is
      I : constant Positive := Find (Offset, MCR);
      Item : constant S.Setting := Plan.Entries (I);
   begin
      Writes := Writes + 1;
      if Item.Masked_Write then
         pragma Assert (Value = (Shift_Left (Item.Mask, 16) or Item.Value));
         Registers (I) := (Registers (I) and not Item.Mask) or Item.Value;
      else
         pragma Assert ((Value and not Item.Mask) = (Registers (I) and not Item.Mask));
         Registers (I) := Value;
      end if;
      Success := Scenario /= 1 and not (Scenario = 6 and I = Fault_Index);
      if Scenario = 3 then Owned := False; end if;
   end Write32;
   package C is new Intel_GPU_Engine_Configure (Owner, Read32, Write32);
   use type C.Result;
   Status : C.Result;
begin
   for Engine in Intel_GPU_ADLN_Inventory.Engine loop
      Plan := S.Build (Inventory, Engine, 3);
      for Case_Number in 0 .. 4 loop
         -- Entry2 exists only on render.
         if Case_Number /= 2 or Engine = Render then
            declare
               Attempt : C.Attempt;
               Old_Writes, Old_Reads : Natural;
            begin
               Scenario := Case_Number; Owned := True; Writes := 0; Reads := 0;
               Registers := [others => 16#12345678#];
               C.Configure (Attempt, Inventory, Engine, Status);
               case Case_Number is
                  when 0 => pragma Assert (Status = C.Ready and Writes = Plan.Count);
                  when 1 => pragma Assert (Status = C.Write_Failed and Writes = 1);
                  when 2 => pragma Assert (Status = C.Read_Failed and Writes = 1);
                  when 3 => pragma Assert (Status = C.Ownership_Lost and Writes = 1);
                  when 4 => pragma Assert (Status = C.Readback_Failed and Writes = 1);
               end case;
               Old_Writes := Writes; Old_Reads := Reads;
               C.Configure (Attempt, Inventory, Engine, Status);
               pragma Assert (Status = C.Rejected and Writes = Old_Writes and Reads = Old_Reads);
            end;
         end if;
      end loop;
   end loop;
   -- Every programmable permission slot is a fail-closed initialization gate.
   Plan := S.Build (Inventory, Render, 3);
   for Slot_Entry in 10 .. Plan.Count loop
      Fault_Index := Slot_Entry;
      for Fault in 5 .. 7 loop
         declare
            Attempt : C.Attempt;
            Old_Writes, Old_Reads : Natural;
         begin
            Scenario := Fault; Owned := True; Writes := 0; Reads := 0;
            Registers := [others => 16#12345678#];
            C.Configure (Attempt, Inventory, Render, Status);
            case Fault is
               when 5 => pragma Assert
                 (Status = C.Read_Failed and Writes = Slot_Entry - 1);
               when 6 => pragma Assert
                 (Status = C.Write_Failed and Writes = Slot_Entry);
               when 7 => pragma Assert
                 (Status = C.Readback_Failed and Writes = Slot_Entry);
            end case;
            Old_Writes := Writes; Old_Reads := Reads;
            C.Configure (Attempt, Inventory, Render, Status);
            pragma Assert (Status = C.Rejected and Writes = Old_Writes
                           and Reads = Old_Reads);
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Engine settings executor PASS: masked/RMW, steering dispatch, failures; mock MMIO only");
end Engine_Configure_Tests;
