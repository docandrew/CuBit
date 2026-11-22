with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;
with Intel_GPU_ADLN_GT_Settings;
with Intel_GPU_Reset_Pages;
with Intel_GPU_MCR_Access;
package body Intel_GPU_Native_GT_Settings is
   use Interfaces;
   package Settings renames Intel_GPU_ADLN_GT_Settings;
   Active : Boolean := False;
   function Find (Offset : Unsigned_32) return Settings.Setting is
      Plan : constant Settings.Plan := Settings.Build (Inventory, Topology);
   begin
      for I in 1 .. Plan.Count loop
         if Plan.Items (I).Offset = Offset then return Plan.Items (I); end if;
      end loop;
      return (others => <>);
   end Find;
   function Address_For (Offset : Unsigned_32) return Unsigned_64 is
   begin
      if Offset /= 16#FDC# and then Find (Offset).Offset = 0 then return 0; end if;
      for Page in Intel_GPU_Reset_Pages.Page_Index loop
         if Intel_GPU_Reset_Pages.Offset (Page) = Unsigned_64 (Offset / 4096 * 4096) then
            return Intel_GPU_Reset_Pages.Virtual_Base + Unsigned_64 (Page) * 4096 +
              Unsigned_64 (Offset mod 4096);
         end if;
      end loop;
      return 0;
   end Address_For;
   function Owned return Boolean is (Active and then Owner_Ready);
   function Raw_Read (Offset : Unsigned_32) return Unsigned_32 is
      Address_Value : constant Unsigned_64 := Address_For (Offset);
      Value : Unsigned_32;
   begin
      if Address_Value = 0 or else not Owned then return Unsigned_32'Last; end if;
      declare Register_Value : Unsigned_32 with Import, Volatile_Full_Access,
        Address => To_Address (Integer_Address (Address_Value));
      begin Value := Register_Value; end;
      return (if Owned then Value else Unsigned_32'Last);
   end Raw_Read;
   procedure Raw_Write (Offset, Value : Unsigned_32; Success : out Boolean) is
      Address_Value : constant Unsigned_64 := Address_For (Offset);
   begin
      Success := False;
      if Address_Value = 0 or else not Owned then return; end if;
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      declare Register_Value : Unsigned_32 with Import, Volatile_Full_Access,
        Address => To_Address (Integer_Address (Address_Value));
      begin Register_Value := Value; end;
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      Success := Owned;
   end Raw_Write;
   package Accessor is new Intel_GPU_MCR_Access (Owned, Raw_Read, Raw_Write);
   Access_State : Accessor.State;
   function Instance (Offset : Unsigned_32) return Natural is
      T : constant Intel_GPU_ADLN_Steering.Topology := Topology;
      Selected : Natural;
   begin
      if not T.Valid then return 6; end if;
      Selected := Intel_GPU_ADLN_Steering.Instance (T, Offset);
      if (T.DSS_Mask and Shift_Left (Unsigned_8'(1), Selected)) = 0 then return 6; end if;
      return Selected;
   end Instance;
   function Read32 (Offset : Unsigned_32; MCR : Boolean) return Unsigned_32 is
      S : constant Settings.Setting := Find (Offset);
      Value : Unsigned_32 := Unsigned_32'Last;
      OK : Boolean;
   begin
      if Active or else not Owner_Ready or else Accessor.Failed (Access_State) or else
        S.Offset = 0 or else S.MCR /= MCR
      then return Value; end if;
      Active := True;
      if MCR then
         Accessor.Access_Register (Access_State, Offset, Instance (Offset), False, Value, OK);
         if not OK then Value := Unsigned_32'Last; end if;
      else Value := Raw_Read (Offset);
      end if;
      Active := False; return Value;
   end Read32;
   procedure Write32 (Offset, Value : Unsigned_32; MCR : Boolean; Success : out Boolean) is
      S : constant Settings.Setting := Find (Offset);
      Prior : Unsigned_32 := 0;
      Copy : Unsigned_32 := Value;
   begin
      Success := False;
      if Active or else not Owner_Ready or else Accessor.Failed (Access_State) or else
        S.Offset = 0 or else S.MCR /= MCR
      then return; end if;
      Prior := Read32 (Offset, MCR);
      if Prior = Unsigned_32'Last then return; end if;
      if Value /= ((Prior and not S.Clear_Mask) or S.Set_Bits) then return; end if;
      Active := True;
      if MCR then
         Accessor.Access_Register (Access_State, Offset, Instance (Offset), True, Copy, Success);
      else Raw_Write (Offset, Value, Success);
      end if;
      Active := False;
   end Write32;
end Intel_GPU_Native_GT_Settings;
