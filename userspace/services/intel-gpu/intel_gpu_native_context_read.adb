with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;
with Intel_GPU_Reset_Pages;
with Intel_GPU_MCR_Access;
package body Intel_GPU_Native_Context_Read is
   use Interfaces;
   Active : Boolean := False;
   function Owned return Boolean is (Active and then Owner_Ready);
   function Selector_Address return Integer_Address is
   begin
      for Page in Intel_GPU_Reset_Pages.Page_Index loop
         if Intel_GPU_Reset_Pages.Offset (Page) = 0 then
            return Integer_Address (Intel_GPU_Reset_Pages.Virtual_Base +
              Unsigned_64 (Page) * 4096 + 16#FDC#);
         end if;
      end loop;
      return 0;
   end Selector_Address;
   function Read32 (Offset : Unsigned_32) return Unsigned_32 is
      Address_Value : Integer_Address;
      Value : Unsigned_32;
   begin
      if not Owned or else Offset not in 16#FDC# | 16#5584# then
         return Unsigned_32'Last;
      end if;
      Address_Value := (if Offset = 16#FDC# then Selector_Address else 16#60005584#);
      if Address_Value = 0 then return Unsigned_32'Last; end if;
      declare R : Unsigned_32 with Import, Volatile_Full_Access,
        Address => To_Address (Address_Value);
      begin Value := R; end;
      return (if Owned then Value else Unsigned_32'Last);
   end Read32;
   procedure Write32 (Offset, Value : Unsigned_32; Success : out Boolean) is
      Address_Value : constant Integer_Address := Selector_Address;
   begin
      Success := False;
      if not Owned or else Offset /= 16#FDC# or else Address_Value = 0 then return; end if;
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      declare R : Unsigned_32 with Import, Volatile_Full_Access,
        Address => To_Address (Address_Value);
      begin R := Value; end;
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      Success := Owned;
   end Write32;
   package MCR is new Intel_GPU_MCR_Access (Owned, Read32, Write32);
   Access_State : MCR.State;
   function Read_WM_Chicken2 return Unsigned_32 is
      T : constant Intel_GPU_ADLN_Steering.Topology := Topology;
      Value : Unsigned_32 := Unsigned_32'Last;
      OK : Boolean;
   begin
      if Active or else not Owner_Ready or else MCR.Failed (Access_State) or else
        not T.Valid or else
        (T.DSS_Mask and Shift_Left (Unsigned_8'(1), T.Default_Instance)) = 0
      then return Value; end if;
      Active := True;
      MCR.Access_Register (Access_State, 16#5584#, T.Default_Instance, False, Value, OK);
      Active := False;
      return (if OK and then Owner_Ready then Value else Unsigned_32'Last);
   end Read_WM_Chicken2;
end Intel_GPU_Native_Context_Read;
