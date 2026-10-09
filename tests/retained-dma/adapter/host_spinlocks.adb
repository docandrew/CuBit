with Ada.Task_Attributes;
package body Spinlocks is
   package Ownership is new Ada.Task_Attributes (Boolean, False);
   function Held_By_Caller return Boolean is (Ownership.Value);
   protected body Spinlock is
      entry Enter when not Locked is
      begin
         Locked := True;
      end Enter;
      procedure Leave is
      begin
         pragma Assert (Locked);
         Locked := False;
      end Leave;
   end Spinlock;
   procedure enterCriticalSection (Object : in out Spinlock) is
   begin
      Object.Enter;
      pragma Assert (not Ownership.Value);
      Ownership.Set_Value (True);
   end enterCriticalSection;
   procedure exitCriticalSection (Object : in out Spinlock) is
   begin
      pragma Assert (Ownership.Value);
      Ownership.Set_Value (False);
      Object.Leave;
   end exitCriticalSection;
end Spinlocks;
