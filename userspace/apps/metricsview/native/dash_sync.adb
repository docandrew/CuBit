with CuBit.Futex_Locks;

package body Dash_Sync is
   Columns_Lock : CuBit.Futex_Locks.Mutex;

   procedure Lock is
   begin
      CuBit.Futex_Locks.Lock (Columns_Lock);
   end Lock;

   procedure Unlock is
   begin
      CuBit.Futex_Locks.Unlock (Columns_Lock);
   end Unlock;
end Dash_Sync;
