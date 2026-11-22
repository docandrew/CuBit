with CuBit.Futex_Locks;
with CuBit.Monotonic;

package body Dash_Sync is
   Columns_Lock : CuBit.Futex_Locks.Mutex;

   procedure Lock is
   begin
      CuBit.Futex_Locks.Lock (Columns_Lock);
   end Lock;

   procedure Lock_Waited (Waited_Us : out Interfaces.Unsigned_64) is
      use type Interfaces.Unsigned_64;
      Started : constant Interfaces.Unsigned_64 := CuBit.Monotonic.Microseconds;
   begin
      CuBit.Futex_Locks.Lock (Columns_Lock);
      Waited_Us := CuBit.Monotonic.Microseconds - Started;
   end Lock_Waited;

   procedure Unlock is
   begin
      CuBit.Futex_Locks.Unlock (Columns_Lock);
   end Unlock;
end Dash_Sync;
