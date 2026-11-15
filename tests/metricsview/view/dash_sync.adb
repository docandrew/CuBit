--  Hosted: both sides run on the test's one thread; no lock to take.
package body Dash_Sync is
   procedure Lock is null;
   procedure Unlock is null;
   procedure Lock_Waited (Waited_Us : out Interfaces.Unsigned_64) is
   begin
      Waited_Us := 0;
   end Lock_Waited;
end Dash_Sync;
