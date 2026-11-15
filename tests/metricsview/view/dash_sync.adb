--  Hosted: both sides run on the test's one thread; no lock to take.
package body Dash_Sync is
   procedure Lock is null;
   procedure Unlock is null;
end Dash_Sync;
