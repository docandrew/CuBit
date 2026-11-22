with Interfaces;
with CuBit.Messages;
with Desktop_Launch;
package Desktop_Launch_Refresh is
   -- Initialize once at startup; no allocation on menu-open or refresh.
   procedure Initialize;
   procedure Request;
   -- At most one request submitted per main-loop pass.
   procedure Pump (Sequence : in out Interfaces.Unsigned_64);
   function Token return Interfaces.Unsigned_64;
   procedure Collect (C : CuBit.Messages.CompletionEntry);
   procedure Quarantine;
   function Can_Take (Visible : Boolean) return Boolean;
   procedure Take (Visible : Boolean; Items : out Desktop_Launch.Menu; Updated : out Boolean);
end Desktop_Launch_Refresh;
