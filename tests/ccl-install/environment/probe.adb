--  Linux-hosted probe: the whole CCL host environment (every interface a
--  front end installs, system.* included) fits the catalog's capacity.
with Ada.Text_IO;
with Interfaces;
with CCL.Catalog;
with CCL_Host_Environment;

procedure Probe is
   function Clock (Available : out Boolean) return Interfaces.Unsigned_64 is
   begin
      Available := True;
      return 0;
   end Clock;
   package Host is new CCL_Host_Environment (Clock);
   Catalog : CCL.Catalog.Interface_Catalog;
   Grants : CCL.Catalog.Granted_Bindings;
   Success : Boolean;
begin
   CCL.Catalog.Initialize (Catalog);
   CCL.Catalog.Initialize (Grants);
   Host.Install (Catalog, Grants, Success);
   Ada.Text_IO.Put_Line ("interfaces" & CCL.Catalog.Length (Catalog)'Image &
                         " schemas" & CCL.Catalog.Schema_Length (Catalog)'Image);
   --  What a run published stays within the declared most (the catalog's
   --  capacity is checked against that at compile time).
   Success := Success and then CCL.Catalog.Length (Catalog) <= Host.MOST_INTERFACES
     and then CCL.Catalog.Schema_Length (Catalog) <= Host.MOST_SCHEMAS;
   Ada.Text_IO.Put_Line ("CCL-ENVIRONMENT-CHECK: " & (if Success then "PASS" else "FAIL"));
end Probe;
