--  Linux-hosted probe: the whole CCL host environment (every interface a
--  front end installs, system.* included) fits the catalog's capacity.
with Ada.Text_IO;
with Interfaces;
with CCL.Catalog;
with CCL.Types;
with CCL_Host_Environment;

procedure Probe is
   use type CCL.Types.Type_Reference;
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
                         " schemas" & CCL.Catalog.Schema_Length (Catalog)'Image &
                         " types" & CCL.Types.Last (CCL.Catalog.Visible_Types (Catalog))'Image &
                         " (most" & Natural'Image (Host.MOST_TYPES) & ")");
   --  What a run published stays within the declared most (the catalog's
   --  capacity is checked against that at compile time).
   Success := Success and then CCL.Catalog.Length (Catalog) <= Host.MOST_INTERFACES
     and then CCL.Catalog.Schema_Length (Catalog) <= Host.MOST_SCHEMAS
     and then CCL.Types.Last (CCL.Catalog.Visible_Types (Catalog)) - CCL.Types.Unit_Type <= Host.MOST_TYPES;
   Ada.Text_IO.Put_Line ("CCL-ENVIRONMENT-CHECK: " & (if Success then "PASS" else "FAIL"));
end Probe;
