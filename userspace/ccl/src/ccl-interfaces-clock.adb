with CCL.VM;

package body CCL.Interfaces.Clock with
   SPARK_Mode => On
is
   use type CCL.Catalog.Catalog_Error;

   procedure Publish
     (Item  : in out CCL.Catalog.Interface_Catalog;
      Error : out CCL.Catalog.Catalog_Error)
   is
      Descriptor : CCL.Catalog.Interface_Descriptor;
      Operation  : CCL.Catalog.Operation_Descriptor;
   begin
      CCL.Catalog.Define_Interface
        ("clock", 1, 0, DESCRIPTOR_DIGEST, Descriptor, Error);
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Define_Operation
           ("monotonic-ms", 0,
            (Argument  => CCL.VM.Integer_Value,
             Result    => CCL.VM.Integer_Value,
             Authority => CCL.VM.Observe_Authority,
             others    => <>),
            Operation, Error);
      end if;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Add_Operation (Descriptor, Operation, Error);
      end if;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Publish (Item, Descriptor, Error);
      end if;
   end Publish;

   procedure Resolve_Monotonic_Ms
     (Item   : CCL.Catalog.Interface_Catalog;
      Result : out CCL.Catalog.Resolved_Operation;
      Found  : out Boolean)
   is
   begin
      CCL.Catalog.Resolve (Item, "clock.monotonic-ms", Result, Found);
   end Resolve_Monotonic_Ms;
end CCL.Interfaces.Clock;
