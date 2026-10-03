with CCL.Host_Values;
with CCL.VM;

package body CCL.Interfaces.Timer with
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
        ("timer", 1, 0, DESCRIPTOR_DIGEST, Descriptor, Error);
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Define_Host_Operation
           ("every", 1,
            (Argument      => CCL.Host_Values.Integer_Value,
             Result        => CCL.Host_Values.Integer_Value,
             Result_Stream => True,
             Authority     => CCL.VM.Observe_Authority,
             others        => <>),
            Operation, Error);
      end if;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Add_Operation (Descriptor, Operation, Error);
      end if;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Publish (Item, Descriptor, Error);
      end if;
   end Publish;

   procedure Resolve_Every
     (Item   : CCL.Catalog.Interface_Catalog;
      Result : out CCL.Catalog.Resolved_Operation;
      Found  : out Boolean)
   is
   begin
      CCL.Catalog.Resolve (Item, "timer.every", Result, Found);
   end Resolve_Every;
end CCL.Interfaces.Timer;
