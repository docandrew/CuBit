with CCL.Catalog;
with CCL.Objects.Catalog;
with VM_Fixture;

package body Discovered_Fixture is
   procedure Build
     (Shifted : Boolean; Contract : out CCL.Objects.Binding;
      Local_Types : out CCL.Types.Registry; First, Second : out CCL.VM.Value;
      Good : out Boolean)
   is
      use CCL.Types;
      Source, Padding, Empty_Types : Registry;
      Root, Local_Root, Ref : Type_Reference;
      Defined_As : Definition_Result;
      Imported_As : Import_Result;
      Catalog : CCL.Catalog.Interface_Catalog;
      Schemas : CCL.Objects.Catalog.Schema_Catalog;
      Publication : CCL.Objects.Catalog.Publication_Result;
      use type CCL.Objects.Catalog.Publication_Result;
      Approved : CCL.Objects.Binding;
      Unbound : CCL.Objects.Binding;
   begin
      Good := False; Contract := Unbound; Local_Types := Empty_Types;
      First := CCL.VM.Integer_Constant (0); Second := First;
      Define (Source, (Identifier => Named ("Reading"), Form => Sum, Count => 2,
        Parts => [1 => (Named ("Value"), Integer_Type),
                  2 => (Named ("Unavailable"), Unit_Type), others => <>]), Root, Defined_As);
      if Defined_As /= Defined then return; end if;
      CCL.Objects.Bind (Source, Root, [9, 10, 11, 12], Approved, Good);
      if not Good then return; end if;
      Good := False;
      CCL.Objects.Catalog.Publish (Schemas, Approved, Publication);
      if Publication /= CCL.Objects.Catalog.Published then return; end if;
      CCL.Objects.Catalog.Resolve (Schemas, [0, 0, 0, 0], Contract);
      if CCL.Objects.Is_Bound (Contract) then return; end if;
      CCL.Objects.Catalog.Resolve (Schemas, [9, 10, 11, 12], Contract);
      if not CCL.Objects.Is_Bound (Contract) then return; end if;
      if Shifted then
         Define (Padding, (Identifier => Named ("ReaderOnly"), Form => Product, others => <>), Ref, Defined_As);
         if Defined_As /= Defined then return; end if;
         CCL.Catalog.Publish_Type (Catalog, Padding, Ref, Local_Root, Imported_As);
         if Imported_As /= Imported then return; end if;
      end if;
      CCL.Catalog.Publish_Type
        (Catalog, CCL.Objects.Catalog.Visible_Types (Schemas),
         CCL.Objects.Root_Type (Contract), Local_Root, Imported_As);
      if Imported_As /= Imported or (Shifted and Local_Root = Root) then return; end if;
      -- No copied declaration text: type names come solely from the authorized
      -- host-provided discovery snapshot, never from a saved value's claim.
      VM_Fixture.Run ("(Reading.Value 42)", Local_Types, First, Good, Catalog);
      if not Good then return; end if;
      VM_Fixture.Run ("Reading.Unavailable", Local_Types, Second, Good, Catalog);
   end Build;
end Discovered_Fixture;
