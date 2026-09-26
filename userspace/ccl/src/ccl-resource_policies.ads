with CCL.Types;
with CCL.Ownership;

-- Approved ownership metadata for nominal resource types. These are catalog
-- descriptions, never authority grants or persistable object schemas. Store
-- one table beside the type registry, not a copy inside every AST/import node.
package CCL.Resource_Policies with SPARK_Mode is
   type Disposition is record
      Verb : CCL.Ownership.Disposition_Id := 0;
      Effect : CCL.Ownership.Disposition_Effect := CCL.Ownership.Consume;
      Next_Type : CCL.Types.Name;
   end record;
   type Disposition_Array is array (Natural range 0 .. CCL.Ownership.MAX_DISPOSITIONS - 1) of Disposition;
   type Description is record
      Mode : CCL.Ownership.Ownership_Mode := CCL.Ownership.Unrestricted;
      Count : CCL.Ownership.Disposition_Count := 0;
      Dispositions : Disposition_Array := [others => (others => <>)];
   end record;
   type Policy_Table is array (CCL.Types.Type_Reference) of Description;
   type Selection is array (CCL.Types.Type_Reference) of Boolean;
   type Binding_Map is array (CCL.Types.Type_Reference) of CCL.Ownership.Type_Id;
   subtype Layout_Count is Natural range 0 .. CCL.Ownership.MAX_TYPES;
   type Layout_Result is (Ready, Invalid_Policy, Missing_Policy, Too_Many_Types);

   function Valid
     (Types : CCL.Types.Registry; Root : CCL.Types.Type_Reference;
      Policy : Description) return Boolean with Global => null;
   -- Canonical non-unrestricted resource ownership, unique nonzero verbs,
   -- and nominal resource targets for transitions. Target numbers are NOT
   -- process-local ownership IDs. Unused fields have their canonical defaults.

   procedure Layout
     (Types : CCL.Types.Registry; Policies : Policy_Table; Roots : Selection;
      Bindings : out Binding_Map; Definitions : out CCL.Ownership.Type_Table;
      Count : out Layout_Count; Result : out Layout_Result)
     with Global => null,
       Post => (if Result /= Ready then Count = 0 and
         (for all Ref in CCL.Types.Type_Reference => Bindings (Ref) = 0));
   -- Include only requested resource types and their transitive ownership
   -- transition targets. Cycles are bounded, not recursively expanded. Tag 0
   -- stays unrestricted; every used resource gets a nonzero local tag. Missing
   -- target policies fail, even if the nominal type itself is discoverable.
end CCL.Resource_Policies;
