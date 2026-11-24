with CCL.Types;
with CCL.Ownership;
private with CCL.Growing_Tables;

-- Approved ownership metadata for nominal resource types. These are catalog
-- descriptions, never authority grants or persistable object schemas. Store
-- one table beside the type registry, not a copy inside every AST/import node.
package CCL.Resource_Policies with SPARK_Mode is
   use type CCL.Types.Type_Reference;
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
   --  The policies of a registry's resource types, by type: only those that
   --  have one are held (a handful), the rest are Unrestricted. A table is a
   --  value; copies share storage until one changes (CCL.Growing_Tables).
   type Policy_Table is private with Default_Initial_Condition => True;
   No_Policy : constant Description := (others => <>);
   function Policy (Table : Policy_Table; Ref : CCL.Types.Type_Reference) return Description
     with Global => null;
   --  Ref's policy becomes Value. Refused (the table unchanged) only when
   --  memory runs out or the table holds a policy for every type.
   procedure Set_Policy
     (Table : in out Policy_Table; Ref : CCL.Types.Type_Reference; Value : Description;
      Done : out Boolean)
     with Global => null, Always_Terminates,
       Post => (if not Done then Table = Table'Old);

   --  Indexed by a registry's references from 0, usually to Last (Types).
   type Selection is array (CCL.Types.Type_Reference range <>) of Boolean;
   type Binding_Map is array (CCL.Types.Type_Reference range <>) of CCL.Ownership.Type_Id;
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
       Pre => Roots'First = 0 and then Bindings'First = 0 and then Bindings'Last = Roots'Last,
       Post => (if Result /= Ready then Count = 0 and
         (for all Ref in Bindings'Range => Bindings (Ref) = 0));
   -- A root past Last (Types) has no policy (Missing_Policy); Bindings gets
   -- the tags of the types it has room for.
   -- Include only requested resource types and their transitive ownership
   -- transition targets. Cycles are bounded, not recursively expanded. Tag 0
   -- stays unrestricted; every used resource gets a nonzero local tag. Missing
   -- target policies fail, even if the nominal type itself is discoverable.
private
   type Policy_Entry is record
      Ref : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
      Value : Description;
   end record;
   package Entry_Tables is new CCL.Growing_Tables
     (Element => Policy_Entry, Ceiling => CCL.Types.Declaration_Ceiling);
   type Policy_Table is record
      Entries : Entry_Tables.Table;
   end record;
end CCL.Resource_Policies;
