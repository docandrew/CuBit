with Ada.Text_IO;
with Ada.Strings.Fixed;
with GNAT.Source_Info;
with Interfaces;
with CCL.Types;
with CCL.Catalog;
with CCL.Ownership;
with CCL.Resource_Policies;

procedure Policy_Tests is
   package T renames CCL.Types;
   package C renames CCL.Catalog;
   package O renames CCL.Ownership;
   package P renames CCL.Resource_Policies;
   use type T.Type_Reference;
   use type T.Definition_Result;
   use type T.Import_Result;
   use type T.Shape;
   use type C.Interface_Catalog;
   use type C.Resource_Publication;
   use type C.Resource_Specialization_Result;
   use type O.Type_Definition;
   use type O.Disposition;
   use type O.Ownership_Mode;
   use type P.Description;
   use type P.Layout_Result;
   use type P.Binding_Map;
   Types : T.Registry;
   Open_Type, Closed_Type, Unrelated : T.Type_Reference;
   Defined : T.Definition_Result;
   Open_Policy, Closed_Policy, Bad : P.Description;
   Catalog, Before : C.Interface_Catalog;
   Open_Ref, Closed_Ref, Ref : T.Type_Reference;
   Published : C.Resource_Publication;
   Imported : T.Import_Result;
   Roots : P.Selection := [others => False];
   Bindings, Previous_Bindings : P.Binding_Map;
   Definitions : O.Type_Table;
   Count : P.Layout_Count;
   Result : P.Layout_Result;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Site : String := GNAT.Source_Info.Source_Location) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Site & " resource policy check" & Checks'Image; end if;
   end Check;
begin
   T.Define (Types, (Identifier => T.Named ("OpenCollection"), Form => T.Resource,
     Count => 1, Parts => [1 => (T.Named ("Value"), T.Integer_Type), others => <>]), Open_Type, Defined);
   Check (Defined = T.Defined);
   T.Define (Types, (Identifier => T.Named ("ClosedCollection"), Form => T.Resource,
     Count => 1, Parts => [1 => (T.Named ("Value"), T.Integer_Type), others => <>]), Closed_Type, Defined);
   Check (Defined = T.Defined);
   Open_Policy := (Mode => O.Must_Handle, Count => 2, others => <>);
   Open_Policy.Dispositions (0) := (Verb => 1, Effect => O.Consume, others => <>);
   Open_Policy.Dispositions (1) := (Verb => 2, Effect => O.Transition, Next_Type => T.Named ("ClosedCollection"));
   Closed_Policy := (Mode => O.Move_Only, Count => 1, others => <>);
   Closed_Policy.Dispositions (0) := (Verb => 3, Effect => O.Consume, others => <>);
   Check (P.Valid (Types, Open_Type, Open_Policy));
   Check (not P.Valid (Types, T.Integer_Type, Open_Policy));
   for Mode in O.Ownership_Mode loop
      Bad := Open_Policy; Bad.Mode := Mode;
      Check (P.Valid (Types, Open_Type, Bad) = (Mode /= O.Unrestricted));
   end loop;
   Bad := Open_Policy; Bad.Dispositions (1).Verb := 1;
   Check (not P.Valid (Types, Open_Type, Bad));
   Bad := Open_Policy; Bad.Dispositions (0).Verb := 0;
   Check (not P.Valid (Types, Open_Type, Bad));
   Bad := Open_Policy; Bad.Dispositions (0).Next_Type := T.Named ("ClosedCollection");
   Check (not P.Valid (Types, Open_Type, Bad));
   Bad := Open_Policy; Bad.Dispositions (1).Next_Type := T.Named ("Integer");
   Check (not P.Valid (Types, Open_Type, Bad));
   Bad := Open_Policy; Bad.Dispositions (1).Next_Type := T.Named ("Missing");
   Check (not P.Valid (Types, Open_Type, Bad));
   Bad := Open_Policy; Bad.Dispositions (1).Next_Type.Data (32) := 'X';
   Check (not P.Valid (Types, Open_Type, Bad));
   Bad := Open_Policy; Bad.Dispositions (2).Verb := 9;
   Check (not P.Valid (Types, Open_Type, Bad));
   C.Publish_Resource (Catalog, Types, Open_Type, Open_Policy, Open_Ref, Published);
   Check (Published = C.Resource_Published);
   Check (C.Resource_Policy (Catalog, Open_Ref) = Open_Policy);
   Check (T.Find (C.Visible_Types (Catalog), T.Named ("ClosedCollection")) /= T.Invalid_Type);
   Roots (Open_Ref) := True;
   C.Layout_Resources (Catalog, Roots, Bindings, Definitions, Count, Result);
   Check (Result = P.Missing_Policy and Count = 0 and (for all B of Bindings => B = 0));
   C.Publish_Resource (Catalog, Types, Closed_Type, Closed_Policy, Closed_Ref, Published);
   Check (Published = C.Resource_Published);
   C.Layout_Resources (Catalog, Roots, Bindings, Definitions, Count, Result);
   Check (Result = P.Ready and Count = 3);
   Check (Bindings (Open_Ref) /= 0 and Bindings (Closed_Ref) /= 0 and Bindings (Open_Ref) /= Bindings (Closed_Ref));
   Check (Definitions (0).Mode = O.Unrestricted);
   Check (Definitions (Bindings (Open_Ref)).Mode = O.Must_Handle);
   Check (Definitions (Bindings (Closed_Ref)).Mode = O.Move_Only);
   Check (Definitions (Bindings (Open_Ref)).Dispositions (1) =
     (Verb => 2, Effect => O.Transition, Next_Type => Bindings (Closed_Ref)));
   Previous_Bindings := Bindings;
   Before := Catalog;
   C.Publish_Resource (Catalog, Types, Open_Type, Open_Policy, Ref, Published);
   Check (Published = C.Resource_Already_Published and Catalog = Before and Ref = Open_Ref);
   Bad := Open_Policy; Bad.Mode := O.Move_Only;
   C.Publish_Resource (Catalog, Types, Open_Type, Bad, Ref, Published);
   Check (Published = C.Resource_Policy_Conflict and Catalog = Before and Ref = T.Invalid_Type);
   C.Publish_Resource (Catalog, Types, Open_Type, (others => <>), Ref, Published);
   Check (Published = C.Invalid_Resource_Policy and Catalog = Before and Ref = T.Invalid_Type);

   declare
      Shifted, Conflicting : T.Registry;
      A, B, Dummy : T.Type_Reference;
   begin
      T.Define (Shifted, (Identifier => T.Named ("Prefix"), Form => T.Product, others => <>), Dummy, Defined);
      Check (Defined = T.Defined);
      T.Define (Shifted, T.Describe (Types, Open_Type), A, Defined); Check (Defined = T.Defined and A /= Open_Type);
      T.Define (Shifted, T.Describe (Types, Closed_Type), B, Defined); Check (Defined = T.Defined);
      C.Publish_Resource (Catalog, Shifted, A, Open_Policy, Ref, Published);
      Check (Published = C.Resource_Already_Published and Catalog = Before and Ref = Open_Ref);
      T.Define (Conflicting, T.Describe (Types, Open_Type), A, Defined); Check (Defined = T.Defined);
      T.Define (Conflicting, (Identifier => T.Named ("ClosedCollection"), Form => T.Resource,
        Count => 1, Parts => [1 => (T.Named ("Value"), T.Boolean_Type), others => <>]), B, Defined);
      Check (Defined = T.Defined);
      C.Publish_Resource (Catalog, Conflicting, A, Open_Policy, Ref, Published);
      Check (Published = C.Resource_Definition_Conflict and Catalog = Before and Ref = T.Invalid_Type);
      -- A newly imported root must not leak into the catalog when a later
      -- transition target conflicts with an existing nominal definition.
      T.Define (Conflicting, (Identifier => T.Named ("NewCollection"),
        Form => T.Resource, others => <>), A, Defined);
      Check (Defined = T.Defined);
      C.Publish_Resource (Catalog, Conflicting, A, Open_Policy, Ref, Published);
      Check (Published = C.Resource_Definition_Conflict and Catalog = Before and Ref = T.Invalid_Type);
      Check (T.Find (C.Visible_Types (Catalog), T.Named ("NewCollection")) = T.Invalid_Type);
   end;
   -- Ordinary visible types do not acquire ownership approval as a side effect.
   T.Define (Types, (Identifier => T.Named ("Unrelated"), Form => T.Resource, others => <>), Unrelated, Defined);
   Check (Defined = T.Defined);
   C.Publish_Type (Catalog, Types, Unrelated, Ref, Imported); Check (Imported = T.Imported);
   C.Layout_Resources (Catalog, Roots, Bindings, Definitions, Count, Result);
   Check (Result = P.Ready and Count = 3 and Bindings = Previous_Bindings);
   Roots (Ref) := True;
   C.Layout_Resources (Catalog, Roots, Bindings, Definitions, Count, Result);
   Check (Result = P.Missing_Policy and Count = 0);

   -- A trusted catalog may instantiate a resource family from an approved
   -- value type. This is common metadata for ConfigCollection<T> and Stream<T>,
   -- not a Config-specific parser form or a live authority grant.
   declare
      Generic_Types : T.Registry;
      Preferences, Collection, Stream, Again : T.Type_Reference;
      Generic_Catalog, Generic_Before : C.Interface_Catalog;
      Generic_Policy : P.Description :=
        (Mode => O.Must_Handle, Count => 1, others => <>);
      Specialized : C.Resource_Specialization_Result;
      D : T.Description;
   begin
      T.Define
        (Generic_Types,
         (Identifier => T.Named ("Preferences"), Form => T.Product, Count => 1,
          Parts => [1 => (T.Named ("Timezone"), T.String_Type), others => <>]),
         Preferences, Defined);
      Check (Defined = T.Defined);
      Generic_Policy.Dispositions (0) :=
        (Verb => 1, Effect => O.Consume, others => <>);
      C.Specialize_Unary_Resource
        (Generic_Catalog, Generic_Types, "ConfigCollection", "Value",
         Preferences, Generic_Policy, Collection, Specialized);
      Check (Specialized = C.Specialization_Ready);
      D := T.Describe (C.Visible_Types (Generic_Catalog), Collection);
      Check (T.Same (D.Identifier, T.Named ("ConfigCollection-Preferences")) and
        D.Form = T.Resource and D.Count = 1 and
        T.Same (D.Parts (1).Identifier, T.Named ("Value")) and
        T.Same (T.Describe (C.Visible_Types (Generic_Catalog), D.Parts (1).Payload).Identifier,
          T.Named ("Preferences")));
      Check (C.Resource_Policy (Generic_Catalog, Collection) = Generic_Policy);
      Generic_Before := Generic_Catalog;
      C.Specialize_Unary_Resource
        (Generic_Catalog, Generic_Types, "ConfigCollection", "Value",
         Preferences, Generic_Policy, Again, Specialized);
      Check (Specialized = C.Specialization_Already_Ready and Again = Collection and
        Generic_Catalog = Generic_Before);
      C.Specialize_Unary_Resource
        (Generic_Catalog, Generic_Types, "Stream", "Element",
         Preferences, Generic_Policy, Stream, Specialized);
      Check (Specialized = C.Specialization_Ready and Stream /= Collection and
        T.Same (T.Describe (C.Visible_Types (Generic_Catalog), Stream).Identifier,
          T.Named ("Stream-Preferences")));
      Generic_Before := Generic_Catalog;
      C.Specialize_Unary_Resource
        (Generic_Catalog, Generic_Types, "Stream?", "Value",
         Preferences, (others => <>), Again, Specialized);
      Check (Specialized = C.Specialization_Invalid and Again = T.Invalid_Type and
        Generic_Catalog = Generic_Before);
      C.Specialize_Unary_Resource
        (Generic_Catalog, Generic_Types, "ConfigCollection", "Value",
         T.Invalid_Type, Generic_Policy, Again, Specialized);
      Check (Specialized = C.Specialization_Invalid and Again = T.Invalid_Type and
        Generic_Catalog = Generic_Before);
   end;

   -- Reverse chains, cycles and every ownership-tag capacity boundary.
   declare
      Universe : T.Registry;
      Policies : P.Policy_Table := [others => (others => <>)];
      type Refs is array (1 .. T.Maximum_Declarations) of T.Type_Reference;
      Nodes : Refs;
      function Name (I : Positive) return T.Name is
        (T.Named ("R" & Ada.Strings.Fixed.Trim (I'Image, Ada.Strings.Both)));
   begin
      for I in Nodes'Range loop
         T.Define (Universe, (Identifier => Name (I), Form => T.Resource, others => <>), Nodes (I), Defined);
         Check (Defined = T.Defined);
         Policies (Nodes (I)) := (Mode => O.Must_Handle, Count => 1, others => <>);
         Policies (Nodes (I)).Dispositions (0) := (Verb => 1, Effect => O.Consume, others => <>);
         if I > 1 then Policies (Nodes (I)).Dispositions (0) :=
           (Verb => 1, Effect => O.Transition, Next_Type => Name (I - 1)); end if;
      end loop;
      for I in Nodes'Range loop
         Roots := [others => False]; Roots (Nodes (I)) := True;
         P.Layout (Universe, Policies, Roots, Bindings, Definitions, Count, Result);
         if I < O.MAX_TYPES then
            Check (Result = P.Ready and Count = I + 1);
            for J in Nodes'Range loop
               Check ((Bindings (Nodes (J)) /= 0) = (J <= I));
               if J in 2 .. I then Check (Definitions (Bindings (Nodes (J))).Dispositions (0).Next_Type =
                 Bindings (Nodes (J - 1))); end if;
            end loop;
         else Check (Result = P.Too_Many_Types and Count = 0 and (for all B of Bindings => B = 0)); end if;
      end loop;
      Policies (Nodes (1)).Dispositions (0) := (Verb => 1, Effect => O.Transition, Next_Type => Name (2));
      Roots := [others => False]; Roots (Nodes (2)) := True;
      P.Layout (Universe, Policies, Roots, Bindings, Definitions, Count, Result);
      Check (Result = P.Ready and Count = 3);
      Check (Definitions (Bindings (Nodes (1))).Dispositions (0).Next_Type = Bindings (Nodes (2)));
      Policies (Nodes (2)).Dispositions (0).Verb := Interfaces.Unsigned_8'Last;
      P.Layout (Universe, Policies, Roots, Bindings, Definitions, Count, Result);
      Check (Result = P.Ready);
      Policies (Nodes (2)).Dispositions (0).Verb := 0;
      P.Layout (Universe, Policies, Roots, Bindings, Definitions, Count, Result);
      Check (Result = P.Invalid_Policy and Count = 0);
      Roots := [others => False];
      P.Layout (Universe, Policies, Roots, Bindings, Definitions, Count, Result);
      Check (Result = P.Ready and Count = 1 and Definitions (0) = (others => <>));
   end;
   Ada.Text_IO.Put_Line ("Approved resource ownership policies: PASS" & Checks'Image & " checks");
end Policy_Tests;
