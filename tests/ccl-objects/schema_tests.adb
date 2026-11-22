with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Types;
with CCL.Objects;
with CCL.Objects.Schemas;

procedure Schema_Tests is
   package T renames CCL.Types;
   package O renames CCL.Objects;
   package S renames CCL.Objects.Schemas;
   use type T.Type_Reference;
   use type T.Definition_Result;
   use type O.Binding;
   use type O.Build_Result;
   use type S.Image;
   Registry : T.Registry;
   Contract, Restored, Empty : O.Binding;
   Data, Saved, Again : S.Image;
   Product, Sum, Ref : T.Type_Reference;
   Definition : T.Description;
   Defined : T.Definition_Result;
   Built : O.Build_Result;
   Value : O.Image;
   Good : Boolean;
   Checks : Natural := 0;
   Key : constant O.Schema_Key := [1, 2, 3, 4];
   Bad_Roots : constant array (1 .. 4) of Unsigned_32 :=
     [0, S.Handler_ID, Unsigned_32'Last, S.Unit_ID + 3];
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "native schema check" & Checks'Image; end if;
   end Check;
   procedure Roundtrip (Root : T.Type_Reference) is
   begin
      O.Bind (Registry, Root, Key, Contract, Good); Check (Good);
      S.Write (Contract, Data, Good); Check (Good);
      S.Read (Data, Restored, Good); Check (Good and Restored = Contract);
      S.Write (Restored, Again, Good); Check (Good and Again = Data);
   end Roundtrip;
   procedure Reject is
   begin
      Restored := Contract;
      S.Read (Data, Restored, Good); Check (not Good and Restored = Empty);
      Data := Saved;
   end Reject;
begin
   Check (S.Image'Size = S.Native_Schema_Bytes * 8);
   S.Write (Empty, Data, Good); Check (not Good and Data = S.Image'(others => <>));
   for Root in T.Integer_Type .. T.Unit_Type loop
      if Root /= T.Handler_Type then Roundtrip (Root); end if;
   end loop;
   Definition := (Identifier => T.Named ("Reading"), Form => T.Product, Count => 2,
     Parts => [1 => (T.Named ("elapsed"), T.Integer_Type),
               2 => (T.Named ("caption"), T.String_Type), others => <>]);
   T.Define (Registry, Definition, Product, Defined); Check (Defined = T.Defined);
   Definition := (Identifier => T.Named ("Outcome"), Form => T.Sum, Count => 2,
     Parts => [1 => (T.Named ("Ready"), Product),
               2 => (T.Named ("Missing"), T.Unit_Type), others => <>]);
   T.Define (Registry, Definition, Sum, Defined); Check (Defined = T.Defined);
   Roundtrip (Sum); Saved := Data;
   Value := O.Empty (Contract);
   O.Append (Value, O.Variant_Cell (1), Built); Check (Built = O.Added);
   O.Append (Value, O.Product_Cell (2), Built); Check (Built = O.Added);
   O.Append (Value, O.Integer_Cell (3661000), Built); Check (Built = O.Added);
   O.Append_Text (Value, "Cubie", Built); Check (Built = O.Added);
   Check (O.Validate (Value, Contract) and O.Validate (Value, Restored));
   Data.Format := 0; Reject;
   Data.Key := O.No_Schema; Reject;
   Data.Count := Unsigned_32'Last; Reject;
   Data.Count := 1; Reject;
   Data.Reserved := 1; Reject;
   for I in Data.Padding'Range loop Data.Padding (I) := 1; Reject; end loop;
   for Root of Bad_Roots loop
      Data.Root := Root; Reject;
   end loop;
   Data.Definitions (1).Form := 0; Reject;
   Data.Definitions (1).Form := Unsigned_32'Last; Reject;
   Data.Definitions (1).Count := Unsigned_32'Last; Reject;
   Data.Definitions (1).Reserved := 1; Reject;
   for I in Data.Definitions (1).Padding'Range loop
      Data.Definitions (1).Padding (I) := 1; Reject;
   end loop;
   Data.Definitions (1).Identifier.Length := Unsigned_32'Last; Reject;
   Data.Definitions (1).Identifier.Text (1) := '-'; Reject;
   Data.Definitions (1).Identifier.Text (32) := 'X'; Reject;
   Data.Definitions (2).Identifier := Data.Definitions (1).Identifier; Reject;
   Data.Definitions (1).Parts (2).Identifier := Data.Definitions (1).Parts (1).Identifier; Reject;
   Data.Definitions (1).Parts (1).Identifier.Length := 0; Reject;
   Data.Definitions (1).Parts (1).Payload := Unsigned_32'Last; Reject;
   Data.Definitions (1).Parts (1).Payload := 0; Reject;
   Data.Definitions (1).Parts (1).Payload := S.Unit_ID + 1; Reject; -- self
   Data.Definitions (1).Parts (1).Payload := S.Unit_ID + 2; Reject; -- forward/cycle
   Data.Definitions (1).Parts (1).Payload := S.Handler_ID; Reject; -- data cannot own a handler
   Data.Definitions (1).Parts (3).Payload := S.Integer_ID; Reject; -- unused component
   Data.Definitions (3).Count := 1; Reject; -- unused definition
   Data.Definitions (2).Count := 0;
   Data.Definitions (2).Parts := [others => <>]; Reject; -- empty sum
   --  Largest registry and maximum component count; decode must use bounded
   --  Define, not cast raw words into private Registry/enum representations.
   declare
      Full : T.Registry;
      Previous : T.Type_Reference := T.Unit_Type;
   begin
      for I in 1 .. T.Maximum_Declarations loop
         Definition := (Identifier => T.Named ("Type_" & Character'Val (Character'Pos ('A') + (I - 1) / 26) &
                           Character'Val (Character'Pos ('A') + (I - 1) mod 26)),
                        Form => T.Sum, Count => T.Maximum_Components, others => <>);
         for Part in Definition.Parts'Range loop
            Definition.Parts (Part) :=
              (T.Named ("field_" & Character'Val (Character'Pos ('A') + Part - 1)), T.Integer_Type);
         end loop;
         Definition.Parts (1).Payload := Previous;
         T.Define (Full, Definition, Ref, Defined); Check (Defined = T.Defined);
         Previous := Ref;
      end loop;
      O.Bind (Full, Ref, Key, Contract, Good); Check (Good);
      S.Write (Contract, Data, Good); Check (Good);
      S.Read (Data, Restored, Good); Check (Good and Restored = Contract);
   end;
   Ada.Text_IO.Put_Line ("Native CCL schema metadata: PASS" & Checks'Image & " checks");
end Schema_Tests;
