with CCL.Call_Context;
with CCL.Highlighting;
with CCL.Hints;
with CCL.Host_Values;
with CCL.Language;
with CCL.Types;

package body CCL.Completions with SPARK_Mode => Off is
   use type CCL.Language.Builtin_Operation;
   package HL renames CCL.Highlighting;

   function Starts_With (Word, Prefix : String) return Boolean is
     (Word'Length >= Prefix'Length and then
      Word (Word'First .. Word'First + Prefix'Length - 1) = Prefix);

   function Type_Name (Kind : CCL.Host_Values.Value_Kind) return String is
     (case Kind is
         when CCL.Host_Values.Integer_Value => "Integer",
         when CCL.Host_Values.Boolean_Value => "Boolean",
         when CCL.Host_Values.Text_Value => "String",
         when CCL.Host_Values.Handler_Value => "Handler",
         when CCL.Host_Values.Object_Value => "Object",
         when CCL.Host_Values.Resource_Value => "Resource");

   function Signature_Image (S : CCL.Catalog.Completion.Suggestion) return String is
      Import : CCL.Host_Values.Import_Declaration renames S.Contract.Import;
      Receiver : constant String :=
        (if CCL.Host_Values.Has_Receiver (Import)
         then " " & CCL.Types.Image (Import.Receiver_Resource) else "");
      Argument : constant String :=
        (if S.Contract.Parameters = 0 then "" else " " & Type_Name (Import.Argument));
   begin
      return "(" & S.Name (1 .. S.Length) & Receiver & Argument & ") -> " &
        Type_Name (Import.Result);
   end Signature_Image;

   function Describe (S : CCL.Catalog.Completion.Suggestion; From : Origin) return String is
     (if From = Host_Operation then Signature_Image (S) else CCL.Hints.Hint (S.Name (1 .. S.Length)));

   procedure Complete
     (Catalog : CCL.Catalog.Interface_Catalog; Before : String; After_Caret : Character;
      Item : out Result)
   is
      Context : CCL.Call_Context.Context;
      Matches : CCL.Catalog.Completion.Match_List;
      Found : Boolean;
      procedure Add (Name : String; From : Origin;
                     Contract : CCL.Catalog.Resolved_Operation := (others => <>)) is
      begin
         if Item.Count = Maximum_Candidates then
            Item.Beyond := True;
         elsif Name'Length <= CCL.Catalog.Completion.Maximum_Qualified_Name then
            Item.Count := Item.Count + 1;
            Item.Candidates (Item.Count) :=
              (Suggestion => (Name => [others => ' '], Length => Name'Length, Contract => Contract),
               Origin => From);
            Item.Candidates (Item.Count).Suggestion.Name (1 .. Name'Length) := Name;
         end if;
      end Add;
   begin
      Item := (others => <>);
      if Before'Length > CCL.Call_Context.Maximum_Source then return; end if;
      CCL.Call_Context.Inspect (Before, Before'Length, Context);
      if not Context.Available then return; end if;
      declare
         Prefix : constant String := Context.Name (1 .. Context.Length);
      begin
         if Context.Arguments_Started then
            CCL.Catalog.Resolve (Catalog, Prefix, Item.Signature.Contract, Found);
            if Found or else CCL.Hints.Hint (Prefix)'Length > 0 then
               Item.Signature.Name := Context.Name;
               Item.Signature.Length := Context.Length;
               Item.Signature_Visible := True;
               Item.Signature_Origin := (if Found then Host_Operation else Builtin);
            end if;
            return;
         end if;
         if After_Caret not in ' ' | ')' | ASCII.LF | ASCII.CR | ASCII.HT then return; end if;
         Item.Prefix_Length := Prefix'Length;
         CCL.Catalog.Completion.Find (Catalog, Prefix, Matches);
         for I in 1 .. Matches.Count loop
            Add (Matches.Items (I).Name (1 .. Matches.Items (I).Length), Host_Operation,
                 Matches.Items (I).Contract);
         end loop;
         Item.Beyond := Item.Beyond or else Matches.Total > Matches.Count;
         for F in HL.Form_Word loop
            if Starts_With (HL.Special_Form_Name (F), Prefix) then
               Add (HL.Special_Form_Name (F), Form);
            end if;
         end loop;
         for Operation in CCL.Language.Builtin_Operation loop
            if Operation /= CCL.Language.No_Builtin and then
              Starts_With (CCL.Language.Builtin_Name (Operation), Prefix)
            then
               Add (CCL.Language.Builtin_Name (Operation), Builtin);
            end if;
         end loop;
         for Operator in HL.Core_Operator loop
            if Prefix'Length > 0 and then Starts_With (HL.Core_Operator_Name (Operator), Prefix) then
               Add (HL.Core_Operator_Name (Operator), Builtin);
            end if;
         end loop;
         --  A finished name needs no list, only its signature.
         if Item.Count = 1 and then Item.Candidates (1).Suggestion.Length = Prefix'Length then
            if Item.Candidates (1).Origin = Host_Operation or else
              CCL.Hints.Hint (Prefix)'Length > 0
            then
               Item.Signature := Item.Candidates (1).Suggestion;
               Item.Signature_Visible := True;
               Item.Signature_Origin := Item.Candidates (1).Origin;
            end if;
            Item.Count := 0;
         end if;
      end;
   end Complete;
end CCL.Completions;
