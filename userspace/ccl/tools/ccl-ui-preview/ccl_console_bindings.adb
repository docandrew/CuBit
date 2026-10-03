with CCL.Objects;

package body CCL_Console_Bindings is
   use Interfaces;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Host_Values.Value_Kind;
   package Console renames CCL.Interfaces.Console;

   Contracts : Console.Contracts;

   function Handles (Binding : Unsigned_32) return Boolean is
     (Binding in Console.FIRST_BINDING .. Console.Binding_Of (Console.Operation'Last));

   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean)
   is
      Error : CCL.Catalog.Catalog_Error;
      Grant : CCL.Catalog.Grant_Result;
      Resolved : CCL.Catalog.Resolved_Operation;
      Found : Boolean;
   begin
      Console.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if not Success then return; end if;
      CCL.Catalog.Resolve_Schema (Catalog, Console.NOTATION_KEY, Contracts.Notation);
      CCL.Catalog.Resolve_Schema (Catalog, Console.THEME_KEY, Contracts.Theme);
      CCL.Catalog.Resolve_Schema (Catalog, Console.STATS_KEY, Contracts.Stats);
      Success := CCL.Objects.Is_Bound (Contracts.Notation) and then
        CCL.Objects.Is_Bound (Contracts.Theme) and then CCL.Objects.Is_Bound (Contracts.Stats);
      for Op in Console.Operation loop
         exit when not Success;
         CCL.Catalog.Resolve (Catalog, "console." & Console.Name (Op), Resolved, Found);
         Success := Found;
         if Success then
            CCL.Catalog.Install (Grants, Resolved, Console.Binding_Of (Op), Grant);
            Success := Grant = CCL.Catalog.Grant_Added;
         end if;
      end loop;
   end Install;

   procedure Invoke
     (Binding : Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      Image : CCL.Objects.Image;
      Member : Positive;
      Built, Found : Boolean;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      if not Handles (Binding) then return; end if;
      case Console.Operation'Val (Binding - Console.FIRST_BINDING) is
         when Console.Title =>
            if Argument.Kind = CCL.Host_Values.Text_Value then
               Set_Title (Argument.Content.Data (1 .. Argument.Content.Length));
               Reply := (Value => CCL.Host_Values.Boolean_Constant (True), Success => True, Why => <>);
            end if;
         when Console.Set_Notation =>
            if Argument.Kind = CCL.Host_Values.Object_Value then
               Console.Member_Of (Contracts.Notation, Argument.Object,
                                  Console.Notation'Pos (Console.Notation'Last) + 1, Member, Found);
               if Found then
                  Set_Notation (Console.Notation'Val (Member - 1));
                  Console.Notation_Value (Contracts.Notation, Notation, Image, Built);
                  if Built then
                     Reply := (Value => CCL.Host_Values.Object_Constant (Image), Success => True, Why => <>);
                  end if;
               end if;
            end if;
         when Console.Set_Theme =>
            if Argument.Kind = CCL.Host_Values.Object_Value then
               Console.Member_Of (Contracts.Theme, Argument.Object,
                                  Console.Theme'Pos (Console.Theme'Last) + 1, Member, Found);
               if Found then
                  Set_Theme (Console.Theme'Val (Member - 1));
                  Console.Theme_Value (Contracts.Theme, Theme, Image, Built);
                  if Built then
                     Reply := (Value => CCL.Host_Values.Object_Constant (Image), Success => True, Why => <>);
                  end if;
               end if;
            end if;
         when Console.Stats =>
            Console.Stats_Value (Contracts.Stats, Stats, Image, Built);
            if Built then
               Reply := (Value => CCL.Host_Values.Object_Constant (Image), Success => True, Why => <>);
            end if;
      end case;
   end Invoke;
end CCL_Console_Bindings;
