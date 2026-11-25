with CCL.Interfaces.Graphics;
with CCL.Objects;
with CCL_Graphics;
with CuBit.Failures;

package body CCL_Graphics_Bindings is
   use type Interfaces.Unsigned_32;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Host_Values.Value_Kind;
   package G renames CCL.Interfaces.Graphics;
   Contract : CCL.Objects.Binding;

   function Handles (Binding : Interfaces.Unsigned_32) return Boolean is
     (for some Op in G.Operation => Binding = G.Binding_Of (Op));

   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean)
   is
      Error : CCL.Catalog.Catalog_Error;
      Grant : CCL.Catalog.Grant_Result;
      Resolved : CCL.Catalog.Resolved_Operation;
      Found : Boolean;
   begin
      Success := True;
      --  Without a graphics-observer endpoint the interface is simply absent.
      if not CCL_Graphics.Available then return; end if;
      G.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if not Success then return; end if;
      CCL.Catalog.Resolve_Schema (Catalog, G.ENTRIES_KEY, Contract);
      Success := CCL.Objects.Is_Bound (Contract);
      for Op in G.Operation loop
         exit when not Success;
         CCL.Catalog.Resolve (Catalog, "graphics." & G.Name (Op), Resolved, Found);
         Success := Found;
         if Success then
            CCL.Catalog.Install (Grants, Resolved, G.Binding_Of (Op), Grant);
            Success := Grant = CCL.Catalog.Grant_Added;
         end if;
      end loop;
   end Install;

   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      Entries : G.Entry_Array (1 .. G.MAXIMUM_LISTED);
      Count : Natural;
      Image : CCL.Objects.Image;
      Built : Boolean;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      --  A parameterless call takes the Integer zero.
      if not Handles (Binding) or else Argument.Kind /= CCL.Host_Values.Integer_Value then
         return;
      end if;
      CCL_Graphics.Read (Entries, Count, Reply.Success, Reply.Why);
      if not Reply.Success then
         return;
      end if;
      G.Report_Value (Contract, Entries (1 .. Count), Image, Built);
      Reply.Success := Built;
      if Built then
         Reply.Value := CCL.Host_Values.Object_Constant (Image);
      else
         Reply.Why := CuBit.Failures.Failed (CuBit.Failures.Exhausted, "graphics: the report does not fit one result");
      end if;
   end Invoke;
end CCL_Graphics_Bindings;
