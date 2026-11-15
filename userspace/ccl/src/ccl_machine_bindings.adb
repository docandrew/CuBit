with CCL.Interfaces.Machine;
with CCL.Objects;
with CCL_Machine;

package body CCL_Machine_Bindings is
   use type Interfaces.Unsigned_32;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Host_Values.Value_Kind;
   package M renames CCL.Interfaces.Machine;
   Contract : CCL.Objects.Binding;

   function Handles (Binding : Interfaces.Unsigned_32) return Boolean is
     (for some Op in M.Operation => Binding = M.Binding_Of (Op));

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
      --  Without a power endpoint the interface is simply absent.
      if not CCL_Machine.Available then return; end if;
      M.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if not Success then return; end if;
      CCL.Catalog.Resolve_Schema (Catalog, M.ANSWER_KEY, Contract);
      Success := CCL.Objects.Is_Bound (Contract);
      for Op in M.Operation loop
         exit when not Success;
         CCL.Catalog.Resolve (Catalog, "machine." & M.Name (Op), Resolved, Found);
         Success := Found;
         if Success then
            CCL.Catalog.Install (Grants, Resolved, M.Binding_Of (Op), Grant);
            Success := Grant = CCL.Catalog.Grant_Added;
         end if;
      end loop;
   end Install;

   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      Result : M.Answer;
      Image : CCL.Objects.Image;
      Built : Boolean;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      --  A parameterless call takes the Integer zero.
      if not Handles (Binding) or else Argument.Kind /= CCL.Host_Values.Integer_Value then
         return;
      end if;
      CCL_Machine.Request (M.Operation'Val (Binding - M.FIRST_BINDING), Result, Reply.Success, Reply.Why);
      if Reply.Success then
         M.Answer_Value (Contract, Result, Image, Built);
         Reply.Success := Built;
         if Built then
            Reply.Value := CCL.Host_Values.Object_Constant (Image);
         end if;
      end if;
   end Invoke;
end CCL_Machine_Bindings;
