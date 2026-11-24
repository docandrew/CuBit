with CCL.Interfaces.Power;
with CCL.Objects;
with CCL_Power;
with CuBit.Failures;
with CuBit.Power_Sources;

package body CCL_Power_Bindings is
   use type Interfaces.Unsigned_32;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Host_Values.Value_Kind;
   package P renames CCL.Interfaces.Power;
   Contract : CCL.Objects.Binding;

   function Handles (Binding : Interfaces.Unsigned_32) return Boolean is
     (for some Op in P.Operation => Binding = P.Binding_Of (Op));

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
      --  Without a power-sources endpoint the interface is simply absent.
      if not CCL_Power.Available then return; end if;
      P.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if not Success then return; end if;
      CCL.Catalog.Resolve_Schema (Catalog, P.SOURCES_KEY, Contract);
      Success := CCL.Objects.Is_Bound (Contract);
      for Op in P.Operation loop
         exit when not Success;
         CCL.Catalog.Resolve (Catalog, "power." & P.Name (Op), Resolved, Found);
         Success := Found;
         if Success then
            CCL.Catalog.Install (Grants, Resolved, P.Binding_Of (Op), Grant);
            Success := Grant = CCL.Catalog.Grant_Added;
         end if;
      end loop;
   end Install;

   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      Sources : CuBit.Power_Sources.Source_Array;
      Count : CuBit.Power_Sources.Source_Count;
      Image : CCL.Objects.Image;
      Built : Boolean;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      --  A parameterless call takes the Integer zero.
      if not Handles (Binding) or else Argument.Kind /= CCL.Host_Values.Integer_Value then
         return;
      end if;
      CCL_Power.Read (Sources, Count, Reply.Success, Reply.Why);
      if not Reply.Success then
         return;
      end if;
      if Count > P.MAXIMUM_LISTED then
         Reply.Success := False;
         Reply.Why := CuBit.Failures.Failed
           (CuBit.Failures.Exhausted, "power:" & Natural'Image (Count) & " sources do not fit one result");
         return;
      end if;
      P.Sources_Value (Contract, Sources, Count, Image, Built);
      Reply.Success := Built;
      if Built then
         Reply.Value := CCL.Host_Values.Object_Constant (Image);
      else
         Reply.Why := CuBit.Failures.Failed (CuBit.Failures.Exhausted, "power: the sources do not fit one result");
      end if;
   end Invoke;
end CCL_Power_Bindings;
