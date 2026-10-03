with CCL.Interfaces.Logs;
with CCL.Objects;
with CCL_Log_IO;
package body CCL_Log_Bindings is
   use type Interfaces.Unsigned_32;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Host_Values.Value_Kind;
   package Logs renames CCL.Interfaces.Logs;
   FIRST_BINDING : constant Interfaces.Unsigned_32 := 16#0005_0001#;
   function Binding_Of (Op : Logs.Operation) return Interfaces.Unsigned_32 is
     (FIRST_BINDING + Logs.Operation'Pos (Op));
   --  The LogEntries and Severity bindings as this catalog resolved them.
   Contract, Severity_Contract : CCL.Objects.Binding;
   function Handles (Binding : Interfaces.Unsigned_32) return Boolean is
     (Binding in FIRST_BINDING .. Binding_Of (Logs.Operation'Last));
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
      --  Even discovery is gated. Without logstore, logs is simply absent.
      if not CCL_Log_IO.Available then return; end if;
      CCL.Interfaces.Logs.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if not Success then return; end if;
      CCL.Catalog.Resolve_Schema (Catalog, Logs.SCHEMA_KEY, Contract);
      CCL.Catalog.Resolve_Schema (Catalog, Logs.SEVERITY_KEY, Severity_Contract);
      Success := CCL.Objects.Is_Bound (Contract) and then CCL.Objects.Is_Bound (Severity_Contract);
      for Op in Logs.Operation loop
         exit when not Success;
         CCL.Catalog.Resolve (Catalog, "logs." & Logs.Name (Op), Resolved, Found);
         Success := Found;
         if Success then
            CCL.Catalog.Install (Grants, Resolved, Binding_Of (Op), Grant);
            Success := Grant = CCL.Catalog.Grant_Added;
         end if;
      end loop;
   end Install;
   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      Image : CCL.Objects.Image;
      Level, Previous : Logs.Severity;
      Found, Built : Boolean;
      --  A Severity result, or the failure to build one.
      procedure Answer (Value : Logs.Severity) is
      begin
         Logs.Severity_Value (Severity_Contract, Value, Image, Built);
         Reply.Success := Reply.Success and then Built;
         if Reply.Success then
            Reply.Value := CCL.Host_Values.Object_Constant (Image);
         end if;
      end Answer;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      if not Handles (Binding) then return; end if;
      case Logs.Operation'Val (Binding - FIRST_BINDING) is
         when Logs.Recent =>
            if Argument.Kind /= CCL.Host_Values.Text_Value then return; end if;
            CCL_Log_IO.Recent (Argument.Content.Data (1 .. Argument.Content.Length), Contract, Image, Reply.Success,
                               Reply.Why);
            if Reply.Success then
               Reply.Value := CCL.Host_Values.Object_Constant (Image);
            end if;
         when Logs.Minimum =>
            CCL_Log_IO.Minimum (Level, Reply.Success, Reply.Why);
            Answer (Level);
         when Logs.Set_Minimum =>
            if Argument.Kind /= CCL.Host_Values.Object_Value then return; end if;
            Logs.Severity_Of (Severity_Contract, Argument.Object, Level, Found);
            if not Found then return; end if;
            CCL_Log_IO.Set_Minimum (Level, Previous, Reply.Success, Reply.Why);
            Answer (Previous);
      end case;
   end Invoke;
end CCL_Log_Bindings;
