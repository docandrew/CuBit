with CCL.Interfaces.Clock;
with CCL_Config_Bindings;
with CCL_Execution;
with CCL_Log_Bindings;

package body CCL_Host_Environment is
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;
   use type Interfaces.Integer_64;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Host_Values.Value_Kind;

   CLOCK_BINDING : constant Interfaces.Unsigned_32 := 16#0001_0001#;

   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean)
   is
      Error : CCL.Catalog.Catalog_Error;
      Grant : CCL.Catalog.Grant_Result;
      Resolved : CCL.Catalog.Resolved_Operation;
      Found : Boolean;
   begin
      CCL_Config_Bindings.Install (Catalog, Grants, Success);
      if Success then CCL_Execution.Install (Catalog, Grants, Success); end if;
      if Success then CCL_Log_Bindings.Install (Catalog, Grants, Success); end if;
      if not Success then return; end if;
      CCL.Interfaces.Clock.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if not Success then return; end if;
      CCL.Interfaces.Clock.Resolve_Monotonic_Ms (Catalog, Resolved, Found);
      Success := Found;
      if not Success then return; end if;
      CCL.Catalog.Install (Grants, Resolved, CLOCK_BINDING, Grant);
      Success := Grant = CCL.Catalog.Grant_Added;
   end Install;

   function Handles (Binding : Interfaces.Unsigned_32) return Boolean is
     (Binding = CLOCK_BINDING or else CCL_Config_Bindings.Handles (Binding) or else
      CCL_Log_Bindings.Handles (Binding));

   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      Available : Boolean;
      Milliseconds : Interfaces.Unsigned_64;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False);
      if CCL_Config_Bindings.Handles (Binding) then
         CCL_Config_Bindings.Invoke (Binding, Argument, Reply);
      elsif CCL_Log_Bindings.Handles (Binding) then
         CCL_Log_Bindings.Invoke (Binding, Argument, Reply);
      elsif Binding = CLOCK_BINDING and then
        Argument.Kind = CCL.Host_Values.Integer_Value and then Argument.Integer = 0
      then
         --  clock.monotonic-ms takes the Integer zero of a parameterless call.
         Milliseconds := Monotonic_Ms (Available);
         Reply.Success := Available and then
           Milliseconds <= Interfaces.Unsigned_64 (Interfaces.Integer_64'Last);
         if Reply.Success then
            Reply.Value := CCL.Host_Values.Integer_Constant (Interfaces.Integer_64 (Milliseconds));
         end if;
      end if;
   end Invoke;
end CCL_Host_Environment;
