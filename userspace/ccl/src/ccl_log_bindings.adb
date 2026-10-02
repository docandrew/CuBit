with CCL.Interfaces.Logs;
with CCL.Objects;
with CCL_Log_IO;
package body CCL_Log_Bindings is
   use type Interfaces.Unsigned_32;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Host_Values.Value_Kind;
   RECENT_BINDING : constant Interfaces.Unsigned_32 := 16#0005_0001#;
   --  The LogEntries binding as this catalog resolved it.
   Contract : CCL.Objects.Binding;
   function Handles (Binding : Interfaces.Unsigned_32) return Boolean is
     (Binding = RECENT_BINDING);
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
      CCL.Catalog.Resolve_Schema (Catalog, CCL.Interfaces.Logs.SCHEMA_KEY, Contract);
      CCL.Catalog.Resolve (Catalog, "logs.recent", Resolved, Found);
      Success := Found and then CCL.Objects.Is_Bound (Contract);
      if not Success then return; end if;
      CCL.Catalog.Install (Grants, Resolved, RECENT_BINDING, Grant);
      Success := Grant = CCL.Catalog.Grant_Added;
   end Install;
   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      Image : CCL.Objects.Image;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False);
      if Binding /= RECENT_BINDING or else Argument.Kind /= CCL.Host_Values.Text_Value then return; end if;
      CCL_Log_IO.Recent (Argument.Content.Data (1 .. Argument.Content.Length), Contract, Image, Reply.Success);
      if Reply.Success then
         Reply.Value := CCL.Host_Values.Object_Constant (Image);
      end if;
   end Invoke;
end CCL_Log_Bindings;
