with CCL.Interfaces.Config;
with CCL_Config_IO;
package body CCL_Config_Bindings is
   use type Interfaces.Unsigned_32;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Host_Values.Value_Kind;
   Bindings : constant array (CCL.Interfaces.Config.Operation) of Interfaces.Unsigned_32 :=
     [CCL.Interfaces.Config.Get => 16#0004_0001#, CCL.Interfaces.Config.Keys => 16#0004_0002#];
   function Handles (Binding : Interfaces.Unsigned_32) return Boolean is
     (Binding in 16#0004_0001# .. 16#0004_0002#);
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
      -- Even discovery is gated. An absent grant is not a startup failure.
      if not CCL_Config_IO.Available then return; end if;
      CCL.Interfaces.Config.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if not Success then return; end if;
      for Op in CCL.Interfaces.Config.Operation loop
         CCL.Catalog.Resolve (Catalog, "config." & CCL.Interfaces.Config.Name (Op), Resolved, Found);
         if not Found then Success := False; return; end if;
         CCL.Catalog.Install (Grants, Resolved, Bindings (Op), Grant);
         if Grant /= CCL.Catalog.Grant_Added then Success := False; return; end if;
      end loop;
   end Install;
   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Value : out CCL.Host_Values.Value; Success : out Boolean)
   is
      Text : CCL.Host_Values.Text;
   begin
      Value := CCL.Host_Values.Text_Constant (Text); Success := False;
      if Argument.Kind /= CCL.Host_Values.Text_Value then return; end if;
      for Op in CCL.Interfaces.Config.Operation loop
         if Binding = Bindings (Op) then
            CCL_Config_IO.Query (Op, Argument.Content.Data (1 .. Argument.Content.Length), Text, Success);
            Value := CCL.Host_Values.Text_Constant (Text);
            return;
         end if;
      end loop;
   end Invoke;
end CCL_Config_Bindings;
