with CuBit.Config_Inspection;
with CuBit.Handles.Services;
with CuBit.Settings.Inspection;
--  The CCL config built-ins. A CCL program's needs are not known when the
--  host is built, so the host finds its "config" entry at run time (the
--  run-time discovery path, docs/declaration-checking.md); Config checks
--  every key against the host's scopes, and Probe tells whether it may
--  inspect at all.
package body CCL_Config_IO is
   use CuBit.Config_Inspection;
   Found : constant CuBit.Handles.Services.Config.Optional_Endpoint :=
     CuBit.Handles.Services.Config.Optional_Named ("config");

   procedure Inspect (Op : Operation; Key : String; Value : out Text; Result : out Status) is
   begin
      if not Found.Present then
         Value := (others => <>);
         Result := Unavailable;
         return;
      end if;
      CuBit.Settings.Inspection.Inspect
        (CuBit.Settings.Inspection.Using (Found.Value), Op, Key, Machine_Context, Value, Result);
   end Inspect;

   function Available return Boolean is
      Value : Text;
      Result : Status;
   begin
      Inspect (Probe, "", Value, Result);
      return Result = OK;
   end Available;

   procedure Query
     (Op : CCL.Interfaces.Config.Operation; Key : String;
      Value : out CCL.Host_Values.Text; Success : out Boolean)
   is
      Data : Text;
      Result : Status;
   begin
      Inspect ((case Op is when CCL.Interfaces.Config.Get => Read_Value,
                           when CCL.Interfaces.Config.Keys => List_Keys), Key, Data, Result);
      Value := (others => <>);
      Success := Result = OK;
      if Success then
         CCL.Host_Values.Copy_Text (Data.Data (1 .. Data.Length), Value, Success);
      end if;
   end Query;
end CCL_Config_IO;
