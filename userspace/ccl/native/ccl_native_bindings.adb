with CuBit.Handles.Startup;
with CuBit.Startup_Tables;

package body CCL_Native_Bindings is
   package HS renames CuBit.Handles.Startup;

   function Named (Label : String; Kind : CuBit.Startup_Tables.Capability_Kind :=
                     CuBit.Startup_Tables.Endpoint) return CuBit.Handles.Endpoint is
      Found : constant HS.Optional_Endpoint := HS.Optional_Endpoint_Named (Label, Kind);
   begin
      return (if Found.Present then Found.Value else CuBit.Handles.No_Endpoint);
   end Named;

   Clock_Entry            : constant CuBit.Handles.Endpoint := Named ("clock");
   Logstore_Entry         : constant CuBit.Handles.Endpoint := Named ("logstore");
   Filesystem_Entry       : constant CuBit.Handles.Endpoint := Named ("filesystem");
   Config_Entry           : constant CuBit.Handles.Endpoint := Named ("config");
   Process_Observer_Entry : constant CuBit.Handles.Endpoint := Named ("process-observer");
   Log_Observer_Entry     : constant CuBit.Handles.Endpoint := Named ("log-observer");
   Log_Control_Entry      : constant CuBit.Handles.Endpoint := Named ("log-control");
   Network_Entry          : constant CuBit.Handles.Endpoint :=
     Named ("network", CuBit.Startup_Tables.Network);

   function Clock            return CuBit.Handles.Endpoint is (Clock_Entry);
   function Logstore         return CuBit.Handles.Endpoint is (Logstore_Entry);
   function Filesystem       return CuBit.Handles.Endpoint is (Filesystem_Entry);
   function Config           return CuBit.Handles.Endpoint is (Config_Entry);
   function Process_Observer return CuBit.Handles.Endpoint is (Process_Observer_Entry);
   function Log_Observer     return CuBit.Handles.Endpoint is (Log_Observer_Entry);
   function Log_Control      return CuBit.Handles.Endpoint is (Log_Control_Entry);
   function Network          return CuBit.Handles.Endpoint is (Network_Entry);
end CCL_Native_Bindings;
