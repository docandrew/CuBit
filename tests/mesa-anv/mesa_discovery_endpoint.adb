with Startup;
function Mesa_Discovery_Endpoint return CuBit.Handles.C_ABI.C_Endpoint is
begin
   return CuBit.Handles.C_ABI.To_C (Startup.Render);
end Mesa_Discovery_Endpoint;
