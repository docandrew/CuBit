with CuBit.Handles.C_ABI;
--  The admitted render endpoint (the startup "render" entry), for the C
--  discovery fixture (CAP-002: a handle, never a slot number).
function Mesa_Discovery_Endpoint return CuBit.Handles.C_ABI.C_Endpoint
  with Export, Convention => C, External_Name => "cubit_test_render_endpoint";
