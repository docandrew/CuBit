with Interfaces;
function Mesa_Discovery_Slot return Interfaces.Unsigned_64
  with Export, Convention => C, External_Name => "cubit_test_render_slot";
