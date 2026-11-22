pragma Ada_2022;
with CuBit.Handles;
with GPIO_Native_Driver;
package GPIO_Driver_Semantic_Fixture is
   -- Semantic-only instantiation. Actual executable must supply generated
   -- Startup.launcher; No_Process is intentionally unable to authenticate.
   Test_Operation_Budget : constant := 32;
   function Startup_Parked return Boolean is (True); -- semantic fixture only
   package Driver is new GPIO_Native_Driver
     (2, Test_Operation_Budget, CuBit.Handles.No_Process, Startup_Parked);
end GPIO_Driver_Semantic_Fixture;
