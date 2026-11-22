pragma Ada_2022;
with GPIO_Driver_Semantic_Fixture;
with GPIO_Native_Pump;
package GPIO_Native_Pump_Fixture is
   procedure Disable_Logging (Reason : String);
   package Runner is new GPIO_Native_Pump
     (GPIO_Driver_Semantic_Fixture.Driver, 3, 2, 1, 1000, 1000, Disable_Logging);
end GPIO_Native_Pump_Fixture;
