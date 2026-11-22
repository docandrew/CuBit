pragma Ada_2022;
with GPIO_Native_Pump_Fixture;
with GPIO_Polling_Loop;
with GPIO_Native_Clock;
package GPIO_Native_Polling_Fixture is
   -- Fixture periods are test data, not hardware timing recommendations.
   package Runner is new GPIO_Polling_Loop
     (GPIO_Native_Pump_Fixture.Runner.Pump, GPIO_Native_Clock.Read, 10, 100, 5);
end GPIO_Native_Polling_Fixture;
