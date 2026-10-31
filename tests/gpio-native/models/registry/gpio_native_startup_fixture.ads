pragma Ada_2022;
with GPIO_Driver_Semantic_Fixture;
with GPIO_Startup;
with GPIO_Native_Communities;
package GPIO_Native_Startup_Fixture is
   package Owner is new GPIO_Startup
     (GPIO_Driver_Semantic_Fixture.Driver.Registry, GPIO_Native_Communities.Admit_Pin);
end GPIO_Native_Startup_Fixture;
