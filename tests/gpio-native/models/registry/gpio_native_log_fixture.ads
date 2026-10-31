pragma Ada_2022;
with GPIO_Native_Log;
package GPIO_Native_Log_Fixture is
   -- Actual Main supplies generated Startup.Logstore; fixture has no authority.
   package Logger is new GPIO_Native_Log ((Present => False));
end GPIO_Native_Log_Fixture;
