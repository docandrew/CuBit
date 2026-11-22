pragma Ada_2022;
with Interfaces;
with CuBit.Logging;
with GPIO_Native_Resources;
generic
   Target : CuBit.Logging.Logstore_Handles.Optional_Endpoint;
package GPIO_Native_Log is
   type Bringup_Stage is (Identify, Resources, Inventory, Pin_Setup, Parked, Registration, Readiness);
   procedure Stage (Item : Bringup_Stage; OK : Boolean; Reason : String; Details : String := "");
   procedure Community (Ordinal : GPIO_Native_Resources.G.Memory_Resource_Ordinal;
      Item : GPIO_Native_Resources.Resource_Report);
   procedure Summary;
   procedure Disable (Reason : String);
   function Hex (Key : String; Value : Interfaces.Unsigned_64) return String;
   -- Main binds generated Startup.Logstore. Uses the existing logger unchanged
   -- and mirrors to serial. Call with compiled diagnostic names/enum images,
   -- never arbitrary firmware text. One success/failure per stage and one line
   -- per community; repeats counted in the single summary, no per-poll floods.
   -- Summary is complete only if every required stage succeeded and none failed.
   -- Publisher remains process-lived, including after an incomplete Disconnect.
end GPIO_Native_Log;
