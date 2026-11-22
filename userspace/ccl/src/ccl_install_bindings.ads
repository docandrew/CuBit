with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;
with CCL_Stream_Table;

--  system.disks, system.install and system.progress (CCL.Interfaces.Install)
--  over the platform's CCL_Installer. system.install answers with a task at
--  once; Pump asks the installer how the install stands and completes the
--  task with its report when it has finished.
package CCL_Install_Bindings is
   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean);
   function Handles (Binding : Interfaces.Unsigned_32) return Boolean;
   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Streams : in out CCL_Stream_Table.Table;
      Reply : out CCL.Host_Values.Call_Result);
   --  Now: the monotonic clock in milliseconds (Clocked False: none), which
   --  paces the installer queries.
   procedure Pump
     (Streams : in out CCL_Stream_Table.Table; Now : Interfaces.Unsigned_64; Clocked : Boolean;
      Arrived : out Boolean);
   --  Milliseconds until Pump next asks the installer (0: now), or
   --  Unsigned_64'Last when no install is followed: a front end that sleeps
   --  until its next deadline wakes for it.
   function Next_Query_Delay (Now : Interfaces.Unsigned_64) return Interfaces.Unsigned_64;
end CCL_Install_Bindings;
