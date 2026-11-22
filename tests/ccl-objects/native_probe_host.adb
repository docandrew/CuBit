with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces;
with System;
with Config_Native_Probe;

--  Linux lifecycle adapter around the exact no-host-IO native test entry.
procedure Native_Probe_Host is
   use type System.Address;
   use type Interfaces.Unsigned_32;
   use type Config_Native_Probe.Boot_Phase;
   function Open_Database (Path : System.Address; Length : Interfaces.Unsigned_64)
      return System.Address
     with Import, Convention => C, External_Name => "cubit_config_test_open";
   function Close_Database (Handle : System.Address) return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "cubit_config_test_close";
   Path : constant String := Ada.Command_Line.Argument (1);
   Database : System.Address;
   Result, Closed, Repeated, Invalid_Phase : Interfaces.Unsigned_32;
begin
   if Config_Native_Probe.Run (System.Null_Address, 0) /= 1 then
      raise Program_Error with "null database accepted";
   end if;
   for Phase in Config_Native_Probe.Boot_Phase loop
      Database := Open_Database (Path'Address, Path'Length);
      if Database = System.Null_Address then raise Program_Error with "open failed"; end if;
      Result := Config_Native_Probe.Run
        (Database, Config_Native_Probe.Boot_Phase'Enum_Rep (Phase));
      --  A successful seed/advance must NOT be silently repeated/reseeded.
      Repeated := Config_Native_Probe.Run
        (Database, Config_Native_Probe.Boot_Phase'Enum_Rep (Phase));
      Invalid_Phase := Config_Native_Probe.Run (Database, Interfaces.Unsigned_32'Last);
      Closed := Close_Database (Database);
      if Result /= 0 or else Closed /= 1 then
         raise Program_Error with Phase'Image & " checkpoint" & Result'Image & " close" & Closed'Image;
      end if;
      if Repeated /= (case Phase is when Config_Native_Probe.Seed => 20,
                                    when Config_Native_Probe.Advance => 9,
                                    when Config_Native_Probe.Verify_Only => 0) or else
        Invalid_Phase /= 1
      then raise Program_Error with "invalid/repeated phase accepted"; end if;
   end loop;
   Ada.Text_IO.Put_Line ("Native-compatible worker entry: seed/advance/verify across reopen PASS (hosted)");
end Native_Probe_Host;
