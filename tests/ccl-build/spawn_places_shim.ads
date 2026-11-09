with Interfaces.C;
with System;
with CuBit.Launch_Grants;

--  The CuBit libc's cubit_posix_spawn_places (<cubit/launch.h>), hosted on
--  Linux for platform_tests built with the CuBit body of Build_Platform:
--  it keeps the places region it is given, so the test can check what a
--  step would be delegated, then starts the program with posix_spawn (a
--  Linux host has no places to give it).
package Spawn_Places_Shim is

   package LG renames CuBit.Launch_Grants;

   --  Calls so far, and the last call's region (Last (1 .. Last_Length)).
   Calls : Natural := 0;
   Last : LG.Bytes (1 .. LG.Maximum_Bytes) := [others => 0];
   Last_Length : Natural := 0;

   function Spawn_With_Places
     (Result : System.Address; Path : System.Address;
      File_Actions : System.Address; Attributes : System.Address;
      Arguments, Environment : System.Address;
      Places_Region : System.Address; Places_Bytes : Interfaces.C.size_t) return Interfaces.C.int
   with Export, Convention => C, External_Name => "cubit_posix_spawn_places";

end Spawn_Places_Shim;
