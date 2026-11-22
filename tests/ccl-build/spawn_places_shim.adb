package body Spawn_Places_Shim is
   use type Interfaces.C.size_t;

   function c_posix_spawn
     (Result : System.Address; Path : System.Address; File_Actions, Attributes : System.Address;
      Arguments, Environment : System.Address) return Interfaces.C.int
     with Import, Convention => C, External_Name => "posix_spawn";

   --  EINVAL (<errno.h>), as the CuBit libc answers a malformed region.
   Invalid_Argument : constant Interfaces.C.int := 22;

   function Spawn_With_Places
     (Result : System.Address; Path : System.Address;
      File_Actions : System.Address; Attributes : System.Address;
      Arguments, Environment : System.Address;
      Places_Region : System.Address; Places_Bytes : Interfaces.C.size_t) return Interfaces.C.int
   is
   begin
      Calls := Calls + 1;
      if Places_Bytes > Interfaces.C.size_t (LG.Maximum_Bytes) then
         return Invalid_Argument;
      end if;
      declare
         Region : constant LG.Bytes (1 .. Natural (Places_Bytes))
         with Import, Address => Places_Region;
      begin
         Last_Length := Region'Length;
         Last (1 .. Last_Length) := Region;
         if Region'Length > 0 and then not LG.Valid (Region) then
            return Invalid_Argument;
         end if;
      end;
      return c_posix_spawn (Result, Path, File_Actions, Attributes, Arguments, Environment);
   end Spawn_With_Places;

end Spawn_Places_Shim;
