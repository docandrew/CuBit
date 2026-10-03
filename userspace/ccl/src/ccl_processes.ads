with Interfaces;
with CCL.Interfaces.Processes;

--  The processes running now (proc.list), one body per platform: native/
--  asks the kernel's process table; the Linux preview's host/ reads /proc,
--  a Linux-hosted stand-in, not CuBit.
package CCL_Processes is
   package Processes renames CCL.Interfaces.Processes;
   MAXIMUM_NAME : constant := 32;
   MAXIMUM_IDENTITY : constant := 64;
   type Listed is record
      Pid : Natural := 0;
      Name : String (1 .. MAXIMUM_NAME) := [others => ' '];
      Name_Length : Natural range 0 .. MAXIMUM_NAME := 0;
      --  The manifest identity (com.cubit.x), "" when procmgr did not start it.
      Identity : String (1 .. MAXIMUM_IDENTITY) := [others => ' '];
      Identity_Length : Natural range 0 .. MAXIMUM_IDENTITY := 0;
      State : Processes.Run_State := Processes.Ready;
      Memory : Interfaces.Unsigned_64 := 0;
      --  The process that launched it (0: unknown) and how long it has run.
      Launcher : Natural := 0;
      Age_Ms : Interfaces.Unsigned_64 := 0;
   end record;
   subtype Listed_Count is Natural range 0 .. Processes.MAXIMUM_LISTED;
   type Listing is array (1 .. Processes.MAXIMUM_LISTED) of Listed;
   type Result_Kind is (Listed_All, Not_Granted, Unavailable);
   --  The first Count processes by pid; Total counts all of them.
   procedure List
     (Entries : out Listing; Count : out Listed_Count; Total : out Natural; Result : out Result_Kind);
end CCL_Processes;
