with Interfaces;
with CuBit.Program_Descriptions;

--  Starting programs with typed parameters for a CCL front end
--  (docs/ccl-launch-parameters.md), one body per platform: native/ goes
--  through procmgr (CuBit.Launching), lends a ring for each outlet and
--  reads them in place (launcher-owned outlet rings); the Linux preview's
--  host/ offers no programs, a Linux-hosted stand-in, not CuBit.
package CCL_Launcher is
   use type Interfaces.Integer_64;
   package PD renames CuBit.Program_Descriptions;

   MAXIMUM_PROGRAMS : constant := 16;
   --  A launch name: a table name ("ld.app") or a staged program's full
   --  name ("@system/work/remote/out/userspace/apps/hello/build/hello.app").
   MAXIMUM_NAME : constant := 255;
   subtype Name_Length is Natural range 0 .. MAXIMUM_NAME;
   MAXIMUM_WHY : constant := 200;
   subtype Why_Count is Natural range 0 .. MAXIMUM_WHY;
   --  Whether it can be offered as an interface (CCL.Interfaces.Programs
   --  .Program_State, whose Not_Published only the catalog decides).
   type Program_State is (Ready, Refused, Not_Described);
   type Program is record
      Name : String (1 .. MAXIMUM_NAME) := [others => ' '];
      Length : Name_Length := 0;
      --  Found beneath one of the launch table's staging directories
      --  (CuBit.Launch_Authority.Admits_Staged), not named by it.
      Staged : Boolean := False;
      State : Program_State := Ready;
      --  Not Ready: a sentence for the console.
      Why : String (1 .. MAXIMUM_WHY) := [others => ' '];
      Why_Length : Why_Count := 0;
      Description : PD.Signature;
   end record;
   subtype Program_Count is Natural range 0 .. MAXIMUM_PROGRAMS;
   type Program_Array is array (1 .. MAXIMUM_PROGRAMS) of Program;

   --  What this process may start: each name in its launch table, then
   --  each program staged beneath the table's staging directories (an
   --  ".app" file at most MAXIMUM_STAGING_DEPTH directories down, read
   --  through this process's own filesystem scopes), each with its
   --  description from procmgr, which runs the launch's admission for it
   --  (a staged one: a read scope over it and the Desktop's "Allow app
   --  replacement" switch). Looked at anew on every call: a program staged
   --  or removed since is found or gone. Only Ready ones have a
   --  Description. More: some did not fit MAXIMUM_PROGRAMS, or a staging
   --  directory held more than the walk's ceilings.
   MAXIMUM_STAGING_DEPTH : constant := 8;
   MAXIMUM_STAGING_DIRECTORIES : constant := 64;
   procedure Programs (Items : out Program_Array; Count : out Program_Count; More : out Boolean);

   --  Program Name (a launch name), as Programs would report it: Refused
   --  with why when this process may not start it (neither named by its
   --  launch table nor beneath one of its staging directories, or procmgr
   --  refused it). The program is not started.
   procedure Check (Name : String; Item : out Program);

   MAXIMUM_RUNS : constant := 16;
   subtype Run_Index is Positive range 1 .. MAXIMUM_RUNS;
   type Run is record
      Index : Run_Index := 1;
      --  The process identity (KERN-003): one life.
      Process : Interfaces.Unsigned_64 := 0;
   end record;

   type Start_Result is (Started, Not_Available, Refused, Too_Many_Runs);
   --  Start Name with argv and places rendered from V (CCL.Interfaces
   --  .Programs checked the types; Render checks the values), lending a ring
   --  for each outlet. Why: a sentence for the console when not
   --  Started.
   procedure Start
     (Name : String; Description : PD.Signature; V : PD.Values;
      Started_As : out Run; Result : out Start_Result;
      Why : out String; Why_Length : out Natural)
   with Pre => Why'First = 1 and then Why'Length >= 128;

   --  How a run ended, as the kernel reports it (CuBit.Child_Exits):
   --  Exited with a code it chose, or Stopped (killed, faulted, or its main
   --  thread ended), with no code.
   type Ending_Kind is (Exited, Stopped);
   EXIT_CODE_MODULUS : constant := 256;
   subtype Exit_Code is Interfaces.Integer_64 range 0 .. EXIT_CODE_MODULUS - 1;
   type Ending is record
      Kind : Ending_Kind := Stopped;
      Code : Exit_Code := 0;
   end record;

   --  What arrived since the last poll: Deliver for each complete line of an
   --  outlet (Connector: its index), then Ended with how it ended when the
   --  program has ended (and every line it wrote was delivered). A run this
   --  launcher does not know (released) is Ended and Stopped.
   generic
      with procedure Deliver (Port : PD.Connector_Index; Line : String);
   procedure Poll (Item : Run; Ended : out Boolean; How : out Ending);

   --  Forget a run that ended and whose lines were delivered (its rings).
   procedure Release (Item : Run);
end CCL_Launcher;
