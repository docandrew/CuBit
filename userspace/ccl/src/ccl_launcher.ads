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
   MAXIMUM_NAME : constant := 64;
   subtype Name_Length is Natural range 0 .. MAXIMUM_NAME;
   type Program is record
      Name : String (1 .. MAXIMUM_NAME) := [others => ' '];
      Length : Name_Length := 0;
      Description : PD.Signature;
   end record;
   subtype Program_Count is Natural range 0 .. MAXIMUM_PROGRAMS;
   type Program_Array is array (1 .. MAXIMUM_PROGRAMS) of Program;

   --  What this process may start (its launch table), with each program's
   --  description; programs without one, or with a malformed one, are left
   --  out.
   procedure Programs (Items : out Program_Array; Count : out Program_Count);

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
