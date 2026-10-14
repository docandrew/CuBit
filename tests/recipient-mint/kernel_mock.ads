--  The scripted kernel behind CuBit.Handles.Kernel.Call (handles_kernel_mock.adb):
--  what the next call answers, and what the last call was given.
with Interfaces; use Interfaces;
with Capability_ABI;
package Kernel_Mock is
   type Arguments is array (0 .. 6) of Unsigned_64;
   --  INSPECT_CAPABILITY: the record it writes and its result word (0, or
   --  a refusal code).
   Inspection     : Capability_ABI.Inspection := [others => 0];
   Inspect_Result : Unsigned_64 := 0;
   --  Whether the handle names a live entry: the kernel writes the record
   --  only then (a refusal leaves the caller's buffer untouched).
   Live           : Boolean := True;
   --  Every other call's result word.
   Answer         : Unsigned_64 := 0;
   Last_Number    : Unsigned_64 := 0;
   Last_Arguments : Arguments := [others => 0];
   --  The derivation record the last install/reply/derive call pointed at.
   Last_Derivation : Capability_ABI.Derivation := [others => 0];
   Calls          : Natural := 0;
end Kernel_Mock;
