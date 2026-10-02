with Interfaces; use Interfaces;
package CuBit.Messages is
   subtype CapabilitySlot is Natural range 0 .. 63;
   SYSCALL_GETPID : constant := 1;
   SYSCALL_INSPECT_CAPABILITY : constant := 2;
   SYSCALL_POLICY_MINT_CAPABILITY_FOR_INCARNATION : constant := 120;
   SYSCALL_POLICY_DELEGATE_ENDPOINT : constant := 121;
   type Words is array (0 .. 5) of Unsigned_64;
   Inspection : Words := [1, 0, 0, 42, 0, 7];
   Arguments : Words := [others => 0];
   Current_PID : Unsigned_64 := 10;
   Inspect_Result : Unsigned_64 := 1;
   Mint_Result : Unsigned_64 := 0;
   Mint_Calls : Natural := 0;
   Last_Number : Unsigned_64 := 0;
   function Syscall (Number : Unsigned_64;
     A, B, C, D, E, F : Unsigned_64 := 0) return Unsigned_64;
end CuBit.Messages;
