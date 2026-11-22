with Interfaces; use Interfaces;
with Deadline_Ownership;
package Proof_Driver with SPARK_Mode is
   type Identity is record
      PID : Natural range 0 .. 255 := 0;
      Generation : Unsigned_64 := 0;
   end record;
   package Deadlines is new Deadline_Ownership (Identity, (0, 0));
   procedure Check with Ghost;
end Proof_Driver;
