with Interfaces; use Interfaces;
with Execution_Accounting;
package Proof_Driver with SPARK_Mode is
   -- Same owner representation/range as Process.ProcessID, without importing
   -- hardware-dependent Process packages into the hosted proof project.
   subtype PID is Natural range 0 .. 255;
   package A is new Execution_Accounting (PID, 0);
   procedure Prove_Handoff (Start, Middle, Finish : Unsigned_64)
     with Ghost, Pre => Start <= Middle and Middle <= Finish;
end Proof_Driver;
