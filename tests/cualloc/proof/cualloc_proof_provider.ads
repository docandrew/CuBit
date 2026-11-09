--  A provider for proof only: no bodies, no known answers.
with Interfaces; use Interfaces;
package CuAlloc_Proof_Provider with SPARK_Mode is
   function Reserve (Bytes, Alignment : Unsigned_64) return Unsigned_64 with Import, Global => null;
   function Commit (Base, Offset, Bytes : Unsigned_64) return Boolean with Import, Global => null;
   function Release (Base, Bytes : Unsigned_64) return Boolean with Import, Global => null;
   procedure Attached with Import, Global => null, Always_Terminates;
   --  The kernel's commit cap, and one that is not a page multiple (the
   --  heap must refuse to grow rather than misuse the provider).
   Kernel_Commit : constant := 16 * 1_048_576;
   Odd_Commit : constant := 12_345;
end CuAlloc_Proof_Provider;
