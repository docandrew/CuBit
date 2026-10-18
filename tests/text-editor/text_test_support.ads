with Interfaces; use Interfaces;

--  What the hosted tests share: checks with a summary, and a small
--  deterministic generator (xorshift64*) so every run with a seed repeats.
package Text_Test_Support is
   procedure Section (Title : String);
   procedure Check (Condition : Boolean; What : String);
   procedure Note (What : String);
   --  Prints the totals; exits with failure when a check failed.
   procedure Summary;
   function Failures return Natural;

   type Generator is record
      State : Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;
   end record;
   function Make (Seed : Unsigned_64) return Generator;
   --  0 .. Bound - 1 (0 when Bound is 0).
   function Random (G : in out Generator; Bound : Natural) return Natural;
end Text_Test_Support;
