with Compositor_Readers;
-- Unconstrained foreign confirmations: proof must handle either result.
package Reader_Proof with SPARK_Mode is
   procedure Release_Lease (Confirmed : out Boolean)
     with Import, Global => null;
   procedure Retire_Grant (Index : Positive; Confirmed : out Boolean)
     with Import, Global => null;
   package R is new Compositor_Readers (3, Release_Lease, Retire_Grant);
end Reader_Proof;
