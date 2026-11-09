------------------------------------------------------------------------------
--  The reader's drain step instantiated for proof (run.sh --prove). The
--  loads and the two callbacks are opaque to the prover (Drain_Loads'
--  body is not SPARK), so the proof holds for every header and every slot
--  content the kernel (or anything else) could leave in the mapped area.
------------------------------------------------------------------------------
pragma Ada_2022;
with Kernel_Log_Publication;
with Kernel_Log_Publication.Drain;
with Drain_Loads; use Drain_Loads;

package Drain_Instance with SPARK_Mode is
   procedure Drain is new Kernel_Log_Publication.Drain
     (Load_Header, Load_Sequence, Load_Record, Deliver, Dropped);
end Drain_Instance;
