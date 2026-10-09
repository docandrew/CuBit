with CuBit.Log_Records;
-- Audited native transport seam. Channel mappings and syscall effects remain
-- outside SPARK; framing, drop accounting and checkpoint policy are callers.
package Desktop_Log_IO with SPARK_Mode, Abstract_State => State,
  Initializes => State is
   procedure Echo (Text : String) with Global => (In_Out => State);
   procedure Emit (Value : CuBit.Log_Records.Log_Record; Accepted : out Boolean)
     with Global => (In_Out => State);
end Desktop_Log_IO;
