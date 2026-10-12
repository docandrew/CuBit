with Interfaces; use Interfaces;
with CuBit.Log_Records;
--  How intel-gpu reports a refused device mapping (CAP-001): the text names
--  the physical range, and any status that says a mapping or grant was
--  denied is published as a Warning, so it reaches logstore and the Desktop
--  bootstrap screen next to the kernel's own denial record.
package Intel_GPU_Map_Report with SPARK_Mode is
   --  "map-denied 0x<physical> +0x<bytes>"
   function Denied (Physical, Bytes : Unsigned_64) return String
     with Post => Denied'Result'Length <= 64;
   --  Warning for a refusal ("denied" anywhere in Text), else Level.
   function Escalate
     (Text : String; Level : CuBit.Log_Records.Severity)
      return CuBit.Log_Records.Severity;
end Intel_GPU_Map_Report;
