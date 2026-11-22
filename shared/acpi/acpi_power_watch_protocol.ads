pragma Ada_2022;
with Interfaces;
-- Observer-badged endpoint only. Request [nonzero ID,seen generation,0,0].
-- Reply [ID,status,generation,schema]. One outstanding request per actual
-- kernel sender, at most8 senders; a new watch supersedes only that sender.
-- Generation is process/session-local and never wraps. Seen=0 synchronizes
-- to the initial generation. A restarted owner requires a fresh connection.
-- This generation describes power inventory changes, not the cache generation
-- required by auxiliary Read_Metrics pages. Readers reread page21/22 summaries.
-- This uses the former unserved label29. Labels30..38 are retired; native
-- measurements use Read_Metrics pages20..22, never those old query labels.
package ACPI_Power_Watch_Protocol with Pure, SPARK_Mode is
   subtype Word is Interfaces.Unsigned_64;
   subtype Generation_Limit is Word range 1..Word(Interfaces.Integer_64'Last);
   Watch : constant Interfaces.Unsigned_32 := 29;
   Schema : constant Word := 1;
   Maximum_Watches : constant Positive := 8;
   type Status is (Changed, Superseded, Unavailable, Stopping, Exhausted,
      Denied, Malformed, Full, Future_Generation);
   for Status use (Changed=>0,Superseded=>1,Unavailable=>2,Stopping=>3,
      Exhausted=>4,Denied=>5,Malformed=>6,Full=>7,Future_Generation=>8);
end ACPI_Power_Watch_Protocol;
