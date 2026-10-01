with Interfaces; use Interfaces;
with Intel_GPU_Physical_Extents;
package Intel_GPU_Extent_Replies with SPARK_Mode is
   package E renames Intel_GPU_Physical_Extents;
   type Words is array (Natural range 0 .. 3) of Unsigned_64;
   -- Four-word supervisor reply: index, DMA address, CPU address, arena ID.
   -- The transport must authenticate endpoint/incarnation and completion token
   -- before each Accept_Reply. Arena ID is correlation, never authority.
   type Assembly is limited private;
   procedure Start
     (Object : in out Assembly; CPU_Base, Arena_ID : Unsigned_64;
      Success : out Boolean);
   procedure Accept_Reply
     (Object : in out Assembly; Data : Words; Success : out Boolean);
   procedure Cancel (Object : in out Assembly);
   function Result (Object : Assembly) return E.Map;
   -- No partial map escapes. Failure/cancellation is terminal for this object;
   -- backing is retained by the supervisor, not freed by this decoder.
private
   type Assembly is limited record
      Started, Broken : Boolean := False;
      CPU, Identity : Unsigned_64 := 0;
      Count : Natural range 0 .. 16 := 0;
      Bases : E.Addresses := [others => 0];
      Mapping : E.Map;
   end record;
end Intel_GPU_Extent_Replies;
