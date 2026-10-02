with Interfaces;
with CuBit.Metric_Records;
with CuBit.Metric_Protocol;
package Compositor_Metric_Completion with SPARK_Mode, Pure is
   use Interfaces;
   package Protocol renames CuBit.Metric_Protocol;
   type Payload is array (0 .. 3) of Unsigned_64;
   type Reply is record
      Kernel_Valid : Boolean := False;
      Kernel_Status : Unsigned_64 := 0;
      Label : Unsigned_32 := 0;
      Length, Flags : Unsigned_8 := 0;
      Reserved : Unsigned_16 := 0;
      Words : Payload := [others => 0];
   end record;
   -- Caller first selects an outstanding request by its non-reused kernel
   -- token. This checks the terminal reply, not token routing/authentication.
   function Definitive
     (Value : Reply; Sent : CuBit.Metric_Records.Record_Count) return Boolean is
     (Sent > 0 and then Value.Kernel_Valid and then Value.Kernel_Status = 0 and then
      Value.Length = Protocol.Message_Words and then Value.Flags = 0 and then
      Value.Reserved = 0 and then Value.Words (2) = 0 and then Value.Words (3) = 0 and then
      (if Value.Label = Protocol.Status'Enum_Rep (Protocol.OK) then
          Value.Words (0) <= Unsigned_64 (Sent) and then
          Value.Words (1) = Unsigned_64 (Sent) - Value.Words (0)
       elsif Value.Label in Protocol.Status'Enum_Rep (Protocol.Denied) |
         Protocol.Status'Enum_Rep (Protocol.Invalid_Request) |
         Protocol.Status'Enum_Rep (Protocol.Exhausted)
       then Value.Words (0) = 0 and Value.Words (1) = 0
       else False))
     with Post => (if Definitive'Result then
       Sent > 0 and Value.Kernel_Valid and Value.Kernel_Status = 0 and
       Value.Length = Protocol.Message_Words and Value.Flags = 0 and
       Value.Reserved = 0 and Value.Words (2) = 0 and Value.Words (3) = 0 and
       (if Value.Label = Protocol.Status'Enum_Rep (Protocol.OK) then
          Value.Words (0) <= Unsigned_64 (Sent) and
          Value.Words (1) <= Unsigned_64 (Sent) and
          Value.Words (0) + Value.Words (1) = Unsigned_64 (Sent)
        else Value.Label in Protocol.Status'Enum_Rep (Protocol.Denied) |
          Protocol.Status'Enum_Rep (Protocol.Invalid_Request) |
          Protocol.Status'Enum_Rep (Protocol.Exhausted) and
          Value.Words (0) = 0 and Value.Words (1) = 0));
end Compositor_Metric_Completion;
