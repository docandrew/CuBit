pragma Ada_2022;
with CuBit.Messages;
with CuBit.Process_IDs;
package ACPI_System_Events with SPARK_Mode is
   type Action is (Not_System, Rejected, Channel_Wake, Grant_Notice,
      Stop_Requested, Ignored_Control);
   function Classify (Publisher : CuBit.Process_IDs.Process_ID;
      Item : CuBit.Messages.Message) return Action;
   -- Publisher is the event syscall's identity, never a payload word. Grant
   -- notices are hints: no cancellation, retirement, mapping or page reuse.
end ACPI_System_Events;
