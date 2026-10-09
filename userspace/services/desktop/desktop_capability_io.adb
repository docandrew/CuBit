with CuBit.Messages; use CuBit.Messages;
with System.Storage_Elements;
package body Desktop_Capability_IO with SPARK_Mode => Off is
   procedure Inspect (Slot : Interfaces.Unsigned_64;
      Data : out Words; Succeeded : out Boolean) is
      use Interfaces;
      Raw : aliased Words := (others => 0);
      Result : Unsigned_64;
   begin
      Result := syscall (SYSCALL_INSPECT_CAPABILITY, syscall (SYSCALL_GETPID),
        Slot, Unsigned_64 (System.Storage_Elements.To_Integer (Raw'Address)));
      Data := Raw; Succeeded := Result = 1;
   end Inspect;
end Desktop_Capability_IO;
