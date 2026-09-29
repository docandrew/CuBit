with Interfaces;
generic
   -- Caller serializes exchanges, retains forcewake, and admits only an
   -- authenticated running GuC with the fixed runtime page mapped UC/NX.
   with function Owner_Ready return Boolean;
package Intel_GPU_Native_GuC_Mailbox is
   function Read_Word (Index : Natural) return Interfaces.Unsigned_32;
   procedure Write_Word
     (Index : Natural; Value : Interfaces.Unsigned_32; Success : out Boolean);
   procedure Notify (Success : out Boolean);
   -- Selector for tests; zero denies. Startup/other GT mailbox registers
   -- are deliberately not admitted by this adapter.
   function Address_For (Offset : Interfaces.Unsigned_32; Writing : Boolean)
     return Interfaces.Unsigned_64;
   -- False after a store is ambiguous; caller must retain backing resources
   -- and break the channel. Success is not firmware acknowledgement.
end Intel_GPU_Native_GuC_Mailbox;
