with Ada.Unchecked_Conversion;
package body CuBit.Memory_Grants is
   type Page_Access is access all CuBit.Metric_Protocol.Summary_Page;
   function Pointer is new Ada.Unchecked_Conversion (System.Address, Page_Access);
   Address : System.Address := System.Null_Address;
   procedure Create_Via_Capability
     (slot : CuBit.Messages.CapabilitySlot; localAddr : System.Address;
      numPages : Natural; readWrite : Boolean; reference : out Grant_Reference; success : out Boolean) is
   begin
      pragma Assert (slot = 17 and numPages = 1 and readWrite);
      Creates := Creates + 1; Address := localAddr;
      reference := (7, Unsigned_64 (Creates)); success := Allow_Create;
   end Create_Via_Capability;
   procedure Revoke (reference : Grant_Reference; success : out Boolean) is
   begin
      pragma Assert (reference.slot = 7);
      Revokes := Revokes + 1; success := Allow_Revoke;
   end Revoke;
   function Retirement_Confirmed (reference : Grant_Reference) return Boolean is
   begin
      pragma Assert (reference.slot = 7);
      Checks := Checks + 1; return Allow_Retirement;
   end Retirement_Confirmed;
   procedure Write_Bytes (Data : Bytes) is
      type Byte_Access is access all Bytes;
      function As_Bytes is new Ada.Unchecked_Conversion (System.Address, Byte_Access);
   begin As_Bytes (Address).all := Data; end Write_Bytes;
   procedure Write_Page (Rows : CuBit.Metric_Protocol.Summary_Page) is
   begin Pointer (Address).all := Rows; end Write_Page;
end CuBit.Memory_Grants;
