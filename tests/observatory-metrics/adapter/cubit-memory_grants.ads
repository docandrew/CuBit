with Interfaces; use Interfaces;
with System;
with CuBit.Messages;
with CuBit.Metric_Protocol;
package CuBit.Memory_Grants is
   type Grant_Reference is record
      slot, generation : Unsigned_64 := 0;
   end record;
   Allow_Create, Allow_Revoke, Allow_Retirement : Boolean := True;
   Creates, Revokes, Checks : Natural := 0;
   procedure Create_Via_Capability
     (slot : CuBit.Messages.CapabilitySlot; localAddr : System.Address;
      numPages : Natural; readWrite : Boolean; reference : out Grant_Reference; success : out Boolean);
   procedure Revoke (reference : Grant_Reference; success : out Boolean);
   function Retirement_Confirmed (reference : Grant_Reference) return Boolean;
   procedure Write_Page (Rows : CuBit.Metric_Protocol.Summary_Page);
end CuBit.Memory_Grants;
