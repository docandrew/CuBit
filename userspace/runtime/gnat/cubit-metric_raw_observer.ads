pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Metric_Protocol;
with CuBit.Process_IDs;
package CuBit.Metric_Raw_Observer with SPARK_Mode is
   package P renames CuBit.Metric_Protocol;
   --  Synchronous collector only. Serialize calls and keep Slot unchanged;
   --  retain this object until Disconnect reports Done. Never use in rendering.
   type Observer (Slot : CuBit.Messages.CapabilitySlot) is limited private;
   function Disabled (Item : Observer) return Boolean;
   function Incarnation (Item : Observer) return CuBit.Process_IDs.Process_ID;
   procedure Query (Item : in out Observer; Cursor : Unsigned_64;
      Page : out P.Raw_Page; Written : out P.Raw_Row_Count;
      Next, Gap, Dropped : out Unsigned_64; Result : out P.Status);
   procedure Disconnect (Item : in out Observer; Done : out Boolean);
private
   type Buffer is new P.Raw_Page with Alignment => 4096;
   type Observer (Slot : CuBit.Messages.CapabilitySlot) is limited record
      Page : Buffer := (others => (others => 0));
      Grant : CuBit.Memory_Grants.Grant_Reference;
      Has_Grant, Off : Boolean := False;
      Identity : CuBit.Process_IDs.Process_ID := CuBit.Process_IDs.No_Process;
   end record;
end CuBit.Metric_Raw_Observer;
