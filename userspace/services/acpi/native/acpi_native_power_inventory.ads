pragma Ada_2022;
with Interfaces;
with CuBit.Messages;
with CuBit.Handles;
with ACPI_Power_Inventory_Watches;
with ACPI_Power_Watch_Protocol;
package ACPI_Native_Power_Inventory is
   procedure Changed;
   procedure Begin_Stop;
   function Quiet return Boolean;
   function Pending return Boolean;
   procedure Advance (Owner_Quiet : Boolean; Did_Work : out Boolean);
   procedure Dispatch (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Owner_Quiet : Boolean; Reply : out CuBit.Messages.Message; Handled,Reply_Now : out Boolean);
private
   procedure Save_Current (Token : out CuBit.Handles.Reply; Saved : out Boolean);
   procedure Answer_Saved (Token : in out CuBit.Handles.Reply; ID : Interfaces.Unsigned_64;
      Status : ACPI_Power_Watch_Protocol.Status; Generation : Interfaces.Unsigned_64; Delivered : out Boolean);
   package Policy is new ACPI_Power_Inventory_Watches
     (CuBit.Handles.Reply,CuBit.Handles.No_Reply,Save_Current=>Save_Current,Answer_Saved=>Answer_Saved);
end ACPI_Native_Power_Inventory;
