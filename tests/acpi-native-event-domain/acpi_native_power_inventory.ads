with CuBit.Messages;
package ACPI_Native_Power_Inventory is
 procedure Changed;procedure Begin_Stop;
 function Quiet return Boolean;function Pending return Boolean;
 procedure Advance(Owner_Quiet:Boolean;Did_Work:out Boolean);
 procedure Dispatch(From:CuBit.Messages.Process_ID;Request:CuBit.Messages.Message;
  Owner_Quiet:Boolean;Reply:out CuBit.Messages.Message;Handled,Reply_Now:out Boolean);
end ACPI_Native_Power_Inventory;
