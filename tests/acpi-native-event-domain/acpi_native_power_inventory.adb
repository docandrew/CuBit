package body ACPI_Native_Power_Inventory is
 procedure Changed is null;procedure Begin_Stop is null;
 function Quiet return Boolean is(True);function Pending return Boolean is(False);
 procedure Advance(Owner_Quiet:Boolean;Did_Work:out Boolean) is
  pragma Unreferenced(Owner_Quiet);begin Did_Work:=False;end;
 procedure Dispatch(From:CuBit.Messages.Process_ID;Request:CuBit.Messages.Message;
  Owner_Quiet:Boolean;Reply:out CuBit.Messages.Message;Handled,Reply_Now:out Boolean) is
  pragma Unreferenced(From,Request,Owner_Quiet);
 begin Reply:=CuBit.Messages.NULL_MESSAGE;Handled:=False;Reply_Now:=True;end;
end ACPI_Native_Power_Inventory;
