package body ACPI_Native_Log is
 procedure Copied(Line:String;Peer_Quiet:Boolean;Success:Boolean:=False) is pragma Unreferenced(Success);
 begin Attempts:=Attempts+1;Last_Quiet:=Peer_Quiet;if Shed then Dropped:=Dropped+1;else
 Records:=Records+1;Last:=Line'Length;Text(1..Last):=Line;end if;end;
 procedure Grant_Notice(Item:CuBit.Control_Events.Event) is pragma Unreferenced(Item);begin Grant_Notices:=Grant_Notices+1;end;
end ACPI_Native_Log;