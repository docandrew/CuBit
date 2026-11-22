with CuBit.Control_Events;package ACPI_Native_Log is
 Attempts,Records,Dropped,Grant_Notices:Natural:=0;Shed:Boolean:=False;Last_Quiet:Boolean:=True;
 Text:String(1..256):=[others=>' '];Last:Natural:=0;
 procedure Copied(Line:String;Peer_Quiet:Boolean;Success:Boolean:=False);
 procedure Grant_Notice(Item:CuBit.Control_Events.Event);
end ACPI_Native_Log;