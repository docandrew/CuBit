with Interfaces; package SCI_Probe is
 subtype Word is Interfaces.Unsigned_64;
 PM_Status, PM_Enable : Word := 0;
 Reads, Writes, Acks, Logs, Copied_Logs, Opening_Logs, Fixed_Calls, GPE_Calls, Other_Events : Natural := 0;
 Ack_OK, Write_OK : Boolean := True;
 Last_Quiet : Boolean := False;
end SCI_Probe;