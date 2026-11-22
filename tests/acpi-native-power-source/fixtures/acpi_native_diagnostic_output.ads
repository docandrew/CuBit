package ACPI_Native_Diagnostic_Output is
   Lines,Unsafe_Lines:Natural:=0;
   procedure Write(Text:String;Peer_Quiet:Boolean;Success:Boolean:=False);
end ACPI_Native_Diagnostic_Output;
