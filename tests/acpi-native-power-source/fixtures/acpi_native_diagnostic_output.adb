package body ACPI_Native_Diagnostic_Output is
   procedure Write(Text:String;Peer_Quiet:Boolean;Success:Boolean:=False) is
      pragma Unreferenced(Success);
   begin
      if Text'Length=0 then raise Program_Error with "empty diagnostic";end if;
      Lines:=Lines+1;if not Peer_Quiet then Unsafe_Lines:=Unsafe_Lines+1;end if;
   end Write;
end ACPI_Native_Diagnostic_Output;
