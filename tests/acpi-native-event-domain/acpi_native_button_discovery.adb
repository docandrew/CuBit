package body ACPI_Native_Button_Discovery is
   procedure Start (Server : in out ACPI_Requests.State; Fixed_Available : Boolean; Result : out Outcome) is
   begin
      if not ACPI_Requests.Valid (Server) or else Fixed_Available then raise Program_Error with "unexpected button source in battery fixture"; end if;
      Result := Source_Unavailable;
   end Start;
   procedure Route (Server : ACPI_Requests.State; Accepted : out Boolean) is
   begin Accepted := ACPI_Requests.Valid (Server); end Route;
   procedure Begin_Stop is null;
   procedure Close (Released : out Boolean) is
   begin Released := True; end Close;
end ACPI_Native_Button_Discovery;
