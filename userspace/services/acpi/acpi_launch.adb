package body ACPI_Launch with SPARK_Mode is
   function Decode (Stamp : Unsigned_64; Request : ACPI_Requests.Packet)
     return Decision is
      Config : constant ACPI_Endpoint.Configuration :=
        (Request.Data (0), Request.Data (1));
   begin
      if Stamp /= Bootstrap_Tag or else Request.Label /= Configure
        or else Request.Length /= 4 or else Request.Flags /= 0 or else Request.Reserved /= 0
        or else Request.Data (3) /= 0 or else Request.Data (2) not in Provider_Slot_Number
        or else not ACPI_Endpoint.Valid (Config)
        or else Config.Observer_Tag = Bootstrap_Tag or else Config.Provider_Tag = Bootstrap_Tag
      then return (Accepted => False); end if;
      return (Accepted => True, Config => Config, Provider_Slot => Request.Data (2));
   end Decode;
end ACPI_Launch;
