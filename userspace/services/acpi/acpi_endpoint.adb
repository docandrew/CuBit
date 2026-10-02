pragma Ada_2022;
package body ACPI_Endpoint with SPARK_Mode is
   function Encode (Result : Response) return Packet is
     (if Result.Status = OK then
        (Label => Reply_OK,
         Data => [Result.Data (0), Result.Data (1), Result.Data (2), Result.Data (3)], others => <>)
      else
        (Label => Reply_Error,
         Data => [Unsigned_64 (Outcome'Pos (Result.Status)), Result.Data (0), Result.Data (1), 0],
         others => <>));
   procedure Dispatch
     (Server : in out State; Config : Configuration;
      Stamp : Unsigned_64; Request : Packet; Reply : out Packet)
   is
      Result : Response;
   begin
      Handle (Server, Classify (Config, Stamp), Request, Result);
      Reply := Encode (Result);
   end Dispatch;
   procedure Dispatch_Block
     (Server : in out State; Config : Configuration; Stamp : Unsigned_64;
      Token : Unsigned_64; ID : Positive; Kind : ACPI_Service.Table_Kind;
      Data : Firmware_Tables.Bytes; Reply : out Packet) is
      Result : Response;
   begin
      Import_Block (Server, Classify (Config, Stamp), Token, ID, Kind, Data, Result);
      Reply := Encode (Result);
   end Dispatch_Block;
end ACPI_Endpoint;
