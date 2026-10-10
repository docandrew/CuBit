pragma Ada_2022;
with Interfaces;
with ACPI_Requests;
with ACPI_Service;
with Firmware_Tables;
-- The tags and received stamp are trusted native-adapter inputs. This unit
-- classifies existing authority; it cannot allocate or authenticate a tag.
package ACPI_Endpoint with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   use Interfaces;
   use ACPI_Requests;
   type Configuration is record
      Observer_Tag : Unsigned_64 := 0;
      Provider_Tag : Unsigned_64 := 0;
   end record;
   function Valid (Config : Configuration) return Boolean is
     (Config.Observer_Tag /= 0 and then Config.Provider_Tag /= 0
      and then Config.Observer_Tag /= Config.Provider_Tag);
   function Classify (Config : Configuration; Stamp : Unsigned_64)
     return Authority is
     (if not Valid (Config) then No_Authority
      elsif Stamp = Config.Observer_Tag then Observer
      elsif Stamp = Config.Provider_Tag then Snapshot_Provider
      else No_Authority);

   Reply_OK : constant Unsigned_32 := 16#F000#;
   Reply_Error : constant Unsigned_32 := 16#F001#;
   -- Success preserves the four response words. Errors carry
   -- [outcome, revision, admission detail, 0]. These are service-local codes.
   function Encode (Result : Response) return Packet with
     Post => Encode'Result.Length = 4 and then Encode'Result.Flags = 0
       and then Encode'Result.Reserved = 0
       and then (if Result.Status = OK then
         Encode'Result.Label = Reply_OK and then Encode'Result.Data =
           [Result.Data (0), Result.Data (1), Result.Data (2), Result.Data (3)]
       else Encode'Result.Label = Reply_Error and then
         Encode'Result.Data =
           [Unsigned_64 (Outcome'Pos (Result.Status)), Result.Data (0), Result.Data (1), 0]);

   procedure Dispatch
     (Server : in out State; Config : Configuration;
      Stamp : Unsigned_64; Request : Packet; Reply : out Packet) with
     Pre => ACPI_Requests.Valid (Server),
     Post => ACPI_Requests.Valid (Server) and then Reply.Length = 4 and then Reply.Flags = 0 and then Reply.Reserved = 0
       and then Reply.Label in Reply_OK | Reply_Error
       and then (for all Word of Reply.Data => Word <= Max_Revision)
       and then (if Classify (Config, Stamp) /= Snapshot_Provider then Model (Server) = Model (Server)'Old)
       and then (if Classify (Config, Stamp) = No_Authority then
         Reply.Label = Reply_Error and then
         Reply.Data = [Unsigned_64 (Outcome'Pos (Denied)), 0, 0, 0]);
   -- Typed bulk adapter boundary; no wire address is accepted. Mapping and
   -- stability obligations are those of ACPI_Requests.Import_Block.
   procedure Dispatch_Block
     (Server : in out State; Config : Configuration; Stamp : Unsigned_64;
      Token : Unsigned_64; ID : Positive; Kind : ACPI_Service.Table_Kind;
      Data : Firmware_Tables.Bytes; Reply : out Packet) with
     Pre => ACPI_Requests.Valid (Server),
     Post => ACPI_Requests.Valid (Server) and then Reply.Length = 4 and then Reply.Flags = 0 and then Reply.Reserved = 0
       and then Reply.Label in Reply_OK | Reply_Error
       and then (for all Word of Reply.Data => Word <= Max_Revision)
       and then (if Classify (Config, Stamp) /= Snapshot_Provider then Model (Server) = Model (Server)'Old)
       and then (if Classify (Config, Stamp) = No_Authority then
         Reply.Label = Reply_Error and then
         Reply.Data = [Unsigned_64 (Outcome'Pos (Denied)), 0, 0, 0]);
end ACPI_Endpoint;
