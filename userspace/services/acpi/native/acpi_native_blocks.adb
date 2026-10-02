with System;
with Firmware_Tables;
package body ACPI_Native_Blocks is
   use type ACPI_Requests.Authority;
   use type ACPI_Requests.Phase;
   use type Interfaces.Unsigned_64;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_8;
   use type System.Address;
   function Pending (Adapter : State) return Boolean is (Adapter.Held);
   procedure Retry_Return (Adapter : in out State) is
      Returned : Boolean;
   begin
      if not Adapter.Held then return; end if;
      CuBit.Memory_Grants.Return_Acquisition (Adapter.Reference, Returned);
      if Returned then Adapter.Held := False; end if;
   end Retry_Return;
   procedure Import_Grant
     (Adapter : in out State; Server : in out ACPI_Requests.State;
      Config : ACPI_Endpoint.Configuration; Stamp : Interfaces.Unsigned_64;
      Provider_Slot : CuBit.Messages.CapabilitySlot;
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Token : Interfaces.Unsigned_64; ID : Positive;
      Kind : ACPI_Service.Table_Kind; Length : Natural;
      Reply : out ACPI_Requests.Packet; Status : out Import_Status) is
      Origin : constant ACPI_Requests.Authority := ACPI_Endpoint.Classify (Config, Stamp);
      Address : System.Address;
      Acquired : Boolean;
      Result : ACPI_Requests.Response :=
        (Status => ACPI_Requests.Denied, Data => [others => 0]);
   begin
      Status := Rejected;
      if Origin /= ACPI_Requests.No_Authority then
         Result.Data (0) := ACPI_Requests.Revision (Server);
      end if;
      Reply := ACPI_Endpoint.Encode (Result);
      -- Reject unauthorized input before touching a grant or pending loan.
      if Origin /= ACPI_Requests.Snapshot_Provider then return; end if;
      Result.Status := ACPI_Requests.Resource_Limit;
      Reply := ACPI_Endpoint.Encode (Result);
      if Adapter.Held then return; end if;
      -- Validate metadata before acquiring any mapping. The service loop must
      -- serialize this check through Dispatch_Block; the core rechecks it too.
      Result.Status := ACPI_Requests.Stale;
      Reply := ACPI_Endpoint.Encode (Result);
      if Token /= ACPI_Requests.Revision (Server) then return; end if;
      Result.Status := ACPI_Requests.Resource_Limit;
      Reply := ACPI_Endpoint.Encode (Result);
      if Token = ACPI_Requests.Max_Revision then return; end if;
      Result.Status := ACPI_Requests.Malformed;
      Reply := ACPI_Endpoint.Encode (Result);
      if Length < Firmware_Tables.Table_Header_Size or else
        Length > Server.Table_Byte_Limit
      then return; end if;
      Result.Status := ACPI_Requests.Wrong_Order;
      Reply := ACPI_Endpoint.Encode (Result);
      if ACPI_Requests.Current (Server) /= ACPI_Requests.Receiving or else
        ACPI_Requests.Table_Open (Server)
      then return; end if;
      -- Preserve a defined error for a broken successful/null mapping result.
      Result.Status := ACPI_Requests.Malformed;
      Reply := ACPI_Endpoint.Encode (Result);
      CuBit.Memory_Grants.Acquire_Via_Capability
        (Provider_Slot, Reference, 0, Interfaces.Unsigned_64 (Length),
         CuBit.Memory_Grants.Read_Access, Address, Acquired);
      if not Acquired then Status := Acquisition_Failed; return; end if;
      Adapter.Reference := Reference;
      Adapter.Held := True;
      -- A success with a null address violates the runtime boundary contract;
      -- still return its acquired loan without dereferencing that address.
      if Address /= System.Null_Address then
         declare
            Data : Firmware_Tables.Bytes (1 .. Length)
              with Import, Address => Address;
         begin
            ACPI_Endpoint.Dispatch_Block
              (Server, Config, Stamp, Token, ID, Kind, Data, Reply);
         end;
      end if;
      Retry_Return (Adapter);
      Status := (if Adapter.Held then Cleanup_Pending else Processed);
   end Import_Grant;
   procedure Dispatch
     (Adapter : in out State; Server : in out ACPI_Requests.State;
      Config : ACPI_Endpoint.Configuration; Stamp : Interfaces.Unsigned_64;
      Provider_Slot : CuBit.Messages.CapabilitySlot;
      Request : ACPI_Requests.Packet; Reply : out ACPI_Requests.Packet) is
      Base : constant Interfaces.Unsigned_64 := 2 ** 32;
      Origin : constant ACPI_Requests.Authority := ACPI_Endpoint.Classify (Config, Stamp);
      Result : ACPI_Requests.Response := (ACPI_Requests.Denied, [others => 0]);
      Status : Import_Status;
   begin
      if Request.Label /= Import_Table_Grant then
         ACPI_Endpoint.Dispatch (Server, Config, Stamp, Request, Reply);
         return;
      end if;
      Reply := ACPI_Endpoint.Encode (Result);
      if Origin /= ACPI_Requests.Snapshot_Provider then return; end if;
      Result := (ACPI_Requests.Malformed,
        [ACPI_Requests.Revision (Server), 0, 0, 0]);
      Reply := ACPI_Endpoint.Encode (Result);
      if Request.Length /= 4 or else Request.Flags /= 0 or else Request.Reserved /= 0
        or else Request.Data (0) > ACPI_Requests.Max_Revision
        or else Request.Data (1) mod Base > CuBit.Memory_Grants.MAXIMUM_GLOBAL_SLOT
        or else Request.Data (1) / Base = 0
        or else Request.Data (2) not in 1 .. Interfaces.Unsigned_64 (Positive'Last)
        or else Request.Data (3) / Base > ACPI_Service.Table_Kind'Pos (ACPI_Service.Description)
        or else Request.Data (3) mod Base < Firmware_Tables.Table_Header_Size
        or else Request.Data (3) mod Base > Interfaces.Unsigned_64 (Server.Table_Byte_Limit)
      then return; end if;
      Import_Grant (Adapter, Server, Config, Stamp, Provider_Slot,
        (slot => Request.Data (1) mod Base, generation => Request.Data (1) / Base),
        Request.Data (0), Positive (Request.Data (2)),
        ACPI_Service.Table_Kind'Val (Request.Data (3) / Base),
        Natural (Request.Data (3) mod Base), Reply, Status);
      if Status = Acquisition_Failed then
         Result.Status := ACPI_Requests.Denied;
         Reply := ACPI_Endpoint.Encode (Result);
      end if;
      -- Cleanup_Pending already imported exactly once. Return its result and
      -- retain Adapter: only Retry_Return may retry the outstanding cleanup.
   end Dispatch;
end ACPI_Native_Blocks;
