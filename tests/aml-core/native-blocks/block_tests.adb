with Ada.Text_IO;
with Interfaces; use Interfaces;
with System;
with Firmware_Tables;
with ACPI_Service;
with ACPI_Requests; use ACPI_Requests;
with ACPI_Endpoint; use ACPI_Endpoint;
with ACPI_Native_Blocks; use ACPI_Native_Blocks;
with CuBit.Memory_Grants; use CuBit.Memory_Grants;
procedure Block_Tests is
   Server, Before : ACPI_Requests.State := ACPI_Requests.Fresh;
   Adapter : ACPI_Native_Blocks.State;
   Config : constant Configuration := (17, 31);
   Reply : Packet;
   Status : Import_Status;
   Raw : aliased Firmware_Tables.Bytes (1 .. 36) := [others => 0];
   Reference : constant Grant_Reference := (19, 27);
   type Stamps is array (Positive range <>) of Unsigned_64;
   Bad_Stamps : constant Stamps := [0, 17, 32, Unsigned_64'Last];
   Checks : Natural := 0;
   procedure Check (Value : Boolean) is
   begin
      Checks := Checks + 1;
      if not Value then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Start is
      Result : Response;
   begin
      Server := Fresh;
      Handle (Server, Snapshot_Provider,
        (Label => Start_Snapshot, Data => [0, 1, 0, 0], others => <>), Result);
      Check (Result.Status = OK);
   end Start;
   procedure Import (Stamp : Unsigned_64 := 31; Length : Natural := 36;
                     Token : Unsigned_64 := Revision (Server)) is
   begin
      Import_Grant (Adapter, Server, Config, Stamp, 7, Reference,
                    Token, 1, ACPI_Service.DSDT, Length, Reply, Status);
   end Import;
begin
   Raw (1 .. 4) := [16#44#, 16#53#, 16#44#, 16#54#];
   Raw (5) := 36; Raw (9) := 2;
   declare
      Sum : Firmware_Tables.Byte := 0;
   begin
      for B of Raw loop Sum := Sum + B; end loop;
      Raw (10) := 0 - Sum;
   end;
   Source := Raw'Address;
   Start;
   Before := Server;
   for Stamp of Bad_Stamps loop
      Import (Stamp);
      Check (Status = Rejected and Acquisitions = 0 and Returns = 0 and Server = Before);
      Check (Reply.Data (0) = Outcome'Pos (Denied));
   end loop;
   Import (Length => 0);
   Check (Status = Rejected and Acquisitions = 0 and Server = Before);
   Import (Length => ACPI_Service.Max_Table_Bytes + 1);
   Check (Status = Rejected and Acquisitions = 0 and Server = Before);
   Acquire_OK := False;
   Import;
   Check (Status = Acquisition_Failed and Acquisitions = 1 and Returns = 0);
   Check (not Pending (Adapter) and Server = Before);
   Acquire_OK := True;
   Import (Token => 0);
   Check (Status = Rejected and Server = Before and Returns = 0 and not Pending (Adapter)
          and Acquisitions = 1);
   Check (Reply.Data (0) = Outcome'Pos (Stale));
   Return_OK := False;
   Import;
   Check (Status = Cleanup_Pending and Pending (Adapter));
   Check (Reply.Label = Reply_OK and Observe (Server).Tables = 1);
   Check (Last_Slot = 7 and Last_Reference = Reference and Last_Offset = 0 and Last_Length = 36);
   Check (Last_Access = Read_Access and Returned_Reference = Reference);
   Before := Server;
   declare
      Old_Acquisitions : constant Natural := Acquisitions;
   begin
      Import;
      Check (Status = Rejected and Server = Before and Acquisitions = Old_Acquisitions);
      Check (Pending (Adapter) and Reply.Data (0) = Outcome'Pos (Resource_Limit));
      Retry_Return (Adapter);
      Check (Pending (Adapter) and Returned_Reference = Reference);
      Return_OK := True;
      Retry_Return (Adapter);
      Check (not Pending (Adapter) and Server = Before);
      declare
         Old_Returns : constant Natural := Returns;
      begin
         Retry_Return (Adapter);
         Check (Returns = Old_Returns);
      end;
   end;
   Start;
   Raw (10) := Raw (10) + 1;
   Import;
   Check (Status = Processed and not Pending (Adapter));
   Check (Current (Server) = Failed and Reply.Data (0) = Outcome'Pos (Table_Rejected));
   Start;
   Source := System.Null_Address;
   Before := Server;
   Import;
   Check (Status = Processed and not Pending (Adapter) and Server = Before);
   Check (Reply.Data (0) = Outcome'Pos (Malformed));
   -- Rejections must not touch grant state, even with a valid provider stamp.
   declare
      Old_Acquisitions : constant Natural := Acquisitions;
      Old_Returns : constant Natural := Returns;
      Result : Response;
      procedure Rejected_Without_Acquire (Expected : Outcome) is
      begin
         Before := Server;
         Import;
         Check (Status = Rejected and Server = Before and not Pending (Adapter));
         Check (Acquisitions = Old_Acquisitions and Returns = Old_Returns);
         Check (Reply.Label = Reply_Error and Reply.Data (0) = Outcome'Pos (Expected));
      end Rejected_Without_Acquire;
   begin
      Server := Fresh;
      Rejected_Without_Acquire (Wrong_Order);
      Server := Fresh (Max_Revision);
      Rejected_Without_Acquire (Resource_Limit);
      Start;
      Handle (Server, Snapshot_Provider,
        (Label => Begin_Table, Data => [Revision (Server), 1, 0, 36], others => <>), Result);
      Check (Result.Status = OK and Table_Open (Server));
      Rejected_Without_Acquire (Wrong_Order);
   end;
   declare
      Request : Packet;
      Old_Acquisitions : Natural;
      Base : constant Unsigned_64 := 2 ** 32;
      procedure Reset_Request is
      begin
         Request := (Label => Import_Table_Grant,
           Data => [Revision (Server), 27 * Base + 19, 1, 36], others => <>);
      end Reset_Request;
      procedure Reject_Wire (Stamp : Unsigned_64 := 31) is
      begin
         Before := Server; Old_Acquisitions := Acquisitions;
         Dispatch (Adapter, Server, Config, Stamp, 7, Request, Reply);
         Check (Server = Before and Acquisitions = Old_Acquisitions);
         Check (Reply.Label = Reply_Error);
      end Reject_Wire;
   begin
      Raw (10) := Raw (10) - 1; -- Restore the valid DSDT after rejection tests.
      Return_OK := True; Acquire_OK := True; Source := Raw'Address;
      Retry_Return (Adapter); Check (not Pending (Adapter));
      Start; Reset_Request;
      for Stamp of Bad_Stamps loop
         Reject_Wire (Stamp);
         Check (Reply.Data = [Outcome'Pos (Denied), 0, 0, 0]);
      end loop;
      for Field in 0 .. 11 loop
         Reset_Request;
         case Field is
            when 0 => Request.Length := 3;
            when 1 => Request.Flags := 1;
            when 2 => Request.Reserved := 1;
            when 3 => Request.Data (0) := Unsigned_64'Last;
            when 4 => Request.Data (1) := 19;
            when 5 => Request.Data (1) := 27 * Base + 4096;
            when 6 => Request.Data (2) := 0;
            when 7 => Request.Data (2) := Unsigned_64 (Positive'Last) + 1;
            when 8 => Request.Data (3) := 3 * Base + 36;
            when 9 => Request.Data (3) := 35;
            when 10 => Request.Data (3) := 65_537;
            when 11 => Request.Data (3) := Unsigned_64'Last;
         end case;
         Reject_Wire; Check (Reply.Data (0) = Outcome'Pos (Malformed));
      end loop;
      Reset_Request; Request.Data (0) := 0;
      Reject_Wire; Check (Reply.Data (0) = Outcome'Pos (Stale));
      Reset_Request; Acquire_OK := False;
      Before := Server;
      Dispatch (Adapter, Server, Config, 31, 7, Request, Reply);
      Check (Server = Before and Reply.Data (0) = Outcome'Pos (Denied));
      Acquire_OK := True; Return_OK := False;
      Dispatch (Adapter, Server, Config, 31, 7, Request, Reply);
      Check (Reply.Label = Reply_OK and Pending (Adapter));
      Check (Last_Reference = Reference and Last_Length = 36 and Last_Slot = 7);
      Before := Server; Old_Acquisitions := Acquisitions;
      Dispatch (Adapter, Server, Config, 31, 7, Request, Reply);
      Check (Server = Before and Acquisitions = Old_Acquisitions);
      Check (Reply.Label = Reply_Error); -- Never replay an imported block.
      Return_OK := True; Retry_Return (Adapter); Check (not Pending (Adapter));
      Start; Reset_Request;
      Request.Data (1) := Unsigned_64 (Unsigned_32'Last) * Base + 4095;
      Dispatch (Adapter, Server, Config, 31, 7, Request, Reply);
      Check (Reply.Label = Reply_OK and not Pending (Adapter));
      Check (Last_Reference.slot = 4095 and Last_Reference.generation = Unsigned_64 (Unsigned_32'Last));
      Request := (Label => Read_Metrics, others => <>);
      Dispatch (Adapter, Server, Config, 17, 7, Request, Reply);
      Check (Reply.Label = Reply_OK); -- Scalar operations share this dispatcher.
   end;
   -- Construction-time capacities travel through the actual grant adapter and
   -- request dispatcher. The mock provides only the mapped bytes; acquisition
   -- bounds, access rights, retention and return/retry behavior are production code.
   declare
      Length : constant := 1_048_577;
      Total : constant := Length + 34 * 36;
      Large : ACPI_Requests.State := Fresh (0, 35, Total, Length);
      Loan : ACPI_Native_Blocks.State;
      Base : constant Unsigned_64 := 2 ** 32;
      function Make_Table (Signature : String; Size : Positive) return Firmware_Tables.Bytes is
         B : Firmware_Tables.Bytes (1 .. Size) := [others => 16#A5#];
         Sum : Firmware_Tables.Byte := 0;
      begin
         for I in 1 .. 4 loop
            B (I) := Character'Pos (Signature (I));
            B (I + 4) := Firmware_Tables.Byte ((Size / 256 ** (I - 1)) mod 256);
         end loop;
         B (9) := 2; B (10) := 0;
         for V of B loop Sum := Sum + V; end loop;
         B (10) := 0 - Sum;
         return B;
      end Make_Table;
      Small : aliased Firmware_Tables.Bytes := Make_Table ("DSDT", 36);
      Big : aliased Firmware_Tables.Bytes := Make_Table ("TEST", Length);
      Request : Packet;
      Old_Acquisitions, Old_Returns : Natural;
      Old_Revision : Revision_Number;
   begin
      Acquire_OK := True; Return_OK := True;
      Dispatch (Loan, Large, Config, 17, 7,
        (Label => Read_Metrics, Data => [4, 0, 0, 0], others => <>), Reply);
      Check (Reply.Label = Reply_OK and Reply.Data (1 .. 3) = [Length, Total, 35]);
      Dispatch (Loan, Large, Config, 31, 7,
        (Label => Start_Snapshot, Data => [0, 35, 0, 0], others => <>), Reply);
      Check (Reply.Label = Reply_OK);
      Source := Small'Address;
      Import_Grant (Loan, Large, Config, 31, 7, Reference, Revision (Large),
        1, ACPI_Service.DSDT, 36, Reply, Status);
      Check (Status = Processed and Reply.Label = Reply_OK);
      Source := Big'Address;
      Request := (Label => Import_Table_Grant,
        Data => [Revision (Large), 27 * Base + 19, 2, 2 * Base + Length], others => <>);
      Old_Acquisitions := Acquisitions;
      Request.Data (3) := 2 * Base + Length + 1;
      Dispatch (Loan, Large, Config, 31, 7, Request, Reply);
      Check (Reply.Label = Reply_Error and Reply.Data (0) = Outcome'Pos (Malformed)
             and Acquisitions = Old_Acquisitions);
      Request.Data (3) := 2 * Base + Length;
      Dispatch (Loan, Large, Config, 17, 7, Request, Reply);
      Check (Reply.Data (0) = Outcome'Pos (Denied) and Acquisitions = Old_Acquisitions);
      Return_OK := False;
      Old_Returns := Returns;
      Dispatch (Loan, Large, Config, 31, 7, Request, Reply);
      Check (Reply.Label = Reply_OK and Pending (Loan));
      Check (Acquisitions = Old_Acquisitions + 1 and Returns = Old_Returns + 1);
      Check (Last_Length = Length and Last_Offset = 0 and Last_Access = Read_Access
             and Last_Slot = 7 and Last_Reference = Reference);
      Big := [others => 0];
      Old_Revision := Revision (Large);
      Dispatch (Loan, Large, Config, 31, 7, Request, Reply);
      Check (Reply.Data (0) = Outcome'Pos (Resource_Limit)
             and Acquisitions = Old_Acquisitions + 1 and Revision (Large) = Old_Revision);
      Return_OK := True;
      Retry_Return (Loan);
      Check (not Pending (Loan) and Returns = Old_Returns + 2);
      Small := Make_Table ("TEST", 36);
      Source := Small'Address;
      for I in 3 .. 35 loop
         Import_Grant (Loan, Large, Config, 31, 7, Reference, Revision (Large),
           I, ACPI_Service.Description, 36, Reply, Status);
         Check (Status = Processed and Reply.Label = Reply_OK);
      end loop;
      Dispatch (Loan, Large, Config, 31, 7,
        (Label => Finish_Snapshot, Data => [Revision (Large), 0, 0, 0], others => <>), Reply);
      Check (Reply.Label = Reply_OK and Current (Large) = Complete
             and Observe (Large).Tables = 35 and Observe (Large).Bytes = Total);
      Dispatch (Loan, Large, Config, 17, 7,
        (Label => Read_Table_Info, Data => [Revision (Large), 35, 0, 0], others => <>), Reply);
      Check (Reply.Label = Reply_OK and Reply.Data (1) = 35 and Reply.Data (2) = 36);
      Dispatch (Loan, Large, Config, 17, 7,
        (Label => Read_Table_Chunk, Data => [Revision (Large), 2, Length - 1, 0], others => <>), Reply);
      Check (Reply.Label = Reply_OK and Reply.Data (1 .. 3) = [1, 16#A5#, 0]);
      Source := Raw'Address;
   end;
   Ada.Text_IO.Put_Line ("ACPI-NATIVE-BLOCK-MOCK: PASS" & Checks'Image);
end Block_Tests;
