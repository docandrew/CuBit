pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Service;
with ACPI_Requests; use ACPI_Requests;
with ACPI_Endpoint; use ACPI_Endpoint;
procedure Endpoint_Tests is
   Checks : Natural := 0;

   Config : constant Configuration := (Observer_Tag => 17, Provider_Tag => 31);
   Server : State (ACPI_Service.Max_Tables, ACPI_Service.Max_Total_Bytes, ACPI_Service.Max_Table_Bytes, 0);
   Empty_Server : State (ACPI_Service.Max_Tables, ACPI_Service.Max_Total_Bytes, ACPI_Service.Max_Table_Bytes, 0);
   Reply : Packet;
   Request : Packet;
   procedure Send (Stamp : Unsigned_64; Expected : Outcome) is
   begin
      Dispatch (Server, Config, Stamp, Request, Reply);
      Checks := Checks + 1; pragma Assert (Reply.Length = 4 and Reply.Flags = 0 and Reply.Reserved = 0);
      Checks := Checks + 1; pragma Assert (Reply.Label = (if Expected = OK then Reply_OK else Reply_Error));
      if Expected /= OK then Checks := Checks + 1; pragma Assert (Reply.Data (0) = Unsigned_64 (Outcome'Pos (Expected))); end if;
   end Send;
begin
   for Stamp in Unsigned_64 range 0 .. 65535 loop
      Checks := Checks + 1; pragma Assert (Classify (Config, Stamp) =
        (if Stamp = 17 then Observer elsif Stamp = 31 then Snapshot_Provider else No_Authority));
   end loop;
   -- Invalid configurations must not accidentally give zero/default stamps
   -- access, or collapse observation and upload into one authority.
   declare
      type Configurations is array (Positive range <>) of Configuration;
      type Stamps is array (Positive range <>) of Unsigned_64;
      Test_Stamps : constant Stamps := [0, 17, 31, Unsigned_64'Last];
      Bad_Configs : constant Configurations := [(0, 0), (17, 0), (0, 31), (17, 17),
                                                (Unsigned_64'Last, Unsigned_64'Last)];
   begin
      for Bad of Bad_Configs loop
         Checks := Checks + 1; pragma Assert (not Valid (Bad));
         for Stamp of Test_Stamps loop
            Checks := Checks + 1; pragma Assert (Classify (Bad, Stamp) = No_Authority);
            Dispatch (Server, Bad, Stamp, Request, Reply);
            Checks := Checks + 1; pragma Assert (Reply.Label = Reply_Error and
                   Reply.Data = [Unsigned_64 (Outcome'Pos (Denied)), 0, 0, 0]);
            Checks := Checks + 1; pragma Assert (Model (Server) = Model (Empty_Server));
         end loop;
      end loop;
   end;
   Checks := Checks + 1; pragma Assert (Classify ((Unsigned_64'Last - 1, Unsigned_64'Last), Unsigned_64'Last) = Snapshot_Provider);
   Checks := Checks + 1; pragma Assert (Classify ((Unsigned_64'Last - 1, Unsigned_64'Last), Unsigned_64'Last - 1) = Observer);
   Checks := Checks + 1; pragma Assert (Classify (Config, Unsigned_64'Last) = No_Authority);
   for Status in Outcome loop
      declare
         Result : constant Response := (Status, [5, 7, 11, 13]);
         Encoded : constant Packet := Encode (Result);
      begin
         Checks := Checks + 1; pragma Assert (Encoded.Length = 4 and Encoded.Flags = 0 and Encoded.Reserved = 0);
         Checks := Checks + 1; pragma Assert (Encoded.Label = (if Status = OK then Reply_OK else Reply_Error));
         Checks := Checks + 1; pragma Assert (Encoded.Data = (if Status = OK then [5, 7, 11, 13]
                                else [Unsigned_64 (Outcome'Pos (Status)), 5, 7, 0]));
      end;
   end loop;
   Request := (Label => Start_Snapshot, Data => [0, 1, 0, 0], others => <>);
   Send (Config.Observer_Tag, Denied);
   Checks := Checks + 1; pragma Assert (Current (Server) = Idle and Revision (Server) = 0);
   Send (Config.Provider_Tag, OK);
   Checks := Checks + 1; pragma Assert (Current (Server) = Receiving and Reply.Data = [1, 0, 0, 0]);
   Send (Config.Provider_Tag, Stale);
   Checks := Checks + 1; pragma Assert (Reply.Data = [Unsigned_64 (Outcome'Pos (Stale)), 1, 0, 0]);
   declare
      Before : constant State_Model (Server.Table_Capacity, Server.Byte_Capacity, Server.Table_Byte_Limit) := Model (Server) with Ghost;
   begin
      -- Forging the provider's tag in any wire word grants nothing.
      for I in Request.Data'Range loop
         Request := (Label => Start_Snapshot, Data => [1, 1, 0, 0], others => <>);
         Request.Data (I) := Config.Provider_Tag;
         Send (0, Denied);
         Checks := Checks + 1; pragma Assert (Reply.Data = [Unsigned_64 (Outcome'Pos (Denied)), 0, 0, 0]);
         Checks := Checks + 1; pragma Assert (Model (Server) = Before);
      end loop;
      for Reserved in Unsigned_16 loop
         Request := (Reserved => Reserved, others => <>);
         Send (0, Denied);
         Checks := Checks + 1; pragma Assert (Model (Server) = Before);
         if Reserved /= 0 then
            Send (Config.Observer_Tag, Malformed);
            Checks := Checks + 1; pragma Assert (Model (Server) = Before);
         end if;
      end loop;
      for Page in Unsigned_64 range 0 .. 6 loop
         Request := (Label => Read_Metrics, Data => [Page, 0, 0, 0], others => <>);
         Send (Config.Observer_Tag, OK);
         Checks := Checks + 1; pragma Assert (Reply.Data (0) = 1 and Model (Server) = Before);
      end loop;
   end;
   Request := (Label => Finish_Snapshot, Data => [1, 0, 0, 0], others => <>);
   Send (Config.Provider_Tag, Incomplete);
   Checks := Checks + 1; pragma Assert (Reply.Data = [Unsigned_64 (Outcome'Pos (Incomplete)), 2, 0, 0]);
   Checks := Checks + 1; pragma Assert (Current (Server) = Failed);
   Ada.Text_IO.Put_Line ("ACPI-ENDPOINT-CHECK: PASS" & Checks'Image);
end Endpoint_Tests;
