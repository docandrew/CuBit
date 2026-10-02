pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Requests; use ACPI_Requests;
with ACPI_Endpoint; use ACPI_Endpoint;
procedure Endpoint_Tests is
   Checks : Natural := 0;
   procedure Check (Value : Boolean) is
   begin
      Checks := Checks + 1;
      if not Value then raise Program_Error with "endpoint check" & Checks'Image; end if;
   end Check;
   Config : constant Configuration := (Observer_Tag => 17, Provider_Tag => 31);
   Server : State := Fresh;
   Reply : Packet;
   Request : Packet;
   procedure Send (Stamp : Unsigned_64; Expected : Outcome) is
   begin
      Dispatch (Server, Config, Stamp, Request, Reply);
      Check (Reply.Length = 4 and Reply.Flags = 0 and Reply.Reserved = 0);
      Check (Reply.Label = (if Expected = OK then Reply_OK else Reply_Error));
      if Expected /= OK then Check (Reply.Data (0) = Unsigned_64 (Outcome'Pos (Expected))); end if;
   end Send;
begin
   for Stamp in Unsigned_64 range 0 .. 65535 loop
      Check (Classify (Config, Stamp) =
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
         Check (not Valid (Bad));
         for Stamp of Test_Stamps loop
            Check (Classify (Bad, Stamp) = No_Authority);
            Dispatch (Server, Bad, Stamp, Request, Reply);
            Check (Reply.Label = Reply_Error and
                   Reply.Data = [Unsigned_64 (Outcome'Pos (Denied)), 0, 0, 0]);
            Check (Server = Fresh);
         end loop;
      end loop;
   end;
   Check (Classify ((Unsigned_64'Last - 1, Unsigned_64'Last), Unsigned_64'Last) = Snapshot_Provider);
   Check (Classify ((Unsigned_64'Last - 1, Unsigned_64'Last), Unsigned_64'Last - 1) = Observer);
   Check (Classify (Config, Unsigned_64'Last) = No_Authority);
   for Status in Outcome loop
      declare
         Result : constant Response := (Status, [5, 7, 11, 13]);
         Encoded : constant Packet := Encode (Result);
      begin
         Check (Encoded.Length = 4 and Encoded.Flags = 0 and Encoded.Reserved = 0);
         Check (Encoded.Label = (if Status = OK then Reply_OK else Reply_Error));
         Check (Encoded.Data = (if Status = OK then [5, 7, 11, 13]
                                else [Unsigned_64 (Outcome'Pos (Status)), 5, 7, 0]));
      end;
   end loop;
   Request := (Label => Start_Snapshot, Data => [0, 1, 0, 0], others => <>);
   Send (Config.Observer_Tag, Denied);
   Check (Current (Server) = Idle and Revision (Server) = 0);
   Send (Config.Provider_Tag, OK);
   Check (Current (Server) = Receiving and Reply.Data = [1, 0, 0, 0]);
   Send (Config.Provider_Tag, Stale);
   Check (Reply.Data = [Unsigned_64 (Outcome'Pos (Stale)), 1, 0, 0]);
   declare
      Before : constant State := Server;
   begin
      -- Forging the provider's tag in any wire word grants nothing.
      for I in Request.Data'Range loop
         Request := (Label => Start_Snapshot, Data => [1, 1, 0, 0], others => <>);
         Request.Data (I) := Config.Provider_Tag;
         Send (0, Denied);
         Check (Reply.Data = [Unsigned_64 (Outcome'Pos (Denied)), 0, 0, 0]);
         Check (Server = Before);
      end loop;
      for Reserved in Unsigned_16 loop
         Request := (Reserved => Reserved, others => <>);
         Send (0, Denied);
         Check (Server = Before);
         if Reserved /= 0 then
            Send (Config.Observer_Tag, Malformed);
            Check (Server = Before);
         end if;
      end loop;
      for Page in Unsigned_64 range 0 .. 6 loop
         Request := (Label => Read_Metrics, Data => [Page, 0, 0, 0], others => <>);
         Send (Config.Observer_Tag, OK);
         Check (Reply.Data (0) = 1 and Server = Before);
      end loop;
   end;
   Request := (Label => Finish_Snapshot, Data => [1, 0, 0, 0], others => <>);
   Send (Config.Provider_Tag, Incomplete);
   Check (Reply.Data = [Unsigned_64 (Outcome'Pos (Incomplete)), 2, 0, 0]);
   Check (Current (Server) = Failed);
   Ada.Text_IO.Put_Line ("ACPI-ENDPOINT-CHECK: PASS" & Checks'Image);
end Endpoint_Tests;
