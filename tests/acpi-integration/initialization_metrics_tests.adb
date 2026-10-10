with ACPI_Requests; use ACPI_Requests;
with ACPI_Service;
with Firmware_Tables; use Firmware_Tables;
with Interfaces; use Interfaces;
with Ada.Text_IO;
procedure Initialization_Metrics_Tests is
   Checks : Natural := 0;
   procedure Check (Value : Boolean) is
   begin
      Checks := Checks + 1;
      if not Value then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Header (Data : in out Bytes; Sig : Signature) is
      Sum : Byte := 0;
   begin
      for I in 1 .. 4 loop Data (I) := Character'Pos (Sig (I)); end loop;
      Data (5) := Byte (Data'Length); Data (9) := 2; Data (10) := 0;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum;
   end Header;
   -- Name(PKG0, Package(){FUTR,MISS,MTHD}); Method(MTHD){Return(Zero)}
   DSDT_Data : Bytes (1 .. 65) := [others => 0];
   -- Name(FUTR,42), intentionally in the following SSDT.
   SSDT_Data : Bytes (1 .. 43) := [others => 0];
begin
   DSDT_Data (37 .. 65) :=
     [16#08#, 80, 75, 71, 48, 16#12#, 14, 3,
      70, 85, 84, 82, 77, 73, 83, 83, 77, 84, 72, 68,
      16#14#, 8, 77, 84, 72, 68, 0, 16#A4#, 0];
   SSDT_Data (37 .. 43) := [16#08#, 70, 85, 84, 82, 16#0A#, 42];
   Header (DSDT_Data, "DSDT"); Header (SSDT_Data, "SSDT");
   for Premature in Boolean loop
      declare
         Server : State (2, 256, 128, 11);
         Reply : Response;
         Before : State_Model (2, 256, 128) with Ghost;
         procedure Metrics (Page : Unsigned_64; Expected : Response_Words;
                            Status : Outcome := OK) is
            Prior : constant Revision_Number := Revision (Server);
         begin
            Before := Model (Server);
            for Origin in Authority loop
               Handle (Server, Origin,
                 (Label => Read_Metrics, Data => [Page, 0, 0, 0], others => <>), Reply);
               Check (Reply.Status = (if Origin = No_Authority then Denied else Status));
               Check (Reply.Data (0) = (if Origin = No_Authority then 0 else Prior));
               if Origin = No_Authority then Check (Reply.Data = [0, 0, 0, 0]);
               elsif Status = OK then Check (Reply.Data (1 .. 3) = Expected (1 .. 3));
               else Check (Reply.Data (1 .. 3) = [0, 0, 0]); end if;
               Check (Revision (Server) = Prior);
               pragma Assert (ACPI_Requests."=" (Model (Server), Before));
            end loop;
         end Metrics;
         procedure Finish (Status : Outcome) is
         begin
            Handle (Server, Snapshot_Provider,
              (Label => Finish_Snapshot, Data => [Revision (Server), 0, 0, 0], others => <>), Reply);
            Check (Reply.Status = Status);
         end Finish;
      begin
         Metrics (7, [0, 0, 0, 0]); Metrics (8, [0, 0, 0, 0]);
         Handle (Server, Snapshot_Provider,
           (Label => Start_Snapshot, Data => [11, 2, 0, 0], others => <>), Reply);
         Check (Reply.Status = OK);
         Import_Block (Server, Snapshot_Provider, Revision (Server), 10, ACPI_Service.DSDT, DSDT_Data, Reply);
         Check (Reply.Status = OK);
         Metrics (7, [0, 0, 0, 0]); Metrics (8, [0, 0, 3, 0]);
         if Premature then
            Finish (Incomplete);
            Check (Current (Server) = Failed);
            Metrics (7, [0, 0, 0, 0]); Metrics (8, [0, 0, 3, 0]);
         else
            Import_Block (Server, Snapshot_Provider, Revision (Server), 20, ACPI_Service.SSDT, SSDT_Data, Reply);
            Check (Reply.Status = OK);
            Metrics (7, [0, 0, 0, 0]); Metrics (8, [0, 0, 3, 0]);
            Before := Model (Server);
            for Origin in No_Authority .. Observer loop
               Handle (Server, Origin,
                 (Label => Finish_Snapshot, Data => [Revision (Server), 0, 0, 0], others => <>), Reply);
               Check (Reply.Status = Denied);
               pragma Assert (ACPI_Requests."=" (Model (Server), Before));
            end loop;
            Handle (Server, Snapshot_Provider,
              (Label => Finish_Snapshot, Data => [11, 0, 0, 0], others => <>), Reply);
            Check (Reply.Status = Stale);
            pragma Assert (ACPI_Requests."=" (Model (Server), Before));
            Finish (OK); Check (Current (Server) = Complete);
            Metrics (7, [0, 1, 1, 1]); Metrics (8, [0, 1, 0, 0]);
            Before := Model (Server); Finish (Wrong_Order);
            pragma Assert (ACPI_Requests."=" (Model (Server), Before));
            Metrics (7, [0, 1, 1, 1]); Metrics (8, [0, 1, 0, 0]);
         end if;
         Metrics (9, [0, 0, 0, 0], Malformed);
         Metrics (Unsigned_64'Last, [0, 0, 0, 0], Malformed);
         Before := Model (Server);
         for Page in Unsigned_64 range 7 .. 8 loop
            for Word in 1 .. 3 loop
               declare
                  Request : Packet := (Label => Read_Metrics, Data => [Page, 0, 0, 0], others => <>);
               begin
                  Request.Data (Word) := 1;
                  Handle (Server, Observer, Request, Reply);
                  Check (Reply.Status = Malformed and Reply.Data (1 .. 3) = [0, 0, 0]);
                  pragma Assert (ACPI_Requests."=" (Model (Server), Before));
               end;
            end loop;
         end loop;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Initialization metrics PASS" & Checks'Image);
end Initialization_Metrics_Tests;
