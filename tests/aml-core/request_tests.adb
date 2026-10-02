with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Requests; use ACPI_Requests;
with ACPI_Service;
with Firmware_Tables;
procedure Request_Tests is
   Checks : Natural := 0;
   Server, Before : State := ACPI_Requests.Fresh;
   Reply : Response;
   function Table (Length : Positive := 42; SSDT : Boolean := False) return Firmware_Tables.Bytes is
      Data : Firmware_Tables.Bytes (1 .. Length) := [others => 0];
      Signature : constant String := (if SSDT then "SSDT" else "DSDT");
      Sum : Firmware_Tables.Byte := 0;
   begin
      for I in 1 .. 4 loop
         Data (I) := Character'Pos (Signature (I));
         Data (I + 4) := Firmware_Tables.Byte (Shift_Right (Unsigned_64 (Length), (I - 1) * 8) and 255);
      end loop;
      Data (9) := 2;
      if Length = 42 then Data (37 .. 42) := [8, 86, 65, 76, 48, 1]; end if;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum;
      return Data;
   end Table;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Send (Label : Unsigned_32; A, B, C : Unsigned_64 := 0;
                   Expected : Outcome := OK; Origin : Authority := Snapshot_Provider) is
      Old_Revision : constant Unsigned_64 := Revision (Server);
   begin
      Before := Server;
      Handle (Server, Origin, (Label => Label, Data => [Old_Revision, A, B, C], others => <>), Reply);
      Check (Reply.Status = Expected and Reply.Data (0) = Revision (Server));
      if Expected in Denied | Malformed | Stale | Wrong_Order | Resource_Limit then
         Check (Server = Before);
      else
         Check (Revision (Server) = Old_Revision + 1);
      end if;
   end Send;
   procedure Metrics is
   begin
      Before := Server;
      Handle (Server, No_Authority, (Label => Read_Metrics, others => <>), Reply);
      Check (Reply.Status = Denied and Reply.Data = [0,0,0,0] and Server = Before);
      for Page in Unsigned_64 range 0 .. 6 loop
         Handle (Server, Observer, (Label => Read_Metrics, Data => [Page, 0, 0, 0], others => <>), Reply);
         Check (Reply.Status = OK and Reply.Data (0) = Revision (Server));
         Check (Server = Before);
      end loop;
   end Metrics;
   procedure Page (Index, A, B, C : Unsigned_64) is
   begin
      Before := Server;
      Handle (Server, Observer, (Label => Read_Metrics, Data => [Index,0,0,0], others => <>), Reply);
      Check (Reply.Status = OK and Reply.Data = [Revision (Server),A,B,C] and Server = Before);
   end Page;
   procedure Upload (Data : Firmware_Tables.Bytes; ID : Positive := 1; SSDT : Boolean := False;
                     Expected : Outcome := OK; Other_Table : Boolean := False) is
      Request : Packet := (Label => Write_Chunk, others => <>);
      Offset : Natural := 0;
      Amount : Natural;
   begin
      Send (Begin_Table, Unsigned_64 (ID), (if Other_Table then 2 else Boolean'Pos (SSDT)), Unsigned_64 (Data'Length));
      Send (Commit_Table, Expected => Wrong_Order);
      Send (Finish_Snapshot, Expected => Wrong_Order);
      Send (Write_Chunk, Unsigned_64'Last, Expected => Wrong_Order);
      while Offset < Data'Length loop
         Amount := Natural'Min (16, Data'Length - Offset);
         Request.Data := [Revision (Server), Unsigned_64 (Offset), 0, 0];
         for I in 0 .. Amount - 1 loop
            Request.Data (2 + I / 8) := Request.Data (2 + I / 8) or
              Shift_Left (Unsigned_64 (Data (Data'First + Offset + I)), (I mod 8) * 8);
         end loop;
         Before := Server;
         Handle (Server, Observer, Request, Reply);
         Check (Reply.Status = Denied and Server = Before);
         if Amount < 16 then
            declare
               Bad : Packet := Request;
            begin
               Bad.Data (3) := Bad.Data (3) or 16#FF00_0000_0000_0000#;
               Handle (Server, Snapshot_Provider, Bad, Reply);
               Check (Reply.Status = Malformed and Server = Before);
            end;
         end if;
         Handle (Server, Snapshot_Provider, Request, Reply);
         Check (Reply.Status = OK and Received (Server) = Offset + Amount);
         Before := Server;
         Handle (Server, Snapshot_Provider, Request, Reply);
         Check (Reply.Status = Stale and Server = Before);
         Offset := Offset + Amount;
      end loop;
      Send (Write_Chunk, Unsigned_64 (Offset), Expected => Wrong_Order);
      Send (Commit_Table, Expected => Expected);
      Check (not Table_Open (Server) and Received (Server) = 0);
   end Upload;
begin
   Server := Fresh;
   Metrics;
   Page (0,0,0,0);
   Page (1,0,0,0);
   Page (2,0,0,0);
   Page (3,0,0,0);
   Page (4,65_536,1_048_576,32);
   Page (5,0,65_536,65_536);
   Page (6,0,512,512);
   for Label in Unsigned_32 range 0 .. 7 loop
      Before := Server;
      Handle (Server, No_Authority, (Label => Label, others => <>), Reply);
      Check (Reply.Status = Denied and Reply.Data = [0,0,0,0] and Server = Before);
   end loop;
   Before := Server;
   Handle (Server, Observer, (Label => Read_Metrics, Data => [7,0,0,0], others => <>), Reply);
   Check (Reply.Status = Malformed and Server = Before);
   Handle (Server, Observer, (Label => Read_Metrics, Data => [Unsigned_64'Last,0,0,0], others => <>), Reply);
   Check (Reply.Status = Malformed and Server = Before);
   Handle (Server, Observer, (Label => Read_Metrics, Data => [0,0,0,1], others => <>), Reply);
   Check (Reply.Status = Malformed and Server = Before);
   for Label in Unsigned_32 range Start_Snapshot .. Finish_Snapshot loop
      Send (Label, Expected => Denied, Origin => Observer);
   end loop;
   for Count in Unsigned_8 loop
      if Count /= 4 then
         Before := Server;
         Handle (Server, Snapshot_Provider,
                 (Label => Start_Snapshot, Length => Count, Data => [0,1,0,0], others => <>), Reply);
         Check (Reply.Status = Malformed and Server = Before);
      end if;
   end loop;
   for Flag in Unsigned_8 range 1 .. Unsigned_8'Last loop
      Before := Server;
      Handle (Server, Snapshot_Provider,
              (Label => Start_Snapshot, Flags => Flag, Data => [0,1,0,0], others => <>), Reply);
      Check (Reply.Status = Malformed and Server = Before);
   end loop;
   for Reserved in Unsigned_16 range 1 .. Unsigned_16'Last loop
      Before := Server;
      Handle (Server, Snapshot_Provider,
              (Label => Start_Snapshot, Reserved => Reserved, Data => [0,1,0,0], others => <>), Reply);
      Check (Reply.Status = Malformed and Server = Before);
   end loop;
   Send (Start_Snapshot, 0, Expected => Malformed);
   Send (Start_Snapshot, 33, Expected => Malformed);
   Send (Start_Snapshot, Unsigned_64'Last, Expected => Malformed);
   Send (Start_Snapshot, 1, 1, Expected => Malformed);
   Send (Begin_Table, 1, 0, 42, Expected => Wrong_Order);
   for Count in 1 .. ACPI_Service.Max_Tables loop
      Server := Fresh;
      Send (Start_Snapshot, Unsigned_64 (Count));
      Send (Start_Snapshot, Unsigned_64 (Count), Expected => Wrong_Order);
      Metrics;
      Upload (Table);
      for ID in 2 .. Count loop Upload (Table (36, True), ID, True); end loop;
      Check (Observe (Server).Tables = Count and Observe (Server).Objects = 1);
      Page (0,1,Unsigned_64 (Count),Unsigned_64 (Count));
      Page (1,Unsigned_64 (42 + 36 * (Count - 1)),1,1);
      Page (2,0,0,0);
      Send (Finish_Snapshot);
      Check (Current (Server) = Complete);
      Page (0,2,Unsigned_64 (Count),Unsigned_64 (Count));
      Metrics;
      Send (Finish_Snapshot, Expected => Wrong_Order);
      Send (Begin_Table, 1, 0, 42, Expected => Wrong_Order);
   end loop;
   Server := Fresh;
   Send (Start_Snapshot, 2);
   Upload (Table);
   Send (Finish_Snapshot, Expected => Incomplete);
   Check (Current (Server) = Failed);
   Send (Begin_Table, 2, 1, 36, Expected => Wrong_Order);
   Server := Fresh;
   Send (Start_Snapshot, 1);
   declare
      Bad : Firmware_Tables.Bytes := Table;
   begin
      Bad (10) := Bad (10) + 1;
      Upload (Bad, Expected => Table_Rejected);
   end;
   Check (Current (Server) = Failed and Observe (Server).Tables = 0);
   Page (2,0,0,1);
   Metrics;
   Server := Fresh;
   Send (Start_Snapshot, 1);
   Send (Begin_Table, 0, 0, 42, Expected => Malformed);
   Send (Begin_Table, Unsigned_64'Last, 0, 42, Expected => Malformed);
   Send (Begin_Table, 1, 3, 42, Expected => Malformed);
   Send (Begin_Table, 1, 0, 35, Expected => Malformed);
   Send (Begin_Table, 1, 0, 65_537, Expected => Malformed);
   Send (Begin_Table, 1, 0, Unsigned_64 (Natural'Last), Expected => Malformed);
   Send (Begin_Table, 1, 0, Unsigned_64 (Natural'Last) + 1, Expected => Malformed);
   Send (Begin_Table, 1, 0, Unsigned_64'Last, Expected => Malformed);
   -- Transport maximum, with valid checksum but unsupported AML body.
   Upload (Table (ACPI_Service.Max_Table_Bytes), Expected => Table_Rejected);
   Check (Current (Server) = Failed and Observe (Server).Tables = 0);
   -- Observe real method-code admission through the same upload protocol.
   declare
      Method_Table : Firmware_Tables.Bytes := Table (45);
      Sum : Firmware_Tables.Byte := 0;
   begin
      Method_Table (37 .. 45) := [16#14#, 8, 77, 69, 84, 72, 0, 16#A4#, 1];
      Method_Table (10) := 0;
      for B of Method_Table loop Sum := Sum + B; end loop;
      Method_Table (10) := 0 - Sum;
      Server := Fresh;
      Send (Start_Snapshot, 1);
      Upload (Method_Table);
      Page (5,2,65_536,65_534);
      Page (6,1,512,511);
      Check (Observe (Server).Method_Bytes = 2);
      Send (Finish_Snapshot);
      Page (5,2,65_536,65_534);
   end;
   Server := Fresh (Max_Revision - 1);
   Send (Start_Snapshot, 1);
   Check (Revision (Server) = Max_Revision);
   Send (Begin_Table, 1, 0, 42, Expected => Resource_Limit);
   Metrics;

   declare
      Raw : Firmware_Tables.Bytes := Table (51);
      Sum : Firmware_Tables.Byte := 0;
      Low, High : Unsigned_64;
      Amount : Natural;
      procedure Query (Label : Unsigned_32; Index, Offset : Unsigned_64;
                       Expected : Outcome := OK; Origin : Authority := Observer) is
      begin
         Before := Server;
         Handle (Server, Origin,
           (Label => Label, Data => [Revision (Server), Index, Offset, 0], others => <>), Reply);
         Check (Reply.Status = Expected and Server = Before);
         Check (Reply.Data (0) = (if Origin = No_Authority then 0 else Revision (Server)));
      end Query;
   begin
      Server := Fresh;
      Query (Read_Table_Info, 1, 0, Wrong_Order);
      Send (Start_Snapshot, 2);
      Upload (Table (36));
      Query (Read_Table_Chunk, 1, 0, Wrong_Order);
      Raw (1 .. 4) := [16#44#, 16#4D#, 16#41#, 16#52#];
      Raw (37 .. Raw'Last) := [others => 255];
      Raw (10) := 0;
      for B of Raw loop Sum := Sum + B; end loop;
      Raw (10) := 0 - Sum;
      Upload (Raw, ID => 37, Other_Table => True);
      Query (Read_Table_Info, 2, 0, Wrong_Order);
      Send (Finish_Snapshot);
      Query (Read_Table_Info, 1, 0);
      Check (Reply.Data = [Revision (Server), 1, 36, 16#5444_5344#]);
      Query (Read_Table_Info, 2, 0);
      Check (Reply.Data = [Revision (Server), 37, 51, 16#5241_4D44#]);
      for Origin in Authority range Observer .. Snapshot_Provider loop
         for Offset in 0 .. Raw'Length loop
            Query (Read_Table_Chunk, 2, Unsigned_64 (Offset), Origin => Origin);
            Amount := Natural'Min (8, Raw'Length - Offset);
            Low := 0; High := 0;
            for I in 0 .. Amount - 1 loop
               if I < 4 then Low := Low + Unsigned_64 (Raw (Offset + I + 1)) * 256 ** I;
               else High := High + Unsigned_64 (Raw (Offset + I + 1)) * 256 ** (I - 4); end if;
            end loop;
            Check (Reply.Data = [Revision (Server), Unsigned_64 (Amount), Low, High]);
         end loop;
      end loop;
      Query (Read_Table_Chunk, 2, 52, Malformed);
      Query (Read_Table_Chunk, 2, Unsigned_64'Last, Malformed);
      Query (Read_Table_Info, 0, 0, Not_Found);
      Query (Read_Table_Info, 3, 0, Not_Found);
      Query (Read_Table_Chunk, Unsigned_64'Last, 0, Not_Found);
      Query (Read_Table_Info, 2, 1, Malformed);
      Query (Read_Table_Info, 2, 0, Denied, No_Authority);
      Check (Reply.Data = [0, 0, 0, 0]);
      Before := Server;
      Handle (Server, Observer,
        (Label => Read_Table_Chunk, Data => [Revision (Server) - 1, 2, 0, 0], others => <>), Reply);
      Check (Reply.Status = Stale and Server = Before and Reply.Data (1 .. 3) = [0, 0, 0]);
      Handle (Server, Observer,
        (Label => Read_Table_Chunk, Data => [Revision (Server), 2, 0, 1], others => <>), Reply);
      Check (Reply.Status = Malformed and Server = Before);
      Server := Fresh;
      Send (Start_Snapshot, 1);
      Upload (Table (36), Expected => Table_Rejected, Other_Table => True);
      Query (Read_Table_Info, 1, 0, Wrong_Order);
   end;
   -- Bulk import shares the same snapshot state and revision space. Exercise
   -- real table validation, source lifetime independence and chunk interlock.
   declare
      Raw : Firmware_Tables.Bytes (101 .. 142) := Table;
      Bad : Firmware_Tables.Bytes := Table;
      Empty : Firmware_Tables.Bytes (1 .. 0);
      Huge : constant Firmware_Tables.Bytes (1 .. ACPI_Service.Max_Table_Bytes + 1) := [others => 0];
      procedure Bulk (Data : Firmware_Tables.Bytes; Expected : Outcome := OK;
                      Origin : Authority := Snapshot_Provider;
                      Token : Unsigned_64 := Revision (Server);
                      ID : Positive := 1;
                      Kind : ACPI_Service.Table_Kind := ACPI_Service.DSDT) is
         Old_Revision : constant Unsigned_64 := Revision (Server);
      begin
         Before := Server;
         Import_Block (Server, Origin, Token, ID, Kind, Data, Reply);
         Check (Reply.Status = Expected);
         Check (Reply.Data (0) = (if Origin = No_Authority then 0 else Revision (Server)));
         if Expected in OK | Table_Rejected then
            Check (Revision (Server) = Old_Revision + 1 and not Table_Open (Server) and Received (Server) = 0);
         else
            Check (Server = Before);
         end if;
      end Bulk;
   begin
      Server := Fresh;
      Bulk (Raw, Wrong_Order);
      Bulk (Raw, Denied, No_Authority);
      Check (Reply.Data = [0, 0, 0, 0]);
      Bulk (Raw, Denied, Observer);
      Send (Start_Snapshot, 2);
      Bulk (Raw, Stale, Token => 0);
      Bulk (Raw, Stale, Token => Unsigned_64'Last);
      Bulk (Empty, Malformed);
      Bulk (Raw (101 .. 135), Malformed);
      Bulk (Huge, Malformed);
      Send (Begin_Table, 1, 0, 42);
      Bulk (Raw, Wrong_Order);
      Server := Fresh;
      Send (Start_Snapshot, 2);
      Bulk (Raw);
      Check (Observe (Server).Tables = 1 and Observe (Server).Bytes = 42);
      Raw := [others => 255]; -- accepted table must no longer borrow this data
      Upload (Table (36, SSDT => True), ID => 2, SSDT => True);
      Bulk (Table (36, SSDT => True), Wrong_Order, ID => 3, Kind => ACPI_Service.SSDT);
      Send (Finish_Snapshot);
      Bulk (Table, Wrong_Order);
      Handle (Server, Observer,
        (Label => Read_Table_Chunk, Data => [Revision (Server), 1, 36, 0], others => <>), Reply);
      Check (Reply.Status = OK and Reply.Data (1 .. 3) = [6, 16#4C41_5608#, 16#0130#]);
      -- A corrupt table poisons the advertised snapshot, as chunked commit does.
      Bad (10) := Bad (10) + 1;
      Server := Fresh;
      Send (Start_Snapshot, 1);
      Bulk (Bad, Table_Rejected);
      Check (Current (Server) = Failed and Observe (Server).Tables = 0);
      Bulk (Table, Wrong_Order);
      -- No implied signature/kind trust at the mapping boundary.
      Server := Fresh;
      Send (Start_Snapshot, 1);
      Bulk (Table, Table_Rejected, Kind => ACPI_Service.Description);
      Check (Current (Server) = Failed);
      -- Last token may admit one table, but can never wrap or finish afterward.
      Server := Fresh (Max_Revision - 2);
      Send (Start_Snapshot, 1);
      Bulk (Table);
      Check (Revision (Server) = Max_Revision);
      Bulk (Table, Resource_Limit);
      Send (Finish_Snapshot, Expected => Resource_Limit);
   end;
   -- Chunk fallback uses the instance's capacity too. A large declared extent
   -- may open, but a partial payload must never commit as a complete table.
   declare
      Length : constant := 1_048_577;
      Large : State := Fresh (0, 2, Length + 36, Length);
      Token : Revision_Number;
   begin
      Handle (Large, Snapshot_Provider,
        (Label => Start_Snapshot, Data => [0, 2, 0, 0], others => <>), Reply);
      Check (Reply.Status = OK);
      Token := Revision (Large);
      Handle (Large, Snapshot_Provider,
        (Label => Begin_Table, Data => [Token, 1, 0, Length + 1], others => <>), Reply);
      Check (Reply.Status = Malformed and Revision (Large) = Token and not Table_Open (Large));
      Handle (Large, Snapshot_Provider,
        (Label => Begin_Table, Data => [Token, 1, 0, Length], others => <>), Reply);
      Check (Reply.Status = OK and Table_Open (Large));
      Handle (Large, Snapshot_Provider,
        (Label => Write_Chunk, Data => [Revision (Large), 0, 0, 0], others => <>), Reply);
      Check (Reply.Status = OK and Received (Large) = 16);
      Token := Revision (Large);
      Handle (Large, Snapshot_Provider,
        (Label => Commit_Table, Data => [Token, 0, 0, 0], others => <>), Reply);
      Check (Reply.Status = Wrong_Order and Revision (Large) = Token
             and Table_Open (Large) and Observe (Large).Tables = 0);
      Handle (Large, Observer,
        (Label => Read_Metrics, Data => [4, 0, 0, 0], others => <>), Reply);
      Check (Reply.Status = OK and Reply.Data (1 .. 3) = [Length, Length + 36, 2]);
   end;
   Ada.Text_IO.Put_Line ("ACPI-REQUEST-CHECK: PASS" & Checks'Image);
end Request_Tests;
