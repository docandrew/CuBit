with ACPI_Requests; use ACPI_Requests;
with ACPI_Endpoint;
with ACPI_Native_Blocks;
with ACPI_Service;
with Firmware_Tables; use Firmware_Tables;
with CuBit.Memory_Grants;
with Interfaces; use Interfaces;
with Ada.Text_IO;
procedure MADT_Query_Tests is
   Checks : Natural := 0;
   procedure Check (Value : Boolean) is
   begin
      Checks := Checks + 1;
      if not Value then raise Program_Error with Checks'Image; end if;
   end Check;
   type Positions is array (Positive range 1 .. 9) of Natural;
   Codes : constant Positions := [0, 1, 2, 3, 4, 5, 9, 10, 255];
   Offsets : constant Positions := [44, 52, 64, 74, 82, 88, 100, 116, 128];
   Lengths : constant Positions := [8, 12, 10, 8, 6, 12, 16, 12, 2];
   Max_Word : constant Unsigned_64 := 16#FFFF_FFFF#;
   Max_Flags : constant Unsigned_64 := 16#FFFF#;
   DSDT_Data : Bytes (1 .. 36) := [others => 0];
   MADT_Data : Bytes (1 .. 130) := [others => 255];
   Bad_MADT : Bytes (1 .. 131) := [others => 255];
   Empty_MADT : Bytes (1 .. 44) := [others => 0];
   procedure Seal (Data : in out Bytes; Sig : Signature) is
      Sum : Byte := 0;
      Size : Natural := Data'Length;
   begin
      for I in 1 .. 4 loop Data (I) := Character'Pos (Sig (I)); end loop;
      for I in 5 .. 8 loop Data (I) := Byte (Size mod 256); Size := Size / 256; end loop;
      Data (9) := 6; Data (10) := 0;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum;
   end Seal;
   Server : State (4, 512, 160, 29);
   Adapter : ACPI_Native_Blocks.State;
   Config : constant ACPI_Endpoint.Configuration := (Observer_Tag => 10, Provider_Tag => 11);
   Reply : Response;
   Before : State_Model (4, 512, 160) with Ghost;
   procedure Query (Label : Unsigned_32; Table_Index, A, B : Unsigned_64;
                    Expected : Outcome; Data : Response_Words := [others => 0]) is
      Prior : constant Revision_Number := Revision (Server);
      Acquisitions : constant Natural := CuBit.Memory_Grants.Acquisitions;
      Returns : constant Natural := CuBit.Memory_Grants.Returns;
      Wire, Native_Reply : Packet;
   begin
      Before := Model (Server);
      for Origin in Authority loop
         Handle (Server, Origin, (Label => Label, Data => [Prior, Table_Index, A, B], others => <>), Reply);
         Check (Reply.Status = (if Origin = No_Authority then Denied else Expected));
         Check (Reply.Data (0) = (if Origin = No_Authority then 0 else Prior));
         if Origin = No_Authority then Check (Reply.Data = [0, 0, 0, 0]);
         elsif Expected = OK then Check (Reply.Data (1 .. 3) = Data (1 .. 3));
         else Check (Reply.Data (1 .. 3) = [0, 0, 0]); end if;
         Check (Revision (Server) = Prior);
         pragma Assert (ACPI_Requests."=" (Model (Server), Before));
      end loop;
      for Stamp in Unsigned_64 range 9 .. 11 loop
         ACPI_Endpoint.Dispatch (Server, Config, Stamp,
           (Label => Label, Data => [Prior, Table_Index, A, B], others => <>), Wire);
         ACPI_Native_Blocks.Dispatch (Adapter, Server, Config, Stamp, 5,
           (Label => Label, Data => [Prior, Table_Index, A, B], others => <>), Native_Reply);
         Check (Wire = Native_Reply);
         Check (CuBit.Memory_Grants.Acquisitions = Acquisitions and
           CuBit.Memory_Grants.Returns = Returns and not ACPI_Native_Blocks.Pending (Adapter));
         pragma Assert (ACPI_Requests."=" (Model (Server), Before));
      end loop;
      Handle (Server, Observer, (Label => Read_Metrics, Data => [7, 0, 0, 0], others => <>), Reply);
      Check (Reply.Status = OK and Reply.Data = [Prior, 0, 0, 0]);
      Handle (Server, Observer, (Label => Read_Metrics, Data => [8, 0, 0, 0], others => <>), Reply);
      Check (Reply.Status = OK and Reply.Data = [Prior, (if Current (Server) = Complete then 1 else 0), 0, 0]);
      pragma Assert (ACPI_Requests."=" (Model (Server), Before));
   end Query;
   procedure Import (ID : Positive; Kind : ACPI_Service.Table_Kind; Source : Bytes) is
      Copy : Bytes := Source;
   begin
      Import_Block (Server, Snapshot_Provider, Revision (Server), ID, Kind, Copy, Reply);
      Check (Reply.Status = OK);
      Copy := [others => 0]; Check (Copy (Copy'First) = 0);
   end Import;
begin
   Seal (DSDT_Data, "DSDT");
   for I in Codes'Range loop
      MADT_Data (Offsets (I) + 1) := Byte (Codes (I));
      MADT_Data (Offsets (I) + 2) := Byte (Lengths (I));
   end loop;
   Seal (MADT_Data, "APIC");
   Bad_MADT (1 .. 130) := MADT_Data; Seal (Bad_MADT, "APIC");
   Seal (Empty_MADT, "APIC");
   for Label in Read_MADT_Info .. Read_MADT_Fields loop Query (Label, 1, 0, 0, Wrong_Order); end loop;
   Handle (Server, Snapshot_Provider, (Label => Start_Snapshot, Data => [29, 4, 0, 0], others => <>), Reply);
   Check (Reply.Status = OK);
   Import (11, ACPI_Service.DSDT, DSDT_Data);
   Import (22, ACPI_Service.Description, MADT_Data);
   Import (33, ACPI_Service.Description, Bad_MADT);
   Import (44, ACPI_Service.Description, Empty_MADT);
   Query (Read_MADT_Info, 2, 0, 0, Wrong_Order);
   Handle (Server, Snapshot_Provider, (Label => Finish_Snapshot, Data => [Revision (Server), 0, 0, 0], others => <>), Reply);
   Check (Reply.Status = OK);
   Query (Read_MADT_Info, 2, 0, 0, OK, [0, 6, 9, Max_Word]);
   Query (Read_MADT_Info, 2, 1, 0, OK, [0, Max_Word, 0, 0]);
   Query (Read_MADT_Info, 4, 0, 0, OK, [0, 6, 0, 0]);
   for I in Codes'Range loop
      Query (Read_MADT_Record, 2, Unsigned_64 (I), 0, OK,
        [0, Unsigned_64 (Codes (I)), Unsigned_64 (Offsets (I)), Unsigned_64 (Lengths (I))]);
   end loop;
   Query (Read_MADT_Fields, 2, 1, 0, OK, [0, 255, 255, Max_Word]);
   Query (Read_MADT_Fields, 2, 2, 0, OK, [0, 255, Max_Word, Max_Word]);
   Query (Read_MADT_Fields, 2, 3, 0, OK, [0, 255, 255, Max_Word]);
   Query (Read_MADT_Fields, 2, 3, 1, OK, [0, Max_Flags, 0, 0]);
   Query (Read_MADT_Fields, 2, 4, 0, OK, [0, Max_Word, Max_Flags, 0]);
   Query (Read_MADT_Fields, 2, 5, 0, OK, [0, 255, 255, Max_Flags]);
   Query (Read_MADT_Fields, 2, 6, 0, OK, [0, Max_Word, Max_Word, 0]);
   Query (Read_MADT_Fields, 2, 7, 0, OK, [0, Max_Word, Max_Word, Max_Word]);
   Query (Read_MADT_Fields, 2, 8, 0, OK, [0, Max_Word, 255, Max_Flags]);
   Query (Read_MADT_Fields, 2, 9, 0, Unsupported_Record_Kind);
   Query (Read_MADT_Fields, 2, 9, 1, Unsupported_Record_Kind);
   for I in Codes'Range loop
      if I /= 3 and I /= 9 then Query (Read_MADT_Fields, 2, Unsigned_64 (I), 1, Malformed); end if;
   end loop;
   for Label in Read_MADT_Info .. Read_MADT_Fields loop
      Query (Label, 0, 0, 0, Not_Found); Query (Label, 5, 0, 0, Not_Found);
      Query (Label, Unsigned_64'Last, 0, 0, Not_Found);
      Query (Label, 1, 0, 0, Wrong_Table_Kind);
      Query (Label, 3, 0, 0, Table_Rejected);
      Before := Model (Server);
      Handle (Server, Observer, (Label => Label, Data => [0, 2, 0, 0], others => <>), Reply);
      Check (Reply.Status = Stale);
      for Field in 0 .. 2 loop
         declare Request : Packet := (Label => Label, Data => [Revision (Server), 2, 0, 0], others => <>); begin
            case Field is
               when 0 => Request.Length := 3;
               when 1 => Request.Flags := 1;
               when others => Request.Reserved := 1;
            end case;
            Handle (Server, Observer, Request, Reply); Check (Reply.Status = Malformed);
         end;
      end loop;
      pragma Assert (ACPI_Requests."=" (Model (Server), Before));
   end loop;
   for Label in Read_MADT_Record .. Read_MADT_Fields loop
      Query (Label, 2, 0, 0, Index_Out_Of_Range);
      Query (Label, 2, 10, 0, Index_Out_Of_Range);
      Query (Label, 2, Unsigned_64'Last, 0, Index_Out_Of_Range);
      Query (Label, 4, 1, 0, Index_Out_Of_Range);
   end loop;
   Query (Read_MADT_Info, 2, 2, 0, Malformed);
   Query (Read_MADT_Info, 2, 0, 1, Malformed);
   Query (Read_MADT_Record, 2, 1, 1, Malformed);
   Query (Read_MADT_Fields, 2, 1, 2, Malformed);
   Query (Read_MADT_Record, 3, 1, 0, Table_Rejected);
   -- Distinct fields catch serialization swaps hidden by all-ones maxima.
   declare
      Other : State (2, 256, 160, 0);
      Distinct : Bytes := MADT_Data;
      type Expected_Rows is array (Positive range 1 .. 8) of Response_Words;
      Expected : constant Expected_Rows :=
        [[0, 17, 34, Max_Word], [0, 35, 16#ABCDEF12#, 16#12345678#],
         [0, 37, 38, 16#78563412#], [0, 16#56781234#, 16#3412#, 0],
         [0, 39, 40, 16#2345#], [0, 16#76543210#, 16#FEDCBA98#, 0],
         [0, 16#12345678#, 16#23456789#, 16#3456789A#],
         [0, 16#456789AB#, 41, 16#4567#]];
      Other_Before : State_Model (2, 256, 160) with Ghost;
      procedure Put (Offset, Width : Natural; Value : Unsigned_64) is
         Rest : Unsigned_64 := Value;
      begin
         for I in 0 .. Width - 1 loop
            Distinct (Offset + I + 1) := Byte (Rest and 255);
            Rest := Shift_Right (Rest, 8);
         end loop;
      end Put;
   begin
      Put (46, 1, 17); Put (47, 1, 34);
      Put (54, 1, 35); Put (56, 4, 16#ABCDEF12#); Put (60, 4, 16#12345678#);
      Put (66, 1, 37); Put (67, 1, 38); Put (68, 4, 16#78563412#); Put (72, 2, 16#1234#);
      Put (76, 2, 16#3412#); Put (78, 4, 16#56781234#);
      Put (84, 1, 39); Put (85, 2, 16#2345#); Put (87, 1, 40);
      Put (92, 8, 16#FEDCBA9876543210#);
      Put (104, 4, 16#23456789#); Put (108, 4, 16#3456789A#); Put (112, 4, 16#12345678#);
      Put (118, 2, 16#4567#); Put (120, 4, 16#456789AB#); Put (124, 1, 41);
      Seal (Distinct, "APIC");
      Handle (Other, Snapshot_Provider, (Label => Start_Snapshot, Data => [0, 2, 0, 0], others => <>), Reply);
      Check (Reply.Status = OK);
      Import_Block (Other, Snapshot_Provider, Revision (Other), 1, ACPI_Service.DSDT, DSDT_Data, Reply);
      Check (Reply.Status = OK);
      Import_Block (Other, Snapshot_Provider, Revision (Other), 2, ACPI_Service.Description, Distinct, Reply);
      Check (Reply.Status = OK);
      Handle (Other, Snapshot_Provider, (Label => Finish_Snapshot, Data => [Revision (Other), 0, 0, 0], others => <>), Reply);
      Check (Reply.Status = OK);
      Other_Before := Model (Other);
      for I in Expected'Range loop
         Handle (Other, Observer, (Label => Read_MADT_Fields,
           Data => [Revision (Other), 2, Unsigned_64 (I), 0], others => <>), Reply);
         Check (Reply.Status = OK and Reply.Data (1 .. 3) = Expected (I) (1 .. 3));
         pragma Assert (ACPI_Requests."=" (Model (Other), Other_Before));
      end loop;
      Handle (Other, Observer, (Label => Read_MADT_Fields,
        Data => [Revision (Other), 2, 3, 1], others => <>), Reply);
      Check (Reply.Status = OK and Reply.Data (1 .. 3) = [16#1234#, 0, 0]);
      pragma Assert (ACPI_Requests."=" (Model (Other), Other_Before));
   end;
   Ada.Text_IO.Put_Line ("MADT queries PASS" & Checks'Image);
end MADT_Query_Tests;
