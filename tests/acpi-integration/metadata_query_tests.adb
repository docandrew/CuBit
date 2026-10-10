with ACPI_Requests; use ACPI_Requests;
with ACPI_Endpoint;
with ACPI_Native_Blocks;
with CuBit.Memory_Grants;
with ACPI_Service;
with Firmware_Tables; use Firmware_Tables;
with Interfaces; use Interfaces;
with Ada.Text_IO;
procedure Metadata_Query_Tests is
   use type ACPI_Service.Install_Status;
   use type ACPI_Service.Metrics;
   Checks : Natural := 0;
   procedure Check (Value : Boolean) is
   begin
      Checks := Checks + 1;
      if not Value then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Header (Data : in out Bytes; Sig : Signature) is
      Sum : Byte := 0;
   begin
      for I in 1 .. 4 loop Data (Data'First + I - 1) := Character'Pos (Sig (I)); end loop;
      Data (Data'First + 4) := Byte (Data'Length);
      Data (Data'First + 8) := 2;
      Data (Data'First + 9) := 0;
      for B of Data loop Sum := Sum + B; end loop;
      Data (Data'First + 9) := 0 - Sum;
   end Header;
   DSDT_Data : Bytes (1 .. 36) := [others => 0];
   MCFG_Data : Bytes (101 .. 176) := [others => 0];
   SLIT_Data : Bytes (1 .. 48) := [others => 0];
   Bad_MCFG : Bytes (1 .. 45) := [others => 0];
   Bad_SLIT : Bytes (1 .. 45) := [others => 0];
   Empty_SLIT : Bytes (1 .. 44) := [others => 0];
   Config : constant ACPI_Endpoint.Configuration := (Observer_Tag => 10, Provider_Tag => 11);
begin
   Header (DSDT_Data, "DSDT");
   MCFG_Data (137 .. 144) := [others => 255];
   MCFG_Data (145 .. 152) := [others => 255];
   MCFG_Data (153 .. 154) := [16#34#, 16#12#];
   MCFG_Data (155 .. 156) := [17, 222];
   MCFG_Data (157 .. 160) := [others => 255];
   MCFG_Data (161) := 7; MCFG_Data (169) := 9;
   MCFG_Data (171 .. 172) := [3, 8];
   Header (MCFG_Data, "MCFG");
   SLIT_Data (37) := 2; SLIT_Data (45 .. 48) := [10, 23, 41, 10];
   Header (SLIT_Data, "SLIT");
   Header (Bad_MCFG, "MCFG"); Bad_SLIT (37) := 2; Header (Bad_SLIT, "SLIT");
   Header (Empty_SLIT, "SLIT");
   declare
      Core : ACPI_Service.State (6, 512, 128);
      Status : ACPI_Service.Install_Status;
      Prior : ACPI_Service.State_Model (6, 512) with Ghost;
      procedure Install (ID : Positive; Kind : ACPI_Service.Table_Kind; Data : Bytes) is
      begin
         ACPI_Service.Install (Core, ID, Kind, Data, Status);
         Check (Status = ACPI_Service.Installed);
      end Install;
   begin
      Install (101, ACPI_Service.DSDT, DSDT_Data);
      Install (203, ACPI_Service.Description, MCFG_Data);
      Install (305, ACPI_Service.Description, SLIT_Data);
      Prior := ACPI_Service.Model (Core);
      Check (ACPI_Service.MCFG_Info (Core, 2).Count = 2);
      Check (ACPI_Service.MCFG_Allocation (Core, 2, 2).Value.Base = 7);
      Check (ACPI_Service.SLIT_Info (Core, 3).Count = 2);
      Check (ACPI_Service.SLIT_Distance (Core, 3, 1, 0).Value = 41);
      Check (not ACPI_Service.MCFG_Info (Core, 3).Valid);
      Check (not ACPI_Service.SLIT_Info (Core, 2).Valid);
      pragma Assert (ACPI_Service."=" (ACPI_Service.Model (Core), Prior));
   end;
   declare
      Server : State (6, 512, 128, 19);
      Adapter : ACPI_Native_Blocks.State;
      Reply : Response;
      Wire : Packet;
      Before : State_Model (6, 512, 128) with Ghost;
      procedure Query (Label : Unsigned_32; Table_Index, A, B : Unsigned_64;
                       Expected : Outcome; Data : Response_Words := [others => 0]) is
         Prior : constant Revision_Number := Revision (Server);
         Stats : constant ACPI_Service.Metrics := Observe (Server);
      begin
         Before := Model (Server);
         for Origin in Authority loop
            Handle (Server, Origin, (Label => Label, Data => [Prior, Table_Index, A, B], others => <>), Reply);
            Check (Reply.Status = (if Origin = No_Authority then Denied else Expected));
            Check (Reply.Data (0) = (if Origin = No_Authority then 0 else Prior));
            if Origin = No_Authority then Check (Reply.Data = [0, 0, 0, 0]);
            elsif Expected = OK then Check (Reply.Data (1 .. 3) = Data (1 .. 3));
            else Check (Reply.Data (1 .. 3) = [0, 0, 0]); end if;
            Check (Revision (Server) = Prior and Observe (Server) = Stats);
            pragma Assert (ACPI_Requests."=" (Model (Server), Before));
         end loop;
         for Stamp in Unsigned_64 range 9 .. 11 loop
            ACPI_Endpoint.Dispatch (Server, Config, Stamp,
              (Label => Label, Data => [Prior, Table_Index, A, B], others => <>), Wire);
            if Stamp = 9 then
               Check (Wire.Label = ACPI_Endpoint.Reply_Error and Wire.Data = [Unsigned_64 (Outcome'Pos (Denied)), 0, 0, 0]);
            elsif Expected = OK then
               Check (Wire.Label = ACPI_Endpoint.Reply_OK and Wire.Data (1) = Data (1) and Wire.Data (2) = Data (2) and Wire.Data (3) = Data (3));
            else
               Check (Wire.Label = ACPI_Endpoint.Reply_Error and Wire.Data (0) = Unsigned_64 (Outcome'Pos (Expected)) and Wire.Data (1) = Prior);
            end if;
            pragma Assert (ACPI_Requests."=" (Model (Server), Before));
            declare
               Native_Reply : Packet;
               Acquisitions : constant Natural := CuBit.Memory_Grants.Acquisitions;
            begin
               ACPI_Native_Blocks.Dispatch (Adapter, Server, Config, Stamp, 5,
                 (Label => Label, Data => [Prior, Table_Index, A, B], others => <>), Native_Reply);
               Check (Native_Reply = Wire);
               Check (CuBit.Memory_Grants.Acquisitions = Acquisitions
                 and not ACPI_Native_Blocks.Pending (Adapter));
               pragma Assert (ACPI_Requests."=" (Model (Server), Before));
            end;
         end loop;
      end Query;
      procedure Import (ID : Positive; Kind : ACPI_Service.Table_Kind; Source : Bytes) is
         Copy : Bytes := Source;
      begin
         Import_Block (Server, Snapshot_Provider, Revision (Server), ID, Kind, Copy, Reply);
         Check (Reply.Status = OK);
         Copy := [others => 255];
         Check (Copy (Copy'First) = 255);
      end Import;
   begin
      Before := Model (Server);
      for Origin in Authority loop
         Handle (Server, Origin,
           (Label => ACPI_Native_Blocks.Import_Table_Grant,
            Data => [Revision (Server), Unsigned_64'Last, 0, 0], others => <>), Reply);
         Check (Reply.Status = (case Origin is
           when No_Authority | Observer => Denied, when Snapshot_Provider => Malformed));
         Check (Reply.Data (1 .. 3) = [0, 0, 0]);
         pragma Assert (ACPI_Requests."=" (Model (Server), Before));
      end loop;
      for Label in Read_MCFG_Info .. Read_SLIT_Distance loop Query (Label, 1, 0, 0, Wrong_Order); end loop;
      Handle (Server, Snapshot_Provider, (Label => Start_Snapshot, Data => [19, 6, 0, 0], others => <>), Reply);
      Check (Reply.Status = OK);
      Import (101, ACPI_Service.DSDT, DSDT_Data);
      Import (203, ACPI_Service.Description, MCFG_Data);
      Import (305, ACPI_Service.Description, SLIT_Data);
      Import (407, ACPI_Service.Description, Bad_MCFG);
      Import (509, ACPI_Service.Description, Bad_SLIT);
      Import (611, ACPI_Service.Description, Empty_SLIT);
      Query (Read_MCFG_Info, 2, 0, 0, Wrong_Order);
      Handle (Server, Snapshot_Provider, (Label => Finish_Snapshot, Data => [Revision (Server), 0, 0, 0], others => <>), Reply);
      Check (Reply.Status = OK);
      Before := Model (Server);
      Handle (Server, Snapshot_Provider,
        (Label => ACPI_Native_Blocks.Import_Table_Grant,
         Data => [Revision (Server), Unsigned_64'Last, 0, 0], others => <>), Reply);
      Check (Reply.Status = Malformed and Reply.Data (1 .. 3) = [0, 0, 0]);
      pragma Assert (ACPI_Requests."=" (Model (Server), Before));
      Query (Read_MCFG_Info, 2, 0, 0, OK, [0, 2, 2, 0]);
      Query (Read_MCFG_Info, 2, 1, 0, OK, [0, 16#FFFF_FFFF#, 16#FFFF_FFFF#, 0]);
      Query (Read_MCFG_Allocation, 2, 1, 0, OK, [0, 16#FFFF_FFFF#, 16#FFFF_FFFF#, 16#1234#]);
      Query (Read_MCFG_Allocation, 2, 1, 1, OK, [0, 17, 222, 16#FFFF_FFFF#]);
      Query (Read_MCFG_Allocation, 2, 2, 0, OK, [0, 7, 0, 9]);
      Query (Read_MCFG_Allocation, 2, 2, 1, OK, [0, 3, 8, 0]);
      Query (Read_SLIT_Info, 3, 0, 0, OK, [0, 2, 2, 0]);
      Query (Read_SLIT_Info, 6, 0, 0, OK, [0, 2, 0, 0]);
      for Row in 0 .. 1 loop
         for Col in 0 .. 1 loop
            Query (Read_SLIT_Distance, 3, Unsigned_64 (Row), Unsigned_64 (Col), OK,
              [0, Unsigned_64 (SLIT_Data (45 + Row * 2 + Col)), 0, 0]);
         end loop;
      end loop;
      Query (Read_SLIT_Distance, 6, 0, 0, Index_Out_Of_Range);
      for Label in Read_MCFG_Info .. Read_SLIT_Distance loop
         Query (Label, 0, 0, 0, Not_Found);
         Query (Label, 7, 0, 0, Not_Found);
         Query (Label, Unsigned_64'Last, 0, 0, Not_Found);
         Query (Label, 1, 0, 0, Wrong_Table_Kind);
         Query (Label, (if Label <= Read_MCFG_Allocation then 4 else 5), 0, 0, Table_Rejected);
         Before := Model (Server);
         Handle (Server, Observer, (Label => Label, Data => [0, 2, 0, 0], others => <>), Reply);
         Check (Reply.Status = Stale);
         for Field in 0 .. 2 loop
            Wire := (Label => Label, Data => [Revision (Server), 2, 0, 0], others => <>);
            case Field is
               when 0 => Wire.Length := 3;
               when 1 => Wire.Flags := 1;
               when others => Wire.Reserved := 1;
            end case;
            Handle (Server, Observer, Wire, Reply); Check (Reply.Status = Malformed);
         end loop;
         pragma Assert (ACPI_Requests."=" (Model (Server), Before));
      end loop;
      Query (Read_MCFG_Allocation, 2, 0, 0, Index_Out_Of_Range);
      Query (Read_MCFG_Allocation, 2, 3, 0, Index_Out_Of_Range);
      Query (Read_MCFG_Allocation, 2, Unsigned_64'Last, 0, Index_Out_Of_Range);
      Query (Read_SLIT_Distance, 3, 2, 0, Index_Out_Of_Range);
      Query (Read_SLIT_Distance, 3, 0, 2, Index_Out_Of_Range);
      Query (Read_SLIT_Distance, 3, Unsigned_64'Last, Unsigned_64'Last, Index_Out_Of_Range);
      Query (Read_MCFG_Info, 2, 2, 0, Malformed);
      Query (Read_MCFG_Info, 2, 0, 1, Malformed);
      Query (Read_MCFG_Allocation, 2, 1, 2, Malformed);
      Query (Read_SLIT_Info, 3, 1, 0, Malformed);
      Query (Read_SLIT_Info, 3, 0, 1, Malformed);
   end;
   Ada.Text_IO.Put_Line ("Metadata queries PASS" & Checks'Image);
end Metadata_Query_Tests;
