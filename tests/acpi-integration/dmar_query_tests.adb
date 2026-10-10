with ACPI_Requests; use ACPI_Requests;
with ACPI_Endpoint;
with ACPI_Native_Blocks;
with ACPI_Service;
with Firmware_Tables; use Firmware_Tables;
with CuBit.Memory_Grants;
with Interfaces; use Interfaces;
with Ada.Text_IO;
procedure DMAR_Query_Tests is
   Checks : Natural := 0;
   procedure Check (Value : Boolean) is
   begin
      Checks := Checks + 1;
      if not Value then raise Program_Error with Checks'Image; end if;
   end Check;
   DSDT_Data : Bytes (1 .. 36) := [others => 0];
   DMAR_Data : Bytes (1 .. 164) := [others => 0];
   Bad_DMAR : Bytes (1 .. 165) := [others => 0];
   Empty_DMAR : Bytes (1 .. 48) := [others => 0];
   type Positions is array (Positive range 1 .. 8) of Natural;
   Offsets : constant Positions := [48,80,104,112,132,144,152,160];
   Lengths : constant Positions := [32,24,8,20,12,8,8,4];
   procedure Put (Offset, Width : Natural; Value : Unsigned_64) is
      Rest : Unsigned_64 := Value;
   begin
      for I in 0 .. Width - 1 loop
         DMAR_Data (Offset + I + 1) := Byte (Rest and 255); Rest := Shift_Right (Rest, 8);
      end loop;
   end Put;
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
   Server : State (4, 1024, 256, 29);
   Adapter : ACPI_Native_Blocks.State;
   Config : constant ACPI_Endpoint.Configuration := (Observer_Tag => 10, Provider_Tag => 11);
   Reply : Response;
   Before : State_Model (4, 1024, 256) with Ghost;
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
   for I in Offsets'Range loop Put (Offsets (I), 2, (if I = 8 then 65535 else Unsigned_64 (I-1))); Put (Offsets (I)+2,2,Unsigned_64 (Lengths (I))); end loop;
   Put (36,1,47); Put (37,1,165);
   Put (52,1,129); Put (53,1,242); Put (54,2,4660); Put (56,8,16#FEDCBA9876543210#);
   Put (64,1,1); Put (65,1,10); Put (66,1,33); Put (67,1,44); Put (68,1,55); Put (69,1,66);
   Put (70,1,17); Put (71,1,3); Put (72,1,29); Put (73,1,7);
   Put (74,1,255); Put (75,1,6); Put (76,1,81); Put (77,1,82); Put (78,1,83); Put (79,1,84);
   Put (86,2,16#2345#); Put (88,8,16#8123456789ABCDEF#); Put (96,8,16#FEDCBA9876543210#);
   Put (108,1,147); Put (110,2,16#3456#);
   Put (120,8,16#FFEEDDCCBBAA9988#); Put (128,4,16#89ABCDEF#);
   Put (139,1,91); Put (140,1,65); Put (141,1,66); Put (142,1,67);
   Put (148,1,163); Put (150,2,16#4567#); Put (158,2,16#5678#);
   Seal (DMAR_Data, "DMAR"); Bad_DMAR (1 .. 164) := DMAR_Data; Seal (Bad_DMAR,"DMAR"); Seal (Empty_DMAR,"DMAR");
   Query (Read_DMAR_Info,1,0,0,Wrong_Order);
   Handle (Server, Snapshot_Provider, (Label => Start_Snapshot, Data => [29,4,0,0], others => <>), Reply); Check (Reply.Status = OK);
   Import (11,ACPI_Service.DSDT,DSDT_Data); Import (22,ACPI_Service.Description,DMAR_Data);
   Import (33,ACPI_Service.Description,Bad_DMAR); Import (44,ACPI_Service.Description,Empty_DMAR);
   Query (Read_DMAR_Info,2,0,0,Wrong_Order);
   Handle (Server,Snapshot_Provider,(Label => Finish_Snapshot, Data => [Revision (Server),0,0,0],others => <>),Reply); Check (Reply.Status = OK);
   Query (Read_DMAR_Info,2,0,0,OK,[0,6,47,165]); Query (Read_DMAR_Info,2,1,0,OK,[0,8,0,0]);
   Query (Read_DMAR_Info,4,1,0,OK,[0,0,0,0]);
   for I in Offsets'Range loop
      Query (Read_DMAR_Record,2,Unsigned_64 (I),0,OK,[0,(if I=8 then 65535 else Unsigned_64 (I-1)),Unsigned_64 (Offsets (I)),Unsigned_64 (Lengths (I))]);
      Query (Read_DMAR_Record,2,Unsigned_64 (I),1,OK,[0,(if I=1 then 2 else 0),0,0]);
   end loop;
   Query (Read_DMAR_Fields,2,1,0,OK,[0,129,242,4660]);
   Query (Read_DMAR_Fields,2,1,1,OK,[0,16#76543210#,16#FEDCBA98#,0]);
   Query (Read_DMAR_Fields,2,2,0,OK,[0,16#2345#,0,0]);
   Query (Read_DMAR_Fields,2,2,1,OK,[0,16#89ABCDEF#,16#81234567#,0]);
   Query (Read_DMAR_Fields,2,2,2,OK,[0,16#76543210#,16#FEDCBA98#,0]);
   Query (Read_DMAR_Fields,2,3,0,OK,[0,147,16#3456#,0]);
   Query (Read_DMAR_Fields,2,4,0,OK,[0,16#89ABCDEF#,0,0]);
   Query (Read_DMAR_Fields,2,4,1,OK,[0,16#BBAA9988#,16#FFEEDDCC#,0]);
   Query (Read_DMAR_Fields,2,5,0,OK,[0,91,140,3]);
   Query (Read_DMAR_Fields,2,6,0,OK,[0,163,16#4567#,0]);
   Query (Read_DMAR_Fields,2,7,0,OK,[0,16#5678#,0,0]);
   for I in 1 .. 8 loop for Page in Unsigned_64 range 0 .. 3 loop
      if I=8 and Page<3 then Query (Read_DMAR_Fields,2,8,Page,Unsupported_Record_Kind);
      elsif Page=3 or else (Page=2 and I/=2) or else (Page=1 and I in 3|5|6|7) then
         Query (Read_DMAR_Fields,2,Unsigned_64(I),Page,Malformed);
      end if;
   end loop; end loop;
   Query (Read_DMAR_Scope,2,1,4,OK,[0,1,64,10]);
   Query (Read_DMAR_Scope,2,1,5,OK,[0,33,44,55]);
   Query (Read_DMAR_Scope,2,1,6,OK,[0,66,2,1]);
   Query (Read_DMAR_Scope,2,1,8,OK,[0,255,74,6]);
   Query (Read_DMAR_Scope,2,1,9,OK,[0,81,82,83]);
   Query (Read_DMAR_Scope,2,1,10,OK,[0,84,0,0]);
   Query (Read_DMAR_Path,2,1,129,OK,[0,17,3,0]); Query (Read_DMAR_Path,2,1,130,OK,[0,29,7,0]);
   Query (Read_DMAR_Path,2,1,131,Index_Out_Of_Range); Query (Read_DMAR_Path,2,1,257,Unsupported_Record_Kind);
   Query (Read_DMAR_Scope,2,8,4,Unsupported_Record_Kind); Query (Read_DMAR_Path,2,8,129,Unsupported_Record_Kind);
   Query (Read_DMAR_Scope,2,2,4,Index_Out_Of_Range);
   for Bad of Words'(0,127,128,Unsigned_64'Last) loop Query (Read_DMAR_Path,2,1,Bad,Malformed); end loop;
   for Bad of Words'(0,3,7,Unsigned_64'Last) loop Query (Read_DMAR_Scope,2,1,Bad,Malformed); end loop;
   for Label in Read_DMAR_Info .. Read_DMAR_Path loop
      declare Selector : constant Unsigned_64 := (if Label=Read_DMAR_Scope then 4 elsif Label=Read_DMAR_Path then 129 else 0);
      begin
         Query (Label,1,0,Selector,Wrong_Table_Kind); Query (Label,3,0,Selector,Table_Rejected);
         Query (Label,0,0,Selector,Not_Found); Query (Label,Unsigned_64'Last,0,Selector,Not_Found);
         Handle (Server,Observer,(Label=>Label,Data=>[Revision(Server)-1,2,0,Selector],others=><>),Reply); Check(Reply.Status=Stale);
         if Label/=Read_DMAR_Info then Query(Label,2,0,Selector,Index_Out_Of_Range); Query(Label,2,Unsigned_64'Last,Selector,Index_Out_Of_Range); end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("DMAR queries PASS" & Checks'Image);
end DMAR_Query_Tests;
