pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Requests; use ACPI_Requests;
with ACPI_Service;
with Migration_Tables;
with Firmware_Tables;
procedure Startup_Metrics_Tests is
   subtype Bytes is Firmware_Tables.Bytes;
   use type Firmware_Tables.Bytes;
   function Enc (Text : String) return Bytes is
      Result : Bytes (1 .. Text'Length);
   begin
      for I in Result'Range loop Result (I) := Character'Pos (Text (Text'First + I - 1)); end loop;
      return Result;
   end Enc;
   function Pkg_Length (Value : Natural) return Bytes is
   begin
      if Value < 64 then return [Unsigned_8 (Value)]; end if;
      if Value < 4_096 then return [Unsigned_8 (16#40# + Value mod 16), Unsigned_8 (Value / 16)]; end if;
      if Value < 1_048_576 then
         return [Unsigned_8 (16#80# + Value mod 16), Unsigned_8 ((Value / 16) mod 256), Unsigned_8 (Value / 4_096)];
      end if;
      raise Program_Error with "fixture package too large";
   end Pkg_Length;
   function Block (Opcode, Payload : Bytes) return Bytes is
      Width : Positive := 1;
   begin
      loop
         declare
            Length : constant Bytes := Pkg_Length (Payload'Length + Width);
         begin
            if Length'Length = Width then return Opcode & Length & Payload; end if;
            Width := Length'Length;
         end;
      end loop;
   end Block;
   function Method (Name : String; Code : Bytes) return Bytes is
     (Block ([16#14#], Enc (Name) & [0] & Code));

   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
   Good : constant Firmware_Tables.Bytes := Migration_Tables.Make ([8,86,65,76,48,1]);
   Bad : constant Firmware_Tables.Bytes := Migration_Tables.Make ([16#FE#]);
   procedure Query (S : in out State; Page : Unsigned_64; Expected : Response_Words) is
      Reply : Response;
      Prior : constant Revision_Number := Revision (S);
      Before : constant State_Model := Model (S) with Ghost;
   begin
      Handle (S, Observer, (Label => Read_Metrics, Data => [Page,0,0,0], others => <>), Reply);
      Check (Reply.Status = OK and then Reply.Data = Expected and then Revision (S) = Prior);
      pragma Assert (Model (S) = Before);
   end Query;
begin
   for Failed_Startup in Boolean loop
      declare
         S : State (1, 256, 256, Load_Capacity => 1, Initial_Revision => 0);
         Reply, Fuel : Response;
         Prior : Revision_Number;
      begin
         Query (S, Metrics_Device_State_Page, [0,0,2,0]);
         Query (S, Metrics_Startup_Fuel_Page, [0,0,0,0]);
         Query (S, Metrics_Device_Calls_Page, [0,0,0,0]);
         Query (S, Metrics_Device_Error_Page, [0,Unsigned_64 (Startup_Error_Code'Enum_Rep (Error_No_Return)),0,0]);
         for Page in Metrics_Device_State_Page .. Metrics_Last_Page loop
            Handle (S, No_Authority, (Label => Read_Metrics, Data => [Page,0,0,0], others => <>), Reply);
            Check (Reply.Status = Denied and then Reply.Data = [0,0,0,0]);
         end loop;
         Handle (S, Observer, (Label => Read_Metrics, Data => [Metrics_Last_Page+1,0,0,0], others => <>), Reply);
         Check (Reply.Status = Malformed);
         Handle (S, Observer, (Label => Read_Metrics, Data => [Metrics_Device_State_Page,1,0,0], others => <>), Reply);
         Check (Reply.Status = Malformed);
         Handle (S, Snapshot_Provider, (Label => Start_Snapshot, Data => [0,1,0,0], others => <>), Reply);
         Check (Reply.Status = OK);
         Import_Block (S, Snapshot_Provider, Revision (S), 1, ACPI_Service.DSDT,
           (if Failed_Startup then Bad else Good), Reply); Check (Reply.Status = OK);
         Prior := Revision (S);
         Handle (S, Observer, (Label => Finish_Snapshot, Data => [Prior,0,0,0], others => <>), Reply);
         Check (Reply.Status = Denied and then Revision (S) = Prior);
         Handle (S, Snapshot_Provider, (Label => Finish_Snapshot, Data => [Prior,0,0,0], others => <>), Reply);
         Check (Revision (S) = Prior+1);
         Query (S, Metrics_Device_State_Page,
           [Revision (S),
            (if Failed_Startup then Unsigned_64 (Device_Phase_Code'Enum_Rep (Device_Not_Started_Code))
             else Unsigned_64 (Device_Phase_Code'Enum_Rep (Device_Completed_Code))),
            (if Failed_Startup then Unsigned_64 (Region_Status_Code'Enum_Rep (Regions_Unavailable_Code))
             else Unsigned_64 (Region_Status_Code'Enum_Rep (No_Regions_Required_Code))),0]);
         Query (S, Metrics_Device_Calls_Page, [Revision (S),0,0,0]);
         Handle (S, Observer, (Label => Read_Metrics, Data => [Metrics_Startup_Fuel_Page,0,0,0], others => <>), Fuel);
         Check (Fuel.Status = OK and then Fuel.Data (1) = Fuel.Data (2)+Fuel.Data (3) and then Fuel.Data (2) = 0);
         Prior := Revision (S);
         Handle (S, Snapshot_Provider, (Label => Finish_Snapshot, Data => [Prior,0,0,0], others => <>), Reply);
         Check (Reply.Status = Wrong_Order and then Revision (S) = Prior);
         Query (S, Metrics_Startup_Fuel_Page, Fuel.Data);
         Query (S, Metrics_Members_State_Page, [Revision (S),(if Failed_Startup then 0 else 1),0,0]);
      end;
   end loop;
   declare
      S : State (1, 256, 256, Load_Capacity => 1, Initial_Revision => 0);
      Reply, Fuel : Response;
      -- Root fails at an actually missing name; system-bus initializer still runs.
      Raw : constant Bytes := Migration_Tables.Make
        (Method ("_INI", [16#A4#] & Enc ("MISS")) &
         Block ([16#10#], Enc ("_SB_") & Method ("_INI", [16#A4#,1])));
   begin
      Handle (S, Snapshot_Provider, (Label => Start_Snapshot, Data => [0,1,0,0], others => <>), Reply);
      Check (Reply.Status = OK);
      Import_Block (S, Snapshot_Provider, Revision (S), 1, ACPI_Service.DSDT, Raw, Reply);
      Check (Reply.Status = OK);
      Handle (S, Snapshot_Provider, (Label => Finish_Snapshot, Data => [Revision (S),0,0,0], others => <>), Reply);
      Check (Reply.Status = OK and then Current (S) = Complete);
      Query (S, Metrics_Device_State_Page,
        [Revision (S),Unsigned_64 (Device_Phase_Code'Enum_Rep (Device_Completed_With_Errors_Code)),
         Unsigned_64 (Region_Status_Code'Enum_Rep (No_Regions_Required_Code)),1]);
      Query (S, Metrics_Device_Calls_Page, [Revision (S),0,2,0]);
      -- Stable wire code19 is Unknown_Name; first device error belongs to root0.
      Query (S, Metrics_Device_Error_Page, [Revision (S),19,0,0]);
      Handle (S, Observer, (Label => Read_Metrics, Data => [Metrics_Startup_Fuel_Page,0,0,0], others => <>), Fuel);
      Check (Fuel.Status = OK and then Fuel.Data (2) > 0 and then Fuel.Data (3) > 0 and then
        Fuel.Data (1) = Fuel.Data (2)+Fuel.Data (3));
      Handle (S, Snapshot_Provider, (Label => Finish_Snapshot, Data => [Revision (S),0,0,0], others => <>), Reply);
      Check (Reply.Status = Wrong_Order);
      Query (S, Metrics_Startup_Fuel_Page, Fuel.Data);
   end;
   Ada.Text_IO.Put_Line ("Startup metrics checks" & Checks'Image);
end Startup_Metrics_Tests;
