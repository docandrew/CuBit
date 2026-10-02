pragma Ada_2022;
package body ACPI_Requests with SPARK_Mode is
   use type ACPI_Service.Install_Status;
   function Revision (Server : State) return Revision_Number is (Server.Version);
   function Current (Server : State) return Phase is
     (if not Server.Started then Idle else
       (case ACPI_Bootstrap.Current (Server.Boot) is
          when ACPI_Bootstrap.Receiving => Receiving,
          when ACPI_Bootstrap.Complete => Complete,
          when ACPI_Bootstrap.Failed => Failed));
   function Table_Open (Server : State) return Boolean is (Server.Open);
   function Received (Server : State) return Natural is (Server.Used);
   function Observe (Server : State) return ACPI_Service.Metrics is (ACPI_Bootstrap.Observe (Server.Boot));
   function Fresh
     (Initial_Revision : Revision_Number := 0;
      Table_Capacity : Positive := ACPI_Service.Max_Tables;
      Byte_Capacity : Positive := ACPI_Service.Max_Total_Bytes;
      Table_Byte_Limit : Positive := ACPI_Service.Max_Table_Bytes) return State is
     ((Table_Capacity => Table_Capacity, Byte_Capacity => Byte_Capacity,
       Table_Byte_Limit => Table_Byte_Limit,
       Boot => ACPI_Bootstrap.Start (0, Table_Capacity, Byte_Capacity, Table_Byte_Limit),
       Version => Initial_Revision, others => <>));
   procedure Import_Block
     (Server : in out State; Origin : Authority; Token : Unsigned_64;
      ID : Positive; Kind : ACPI_Service.Table_Kind;
      Data : Firmware_Tables.Bytes; Reply : out Response) is
      Result : ACPI_Service.Install_Status;
   begin
      Reply := (Status => Denied, Data => [others => 0]);
      if Origin = No_Authority then return; end if;
      Reply.Data (0) := Server.Version;
      if Origin /= Snapshot_Provider then return; end if;
      Reply.Status := Stale;
      if Token /= Server.Version then return; end if;
      Reply.Status := Resource_Limit;
      if Server.Version = Max_Revision then return; end if;
      Reply.Status := Malformed;
      if Data'Length < Firmware_Tables.Table_Header_Size
        or else Data'Length > Server.Table_Byte_Limit
      then return; end if;
      Reply.Status := Wrong_Order;
      if Current (Server) /= Receiving or else Server.Open or else
        ACPI_Bootstrap.Installed (Server.Boot) = ACPI_Bootstrap.Expected (Server.Boot)
      then return; end if;
      ACPI_Bootstrap.Import_Table (Server.Boot, ID, Kind, Data, Result);
      Server.Version := Server.Version + 1;
      Reply.Data (0) := Server.Version;
      Reply.Status := (if Result = ACPI_Service.Installed then OK else Table_Rejected);
      if Reply.Status = Table_Rejected then
         Reply.Data (1) := Unsigned_64 (ACPI_Service.Install_Status'Pos (Result));
      end if;
   end Import_Block;
   procedure Handle (Server : in out State; Origin : Authority;
                     Request : Packet; Reply : out Response) is
      Amount : Natural range 0 .. 16;
      Initial_Version : constant Revision_Number := Server.Version;
      Result : ACPI_Service.Install_Status;
      function Octet (Index : Natural) return Firmware_Tables.Byte is
        (Firmware_Tables.Byte (Shift_Right (Request.Data (2 + Index / 8), (Index mod 8) * 8) and 255))
        with Pre => Index < 16;
   begin
      Reply := (Status => Malformed, Data => [0 => Server.Version, others => 0]);
      if Origin = No_Authority then
         Reply := (Status => Denied, Data => [others => 0]);
         return;
      end if;
      if Request.Length /= 4 or Request.Flags /= 0 or Request.Reserved /= 0 then return; end if;
      if Request.Label = Read_Metrics then
         if Request.Data (0) > 6 or else Request.Data (1 .. 3) /= [0, 0, 0] then return; end if;
         declare
            Stats : constant ACPI_Service.Metrics := Observe (Server);
         begin
            Reply.Status := OK;
            case Request.Data (0) is
               when 0 => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (Phase'Enum_Rep (Current (Server))),
                  Unsigned_64 (ACPI_Bootstrap.Expected (Server.Boot)), Unsigned_64 (Stats.Tables)];
               when 1 => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (Stats.Bytes), Unsigned_64 (Stats.Objects), Unsigned_64 (Stats.Value_Objects)];
               when 2 => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (Stats.Value_Bytes), Unsigned_64 (Stats.Package_Elements), Unsigned_64 (Stats.Rejections)];
               when 3 => Reply.Data (1 .. 3) :=
                 [Boolean'Pos (Stats.Counter_Saturated), Boolean'Pos (Server.Open), Unsigned_64 (Server.Used)];
               when 4 => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (Server.Table_Byte_Limit), Unsigned_64 (Server.Byte_Capacity),
                  Unsigned_64 (Server.Table_Capacity)];
               when 5 => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (Stats.Method_Bytes),
                  Unsigned_64 (ACPI_Service.Max_Method_Bytes),
                  Unsigned_64 (ACPI_Service.Max_Method_Bytes - Stats.Method_Bytes)];
               when others => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (Stats.Last_Load_Code), Unsigned_64 (ACPI_Service.Max_Namespace_Nodes),
                  Unsigned_64 (ACPI_Service.Max_Namespace_Nodes - Stats.Objects)];
            end case;
         end;
         return;
      end if;
      if Request.Label in Read_Table_Info | Read_Table_Chunk then
         if Request.Data (3) /= 0 or else
           (Request.Label = Read_Table_Info and then Request.Data (2) /= 0)
         then return; end if;
         if Request.Data (0) /= Server.Version then Reply.Status := Stale; return; end if;
         if Current (Server) /= Complete then Reply.Status := Wrong_Order; return; end if;
         if Request.Data (1) = 0 or else
           Request.Data (1) > Unsigned_64 (Server.Table_Capacity)
         then Reply.Status := Not_Found; return; end if;
         declare
            Index : constant Positive := Positive (Request.Data (1));
            Info : ACPI_Service.Table_Metadata;
            Low, High : Unsigned_32 := 0;
            Offset : Natural;
         begin
            if Index > ACPI_Bootstrap.Installed (Server.Boot) then
               Reply.Status := Not_Found; return;
            end if;
            Info := ACPI_Bootstrap.Table_Info (Server.Boot, Index);
            if Request.Label = Read_Table_Info then
               for I in Info.Signature'Range loop
                  Low := Low or Shift_Left (Unsigned_32 (Character'Pos (Info.Signature (I))), (I - 1) * 8);
               end loop;
               Reply.Data (1 .. 3) := [Unsigned_64 (Info.ID), Unsigned_64 (Info.Extent), Unsigned_64 (Low)];
            else
               if Request.Data (2) > Unsigned_64 (Server.Table_Byte_Limit) then return; end if;
               Offset := Natural (Request.Data (2));
               if Offset > Info.Extent then return; end if;
               Amount := Natural'Min (8, Info.Extent - Offset);
               for I in 0 .. Amount - 1 loop
                  declare
                     B : constant Unsigned_32 := Unsigned_32 (ACPI_Bootstrap.Table_Byte (Server.Boot, Index, Offset + I));
                  begin
                     if I < 4 then Low := Low or Shift_Left (B, I * 8);
                     else High := High or Shift_Left (B, (I - 4) * 8); end if;
                  end;
               end loop;
               Reply.Data (1 .. 3) := [Unsigned_64 (Amount), Unsigned_64 (Low), Unsigned_64 (High)];
            end if;
            Reply.Status := OK;
         end;
         return;
      end if;
      if Origin /= Snapshot_Provider then Reply.Status := Denied; return; end if;
      if Request.Label not in Start_Snapshot .. Finish_Snapshot then return; end if;
      if Request.Data (0) /= Server.Version then Reply.Status := Stale; return; end if;
      if Initial_Version = Max_Revision then Reply.Status := Resource_Limit; return; end if;
      case Request.Label is
         when Start_Snapshot =>
            if Request.Data (1) not in 1 .. Unsigned_64 (Server.Table_Capacity)
              or else Request.Data (2 .. 3) /= [0, 0] then return; end if;
            if Server.Started then Reply.Status := Wrong_Order; return; end if;
            Server.Boot := ACPI_Bootstrap.Start
              (Natural (Request.Data (1)), Server.Table_Capacity,
               Server.Byte_Capacity, Server.Table_Byte_Limit);
            Server.Started := True;
         when Begin_Table =>
            if Request.Data (1) not in 1 .. Unsigned_64 (Positive'Last)
              or else Request.Data (2) > 2
              or else Request.Data (3) < Unsigned_64 (Firmware_Tables.Table_Header_Size)
              or else Request.Data (3) > Unsigned_64 (Natural'Last)
            then return; end if;
            -- Check representation before conversion, then compare the quota
            -- in the same integer domain as the retained buffer extent.
            if Natural (Request.Data (3)) > Server.Table_Byte_Limit then return; end if;
            if Current (Server) /= Receiving or else Server.Open or else
              ACPI_Bootstrap.Installed (Server.Boot) = ACPI_Bootstrap.Expected (Server.Boot)
            then Reply.Status := Wrong_Order; return; end if;
            Server.ID := Positive (Request.Data (1));
            Server.Kind := ACPI_Service.Table_Kind'Val (Request.Data (2));
            Server.Extent := Natural (Request.Data (3));
            Server.Open := True;
         when Write_Chunk =>
            if not Server.Open or else Server.Used = Server.Extent then Reply.Status := Wrong_Order; return; end if;
            if Request.Data (1) /= Unsigned_64 (Server.Used) then Reply.Status := Wrong_Order; return; end if;
            Amount := Natural'Min (16, Server.Extent - Server.Used);
            -- Reject noncanonical padding before changing any buffered byte.
            for I in Amount .. 15 loop
               if Octet (I) /= 0 then return; end if;
            end loop;
            declare
               Chunk_Length : constant Natural := Amount;
               Chunk : Firmware_Tables.Bytes (1 .. Chunk_Length);
            begin
               for I in Chunk'Range loop Chunk (I) := Octet (I - 1); end loop;
               Server.Buffer_Data (Server.Used + 1 .. Server.Used + Amount) := Chunk;
            end;
            Server.Used := Server.Used + Amount;
         when Commit_Table =>
            if Request.Data (1 .. 3) /= [0, 0, 0] then return; end if;
            if not Server.Open or else Server.Used /= Server.Extent then Reply.Status := Wrong_Order; return; end if;
            declare
               Count : constant Natural := Server.Extent;
            begin
               Server.Open := False;
               Server.Used := 0;
               Server.Extent := 0;
               ACPI_Bootstrap.Import_Table (Server.Boot, Server.ID, Server.Kind, Server.Buffer_Data (1 .. Count), Result);
            end;
            if Result /= ACPI_Service.Installed then
               Reply.Status := Table_Rejected;
               Reply.Data (1) := Unsigned_64 (ACPI_Service.Install_Status'Pos (Result));
            end if;
         when Finish_Snapshot =>
            if Request.Data (1 .. 3) /= [0, 0, 0] then return; end if;
            if Current (Server) /= Receiving or else Server.Open then Reply.Status := Wrong_Order; return; end if;
            ACPI_Bootstrap.Finish (Server.Boot);
            if Current (Server) /= Complete then Reply.Status := Incomplete; end if;
         when others => return;
      end case;
      -- Establish the small request-state invariant before advancing the token;
      -- keep these obligations separate from the retained-table representation.
      pragma Assert (Server.Used <= Server.Extent);
      pragma Assert (Server.Extent <= Server.Table_Byte_Limit);
      pragma Assert
        (if Server.Open then Server.Started and then
           ACPI_Bootstrap.Current (Server.Boot) = ACPI_Bootstrap.Receiving
           and then Server.Extent >= Firmware_Tables.Table_Header_Size
         else Server.Used = 0 and then Server.Extent = 0);
      Server.Version := Initial_Version + 1;
      Reply.Data (0) := Server.Version;
      if Reply.Status = Malformed then Reply.Status := OK; end if;
   end Handle;
end ACPI_Requests;
