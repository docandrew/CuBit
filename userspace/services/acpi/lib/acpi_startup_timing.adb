pragma Ada_2022;
with ACPI_Inventory_Protocol; with ACPI_Region_Protocol; with ACPI_NVS_Protocol;
with ACPI_PCI_Protocol; with ACPI_Global_Lock_Protocol; with ACPI_Locked_NVS_Protocol;
package body ACPI_Startup_Timing with SPARK_Mode is
   use type Word, Interfaces.Unsigned_32;
   function Name (Item : Kind) return String is
     (case Item is when Startup => "STARTUP",when Provider_Init => "PROVIDER_INIT",
      when Snapshot_Load => "SNAPSHOT_LOAD",when Module_Execution => "MODULE",
      when Work_Lookup => "WORK_LOOKUP",when Work_Store => "WORK_STORE",
      when Work_Clone => "WORK_CLONE",when Work_Allocation => "WORK_ALLOCATION",when Work_Snapshot => "WORK_SNAPSHOT",
      when Inventory_Header => "INVENTORY_HEADER",when Inventory_Part => "INVENTORY_PART",
      when NVS_Header => "NVS_HEADER",when NVS_Describe => "NVS_DESCRIBE",when NVS_Read => "NVS_READ",
      when PCI_Header => "PCI_HEADER",when PCI_Describe => "PCI_DESCRIBE",when PCI_Read => "PCI_READ",
      when Scalar_Read => "SCALAR_READ",when Scalar_Write => "SCALAR_WRITE",
      when Lock_Acquire => "LOCK_ACQUIRE",when Lock_Release => "LOCK_RELEASE",when Lock_Other => "LOCK_OTHER",
      when Scoped_Select => "SCOPED_SELECT",when Scoped_Read => "SCOPED_READ",when Scoped_Query => "SCOPED_QUERY",
      when Other_RPC => "OTHER_RPC",when AML_Sleep => "AML_SLEEP",when AML_Stall => "AML_STALL",
      when Firmware_Data_RPC => "FIRMWARE_DATA");
   function Classify (Label : Interfaces.Unsigned_32) return Kind is
      package I renames ACPI_Inventory_Protocol; package R renames ACPI_Region_Protocol;
      package N renames ACPI_NVS_Protocol; package C renames ACPI_PCI_Protocol;
      package G renames ACPI_Global_Lock_Protocol; package D renames ACPI_Locked_NVS_Protocol;
   begin
      if Label = I.Header_Operation then return Inventory_Header;
      elsif Label = I.Part_Operation then return Inventory_Part;
      elsif Label = N.Header_Operation then return NVS_Header;
      elsif Label = N.Describe_Operation then return NVS_Describe;
      elsif Label = N.Read_Operation then return NVS_Read;
      elsif Label = C.Header_Operation then return PCI_Header;
      elsif Label = C.Describe_Operation then return PCI_Describe;
      elsif Label = C.Read_Operation then return PCI_Read;
      elsif Label = R.Read_Operation then return Scalar_Read;
      elsif Label = R.Write_Operation then return Scalar_Write;
      elsif Label = G.First_Operation+G.Operation'Pos (G.Acquire) then return Lock_Acquire;
      elsif Label = G.First_Operation+G.Operation'Pos (G.Release) then return Lock_Release;
      elsif Label in G.First_Operation .. G.First_Operation+G.Operation'Pos (G.Operation'Last) then return Lock_Other;
      elsif Label = D.First_Operation+D.Operation'Pos (D.Select_Datum) then return Scoped_Select;
      elsif Label = D.First_Operation+D.Operation'Pos (D.Read_Datum) then return Scoped_Read;
      elsif Label = D.First_Operation+D.Operation'Pos (D.Query_Read) then return Scoped_Query;
      else return Other_RPC; end if;
   end Classify;
   function Add (Left,Right : Word) return Word is
     (if Right > Word'Last-Left then Word'Last else Left+Right);
   procedure Start (State : in out Context) is
   begin if not State.Started then State.Started := True; State.Recording := True; end if; end Start;
   function Active (State : Context) return Boolean is (State.Recording);
   procedure Sample (State : in out Context; Now : Word; Available : Boolean; Good : out Boolean) is
   begin
      State.Clock_Reads := Add (State.Clock_Reads,1);
      if State.Clock_Reads = Word'Last then State.Saturated := True; end if;
      if not Available or else (State.Seen_Clock and then Now < State.Last) then State.Clock_Broken := True; end if;
      Good := not State.Clock_Broken;
      if Good then State.Seen_Clock := True; State.Last := Now; end if;
   end Sample;
   function External_At (State : Context; Now : Word) return Word is
     (if State.External_Depth = 0 then State.External_Total
      else Add (State.External_Total,Now-State.External_Began));
   procedure Enter (State : in out Context; Item : Kind; Now : Word; Available : Boolean; Handle : out Token) is
      Good : Boolean;
   begin
      Handle := No_Token; if not State.Recording then return; end if;
      -- Count attempts even if timing is unavailable; never affect driver flow.
      State.Items (Item).Calls := Add (State.Items (Item).Calls,1);
      if State.Items (Item).Calls = Word'Last then State.Saturated := True; end if;
      Sample (State,Now,Available,Good);
      if State.Used = Depth'Last or else State.Serial = Word'Last then State.Malformed := True; return; end if;
      State.Serial := State.Serial+1; State.Used := State.Used+1;
      State.Frames (State.Used) := (State.Serial,Item,Now,(if Good then External_At (State,Now) else 0),Good);
      Handle := (State.Serial,State.Used);
      if External (Item) then
         if State.External_Depth = 0 then State.External_Began := Now; end if;
         State.External_Depth := State.External_Depth+1;
      end if;
   end Enter;
   procedure Leave (State : in out Context; Handle : in out Token; Now : Word; Available : Boolean) is
      F : Frame; Good : Boolean; Duration,Union_Now,External_Time : Word := 0;
   begin
      if Handle = No_Token then return; end if;
      if not State.Recording or else State.Used = 0 or else Handle.Slot /= State.Used
        or else Handle.Serial /= State.Frames (State.Used).Serial
      then State.Malformed := True; Handle := No_Token; return; end if;
      F := State.Frames (State.Used); Sample (State,Now,Available,Good);
      if Good and then F.Valid then
         Duration := Now-F.Began; Union_Now := External_At (State,Now);
         if Union_Now < F.External_Before then State.Malformed := True;
         else External_Time := Union_Now-F.External_Before; end if;
         if External_Time > Duration then State.Malformed := True; External_Time := Duration; end if;
         State.Items (F.Item).Samples := Add (State.Items (F.Item).Samples,1);
         declare C : Counters := State.Items (F.Item); begin
            if Duration > Word'Last-C.Total_US or else External_Time > Word'Last-C.External_US then State.Saturated := True; end if;
            C.Total_US := Add (C.Total_US,Duration); C.External_US := Add (C.External_US,External_Time);
            C.Worst_US := Word'Max (C.Worst_US,Duration); State.Items (F.Item) := C;
         end;
      end if;
      if External (F.Item) then
         State.External_Depth := State.External_Depth-1;
         if Good and then F.Valid and then State.External_Depth = 0 then State.External_Total := Union_Now; end if;
      end if;
      State.Used := State.Used-1; Handle := No_Token;
   end Leave;
   procedure Stop (State : in out Context) is
   begin if State.Used /= 0 then State.Malformed := True; end if; State.Recording := False; end Stop;
   function Read (State : Context; Item : Kind) return Counters is (State.Items (Item));
   function Complete (State : Context) return Boolean is
     (State.Started and not State.Clock_Broken and not State.Malformed and not State.Saturated and State.Used = 0);
   function Open_Scopes (State : Context) return Natural is (State.Used);
   function Clock_Samples (State : Context) return Word is (State.Clock_Reads);
end ACPI_Startup_Timing;
