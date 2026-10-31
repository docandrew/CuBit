pragma Ada_2022;
with ACPI_HID_I2C_Discovery;
with ACPI_HID_I2C_Resolved;
with ACPI_Requests.HID_Devices;
with ACPI_Native_Log;
with Firmware_Tables;
with AML_Execute;
package body ACPI_Native_HID_Detection is
   package Core renames ACPI_Service;
   package Discovery is new ACPI_HID_I2C_Discovery (Core);
   -- Own controller-source/vendor spans without truncation. Longer spans fail
   -- explicitly; this is a diagnostic budget, never firmware authority.
   package Resolver is new ACPI_HID_I2C_Resolved (Core, Candidates);
   package Owner is new ACPI_Requests.HID_Devices (Discovery, Resolver);
   use type Owner.Controller.Admission, Owner.Controller.Search_Outcome;
   use type Discovery.Action_Result, Discovery.Outcome, Resolver.Outcome;
   use type Channels.Outcome;
   function Uncertain (Execution : AML_Execute.Execution_Status) return Boolean is
     (Execution in AML_Execute.Fatal_Requested | AML_Execute.Field_Indeterminate);
   function Search_Recovery (Item : Owner.Controller.Search_Report) return Boolean is
     (Item.Last_Identity.Outcome in Discovery.Queries.Cleanup_Failed |
        Discovery.Queries.Provider_Contract_Error or else Uncertain (Item.Last_Identity.Execution));
   function Probe_Recovery (Item : Owner.Controller.Report) return Boolean is
     (Item.Firmware.Result in Discovery.Cleanup_Failed | Discovery.Provider_Contract_Error
      or else Item.Firmware.Candidate.Outcome in Discovery.Queries.Cleanup_Failed |
        Discovery.Queries.Provider_Contract_Error
      or else Item.Firmware.Status.Outcome in Discovery.Queries.Cleanup_Failed |
        Discovery.Queries.Provider_Contract_Error
      or else Item.Firmware.Descriptor.Outcome in Discovery.Queries.Cleanup_Failed |
        Discovery.Queries.Provider_Contract_Error
      or else Uncertain (Item.Firmware.Execution)
      or else Uncertain (Item.Firmware.Candidate.Execution)
      or else Uncertain (Item.Firmware.Status.Execution)
      or else Uncertain (Item.Firmware.Descriptor.Execution));
   Attempted : Boolean := False;
   Saved : Report;
   Probe : Owner.Controller.Probe;
   procedure Run (Server : in out ACPI_Requests.State; Result : out Report;
      Limits : Policy := (others => <>)) is
      Cursor, Device : Core.Namespace.Node_ID := Core.Namespace.Root;
      Search : Owner.Controller.Search_Report;
      Allowed : Owner.Controller.Admission;
      Action : Discovery.Action_Result;
      Info : Owner.Controller.Report;
      Data : Firmware_Tables.Bytes (1 .. Limits.Resource_Bytes);
      procedure Finish (Status : Outcome) is
      begin
         Saved.Status := Status;
         Saved.Cleanup_Required := Owner.Controller.Pending (Probe);
         Saved.Recovery_Needed := Saved.Recovery_Needed or Saved.Cleanup_Required;
         if Status in No_Devices | Finished then
            ACPI_Native_Log.Info ("acpi: stage hid-discovery ok " & Status'Image &
              " candidates=" & Saved.Candidates'Image & " observed=" & Saved.Observed'Image &
              " fuel=" & Saved.Charged'Image);
         else
            ACPI_Native_Log.Warning ("acpi: stage hid-discovery fail " & Status'Image &
              " search=" & Search.Outcome'Image & " query=" & Info.Firmware.Result'Image &
              " source=" & Info.Sources.Status'Image &
              " channel=" & (if Saved.Channel_Attempted then Saved.Last_Channel_Status'Image else "NOT_ATTEMPTED") &
              " identity-exec=" & Search.Last_Identity.Execution'Image &
              " resource-exec=" & Info.Firmware.Execution'Image &
              " status-exec=" & Info.Firmware.Status.Execution'Image &
              " descriptor-exec=" & Info.Firmware.Descriptor.Execution'Image &
              " recovery=" & Boolean'Image (Saved.Recovery_Needed) &
              " required=" & Info.Firmware.Copy.Required'Image &
              " candidates=" & Saved.Candidates'Image & " observed=" & Saved.Observed'Image &
              " fuel=" & Saved.Charged'Image);
         end if;
         Result := Saved;
      end Finish;
   begin
      if Attempted then Result := Saved; return; end if;
      Attempted := True;
      Saved.Status := Owner_Error;
      loop
         Saved.Recovery_Needed := True;
         Owner.Begin_Next_Probe (Probe, Server, Cursor, Limits.Fuel - Saved.Charged,
           Device, Search, Allowed, Action);
         Saved.Charged := Saved.Charged + Search.Charged;
         Saved.Recovery_Needed := Search_Recovery (Search);
         if Saved.Recovery_Needed then Finish (Recovery_Required); return; end if;
         if Allowed = Owner.Controller.Not_Ready then Finish (Not_Ready); return; end if;
         if Search.Outcome = Owner.Controller.No_Match then
            Finish (if Saved.Candidates = 0 then No_Devices else Finished); return;
         end if;
         if Search.Outcome = Owner.Controller.Incomplete then Finish (Budget_Exhausted); return; end if;
         if Search.Outcome /= Owner.Controller.Found then Finish (Search_Failed); return; end if;
         if Action /= Discovery.Accepted then Finish (Owner_Error); return; end if;
         Saved.Candidates := Saved.Candidates + 1;
         Info := Owner.Controller.Observe (Probe);
         Saved.Recovery_Needed := Probe_Recovery (Info);
         if Saved.Recovery_Needed then
            Saved.Charged := Saved.Charged + Info.Firmware.Charged;
            Finish (Recovery_Required); return;
         end if;
         if Owner.Controller.Pending (Probe) then
            Saved.Recovery_Needed := True;
            Owner.Copy_And_Resolve (Probe, Server, Data, Allowed, Action);
         end if;
         Info := Owner.Controller.Observe (Probe);
         Saved.Charged := Saved.Charged + Info.Firmware.Charged;
         Saved.Recovery_Needed := Probe_Recovery (Info);
         if Saved.Recovery_Needed then Finish (Recovery_Required); return; end if;
         if Owner.Controller.Pending (Probe) then
            Saved.Recovery_Needed := True;
            Owner.Cancel (Probe, Server, Action);
            Info := Owner.Controller.Observe (Probe);
            Saved.Recovery_Needed := Probe_Recovery (Info);
            if Owner.Controller.Pending (Probe) then Finish (Cleanup_Failed); return; end if;
            if Saved.Recovery_Needed then Finish (Recovery_Required); return; end if;
            -- No sizing retry executes AML. The configured byte budget is final.
            Finish (Capacity_Refused); return;
         end if;
         if Info.Firmware.Result = Discovery.Budget_Exhausted then Finish (Budget_Exhausted); return; end if;
         if Info.Firmware.Result /= Discovery.Observed then Finish (Discovery_Failed); return; end if;
         if not Info.Sources_Attempted or else Info.Sources.Status /= Resolver.Resolved
           or else not Owner.Current (Probe, Server) then Finish (Sources_Failed); return; end if;
         declare
            Facts : constant Candidates.Candidate := Resolver.Facts (Info.Sources.Value);
            Prepared : constant Channels.Preparation := Channels.Prepare (Facts);
         begin
            Saved.Channel_Attempted := True;
            Saved.Last_Channel_Status := Prepared.Status;
            if Prepared.Status /= Channels.Prepared then Finish (Channel_Refused); return; end if;
            ACPI_Native_Log.Info ("acpi: stage hid-resource ok OBSERVED" &
              " device=" & Device'Image &
              " i2c=" & Core.Values.Position (Resolver.I2C_Controller (Info.Sources.Value))'Image &
              " gpio=" & Core.Values.Position (Resolver.GPIO_Controller (Info.Sources.Value))'Image &
              " address=" & Facts.I2C.Address'Image &
              " descriptor=" & Facts.Descriptor_Register'Image & " pin=" & Facts.Pin'Image);
            Saved.Items (Saved.Observed + 1) :=
              (Device => Resolver.Device (Info.Sources.Value),
               I2C_Controller => Resolver.I2C_Controller (Info.Sources.Value),
               GPIO_Controller => Resolver.GPIO_Controller (Info.Sources.Value),
               Channel_Data => Prepared);
         end;
         Saved.Observed := Saved.Observed + 1;
         if Saved.Candidates = Limits.Candidates then Finish (Candidate_Limit); return; end if;
         Cursor := Device;
      end loop;
   end Run;
end ACPI_Native_HID_Detection;
