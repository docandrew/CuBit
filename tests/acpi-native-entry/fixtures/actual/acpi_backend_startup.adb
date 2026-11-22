pragma Ada_2022;
with ACPI_Register_Roles;
with ACPI_Backend_Register_IDs;
package body ACPI_Backend_Startup is
   use type B.Tick, B.Resource_Kind, B.Outcome, W.Window_ID, Timer.Status;
   use type Catalog.Add_Code;
   Frontend_Stamp : constant Unsigned_64 := Unsigned_64 (ACPI_Register_Roles.Frontend_Tag);
   function Observe (Run : Context) return Report is (Run.Info);
   procedure Initialize
     (Run : in out Context; Hardware : in out Catalog.Context;
      Input : aliased in Grant; FADT : ACPI_FADT.Description;
      Deadline : B.Tick; Result : out Report)
   is
      Saved : Grant;
      Saved_FADT : constant ACPI_FADT.Description := FADT;
      Windows : array (1 .. Maximum_IO_Windows + Maximum_Memory_Windows) of W.Granted_Window;
      Count : Natural := 0;
      ID : W.Window_ID := W.No_Window;
      Actual : B.Description;
      Window : W.Window_Result;
      Bound : B.Report;
      Known, Seen_Clock : Boolean := False;
      Last : B.Tick := 0;
      Stats : constant Catalog.Statistics := Catalog.Observe (Hardware);
      Plan : Timer.Result;
      Added : Catalog.Add_Report;
      Timer_Window : W.Window_ID := W.No_Window;
      Timer_Item : Catalog.P.Configuration;
      procedure Stop (Why : Outcome) is
      begin Run.Info.Status := Why; Run.Stage := Finished; Result := Run.Info; end Stop;
      function In_Time return Boolean is
         Now : B.Tick;
         Available : Boolean;
      begin
         if Deadline = B.No_Finite_Deadline then return False; end if;
         Read_Clock (Now, Available);
         if not Available or else (Seen_Clock and then Now < Last) or else Now >= Deadline then return False; end if;
         Last := Now; Seen_Clock := True; return True;
      end In_Time;
      function Matches (Space : B.Resource_Kind) return Boolean is
        (Actual.Current and then Actual.Kind = Space and then (Actual.Readable or Actual.Writable));
   begin
      Result := (Status => Already_Consumed, others => <>);
      if Run.Stage /= Fresh then return; end if;
      Run.Stage := Building;
      if Stats.Sealed or Stats.Working or Stats.Quarantined or Stats.Registers /= 0 or Stats.Retained_Windows /= 0 then
         Stop (Catalog_Not_Empty); return;
      end if;
      Saved.IO_Used := Input.IO_Used; Saved.Memory_Used := Input.Memory_Used;
      for I in 1 .. Saved.IO_Used loop Saved.IO (I) := Input.IO (I); end loop;
      for I in 1 .. Saved.Memory_Used loop Saved.Memory (I) := Input.Memory (I); end loop;
      -- Geometry alone is validated here; no rights or authority are inferred.
      for I in 1 .. Saved.IO_Used loop
         if not W.IO_Description_Valid (1, Unsigned_64 (Saved.IO (I).First), Unsigned_64 (Saved.IO (I).Count), True, False)
           or else (I > 1 and then (Saved.IO (I).First <= Saved.IO (I - 1).First
             or else Unsigned_64 (Saved.IO (I - 1).Count) > Unsigned_64 (Saved.IO (I).First - Saved.IO (I - 1).First)))
         then Stop (Invalid_Grant); return; end if;
      end loop;
      for I in 1 .. Saved.Memory_Used loop
         if not W.Memory_Description_Valid (1, Saved.Memory (I).First_Page, Saved.Memory (I).Pages, True, False)
           or else (I > 1 and then (Saved.Memory (I).First_Page <= Saved.Memory (I - 1).First_Page
             or else Saved.Memory (I - 1).Pages > Saved.Memory (I).First_Page - Saved.Memory (I - 1).First_Page))
         then Stop (Invalid_Grant); return; end if;
      end loop;
      for I in 1 .. Saved.IO_Used loop
         ID := ID + 1; Run.Info.Failed_Window := ID;
         if not In_Time then Stop (Clock_Failed); return; end if;
         Catalog.Describe_Ports (Saved.IO (I).Ports, Actual);
         if not Matches (B.Port_Resource) then Stop (Invalid_Grant); return; end if;
         Window := W.From_IO_Ports (ID, Unsigned_64 (Saved.IO (I).First), Unsigned_64 (Saved.IO (I).Count),
           Actual.Readable, Actual.Writable, Actual.Delegable);
         if not Window.Allowed or else Actual.Base /= Window.Value.Base or else Actual.Length /= Window.Value.Length then
            Stop (Invalid_Grant); return;
         end if;
         if not In_Time then Stop (Clock_Failed); return; end if;
         Catalog.Bind_Ports (Hardware, Window.Value, Saved.IO (I).Ports, Deadline, Bound);
         Run.Info.Binding := Bound;
         if Bound.Status /= B.Bound then Stop (Binding_Failed); return; end if;
         Count := Count + 1; Windows (Count) := Window.Value; Run.Info.Bound_Windows := Count;
      end loop;
      for I in 1 .. Saved.Memory_Used loop
         ID := ID + 1; Run.Info.Failed_Window := ID;
         if not In_Time then Stop (Clock_Failed); return; end if;
         Catalog.Describe_Memory (Saved.Memory (I).Memory, Actual);
         if not Matches (B.Memory_Resource) then Stop (Invalid_Grant); return; end if;
         Window := W.From_Memory_Pages (ID, Saved.Memory (I).First_Page, Saved.Memory (I).Pages,
           Actual.Readable, Actual.Writable, Actual.Delegable);
         if not Window.Allowed or else Actual.Base /= Window.Value.Base or else Actual.Length /= Window.Value.Length then
            Stop (Invalid_Grant); return;
         end if;
         if not In_Time then Stop (Clock_Failed); return; end if;
         Catalog.Bind_Memory (Hardware, Window.Value, Saved.Memory (I).Memory, Deadline, Bound);
         Run.Info.Binding := Bound;
         if Bound.Status /= B.Bound then Stop (Binding_Failed); return; end if;
         Count := Count + 1; Windows (Count) := Window.Value; Run.Info.Bound_Windows := Count;
      end loop;
      if not In_Time then Stop (Clock_Failed); return; end if;
      Plan := Timer.Prepare (Saved_FADT, (others => <>), Frontend_Stamp, ACPI_Backend_Register_IDs.PM_Timer);
      Run.Info.Timer_Status := Plan.Code; Run.Info.Timer_Block := Plan.Block_Status; Run.Info.Timer_Admission := Plan.Admission;
      for I in 1 .. Count loop
         Plan := Timer.Prepare (Saved_FADT, Windows (I), Frontend_Stamp, ACPI_Backend_Register_IDs.PM_Timer);
         Run.Info.Timer_Status := Plan.Code; Run.Info.Timer_Block := Plan.Block_Status; Run.Info.Timer_Admission := Plan.Admission;
         if Plan.Code = Timer.Ready then Timer_Window := Windows (I).ID; Timer_Item := Plan.Configuration; exit; end if;
      end loop;
      Catalog.Authorize_Scope (Hardware, Frontend_Stamp, Known);
      if not Known then Stop (Scope_Failed); return; end if;
      if Timer_Window /= W.No_Window then
         Catalog.Add (Hardware, Timer_Window, Timer_Item, W.Unestablished, Added);
         if Added.Status /= Catalog.Added then Stop (Record_Failed); return; end if;
      end if;
      if not In_Time then Stop (Clock_Failed); return; end if;
      Catalog.Seal (Hardware, Known);
      if not Known then Stop (Seal_Failed); return; end if;
      Run.Info.Failed_Window := W.No_Window;
      Stop (Ready);
   end Initialize;
end ACPI_Backend_Startup;
