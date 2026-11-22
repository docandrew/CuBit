pragma Ada_2022;
with ACPI_Firmware_Data_Layouts;
with Firmware_Data_ABI;
with Capability_ABI;
package body ACPI_Firmware_Data_Mapping with SPARK_Mode is
   package L renames ACPI_Firmware_Data_Layouts;
   use type Word, R.IPC.Time_Instant, R.Wire.Disposition;
   function Observe (Owner : Context) return Report is (Owner.Last);
   procedure Map_Exact (Owner : in out Context; Held : Capability; Item : Arena.Reservation;
      Physical, Virtual, Pages : Word; Deadline : R.IPC.Time_Instant; Result : out Report) is
      procedure Check_Time is
         Now : R.IPC.Time_Instant; Available : Boolean;
      begin
         if Owner.Clock_Broken then Result.Status := Clock_Reversed; return; end if;
         Clock (Now, Available);
         if not Available then Result.Status := Clock_Unavailable;
         elsif Owner.Seen_Clock and then Now < Owner.Last_Clock then
            Owner.Clock_Broken := True; Result.Status := Clock_Reversed;
         else
            Owner.Seen_Clock := True; Owner.Last_Clock := Now;
            Result.Status := (if Now >= Deadline then Expired else Mapped);
         end if;
      end Check_Time;
      procedure Run is
         Base, Reserved_Pages, Chunk : Word; Known : Boolean;
         Evidence : R.Grant_Evidence;
         Kind : R.Wire.Disposition;
         First_Kind : R.Wire.Disposition := R.Wire.Malformed;
         Seen_Kind : Boolean := False;
         Completed : Boolean;
      begin
         if Deadline = 0 or else Deadline = R.IPC.No_Finite_Deadline then Result.Status := Invalid_Deadline; return; end if;
         if Physical mod L.Page_Bytes /= 0 or else
           not P.Valid ((Physical / L.Page_Bytes, Pages)) or else
           not L.Virtual_Valid (Virtual, (Physical / L.Page_Bytes, Pages))
         then Result.Status := Invalid_Request; return; end if;
         Arena.Describe (Reservations, Item, Base, Reserved_Pages, Known);
         if not Known or else Base /= Virtual or else Reserved_Pages /= Pages then
            Result.Status := Invalid_Reservation; return;
         end if;
         Check_Time; if Result.Status /= Mapped then return; end if;
         Arena.Claim (Reservations, Item, Known);
         if not Known then Result.Status := Already_Attempted; return; end if;
         Result.Reservation_Claimed := True;
         while Result.Confirmed_Pages < Pages loop
            Check_Time; if Result.Status /= Mapped then return; end if;
            Inspect (Held, Evidence);
            Kind := (if Evidence.Tag = Firmware_Data_ABI.Firmware_Data_Tag then R.Wire.Granted_Data
              elsif Evidence.Tag = Capability_ABI.Firmware_NVS_Tag then R.Wire.Reused_NVS else R.Wire.Malformed);
            if not R.Admitted (Evidence, Kind, (Physical / L.Page_Bytes, Pages)) then
               Result.Status := Invalid_Grant; return;
            end if;
            if Seen_Kind and then Kind /= First_Kind then Result.Status := Invalid_Grant; return; end if;
            First_Kind := Kind; Seen_Kind := True;
            Check_Time; if Result.Status /= Mapped then return; end if;
            Chunk := Word'Min (Pages - Result.Confirmed_Pages, Word (Maximum_Per_Call));
            Result.Calls := Result.Calls + 1;
            Map_Chunk (Held, Physical + Result.Confirmed_Pages * L.Page_Bytes,
              Virtual + Result.Confirmed_Pages * L.Page_Bytes, Chunk, Completed);
            if not Completed then Result.Status := Mapping_Indeterminate; return; end if;
            Result.Confirmed_Pages := Result.Confirmed_Pages + Chunk;
         end loop;
         Check_Time;
         if Result.Status /= Mapped then Result.Status := Late_Completed; end if;
      end Run;
   begin
      Result := (others => <>);
      if Owner.Working then Result.Status := Reentered; return; end if;
      Owner.Working := True; Run; Owner.Working := False; Owner.Last := Result;
   end Map_Exact;
end ACPI_Firmware_Data_Mapping;
