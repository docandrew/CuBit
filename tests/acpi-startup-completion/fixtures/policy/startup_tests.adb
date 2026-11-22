pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with ACPI_Startup_Completion;
with CuBit.Consumer_Initialization_Exchange;
procedure Startup_Tests is
   package C renames ACPI_Startup_Completion;
   package P renames CuBit.Consumer_Initialization_Exchange;
   use type P.Phase_Status, Interfaces.Unsigned_64;
   use type C.Phase_Result;
   type Results is array (Positive range 1 .. 4) of C.Phase_Result;
   Count : Natural := 0;
   type Release_Values is array (Positive range 1 .. 5) of Interfaces.Unsigned_64;
   Releases : constant Release_Values := [0, 1, 2, 16#1_0000_0000#, Interfaces.Unsigned_64'Last];
begin
   for Devices in C.Phase_Result loop
      for HID in C.Phase_Result loop
         for Power in C.Phase_Result loop
            for Events in C.Phase_Result loop
            for Modules in Boolean loop
            for Closed in Boolean loop
               declare
                  Items : constant Results := [Devices, HID, Power, Events];
                  Failures, Pending, Errors : Natural := 0;
                  Expected : P.Phase_Status;
               begin
                  for Item of Items loop
                     case Item is
                        when C.Failed => Failures := Failures + 1;
                        when C.Incomplete => Pending := Pending + 1;
                        when C.Finished_With_Errors => Errors := Errors + 1;
                        when C.Finished => null;
                     end case;
                  end loop;
                  if Failures > 0 then Expected := P.Failed;
                  elsif Pending > 0 or else not Closed then Expected := P.Incomplete;
                  elsif Errors > 0 or else Modules then Expected := P.Completed_With_Errors;
                  else Expected := P.Completed;
                  end if;
                  if C.Decide (Devices, HID, Power, Closed, Events, Modules) /= Expected then
                     raise Program_Error with "startup classification";
                  end if;
                  Count := Count + 1;
               end;
            end loop;
            end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   for Dispatched in Boolean loop
      for Known in Boolean loop
         for Receipt in P.Receipt loop
            for Released of Releases loop
               declare Expected : Boolean := False; begin
                  if Dispatched then
                     if not Known then Expected := True;
                     else case Receipt is
                        when P.Received => Expected := Released > 0;
                        when others => Expected := True;
                     end case; end if;
                  end if;
                  if C.Requires_Retention (Dispatched, Known, Receipt, Released) /= Expected then
                     raise Program_Error with "completion receipt lifecycle classification";
                  end if;
                  Count := Count + 1;
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   for Live in Boolean loop
      for Absent in Boolean loop
         for Recovery in Boolean loop
            declare
               Actual : constant C.Phase_Result := C.Unavailable_Events (Live, Absent, Recovery);
               Expected : C.Phase_Result;
               Status : P.Phase_Status;
            begin
               if Recovery then Expected := C.Failed;
               elsif not Live then Expected := C.Incomplete;
               elsif Absent then Expected := C.Finished;
               else Expected := C.Finished_With_Errors;
               end if;
               if Actual /= Expected then raise Program_Error with "optional event phase classification"; end if;
               Count := Count + 1;
               Status := C.Decide (C.Finished, C.Finished, C.Finished, True, Events => Actual);
               if Status /= (case Expected is when C.Finished => P.Completed,
                  when C.Finished_With_Errors => P.Completed_With_Errors,
                  when C.Incomplete => P.Incomplete, when C.Failed => P.Failed)
               then raise Program_Error with "optional event consumer declaration"; end if;
               Count := Count + 1;
               if P.Ran_To_End (Status) /= (Live and not Recovery) then
                  raise Program_Error with "only safe completed optional event phase can release consumers";
               end if;
               Count := Count + 1;
            end;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Startup completion checks" & Count'Image);
end Startup_Tests;
