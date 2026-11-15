pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with ACPI_Startup_Completion;
with CuBit.Consumer_Initialization_Exchange;
procedure Startup_Tests is
   package C renames ACPI_Startup_Completion;
   package P renames CuBit.Consumer_Initialization_Exchange;
   use type P.Phase_Status, Interfaces.Unsigned_64;
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
   Ada.Text_IO.Put_Line ("Startup completion checks" & Count'Image);
end Startup_Tests;
