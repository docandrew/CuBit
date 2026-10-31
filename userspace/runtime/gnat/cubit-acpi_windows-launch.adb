pragma Ada_2022;
with CuBit.Handles.IPC;
with CuBit.Handles.Startup;
with CuBit.Startup_Tables;

package body CuBit.ACPI_Windows.Launch with SPARK_Mode => Off is

   package ST renames CuBit.Startup_Tables;
   package HS renames CuBit.Handles.Startup;
   package Labels renames CuBit.ACPI_Hardware_Authority;
   use type CuBit.Handles.ABI.Kind;

   --  The port space's size (ports 0 .. 65535).
   Port_Space : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (Port_Count'Last);

   function Named (Label : String) return Grant is
      Empty  : constant Grant := (others => <>);
      Result : Grant := (others => <>);
   begin
      if CuBit.Startup_State.Granted (Label, ST.ACPI_Hardware) = ST.No_Handle then
         return Empty;
      end if;
      for K in Labels.IO_Window_Number loop
         declare
            Found : constant HS.Optional_IO_Ports :=
              HS.Optional_IO_Ports_Named (Labels.IO_Label (Label, K));
         begin
            exit when not Found.Present;
            declare
               What : constant CuBit.Handles.IPC.Description :=
                 CuBit.Handles.IPC.Describe (Found.Value);
            begin
               if not What.Current
                 or else What.Kind /= CuBit.Handles.ABI.IO_Ports
                 or else not (What.Held.Read and then What.Held.Write)
                 or else What.Object >= Port_Space
                 or else What.Parameter not in 1 .. Port_Space - What.Object
               then
                  return Empty;
               end if;
               Result.IO (K) :=
                 (Ports => Found.Value,
                  First => Port_Number (What.Object),
                  Count => Port_Count (What.Parameter));
               Result.IO_Used := K;
            end;
         end;
      end loop;
      --  The declared entry holds the first window's handle: a grant is
      --  never empty, and is all or nothing.
      if Result.IO_Used = 0 or else not Well_Formed (Result) then
         return Empty;
      end if;
      return Result;
   end Named;

end CuBit.ACPI_Windows.Launch;
