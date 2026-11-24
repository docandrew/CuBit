pragma Ada_2022;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with CCL.Manifests;
with ACPI_CMOS_Manifest_Windows;
with ACPI_CMOS_Handover_Protocol;
with CCL.Resource_Sections;
with CCL.Device_Sections;
with CuBit.Startup_Tables;
procedure Manifest_Sections is
   package IO renames Ada.Streams.Stream_IO;
   use type Ada.Streams.Stream_Element_Offset;
   function Read (Path : String) return String is
      F : IO.File_Type;
   begin
      IO.Open (F, IO.In_File, Path);
      declare
         Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (F)));
         Last : Ada.Streams.Stream_Element_Offset;
         Text : String (1 .. Raw'Length);
      begin
         IO.Read (F, Raw, Last); IO.Close (F);
         if Last /= Raw'Last then raise Program_Error with "short read"; end if;
         for I in Text'Range loop Text (I) := Character'Val (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Text;
      end;
   end Read;
   procedure Write (Name : String; Item : CCL.Manifests.Section) is
      F : IO.File_Type;
      Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Item.Length));
   begin
      for I in Raw'Range loop Raw (I) := Ada.Streams.Stream_Element (Item.Data (Natural (I))); end loop;
      IO.Create (F, IO.Out_File, Ada.Command_Line.Argument (4) & "/" & Name);
      IO.Write (F, Raw); IO.Close (F);
      Ada.Text_IO.Put_Line (Name & Item.Length'Image);
   end Write;
   Result : CCL.Manifests.Compilation_Result;
begin
   CCL.Manifests.Compile (Read (Ada.Command_Line.Argument (1)), Read (Ada.Command_Line.Argument (2)), Result,
                          Read (Ada.Command_Line.Argument (3)));
   if not Result.Success then raise Program_Error with "actual CCL manifest compiler refusal " & Result.Diagnostic'Image & " at" & Result.Position'Image & " expression=" & Result.Expression_Diagnostic'Image; end if;
   Write ("caps.bin", Result.Capabilities); Write ("resources.bin", Result.Resources);
   Write ("devices.bin", Result.Devices); Write ("startup.bin", Result.Startup);
   declare
      package P renames ACPI_CMOS_Manifest_Windows;
      package ST renames CuBit.Startup_Tables;
      package RS renames CCL.Resource_Sections;
      package DS renames CCL.Device_Sections;
      package W renames ACPI_CMOS_Handover_Protocol;
      use type ST.Decode_Result, P.Selection_Status, P.Window_Kind;
      use type W.Profile;
      Raw : ST.Bytes (1 .. Result.Startup.Length);
      Resources : RS.Byte_Array (1 .. Result.Resources.Length);
      Devices : DS.Byte_Array (1 .. Result.Devices.Length);
      Declared : ST.Table; Status : ST.Decode_Result;
      Checks : Natural := 0;
      procedure Check (OK : Boolean) is
      begin Checks := Checks + 1; if not OK then raise Program_Error with "typed CMOS window check" & Checks'Image; end if; end Check;
      function Pick (D : ST.Table; R : RS.Byte_Array; M : DS.Byte_Array;
                     Profile : W.Profile := W.Legacy_64) return P.Selection is
        (P.Select_Window (P.Inspect (D,R,M),True,True,True,(Profile,False)));
      Positive : P.Selection;
   begin
      for I in Raw'Range loop Raw (I) := Result.Startup.Data (I); end loop;
      for I in Resources'Range loop Resources (I) := Result.Resources.Data (I); end loop;
      for I in Devices'Range loop Devices (I) := Result.Devices.Data (I); end loop;
      ST.Decode (Raw,Declared,Status); Check (Status = ST.Decoded and then ST.Declaration (Declared));
      Positive := Pick (Declared,Resources,Devices); Check (Positive.Status = P.Admitted and Positive.Binding = 5 and Positive.Ports = 2);
      for Profile in W.Profile loop
         for Approved in Boolean loop
            for Known in Boolean loop
               for Free in Boolean loop
                  for NMI in Boolean loop
                     declare
                        S : constant P.Selection := P.Select_Window
                          (P.Inspect (Declared,Resources,Devices),Approved,Known,Free,(Profile,NMI));
                        Expected : constant Boolean := Approved and Known and Free and Profile /= W.Unidentified;
                     begin
                        Check ((S.Status = P.Admitted) = Expected);
                        if Expected then Check (S.Kind = P.System_CMOS_Window and S.Binding = 5 and S.Ports = Natural (W.Port_Count (Profile)));
                        else Check (S.Kind = P.No_Window and S.Binding = 0 and S.Ports = 0); end if;
                     end;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
      -- Actual compiler bytes: reject every wrong binding geometry/class.
      for Probe in 1 .. 13 loop
         declare R : RS.Byte_Array := Resources; M : DS.Byte_Array := Devices; D : ST.Table := Declared; begin
            case Probe is
               when 1 => R (26) := 1; -- readonly resource, not selector/data grant
               when 2 => R (25) := 16; -- memory instead of ports
               when 3 => R (29) := 1; -- wrong platform resource index
               when 4 => R (33) := 1; -- too small
               when 5 => R (33) := 3; -- untyped size
               when 6 => R (33) := 5; -- too wide
               when 7 => R (41) := 1; -- extra payload
               when 8 => R (11) := 1; -- wrong resource platform
               when 9 => R (7) := 2; -- count/length disagreement
               when 10 => R (1) := 0; -- malformed wire header
               when 11 => M (13) := 1; -- wrong devices platform match
               when 12 => D.Entries (4).Kind := ST.Endpoint; -- no ACPI hardware route
               when 13 => D.Entries (5).Label := ST.To_Name ("other");
            end case;
            Check (Pick (D,R,M).Status /= P.Admitted);
         end;
      end loop;
      declare R : RS.Byte_Array := Resources; begin
         R (33) := 2;
         Check (Pick (Declared,R,Devices,W.Intel_256).Status = P.Receipt_Unavailable);
         Check (Pick (Declared,R,Devices,W.Intel_128).Status = P.Admitted);
         Check (Pick (Declared,R,Devices,W.Legacy_64).Status = P.Admitted);
      end;
      Check (Pick (Declared,Resources (1 .. 0),Devices).Status = P.Malformed_Declaration);
      Check (Pick (Declared,Resources,Devices (1 .. 0)).Status = P.Malformed_Declaration);
      Ada.Text_IO.Put_Line ("CMOS_MANIFEST_WINDOW_CHECKS" & Checks'Image & " PASS");
   end;
end Manifest_Sections;
