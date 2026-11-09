--  DISP-009/010: display.svc's catalog of attached-but-not-lit outputs
--  (CuBit.Detected_Outputs). A GPU catalog with a Connected_Not_Lit output
--  puts it after the broker's own outputs without touching them; reading
--  the same GPU catalog again changes nothing; a disconnect removes it;
--  each connect and each disconnect changes the catalog exactly once (one
--  revision bump in display.svc). Lit and other sources' entries stay.
with Ada.Text_IO; use Ada.Text_IO;
with CuBit.Output_Discovery; use CuBit.Output_Discovery;
with CuBit.Detected_Outputs;
with Interfaces; use Interfaces;
procedure Detected_Tests is
   package Detected renames CuBit.Detected_Outputs;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         raise Program_Error with "detected outputs: " & What;
      end if;
   end Check;
   Panel : constant Description :=
     (Selected_For_Desktop, Intel_Display, 0, 1920, 1080, 344, 193, Lit, Embedded_DisplayPort, 1920, 1080);
   HDMI_Monitor : constant Description :=
     (Detected_Only, Intel_Display, 2, 3840, 2160, 600, 340, Connected_Not_Lit, HDMI);
   TC_Monitor : constant Description :=
     (Detected_Only, Intel_Display, 3, 1920, 1080, 527, 296, Connected_Not_Lit, DisplayPort);
   --  The GPU's own catalog: its lit output first (never copied as a
   --  detected one), then what is attached.
   function GPU (A, B : Boolean) return Catalog is
      Result : Catalog;
   begin
      Result.Count := 1;
      Result.Items (1) := (Backend_Ready, Intel_Display, 0, 1920, 1080, 344, 193, Lit, Embedded_DisplayPort,
                           1920, 1080);
      if A then
         Result.Count := Result.Count + 1;
         Result.Items (Result.Count) := HDMI_Monitor;
      end if;
      if B then
         Result.Count := Result.Count + 1;
         Result.Items (Result.Count) := TC_Monitor;
      end if;
      return Result;
   end GPU;
   Broker : Catalog;
   Changed : Boolean;
   Revision : Catalog_Revision := 1;
   --  display.svc's rule: one revision bump per changed replacement.
   procedure Event (Fresh : Catalog) is
   begin
      Detected.Replace (Broker, Intel_Display, Fresh, Changed);
      if Changed then
         Revision := Revision + 1;
      end if;
   end Event;
begin
   Broker.Count := 1;
   Broker.Items (1) := Panel;
   --  Boot without an external monitor.
   Event (GPU (False, False));
   Check (not Changed and then Broker.Count = 1 and then Revision = 1, "nothing attached");
   --  Connect: listed once, after the panel, unlit.
   Event (GPU (True, False));
   Check (Changed and then Revision = 2, "connect bumps once");
   Check (Broker.Count = 2 and then Broker.Items (1) = Panel and then Broker.Items (2) = HDMI_Monitor,
          "connect lists it after the panel");
   Check (Detected.Find (Broker, Intel_Display, 2) = 2 and then Detected.Find (Broker, Intel_Display, 0) = 0 and then
          Detected.Find (Broker, Virtio_GPU, 2) = 0, "find");
   --  The same state read again (a duplicate or debounced event): nothing.
   Event (GPU (True, False));
   Check (not Changed and then Revision = 2, "same state, no bump");
   --  A second monitor, then the first goes.
   Event (GPU (True, True));
   Check (Changed and then Revision = 3 and then Broker.Count = 3, "second connect");
   Event (GPU (False, True));
   Check (Changed and then Revision = 4 and then Broker.Count = 2 and then Broker.Items (2) = TC_Monitor,
          "disconnect of the first");
   --  Disconnect: gone once.
   Event (GPU (False, False));
   Check (Changed and then Revision = 5 and then Broker.Count = 1 and then Broker.Items (1) = Panel,
          "disconnect bumps once");
   Event (GPU (False, False));
   Check (not Changed and then Revision = 5, "no bump after");
   --  Another source's not-lit outputs are not this GPU's to replace.
   Broker.Count := 2;
   Broker.Items (2) := (TC_Monitor with delta Source => Virtio_GPU);
   Event (GPU (False, False));
   Check (not Changed and then Broker.Count = 2, "other source kept");
   --  A full catalog keeps the broker's entries and drops what does not fit.
   Broker.Count := Output_Count'Last - 1;
   for I in 1 .. Broker.Count loop
      Broker.Items (I) := Panel;
   end loop;
   Event (GPU (True, True));
   Check (Changed and then Broker.Count = Output_Count'Last and then
          Broker.Items (Broker.Count) = HDMI_Monitor, "full");
   Put_Line ("PASS detected outputs:" & Checks'Image & " checks");
end Detected_Tests;
