with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Devices; use Intel_GPU_ADLN_Devices;
with Intel_GPU_Probe;
with Intel_GPU_Device_Query; use Intel_GPU_Device_Query;
with Intel_GPU_ADLN_Inventory;
with Intel_GPU_Memory_Admission;
with Intel_GPU_Media_Engines;
with Intel_GPU_Bring_Up;
--  HW-002 regression: every gate that pinned the N95 NUC's 8086:46D2 now
--  accepts the ADL-N/Twin Lake family the manifest declares (46D0..46D4;
--  the i3-N350 IdeaPad is 46D4) and still refuses its neighbours and other
--  vendors. Before HW-002 the device query, the engine inventory, memory
--  admission and media engines refused 46D4. Hosted logic only: no
--  hardware evidence for any device but 46D2.
procedure ADLN_Family_Tests is
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Label : String) is
   begin
      if not Condition then
         Put_Line ("FAIL: " & Label);
         raise Program_Error;
      end if;
      Checks := Checks + 1;
   end Check;

   Laptop_Device : constant Unsigned_16 := 16#46D4#;   --  IdeaPad Slim 3i, i3-N350
   NUC_Device    : constant Unsigned_16 := 16#46D2#;   --  N95 NUC
   Display_Class : constant Unsigned_8 := 16#03#;
   Revision      : constant Unsigned_8 := 16#11#;
   Outside : constant array (1 .. 4) of Unsigned_16 := [16#46CF#, 16#46D5#, 16#5916#, 16#0000#];
   Other_Vendor : constant Unsigned_16 := 16#1002#;
   No_Media_Fused_Off : constant Unsigned_32 := 0;

   function Identity_Word (Device : Unsigned_16) return Unsigned_64 is
     (Shift_Left (Unsigned_64 (Revision), 32) or Shift_Left (Unsigned_64 (Device), 16)
      or Unsigned_64 (Intel_Vendor));
   function Query (Device : Unsigned_16) return Words is
     (Respond ((Device, Revision, True, 1, 16#FFFF#), Label, 4, 0, 0, [Version, Identity, 0, 0]));
   function Live (Device : Unsigned_16) return Boolean is
     (Intel_GPU_Memory_Admission.Live
        ((Device => Device, Runtime_Admitted => True, Owner_Held => True,
          Session_Healthy => True, Faulted => False, others => <>)));
begin
   Check (Facts (NUC_Device).GT = GT0_5, "46D2 is GT0.5 (Mesa adl_gt05)");
   Check (Facts (Laptop_Device).GT = GT1, "46D4 is GT1 (Mesa adl_gt1)");
   for Device in Device_ID loop
      declare
         Name : constant String := Unsigned_16'Image (Device);
      begin
         Check (Supported (Intel_Vendor, Device), Name & ": supported");
         Check (Supported_Identity (Shift_Left (Unsigned_32 (Device), 16) or Unsigned_32 (Intel_Vendor)),
                Name & ": supported identity word");
         Check (Intel_GPU_Probe."=" (Intel_GPU_Probe.Identify (Intel_Vendor, Device, Display_Class),
                                     Intel_GPU_Probe.Alder_Lake_N), Name & ": probe identifies ADL-N");
         Check (Query (Device) = [OK, Version, Identity_Word (Device), 0], Name & ": device query answers");
         Check (Intel_GPU_ADLN_Inventory.Decode (Intel_Vendor, Device, No_Media_Fused_Off).Valid,
                Name & ": engine inventory decodes");
         Check (Live (Device), Name & ": memory admission live");
         Check (Intel_GPU_Media_Engines.Identify (Intel_Vendor, Device).Known, Name & ": media engines known");
      end;
   end loop;
   for Device of Outside loop
      declare
         Name : constant String := Unsigned_16'Image (Device);
      begin
         Check (not Supported (Intel_Vendor, Device), Name & ": not supported");
         Check (Query (Device) = [Unavailable, Version, 0, 0], Name & ": device query refuses");
         Check (not Intel_GPU_ADLN_Inventory.Decode (Intel_Vendor, Device, No_Media_Fused_Off).Valid,
                Name & ": no engine inventory");
         Check (not Live (Device), Name & ": memory admission refuses");
         Check (not Intel_GPU_Media_Engines.Identify (Intel_Vendor, Device).Known, Name & ": media engines unknown");
      end;
   end loop;
   Check (not Supported (Other_Vendor, Laptop_Device), "another vendor's 46D4 is not supported");
   Check (not Intel_GPU_ADLN_Inventory.Decode (Other_Vendor, Laptop_Device, No_Media_Fused_Off).Valid,
          "another vendor's 46D4 has no inventory");
   --  The bring-up stage lines and summary (the logging convention).
   declare
      package B renames Intel_GPU_Bring_Up;
      use type B.Stage;
   begin
      Check (B.Summary = "intel-gpu: bring-up reached nothing (0/9)", "nothing reported");
      Check (B.Line (B.Identify, True, "device=8086:46D4") =
               "intel-gpu: stage identify ok device=8086:46D4", "stage line");
      B.Report (B.Identify, True);
      B.Report (B.Registers, True);
      B.Report (B.Forcewake, False);
      Check (B.Next = B.Forcewake, "next is the failed stage");
      Check (B.Summary = "intel-gpu: bring-up reached registers (2/9)", "summary after a failure");
      for S in B.Stage loop
         B.Report (S, True);
      end loop;
      Check (B.Complete and then B.Summary = "intel-gpu: bring-up complete", "complete");
      Check (B.Line (B.Render, False, "") = "intel-gpu: stage render fail", "fail line");
   end;
   Put_Line ("adln family tests:" & Natural'Image (Checks) & " checks passed");
end ADLN_Family_Tests;
