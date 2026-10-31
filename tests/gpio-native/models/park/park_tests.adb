pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Interfaces;
with Intel_ADLN_GPIO;
with Intel_GPIO_IRQ;
with GPIO_Community_Types;
with GPIO_Community_Park;
procedure Park_Tests is
   package G renames Intel_ADLN_GPIO;
   package I renames Intel_GPIO_IRQ;
   package T renames GPIO_Community_Types;
   use type G.ACPI_Pin, G.Outcome, G.Word, G.Memory_Resource_Ordinal;
   use type I.Access_Status, I.Read_ID, I.Write_ID, I.Tick;
   type Register_Array is array (Natural range 0 .. 2047) of I.Word;
   type Communities is array (G.Memory_Resource_Ordinal) of Register_Array;
   Memory, Before : Communities := [others => [others => 0]];
   Checks, Accesses, Writes, Configuration_Writes, Clock_Samples : Natural := 0;
   Case_Name : constant String := Ada.Command_Line.Argument (1);
   Clock_Value : I.Tick := 1;
   Clock_OK : Boolean := True;
   Inject_At : Natural := 0;
   Inject_Status : I.Access_Status := I.Access_OK;
   Lie_Readback, Wrong_Plan : Boolean := False;
   procedure Check (OK : Boolean; Why : String) is
   begin
      Checks := Checks + 1; if not OK then raise Program_Error with Why; end if;
   end Check;
   function Available (Resource : G.Memory_Resource_Ordinal) return Boolean is (Case_Name /= "no-resources" and then Resource /= 2);
   procedure Admit (Pin : G.ACPI_Pin; Result : out T.Pin_Admission; Plan : out G.Plan_Result) is
   begin
      Plan := G.Plan (G.Target_HID, (if Wrong_Plan then 161 else Pin), 16#0094_0000#, 16#1000#, 8192);
      Result := T.Admitted;
   end Admit;
   procedure Clock (Now : out I.Tick; Present : out Boolean) is
   begin Clock_Samples := Clock_Samples + 1; Now := Clock_Value; Present := Clock_OK; end Clock;
   procedure Read (Plan : G.Plan_Result; ID : I.Read_ID; Value : out I.Word; Status : out I.Access_Status) is
   begin
      Check (Plan.Status = G.Ready and then Available (Plan.Selected.Resource), "only admitted present resource");
      Accesses := Accesses + 1;
      Value := Memory (Plan.Selected.Resource) (Natural (I.Read_Offset (Plan, ID)) / 4);
      if Lie_Readback and then ID = I.Group_Enable then Value := Value or Plan.Selected.Interrupt_Mask; end if;
      Status := (if Accesses = Inject_At then Inject_Status else I.Access_OK);
   end Read;
   procedure Write (Plan : G.Plan_Result; ID : I.Write_ID; Value : I.Word; Status : out I.Access_Status) is
      Offset : constant Natural := Natural (I.Write_Offset (Plan, ID)) / 4;
      Resource : constant G.Memory_Resource_Ordinal := Plan.Selected.Resource;
      Owner_Value : constant I.Word := Memory (Resource) (Natural (Plan.Selected.Owner_Register) / 4);
      Host_Value : constant I.Word := Memory (Resource) (Natural (Plan.Selected.Host_Register) / 4);
   begin
      Check (G.Interrupt_Eligible (Plan.Selected, Owner_Value, Host_Value, 0), "writes only observed host-owned GPIO");
      Check (ID in I.Write_Enable | I.Clear_Status, "parking never changes pad configuration");
      Accesses := Accesses + 1; Writes := Writes + 1;
      if ID = I.Write_Configuration then Configuration_Writes := Configuration_Writes + 1; end if;
      Status := (if Accesses = Inject_At then Inject_Status else I.Access_OK);
      if Status in I.Access_OK | I.Unknown_Effect then
         if ID = I.Clear_Status then
            Check (Value = Plan.Selected.Interrupt_Mask, "W1C exact selected host-owned bit only");
            Memory (Resource) (Offset) := Memory (Resource) (Offset) and not Value;
         else
            Check (Value = (Memory (Resource) (Offset) and not Plan.Selected.Interrupt_Mask),
               "RMW preserves every other enable bit");
            Memory (Resource) (Offset) := Value;
         end if;
      end if;
   end Write;
   package Owner is new GPIO_Community_Park (Available, Admit, Clock, Read, Write);
   use type Owner.Phase, Owner.Failure, Owner.Work_Count;
   S : Owner.State;
   OK : Boolean;
   procedure Drive is
   begin
      for N in 1 .. 4096 loop
         declare Previous : constant Natural := Accesses; begin
            Owner.Step (S); Check (Accesses <= Previous + 1, "one access per bounded step");
         end;
         exit when Owner.Observe (S).At_Phase in Owner.Ready | Owner.Failed | Owner.Cancelled;
      end loop;
   end Drive;
   Starts : constant array (Positive range 1 .. 12) of G.ACPI_Pin :=
     [0, 32, 64, 96, 128, 160, 192, 224, 256, 288, 320, 352];
   Sizes : constant array (Positive range 1 .. 12) of Positive :=
     [26, 16, 25, 8, 20, 24, 21, 29, 24, 25, 25, 8];
   Expected_Total, Expected_Mapped, Expected_Host : Natural := 0;
begin
   Check (Case_Name in "happy" | "budget" | "expired" | "clock-loss" | "clock-reverse" |
      "wrong-plan" | "readback" | "cancel-before" | "cancel-after" | "deny-read" | "unknown-write" | "no-resources" | "fault", "known case");
   declare Group : Positive := Starts'First; begin
      for From in G.ACPI_Pin loop
         while Group <= Starts'Last and then From >= Starts (Group) + G.ACPI_Pin (Sizes (Group)) loop Group := Group + 1; end loop;
         declare Found : constant G.Pin_Search := G.First_At_Or_After (From); begin
            Check (Found.Found = (Group <= Starts'Last), "enumerator complete at every 16-bit starting point");
            if Found.Found then
               Check (Found.Pin = (if From < Starts (Group) then Starts (Group) else From), "first actual mapped GPIO skips every gap");
            end if;
         end;
      end loop;
   end;
   for Group in Starts'Range loop
      Expected_Total := Expected_Total + Sizes (Group);
      for N in 0 .. Sizes (Group) - 1 loop
         declare
            Pin : constant G.ACPI_Pin := Starts (Group) + G.ACPI_Pin (N);
            L : constant G.Location := G.Locate (Pin);
            Resource : constant G.Memory_Resource_Ordinal := L.Resource;
         begin
            Memory (Resource) (Natural (L.Host_Register) / 4) := 16#AAAA_AAAA#;
            Memory (Resource) (Natural (L.Enable_Register) / 4) := I.Word'Last;
            Memory (Resource) (Natural (L.Status_Register) / 4) := I.Word'Last;
            if N mod 3 = 0 then
               Memory (Resource) (Natural (L.Owner_Register) / 4) :=
                 Memory (Resource) (Natural (L.Owner_Register) / 4) or
                 Interfaces.Shift_Left (I.Word'(1), Natural (L.Owner_Field_Shift));
            end if;
         end;
      end loop;
   end loop;
   Before := Memory;
   for Group in Starts'Range loop
      for N in 0 .. Sizes (Group) - 1 loop
         declare L : constant G.Location := G.Locate (Starts (Group) + G.ACPI_Pin (N)); begin
            if Available (L.Resource) then
               Expected_Mapped := Expected_Mapped + 1;
               if G.Interrupt_Eligible (L, Memory (L.Resource) (Natural (L.Owner_Register) / 4),
                    Memory (L.Resource) (Natural (L.Host_Register) / 4), 0)
               then Expected_Host := Expected_Host + 1; end if;
            end if;
         end;
      end loop;
   end loop;
   if Case_Name = "fault" then
      Inject_At := Positive'Value (Ada.Command_Line.Argument (2));
      Inject_Status := I.Access_Status'Value (Ada.Command_Line.Argument (3));
      Check (Inject_At <= 9 and then Inject_Status /= I.Access_OK, "explicit callback fault position and status");
   elsif Case_Name = "wrong-plan" then Wrong_Plan := True;
   elsif Case_Name = "readback" then Lie_Readback := True;
   elsif Case_Name = "deny-read" then Inject_At := 1; Inject_Status := I.Denied;
   elsif Case_Name = "unknown-write" then Inject_At := 6; Inject_Status := I.Unknown_Effect;
   elsif Case_Name = "clock-loss" then Clock_OK := False; end if;
   if Case_Name = "cancel-before" then Owner.Request_Stop (S); end if;
   Owner.Start (S, (if Case_Name = "expired" then 1 else 1000), (if Case_Name = "budget" then 1 else 4096), OK);
   Check (OK, "single parked startup accepted");
   if Case_Name in "cancel-after" | "clock-reverse" then
      Owner.Step (S); Owner.Step (S);
      if Case_Name = "cancel-after" then Owner.Request_Stop (S); else Clock_Value := 0; end if;
   end if;
   Drive;
   Check (Natural (Owner.Observe (S).Starting_Budget) = (if Case_Name = "budget" then 1 else 4096),
      "starting work allowance is retained exactly");
   Check (Natural (Owner.Observe (S).Consumed_Budget) = Clock_Samples,
      "each consumed work unit corresponds to its actual clock-gated step");
   Check (Owner.Observe (S).Consumed_Budget <= Owner.Observe (S).Starting_Budget,
      "consumed work never exceeds the initial allowance");
   if Case_Name = "happy" then
      Check (Owner.Observe (S).At_Phase = Owner.Ready, "all admitted communities parked");
      Check (Natural (Owner.Observe (S).Visited) = Expected_Total and then
         Natural (Owner.Observe (S).Unmapped) = Expected_Total - Expected_Mapped and then
         Natural (Owner.Observe (S).Parked) = Expected_Host and then
         Natural (Owner.Observe (S).Firmware_Owned) = Expected_Mapped - Expected_Host, "exact complete coverage and ownership partitions");
      for Resource in G.Memory_Resource_Ordinal loop
         for Offset in Register_Array'Range loop
            declare Expected : I.Word := Before (Resource) (Offset); begin
               for Group in Starts'Range loop
                  for N in 0 .. Sizes (Group) - 1 loop
                     declare L : constant G.Location := G.Locate (Starts (Group) + G.ACPI_Pin (N)); begin
                        if L.Resource = Resource and then Available (Resource) and then
                           G.Interrupt_Eligible (L, Before (Resource) (Natural (L.Owner_Register) / 4),
                             Before (Resource) (Natural (L.Host_Register) / 4), 0) and then
                           (Offset = Natural (L.Enable_Register) / 4 or else Offset = Natural (L.Status_Register) / 4)
                        then Expected := Expected and not L.Interrupt_Mask; end if;
                     end;
                  end loop;
               end loop;
               Check (Memory (Resource) (Offset) = Expected, "exact complete memory after-image preserves firmware/gaps/configuration");
            end;
         end loop;
      end loop;
      Check (Writes = 2 * Expected_Host and then Configuration_Writes = 0, "exact mask and W1C effect count");
   else
      Check (Owner.Observe (S).At_Phase = (if Case_Name in "cancel-before" | "cancel-after" then Owner.Cancelled else Owner.Failed), "refusal never becomes readiness");
      if Case_Name = "fault" then
         Check (Accesses = Inject_At and then Owner.Observe (S).Last_Status = Inject_Status,
           "fault stops at exact callback without later access");
         Check (Owner.Observe (S).Reason = (if Inject_Status = I.Unknown_Effect then
           Owner.Register_Indeterminate else Owner.Register_Refused), "typed callback outcome retained");
         Check (Owner.Observe (S).Last_Access_Phase =
           (case Inject_At is when 1 | 3 => Owner.Read_Owner, when 2 | 4 => Owner.Read_Host,
              when 5 => Owner.Read_Enable, when 6 => Owner.Mask_Write, when 7 => Owner.Mask_Confirm,
              when 8 => Owner.Clear_Status, when others => Owner.Clear_Confirm),
           "failure identifies exact attempted register phase");
      end if;
      if Case_Name = "unknown-write" then
         Check (Owner.Observe (S).Reason = Owner.Register_Indeterminate and then Owner.Observe (S).Effects_Possible,
           "unknown write keeps possible effect and stops");
      end if;
   end if;
   declare Count : constant Natural := Accesses; begin
      for N in 1 .. 16 loop Owner.Step (S); end loop;
      Owner.Start (S, 2000, 4096, OK);
      Check (not OK and then Accesses = Count, "terminal startup cannot replay or restart");
   end;
   Ada.Text_IO.Put_Line ("GPIO community parking checks" & Checks'Image);
end Park_Tests;
