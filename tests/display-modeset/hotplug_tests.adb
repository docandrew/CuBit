--  DISP-009: the hot-plug interrupt sequencer against a register model
--  that records every access in order and models IIR (write one to clear),
--  the HOTPLUG_CTL status latches (write one to clear) and the master /
--  display enables; and the debounce state machine. Intel HPD itself is
--  modelled only by i915's register semantics (QEMU has no Intel HPD).
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Display_Registers;
with Intel_GPU_Port_Detection;
with Intel_GPU_Hotplug_IRQ;
with Intel_GPU_Hotplug_Debounce;
procedure Hotplug_Tests is
   package R renames Intel_GPU_Display_Registers;
   package PD renames Intel_GPU_Port_Detection;
   package DB renames Intel_GPU_Hotplug_Debounce;
   use type PD.Port;

   type Access_Kind is (Read_Access, Write_Access);
   type Access_Record is record
      Kind : Access_Kind := Read_Access;
      Offset : Unsigned_32 := 0;
      Value : Unsigned_32 := 0;
   end record;
   Log : array (1 .. 256) of Access_Record;
   Count : Natural := 0;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         raise Program_Error with "hotplug check failed: " & What;
      end if;
   end Check;

   Master_Register : constant := 16#19_0010#;
   IRQ_Master_Enable : constant Unsigned_32 := 16#8000_0000#;
   Display_Register : constant := 16#4_4200#;
   --  The modelled registers.
   Master, Display, SDE_IIR, SDE_IMR, SDE_IER, HPD_IIR, HPD_IMR, HPD_IER : Unsigned_32 := 0;
   DDI_CTL, TC_CTL, North_TC_CTL, North_TBT_CTL : Unsigned_32 := 0;
   --  Bits 1:0 of each 4-bit field are write-one-to-clear status latches.
   Latches : constant Unsigned_32 := 16#3333#;
   function Latched_Write (Old, Value : Unsigned_32) return Unsigned_32 is
     ((Value and not Latches) or (Old and Latches and not Value));

   --  Every other interrupt enable Quiesce writes (GT, pipes, DE port,
   --  misc, GU misc), modelled as plain registers; Stuck ignores writes.
   type Plain_Register is record
      Offset : Unsigned_32 := 0;
      Value : Unsigned_32 := 0;
   end record;
   Plain : array (1 .. 32) of Plain_Register;
   Plain_Used : Natural := 0;
   Stuck : Unsigned_32 := 0;
   function Plain_Slot (Offset : Unsigned_32) return Positive is
   begin
      for I in 1 .. Plain_Used loop
         if Plain (I).Offset = Offset then
            return I;
         end if;
      end loop;
      Plain_Used := Plain_Used + 1;
      Plain (Plain_Used) := (Offset, 0);
      return Plain_Used;
   end Plain_Slot;
   function Plain_Known (Offset : Unsigned_32) return Boolean is
     ((Offset in 16#19_0030# .. 16#19_0040# and then Offset mod 4 = 0) or else
      Offset in 16#4_440C# | 16#4_441C# | 16#4_442C# | 16#4_443C# | 16#4_444C# | 16#4_446C# | 16#4_44FC#);

   procedure Read (Offset : R.Register_Offset; Value : out Unsigned_32) is
   begin
      if Plain_Known (Offset) then
         Value := Plain (Plain_Slot (Offset)).Value;
         Count := Count + 1;
         Log (Count) := (Read_Access, Offset, Value);
         return;
      end if;
      case Offset is
         when Master_Register => Value := Master;
         when Display_Register => Value := Display;
         when R.SDEIIR => Value := SDE_IIR;
         when R.SDEIMR => Value := SDE_IMR;
         when R.SDEIER => Value := SDE_IER;
         when R.GEN11_DE_HPD_IIR => Value := HPD_IIR;
         when R.GEN11_DE_HPD_IMR => Value := HPD_IMR;
         when R.GEN11_DE_HPD_IER => Value := HPD_IER;
         when R.SHOTPLUG_CTL_DDI => Value := DDI_CTL;
         when R.SHOTPLUG_CTL_TC => Value := TC_CTL;
         when R.GEN11_TC_HOTPLUG_CTL => Value := North_TC_CTL;
         when R.GEN11_TBT_HOTPLUG_CTL => Value := North_TBT_CTL;
         when others => raise Program_Error with "read outside the HPD registers";
      end case;
      Count := Count + 1;
      Log (Count) := (Read_Access, Offset, Value);
   end Read;
   procedure Write (Offset : R.Register_Offset; Value : Unsigned_32) is
   begin
      Count := Count + 1;
      Log (Count) := (Write_Access, Offset, Value);
      if Plain_Known (Offset) then
         if Offset /= Stuck then
            Plain (Plain_Slot (Offset)).Value := Value;
         end if;
         return;
      end if;
      case Offset is
         --  GEN11_GFX_MSTR_IRQ: bit 31 is the enable; the rest report
         --  pending sources and are not changed by a write.
         when Master_Register => Master := (Master and not IRQ_Master_Enable) or (Value and IRQ_Master_Enable);
         when Display_Register => Display := Value;
         when R.SDEIIR => SDE_IIR := SDE_IIR and not Value;
         when R.SDEIMR => SDE_IMR := Value;
         when R.SDEIER => SDE_IER := Value;
         when R.GEN11_DE_HPD_IIR => HPD_IIR := HPD_IIR and not Value;
         when R.GEN11_DE_HPD_IMR => HPD_IMR := Value;
         when R.GEN11_DE_HPD_IER => HPD_IER := Value;
         when R.SHOTPLUG_CTL_DDI => DDI_CTL := Latched_Write (DDI_CTL, Value);
         when R.SHOTPLUG_CTL_TC => TC_CTL := Latched_Write (TC_CTL, Value);
         when R.GEN11_TC_HOTPLUG_CTL => North_TC_CTL := Latched_Write (North_TC_CTL, Value);
         when R.GEN11_TBT_HOTPLUG_CTL => North_TBT_CTL := Latched_Write (North_TBT_CTL, Value);
         when others => raise Program_Error with "write outside the HPD registers";
      end case;
   end Write;
   package IRQ is new Intel_GPU_Hotplug_IRQ (Read, Write);
   use type IRQ.Pulse, IRQ.Pulses;

   --  The position of the first (or last) access matching (0: none).
   function Find (Kind : Access_Kind; Offset : Unsigned_32) return Natural is
   begin
      for I in 1 .. Count loop
         if Log (I).Kind = Kind and then Log (I).Offset = Offset then
            return I;
         end if;
      end loop;
      return 0;
   end Find;
   function Last (Kind : Access_Kind; Offset : Unsigned_32) return Natural is
   begin
      for I in reverse 1 .. Count loop
         if Log (I).Kind = Kind and then Log (I).Offset = Offset then
            return I;
         end if;
      end loop;
      return 0;
   end Last;
   function Only (Port : PD.Port) return IRQ.Port_Set is ([for P in PD.Port => P = Port]);

   All_Ports : constant IRQ.Port_Set := [others => True];
   type Offsets is array (Positive range <>) of Unsigned_32;
   --  GT enables, pipe A and B IERs, DE port, misc, GU misc.
   Enables_Set : constant Offsets :=
     [16#19_0030#, 16#19_0034#, 16#19_0038#, 16#19_003C#, 16#19_0040#,
      16#4_440C#, 16#4_441C#, 16#4_444C#, 16#4_446C#, 16#4_44FC#];
   --  The same less pipe B, which is not powered in the test.
   Enables_Zeroed : constant Offsets :=
     [16#19_0030#, 16#19_0034#, 16#19_0038#, 16#19_003C#, 16#19_0040#,
      16#4_440C#, 16#4_444C#, 16#4_446C#, 16#4_44FC#];
   Fired : IRQ.Pulses;
   Unexpected : Boolean;
   State : DB.State := DB.Initial;
   Due, Ready : DB.Port_Set;
   Masked : Boolean;
   use type DB.Port_Set;
begin
   --  Arming: IIR cleared first, IER before IMR, only the HPD pins
   --  unmasked, the display and master enables last, in that order.
   SDE_IMR := Unsigned_32'Last;
   HPD_IMR := Unsigned_32'Last;
   SDE_IIR := 16#0100_0000#;
   IRQ.Arm (All_Ports);
   Check (SDE_IIR = 0 and then HPD_IIR = 0, "stale IIR cleared");
   Check (Find (Write_Access, R.SDEIIR) < Find (Write_Access, R.SDEIER) and then
          Last (Write_Access, R.SDEIER) < Find (Write_Access, R.SDEIMR), "IIR, IER, then IMR");
   Check (Last (Write_Access, R.GEN11_DE_HPD_IER) < Find (Write_Access, R.GEN11_DE_HPD_IMR), "DE HPD IER first");
   Check ((SDE_IMR and IRQ.South_Bits (All_Ports)) = 0 and then
          (SDE_IMR or IRQ.South_Bits (All_Ports)) = Unsigned_32'Last, "only the HPD pins unmasked (SDE)");
   Check ((HPD_IMR and IRQ.North_Bits (All_Ports)) = 0 and then
          (HPD_IMR or IRQ.North_Bits (All_Ports)) = Unsigned_32'Last, "only the HPD pins unmasked (DE)");
   Check (DDI_CTL = 16#88# and then TC_CTL = 16#8888# and then North_TC_CTL = 16#8888#, "detection enabled");
   Check (Last (Write_Access, Display_Register) < Last (Write_Access, Master_Register) and then
          Last (Write_Access, Master_Register) = Count and then Master = IRQ.Master_Enable, "master last");
   Check (IRQ.South_Bits (Only (PD.DDI_B)) = 16#2_0000# and then
          IRQ.North_Bits (Only (PD.DDI_TC2)) = 16#2_0002#, "pin bits");

   --  Quiesce: master off first, then every enable zero (GT, the powered
   --  pipes only, DE port, misc, GU misc, DE HPD, SDE); IMRs untouched.
   Count := 0;
   Master := IRQ.Master_Enable;
   Display := IRQ.Display_Enable;
   for Offset of Enables_Set loop
      Plain (Plain_Slot (Offset)).Value := 16#FFFF#;
   end loop;
   SDE_IER := 16#FFFF_FFFF#;
   HPD_IER := 16#FFFF_FFFF#;
   SDE_IMR := 16#1234#;
   declare
      Quiet : Boolean;
   begin
      IRQ.Quiesce ([R.A => True, R.B => False, R.C => False, R.D => False], Quiet);
      Check (Quiet and then Log (1) = (Write_Access, Master_Register, 0) and then Master = 0, "master first");
      for Offset of Enables_Zeroed loop
         Check (Plain (Plain_Slot (Offset)).Value = 0, "enable zeroed");
      end loop;
      Check (Plain (Plain_Slot (16#4_441C#)).Value = 16#FFFF# and then
             Find (Write_Access, 16#4_441C#) = 0, "an unpowered pipe is not touched");
      Check (SDE_IER = 0 and then HPD_IER = 0 and then Display = 0, "display enables zeroed");
      Check (SDE_IMR = 16#1234# and then Find (Write_Access, R.SDEIMR) = 0 and then
             Find (Write_Access, R.GEN11_DE_HPD_IMR) = 0, "IMRs untouched (polled IIRs still latch)");
      --  A register that does not take the write: not quiet.
      Stuck := 16#19_0038#;
      Plain (Plain_Slot (Stuck)).Value := 1;
      IRQ.Quiesce ([R.Transcoder => True], Quiet);
      Check (not Quiet, "a stuck enable is reported");
      Stuck := 0;
   end;

   --  An HDMI connect on DDI-B (long pulse), handled in i915 order.
   Count := 0;
   SDE_IIR := IRQ.South_Bits (Only (PD.DDI_B));
   DDI_CTL := DDI_CTL or Shift_Left (2, 4);    --  DDI-B field: long pulse
   Display := IRQ.Display_Enable or IRQ.Display_PCH;
   Master := IRQ.Master_Enable or IRQ.Master_Display;
   IRQ.Handle (Fired, Unexpected);
   Check (Log (1) = (Write_Access, Master_Register, 0), "master off first");
   Check (Find (Read_Access, Master_Register) = 2, "then read the master");
   Check (Find (Write_Access, Display_Register) < Find (Read_Access, R.SDEIIR), "display control off before IIR");
   Check (Find (Write_Access, R.SDEIIR) < Find (Write_Access, R.SHOTPLUG_CTL_DDI), "IIR acked before latches");
   Check (Last (Write_Access, Display_Register) > Find (Write_Access, R.SHOTPLUG_CTL_DDI) and then
          Log (Last (Write_Access, Display_Register)).Value = IRQ.Display_Enable, "display control re-armed");
   Check (Log (Count) = (Write_Access, Master_Register, IRQ.Master_Enable), "master re-armed last");
   Check (SDE_IIR = 0 and then (DDI_CTL and Latches) = 0, "acknowledged and latch cleared");
   Check (Fired = IRQ.Pulses'[PD.DDI_B => IRQ.Long_Pulse, others => IRQ.No_Pulse] and then not Unexpected,
          "DDI-B long pulse");
   Check (DDI_CTL = 16#88#, "detection still enabled");

   --  A Type-C DP-alt pulse comes through DE HPD; a Type-C legacy pulse
   --  through the PCH.
   Count := 0;
   HPD_IIR := IRQ.North_Bits (Only (PD.DDI_TC2)) and 16#FFFF_0000#;
   North_TC_CTL := North_TC_CTL or Shift_Left (1, 4);   --  TC2 short pulse
   SDE_IIR := IRQ.South_Bits (Only (PD.DDI_TC1));
   TC_CTL := TC_CTL or 2;   --  TC1 long
   Display := IRQ.Display_Enable or IRQ.Display_PCH or IRQ.Display_HPD;
   Master := IRQ.Master_Enable or IRQ.Master_Display;
   IRQ.Handle (Fired, Unexpected);
   Check (Fired = IRQ.Pulses'[PD.DDI_TC2 => IRQ.Short_Pulse, PD.DDI_TC1 => IRQ.Long_Pulse, others => IRQ.No_Pulse],
          "TC2 short (DP-alt), TC1 long (legacy)");
   Check (Find (Write_Access, R.GEN11_DE_HPD_IIR) < Find (Write_Access, R.GEN11_TC_HOTPLUG_CTL), "DE order");
   Check (HPD_IIR = 0 and then SDE_IIR = 0 and then (North_TC_CTL and Latches) = 0, "both acked");

   --  A spurious interrupt (no display bit): only the master is touched.
   Count := 0;
   Master := IRQ.Master_Enable;
   IRQ.Handle (Fired, Unexpected);
   Check (Count = 3 and then Fired = IRQ.Pulses'[others => IRQ.No_Pulse] and then not Unexpected, "spurious");
   --  A GT source pending (it should be masked): reported, not acknowledged.
   Count := 0;
   Master := IRQ.Master_Enable or 1;
   IRQ.Handle (Fired, Unexpected);
   Check (Unexpected and then Fired = IRQ.Pulses'[others => IRQ.No_Pulse] and then
          Log (Count) = (Write_Access, Master_Register, IRQ.Master_Enable), "unexpected GT source");
   --  A pipe source behind the display control: reported too.
   Count := 0;
   Master := IRQ.Master_Enable or IRQ.Master_Display;
   Display := IRQ.Display_Enable or 16#0001_0000#;
   IRQ.Handle (Fired, Unexpected);
   Check (Unexpected and then Display = IRQ.Display_Enable, "unexpected pipe source");

   --  Storm masking touches only that pin.
   SDE_IMR := 0;
   HPD_IMR := 0;
   IRQ.Mask (PD.DDI_TC3);
   Check (SDE_IMR = IRQ.South_Bits (Only (PD.DDI_TC3)) and then
          HPD_IMR = IRQ.North_Bits (Only (PD.DDI_TC3)), "mask one pin");
   IRQ.Unmask (PD.DDI_TC3);
   Check (SDE_IMR = 0 and then HPD_IMR = 0, "unmask");

   --  Debounce: a connector's bounce (pulses at 0, 20, 45 ms) gives one
   --  detection, Settle_Ms after the last pulse.
   DB.Pulse (State, PD.DDI_B, 1_000, Masked);
   DB.Pulse (State, PD.DDI_B, 1_020, Masked);
   DB.Pulse (State, PD.DDI_B, 1_045, Masked);
   Check (not Masked and then DB.Next_Wake (State) = 1_045 + DB.Settle_Ms, "settle after the last pulse");
   DB.Take_Due (State, 1_100, Due);
   Check (Due = DB.Port_Set'[PD.Port => False], "not yet");
   DB.Take_Due (State, 1_145, Due);
   Check (Due = DB.Port_Set'[PD.DDI_B => True, others => False], "one detection");
   DB.Take_Due (State, 1_200, Due);
   Check (Due = DB.Port_Set'[PD.Port => False] and then DB.Next_Wake (State) = DB.Never, "only once");
   --  Connected but the EDID did not read yet: one retry a second later.
   DB.Detected (State, PD.DDI_B, 1_145, Sink_Without_EDID => True);
   Check (DB.Next_Wake (State) = 1_145 + DB.Retry_Ms, "retry scheduled");
   DB.Take_Due (State, 2_145, Due);
   Check (Due (PD.DDI_B), "retry due");
   DB.Detected (State, PD.DDI_B, 2_145, Sink_Without_EDID => True);
   Check (DB.Next_Wake (State) = DB.Never, "a single retry");
   DB.Detected (State, PD.DDI_B, 2_200, Sink_Without_EDID => False);
   Check (State (PD.DDI_B).Retry_Left, "a settled result restores it");
   --  Two ports at once stay independent.
   DB.Pulse (State, PD.DDI_A, 5_000, Masked);
   DB.Pulse (State, PD.DDI_TC1, 5_050, Masked);
   DB.Take_Due (State, 5_100, Due);
   Check (Due = DB.Port_Set'[PD.DDI_A => True, others => False], "A first");
   DB.Take_Due (State, 5_150, Due);
   Check (Due = DB.Port_Set'[PD.DDI_TC1 => True, others => False], "then TC1");
   --  A storm: the pulse past the threshold within one period masks the
   --  pin; nothing is due until it is re-enabled two minutes later.
   for I in 1 .. DB.Storm_Threshold loop
      DB.Pulse (State, PD.DDI_TC2, 10_000 + DB.Milliseconds (I), Masked);
      Check (not Masked, "below the threshold");
   end loop;
   DB.Pulse (State, PD.DDI_TC2, 10_100, Masked);
   Check (Masked and then DB.Storming (State (PD.DDI_TC2)) and then State (PD.DDI_TC2).Due = DB.Never,
          "storm masks");
   DB.Pulse (State, PD.DDI_TC2, 10_200, Masked);
   Check (not Masked and then State (PD.DDI_TC2).Due = DB.Never, "a masked pin ignores pulses");
   DB.Take_Reenabled (State, 10_100 + DB.Storm_Reenable_Ms - 1, Ready);
   Check (Ready = DB.Port_Set'[PD.Port => False], "not before two minutes");
   DB.Take_Reenabled (State, 10_100 + DB.Storm_Reenable_Ms, Ready);
   Check (Ready = DB.Port_Set'[PD.DDI_TC2 => True, others => False] and then not DB.Storming (State (PD.DDI_TC2)),
          "re-enabled");
   --  Pulses spread over more than a period never storm.
   for I in 1 .. 3 * DB.Storm_Threshold loop
      DB.Pulse (State, PD.DDI_TC3, 200_000 + DB.Milliseconds (I) * 30, Masked);
      Check (not Masked, "slow pulses");
   end loop;
   Put_Line ("PASS hotplug:" & Checks'Image & " checks");
end Hotplug_Tests;
