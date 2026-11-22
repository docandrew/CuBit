pragma Ada_2022;

package body Platform_Quirks.Sideband is

   use type PP.Block_Status;
   use type P2SB.P2SB_Action;
   use type P2SB.Outcome;

   Not_Found : constant PP.Block := (Status => PP.Not_Placed, Base => 0, Bytes => 0);

   Found_By    : Window_Source := No_Window;
   The_Window  : PP.Block := (Status => PP.No_Source, Base => 0, Bytes => 0);
   Test_Community_Block : PP.Block := Not_Found;
   Reported    : Discovery_Report := (Ran => False, Outcome => P2SB.Absent);

   --  Read the P2SB the way Linux p2sb.c does (Intel_P2SB), recording every
   --  value read. On the Unhide_Read path the function is hidden again
   --  before this returns, and checked. Beyond Linux, two reads only
   --  diagnose: P2SBC read back after the unhide write (all-ones: the write
   --  did not make the function answer) and the unhidden device ID.
   function Observe return P2SB.Observation is
      Bus  : constant Unsigned_8 := P2SB.P2SB_Bus;
      Slot : constant Unsigned_8 := P2SB.P2SB_Slot;
      Func : constant Unsigned_8 := P2SB.P2SB_Function;
      Obs : P2SB.Observation;
   begin
      --  Step Identify.
      Obs.Class := Read_Config_16 (Bus, Slot, Func, P2SB.PCI_Class_Offset);
      Obs.Control := Read_Config_32 (Bus, Slot, Func, P2SB.P2SB_Control_Offset);
      Obs.Action := P2SB.Action_Of (Obs.Class, Obs.Control);
      case Obs.Action is
         when P2SB.Leave_Alone =>
            null;
         when P2SB.Read_Visible =>
            --  Step Read_BAR.
            Obs.BAR0 := Read_Config_32 (Bus, Slot, Func, P2SB.PCI_BAR0_Offset);
            Obs.BAR1 := Read_Config_32 (Bus, Slot, Func, P2SB.PCI_BAR1_Offset);
         when P2SB.Unhide_Read =>
            --  Step Unhide.
            Write_Config_32 (Bus, Slot, Func, P2SB.P2SB_Control_Offset, P2SB.P2SB_Unhidden);
            Obs.Control_Unhidden := Read_Config_32 (Bus, Slot, Func, P2SB.P2SB_Control_Offset);
            --  Step Confirm_Identity.
            Obs.Vendor_Unhidden := Read_Config_16 (Bus, Slot, Func, P2SB.PCI_Vendor_Offset);
            Obs.Device_Unhidden := Read_Config_16 (Bus, Slot, Func, P2SB.PCI_Device_Offset);
            Obs.Class_Unhidden := Read_Config_16 (Bus, Slot, Func, P2SB.PCI_Class_Offset);
            --  Step Read_BAR.
            Obs.BAR0 := Read_Config_32 (Bus, Slot, Func, P2SB.PCI_BAR0_Offset);
            Obs.BAR1 := Read_Config_32 (Bus, Slot, Func, P2SB.PCI_BAR1_Offset);
            --  Step Rehide: always, whatever the reads said (Linux too).
            Write_Config_32 (Bus, Slot, Func, P2SB.P2SB_Control_Offset, P2SB.P2SB_Hide);
            Obs.Vendor_After_Hide := Read_Config_16 (Bus, Slot, Func, P2SB.PCI_Vendor_Offset);
      end case;
      return Obs;
   end Observe;

   --  The one line a laptop log needs: the outcome, the step that decided
   --  it, and every value read.
   procedure Log (Obs : P2SB.Observation; O : P2SB.Outcome) is
      Line : constant String :=
        "devmgr: sideband: P2SB outcome=" & P2SB.Outcome_Name (O) &
        " step=" & P2SB.Step_Name (P2SB.Deciding_Step (O)) &
        " class=" & Hex (Unsigned_64 (Obs.Class)) &
        " p2sbc=" & Hex (Unsigned_64 (Obs.Control)) &
        " p2sbc-unhidden=" & Hex (Unsigned_64 (Obs.Control_Unhidden)) &
        " id=" & Hex (Unsigned_64 (Obs.Vendor_Unhidden)) & ":" &
        Hex (Unsigned_64 (Obs.Device_Unhidden)) &
        " class-unhidden=" & Hex (Unsigned_64 (Obs.Class_Unhidden)) &
        " bar0=" & Hex (Unsigned_64 (Obs.BAR0)) & " bar1=" & Hex (Unsigned_64 (Obs.BAR1)) &
        " vendor-after-hide=" & Hex (Unsigned_64 (Obs.Vendor_After_Hide));
   begin
      if O in P2SB.Found_Outcome then
         Inform (Line);
      else
         Warn (Line);
      end if;
      if P2SB.Still_Visible_After_Hide (Obs) then
         Warn ("devmgr: sideband: P2SB step=rehide: still visible after hiding it");
      end if;
   end Log;

   procedure Discover_Before_ACPI is
      Obs : constant P2SB.Observation := Observe;
      O   : constant P2SB.Outcome := P2SB.Classify (Obs);
      Found : constant PP.Block := P2SB.Window_Of (Obs);
   begin
      Reported := (Ran => True, Outcome => O);
      Log (Obs, O);
      Found_By := No_Window;
      The_Window := Found;
      if Found.Status = PP.Found then
         Found_By := P2SB_BAR;
         Inform ("devmgr: sideband: window " & Hex (Found.Base) & " +" & Hex (Found.Bytes) &
                 " (P2SB SBREG_BAR)");
      end if;
   end Discover_Before_ACPI;

   procedure Adopt_Test_Window (BAR_Base : Unsigned_64) is
   begin
      Found_By := No_Window;
      The_Window := GC.Test_Window (BAR_Base);
      Test_Community_Block := GC.Test_Community (BAR_Base);
      if The_Window.Status = PP.Found then
         Found_By := Test_Profile;
      end if;
   end Adopt_Test_Window;

   function Source return Window_Source is (Found_By);

   function Discovery return Discovery_Report is (Reported);

   function Window return PP.Block is
     (if Found_By = No_Window then
        (Status => (if The_Window.Status = PP.Found then PP.Not_Placed else The_Window.Status),
         Base => 0, Bytes => 0)
      else The_Window);

   function Port_Block (Port : GC.Port_ID) return PP.Block is
     (if Found_By = P2SB_BAR then GC.Port_Block (The_Window, Port) else Not_Found);

   function Community_Range_Count return Natural is
     (case Found_By is
        when No_Window    => 0,
        when P2SB_BAR     => GC.ADL_N_GPIO_Ports'Length,
        when Test_Profile => (if Test_Community_Block.Status = PP.Found then 1 else 0));

   function Community_Range (Index : Positive) return PP.Block is
     (case Found_By is
        when No_Window    => Not_Found,
        when P2SB_BAR     =>
          (Status => PP.Found,
           Base   => Port_Block (GC.ADL_N_GPIO_Ports (GC.ADL_N_GPIO_Ports'First + Index - 1)).Base,
           Bytes  => GC.GPIO_Community_Bytes),
        when Test_Profile => Test_Community_Block);

end Platform_Quirks.Sideband;
