pragma Ada_2022;

package body Devmgr_Sideband is

   use type PP.Block_Status;
   use type SL.P2SB_Action;

   PCI_Vendor_Offset : constant := 16#00#;

   Not_Found : constant PP.Block := (Status => PP.Not_Placed, Base => 0, Bytes => 0);

   Found_By    : Window_Source := No_Window;
   The_Window  : PP.Block := (Status => PP.No_Source, Base => 0, Bytes => 0);
   Test_Community_Block : PP.Block := Not_Found;

   --  The P2SB's SBREG_BAR: Linux p2sb.c's protocol. The function is
   --  hidden again before this returns, and checked.
   function Read_SBREG return PP.Block is
      Bus  : constant Unsigned_8 := SL.P2SB_Bus;
      Slot : constant Unsigned_8 := SL.P2SB_Slot;
      Func : constant Unsigned_8 := SL.P2SB_Function;
      Class   : constant Unsigned_16 := Read_Config_16 (Bus, Slot, Func, SL.PCI_Class_Offset);
      Control : constant Unsigned_32 := Read_Config_32 (Bus, Slot, Func, SL.P2SB_Control_Offset);
      Action  : constant SL.P2SB_Action := SL.Action_Of (Class, Control);
      BAR0, BAR1 : Unsigned_32 := 0;
      Seen_Vendor, Seen_Class, After_Vendor : Unsigned_16 := 0;
   begin
      case Action is
         when SL.Leave_Alone =>
            Warn ("devmgr: sideband: 00:1F.1 class " & Hex (Unsigned_64 (Class)) &
                  " is not a P2SB; left alone");
            return Not_Found;
         when SL.Read_Visible =>
            BAR0 := Read_Config_32 (Bus, Slot, Func, SL.PCI_BAR0_Offset);
            BAR1 := Read_Config_32 (Bus, Slot, Func, SL.PCI_BAR1_Offset);
         when SL.Unhide_Read =>
            Write_Config_32 (Bus, Slot, Func, SL.P2SB_Control_Offset, 0);
            Seen_Vendor := Read_Config_16 (Bus, Slot, Func, PCI_Vendor_Offset);
            Seen_Class := Read_Config_16 (Bus, Slot, Func, SL.PCI_Class_Offset);
            BAR0 := Read_Config_32 (Bus, Slot, Func, SL.PCI_BAR0_Offset);
            BAR1 := Read_Config_32 (Bus, Slot, Func, SL.PCI_BAR1_Offset);
            Write_Config_32 (Bus, Slot, Func, SL.P2SB_Control_Offset, SL.P2SB_Hide);
            After_Vendor := Read_Config_16 (Bus, Slot, Func, PCI_Vendor_Offset);
            Inform ("devmgr: sideband: P2SB unhidden for its BAR (vendor " &
                    Hex (Unsigned_64 (Seen_Vendor)) & " class " &
                    Hex (Unsigned_64 (Seen_Class)) & "), hidden again (vendor " &
                    Hex (Unsigned_64 (After_Vendor)) & ")");
            if After_Vendor /= SL.PCI_Absent then
               Warn ("devmgr: sideband: P2SB still visible after hiding it");
            end if;
            if Seen_Vendor /= PP.Intel_Vendor or else Seen_Class /= SL.PCI_Class_Memory_Other then
               Warn ("devmgr: sideband: P2SB did not appear when unhidden; window left out");
               return Not_Found;
            end if;
      end case;
      return SL.Decode_SBREG (BAR0, BAR1);
   end Read_SBREG;

   procedure Discover_Before_ACPI is
      Found : constant PP.Block := Read_SBREG;
   begin
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
      The_Window := SL.Test_Window (BAR_Base);
      Test_Community_Block := SL.Test_Community (BAR_Base);
      if The_Window.Status = PP.Found then
         Found_By := Test_Profile;
      end if;
   end Adopt_Test_Window;

   function Source return Window_Source is (Found_By);

   function Window return PP.Block is
     (if Found_By = No_Window then
        (Status => (if The_Window.Status = PP.Found then PP.Not_Placed else The_Window.Status),
         Base => 0, Bytes => 0)
      else The_Window);

   function Port_Block (Port : SL.Port_ID) return PP.Block is
     (if Found_By = P2SB_BAR then SL.Port_Block (The_Window, Port) else Not_Found);

   function Community_Range_Count return Natural is
     (case Found_By is
        when No_Window    => 0,
        when P2SB_BAR     => SL.ADL_N_GPIO_Ports'Length,
        when Test_Profile => (if Test_Community_Block.Status = PP.Found then 1 else 0));

   function Community_Range (Index : Positive) return PP.Block is
     (case Found_By is
        when No_Window    => Not_Found,
        when P2SB_BAR     =>
          (Status => PP.Found,
           Base   => Port_Block (SL.ADL_N_GPIO_Ports (SL.ADL_N_GPIO_Ports'First + Index - 1)).Base,
           Bytes  => SL.GPIO_Community_Bytes),
        when Test_Profile => Test_Community_Block);

end Devmgr_Sideband;
