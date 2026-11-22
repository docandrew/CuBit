pragma Ada_2022;

package body Platform_Quirks.Intel_P2SB with SPARK_Mode is

   function Decode_SBREG (BAR0, BAR1 : Unsigned_32) return Block is
      Kind : constant Unsigned_32 := BAR0 and PCI_BAR_Type_Mask;
      Low  : constant Unsigned_64 := Unsigned_64 (BAR0 and not PCI_BAR_Flag_Mask);
      Base : Unsigned_64;
   begin
      if not BAR_Is_Memory (BAR0) then
         return (Status => Not_Placed, Base => 0, Bytes => 0);
      end if;
      Base := (if Kind = PCI_BAR_Type_64 then Shift_Left (Unsigned_64 (BAR1), 32) or Low else Low);
      if Base = 0 or else Base mod SBREG_Bytes /= 0 then
         return (Status => Not_Placed, Base => 0, Bytes => 0);
      elsif Base >= Byte_Limit or else SBREG_Bytes > Byte_Limit - Base then
         return (Status => Beyond_Limit, Base => Base, Bytes => SBREG_Bytes);
      end if;
      return (Status => Found, Base => Base, Bytes => SBREG_Bytes);
   end Decode_SBREG;

   function Classify (Obs : Observation) return Outcome is
      Window : Block;
   begin
      case Obs.Action is
         when Leave_Alone =>
            return (if Obs.Class = PCI_Absent then Absent else Other_Function);
         when Unhide_Read =>
            if Obs.Control_Unhidden = PCI_Absent_32 and then Obs.Vendor_Unhidden = PCI_Absent then
               return Unhide_Ignored;
            elsif Obs.Vendor_Unhidden /= Intel_Vendor
              or else Obs.Class_Unhidden /= PCI_Class_Memory_Other
            then
               return Unhidden_Not_P2SB;
            end if;
         when Read_Visible =>
            null;
      end case;
      if not BAR_Is_Memory (Obs.BAR0) then
         return BAR_Not_Memory;
      end if;
      Window := Decode_SBREG (Obs.BAR0, Obs.BAR1);
      case Window.Status is
         when Found =>
            return (if Obs.Action = Unhide_Read then Found_Unhidden else Found_Visible);
         when Beyond_Limit =>
            return BAR_Beyond_Limit;
         when others =>
            return BAR_Not_Placed;
      end case;
   end Classify;

end Platform_Quirks.Intel_P2SB;
