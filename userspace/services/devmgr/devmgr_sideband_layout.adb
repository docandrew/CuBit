pragma Ada_2022;

package body Devmgr_Sideband_Layout with SPARK_Mode is

   function Test_Window (BAR_Base : Unsigned_64) return Block is
   begin
      if BAR_Base mod Page_Bytes = 0
        and then BAR_Base < Byte_Limit and then Edu_BAR0_Bytes <= Byte_Limit - BAR_Base
      then
         return (Status => Found, Base => BAR_Base + MCHBAR_Bytes,
                 Bytes => Edu_BAR0_Bytes - MCHBAR_Bytes);
      end if;
      return (Status => Not_Placed, Base => 0, Bytes => 0);
   end Test_Window;

   function Test_Community (BAR_Base : Unsigned_64) return Block is
   begin
      if BAR_Base mod Page_Bytes = 0
        and then BAR_Base < Byte_Limit and then Edu_BAR0_Bytes <= Byte_Limit - BAR_Base
      then
         return (Status => Found, Base => BAR_Base + Test_Community_Offset,
                 Bytes => GPIO_Community_Bytes);
      end if;
      return (Status => Not_Placed, Base => 0, Bytes => 0);
   end Test_Community;

   function Decode_SBREG (BAR0, BAR1 : Unsigned_32) return Block is
      Kind : constant Unsigned_32 := BAR0 and PCI_BAR_Type_Mask;
      Low  : constant Unsigned_64 := Unsigned_64 (BAR0 and not PCI_BAR_Flag_Mask);
      Base : Unsigned_64;
   begin
      if (BAR0 and PCI_BAR_IO) /= 0
        or else (Kind /= PCI_BAR_Type_32 and then Kind /= PCI_BAR_Type_64)
      then
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

   function Port_Block (Window : Block; Port : Port_ID) return Block is
     ((Status => Found, Base => Window.Base + Port * Port_Bytes, Bytes => Port_Bytes));

end Devmgr_Sideband_Layout;
