pragma Ada_2022;

package body Platform_Quirks.Intel_GPIO_Communities with SPARK_Mode is

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

   function Port_Block (Window : Block; Port : Port_ID) return Block is
     ((Status => Found, Base => Window.Base + Port * Port_Bytes, Bytes => Port_Bytes));

end Platform_Quirks.Intel_GPIO_Communities;
