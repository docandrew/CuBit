pragma Ada_2022;
package body I2C_Mapping_Policy with SPARK_Mode is
   function Admit
     (Authority : Description; Physical, BAR_Bytes, Virtual : Address) return Outcome is
   begin
      if not Authority.Current then return No_Current_Authority;
      elsif not Authority.Device_Memory then return Wrong_Kind;
      elsif not Authority.Readable or else not Authority.Writable then return Missing_Rights;
      elsif Physical = 0 or else Physical mod Page_Bytes /= 0 or else
        Physical > Address'Last - Page_Bytes then return Invalid_Physical;
      elsif Virtual < Page_Bytes or else Virtual mod Page_Bytes /= 0 or else
        Virtual > User_Virtual_Limit - Page_Bytes then return Invalid_Virtual;
      elsif BAR_Bytes < Page_Bytes or else Authority.Bytes < Page_Bytes then return Short_Window;
      elsif Physical /= Authority.First then return Different_BAR;
      else return Allowed;
      end if;
   end Admit;
end I2C_Mapping_Policy;
