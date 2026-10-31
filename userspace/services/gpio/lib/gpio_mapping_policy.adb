pragma Ada_2022;
package body GPIO_Mapping_Policy with SPARK_Mode is
   function Admit
     (Authority : Description; Physical, Bytes, Virtual : Address;
      Page_Limit : Address) return Outcome is
   begin
      if not Authority.Current then return No_Authority; end if;
      if not Authority.Device_Memory then return Wrong_Kind; end if;
      if not Authority.Readable or else not Authority.Writable then return Missing_Rights; end if;
      if Page_Limit = 0 or else Page_Limit > Maximum_Map_Pages or else
        Bytes < Page_Bytes or else Bytes mod Page_Bytes /= 0 or else
        Bytes / Page_Bytes > Page_Limit or else Bytes > Authority.Bytes
      then return Invalid_Extent; end if;
      if Physical = 0 or else Physical mod Page_Bytes /= 0 or else
        Physical > Address'Last - Bytes
      then return Invalid_Physical; end if;
      if Physical /= Authority.First then return Different_Resource; end if;
      if Virtual < Page_Bytes or else Virtual mod Page_Bytes /= 0 or else
        Virtual > User_Virtual_Limit - Bytes
      then return Invalid_Virtual; end if;
      return Allowed;
   end Admit;
end GPIO_Mapping_Policy;
