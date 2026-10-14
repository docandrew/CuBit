package body Reference is
   type Wide is range -(2 ** 127) .. 2 ** 127 - 1;

   function Derivable
     (Source : Capability; Item : Capability_Install.Request) return Boolean
   is
      First_Device_Vector : constant Wide := 48;
      Last_Device_Vector  : constant Wide := 126;
      Max_Microseconds    : constant Wide := 2 ** 30;
      S_First : constant Wide := Wide (Source.object.ref);
      S_Count : constant Wide := Wide (Source.object.param);
      I_First : constant Wide := Wide (Item.Base);
      I_Count : constant Wide := Wide (Item.Length);
      Limit   : constant Wide := Wide (Unsigned_64'Last);
   begin
      case Source.capType is
         when CAP_ENDPOINT | CAP_NOTIFICATION | CAP_IOPORT | CAP_IRQ |
              CAP_PROCESS | CAP_DEVICE_MEM | CAP_SCHEDULING => null;
         when others => return False;
      end case;
      if not Source.rights (RIGHT_GRANT) then
         return False;
      end if;
      for R in CapabilityRight loop
         if Item.Rights (R) and then not Source.rights (R) then
            return False;
         end if;
      end loop;
      if Source.capType = CAP_IRQ then
         -- A nonempty, nonwrapping range clear of the device vectors.
         if S_Count = 0 or else S_First + S_Count - 1 > Limit then
            return False;
         end if;
         if S_First <= Last_Device_Vector
           and then S_First + S_Count - 1 >= First_Device_Vector
         then
            return False;
         end if;
      end if;
      if Item.Set_Tag then
         if Source.capType not in CAP_ENDPOINT | CAP_NOTIFICATION
           or else Source.authorityTag /= 0
           or else Item.Tag = 0
         then
            return False;
         end if;
      end if;
      if Item.Narrow then
         case Source.capType is
            when CAP_DEVICE_MEM | CAP_IOPORT | CAP_IRQ | CAP_NOTIFICATION =>
               if I_Count = 0 or else S_Count = 0
                 or else I_First + I_Count - 1 > Limit
                 or else S_First + S_Count - 1 > Limit
                 or else I_First < S_First
                 or else I_First + I_Count > S_First + S_Count
               then
                  return False;
               end if;
            when CAP_SCHEDULING =>
               if I_First > Max_Microseconds or else S_First > Max_Microseconds
                 or else I_Count < 1 or else I_Count > Max_Microseconds
                 or else S_Count < 1 or else S_Count > Max_Microseconds
                 or else I_First * S_Count > S_First * I_Count
               then
                  return False;
               end if;
            when others =>
               return False;
         end case;
      end if;
      return True;
   end Derivable;
end Reference;
