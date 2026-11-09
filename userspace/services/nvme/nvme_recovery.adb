------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body NVMe_Recovery with SPARK_Mode is

   function Next (Phase : Recovery_Phase; Event : Recovery_Event)
     return Recovery_Phase is
   begin
      case Phase is
         when Serving =>
            return (if Event = Commands_Expired then Aborting else Serving);
         when Aborting =>
            return (if Event = Abort_Settled then Serving else Resetting);
         when Resetting =>
            return (if Event = Reset_Succeeded then Serving else Offline);
         when Offline =>
            return Resetting;
      end case;
   end Next;

   procedure Lemma_Recovery_Paths (Phase : Recovery_Phase; Event : Recovery_Event)
   is null;

end NVMe_Recovery;
