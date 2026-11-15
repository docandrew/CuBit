pragma Ada_2022;

package body Power_Decisions with SPARK_Mode is

   procedure After_Watch
     (Status : PE.Watch_Status; Count : Unsigned_64; Seen : in out Unsigned_64; Again : out Boolean;
      Pressed : out Boolean)
   is
   begin
      Pressed := Status = PE.Pressed and then Count > Seen;
      case Status is
         when PE.Pressed | PE.Counter_Reset =>
            --  Presses are coalesced into the count: watch from it.
            Seen := Count;
            Again := True;
         when PE.Superseded =>
            Again := True;
         when PE.Not_Approved | PE.Malformed | PE.No_Power_Button | PE.Stopping | PE.Watch_Unavailable =>
            --  Watch_Unavailable is retried after a delay (Retry_Later).
            Again := False;
      end case;
   end After_Watch;

end Power_Decisions;
