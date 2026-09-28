------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body TCP_Timestamps with SPARK_Mode is

   procedure Check (S : in out State; TSval : Seq; RST : Boolean;
                    Now : Unsigned_64; V : out Verdict)
   is
      Trusted : constant Boolean := Fresh (S, Now);
   begin
      S.Recent_Valid := Trusted;
      V := (if not RST and then Trusted and then Lt (TSval, S.Recent) then Refuse
            else Pass);
   end Check;

   procedure Update (S : in out State; TSval : Seq; Seg_Seq, Last_Ack_Sent : Seq;
                     Now : Unsigned_64)
   is
   begin
      if Le (Seg_Seq, Last_Ack_Sent) and then
         (not S.Recent_Valid or else Ge (TSval, S.Recent))
      then
         S := (Recent => TSval, Recent_Valid => True, Recent_Time => Now);
      end if;
   end Update;
end TCP_Timestamps;
