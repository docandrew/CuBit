------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body TCP_Options with SPARK_Mode is

   function Negotiate (Ours : Offer; Peer : Received; Link_MSS : MSS_Value;
                       IPv6 : Boolean) return Agreement
   is
      A : Agreement;
      Limit    : constant Unsigned_32 := Unsigned_32'Min (Peer_MSS (Peer, IPv6), Link_MSS);
      Overhead : Unsigned_32;
   begin
      A.Scaling := Ours.Window_Scale and then Peer.Has_Window_Scale;
      if A.Scaling then
         A.Rcv_Shift := Ours.Shift_Count;
         --  RFC 7323 2.3: a larger shift is used as 14.
         A.Snd_Shift := (if Peer.Shift_Count > Maximum_Shift then Maximum_Shift
                         else Natural (Peer.Shift_Count));
      else
         A.Rcv_Shift := 0;
         A.Snd_Shift := 0;
      end if;
      A.SACK := Ours.SACK_Permitted and then Peer.SACK_Permitted;
      A.Timestamps := Ours.Timestamps and then Peer.Has_Timestamps;
      Overhead := (if A.Timestamps then Timestamp_Size else 0);
      A.Send_MSS := (if Limit <= Minimum_MSS + Overhead then Minimum_MSS
                     else Limit - Overhead);
      return A;
   end Negotiate;
end TCP_Options;
