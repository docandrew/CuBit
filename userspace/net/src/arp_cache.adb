------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body ARP_Cache with SPARK_Mode is

   function Position (T : Table; IP : IPv4) return Integer is
   begin
      for I in Index loop
         if T (I).St /= Free and then T (I).IP = IP then
            return I;
         end if;
         pragma Loop_Invariant
           (for all J in 0 .. I => not (T (J).St /= Free and then T (J).IP = IP));
      end loop;
      return -1;
   end Position;

   --  Where a new entry goes: a free one, else the one learned longest ago.
   function Victim (T : Table) return Index is
      V : Index := 0;
   begin
      for I in Index loop
         if T (I).St = Free then
            return I;
         elsif T (I).Since < T (V).Since then
            V := I;
         end if;
      end loop;
      return V;
   end Victim;

   procedure Request (T : in out Table; IP : IPv4; Now : Unsigned_64) is
      S : Index;
   begin
      if Position (T, IP) >= 0 then
         return;
      end if;
      S := Victim (T);
      T (S) := (St => Pending, IP => IP, HW => [others => 0], Since => Now);
      pragma Assert (for all I in Index =>
                       (if I /= S and then T (I).St /= Free then T (I).IP /= IP));
   end Request;

   procedure Learn (T : in out Table; P : Packet; Ours : Boolean; Now : Unsigned_64) is
      Pos : constant Integer := Position (T, P.Sender_IP);
      S   : Index;
   begin
      if not Usable_Sender (P.Sender_IP, P.Sender_HW) then
         return;
      end if;
      case P.Op is
         when Reply =>
            --  Only the answer to our own question.
            if Pos >= 0 and then T (Pos).St in Pending | Probing then
               T (Pos) := (St => Resolved, IP => P.Sender_IP, HW => P.Sender_HW, Since => Now);
            end if;
         when Request =>
            if not Ours then
               return;
            end if;
            if Pos < 0 then
               --  RFC 826 merge: the sender is talking to us.
               S := Victim (T);
               T (S) := (St => Resolved, IP => P.Sender_IP, HW => P.Sender_HW, Since => Now);
               pragma Assert (for all I in Index =>
                                (if I /= S and then T (I).St /= Free then T (I).IP /= P.Sender_IP));
            elsif T (Pos).St = Pending then
               T (Pos) := (St => Resolved, IP => P.Sender_IP, HW => P.Sender_HW, Since => Now);
            elsif T (Pos).HW = P.Sender_HW then
               --  Still there; a different address is not taken.
               T (Pos).Since := Now;
               T (Pos).St := Resolved;
            end if;
      end case;
   end Learn;

   procedure Lookup (T : Table; IP : IPv4; HW : out MAC; Found : out Boolean) is
      Pos : constant Integer := Position (T, IP);
   begin
      Found := Pos >= 0 and then T (Pos).St in Usable;
      HW := (if Found then T (Pos).HW else [others => 0]);
   end Lookup;

   procedure Reconfirm (T : in out Table; IP : IPv4; Now : Unsigned_64) is
      Pos : constant Integer := Position (T, IP);
   begin
      if Pos >= 0 and then T (Pos).St = Resolved then
         T (Pos).St := Probing;
         T (Pos).Since := Now;
      end if;
   end Reconfirm;

   procedure Expire (T : in out Table; Now, Timeout : Unsigned_64) is
   begin
      if Now < Timeout then
         return;
      end if;
      for I in Index loop
         pragma Loop_Invariant (Unique (T));
         pragma Loop_Invariant
           (for all J in Index =>
              T (J) = T'Loop_Entry (J) or else
              (J < I and then T (J).St = Free and then
               T'Loop_Entry (J).St in Pending | Probing and then
               Now - Timeout >= T'Loop_Entry (J).Since));
         if T (I).St in Pending | Probing and then Now - Timeout >= T (I).Since then
            T (I) := (others => <>);
         end if;
      end loop;
   end Expire;

end ARP_Cache;
