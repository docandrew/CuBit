------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body Neighbor_Cache with SPARK_Mode is

   function Position (T : Table; IP : Address) return Integer is
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

   procedure Solicit (T : in out Table; IP : Address; Now : Unsigned_64) is
      S : Index;
   begin
      if Position (T, IP) >= 0 then
         return;
      end if;
      S := Victim (T);
      T (S) := (St => Pending, IP => IP, Link => [others => 0], Since => Now);
      pragma Assert (for all I in Index =>
                       (if I /= S and then T (I).St /= Free then T (I).IP /= IP));
   end Solicit;

   procedure Learn (T : in out Table; E : Event; Now : Unsigned_64) is
      Pos : constant Integer := Position (T, E.Peer);
      S   : Index;
   begin
      if not Usable (E) then
         return;
      end if;
      case E.Of_Kind is
         when Advertisement =>
            --  Only the answer to our own question.
            if E.Solicited and then Pos >= 0 and then T (Pos).St = Pending then
               T (Pos) := (St => Resolved, IP => E.Peer, Link => E.Link, Since => Now);
            end if;
         when Solicitation =>
            if not E.Ours then
               return;
            end if;
            if Pos < 0 then
               --  The sender is talking to us.
               S := Victim (T);
               T (S) := (St => Resolved, IP => E.Peer, Link => E.Link, Since => Now);
               pragma Assert (for all I in Index =>
                                (if I /= S and then T (I).St /= Free then T (I).IP /= E.Peer));
            elsif T (Pos).St = Pending then
               T (Pos) := (St => Resolved, IP => E.Peer, Link => E.Link, Since => Now);
            elsif T (Pos).Link = E.Link then
               T (Pos).Since := Now;   --  still there; a different address is not taken
            end if;
      end case;
   end Learn;

   procedure Lookup (T : Table; IP : Address; Link : out MAC; Found : out Boolean) is
      Pos : constant Integer := Position (T, IP);
   begin
      Found := Pos >= 0 and then T (Pos).St = Resolved;
      Link := (if Found then T (Pos).Link else [others => 0]);
   end Lookup;

end Neighbor_Cache;
