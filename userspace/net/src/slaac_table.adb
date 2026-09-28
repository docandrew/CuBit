------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body SLAAC_Table with SPARK_Mode is

   function Stable_ID
     (Secret : SipHash.Key; Network : Address; Counter : Unsigned_8)
      return Interface_ID
   is
      Input : SipHash.Byte_Array (0 .. 8) := [others => 0];
      H : Unsigned_64;
      Result : Interface_ID;
   begin
      for K in 0 .. 7 loop
         Input (K) := Network (K);
      end loop;
      Input (8) := Counter;
      H := SipHash.Hash (Secret, Input);
      for K in Interface_ID'Range loop
         Result (K) := Unsigned_8 (Shift_Right (H, 8 * K) and 16#FF#);
      end loop;
      return Result;
   end Stable_ID;

   function Position (T : Table; Network : Address) return Integer is
   begin
      for I in Index loop
         if T (I).St /= Free and then T (I).Network = Network then
            return I;
         end if;
         pragma Loop_Invariant
           (for all J in 0 .. I =>
              not (T (J).St /= Free and then T (J).Network = Network));
      end loop;
      return -1;
   end Position;

   procedure Advertised
     (T : in out Table; P : RA_Message.Prefix; ID : Interface_ID; Now : Time)
   is
      Pos : constant Integer := Position (T, P.Network);
      New_Valid : constant Unsigned_64 := Deadline (Now, P.Valid);
      Addr : Address := P.Network;
   begin
      if Pos >= 0 then
         if T (Pos).St = Duplicate then
            return;
         end if;
         T (Pos).Preferred_Until := Deadline (Now, P.Preferred);
         --  RFC 4862 5.5.3 (e).
         if P.Valid > Two_Hours or else New_Valid > T (Pos).Valid_Until then
            T (Pos).Valid_Until := New_Valid;
         elsif T (Pos).Valid_Until <= Now + Two_Hours then
            null;   --  within two hours: only a longer lifetime is taken
         else
            T (Pos).Valid_Until := Now + Two_Hours;
         end if;
         return;
      end if;
      if P.Valid = 0 then
         return;
      end if;
      for K in 0 .. 7 loop
         Addr (8 + K) := ID (K);
      end loop;
      for I in Index loop
         if T (I).St = Free then
            T (I) := (St => Tentative, Network => P.Network, Addr => Addr,
                      Valid_Until => New_Valid,
                      Preferred_Until => Deadline (Now, P.Preferred),
                      DAD_Until => Now + DAD_Seconds);
            pragma Assert
              (for all J in Index =>
                 (if J /= I and then T (J).St /= Free then T (J).Network /= P.Network));
            return;
         end if;
         pragma Loop_Invariant (T = T'Loop_Entry);
      end loop;
   end Advertised;

   procedure Conflict (T : in out Table; Addr : Address) is
   begin
      for I in Index loop
         if T (I).St = Tentative and then T (I).Addr = Addr then
            T (I).St := Duplicate;
         end if;
         pragma Loop_Invariant
           (for all J in Index =>
              (if J <= I and then T'Loop_Entry (J).St = Tentative and then
                  T'Loop_Entry (J).Addr = Addr
               then T (J).St = Duplicate and then
                    T (J).Network = T'Loop_Entry (J).Network
               else T (J) = T'Loop_Entry (J)));
         pragma Loop_Invariant
           (for all J in Index => (T (J).St = Free) = (T'Loop_Entry (J).St = Free));
         pragma Loop_Invariant
           (for all J in Index => T (J).Network = T'Loop_Entry (J).Network);
      end loop;
   end Conflict;

   procedure Tick (T : in out Table; Now : Time) is
   begin
      for I in Index loop
         if T (I).St /= Free and then T (I).Valid_Until <= Now then
            T (I) := (others => <>);
         elsif T (I).St = Tentative and then T (I).DAD_Until <= Now then
            T (I).St :=
              (if T (I).Preferred_Until <= Now then Deprecated else Preferred);
         elsif T (I).St = Preferred and then T (I).Preferred_Until <= Now then
            T (I).St := Deprecated;
         end if;
         pragma Loop_Invariant
           (for all J in Index =>
              (if J > I then T (J) = T'Loop_Entry (J)));
         pragma Loop_Invariant
           (for all J in Index =>
              (if J <= I then
                 T (J).St = T'Loop_Entry (J).St or else T (J).St = Free or else
                 (T'Loop_Entry (J).St = Tentative and then
                  T'Loop_Entry (J).DAD_Until <= Now and then
                  T (J).St in Preferred | Deprecated) or else
                 (T'Loop_Entry (J).St = Preferred and then T (J).St = Deprecated)));
         pragma Loop_Invariant
           (for all J in Index =>
              (if T (J).St /= Free then T (J).Network = T'Loop_Entry (J).Network
                                       and then T'Loop_Entry (J).St /= Free));
      end loop;
   end Tick;

end SLAAC_Table;
