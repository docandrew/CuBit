------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body TCP_Time_Wait with SPARK_Mode is

   function Find (T : Table; K : Tuple) return Maybe_Index is
   begin
      for I in Index loop
         if T (I).Active and then T (I).Key = K then
            return I;
         end if;
         pragma Loop_Invariant (for all J in 0 .. I => not (T (J).Active and then T (J).Key = K));
      end loop;
      return No_Entry;
   end Find;

   procedure Remove (T : in out Table; K : Tuple) is
   begin
      for I in Index loop
         if T (I).Active and then T (I).Key = K then
            T (I).Active := False;
         end if;
         pragma Loop_Invariant
           (for all J in Index =>
              (if J <= I and then T'Loop_Entry (J).Active and then T'Loop_Entry (J).Key = K
               then not T (J).Active
               elsif J <= I then T (J) = T'Loop_Entry (J)
               else T (J) = T'Loop_Entry (J)));
         pragma Loop_Invariant (Unique (T));
      end loop;
   end Remove;

   procedure Enter (T : in out Table; K : Tuple; Remote_MAC : MAC; Snd_Nxt, Rcv_Nxt : Seq;
                    Now : Unsigned_64)
   is
      Victim : Index := 0;
   begin
      Remove (T, K);
      pragma Assert (for all I in Index => not (T (I).Active and then T (I).Key = K));
      for I in Index loop
         if not T (I).Active then
            Victim := I;
            exit;
         elsif T (I).Deadline < T (Victim).Deadline then
            Victim := I;   --  full so far: the wait that ends first gives way
         end if;
      end loop;
      T (Victim) := (Active => True, Key => K, Remote_MAC => Remote_MAC, Snd_Nxt => Snd_Nxt,
                     Rcv_Nxt => Rcv_Nxt, Deadline => Later (Now, Wait_Milliseconds));
      pragma Assert (for all I in Index => (if I /= Victim and then T (I).Active then T (I).Key /= K));
      pragma Assert (Unique (T));
   end Enter;

   procedure Arrive (T : in out Table; I : Index; SYN, ACK, FIN, RST : Boolean; Seq_No : Seq;
                     Now : Unsigned_64; D : out Decision)
   is
   begin
      if RST then
         D := Ignore;
      elsif SYN and then not ACK and then Gt (Seq_No, T (I).Rcv_Nxt) then
         D := Reopen;
         T (I).Active := False;
      else
         D := Acknowledge;
         if FIN then
            T (I).Deadline := Later (Now, Wait_Milliseconds);
         end if;
      end if;
   end Arrive;

   procedure Expire (T : in out Table; Now : Unsigned_64) is
   begin
      for I in Index loop
         if T (I).Active and then T (I).Deadline <= Now then
            T (I).Active := False;
         end if;
         pragma Loop_Invariant
           (for all J in Index =>
              (if J <= I then
                 T (J) = (if T'Loop_Entry (J).Active and then T'Loop_Entry (J).Deadline <= Now
                          then (T'Loop_Entry (J) with delta Active => False) else T'Loop_Entry (J))
               else T (J) = T'Loop_Entry (J)));
         pragma Loop_Invariant (Unique (T));
      end loop;
   end Expire;

   function Next_Deadline (T : Table) return Unsigned_64 is
      Result : Unsigned_64 := Unsigned_64'Last;
   begin
      for I in Index loop
         if T (I).Active and then T (I).Deadline < Result then
            Result := T (I).Deadline;
         end if;
         pragma Loop_Invariant
           (for all J in 0 .. I => (if T (J).Active then Result <= T (J).Deadline));
      end loop;
      return Result;
   end Next_Deadline;

end TCP_Time_Wait;
