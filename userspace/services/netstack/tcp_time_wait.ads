------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Connections in TIME-WAIT (RFC 9293 3.6), kept out of their connection
--  slots: only addresses, ports and the two sequence numbers are needed.
--
--  Proved (tests/tcp-session): at most one entry per 4-tuple; Find returns
--  that entry or reports none; Enter always records the new wait; a RST
--  never ends the wait (RFC 1337); a new SYN beyond the old sequence space
--  ends it (the new connection may proceed); anything else is acknowledged,
--  a FIN restarting the 2 MSL wait; Expire ends exactly the waits that are
--  due. Tested, not proved: a full table gives up the wait that ends first.
------------------------------------------------------------------------------
with Interfaces;   use Interfaces;
with TCP_Sequence; use TCP_Sequence;

package TCP_Time_Wait with SPARK_Mode is

   Capacity : constant := 128;
   --  2 * MSL, as Linux's TCP_TIMEWAIT_LEN (milliseconds).
   Wait_Milliseconds : constant := 60_000;

   subtype Index is Natural range 0 .. Capacity - 1;
   subtype Maybe_Index is Integer range -1 .. Capacity - 1;
   No_Entry : constant Maybe_Index := -1;

   type MAC is array (0 .. 5) of Unsigned_8;

   --  The remote address (network order, packed) and the two ports.
   type Tuple is record
      Remote_IP   : Unsigned_32 := 0;
      Remote_Port : Unsigned_16 := 0;
      Local_Port  : Unsigned_16 := 0;
   end record;

   type Waiting is record
      Active    : Boolean := False;
      Key       : Tuple;
      Remote_MAC : MAC := [others => 0];
      Snd_Nxt   : Seq := 0;
      Rcv_Nxt   : Seq := 0;
      Deadline  : Unsigned_64 := 0;
   end record;

   type Table is array (Index) of Waiting;

   --  At most one entry per 4-tuple.
   function Unique (T : Table) return Boolean is
     (for all I in Index =>
        (for all J in Index =>
           (if I /= J and then T (I).Active and then T (J).Active then T (I).Key /= T (J).Key)));

   function Later (Now : Unsigned_64; Delay_MS : Unsigned_64) return Unsigned_64 is
     (if Delay_MS > Unsigned_64'Last - Now then Unsigned_64'Last else Now + Delay_MS);

   function Find (T : Table; K : Tuple) return Maybe_Index with
     Post => (if Find'Result = No_Entry then
                (for all I in Index => not (T (I).Active and then T (I).Key = K))
              else T (Find'Result).Active and then T (Find'Result).Key = K);

   --  Remove the entry for K, if any.
   procedure Remove (T : in out Table; K : Tuple) with
     Pre  => Unique (T),
     Post => Unique (T) and then
             (for all I in Index =>
                (if T'Old (I).Active and then T'Old (I).Key = K then not T (I).Active
                 else T (I) = T'Old (I)));

   --  A connection enters TIME-WAIT at Now. A full table gives up the entry
   --  whose wait ends first.
   procedure Enter (T : in out Table; K : Tuple; Remote_MAC : MAC; Snd_Nxt, Rcv_Nxt : Seq;
                    Now : Unsigned_64)
   with
     Pre  => Unique (T),
     Post => Unique (T) and then Find (T, K) /= No_Entry and then
             T (Find (T, K)) =
               (Active => True, Key => K, Remote_MAC => Remote_MAC, Snd_Nxt => Snd_Nxt,
                Rcv_Nxt => Rcv_Nxt, Deadline => Later (Now, Wait_Milliseconds));

   type Decision is
     (Ignore,        --  a RST: the wait continues (RFC 1337)
      Reopen,        --  a new SYN beyond the old sequence space: entry ended
      Acknowledge);  --  answer <SEQ=Snd_Nxt><ACK=Rcv_Nxt><CTL=ACK>

   --  A segment for entry I's 4-tuple.
   procedure Arrive (T : in out Table; I : Index; SYN, ACK, FIN, RST : Boolean; Seq_No : Seq;
                     Now : Unsigned_64; D : out Decision)
   with
     Pre  => Unique (T) and then T (I).Active,
     Post => Unique (T) and then
             D = (if RST then Ignore
                  elsif SYN and then not ACK and then Gt (Seq_No, T'Old (I).Rcv_Nxt) then Reopen
                  else Acknowledge) and then
             (if D = Reopen then not T (I).Active
              elsif D = Acknowledge and then FIN then
                T (I) = (T'Old (I) with delta Deadline => Later (Now, Wait_Milliseconds))
              else T (I) = T'Old (I)) and then
             (for all J in Index => (if J /= I then T (J) = T'Old (J)));

   --  End the waits that are due.
   procedure Expire (T : in out Table; Now : Unsigned_64) with
     Pre  => Unique (T),
     Post => Unique (T) and then
             (for all I in Index =>
                T (I) = (if T'Old (I).Active and then T'Old (I).Deadline <= Now
                         then (T'Old (I) with delta Active => False) else T'Old (I)));

   --  The earliest deadline among waiting entries (Unsigned_64'Last if none).
   function Next_Deadline (T : Table) return Unsigned_64 with
     Post => (for all I in Index => (if T (I).Active then Next_Deadline'Result <= T (I).Deadline));

end TCP_Time_Wait;
