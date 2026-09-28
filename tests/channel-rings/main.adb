------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  Host tests for CuBit.Channel_Rings: a byte stream through a producer and
--  consumer pair arrives in order and intact, across 32-bit index wrap, with
--  random write and read sizes; hostile peer indices are refused and leave
--  the ring unchanged. Contracts are checked at run time (-gnata).
------------------------------------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Channel_Rings; use CuBit.Channel_Rings;
with CuBit.Datagram_Rings; use CuBit.Datagram_Rings;

procedure Main is
   Failures : Natural := 0;
   Seed     : Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;

   function Random return Unsigned_64 is
   begin
      Seed := Seed xor Shift_Left (Seed, 13);
      Seed := Seed xor Shift_Right (Seed, 7);
      Seed := Seed xor Shift_Left (Seed, 17);
      return Seed;
   end Random;

   function Below (N : Positive) return Natural is (Natural (Random mod Unsigned_64 (N)));

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & Name);
      end if;
   end Check;

   --  The stream's byte at a given stream offset.
   function Pattern (Offset : Unsigned_64) return Unsigned_8 is
     (Unsigned_8 ((Offset * 131 + Offset / 251) mod 256));

   --  Push Total bytes through a ring of Size whose indices start at Origin,
   --  alternating copying and zero-copy (slice) operations.
   procedure Stream (Size : Ring_Size; Origin : Index; Total : Unsigned_64) is
      Ring     : Bytes (0 .. Size - 1) := [others => 16#EE#];
      P        : Producer := New_Producer (Size, Origin);
      C        : Consumer := New_Consumer (Size, Origin);
      Sent, Got : Unsigned_64 := 0;
      OK       : Boolean;
      Intact   : Boolean := True;
   begin
      while Got < Total loop
         --  Produce.
         declare
            Want : constant Natural :=
              Natural (Unsigned_64'Min (Unsigned_64 (Below (Size + Size / 2)), Total - Sent));
         begin
            if Random mod 2 = 0 then
               declare
                  Data : Bytes (1_000 .. 999 + Want);
                  Written : Natural;
               begin
                  for I in Data'Range loop
                     Data (I) := Pattern (Sent + Unsigned_64 (I - Data'First));
                  end loop;
                  Write (P, Ring, Data, Written);
                  Sent := Sent + Unsigned_64 (Written);
               end;
            else
               declare
                  First, L1, L2, N : Natural;
               begin
                  Free_Slices (P, First, L1, L2);
                  N := Natural'Min (Want, L1 + L2);
                  for K in 0 .. N - 1 loop
                     Ring (if K < L1 then First + K else K - L1) := Pattern (Sent + Unsigned_64 (K));
                  end loop;
                  Commit (P, N);
                  Sent := Sent + Unsigned_64 (N);
               end;
            end if;
         end;
         Accept_Produced (C, P.Produced, OK);
         Check (OK, "honest produced index accepted");

         --  Consume.
         declare
            Want : constant Natural := Below (Size + Size / 2);
         begin
            if Random mod 2 = 0 then
               declare
                  Data  : Bytes (7 .. 6 + Want) := [others => 0];
                  Count : Natural;
               begin
                  Read (C, Ring, Data, Count);
                  for I in 0 .. Count - 1 loop
                     Intact := Intact and then Data (Data'First + I) = Pattern (Got + Unsigned_64 (I));
                  end loop;
                  Got := Got + Unsigned_64 (Count);
               end;
            else
               declare
                  First, L1, L2, N : Natural;
               begin
                  Data_Slices (C, First, L1, L2);
                  N := Natural'Min (Want, L1 + L2);
                  for K in 0 .. N - 1 loop
                     Intact := Intact and then
                       Ring (if K < L1 then First + K else K - L1) = Pattern (Got + Unsigned_64 (K));
                  end loop;
                  Consume (C, N);
                  Got := Got + Unsigned_64 (N);
               end;
            end if;
         end;
         Accept_Consumed (P, C.Consumed, OK);
         Check (OK, "honest consumed index accepted");
      end loop;
      Check (Intact, "stream intact, size" & Size'Image & " origin" & Origin'Image);
      Check (Sent = Total and then Got = Total, "stream complete");
   end Stream;

   --  A hostile peer writes random indices; each is accepted exactly when it
   --  follows the rules, and a refusal changes nothing.
   procedure Hostile (Size : Ring_Size) is
      P  : Producer := New_Producer (Size, Index'Last - 10);
      C  : Consumer := New_Consumer (Size, Index'Last - 10);
      OK : Boolean;
      Accepted, Refused : Natural := 0;
   begin
      for Round in 1 .. 200_000 loop
         Commit (P, Below (Space (P) + 1));
         declare
            Before : constant Producer := P;
            Value  : constant Index :=
              (case Random mod 4 is
                 when 0 => Index (Random mod 2 ** 32),
                 when 1 => Consumed (P) + Index (Below (P.Fill + 1)),
                 when 2 => P.Produced + Index (Below (Size) + 1),
                 when others => Consumed (P) - Index (Below (Size) + 1));
            Legal  : constant Boolean :=
              Value - Consumed (Before) <= Before.Produced - Consumed (Before);
         begin
            Accept_Consumed (P, Value, OK);
            Check (OK = Legal, "consumed index judged");
            Check ((if OK then Consumed (P) = Value and then P.Produced = Before.Produced
                    else P = Before), "consumed effect");
            Check (P.Fill <= Size, "fill bounded");
         end;
         declare
            Before : constant Consumer := C;
            Value  : constant Index :=
              (case Random mod 4 is
                 when 0 => Index (Random mod 2 ** 32),
                 when 1 => Produced (C) + Index (Below (Size - C.Available + 1)),
                 when 2 => C.Consumed + Index (Below (Size) + Size + 1),
                 when others => Produced (C) - Index (Below (Size) + 1));
            Legal  : constant Boolean :=
              Value - Before.Consumed <= Index (Size) and then
              Value - Before.Consumed >= Produced (Before) - Before.Consumed;
         begin
            Accept_Produced (C, Value, OK);
            Check (OK = Legal, "produced index judged");
            Check ((if OK then Produced (C) = Value and then C.Consumed = Before.Consumed
                    else C = Before), "produced effect");
            if OK then Accepted := Accepted + 1; else Refused := Refused + 1; end if;
            Consume (C, Below (C.Available + 1));
         end;
      end loop;
      Check (Accepted > 10_000 and then Refused > 10_000, "hostile indices exercised both ways");
   end Hostile;

   --  Datagrams of random sizes through a ring (wrapping, padding), each
   --  read back intact and in order, some into short buffers (truncated).
   procedure Datagrams (Size : Ring_Size; Origin : Index) is
      Ring : Bytes (0 .. Size - 1) := [others => 16#EE#];
      P : Producer := New_Producer (Size, Origin);
      C : Consumer := New_Consumer (Size, Origin);
      Sent, Got : Natural := 0;
      Intact : Boolean := True;
      Lengths : array (0 .. 1023) of Natural := [others => 0];
      PR : Put_Result;
      TR : Take_Result;
      OK : Boolean;
   begin
      for Round in 1 .. 60_000 loop
         if Random mod 2 = 0 and then Sent - Got < Lengths'Length then
            declare
               Len : constant Natural := Below (Natural'Min (Size / 3, 2_000));
               Data : Bytes (1 .. Len);
            begin
               for I in Data'Range loop
                  Data (I) := Pattern (Unsigned_64 (Sent) * 7 + Unsigned_64 (I));
               end loop;
               Put (P, Ring, Data, PR);
               if PR = Put then
                  Lengths (Sent mod Lengths'Length) := Len;
                  Sent := Sent + 1;
               end if;
               Check (PR /= Too_Large, "datagram fits");
            end;
         else
            Accept_Produced (C, P.Produced, OK);
            declare
               Room : constant Natural := Below (2_100);
               Into : Bytes (5 .. 4 + Room) := [others => 0];
               Len : Natural;
               Trunc : Boolean;
            begin
               Take (C, Ring, Into, Len, Trunc, TR);
               Check (TR /= Malformed, "honest records are well formed");
               if TR = Taken then
                  declare
                     Want : constant Natural := Lengths (Got mod Lengths'Length);
                  begin
                     Intact := Intact and then Len = Natural'Min (Want, Room) and then
                       Trunc = (Want > Room);
                     for I in 1 .. Len loop
                        Intact := Intact and then
                          Into (4 + I) = Pattern (Unsigned_64 (Got) * 7 + Unsigned_64 (I));
                     end loop;
                  end;
                  Got := Got + 1;
               end if;
            end;
            Accept_Consumed (P, C.Consumed, OK);
         end if;
      end loop;
      Check (Intact and then Got > 1_000, "datagrams intact, in order, truncation reported");
   end Datagrams;

   --  A hostile producer's headers: never a read outside the ring or the
   --  buffer (contracts are checked), and bad headers are reported.
   procedure Hostile_Datagrams is
      Ring : Bytes (0 .. 4_095);
      C : Consumer;
      OK : Boolean;
      Into : Bytes (1 .. 100);
      Len : Natural;
      Trunc : Boolean;
      TR : Take_Result;
      Malformed_Seen : Natural := 0;
   begin
      for Round in 1 .. 50_000 loop
         for I in Ring'Range loop
            Ring (I) := Unsigned_8 (Random mod 256);
         end loop;
         C := New_Consumer (4_096, Index (Random mod 2 ** 32));
         Accept_Produced (C, C.Consumed + Index (Below (4_097)), OK);
         Take (C, Ring, Into, Len, Trunc, TR);
         if TR = Malformed then
            Malformed_Seen := Malformed_Seen + 1;
         end if;
      end loop;
      Check (Malformed_Seen > 10_000, "hostile datagram headers reported");
   end Hostile_Datagrams;

begin
   Datagrams (4_096, 0);
   Datagrams (4_096, Index'Last - 703);   --  aligned: 2 ** 32 - 704
   Datagrams (65_536, Index'Last - 29_999);   --  aligned: 2 ** 32 - 30_000
   Hostile_Datagrams;
   Check (Valid_Size (4_096) and then Valid_Size (1_048_576), "sizes");
   Check (not Valid_Size (0) and then not Valid_Size (6_000) and then
          not Valid_Size (2_097_152) and then not Valid_Size (2_048), "bad sizes");
   for Order in 0 .. 2 loop
      declare
         Size : constant Ring_Size := 4_096 * 2 ** (Order * 2);
      begin
      Stream (Size, 0, 3_000_000);
      Stream (Size, Index'Last - Index (Size / 3), 3_000_000);   --  wraps 2 ** 32
      Stream (Size, Index'Last, 500_000);
      Hostile (Size);
      end;
   end loop;
   if Failures = 0 then
      Put_Line ("channel-rings: PASS");
   else
      Put_Line ("channel-rings: FAIL (" & Failures'Image & " )");
   end if;
end Main;
