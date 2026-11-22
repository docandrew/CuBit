------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body TCP_Send_Queue with SPARK_Mode is

   --  Moving the head by F keeps every later byte at the same storage slot.
   procedure Lemma_Shift (H : Index; F, I : Natural) with
     Ghost, Global => null,
     Pre  => F <= Capacity and then I < Capacity,
     Post => ((H + F) mod Capacity + I) mod Capacity = (H + (I + F)) mod Capacity;
   procedure Lemma_Shift (H : Index; F, I : Natural) is null;

   procedure Initialize (Q : out Queue; Start : Seq) is
   begin
      Q := (Data => [others => 0], Head => 0, Length => 0, In_Flight => 0,
            Start => Start);
   end Initialize;

   procedure Push (Q : in out Queue; Data : Byte_Array; Accepted : out Byte_Count) is
      Before : constant Queue := Q with Ghost;
   begin
      Accepted := Natural'Min (Data'Length, Capacity - Q.Length);
      for K in 0 .. Accepted - 1 loop
         Q.Data ((Q.Head + Q.Length + K) mod Capacity) := Data (Data'First + K);
         pragma Loop_Invariant (Q.Head = Before.Head and then Q.Length = Before.Length and then
           Q.In_Flight = Before.In_Flight and then Q.Start = Before.Start);
         pragma Loop_Invariant
           (for all I in 0 .. Before.Length - 1 =>
              Q.Data ((Q.Head + I) mod Capacity) = Before.Data ((Before.Head + I) mod Capacity));
         pragma Loop_Invariant
           (for all J in 0 .. K =>
              Q.Data ((Q.Head + Q.Length + J) mod Capacity) = Data (Data'First + J));
      end loop;
      Q.Length := Q.Length + Accepted;
   end Push;

   procedure Take (Q : in out Queue; Limit : Natural; Data : out Byte_Array;
                   First : out Seq; Taken : out Byte_Count) is
   begin
      Data := [others => 0];
      Taken := Natural'Min (Limit, Q.Length - Q.In_Flight);
      First := Q.Start + Seq (Q.In_Flight);
      for K in 1 .. Taken loop
         Data (K) := Q.Data ((Q.Head + Q.In_Flight + K - 1) mod Capacity);
         pragma Loop_Invariant
           (for all J in 1 .. K =>
              Data (J) = Q.Data ((Q.Head + Q.In_Flight + J - 1) mod Capacity));
      end loop;
      Q.In_Flight := Q.In_Flight + Taken;
   end Take;

   procedure Acknowledge (Q : in out Queue; Ack : Seq; Result : out Ack_Result;
                          Freed : out Byte_Count) is
      Before : constant Queue := Q with Ghost;
      --  The forward distance as an integer (exact), so the comparison
      --  below is integer arithmetic, not modular.
      Gap  : constant Long_Long_Integer :=
        Long_Long_Integer (Distance (Q.Start, Ack));
   begin
      if Gap <= Long_Long_Integer (Q.In_Flight) then
         Freed := Natural (Gap);
         pragma Assert (Freed <= Q.In_Flight and then Q.In_Flight <= Q.Length);
         Result := (if Freed = 0 then Duplicate else Advanced);
         for I in 0 .. Q.Length - Freed - 1 loop
            Lemma_Shift (Q.Head, Freed, I);
            pragma Loop_Invariant
              (for all J in 0 .. I =>
                 ((Q.Head + Freed) mod Capacity + J) mod Capacity =
                   (Q.Head + (J + Freed)) mod Capacity);
            pragma Loop_Invariant (Freed <= Q.In_Flight and then Q.In_Flight <= Q.Length);
         end loop;
         Q.Head := (Q.Head + Freed) mod Capacity;
         Q.Length := Q.Length - Freed;
         Q.In_Flight := Q.In_Flight - Freed;
         Q.Start := Ack;
         pragma Assert
           (for all I in 0 .. Q.Length - 1 =>
              Q.Data ((Q.Head + I) mod Capacity) =
                Before.Data ((Before.Head + (I + Freed)) mod Capacity));
      else
         Freed := 0;
         Result := (if Lt (Q.Start + Seq (Q.In_Flight), Ack) then Unsent_Data else Old);
      end if;
   end Acknowledge;

   procedure Rewind (Q : in out Queue) is
   begin
      Q.In_Flight := 0;
   end Rewind;
end TCP_Send_Queue;
