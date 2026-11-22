------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body TCP_Receive_Queue with SPARK_Mode is

   procedure Initialize (Q : out Queue; Start : Seq) is
   begin
      Q := (Data => [others => 0], Have => [others => False], Head => 0,
            Count => 0, Start => Start);
   end Initialize;

   --  RCV.NXT advances over what is now contiguous: bytes that arrived out
   --  of order, each scanned once.
   procedure Extend (Q : in out Queue) with
     Pre  => Contiguous (Q),
     Post => Contiguous (Q) and then Q.Count >= Q.Count'Old and then
             Q.Have = Q.Have'Old and then Q.Data = Q.Data'Old and then
             Q.Head = Q.Head'Old and then Q.Start = Q.Start'Old;
   procedure Extend (Q : in out Queue) is
      C : Byte_Count := Q.Count;
   begin
      loop
         pragma Loop_Invariant (C >= Q.Count);
         pragma Loop_Invariant (for all O in 0 .. C - 1 => Present (Q, O));
         pragma Loop_Variant (Increases => C);
         exit when C >= Capacity or else not Q.Have (Slot (Q, C));
         pragma Assert (Present (Q, C));
         C := C + 1;
      end loop;
      declare
         Scanned : constant Queue := Q with Ghost;
      begin
         Q.Count := C;
         pragma Assert (for all O in 0 .. Capacity - 1 => Present (Q, O) = Present (Scanned, O));
      end;
   end Extend;

   procedure Insert (Q : in out Queue; Offset : Byte_Offset; Data : Byte_Array) is
      Before : constant Queue := Q with Ghost;
      L  : constant Byte_Count := Fitting (Offset, Data'Length);
      P  : constant Index := Slot (Q, Offset);
      --  Up to the end of the ring, then from its start.
      N1 : constant Natural := Natural'Min (L, Capacity - P);
      N2 : constant Natural := L - N1;
   begin
      pragma Assert
        (for all K in 1 .. L =>
           Slot (Q, Offset + K - 1) = (if K <= N1 then P + K - 1 else K - N1 - 1));
      pragma Assert
        (for all Off in 0 .. Capacity - 1 =>
           (if Off < Offset or else Off - Offset >= L then
              Slot (Q, Off) not in P .. P + N1 - 1 and then Slot (Q, Off) >= N2));

      Q.Data (P .. P + N1 - 1) := Raw (Data (1 .. N1));
      Q.Have (P .. P + N1 - 1) := [others => True];
      if N2 > 0 then
         Q.Data (0 .. N2 - 1) := Raw (Data (N1 + 1 .. L));
         Q.Have (0 .. N2 - 1) := [others => True];
      end if;
      pragma Assert (Q.Head = Before.Head);
      pragma Assert
        (for all K in 1 .. L =>
           Present (Q, Offset + K - 1) and then Element (Q, Offset + K - 1) = Data (K));
      pragma Assert
        (for all Off in 0 .. Capacity - 1 =>
           (if Off < Offset or else Off - Offset >= L then
              Q.Have (Slot (Q, Off)) = Before.Have (Slot (Before, Off)) and then
              Q.Data (Slot (Q, Off)) = Before.Data (Slot (Before, Off))));
      pragma Assert
        (for all Off in 0 .. Capacity - 1 =>
           (if Off < Offset or else Off - Offset >= L then
              Present (Q, Off) = Present (Before, Off) and then
              Element (Q, Off) = Element (Before, Off)));

      --  Bytes below RCV.NXT are outside the insert, so still present.
      pragma Assert (for all Off in 0 .. Before.Count - 1 => Off < Offset);
      pragma Assert (for all Off in 0 .. Before.Count - 1 => Present (Q, Off));
      --  In-order text: everything up to its end is now present.
      declare
         Written : constant Queue := Q with Ghost;
      begin
         if Offset = Q.Count then
            Q.Count := Offset + L;
         end if;
         pragma Assert (for all Off in 0 .. Capacity - 1 => Present (Q, Off) = Present (Written, Off));
         pragma Assert (for all Off in 0 .. Capacity - 1 => Element (Q, Off) = Element (Written, Off));
      end;
      pragma Assert (Contiguous (Q));
      Extend (Q);
   end Insert;

   procedure Read (Q : in out Queue; Output : out Byte_Array; Got : out Byte_Count) is
      Before : constant Queue := Q with Ghost;
      G  : constant Byte_Count := Natural'Min (Output'Length, Q.Count);
      --  From Head to the end of the ring, then from its start.
      N1 : constant Natural := Natural'Min (G, Capacity - Q.Head);
      N2 : constant Natural := G - N1;
      --  Conversions to these slide the ring's 0-based slices.
      subtype First_Part is Byte_Array (1 .. N1);
      subtype Second_Part is Byte_Array (N1 + 1 .. G);
   begin
      Output := [others => 0];
      Got := G;
      pragma Assert
        (for all K in 1 .. Got =>
           Slot (Q, K - 1) = (if K <= N1 then Q.Head + K - 1 else K - N1 - 1));

      Output (1 .. N1) := First_Part (Q.Data (Q.Head .. Q.Head + N1 - 1));
      Output (N1 + 1 .. Got) := Second_Part (Q.Data (0 .. N2 - 1));
      pragma Assert (for all K in 1 .. Got => Output (K) = Element (Before, K - 1));

      Q.Have (Q.Head .. Q.Head + N1 - 1) := [others => False];
      Q.Have (0 .. N2 - 1) := [others => False];
      --  Cleared: exactly the slots of offsets 0 .. Got - 1.
      pragma Assert
        (for all Off in 0 .. Capacity - 1 =>
           (if Off < Got then not Q.Have (Slot (Before, Off))
            else Q.Have (Slot (Before, Off)) = Before.Have (Slot (Before, Off))));

      Q.Head := (if Q.Head + Got < Capacity then Q.Head + Got else Q.Head + Got - Capacity);
      Q.Count := Q.Count - Got;
      Q.Start := Q.Start + Seq (Got);
      pragma Assert
        (for all Off in 0 .. Capacity - 1 - Got => Slot (Q, Off) = Slot (Before, Off + Got));
      pragma Assert
        (for all Off in Capacity - Got .. Capacity - 1 =>
           Slot (Q, Off) = Slot (Before, Off + Got - Capacity));
      pragma Assert
        (for all Off in 0 .. Capacity - 1 - Got => Present (Q, Off) = Present (Before, Off + Got));
      pragma Assert (for all Off in 0 .. Q.Count - 1 => Present (Before, Off + Got));
   end Read;
end TCP_Receive_Queue;
