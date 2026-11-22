pragma Ada_2022;
package body Pool_Region_Maps with SPARK_Mode is

   function Joined return Region_Map is
     ((Free => [others => True], Clean => [others => True]));

   -- One procedure per run length (literal steps), so every proof
   -- obligation is linear arithmetic.
   procedure Find_0
     (B : Bits; Hint : Frame_Offset; First : out Frame_Offset; Found : out Boolean)
   with Post => (if Found then Aligned (First, 0) and then Run_Set (B, First, 0))
   is
      Start : constant Natural := Hint / 1;
      Slot, Candidate : Natural;
      Ok : Boolean;
   begin
      First := 0;
      Found := False;
      for K in 0 .. 511 loop
         Slot := (if Start + K < 512 then Start + K else Start + K - 512);
         Candidate := Slot * 1;
         Ok := True;
         for I in 0 .. 0 loop
            if not B (Candidate + I) then
               Ok := False;
               exit;
            end if;
            pragma Loop_Invariant (for all F in Candidate .. Candidate + I => B (F));
         end loop;
         if Ok then
            First := Candidate;
            Found := True;
            return;
         end if;
      end loop;
   end Find_0;

   procedure Find_1
     (B : Bits; Hint : Frame_Offset; First : out Frame_Offset; Found : out Boolean)
   with Post => (if Found then Aligned (First, 1) and then Run_Set (B, First, 1))
   is
      Start : constant Natural := Hint / 2;
      Slot, Candidate : Natural;
      Ok : Boolean;
   begin
      First := 0;
      Found := False;
      for K in 0 .. 255 loop
         Slot := (if Start + K < 256 then Start + K else Start + K - 256);
         Candidate := Slot * 2;
         Ok := True;
         for I in 0 .. 1 loop
            if not B (Candidate + I) then
               Ok := False;
               exit;
            end if;
            pragma Loop_Invariant (for all F in Candidate .. Candidate + I => B (F));
         end loop;
         if Ok then
            First := Candidate;
            Found := True;
            return;
         end if;
      end loop;
   end Find_1;

   procedure Find_2
     (B : Bits; Hint : Frame_Offset; First : out Frame_Offset; Found : out Boolean)
   with Post => (if Found then Aligned (First, 2) and then Run_Set (B, First, 2))
   is
      Start : constant Natural := Hint / 4;
      Slot, Candidate : Natural;
      Ok : Boolean;
   begin
      First := 0;
      Found := False;
      for K in 0 .. 127 loop
         Slot := (if Start + K < 128 then Start + K else Start + K - 128);
         Candidate := Slot * 4;
         Ok := True;
         for I in 0 .. 3 loop
            if not B (Candidate + I) then
               Ok := False;
               exit;
            end if;
            pragma Loop_Invariant (for all F in Candidate .. Candidate + I => B (F));
         end loop;
         if Ok then
            First := Candidate;
            Found := True;
            return;
         end if;
      end loop;
   end Find_2;

   procedure Find_3
     (B : Bits; Hint : Frame_Offset; First : out Frame_Offset; Found : out Boolean)
   with Post => (if Found then Aligned (First, 3) and then Run_Set (B, First, 3))
   is
      Start : constant Natural := Hint / 8;
      Slot, Candidate : Natural;
      Ok : Boolean;
   begin
      First := 0;
      Found := False;
      for K in 0 .. 63 loop
         Slot := (if Start + K < 64 then Start + K else Start + K - 64);
         Candidate := Slot * 8;
         Ok := True;
         for I in 0 .. 7 loop
            if not B (Candidate + I) then
               Ok := False;
               exit;
            end if;
            pragma Loop_Invariant (for all F in Candidate .. Candidate + I => B (F));
         end loop;
         if Ok then
            First := Candidate;
            Found := True;
            return;
         end if;
      end loop;
   end Find_3;

   procedure Find_4
     (B : Bits; Hint : Frame_Offset; First : out Frame_Offset; Found : out Boolean)
   with Post => (if Found then Aligned (First, 4) and then Run_Set (B, First, 4))
   is
      Start : constant Natural := Hint / 16;
      Slot, Candidate : Natural;
      Ok : Boolean;
   begin
      First := 0;
      Found := False;
      for K in 0 .. 31 loop
         Slot := (if Start + K < 32 then Start + K else Start + K - 32);
         Candidate := Slot * 16;
         Ok := True;
         for I in 0 .. 15 loop
            if not B (Candidate + I) then
               Ok := False;
               exit;
            end if;
            pragma Loop_Invariant (for all F in Candidate .. Candidate + I => B (F));
         end loop;
         if Ok then
            First := Candidate;
            Found := True;
            return;
         end if;
      end loop;
   end Find_4;

   procedure Find_5
     (B : Bits; Hint : Frame_Offset; First : out Frame_Offset; Found : out Boolean)
   with Post => (if Found then Aligned (First, 5) and then Run_Set (B, First, 5))
   is
      Start : constant Natural := Hint / 32;
      Slot, Candidate : Natural;
      Ok : Boolean;
   begin
      First := 0;
      Found := False;
      for K in 0 .. 15 loop
         Slot := (if Start + K < 16 then Start + K else Start + K - 16);
         Candidate := Slot * 32;
         Ok := True;
         for I in 0 .. 31 loop
            if not B (Candidate + I) then
               Ok := False;
               exit;
            end if;
            pragma Loop_Invariant (for all F in Candidate .. Candidate + I => B (F));
         end loop;
         if Ok then
            First := Candidate;
            Found := True;
            return;
         end if;
      end loop;
   end Find_5;

   procedure Find_6
     (B : Bits; Hint : Frame_Offset; First : out Frame_Offset; Found : out Boolean)
   with Post => (if Found then Aligned (First, 6) and then Run_Set (B, First, 6))
   is
      Start : constant Natural := Hint / 64;
      Slot, Candidate : Natural;
      Ok : Boolean;
   begin
      First := 0;
      Found := False;
      for K in 0 .. 7 loop
         Slot := (if Start + K < 8 then Start + K else Start + K - 8);
         Candidate := Slot * 64;
         Ok := True;
         for I in 0 .. 63 loop
            if not B (Candidate + I) then
               Ok := False;
               exit;
            end if;
            pragma Loop_Invariant (for all F in Candidate .. Candidate + I => B (F));
         end loop;
         if Ok then
            First := Candidate;
            Found := True;
            return;
         end if;
      end loop;
   end Find_6;

   procedure Find_7
     (B : Bits; Hint : Frame_Offset; First : out Frame_Offset; Found : out Boolean)
   with Post => (if Found then Aligned (First, 7) and then Run_Set (B, First, 7))
   is
      Start : constant Natural := Hint / 128;
      Slot, Candidate : Natural;
      Ok : Boolean;
   begin
      First := 0;
      Found := False;
      for K in 0 .. 3 loop
         Slot := (if Start + K < 4 then Start + K else Start + K - 4);
         Candidate := Slot * 128;
         Ok := True;
         for I in 0 .. 127 loop
            if not B (Candidate + I) then
               Ok := False;
               exit;
            end if;
            pragma Loop_Invariant (for all F in Candidate .. Candidate + I => B (F));
         end loop;
         if Ok then
            First := Candidate;
            Found := True;
            return;
         end if;
      end loop;
   end Find_7;

   procedure Find_8
     (B : Bits; Hint : Frame_Offset; First : out Frame_Offset; Found : out Boolean)
   with Post => (if Found then Aligned (First, 8) and then Run_Set (B, First, 8))
   is
      Start : constant Natural := Hint / 256;
      Slot, Candidate : Natural;
      Ok : Boolean;
   begin
      First := 0;
      Found := False;
      for K in 0 .. 1 loop
         Slot := (if Start + K < 2 then Start + K else Start + K - 2);
         Candidate := Slot * 256;
         Ok := True;
         for I in 0 .. 255 loop
            if not B (Candidate + I) then
               Ok := False;
               exit;
            end if;
            pragma Loop_Invariant (for all F in Candidate .. Candidate + I => B (F));
         end loop;
         if Ok then
            First := Candidate;
            Found := True;
            return;
         end if;
      end loop;
   end Find_8;

   procedure Find_9
     (B : Bits; Hint : Frame_Offset; First : out Frame_Offset; Found : out Boolean)
   with Post => (if Found then Aligned (First, 9) and then Run_Set (B, First, 9))
   is
      Start : constant Natural := Hint / 512;
      Slot, Candidate : Natural;
      Ok : Boolean;
   begin
      First := 0;
      Found := False;
      for K in 0 .. 0 loop
         Slot := (if Start + K < 1 then Start + K else Start + K - 1);
         Candidate := Slot * 512;
         Ok := True;
         for I in 0 .. 511 loop
            if not B (Candidate + I) then
               Ok := False;
               exit;
            end if;
            pragma Loop_Invariant (for all F in Candidate .. Candidate + I => B (F));
         end loop;
         if Ok then
            First := Candidate;
            Found := True;
            return;
         end if;
      end loop;
   end Find_9;

   procedure Find
     (B : Bits; Order : Run_Order; Hint : Frame_Offset;
      First : out Frame_Offset; Found : out Boolean) is
   begin
      case Order is
         when 0 => Find_0 (B, Hint, First, Found);
         when 1 => Find_1 (B, Hint, First, Found);
         when 2 => Find_2 (B, Hint, First, Found);
         when 3 => Find_3 (B, Hint, First, Found);
         when 4 => Find_4 (B, Hint, First, Found);
         when 5 => Find_5 (B, Hint, First, Found);
         when 6 => Find_6 (B, Hint, First, Found);
         when 7 => Find_7 (B, Hint, First, Found);
         when 8 => Find_8 (B, Hint, First, Found);
         when 9 => Find_9 (B, Hint, First, Found);
      end case;
   end Find;

   procedure Take (M : in out Region_Map; First : Frame_Offset; Order : Run_Order) is
      Last : constant Frame_Offset := First + Run_Frames (Order) - 1;
   begin
      for F in First .. Last loop
         M.Free (F) := False;
         M.Clean (F) := False;
         pragma Loop_Invariant (for all G in First .. F => not M.Free (G) and then not M.Clean (G));
         pragma Loop_Invariant
           (for all G in Frame_Offset =>
              (if G < First or else G > F then
                 M.Free (G) = M'Loop_Entry.Free (G) and then M.Clean (G) = M'Loop_Entry.Clean (G)));
      end loop;
   end Take;

   procedure Give (M : in out Region_Map; First : Frame_Offset; Order : Run_Order) is
      Last : constant Frame_Offset := First + Run_Frames (Order) - 1;
   begin
      for F in First .. Last loop
         M.Free (F) := True;
         M.Clean (F) := False;
         pragma Loop_Invariant (for all G in First .. F => M.Free (G) and then not M.Clean (G));
         pragma Loop_Invariant
           (for all G in Frame_Offset =>
              (if G < First or else G > F then
                 M.Free (G) = M'Loop_Entry.Free (G) and then M.Clean (G) = M'Loop_Entry.Clean (G)));
      end loop;
   end Give;

   procedure Mark_Clean (M : in out Region_Map; F : Frame_Offset) is
   begin
      M.Clean (F) := True;
   end Mark_Clean;
end Pool_Region_Maps;
