pragma Ada_2022;
with Ada.Unchecked_Deallocation;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code; use System.Machine_Code;

package body Perf_Probe is
   ROUNDS : constant := 21;
   MEDIAN : constant := 11;
   type Sample_Index is range 1 .. ROUNDS;
   type Samples is array (Sample_Index) of Unsigned_64;

   type Word_Array is array (Natural range <>) of Unsigned_32;
   type Word_Access is access Word_Array;
   procedure Free is new Ada.Unchecked_Deallocation (Word_Array, Word_Access);

   function Image (V : Unsigned_64) return String is
      S : constant String := V'Image;
   begin
      return S (S'First + 1 .. S'Last);
   end Image;

   function TSC return Unsigned_64 is
      Lo, Hi : Unsigned_32;
   begin
      Asm ("lfence" & ASCII.LF & "rdtsc",
           Outputs => (Unsigned_32'Asm_Output ("=a", Lo), Unsigned_32'Asm_Output ("=d", Hi)),
           Volatile => True);
      return Shift_Left (Unsigned_64 (Hi), 32) or Unsigned_64 (Lo);
   end TSC;

   procedure Sort (S : in out Samples) is
      T : Unsigned_64;
      J : Sample_Index;
   begin
      for I in Sample_Index'First + 1 .. Sample_Index'Last loop
         J := I;
         while J > Sample_Index'First and then S (J - 1) > S (J) loop
            T := S (J);
            S (J) := S (J - 1);
            S (J - 1) := T;
            J := J - 1;
         end loop;
      end loop;
   end Sort;

   --  Durations in us; per-unit cost in picoseconds.
   procedure Report (Name : String; S : in out Samples; Units : Unsigned_64; Unit_Name : String) is
   begin
      Sort (S);
      Print ("perf011: " & Name & " us min=" & Image (S (1)) & " med=" & Image (S (MEDIAN))
             & " max=" & Image (S (ROUNDS))
             & (if Units > 0 then " ps/" & Unit_Name & " min=" & Image (S (1) * 1_000_000 / Units)
                & " med=" & Image (S (MEDIAN) * 1_000_000 / Units) else ""));
   end Report;

   --  Cycles (TSC) per operation, as a decimal with one place.
   function Per (Cycles, Count : Unsigned_64) return String is
     (Image (Cycles / Count) & "." & Image ((Cycles * 10 / Count) mod 10));

   Sink : Unsigned_64 := 0 with Volatile;

   procedure Run (Frame : CuBit.UI.Canvas) is
      W : constant Natural := Frame.width;
      H : constant Natural := Frame.height;
      Pixels : constant Unsigned_64 := Unsigned_64 (W) * Unsigned_64 (H);
      Stride : constant Natural := Frame.pitch / 4;
      Whole : constant CuBit.UI.Rect := (0, 0, W, H);
      S : Samples;
      T : Unsigned_64;
      Start_Us : constant Unsigned_64 := Now_Us;
      Start_TSC : constant Unsigned_64 := TSC;
   begin
      if Frame.addr = Null_Address or else W = 0 or else H = 0 then
         return;
      end if;
      Print ("perf011: frame " & Image (Unsigned_64 (W)) & "x" & Image (Unsigned_64 (H)) & " pitch="
             & Image (Unsigned_64 (Frame.pitch)) & " addr=" & Image (Unsigned_64 (To_Integer (Frame.addr)))
             & " addr_mod_4k=" & Image (Unsigned_64 (To_Integer (Frame.addr) mod 4096))
             & " addr_mod_2m=" & Image (Unsigned_64 (To_Integer (Frame.addr) mod 2 ** 21)));

      --  The clock: cost of a read and how often it changes.
      declare
         Calls : constant := 100_000;
         Changes : Unsigned_64 := 0;
         Last, Now : Unsigned_64 := Now_Us;
         C0 : constant Unsigned_64 := TSC;
         C1 : Unsigned_64;
      begin
         for I in 1 .. Calls loop
            Now := Now_Us;
            if Now /= Last then
               Changes := Changes + 1;
               Last := Now;
            end if;
         end loop;
         C1 := TSC;
         Print ("perf011: clock read cycles=" & Per (C1 - C0, Calls) & " changes=" & Image (Changes)
                & " over_cycles=" & Image (C1 - C0));
      end;

      --  The dashboard's full redraw, back to back.
      for I in Sample_Index loop
         T := Now_Us;
         Redraw;
         S (I) := Now_Us - T;
      end loop;
      Report ("full_redraw", S, Pixels, "px");

      --  The same redraw after the thread was idle (asleep), after a busy
      --  spin of the same length, and after this thread evicted the caches
      --  itself: which part of "cold" costs what.
      declare
         COLD : constant := 7;
         type Cold_Index is range 1 .. COLD;
         type Cold_Samples is array (Cold_Index) of Unsigned_64;
         Thrash : Word_Access := new Word_Array (0 .. 40 * 2 ** 18 - 1);  --  40 MiB
         Pages_Touch : Unsigned_64;
         procedure Show (Name : String; C : in out Cold_Samples) is
            T2 : Unsigned_64;
            J : Cold_Index;
         begin
            for I in Cold_Index'First + 1 .. Cold_Index'Last loop
               J := I;
               while J > Cold_Index'First and then C (J - 1) > C (J) loop
                  T2 := C (J);
                  C (J) := C (J - 1);
                  C (J - 1) := T2;
                  J := J - 1;
               end loop;
            end loop;
            Print ("perf011: cold " & Name & " redraw us min=" & Image (C (1)) & " med=" & Image (C (4))
                   & " max=" & Image (C (COLD)));
         end Show;
         procedure Evict (Words : Natural) is
         begin
            for J in 0 .. Words - 1 loop
               Thrash (J) := Unsigned_32 (J);
            end loop;
         end Evict;
         procedure Spin (Us : Unsigned_64) is
            Until_Us : constant Unsigned_64 := Now_Us + Us;
         begin
            while Now_Us < Until_Us loop
               null;
            end loop;
         end Spin;
         --  One load per page of the frame: what refilling its translations costs.
         function Touch_Frame return Unsigned_64 is
            Target : Word_Array (0 .. Stride * H - 1) with Import, Address => Frame.addr;
            Sum : Unsigned_32 := 0;
            C0 : constant Unsigned_64 := TSC;
         begin
            for J in 0 .. (Stride * H - 1) / 1024 loop
               Sum := Sum + Target (J * 1024);
            end loop;
            Sink := Unsigned_64 (Sum);
            return TSC - C0;
         end Touch_Frame;
         C : Cold_Samples;
         type Naps is array (1 .. 3) of Natural;
         NAP_MS : constant Naps := [1, 20, 200];
      begin
         Evict (Thrash'Length);
         for Ms of NAP_MS loop
            for I in Cold_Index loop
               Sleep_Ms (Ms);
               T := Now_Us;
               Redraw;
               C (I) := Now_Us - T;
            end loop;
            Show ("sleep_" & Image (Unsigned_64 (Ms)) & "ms", C);
         end loop;
         for I in Cold_Index loop
            Spin (200_000);
            T := Now_Us;
            Redraw;
            C (I) := Now_Us - T;
         end loop;
         Show ("spin_200ms", C);
         for I in Cold_Index loop
            Evict (Thrash'Length);
            T := Now_Us;
            Redraw;
            C (I) := Now_Us - T;
         end loop;
         Show ("evict_40MiB", C);
         for I in Cold_Index loop
            Evict (2 ** 18);
            T := Now_Us;
            Redraw;
            C (I) := Now_Us - T;
         end loop;
         Show ("evict_1MiB", C);
         Redraw;
         Pages_Touch := Touch_Frame;
         Print ("perf011: frame page touch warm cycles=" & Image (Pages_Touch));
         Sleep_Ms (20);
         Pages_Touch := Touch_Frame;
         Print ("perf011: frame page touch after sleep_20ms cycles=" & Image (Pages_Touch));
         Spin (20_000);
         Pages_Touch := Touch_Frame;
         Print ("perf011: frame page touch after spin_20ms cycles=" & Image (Pages_Touch));
         Free (Thrash);
      end;

      --  The toolkit's fill over the whole frame.
      for I in Sample_Index loop
         T := Now_Us;
         CuBit.UI.Fill_Rect (Frame, Whole, Unsigned_32 (I) * 16#010101#);
         S (I) := Now_Us - T;
      end loop;
      Report ("toolkit_fill", S, Pixels, "px");

      declare
         Target : Word_Array (0 .. Stride * H - 1) with Import, Address => Frame.addr;
      begin
         --  A plain store loop and a read-modify-write blend.
         for I in Sample_Index loop
            T := Now_Us;
            for Y in 0 .. H - 1 loop
               for X in 0 .. W - 1 loop
                  Target (Y * Stride + X) := Unsigned_32 (I) * 16#0F0F0F#;
               end loop;
            end loop;
            S (I) := Now_Us - T;
         end loop;
         Report ("loop_fill", S, Pixels, "px");
         for I in Sample_Index loop
            T := Now_Us;
            for Y in 0 .. H - 1 loop
               for X in 0 .. W - 1 loop
                  declare
                     P : constant Unsigned_32 := Target (Y * Stride + X);
                     A : constant Unsigned_32 := 160;
                     Ink : constant Unsigned_32 := 16#3080C0#;
                     R : constant Unsigned_32 :=
                       ((Shift_Right (P, 16) and 255) * (256 - A) + (Shift_Right (Ink, 16) and 255) * A) / 256;
                     G : constant Unsigned_32 :=
                       ((Shift_Right (P, 8) and 255) * (256 - A) + (Shift_Right (Ink, 8) and 255) * A) / 256;
                     B : constant Unsigned_32 := ((P and 255) * (256 - A) + (Ink and 255) * A) / 256;
                  begin
                     Target (Y * Stride + X) := 16#FF00_0000# or Shift_Left (R, 16) or Shift_Left (G, 8) or B;
                  end;
               end loop;
            end loop;
            S (I) := Now_Us - T;
         end loop;
         Report ("loop_blend", S, Pixels, "px");
      end;

      --  Text through the toolkit (glyphs cached after the first round).
      declare
         Line : constant String := "The quick brown fox 0123456789 jumps over";
         Lines : constant := 200;
      begin
         for I in Sample_Index loop
            T := Now_Us;
            for L in 0 .. Lines - 1 loop
               CuBit.UI.Draw_UI_Text_Transparent (Frame, 8, 8 + (L mod 30) * 18, Line, 16#FFE0E0E0#);
            end loop;
            S (I) := Now_Us - T;
         end loop;
         Report ("text", S, Unsigned_64 (Lines * Line'Length), "glyph");
      end;

      --  A fresh heap buffer of the frame's size: first touch, then warm.
      for Round in 1 .. 3 loop
         declare
            Buffer : Word_Access := new Word_Array (0 .. Natural (Pixels) - 1);
            First, Second : Unsigned_64;
         begin
            T := Now_Us;
            for J in Buffer'Range loop
               Buffer (J) := Unsigned_32 (J);
            end loop;
            First := Now_Us - T;
            T := Now_Us;
            for J in Buffer'Range loop
               Buffer (J) := Unsigned_32 (J) + 1;
            end loop;
            Second := Now_Us - T;
            Print ("perf011: fresh_buffer round=" & Image (Unsigned_64 (Round)) & " addr_mod_4k="
                   & Image (Unsigned_64 (To_Integer (Buffer.all'Address) mod 4096))
                   & " first_us=" & Image (First) & " second_us=" & Image (Second));
            Free (Buffer);
         end;
      end loop;

      --  Dependent loads, one per 4 KiB page, in a scattered page order:
      --  the cost of a load whose translation misses (page walks), over a
      --  frame-sized buffer and a 16 MiB one. Everything is pre-faulted.
      declare
         PAGE_WORDS : constant := 1024;
         Hops : constant := 2_000_000;
         procedure Chase (Pages : Positive; Name : String) is
            Buffer : Word_Access := new Word_Array (0 .. Pages * PAGE_WORDS - 1);
            Step : constant Natural := 1_000_003 mod Pages;  --  a prime stride
            Here : Natural := 0;
            C0, C1 : Unsigned_64;
         begin
            Buffer.all := [others => 0];
            --  Page P links to P + Step (mod Pages): one cycle through every page when
            --  Step and Pages are coprime; otherwise a shorter cycle, still scattered.
            for P in 0 .. Pages - 1 loop
               Buffer (P * PAGE_WORDS + (P mod 16) * 64) :=
                 Unsigned_32 (((P + (if Step = 0 then 1 else Step)) mod Pages) * PAGE_WORDS
                              + (((P + (if Step = 0 then 1 else Step)) mod Pages) mod 16) * 64);
            end loop;
            C0 := TSC;
            for I in 1 .. Hops loop
               Here := Natural (Buffer (Here));
            end loop;
            C1 := TSC;
            Sink := Unsigned_64 (Here);
            Print ("perf011: page_chase " & Name & " pages=" & Image (Unsigned_64 (Pages))
                   & " cycles_per_load=" & Per (C1 - C0, Hops));
            --  The same loads within one page's lines: cache, no translation misses.
            Buffer.all := [others => 0];
            for L in 0 .. 63 loop
               Buffer (L * 16) := Unsigned_32 (((L + 37) mod 64) * 16);
            end loop;
            Here := 0;
            C0 := TSC;
            for I in 1 .. Hops loop
               Here := Natural (Buffer (Here));
            end loop;
            C1 := TSC;
            Sink := Unsigned_64 (Here);
            Print ("perf011: line_chase " & Name & " cycles_per_load=" & Per (C1 - C0, Hops));
            Free (Buffer);
         end Chase;
      begin
         Chase (Natural (Pixels * 4 / 4096) + 1, "frame_sized");
         Chase (4096, "16MiB");
         Chase (48, "192KiB");
      end;

      --  Small copies (memmove) and secondary-stack returns.
      declare
         Src : Word_Access := new Word_Array (0 .. 4095);
         Dst : Word_Access := new Word_Array (0 .. 4095);
         Copies : constant := 200_000;
         C0, C1 : Unsigned_64;
         procedure Copy_Of (Words : Natural) is
         begin
            C0 := TSC;
            for I in 1 .. Copies loop
               declare
                  K : constant Natural := (I * 64) mod 2048;
               begin
                  Dst (K .. K + Words - 1) := Src (2048 - K .. 2048 - K + Words - 1);
               end;
            end loop;
            C1 := TSC;
            Print ("perf011: copy bytes=" & Image (Unsigned_64 (Words * 4)) & " cycles=" & Per (C1 - C0, Copies));
         end Copy_Of;
         --  Raw rep movsb (as the native runtime's memmove does) and a
         --  plain word loop, outside any runtime routine.
         procedure Rep_Movsb (Bytes : Natural) is
            D, S2 : System.Address;
            N : Unsigned_64;
         begin
            C0 := TSC;
            for I in 1 .. Copies loop
               declare
                  K : constant Natural := (I * 64) mod 2048;
               begin
                  D := Dst (K)'Address;
                  S2 := Src (2048 - K)'Address;
                  N := Unsigned_64 (Bytes);
                  Asm ("rep movsb",
                       Outputs => (System.Address'Asm_Output ("+D", D), System.Address'Asm_Output ("+S", S2),
                                   Unsigned_64'Asm_Output ("+c", N)),
                       Clobber => "memory", Volatile => True);
               end;
            end loop;
            C1 := TSC;
            Print ("perf011: rep_movsb bytes=" & Image (Unsigned_64 (Bytes)) & " cycles=" & Per (C1 - C0, Copies));
         end Rep_Movsb;
         function Made (N : Natural) return String is [1 .. N => 'x'];
         Total : Natural := 0;
      begin
         Src.all := [others => 7];
         Copy_Of (W / W * 4);
         Copy_Of (W / W * 16);
         Copy_Of (W / W * 64);
         Copy_Of (W / W * 256);
         Copy_Of (W / W * 1024);
         Rep_Movsb (16);
         Rep_Movsb (64);
         Rep_Movsb (172);
         Rep_Movsb (1024);
         Rep_Movsb (4096);
         C0 := TSC;
         for I in 1 .. Copies loop
            Total := Total + Made (8 + I mod 24)'Length;
         end loop;
         C1 := TSC;
         Sink := Unsigned_64 (Total);
         Print ("perf011: secondary_stack_string cycles=" & Per (C1 - C0, Copies));
         C0 := TSC;
         for I in 1 .. Copies / 10 loop
            declare
               Small : Word_Access := new Word_Array (0 .. 15 + I mod 48);
            begin
               Small (0) := 1;
               Free (Small);
            end;
         end loop;
         C1 := TSC;
         Print ("perf011: heap_new_free cycles=" & Per (C1 - C0, Copies / 10));
         Free (Src);
         Free (Dst);
      end;

      declare
         End_Us : constant Unsigned_64 := Now_Us;
         End_TSC : constant Unsigned_64 := TSC;
      begin
         Print ("perf011: tsc_per_us=" & Per (End_TSC - Start_TSC, Unsigned_64'Max (1, End_Us - Start_Us))
                & " probe_us=" & Image (End_Us - Start_Us));
      end;
      Redraw;
   end Run;
end Perf_Probe;
