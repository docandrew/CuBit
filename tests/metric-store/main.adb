--  Hosted tests of metrics.svc's series store (MET-002): the codecs'
--  round trips, the sketch's bucket enclosure and relative error, the
--  string table, and the store's raw chunks, tiers, gaps, selection and
--  ceilings. Run by tests/metric-store/run.sh.
pragma Ada_2022;
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Interfaces; use Interfaces;
with CuBit.Metric_Outlet;
with CuAlloc_Host;
with Metric_Codecs;
with Metric_Sketches;
with Metric_Strings;
with Metric_Series;

procedure Main is
   package Codecs renames Metric_Codecs;
   package Sketches renames Metric_Sketches;
   package Series renames Metric_Series;
   package Outlet renames CuBit.Metric_Outlet;

   Checks, Failures : Natural := 0;
   procedure Check (Condition : Boolean; Name : String);
   procedure Check (Condition : Boolean; Name : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Name);
      end if;
   end Check;

   package Random_Words is new Ada.Numerics.Discrete_Random (Unsigned_64);
   Generator : Random_Words.Generator;

   procedure Test_Codecs;
   procedure Test_Codecs is
      Buffer : Codecs.Byte_Array (0 .. 31) := [others => 0];
      Value : Unsigned_64;
      Length : Codecs.Varint_Length;
      Valid : Boolean;
      procedure One (V : Unsigned_64);
      procedure One (V : Unsigned_64) is
      begin
         Check (Codecs.Unzigzag (Codecs.Zigzag (V)) = V, "zigzag round trip");
         Codecs.Put_Varint (Buffer, 3, V);
         Codecs.Get_Varint (Buffer, 3, Value, Length, Valid);
         Check (Valid and then Value = V and then Length = Codecs.Varint_Bytes (V), "varint round trip");
      end One;
   begin
      for V in Unsigned_64 range 0 .. 70_000 loop
         One (V);
      end loop;
      for Bit in 0 .. 63 loop
         One (Shift_Left (1, Bit));
         One (Shift_Left (1, Bit) - 1);
      end loop;
      for Round in 1 .. 100_000 loop
         One (Random_Words.Random (Generator));
      end loop;
      Check (Codecs.Zigzag (Unsigned_64'Last) = 1, "zigzag -1 is 1");
      --  A truncated varint is refused.
      Buffer (30) := Codecs.More_Bit;
      Buffer (31) := Codecs.More_Bit;
      Codecs.Get_Varint (Buffer, 30, Value, Length, Valid);
      Check (not Valid, "a varint past the end is refused");
   end Test_Codecs;

   --  The stdmetric outlet's elements: every kind round trips, every
   --  malformed element is refused with its reason.
   procedure Test_Outlet;
   procedure Test_Outlet is
      use type Outlet.Decode_Error;
      use type Outlet.Element;
      Bytes : Outlet.Element_Buffer;
      Length : Outlet.Element_Length;
      D : Outlet.Element (Outlet.Describe);
      Name : constant String := "desktop.latency.input_to_ready";
      function Round_Trips (Item : Outlet.Element) return Boolean is
         Back : Outlet.Decoded;
      begin
         Outlet.Encode (Item, Bytes, Length);
         Back := Outlet.Decode (Bytes, Length);
         return Back.Success and then Back.Value = Item and then Length = Outlet.Length_Of (Item);
      end Round_Trips;
      function Refused (Why : Outlet.Decode_Error) return Boolean is
         Back : constant Outlet.Decoded := Outlet.Decode (Bytes, Length);
      begin
         return not Back.Success and then Back.Why = Why;
      end Refused;
   begin
      D.Key := 7;
      D.Series := Outlet.Distribution;
      D.Measure := Outlet.Microseconds;
      D.Length := Name'Length;
      D.Name (1 .. Name'Length) := Name;
      Check (Round_Trips (D), "describe round trip");
      Check (Round_Trips ((Kind => Outlet.Sample, Key => Outlet.Series_Key'Last, Time_Us => Unsigned_64'Last,
                           Value => 12_345)), "sample round trip");
      Check (Round_Trips ((Kind => Outlet.Loss, Key => 1, Shed => 3, First_Us => 10, Last_Us => 20)),
             "loss round trip");
      Check (Outlet.Length_Of (D) = Outlet.Header_Bytes + Name'Length, "describe length");
      --  A one-letter name: shorter than a sample, still an element.
      D.Length := 1;
      D.Name := [others => ' '];
      D.Name (1) := 'x';
      Check (Round_Trips (D), "a short describe round trips");
      D.Length := Name'Length;
      D.Name (1 .. Name'Length) := Name;
      --  Malformed: a key of zero, an unknown kind, a bad unit, an upper-case
      --  name, a wrong length, a reversed loss.
      Outlet.Encode ((Kind => Outlet.Sample, Key => 1, Time_Us => 1, Value => 1), Bytes, Length);
      Bytes (5) := 0;
      Check (Refused (Outlet.Bad_Key), "key zero refused");
      Bytes (5) := 1;
      Bytes (1) := 9;
      Check (Refused (Outlet.Unknown_Kind), "unknown kind refused");
      Outlet.Encode (D, Bytes, Length);
      Bytes (10) := 99;
      Check (Refused (Outlet.Bad_Unit), "unknown unit refused");
      Outlet.Encode (D, Bytes, Length);
      Bytes (Outlet.Header_Bytes + 1) := Character'Pos ('D');
      Check (Refused (Outlet.Bad_Name), "a name outside the charset refused");
      Outlet.Encode (D, Bytes, Length);
      Bytes (12) := 1;
      Check (Refused (Outlet.Nonzero_Reserved), "reserved bytes refused");
      Outlet.Encode (D, Bytes, Length);
      Length := Length - 1;
      Check (Refused (Outlet.Bad_Length), "a short describe refused");
      Outlet.Encode ((Kind => Outlet.Loss, Key => 1, Shed => 3, First_Us => 10, Last_Us => 20), Bytes, Length);
      Bytes (17) := 255;
      Bytes (18) := 255;
      Check (Refused (Outlet.Bad_Loss), "a loss that ends before it starts refused");
      for Random_Round in 1 .. 10_000 loop
         declare
            Junk : Outlet.Element_Buffer;
            Back : Outlet.Decoded;
         begin
            for B of Junk loop
               B := Unsigned_8 (Random_Words.Random (Generator) mod 256);
            end loop;
            Back := Outlet.Decode (Junk, Natural (Random_Words.Random (Generator) mod 100));
            if Back.Success and then not Outlet.Valid (Back.Value) then
               Check (False, "a decoded element is valid");
            end if;
         end;
      end loop;
   end Test_Outlet;

   procedure Test_Sketches;
   procedure Test_Sketches is
      Bound : constant := 2 ** 22;
      procedure Enclose (V : Unsigned_64);
      procedure Enclose (V : Unsigned_64) is
         B : constant Sketches.Bucket := Sketches.Bucket_Of (V);
      begin
         if Sketches.Lower (B) > V or else V > Sketches.Upper (B) then
            Check (False, "bucket encloses" & V'Image);
         end if;
         --  Upper - Lower within Lower / Sub_Buckets.
         if Sketches.Upper (B) - Sketches.Lower (B) > Sketches.Lower (B) / Sketches.Sub_Buckets then
            Check (False, "relative error bound at" & V'Image);
         end if;
      end Enclose;
      D : Sketches.Dense;
   begin
      for V in Unsigned_64 range 0 .. Bound loop
         Enclose (V);
      end loop;
      for B in Sketches.Bucket loop
         Enclose (Sketches.Lower (B));
         Enclose (Sketches.Upper (B));
         if B > 0 then
            Check (Sketches.Lower (B) = Sketches.Upper (B - 1) + 1, "buckets are contiguous");
         end if;
      end loop;
      Check (Sketches.Upper (Sketches.Bucket'Last) = Unsigned_64'Last, "the last bucket ends the word");
      for Round in 1 .. 200_000 loop
         Enclose (Random_Words.Random (Generator));
      end loop;
      Checks := Checks + 1;
      --  1..10_000 once each: p50 and p99 within the relative error.
      for V in Unsigned_64 range 1 .. 10_000 loop
         Sketches.Add (D, V);
      end loop;
      Check (D.Count = 10_000 and then D.Sum = 50_005_000 and then D.Minimum = 1 and then D.Maximum = 10_000,
             "dense sketch counts");
      Check (Sketches.Quantile (D, 500) in 5_000 .. 5_000 + 5_000 / Sketches.Sub_Buckets + 1, "p50");
      Check (Sketches.Quantile (D, 990) in 9_900 .. 10_000, "p99");
      Check (Sketches.Quantile (D, 1000) = 10_000, "p100 is the maximum");
   end Test_Sketches;

   procedure Test_Strings;
   procedure Test_Strings is
      T : Metric_Strings.Table;
      A, B, C : Metric_Strings.String_Id;
   begin
      Metric_Strings.Intern (T, "desktop.latency.input_to_ready", A);
      Metric_Strings.Intern (T, "com.cubit.desktop", B);
      Metric_Strings.Intern (T, "desktop.latency.input_to_ready", C);
      Check (A /= Metric_Strings.No_String and then A = C and then A /= B, "interned once");
      Check (Metric_Strings.Text_Of (T, B) = "com.cubit.desktop", "text back");
      for I in 1 .. 5_000 loop
         Metric_Strings.Intern (T, "s" & I'Image, C);
         Check (C /= Metric_Strings.No_String, "many strings");
      end loop;
      Check (Metric_Strings.Find (T, "s 4999") /= Metric_Strings.No_String, "found after growth");
      Check (Metric_Strings.Find (T, "absent") = Metric_Strings.No_String, "absent not found");
      Metric_Strings.Intern (T, "", C);
      Check (C = Metric_Strings.No_String and then Metric_Strings.Refused (T) = 1, "empty refused");
   end Test_Strings;

   procedure Test_Store;
   procedure Test_Store is
      S : Series.Store;
      L, L2 : Series.Labels;
      Index, Other : Series.Series_Count;
      Same : Boolean;
      Period : constant := 4_167;
      Samples : constant := 50_000;
      Start : constant Unsigned_64 := 1_000_000_000;
      Seen : Natural := 0;
      Mismatch : Natural := 0;
      procedure Visit (Time_Us, Value : Unsigned_64; Continue : out Boolean);
      procedure Visit (Time_Us, Value : Unsigned_64; Continue : out Boolean) is
         Expected_Time : constant Unsigned_64 := Start + Unsigned_64 (Seen) * Period;
      begin
         if Time_Us /= Expected_Time or else Value /= Unsigned_64 (Seen mod 997) then
            Mismatch := Mismatch + 1;
         end if;
         Seen := Seen + 1;
         Continue := True;
      end Visit;
      procedure Read_All is new Series.Read_Raw (Visit);
      Windows : Natural := 0;
      Window_Samples : Unsigned_64 := 0;
      procedure Each_Window (Row : Series.Window_Row; Continue : out Boolean);
      procedure Each_Window (Row : Series.Window_Row; Continue : out Boolean) is
      begin
         Windows := Windows + 1;
         Window_Samples := Window_Samples + Row.Count;
         Continue := True;
      end Each_Window;
      procedure Read_Seconds is new Series.Read_Windows (Each_Window);
      Gap_Count : Unsigned_64 := 0;
      procedure Each_Gap (Item : Series.Gap; Continue : out Boolean);
      procedure Each_Gap (Item : Series.Gap; Continue : out Boolean) is
      begin
         Gap_Count := Gap_Count + Item.Count;
         Continue := True;
      end Each_Gap;
      procedure Read_Gaps is new Series.Read_Gaps (Each_Gap);
      D : Sketches.Dense;
      Resolution, Missing : Unsigned_64;
      Sel : Series.Selector;
      Found : Series.Series_Count;
      Info : Series.Series_Info;
   begin
      Series.Intern (S, "desktop.latency.input_to_ready", L.Name);
      Series.Intern (S, "com.cubit.desktop", L.Source);
      Series.Intern (S, "desktop", L.Program);
      Series.Intern (S, "12", L.Instance);
      Series.Intern (S, "local", L.Node);
      Series.Find_Or_Add (S, L, Outlet.Distribution, Outlet.Microseconds, Index, Same);
      Check (Index = 1 and then Same, "first series");
      Series.Find_Or_Add (S, L, Outlet.Distribution, Outlet.Microseconds, Other, Same);
      Check (Other = Index and then Same, "same labels, same series");
      Series.Find_Or_Add (S, L, Outlet.Counter, Outlet.Count, Other, Same);
      Check (Other = Series.No_Series and then not Same, "same labels, other kind: refused");
      for I in 0 .. Samples - 1 loop
         Series.Append (S, Index, Start + Unsigned_64 (I) * Period, Unsigned_64 (I mod 997));
      end loop;
      Info := Series.Info (S, Index);
      Check (Info.Published = Samples and then Info.Retained = Samples and then Info.Evicted = 0,
             "every sample kept raw");
      Read_All (S, Index, 0, Unsigned_64'Last);
      Check (Seen = Samples and then Mismatch = 0, "raw read back exactly");
      Ada.Text_IO.Put_Line ("metric-store: raw bytes for" & Samples'Image & " samples at a steady rate:" &
                            Info.Raw_Bytes'Image & "; store bytes held:" & Series.Bytes_Held (S)'Image);
      Check (Info.Raw_Bytes <= 2 * Samples, "a steady rate and small steps: 2 bytes a sample at most");
      Read_Seconds (S, Index, Series.Second, 0, Unsigned_64'Last);
      Check (Windows > 200 and then Window_Samples = Samples, "1 s windows hold every sample");
      Series.Summarize (S, Index, 0, Unsigned_64'Last, D, Resolution, Missing);
      Check (D.Count = Samples and then Resolution = 0 and then Missing = 0, "summary from raw");
      Check (D.Maximum = 996 and then Sketches.Quantile (D, 500) in 498 .. 498 + 498 / 32 + 1,
             "p50 within the relative error");
      --  A producer's loss and the store's refusal are gaps.
      Series.Note_Shed (S, Index, 17, Start, Start + 100);
      Read_Gaps (S, Index, 0, Unsigned_64'Last);
      Check (Gap_Count = 17 and then Series.Info (S, Index).Shed = 17, "shed samples are a gap");
      for G in 1 .. 1_000 loop
         Series.Note_Shed (S, Index, 1, Start + Unsigned_64 (G), Start + Unsigned_64 (G));
      end loop;
      Gap_Count := 0;
      Read_Gaps (S, Index, 0, Unsigned_64'Last);
      Check (Gap_Count = 1_017 and then Series.Info (S, Index).Gaps <= Series.Maximum_Gaps_Per_Series,
             "gaps past the ceiling merge, counts kept");
      --  Selection: an exact name and a source intersect posting lists.
      L2 := L;
      Series.Intern (S, "13", L2.Instance);
      Series.Find_Or_Add (S, L2, Outlet.Distribution, Outlet.Microseconds, Other, Same);
      Series.Intern (S, "com.cubit.calculator", L2.Source);
      Series.Find_Or_Add (S, L2, Outlet.Distribution, Outlet.Microseconds, Other, Same);
      Check (Series.Count (S) = 3, "three series");
      Sel.Match := Series.Exact_Name;
      Sel.Name (1 .. 30) := "desktop.latency.input_to_ready";
      Sel.Name_Length := 30;
      Sel.Source (1 .. 17) := "com.cubit.desktop";
      Sel.Source_Length := 17;
      Series.Next_Selected (S, Sel, 0, Found);
      Check (Found = 1, "first selected");
      Series.Next_Selected (S, Sel, Found, Found);
      Check (Found = 2, "second selected (other instance)");
      Series.Next_Selected (S, Sel, Found, Found);
      Check (Found = Series.No_Series, "the calculator's is not the desktop's");
      Sel.Match := Series.Name_Prefix;
      Sel.Name_Length := 8;
      Sel.Source_Length := 0;
      Series.Next_Selected (S, Sel, 2, Found);
      Check (Found = 3, "prefix selects across sources");
      Check (Series.Find_Id (S, Series.Info (S, 2).Id) = 2, "found by ID");
      --  Out-of-order and backwards samples are kept raw.
      Series.Append (S, 3, 500, 5);
      Series.Append (S, 3, 400, 4);
      Seen := 0;
      declare
         Times : array (1 .. 2) of Unsigned_64 := [0, 0];
         procedure Two (Time_Us, Value : Unsigned_64; Continue : out Boolean);
         procedure Two (Time_Us, Value : Unsigned_64; Continue : out Boolean) is
            pragma Unreferenced (Value);
         begin
            Seen := Seen + 1;
            if Seen <= 2 then
               Times (Seen) := Time_Us;
            end if;
            Continue := True;
         end Two;
         procedure Read_Two is new Series.Read_Raw (Two);
      begin
         Read_Two (S, 3, 0, Unsigned_64'Last);
         Check (Seen = 2 and then Times = [500, 400], "out of order kept in order of arrival");
      end;
      --  The heap refusing: a new series is refused and counted.
      CuAlloc_Host.Refusing := True;
      for I in 1 .. 200 loop
         Series.Intern (S, "refused" & I'Image, L2.Instance);
         exit when L2.Instance = Metric_Strings.No_String;
         Series.Find_Or_Add (S, L2, Outlet.Gauge, Outlet.Count, Other, Same);
         exit when Other = Series.No_Series;
      end loop;
      CuAlloc_Host.Refusing := False;
      Check (Series.Series_Refused (S) > 0 or else L2.Instance = Metric_Strings.No_String,
             "a refused growth is counted");
   end Test_Store;
begin
   Random_Words.Reset (Generator, 20261114);
   Test_Codecs;
   Test_Outlet;
   Test_Sketches;
   Test_Strings;
   Test_Store;
   Ada.Text_IO.Put_Line ("metric-store:" & Checks'Image & " checks," & Failures'Image & " failures");
   if Failures > 0 then
      raise Program_Error;
   end if;
end Main;
