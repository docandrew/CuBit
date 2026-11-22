pragma Ada_2022;
with Heap_Growth;

package body Metric_Series with SPARK_Mode => Off is
   use type Outlet.Series_Kind;
   use type Outlet.Unit;
   package Codecs renames Metric_Codecs;
   package Sketches renames Metric_Sketches;

   Initial_Series : constant := 64;
   Initial_Chunks : constant := 4;
   Initial_Windows : constant := 8;
   Initial_Gaps : constant := 4;
   Initial_Sketch : constant := 8;
   Initial_Postings : constant := 64;
   Initial_Posting_Items : constant := 8;
   --  Open-addressed tables stay at most half full.
   Load_Divisor : constant := 2;
   --  The most two varints of one sample take.
   Sample_Room : constant := 2 * Codecs.Maximum_Varint_Bytes;
   Byte_Bits : constant := 8;

   procedure Grow_Series is new Heap_Growth (Series_Blocks);
   procedure Grow_Entries is new Heap_Growth (Entry_Blocks);
   procedure Grow_Items is new Heap_Growth (Index_Blocks);

   function Add (A, B : Unsigned_64) return Unsigned_64 is
     (if B > Unsigned_64'Last - A then Unsigned_64'Last else A + B);
   function Sub (A, B : Unsigned_64) return Unsigned_64 is (if B > A then 0 else A - B);

   function Tier_For_Step (Step_Us : Unsigned_64) return Tier is
      Chosen : Tier := Second;
   begin
      for T in Tier loop
         if Width_Us (T) <= Step_Us then
            Chosen := T;
         end if;
      end loop;
      return Chosen;
   end Tier_For_Step;

   ---------------------------------------------------------------------------
   --  Strings and label keys
   ---------------------------------------------------------------------------
   procedure Ready_Keys (S : in out Store);
   procedure Ready_Keys (S : in out Store) is
   begin
      if S.Keys_Ready then
         return;
      end if;
      Metric_Strings.Intern (S.Strings, Name_Key, S.Keys.Name);
      Metric_Strings.Intern (S.Strings, Source_Key, S.Keys.Source);
      Metric_Strings.Intern (S.Strings, Program_Key, S.Keys.Program);
      Metric_Strings.Intern (S.Strings, Instance_Key, S.Keys.Instance);
      Metric_Strings.Intern (S.Strings, Node_Key, S.Keys.Node);
      S.Keys_Ready := S.Keys.Node /= Metric_Strings.No_String;
   end Ready_Keys;

   procedure Intern (S : in out Store; Text : String; Id : out Metric_Strings.String_Id) is
   begin
      Ready_Keys (S);
      Metric_Strings.Intern (S.Strings, Text, Id);
   end Intern;
   function Find_Text (S : Store; Text : String) return Metric_Strings.String_Id is
     (Metric_Strings.Find (S.Strings, Text));
   function Text_Of (S : Store; Id : Metric_Strings.String_Id) return String is
     (Metric_Strings.Text_Of (S.Strings, Id));

   --  The canonical label set, in label-name order (instance, name, node,
   --  program, source), hashed.
   function Label_Hash (S : Store; L : Labels) return Series_Id;
   function Label_Hash (S : Store; L : Labels) return Series_Id is
      H : Unsigned_64 := Metric_Strings.Offset_Basis;
      procedure Mix (Key : String; Value : Metric_Strings.String_Id);
      procedure Mix (Key : String; Value : Metric_Strings.String_Id) is
      begin
         H := Metric_Strings.Hash (Key & "=" & Text_Of (S, Value) & ASCII.NUL, H);
      end Mix;
   begin
      Mix (Instance_Key, L.Instance);
      Mix (Name_Key, L.Name);
      Mix (Node_Key, L.Node);
      Mix (Program_Key, L.Program);
      Mix (Source_Key, L.Source);
      return H and Id_Mask;
   end Label_Hash;

   ---------------------------------------------------------------------------
   --  Series ID index and posting lists
   ---------------------------------------------------------------------------
   function Get (S : Store; Index : Series_Index) return Series_Record is
     (Series_Blocks.Get (S.Series, Index - 1));
   procedure Put (S : in out Store; Index : Series_Index; Item : Series_Record);
   procedure Put (S : in out Store; Index : Series_Index; Item : Series_Record) is
   begin
      Series_Blocks.Set (S.Series, Index - 1, Item);
   end Put;

   function Id_Slot (S : Store; Id : Series_Id) return Natural;
   function Id_Slot (S : Store; Id : Series_Id) return Natural is
      Mask : constant Unsigned_64 := Unsigned_64 (Index_Blocks.Capacity (S.By_Id)) - 1;
      Slot : Natural := Natural (Id and Mask);
      Held : Series_Count;
   begin
      loop
         Held := Index_Blocks.Get (S.By_Id, Slot);
         exit when Held = No_Series or else Get (S, Held).Id = Id;
         Slot := Natural ((Unsigned_64 (Slot) + 1) and Mask);
      end loop;
      return Slot;
   end Id_Slot;

   function Find_Id (S : Store; Id : Series_Id) return Series_Count is
     (if Index_Blocks.Capacity (S.By_Id) = 0 then No_Series
      else Index_Blocks.Get (S.By_Id, Id_Slot (S, Id)));

   procedure Rehash_Ids (S : in out Store; Size : Positive; Done : out Boolean);
   procedure Rehash_Ids (S : in out Store; Size : Positive; Done : out Boolean) is
      Fresh : Index_Blocks.Block;
      Old : Index_Blocks.Block := S.By_Id;
   begin
      Index_Blocks.Allocate (Fresh, Size);
      Done := Index_Blocks.Capacity (Fresh) = Size;
      if not Done then
         return;
      end if;
      for I in 0 .. Size - 1 loop
         Index_Blocks.Set (Fresh, I, No_Series);
      end loop;
      S.By_Id := Fresh;
      for Index in 1 .. S.Used loop
         Index_Blocks.Set (S.By_Id, Id_Slot (S, Get (S, Index).Id), Index);
      end loop;
      Index_Blocks.Release (Old);
   end Rehash_Ids;

   function Posting_Slot (S : Store; Key, Value : Metric_Strings.String_Id) return Natural;
   function Posting_Slot (S : Store; Key, Value : Metric_Strings.String_Id) return Natural is
      Mask : constant Unsigned_64 := Unsigned_64 (Posting_Blocks.Capacity (S.Postings)) - 1;
      Slot : Natural :=
        Natural ((Unsigned_64 (Key) * Metric_Strings.Prime xor Unsigned_64 (Value)) and Mask);
      Held : Posting;
   begin
      loop
         Held := Posting_Blocks.Get (S.Postings, Slot);
         exit when Held.Key = Metric_Strings.No_String or else (Held.Key = Key and then Held.Value = Value);
         Slot := Natural ((Unsigned_64 (Slot) + 1) and Mask);
      end loop;
      return Slot;
   end Posting_Slot;

   procedure Rehash_Postings (S : in out Store; Size : Positive; Done : out Boolean);
   procedure Rehash_Postings (S : in out Store; Size : Positive; Done : out Boolean) is
      Fresh : Posting_Blocks.Block;
      Old : Posting_Blocks.Block := S.Postings;
      Old_Size : constant Natural := Posting_Blocks.Capacity (Old);
   begin
      Posting_Blocks.Allocate (Fresh, Size);
      Done := Posting_Blocks.Capacity (Fresh) = Size;
      if not Done then
         return;
      end if;
      for I in 0 .. Size - 1 loop
         Posting_Blocks.Set (Fresh, I, (others => <>));
      end loop;
      S.Postings := Fresh;
      for I in 0 .. Old_Size - 1 loop
         declare
            P : constant Posting := Posting_Blocks.Get (Old, I);
         begin
            if P.Key /= Metric_Strings.No_String then
               Posting_Blocks.Set (S.Postings, Posting_Slot (S, P.Key, P.Value), P);
            end if;
         end;
      end loop;
      Posting_Blocks.Release (Old);
   end Rehash_Postings;

   --  Add Index to the posting list of Key=Value (indexes only grow, so the
   --  list stays sorted). Done False when the heap refused.
   procedure Post (S : in out Store; Key, Value : Metric_Strings.String_Id; Index : Series_Index;
                   Done : out Boolean);
   procedure Post (S : in out Store; Key, Value : Metric_Strings.String_Id; Index : Series_Index;
                   Done : out Boolean)
   is
      Slot : Natural;
      P : Posting;
   begin
      Done := True;
      if Value = Metric_Strings.No_String then
         return;
      end if;
      if (S.Posting_Count + 1) * Load_Divisor > Posting_Blocks.Capacity (S.Postings) then
         Rehash_Postings
           (S, (if Posting_Blocks.Capacity (S.Postings) = 0 then Initial_Postings
                else 2 * Posting_Blocks.Capacity (S.Postings)), Done);
         if not Done then
            return;
         end if;
      end if;
      Slot := Posting_Slot (S, Key, Value);
      P := Posting_Blocks.Get (S.Postings, Slot);
      if P.Key = Metric_Strings.No_String then
         P := (Key => Key, Value => Value, Items => Index_Blocks.Empty, Used => 0);
         S.Posting_Count := S.Posting_Count + 1;
      end if;
      Grow_Items (P.Items, P.Used, Initial_Posting_Items, Maximum_Series, P.Used + 1, Done);
      if Done then
         Index_Blocks.Set (P.Items, P.Used, Index);
         P.Used := P.Used + 1;
      end if;
      Posting_Blocks.Set (S.Postings, Slot, P);
   end Post;

   function Posting_Of (S : Store; Key, Value : Metric_Strings.String_Id) return Posting is
     (if Posting_Blocks.Capacity (S.Postings) = 0 or else Value = Metric_Strings.No_String then (others => <>)
      else Posting_Blocks.Get (S.Postings, Posting_Slot (S, Key, Value)));

   ---------------------------------------------------------------------------
   --  Series
   ---------------------------------------------------------------------------
   procedure Find_Or_Add
     (S : in out Store; With_Labels : Labels; Kind : Outlet.Series_Kind; Measure : Outlet.Unit;
      Index : out Series_Count; Same_Kind : out Boolean)
   is
      Id : Series_Id;
      Found : Series_Count;
      Done : Boolean;
      Item : Series_Record;
   begin
      Ready_Keys (S);
      Index := No_Series;
      Same_Kind := True;
      Id := Label_Hash (S, With_Labels);
      --  The ID taken, unless by another label set (a collision): the next.
      loop
         Found := Find_Id (S, Id);
         exit when Found = No_Series;
         if Get (S, Found).With_Labels = With_Labels then
            Same_Kind := Get (S, Found).Kind = Kind and then Get (S, Found).Measure = Measure;
            Index := (if Same_Kind then Found else No_Series);
            return;
         end if;
         Id := (Id + 1) and Id_Mask;
      end loop;
      if S.Used = Maximum_Series then
         S.Refusals := Add (S.Refusals, 1);
         return;
      end if;
      Grow_Series (S.Series, S.Used, Initial_Series, Maximum_Series, S.Used + 1, Done);
      if Done and then (S.Used + 1) * Load_Divisor > Index_Blocks.Capacity (S.By_Id) then
         Rehash_Ids (S, Load_Divisor * Series_Blocks.Capacity (S.Series), Done);
      end if;
      if not Done then
         S.Refusals := Add (S.Refusals, 1);
         return;
      end if;
      Item.Id := Id;
      Item.With_Labels := With_Labels;
      Item.Kind := Kind;
      Item.Measure := Measure;
      S.Used := S.Used + 1;
      Put (S, S.Used, Item);
      Index_Blocks.Set (S.By_Id, Id_Slot (S, Id), S.Used);
      Post (S, S.Keys.Name, With_Labels.Name, S.Used, Done);
      if Done then
         Post (S, S.Keys.Source, With_Labels.Source, S.Used, Done);
      end if;
      if Done then
         Post (S, S.Keys.Program, With_Labels.Program, S.Used, Done);
      end if;
      if Done then
         Post (S, S.Keys.Instance, With_Labels.Instance, S.Used, Done);
      end if;
      if Done then
         Post (S, S.Keys.Node, With_Labels.Node, S.Used, Done);
      end if;
      if not Done then
         --  Kept, but not findable by every label: counted.
         S.Refusals := Add (S.Refusals, 1);
      end if;
      Index := S.Used;
   end Find_Or_Add;

   --  A gap, merged into the newest when the ring is at its ceiling (the
   --  two oldest are joined first), so no count is ever dropped.
   procedure Add_Gap (R : in out Series_Record; Item : Gap);
   procedure Add_Gap (R : in out Series_Record; Item : Gap) is
      Capacity : Natural := Gap_Blocks.Capacity (R.Gaps);
      function At_Ring (Position : Natural) return Natural is ((R.At_Gaps.First + Position) mod Capacity);
   begin
      if R.At_Gaps.Count = Capacity and then Capacity < Maximum_Gaps_Per_Series then
         --  Unroll the ring into a larger block.
         declare
            Fresh : Gap_Blocks.Block;
            Size : constant Natural := (if Capacity = 0 then Initial_Gaps else 2 * Capacity);
         begin
            Gap_Blocks.Allocate (Fresh, Size);
            if Gap_Blocks.Capacity (Fresh) = Size then
               for P in 0 .. R.At_Gaps.Count - 1 loop
                  Gap_Blocks.Set (Fresh, P, Gap_Blocks.Get (R.Gaps, At_Ring (P)));
               end loop;
               Gap_Blocks.Release (R.Gaps);
               R.Gaps := Fresh;
               R.At_Gaps.First := 0;
               Capacity := Size;
            end if;
         end;
      end if;
      if Capacity = 0 then
         return;
      end if;
      if R.At_Gaps.Count = Capacity then
         --  Join the two oldest: their range and their counts.
         declare
            Oldest : constant Gap := Gap_Blocks.Get (R.Gaps, At_Ring (0));
            Next : Gap := Gap_Blocks.Get (R.Gaps, At_Ring (1));
         begin
            Next.First_Us := Unsigned_64'Min (Oldest.First_Us, Next.First_Us);
            Next.Last_Us := Unsigned_64'Max (Oldest.Last_Us, Next.Last_Us);
            Next.Count := Add (Oldest.Count, Next.Count);
            Gap_Blocks.Set (R.Gaps, At_Ring (1), Next);
            R.At_Gaps.First := At_Ring (1);
            R.At_Gaps.Count := R.At_Gaps.Count - 1;
         end;
      end if;
      Gap_Blocks.Set (R.Gaps, At_Ring (R.At_Gaps.Count), Item);
      R.At_Gaps.Count := R.At_Gaps.Count + 1;
   end Add_Gap;

   procedure Note_Shed (S : in out Store; Index : Series_Index; Count, First_Us, Last_Us : Unsigned_64) is
      R : Series_Record := Get (S, Index);
   begin
      R.Shed := Add (R.Shed, Count);
      Add_Gap (R, (First_Us => First_Us, Last_Us => Last_Us, Count => Count, Reason => Shed_By_Producer));
      Put (S, Index, R);
   end Note_Shed;

   --  Seal R's head chunk into an exactly sized block, evicting R's oldest
   --  sealed chunk at its ceiling or at the store's. Sealed False: the store
   --  could not keep it (the head stays; the caller refuses the sample).
   procedure Release_Oldest (S : in out Store; R : in out Series_Record);
   procedure Release_Oldest (S : in out Store; R : in out Series_Record) is
      Capacity : constant Natural := Chunk_Blocks.Capacity (R.Sealed);
      Oldest : Sealed_Chunk := Chunk_Blocks.Get (R.Sealed, R.At_Sealed.First);
   begin
      S.Held := Sub (S.Held, Unsigned_64 (Byte_Blocks.Capacity (Oldest.Data)));
      R.Evicted := Add (R.Evicted, Unsigned_64 (Oldest.Header.Count));
      R.Raw_From_Us := Unsigned_64'Max (R.Raw_From_Us, Add (Oldest.Header.Max_Time, 1));
      Byte_Blocks.Release (Oldest.Data);
      Chunk_Blocks.Set (R.Sealed, R.At_Sealed.First, Oldest);
      R.At_Sealed.First := (R.At_Sealed.First + 1) mod Capacity;
      R.At_Sealed.Count := R.At_Sealed.Count - 1;
   end Release_Oldest;

   procedure Seal (S : in out Store; R : in out Series_Record; Sealed : out Boolean);
   procedure Seal (S : in out Store; R : in out Series_Record; Sealed : out Boolean) is
      Capacity : Natural := Chunk_Blocks.Capacity (R.Sealed);
      Data : Byte_Blocks.Block;
   begin
      Sealed := False;
      if R.At_Sealed.Count = Capacity and then Capacity < Maximum_Chunks_Per_Series then
         declare
            Fresh : Chunk_Blocks.Block;
            Size : constant Natural := (if Capacity = 0 then Initial_Chunks else 2 * Capacity);
         begin
            Chunk_Blocks.Allocate (Fresh, Size);
            if Chunk_Blocks.Capacity (Fresh) = Size then
               for P in 0 .. R.At_Sealed.Count - 1 loop
                  Chunk_Blocks.Set (Fresh, P, Chunk_Blocks.Get (R.Sealed, (R.At_Sealed.First + P) mod Capacity));
               end loop;
               Chunk_Blocks.Release (R.Sealed);
               R.Sealed := Fresh;
               R.At_Sealed.First := 0;
               Capacity := Size;
            end if;
         end;
      end if;
      if Capacity = 0 then
         return;
      end if;
      if R.At_Sealed.Count = Capacity or else
        (S.Held > Maximum_Store_Bytes - Chunk_Bytes and then R.At_Sealed.Count > 0)
      then
         Release_Oldest (S, R);
      end if;
      if S.Held > Maximum_Store_Bytes - Chunk_Bytes then
         return;
      end if;
      if R.Head.Used > 0 then
         Byte_Blocks.Allocate (Data, R.Head.Used);
         if Byte_Blocks.Capacity (Data) /= R.Head.Used then
            return;
         end if;
         for I in 0 .. R.Head.Used - 1 loop
            Byte_Blocks.Set (Data, I, R.Head_Bytes (I));
         end loop;
      end if;
      S.Held := Add (S.Held, Unsigned_64 (R.Head.Used));
      Chunk_Blocks.Set (R.Sealed, (R.At_Sealed.First + R.At_Sealed.Count) mod Capacity,
                        (Header => R.Head, Data => Data));
      R.At_Sealed.Count := R.At_Sealed.Count + 1;
      R.Head := (others => <>);
      Sealed := True;
   end Seal;

   --  Fold one sample into a window's sparse sketch (sorted by bucket).
   procedure Sketch_Add (S : in out Store; W : in out Window; Value : Unsigned_64);
   procedure Sketch_Add (S : in out Store; W : in out Window; Value : Unsigned_64) is
      B : constant Sketches.Bucket := Sketches.Bucket_Of (Value);
      Position : Natural := 0;
      Before : constant Natural := Entry_Blocks.Capacity (W.Sketch);
      Grown : Boolean;
   begin
      while Position < W.Sketch_Used and then
        Sketches.Bucket_Part (Entry_Blocks.Get (W.Sketch, Position)) < B
      loop
         Position := Position + 1;
      end loop;
      if Position < W.Sketch_Used and then Sketches.Bucket_Part (Entry_Blocks.Get (W.Sketch, Position)) = B then
         declare
            E : constant Sketches.Sparse_Entry := Entry_Blocks.Get (W.Sketch, Position);
         begin
            Entry_Blocks.Set (W.Sketch, Position, Sketches.Entry_Of (B, Add (Sketches.Count_Part (E), 1)));
         end;
         return;
      end if;
      Grow_Entries (W.Sketch, W.Sketch_Used, Initial_Sketch, Sketches.Bucket_Count, W.Sketch_Used + 1, Grown);
      if not Grown then
         --  The window's counts stay exact; only its sketch misses it.
         return;
      end if;
      S.Held := Add (S.Held, Unsigned_64 (Entry_Blocks.Capacity (W.Sketch) - Before) * Byte_Bits);
      for I in reverse Position .. W.Sketch_Used - 1 loop
         Entry_Blocks.Set (W.Sketch, I + 1, Entry_Blocks.Get (W.Sketch, I));
      end loop;
      Entry_Blocks.Set (W.Sketch, Position, Sketches.Entry_Of (B, 1));
      W.Sketch_Used := W.Sketch_Used + 1;
   end Sketch_Add;

   function Sketch_Of (W : Window) return Sketches.Dense;
   function Sketch_Of (W : Window) return Sketches.Dense is
      D : Sketches.Dense;
   begin
      for I in 0 .. W.Sketch_Used - 1 loop
         declare
            E : constant Sketches.Sparse_Entry := Entry_Blocks.Get (W.Sketch, I);
         begin
            D.Buckets (Sketches.Bucket_Part (E)) := Sketches.Count_Part (E);
            D.Count := Add (D.Count, Sketches.Count_Part (E));
         end;
      end loop;
      D.Minimum := W.Row.Minimum;
      D.Maximum := W.Row.Maximum;
      D.Sum := W.Row.Sum;
      return D;
   end Sketch_Of;

   Quantile_50 : constant := 500;
   Quantile_99 : constant := 990;

   procedure Fold (S : in out Store; R : in out Series_Record; Of_Tier : Tier; Time_Us, Value : Unsigned_64);
   procedure Fold (S : in out Store; R : in out Series_Record; Of_Tier : Tier; Time_Us, Value : Unsigned_64) is
      Ring : Window_Ring renames R.Windows (Of_Tier);
      Width : constant Unsigned_64 := Width_Us (Of_Tier);
      Start : constant Unsigned_64 := Time_Us - Time_Us mod Width;
      Capacity : Natural := Window_Blocks.Capacity (Ring.Items);
      Position : Natural;
      W : Window;
      Found : Boolean := False;
   begin
      --  Out-of-order samples land in a kept window, newest first.
      for Back in 0 .. Ring.At_Ring.Count - 1 loop
         Position := (Ring.At_Ring.First + Ring.At_Ring.Count - 1 - Back) mod Capacity;
         W := Window_Blocks.Get (Ring.Items, Position);
         exit when W.Row.Start_Us < Start;
         if W.Row.Start_Us = Start then
            Found := True;
            exit;
         end if;
      end loop;
      if not Found then
         if Ring.At_Ring.Count > 0 and then
           Window_Blocks.Get (Ring.Items, (Ring.At_Ring.First + Ring.At_Ring.Count - 1) mod Capacity).Row.Start_Us
             > Start
         then
            --  Older than a newer window yet between kept ones, or older
            --  than every kept window: the raw chunk keeps it; the tier
            --  does not insert into its past.
            return;
         end if;
         if Ring.At_Ring.Count = Capacity and then Capacity < Windows_Kept (Of_Tier) then
            declare
               Fresh : Window_Blocks.Block;
               Size : constant Natural :=
                 (if Capacity = 0 then Initial_Windows else Natural'Min (2 * Capacity, Windows_Kept (Of_Tier)));
            begin
               Window_Blocks.Allocate (Fresh, Size);
               if Window_Blocks.Capacity (Fresh) = Size then
                  for P in 0 .. Ring.At_Ring.Count - 1 loop
                     Window_Blocks.Set
                       (Fresh, P, Window_Blocks.Get (Ring.Items, (Ring.At_Ring.First + P) mod Capacity));
                  end loop;
                  Window_Blocks.Release (Ring.Items);
                  Ring.Items := Fresh;
                  Ring.At_Ring.First := 0;
                  S.Held := Add (S.Held, Unsigned_64 (Size - Capacity) * (Window'Size / Byte_Bits));
                  Capacity := Size;
               end if;
            end;
         end if;
         if Capacity = 0 then
            return;
         end if;
         if Ring.At_Ring.Count = Capacity then
            --  The oldest window ages out, its sketch with it.
            declare
               Oldest : Window := Window_Blocks.Get (Ring.Items, Ring.At_Ring.First);
            begin
               S.Held := Sub (S.Held, Unsigned_64 (Entry_Blocks.Capacity (Oldest.Sketch)) * Byte_Bits);
               Entry_Blocks.Release (Oldest.Sketch);
               Window_Blocks.Set (Ring.Items, Ring.At_Ring.First, Oldest);
               Ring.At_Ring.First := (Ring.At_Ring.First + 1) mod Capacity;
               Ring.At_Ring.Count := Ring.At_Ring.Count - 1;
            end;
         end if;
         Position := (Ring.At_Ring.First + Ring.At_Ring.Count) mod Capacity;
         W := (Row => (Start_Us => Start, Minimum => Unsigned_64'Last, others => 0), others => <>);
         Ring.At_Ring.Count := Ring.At_Ring.Count + 1;
      end if;
      W.Row.Count := Add (W.Row.Count, 1);
      W.Row.Sum := Add (W.Row.Sum, Value);
      W.Row.Minimum := Unsigned_64'Min (W.Row.Minimum, Value);
      W.Row.Maximum := Unsigned_64'Max (W.Row.Maximum, Value);
      W.Row.Last := Value;
      if R.Kind = Outlet.Distribution then
         Sketch_Add (S, W, Value);
      end if;
      Window_Blocks.Set (Ring.Items, Position, W);
   end Fold;

   procedure Append (S : in out Store; Index : Series_Index; Time_Us, Value : Unsigned_64) is
      R : Series_Record := Get (S, Index);
      Sealed : Boolean;
   begin
      if R.Head.Count > 0 and then R.Head.Used + Sample_Room > Chunk_Bytes then
         Seal (S, R, Sealed);
         if not Sealed then
            R.Refused := Add (R.Refused, 1);
            Add_Gap (R, (First_Us => Time_Us, Last_Us => Time_Us, Count => 1, Reason => Refused_By_Store));
            Put (S, Index, R);
            return;
         end if;
      end if;
      if R.Head.Count = 0 then
         R.Head := (Count => 1, Used => 0, First_Time => Time_Us, First_Value => Value,
                    Last_Time => Time_Us, Last_Value => Value, Last_Delta => 0,
                    Min_Time => Time_Us, Max_Time => Time_Us);
      else
         declare
            Delta_Time : constant Unsigned_64 := Time_Us - R.Head.Last_Time;
         begin
            Codecs.Put_Varint (R.Head_Bytes, R.Head.Used, Codecs.Zigzag (Delta_Time - R.Head.Last_Delta));
            R.Head.Used := R.Head.Used + Codecs.Varint_Bytes (Codecs.Zigzag (Delta_Time - R.Head.Last_Delta));
            Codecs.Put_Varint (R.Head_Bytes, R.Head.Used, Codecs.Zigzag (Value - R.Head.Last_Value));
            R.Head.Used := R.Head.Used + Codecs.Varint_Bytes (Codecs.Zigzag (Value - R.Head.Last_Value));
            R.Head.Count := R.Head.Count + 1;
            R.Head.Last_Delta := Delta_Time;
            R.Head.Last_Time := Time_Us;
            R.Head.Last_Value := Value;
            R.Head.Min_Time := Unsigned_64'Min (R.Head.Min_Time, Time_Us);
            R.Head.Max_Time := Unsigned_64'Max (R.Head.Max_Time, Time_Us);
         end;
      end if;
      if R.Published = 0 then
         R.First_Us := Time_Us;
      end if;
      R.Published := Add (R.Published, 1);
      R.First_Us := Unsigned_64'Min (R.First_Us, Time_Us);
      if Time_Us >= R.Last_Us then
         R.Last_Us := Time_Us;
         R.Last_Value := Value;
      end if;
      for T in Tier loop
         Fold (S, R, T, Time_Us, Value);
      end loop;
      Put (S, Index, R);
   end Append;

   ---------------------------------------------------------------------------
   --  Reading
   ---------------------------------------------------------------------------
   function Count (S : Store) return Series_Count is (S.Used);

   function Info (S : Store; Index : Series_Index) return Series_Info is
      R : constant Series_Record := Get (S, Index);
      Retained : Unsigned_64 := Unsigned_64 (R.Head.Count);
      Raw_Bytes : Unsigned_64 := Unsigned_64 (R.Head.Used);
      Capacity : constant Natural := Chunk_Blocks.Capacity (R.Sealed);
   begin
      for P in 0 .. R.At_Sealed.Count - 1 loop
         declare
            H : constant Chunk_Header := Chunk_Blocks.Get (R.Sealed, (R.At_Sealed.First + P) mod Capacity).Header;
         begin
            Retained := Add (Retained, Unsigned_64 (H.Count));
            Raw_Bytes := Add (Raw_Bytes, Unsigned_64 (H.Used));
         end;
      end loop;
      return (Id => R.Id, With_Labels => R.With_Labels, Kind => R.Kind, Measure => R.Measure,
              Published => R.Published, Retained => Retained, Evicted => R.Evicted, Shed => R.Shed,
              Refused => R.Refused, First_Us => R.First_Us, Last_Us => R.Last_Us,
              Last_Value => R.Last_Value, Raw_From_Us => R.Raw_From_Us, Gaps => R.At_Gaps.Count,
              Raw_Bytes => Raw_Bytes);
   end Info;

   function Selects (S : Store; Sel : Selector; Index : Series_Index) return Boolean is
      L : constant Labels := Get (S, Index).With_Labels;
      Name : constant String := Text_Of (S, L.Name);
      Wanted : constant String := Sel.Name (1 .. Sel.Name_Length);
   begin
      if Sel.Source_Length > 0 and then Text_Of (S, L.Source) /= Sel.Source (1 .. Sel.Source_Length) then
         return False;
      end if;
      return (case Sel.Match is
                when Any_Name => True,
                when Exact_Name => Name = Wanted,
                when Name_Prefix => Name'Length >= Wanted'Length
                                    and then Name (Name'First .. Name'First + Wanted'Length - 1) = Wanted);
   end Selects;

   procedure Next_Selected (S : Store; Sel : Selector; After : Series_Count; Found : out Series_Count) is
   begin
      Found := No_Series;
      if Sel.Match = Exact_Name then
         declare
            Names : constant Posting :=
              Posting_Of (S, S.Keys.Name, Find_Text (S, Sel.Name (1 .. Sel.Name_Length)));
            Sources : constant Posting :=
              (if Sel.Source_Length = 0 then (others => <>)
               else Posting_Of (S, S.Keys.Source, Find_Text (S, Sel.Source (1 .. Sel.Source_Length))));
            J : Natural := 0;
         begin
            if Names.Used = 0 or else (Sel.Source_Length > 0 and then Sources.Used = 0) then
               return;
            end if;
            --  Both lists are sorted: walk them together.
            for I in 0 .. Names.Used - 1 loop
               declare
                  Candidate : constant Series_Count := Index_Blocks.Get (Names.Items, I);
               begin
                  if Candidate > After then
                     if Sel.Source_Length = 0 then
                        Found := Candidate;
                        return;
                     end if;
                     while J < Sources.Used and then Index_Blocks.Get (Sources.Items, J) < Candidate loop
                        J := J + 1;
                     end loop;
                     exit when J = Sources.Used;
                     if Index_Blocks.Get (Sources.Items, J) = Candidate then
                        Found := Candidate;
                        return;
                     end if;
                  end if;
               end;
            end loop;
         end;
         return;
      end if;
      for Index in After + 1 .. S.Used loop
         if Selects (S, Sel, Index) then
            Found := Index;
            return;
         end if;
      end loop;
   end Next_Selected;

   --  Decode one chunk's samples into Visit.
   generic
      with procedure Each (Time_Us, Value : Unsigned_64; Continue : out Boolean);
   procedure Decode_Chunk (Header : Chunk_Header; Data : Metric_Codecs.Byte_Array; Continue : out Boolean);
   procedure Decode_Chunk (Header : Chunk_Header; Data : Metric_Codecs.Byte_Array; Continue : out Boolean) is
      Time : Unsigned_64 := Header.First_Time;
      Value : Unsigned_64 := Header.First_Value;
      Step : Unsigned_64 := 0;
      At_Byte : Natural := Data'First;
      Word : Unsigned_64;
      Length : Codecs.Varint_Length;
      Valid : Boolean;
   begin
      Continue := True;
      if Header.Count = 0 then
         return;
      end if;
      Each (Time, Value, Continue);
      for K in 2 .. Header.Count loop
         exit when not Continue;
         Codecs.Get_Varint (Data, At_Byte, Word, Length, Valid);
         exit when not Valid;
         At_Byte := At_Byte + Length;
         Step := Step + Codecs.Unzigzag (Word);
         Time := Time + Step;
         Codecs.Get_Varint (Data, At_Byte, Word, Length, Valid);
         exit when not Valid;
         At_Byte := At_Byte + Length;
         Value := Value + Codecs.Unzigzag (Word);
         Each (Time, Value, Continue);
      end loop;
   end Decode_Chunk;

   procedure Read_Raw (S : Store; Index : Series_Index; From_Us, To_Us : Unsigned_64) is
      R : constant Series_Record := Get (S, Index);
      Capacity : constant Natural := Chunk_Blocks.Capacity (R.Sealed);
      Going : Boolean := True;
      procedure Each (Time_Us, Value : Unsigned_64; Continue : out Boolean);
      procedure Each (Time_Us, Value : Unsigned_64; Continue : out Boolean) is
      begin
         Continue := True;
         if Time_Us >= From_Us and then Time_Us <= To_Us then
            Visit (Time_Us, Value, Continue);
         end if;
      end Each;
      procedure Decode is new Decode_Chunk (Each);
   begin
      for P in 0 .. R.At_Sealed.Count - 1 loop
         exit when not Going;
         declare
            C : constant Sealed_Chunk := Chunk_Blocks.Get (R.Sealed, (R.At_Sealed.First + P) mod Capacity);
            Data : Metric_Codecs.Byte_Array (0 .. Natural'Max (C.Header.Used, 1) - 1) := [others => 0];
         begin
            if C.Header.Max_Time >= From_Us and then C.Header.Min_Time <= To_Us then
               for I in 0 .. C.Header.Used - 1 loop
                  Data (I) := Byte_Blocks.Get (C.Data, I);
               end loop;
               Decode (C.Header, Data, Going);
            end if;
         end;
      end loop;
      if Going and then R.Head.Count > 0 and then R.Head.Max_Time >= From_Us and then R.Head.Min_Time <= To_Us then
         Decode (R.Head, R.Head_Bytes, Going);
      end if;
   end Read_Raw;

   procedure Read_Windows (S : Store; Index : Series_Index; Of_Tier : Tier; From_Us, To_Us : Unsigned_64) is
      R : constant Series_Record := Get (S, Index);
      Ring : Window_Ring renames R.Windows (Of_Tier);
      Capacity : constant Natural := Window_Blocks.Capacity (Ring.Items);
      Going : Boolean := True;
   begin
      for P in 0 .. Ring.At_Ring.Count - 1 loop
         exit when not Going;
         declare
            W : constant Window := Window_Blocks.Get (Ring.Items, (Ring.At_Ring.First + P) mod Capacity);
         begin
            if W.Row.Start_Us <= To_Us and then Add (W.Row.Start_Us, Width_Us (Of_Tier)) > From_Us then
               if W.Sketch_Used = 0 then
                  Visit (W.Row, Going);
               else
                  declare
                     D : constant Sketches.Dense := Sketch_Of (W);
                     Row : Window_Row := W.Row;
                  begin
                     Row.P50 := Sketches.Quantile (D, Quantile_50);
                     Row.P99 := Sketches.Quantile (D, Quantile_99);
                     Visit (Row, Going);
                  end;
               end if;
            end if;
         end;
      end loop;
   end Read_Windows;

   procedure Read_Gaps (S : Store; Index : Series_Index; From_Us, To_Us : Unsigned_64) is
      R : constant Series_Record := Get (S, Index);
      Capacity : constant Natural := Gap_Blocks.Capacity (R.Gaps);
      Going : Boolean := True;
   begin
      for P in 0 .. R.At_Gaps.Count - 1 loop
         exit when not Going;
         declare
            G : constant Gap := Gap_Blocks.Get (R.Gaps, (R.At_Gaps.First + P) mod Capacity);
         begin
            if G.First_Us <= To_Us and then G.Last_Us >= From_Us then
               Visit (G, Going);
            end if;
         end;
      end loop;
   end Read_Gaps;

   procedure Summarize
     (S : Store; Index : Series_Index; From_Us, To_Us : Unsigned_64;
      Into : in out Metric_Sketches.Dense; Resolution_Us, Missing : out Unsigned_64)
   is
      R : constant Series_Record := Get (S, Index);
      Lost : Unsigned_64 := 0;
      procedure Add_Raw (Time_Us, Value : Unsigned_64; Continue : out Boolean);
      procedure Add_Raw (Time_Us, Value : Unsigned_64; Continue : out Boolean) is
         pragma Unreferenced (Time_Us);
      begin
         Sketches.Add (Into, Value);
         Continue := True;
      end Add_Raw;
      procedure Raw is new Read_Raw (Add_Raw);
      procedure Count_Gap (Item : Gap; Continue : out Boolean);
      procedure Count_Gap (Item : Gap; Continue : out Boolean) is
      begin
         Lost := Add (Lost, Item.Count);
         Continue := True;
      end Count_Gap;
      procedure Gaps is new Read_Gaps (Count_Gap);
      Chosen : Tier := Ten_Minutes;
   begin
      Gaps (S, Index, From_Us, To_Us);
      Missing := Lost;
      if R.Raw_From_Us <= From_Us then
         Resolution_Us := 0;
         Raw (S, Index, From_Us, To_Us);
         return;
      end if;
      --  The finest tier whose oldest kept window reaches back to From_Us.
      for T in reverse Tier loop
         declare
            Ring : Window_Ring renames R.Windows (T);
         begin
            if Ring.At_Ring.Count > 0 and then
              Window_Blocks.Get (Ring.Items, Ring.At_Ring.First).Row.Start_Us <= From_Us
            then
               Chosen := T;
            end if;
         end;
      end loop;
      Resolution_Us := Width_Us (Chosen);
      declare
         Ring : Window_Ring renames R.Windows (Chosen);
         Capacity : constant Natural := Window_Blocks.Capacity (Ring.Items);
      begin
         for P in 0 .. Ring.At_Ring.Count - 1 loop
            declare
               W : constant Window := Window_Blocks.Get (Ring.Items, (Ring.At_Ring.First + P) mod Capacity);
            begin
               if W.Row.Start_Us <= To_Us and then Add (W.Row.Start_Us, Width_Us (Chosen)) > From_Us
                 and then W.Row.Count > 0
               then
                  if W.Sketch_Used > 0 then
                     for I in 0 .. W.Sketch_Used - 1 loop
                        declare
                           E : constant Sketches.Sparse_Entry := Entry_Blocks.Get (W.Sketch, I);
                           B : constant Sketches.Bucket := Sketches.Bucket_Part (E);
                        begin
                           Into.Buckets (B) := Add (Into.Buckets (B), Sketches.Count_Part (E));
                        end;
                     end loop;
                  else
                     --  No sketch (a counter or gauge): its extremes only.
                     Into.Buckets (Sketches.Bucket_Of (W.Row.Maximum)) :=
                       Add (Into.Buckets (Sketches.Bucket_Of (W.Row.Maximum)), W.Row.Count);
                  end if;
                  Into.Count := Add (Into.Count, W.Row.Count);
                  if W.Row.Sum = Unsigned_64'Last or else W.Row.Sum > Unsigned_64'Last - Into.Sum then
                     Into.Sum := Unsigned_64'Last;
                     Into.Sum_Saturated := True;
                  else
                     Into.Sum := Into.Sum + W.Row.Sum;
                  end if;
                  Into.Minimum := Unsigned_64'Min (Into.Minimum, W.Row.Minimum);
                  Into.Maximum := Unsigned_64'Max (Into.Maximum, W.Row.Maximum);
               end if;
            end;
         end loop;
      end;
   end Summarize;

   function Bytes_Held (S : Store) return Unsigned_64 is
     (Add (Add (S.Held, Metric_Strings.Bytes_Held (S.Strings)),
           Unsigned_64 (Series_Blocks.Capacity (S.Series)) * (Series_Record'Size / Byte_Bits)));
   function Series_Refused (S : Store) return Unsigned_64 is (S.Refusals);
end Metric_Series;
