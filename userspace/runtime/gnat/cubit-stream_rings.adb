------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Stream_Rings with SPARK_Mode is
   use type Datagram_Rings.Put_Result;

   --  Move OLDEST past the oldest record (a pad runs to the end of the
   --  ring). The producer wrote every record itself; a header that is not
   --  one empties the ring rather than trusting it. Pre: something to evict.
   procedure Evict_Oldest (P : in out Producer; Ring : Bytes; Freed : out Positive)
   with
     Pre  => Valid (P) and then P.Fill > 0
             and then Ring'First = 0 and then Ring'Last = P.Size - 1,
     Post => Valid (P) and then P.Size = P'Old.Size
             and then P.Produced = P'Old.Produced
             and then P.Fill < P'Old.Fill and then Freed = P'Old.Fill - P.Fill;

   procedure Evict_Oldest (P : in out Producer; Ring : Bytes; Freed : out Positive) is
      First : constant Natural := Position (Consumed (P), P.Size);
      To_End : constant Natural := P.Size - First;
      Length, Kind : Natural;
      Step : Natural;
   begin
      if First mod 4 /= 0 or else To_End < Datagram_Rings.Header_Bytes then
         Step := P.Fill;                       --  not a record boundary
      else
         Length := Natural (Ring (First)) + 256 * Natural (Ring (First + 1));
         Kind := Natural (Ring (First + 2));
         Step :=
           (if Kind = Datagram_Rings.Kind_Pad then To_End
            elsif Kind = Datagram_Rings.Kind_Data then Record_Bytes (Length)
            else P.Fill);
         if Step > P.Fill or else Step > To_End or else Step mod 4 /= 0 then
            Step := P.Fill;
         end if;
      end if;
      pragma Assert (Step in 1 .. P.Fill);
      Freed := Step;
      P.Fill := P.Fill - Step;
   end Evict_Oldest;

   function Room (P : Producer; Needed : Positive) return Boolean is
      First, L1, L2 : Natural;
   begin
      Free_Slices (P, First, L1, L2);
      return First mod 4 = 0
        and then (L1 >= Needed
                  or else (L1 = P.Size - First and then L2 >= Needed
                           and then L1 >= Datagram_Rings.Header_Bytes));
   end Room;

   procedure Make_Room
     (P : in out Producer; Ring : Bytes; Length : Natural;
      Evicted : out Natural; Result : out Publish_Result)
   is
      Freed : Positive;
   begin
      Evicted := 0;
      if not Fits (P.Size, Length) then
         Result := Too_Large;
         return;
      end if;
      loop
         pragma Loop_Invariant
           (Valid (P) and then P.Size = P'Loop_Entry.Size
            and then P.Produced = P'Loop_Entry.Produced
            and then Evicted <= P'Loop_Entry.Fill - P.Fill
            and then (if Evicted = 0 then P = P'Loop_Entry));
         pragma Loop_Variant (Decreases => P.Fill);
         if Room (P, Record_Bytes (Length)) then
            Result := Published;
            return;
         end if;
         --  An empty ring takes any record that Fits (half the ring), so
         --  there is something to evict whenever there is no room.
         if P.Fill = 0 then
            Result := Too_Large;
            return;
         end if;
         Evict_Oldest (P, Ring, Freed);
         Evicted := Evicted + Freed;
      end loop;
   end Make_Room;

   procedure Publish
     (P : in out Producer; Ring : in out Bytes; Data : Bytes;
      Evicted : out Natural; Result : out Publish_Result)
   is
      Put : Datagram_Rings.Put_Result;
   begin
      Make_Room (P, Ring, Data'Length, Evicted, Result);
      if Result /= Published then
         Evicted := 0;
         return;
      end if;
      Datagram_Rings.Put (P, Ring, Data, Put);
      if Put /= Datagram_Rings.Put then
         Result := Too_Large;
      end if;
   end Publish;

   procedure Read
     (Cursor : in out Index; Size : Ring_Size; Ring : Bytes;
      Produced, Oldest : Index; Into : in out Bytes;
      Length : out Natural; Truncated, Lost : out Boolean;
      Result : out Read_Result)
   is
      C : Consumer;
      Take : Datagram_Rings.Take_Result;
   begin
      Length := 0;
      Truncated := False;
      Lost := False;
      if Distance (Oldest, Produced) > Count (Size) then
         Result := Malformed;
         return;
      end if;
      --  A cursor older than OLDEST (or one the indices do not explain)
      --  resumes at OLDEST: the records between are gone.
      if not Intact (Cursor, Produced, Oldest) then
         Lost := Cursor /= Oldest;
         Cursor := Oldest;
      end if;
      C := (Size => Size, Consumed => Cursor,
            Available => Natural (Distance (Cursor, Produced)));
      Datagram_Rings.Take (C, Ring, Into, Length, Truncated, Take);
      Cursor := C.Consumed;
      Result := (case Take is
                   when Datagram_Rings.Taken => Taken,
                   when Datagram_Rings.Empty => Empty,
                   when Datagram_Rings.Malformed => Malformed);
   end Read;

end CuBit.Stream_Rings;
