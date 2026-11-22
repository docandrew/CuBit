------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package body CuBit.Datagram_Rings with SPARK_Mode is

   procedure Write_Header
     (Ring : in out Bytes; At_Position : Natural; Length, Kind : Natural)
   with
     Pre  => Ring'First = 0 and then At_Position < Natural'Last - 3 and then
             At_Position + 3 <= Ring'Last and then
             Length <= Maximum_Payload and then Kind <= 1,
     Post => Ring'First = Ring'Old'First and then Ring'Last = Ring'Old'Last;

   procedure Write_Header
     (Ring : in out Bytes; At_Position : Natural; Length, Kind : Natural)
   is
   begin
      Ring (At_Position) := Unsigned_8 (Length mod 256);
      Ring (At_Position + 1) := Unsigned_8 (Length / 256);
      Ring (At_Position + 2) := Unsigned_8 (Kind);
      Ring (At_Position + 3) := 0;
   end Write_Header;

   procedure Put
     (P : in out Producer; Ring : in out Bytes; Data : Bytes;
      Result : out Put_Result)
   is
      Needed : constant Positive := Record_Bytes (Data'Length);
      First, L1, L2 : Natural;
      Tail : Natural;     --  bytes padded at the end, if any
      Start : Natural;    --  where the record goes
   begin
      if Needed > P.Size then
         Result := Too_Large;
         return;
      end if;
      Free_Slices (P, First, L1, L2);
      if First mod 4 /= 0 then
         Result := No_Room;   --  not reached: we only commit multiples of 4
         return;
      end if;
      if L1 >= Needed then
         Tail := 0;
         Start := First;
      elsif L1 + L2 = Space (P) and then L1 = P.Size - First and then
        L2 >= Needed and then L1 >= Header_Bytes
      then
         --  Pad the end; the record starts the ring.
         Tail := L1;
         Start := 0;
      else
         Result := No_Room;
         return;
      end if;
      if Tail > 0 then
         Write_Header (Ring, First, Tail - Header_Bytes, Kind_Pad);
      end if;
      Write_Header (Ring, Start, Data'Length, Kind_Data);
      if Data'Length > 0 then
         Ring (Start + Header_Bytes ..
               Start + Header_Bytes + Data'Length - 1) := Data;
      end if;
      Commit (P, Tail + Needed);
      Result := Put;
   end Put;

   procedure Take
     (C : in out Consumer; Ring : Bytes; Into : in out Bytes;
      Length : out Natural; Truncated : out Boolean;
      Result : out Take_Result)
   is
      First, L1, L2 : Natural;
      Header_Length, Kind, Needed : Natural;
      D : constant Integer := Into'First;
   begin
      Length := 0;
      Truncated := False;
      --  At most one pad before a record.
      for Pass in 1 .. 2 loop
         pragma Loop_Invariant
           (Valid (C) and then C.Size = C'Loop_Entry.Size and then
            Length = 0 and then not Truncated);
         Data_Slices (C, First, L1, L2);
         if L1 = 0 then
            Result := Empty;
            return;
         elsif L1 < Header_Bytes or else First mod 4 /= 0 then
            Result := Malformed;   --  a partial or misaligned record
            return;
         end if;
         Header_Length :=
           Natural (Ring (First)) + 256 * Natural (Ring (First + 1));
         Kind := Natural (Ring (First + 2));
         if Kind = Kind_Pad then
            --  A pad runs exactly to the end of the ring.
            if Header_Length + Header_Bytes /= C.Size - First or else
              L1 /= C.Size - First or else Pass = 2
            then
               Result := Malformed;
               return;
            end if;
            Consume (C, L1);
         elsif Kind = Kind_Data then
            Needed := Record_Bytes (Header_Length);
            if Needed > L1 then
               Result := Malformed;
               return;
            end if;
            Length := Natural'Min (Header_Length, Into'Length);
            Truncated := Header_Length > Into'Length;
            if Length > 0 then
               Into (D .. D + Length - 1) :=
                 Ring (First + Header_Bytes ..
                       First + Header_Bytes + Length - 1);
            end if;
            Consume (C, Needed);
            Result := Taken;
            return;
         else
            Result := Malformed;
            return;
         end if;
      end loop;
      Result := Empty;
   end Take;

end CuBit.Datagram_Rings;
