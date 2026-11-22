package body Module_Patches is
   procedure Replace
     (Data : in out CCL.Format.Byte_Array; Length : in out CCL.Format.Module_Length;
      From, To : Bytes; Found : out Boolean; Occurrence : Natural := 0)
   is
      At_Position : Natural := 0;
      Matches : Natural := 0;
   begin
      Found := False;
      if From'Length = 0 or else From'Length > Length then return; end if;
      for Start in 0 .. Length - From'Length loop
         declare
            Same : Boolean := True;
         begin
            for I in From'Range loop
               if Data (Start + I - From'First) /= From (I) then
                  Same := False;
                  exit;
               end if;
            end loop;
            if Same then
               Matches := Matches + 1;
               if Occurrence = 0 or else Matches = Occurrence then
                  At_Position := Start;
               end if;
            end if;
         end;
      end loop;
      if (if Occurrence = 0 then Matches /= 1 else Matches < Occurrence) or else
        Length - From'Length + To'Length > CCL.Format.MAX_MODULE_SIZE
      then
         return;
      end if;
      declare
         Tail : constant CCL.Format.Byte_Array := Data;
         New_Length : constant Natural := Length - From'Length + To'Length;
      begin
         for I in To'Range loop
            Data (At_Position + I - To'First) := To (I);
         end loop;
         for I in At_Position + From'Length .. Length - 1 loop
            Data (I - From'Length + To'Length) := Tail (I);
         end loop;
         for I in New_Length .. Length - 1 loop
            Data (I) := 0;
         end loop;
         Length := New_Length;
      end;
      Found := True;
   end Replace;

   function Encoded_Digest (Words : CCL.Catalog.Descriptor_Digest) return Bytes is
      Result : Bytes (1 .. 34) := [1 => 16#58#, 2 => 16#20#, others => 0];
   begin
      for Word in Words'Range loop
         for B in 0 .. 7 loop
            Result (3 + Word * 8 + B) :=
              Unsigned_8 (Shift_Right (Words (Word), (7 - B) * 8) and 16#FF#);
         end loop;
      end loop;
      return Result;
   end Encoded_Digest;

   function Encoded_Digest (Words : CCL.Objects.Schema_Key) return Bytes is
      Same : CCL.Catalog.Descriptor_Digest;
   begin
      for Word in Same'Range loop
         Same (Word) := Words (Word);
      end loop;
      return Encoded_Digest (Same);
   end Encoded_Digest;
end Module_Patches;
