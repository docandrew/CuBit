package body Windowed_Reads with SPARK_Mode is
   function Read_All (Length : Natural) return Boolean is
      Offset : Natural := 0;
      Transferred : Natural;
      Success : Boolean;
   begin
      if Length = 0 then
         return False;
      end if;
      while Offset < Length loop
         pragma Loop_Variant (Increases => Offset);
         declare
            Count : constant Positive := Natural'Min (Window_Bytes, Length - Offset);
         begin
            Transfer (Offset, Count, Transferred, Success);
            if not Success or else Transferred /= Count then
               return False;
            end if;
            Offset := Offset + Count;
         end;
      end loop;
      return True;
   end Read_All;
end Windowed_Reads;
