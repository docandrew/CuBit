pragma Ada_2022;
package body AML_Names with SPARK_Mode is
   function Read_Name (Data : AML_Decode.Bytes) return Name_Result is
      Position : Natural := 0;
      Rooted : Boolean := False;
      Parents : Natural range 0 .. 255 := 0;
      Count : Natural range 0 .. 255 := 0;
      Parts : Segment_Array := [others => "____"];
      C : AML_Decode.Byte;
   begin
      if Data'Length = 0 then
         return (Kind => Truncated);
      end if;
      if Data (Data'First) = 16#5C# then
         Rooted := True;
         Position := 1;
      else
         while Position < Data'Length loop
            pragma Loop_Invariant (Position <= Data'Length);
            pragma Loop_Invariant (not Rooted);
            pragma Loop_Variant (Decreases => Data'Length - Position);
            exit when Data (Data'First + Position) /= 16#5E#;
            if Parents = 255 then
               return (Kind => Limit_Exceeded);
            end if;
            Parents := Parents + 1;
            Position := Position + 1;
         end loop;
      end if;
      if Position = Data'Length then
         return (Kind => Truncated);
      end if;
      C := Data (Data'First + Position);
      case C is
         when 0 =>
            Position := Position + 1;
         when 16#2E# =>
            Count := 2;
            Position := Position + 1;
         when 16#2F# =>
            Position := Position + 1;
            if Position = Data'Length then
               return (Kind => Truncated);
            end if;
            Count := Natural (Data (Data'First + Position));
            Position := Position + 1;
            if Count = 0 then
               return (Kind => Malformed);
            end if;
         when others =>
            if not Lead (C) then
               return (Kind => Malformed);
            end if;
            Count := 1;
      end case;
      if Count > (Data'Length - Position) / 4 then
         return (Kind => Truncated);
      end if;
      for I in 1 .. Count loop
         pragma Loop_Invariant (Position <= Data'Length);
         pragma Loop_Invariant (Count - I + 1 <= (Data'Length - Position) / 4);
         pragma Loop_Invariant
           (for all K in 1 .. I - 1 => Valid (Parts (K)));
         for J in 1 .. 4 loop
            pragma Loop_Invariant
              (for all K in 1 .. I - 1 => Valid (Parts (K)));
            pragma Loop_Invariant
              (if J > 1 then
                 Lead (AML_Decode.Byte (Character'Pos (Parts (I) (1)))));
            pragma Loop_Invariant
              (for all K in 1 .. J - 1 =>
                 Tail (AML_Decode.Byte (Character'Pos (Parts (I) (K)))));
            C := Data (Data'First + Position + (J - 1));
            if (J = 1 and then not Lead (C)) or else not Tail (C) then
               return (Kind => Malformed);
            end if;
            Parts (I) (J) := Character'Val (C);
         end loop;
         Position := Position + 4;
      end loop;
      return (Kind => Accepted, Rooted => Rooted, Parents => Parents,
              Count => Count, Parts => Parts, Consumed => Position);
   end Read_Name;
end AML_Names;
