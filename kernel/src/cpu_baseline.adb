package body CPU_Baseline with SPARK_Mode is

   function Has (W : Words; F : Feature) return Boolean is
     (case F is
        when FMA      => Bit (W.Leaf1_ECX, Leaf1_FMA),
        when MOVBE    => Bit (W.Leaf1_ECX, Leaf1_MOVBE),
        when OSXSAVE  => Bit (W.Leaf1_ECX, Leaf1_OSXSAVE),
        when AVX      => Bit (W.Leaf1_ECX, Leaf1_AVX),
        when F16C     => Bit (W.Leaf1_ECX, Leaf1_F16C),
        when BMI1     => W.Leaf7_Present and then Bit (W.Leaf7_EBX, Leaf7_BMI1),
        when AVX2     => W.Leaf7_Present and then Bit (W.Leaf7_EBX, Leaf7_AVX2),
        when BMI2     => W.Leaf7_Present and then Bit (W.Leaf7_EBX, Leaf7_BMI2),
        when LZCNT    => W.Extended_Present and then Bit (W.Extended_ECX, Extended_LZCNT),
        when XCR0_AVX => (W.XCR0 and XCR0_SSE_AVX) = XCR0_SSE_AVX);

   function Missing (W : Words) return Feature_Set is
      Result : Feature_Set := (others => False);
   begin
      for F in Feature loop
         Result (F) := not Has (W, F);
         pragma Loop_Invariant (for all G in Feature'First .. F => Result (G) = not Has (W, G));
      end loop;
      return Result;
   end Missing;

   procedure Put (Text : in out Names_Text; Length : in out Natural; Item : String)
     with Pre => Item'Length in 1 .. 8 and then Length <= Names_Capacity - Item'Length,
          Post => Length = Length'Old + Item'Length
   is
   begin
      for I in Item'Range loop
         Text (Length + 1 + (I - Item'First)) := Item (I);
         pragma Loop_Invariant (True);
      end loop;
      Length := Length + Item'Length;
   end Put;

   procedure Describe (Set : Feature_Set; Text : out Names_Text; Length : out Natural) is
   begin
      Text := (others => ' ');
      Length := 0;
      for F in Feature loop
         pragma Loop_Invariant (Length <= 9 * Feature'Pos (F));
         if Set (F) then
            if Length > 0 then
               Length := Length + 1;
            end if;
            --  Literals only: no function result on the secondary stack.
            case F is
               when AVX2 => Put (Text, Length, "AVX2");
               when FMA => Put (Text, Length, "FMA");
               when BMI1 => Put (Text, Length, "BMI1");
               when BMI2 => Put (Text, Length, "BMI2");
               when MOVBE => Put (Text, Length, "MOVBE");
               when F16C => Put (Text, Length, "F16C");
               when LZCNT => Put (Text, Length, "LZCNT");
               when OSXSAVE => Put (Text, Length, "OSXSAVE");
               when AVX => Put (Text, Length, "AVX");
               when XCR0_AVX => Put (Text, Length, "XCR0.AVX");
            end case;
         end if;
      end loop;
   end Describe;

   function Names (Set : Feature_Set) return String is
      Text : Names_Text;
      Length : Natural;
   begin
      Describe (Set, Text, Length);
      return Text (1 .. Length);
   end Names;
end CPU_Baseline;
