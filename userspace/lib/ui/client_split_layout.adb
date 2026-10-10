package body Client_Split_Layout with SPARK_Mode is
   procedure Distribute (Total : Length; Count : Part_Count; Gap : Length; Weights : Lengths; Parts : out Lengths) is
      type Wide is range 0 .. 2 ** 62;
      Gaps : constant Wide := (if Count > 1 then Wide (Count - 1) * Wide (Gap) else 0);
      Available : constant Wide := (if Wide (Total) > Gaps then Wide (Total) - Gaps else 0);
      Sum : Wide := 0;
      Given : Wide := 0;
   begin
      Parts := [others => 0];
      if Count = 0 then
         return;
      end if;
      for I in 1 .. Count loop
         Sum := Sum + Wide (Natural'Max (1, Weights (I)));
         pragma Loop_Invariant (Sum <= Wide (I) * MAXIMUM_LENGTH and then Sum >= Wide (I));
      end loop;
      for I in 1 .. Count loop
         pragma Loop_Invariant (Given <= Available and then (for all J in I .. MAXIMUM_PARTS => Parts (J) = 0)
                                and then (for all J in 1 .. I - 1 => Parts (J) <= Total));
         if I = Count then
            Parts (I) := Length (Available - Given);
         else
            declare
               Share : constant Wide :=
                 Wide'Min (Available - Given, Available * Wide (Natural'Max (1, Weights (I))) / Sum);
            begin
               Parts (I) := Length (Share);
               Given := Given + Share;
            end;
         end if;
      end loop;
   end Distribute;

   procedure Drag (Parts : in out Lengths; K : Part_Index; New_Length : Length; Minimum : Length) is
      Pair : constant Length := Parts (K) + Parts (K + 1);
      Low : constant Length := Natural'Min (Minimum, Pair / 2);
      Wanted : constant Length := Natural'Max (Low, Natural'Min (New_Length, Pair - Low));
   begin
      Parts (K) := Wanted;
      Parts (K + 1) := Pair - Wanted;
   end Drag;
end Client_Split_Layout;
