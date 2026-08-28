package body CuBit.UI.Editor.Buffers with SPARK_Mode is
   procedure Initialize (Value : out Candidate_Buffer) is
   begin
      Value.Data := [others => ' '];
      Value.Last := 0;
   end Initialize;

   function Length (Value : Candidate_Buffer) return Natural is (Value.Last);
   function Remaining (Value : Candidate_Buffer) return Natural is
     (Value.Capacity - Value.Last);
   function Content (Value : Candidate_Buffer) return String is
     (Value.Data (1 .. Value.Last));

   procedure Append
     (Value : in out Candidate_Buffer; Text : String;
      Result : out Append_Result)
   is
   begin
      if Text'Length > Value.Capacity - Value.Last then
         Result := Capacity_Exceeded;
      else
         for Offset in 0 .. Text'Length - 1 loop
            pragma Loop_Invariant (Offset < Text'Length);
            Value.Data (Value.Last + Offset + 1) := Text (Text'First + Offset);
         end loop;
         Value.Last := Value.Last + Text'Length;
         Result := Appended;
      end if;
   end Append;
end CuBit.UI.Editor.Buffers;
