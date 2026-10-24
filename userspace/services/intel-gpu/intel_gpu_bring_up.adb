package body Intel_GPU_Bring_Up with SPARK_Mode is
   type Reported is array (Stage) of Boolean;
   Passed : Reported := [others => False];

   procedure Report (S : Stage; OK : Boolean) is
   begin
      Passed (S) := OK;
   end Report;

   function Next return Stage is
   begin
      for S in Stage loop
         if not Passed (S) then
            return S;
         end if;
      end loop;
      return Stage'Last;
   end Next;

   function Complete return Boolean is (for all S in Stage => Passed (S));

   function Count_Passed return Natural is
      Result : Natural := 0;
   begin
      for S in Stage loop
         pragma Loop_Invariant (Result <= Stage'Pos (S));
         exit when not Passed (S);
         Result := Result + 1;
      end loop;
      return Result;
   end Count_Passed;

   function Image (N : Natural) return String is
      Text : constant String := Natural'Image (N);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   function Summary return String is
     (if Complete then Prefix & "bring-up complete"
      elsif Count_Passed = 0 then Prefix & "bring-up reached nothing (0/" &
        Image (Stage'Pos (Stage'Last) + 1) & ")"
      else Prefix & "bring-up reached " & Name (Stage'Val (Count_Passed - 1)) & " (" &
        Image (Count_Passed) & "/" & Image (Stage'Pos (Stage'Last) + 1) & ")");
end Intel_GPU_Bring_Up;
