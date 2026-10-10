package body Files_Plan with SPARK_Mode is
   procedure Clear (P : in out Plan) is
   begin
      P.Count := 0;
      P.Used := 0;
      P.Files := 0;
      P.Folders := 0;
      P.Bytes := 0;
   end Clear;

   procedure Add (P : in out Plan; Kind : Item_Kind; Relative : String; Size : Unsigned_64) is
      First : constant Arena_Index := P.Used + 1;
   begin
      for K in 0 .. Relative'Length - 1 loop
         pragma Loop_Invariant (P.Used = P.Used'Loop_Entry and then P.Count = P.Count'Loop_Entry);
         P.Paths (First + K) := Character'Pos (Relative (Relative'First + K));
      end loop;
      P.Count := P.Count + 1;
      P.Items (P.Count) := (Kind => Kind, First => First, Length => Relative'Length, Size => Size);
      P.Used := P.Used + Relative'Length;
      if Kind = Folder_Item then
         P.Folders := P.Folders + 1;
      else
         P.Files := P.Files + 1;
         P.Bytes := (if Size > Unsigned_64'Last - P.Bytes then Unsigned_64'Last else P.Bytes + Size);
      end if;
   end Add;

   function Relative (P : Plan; Index : Entry_Id) return String is
      Length : constant Path_Length :=
        (if Index <= P.Count and then Index <= P.Capacity then P.Items (Index).Length else 0);
      Result : String (1 .. Length) := [others => ' '];
   begin
      if Length > 0 then
         declare
            First : constant Arena_Index := P.Items (Index).First;
         begin
            if First <= P.Path_Bytes - (Length - 1) then
               for K in Result'Range loop
                  Result (K) := Character'Val (P.Paths (First + K - 1));
               end loop;
            end if;
         end;
      end if;
      return Result;
   end Relative;

   function Conflict_Name (Name : String; Attempt : Attempt_Number) return String is
      Dot : Natural := 0;
   begin
      if Attempt = 1 then
         return Name;
      end if;
      for K in 2 .. Name'Last - 1 loop
         if Name (K) = '.' then
            Dot := K;
         end if;
         pragma Loop_Invariant (Dot = 0 or else Dot in 2 .. K);
      end loop;
      declare
         Raw : constant String := Positive'Image (Attempt);
         Number : constant String := " (" & Raw (Raw'First + 1 .. Raw'Last) & ")";
         --  The stem gives way so the result fits one component.
         Tail_Length : constant Natural := (if Dot = 0 then 0 else Name'Last - Dot + 1);
         Stem_Room : constant Natural :=
           (if MAXIMUM_NAME_BYTES > Number'Length + Tail_Length then MAXIMUM_NAME_BYTES - Number'Length - Tail_Length
            else 0);
         Stem_Last : constant Natural := Natural'Min ((if Dot = 0 then Name'Last else Dot - 1), Stem_Room);
      begin
         if Stem_Last = 0 or else Number'Length + Tail_Length > MAXIMUM_NAME_BYTES then
            return Name;
         end if;
         declare
            Result : constant String := Name (1 .. Stem_Last) & Number
              & (if Dot = 0 then "" else Name (Dot .. Name'Last));
         begin
            return Result;
         end;
      end;
   end Conflict_Name;
end Files_Plan;
