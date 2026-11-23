pragma Ada_2022;
package body Power_ACPI_Sources with SPARK_Mode is
   use type ID.Result_Code, PS.Source_Kind;

   --  The most chunks a path takes.
   Maximum_Chunks : constant := (ID.Maximum_Path_Characters + ID.Chunk_Characters - 1) / ID.Chunk_Characters;

   procedure Count_Request (Requests : in out Natural) is
   begin
      if Requests < Natural'Last then
         Requests := Requests + 1;
      end if;
   end Count_Request;

   procedure Read_Path
     (Index : ID.Source_Index; Expected : AF.Word; Value : out ID.Path; Outcome : out Path_Outcome;
      Requests : in out Natural)
   is
      Label : Unsigned_32;
      Reply : AF.Words;
      Delivered, Valid : Boolean;
      Generation : AF.Word;
      Code : ID.Result_Code;
      Total, Length : ID.Path_Length := 0;
      Next : ID.Path := ID.Unknown_Path;
   begin
      Value := ID.Unknown_Path;
      Outcome := Unknown;
      for Chunk in 1 .. Maximum_Chunks + 1 loop
         pragma Loop_Invariant (Next.Length <= Total and then (if Chunk = 1 then Next.Length = 0));
         declare
            Request : constant ID.Words := ID.Request (Index, Next.Length, Expected);
         begin
            Ask ([for I in AF.Words'Range => Request (I)], Label, Reply, Delivered);
         end;
         Count_Request (Requests);
         if not Delivered or else Label /= AF.Reply_OK then
            return;
         end if;
         ID.Decode ([for I in ID.Words'Range => Reply (I)], Generation, Code, Length, Valid);
         if not Valid then
            return;
         elsif Code = ID.Stale or else (Code = ID.OK and then Generation /= Expected) then
            Outcome := Moved_On;
            return;
         elsif Code /= ID.OK or else Length = 0 or else (Chunk > 1 and then Length /= Total) then
            return;
         end if;
         Total := Length;
         if Next.Length < Total then
            ID.Append (Next, Total, Reply (3), Valid);
            if not Valid then
               return;
            end if;
         end if;
         if Next.Length = Total then
            Value := Next;
            Outcome := Known;
            return;
         end if;
      end loop;
   end Read_Path;

   procedure Read_Once (Maximum_Age, AC_Maximum_Age : AF.Word; Result : out Reading; Moved : out Boolean)
   with Pre => Maximum_Age <= AF.Any_Age and then AC_Maximum_Age <= AF.Any_Age
   is
      Values : Reader.Reading;
      Origins : PM.Origin_Array;
      Outcome : Path_Outcome;
   begin
      Result := (others => <>);
      Moved := False;
      Reader.Read (Maximum_Age, Values, AC_Maximum_Age);
      Result.Requests := Values.Requests;
      PM.Build (Values.AC, Values.Batteries, Values.Count, Result.Sources, Result.Count, Origins);
      for I in 1 .. Result.Count loop
         declare
            Expected : constant AF.Word :=
              (if Result.Sources (I).Kind = PS.AC_Adapter then Values.AC_Generation else Values.Battery_Generation);
         begin
            if Expected /= 0 then
               Read_Path (Origins (I), Expected, Result.Paths (I), Outcome, Result.Requests);
               if Outcome = Moved_On then
                  Moved := True;
                  return;
               end if;
            end if;
         end;
      end loop;
      ID.Name_Sources (Result.Sources, Result.Count, Result.Paths);
   end Read_Once;

   procedure Read (Maximum_Age : AF.Word; Result : out Reading; AC_Maximum_Age : AF.Word := 0) is
      Moved : Boolean := True;
      Spent : Natural := 0;
   begin
      Result := (others => <>);
      for Attempt in 1 .. Attempts loop
         Read_Once (Maximum_Age, AC_Maximum_Age, Result, Moved);
         Spent := (if Natural'Last - Spent < Result.Requests then Natural'Last else Spent + Result.Requests);
         exit when not Moved;
      end loop;
      if Moved then
         --  The families kept moving on: the last values, by slot name.
         Result.Paths := [others => ID.Unknown_Path];
      end if;
      Result.Requests := Spent;
   end Read;
end Power_ACPI_Sources;
