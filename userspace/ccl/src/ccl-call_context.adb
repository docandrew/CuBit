package body CCL.Call_Context with SPARK_Mode is
   procedure Inspect (Source : String; Offset : Natural; Result : out Context) is
      type Positions is array (Positive range 1 .. Maximum_Source) of Positive;
      Openers : Positions := [others => 1];
      Depth : Natural range 0 .. Maximum_Source := 0;
      Quoted, Escaped, Comment : Boolean := False;
      First : Positive;
      function At_Position (Position : Positive) return Character is
        (Source (Source'First + (Position - 1)))
        with Pre => Position <= Source'Length;
      function Space (C : Character) return Boolean is
        (C in ' ' | ASCII.HT | ASCII.CR | ASCII.LF);
      function Name_Character (C : Character) return Boolean is
        (C in 'a' .. 'z' | '0' .. '9' | '-' | '.');
   begin
      Result := (others => <>);
      if Source'Length > Maximum_Source or else Offset > Source'Length then return; end if;
      for I in 1 .. Offset loop
         pragma Loop_Invariant (Depth < I);
         pragma Loop_Invariant
           (for all D in 1 .. Depth => Openers (D) < I);
         declare C : constant Character := At_Position (I); begin
            if Comment then
               if C = ASCII.LF then Comment := False; end if;
            elsif Escaped then Escaped := False;
            elsif Quoted then
               if C = '\' then Escaped := True;
               elsif C = '"' then Quoted := False; end if;
            elsif C = '#' then Comment := True;
            elsif C = '"' then Quoted := True;
            elsif C = '(' then
               Depth := Depth + 1;
               Openers (Depth) := I;
            elsif C = ')' then
               if Depth = 0 then return; end if;
               Depth := Depth - 1;
            end if;
         end;
      end loop;
      if Depth = 0 or else Quoted or else Comment then return; end if;
      First := Openers (Depth) + 1;
      while First <= Offset and then Space (At_Position (First)) loop
         pragma Loop_Variant (Increases => First);
         First := First + 1;
      end loop;
      declare
         Name_Start : constant Positive := First;
         subtype Name_Position is Positive range Name_Start .. Maximum_Source + 1;
         Last : Name_Position := Name_Start;
      begin
         while Last <= Offset and then Name_Character (At_Position (Last)) loop
            pragma Loop_Variant (Increases => Last);
            Last := Last + 1;
         end loop;
         if Last = Name_Start or else Last - Name_Start > Maximum_Name or else
           (Last <= Offset and then not Space (At_Position (Last)))
         then return; end if;
         Result.Length := Last - Name_Start;
         Result.Name (1 .. Result.Length) :=
           Source (Source'First + (Name_Start - 1) .. Source'First + (Last - 2));
         Result.Arguments_Started := Last <= Offset;
         Result.Available := True;
      end;
   end Inspect;
end CCL.Call_Context;
