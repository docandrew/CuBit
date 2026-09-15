package body CCL.Declarations with SPARK_Mode => On is
   use type CCL.Language.Interpretation_Status;
   function Select_Format (Token : String) return Format_Selection is
   begin
      if Token = "v1" then
         return (Supported => True, Version => V1);
      else
         return (Supported => False);
      end if;
   end Select_Format;
   function Matches (Value : Symbol; Text : String) return Boolean is
     (Value.Data (1 .. Value.Length) = Text);
   function Failed (Item : Scanner) return Boolean is (Item.Error /= No_Error);
   function Diagnostic (Item : Scanner) return Diagnostic_Code is (Item.Error);
   function Position (Item : Scanner) return Positive is (Item.Cursor);
   function Space (C : Character) return Boolean is
     (C in ' ' | ASCII.HT | ASCII.CR | ASCII.LF);

   procedure Start (Item : out Scanner; Source : String) is
   begin
      Item := (others => <>);
      if Source'Length > MAX_SOURCE then
         Item.Error := Source_Too_Long;
      else
         Item.Last := Source'Length;
         Item.Text (1 .. Item.Last) := Source;
      end if;
   end Start;

   function Next_Position (Item : Scanner) return Positive is
      Cursor : Positive range 1 .. MAX_SOURCE + 1 := Item.Cursor;
      Comment : Boolean := False;
   begin
      while Cursor <= Item.Last loop
         pragma Loop_Variant (Increases => Cursor);
         if Comment then
            if Item.Text (Cursor) = ASCII.LF then Comment := False; end if;
         elsif Item.Text (Cursor) = '#' then
            Comment := True;
         elsif not Space (Item.Text (Cursor)) then
            exit;
         end if;
         Cursor := Cursor + 1;
      end loop;
      return Cursor;
   end Next_Position;

   procedure Skip (Item : in out Scanner) is
   begin
      Item.Cursor := Next_Position (Item);
   end Skip;

   function At_Close (Item : Scanner) return Boolean is
      Cursor : constant Positive := Next_Position (Item);
   begin
      return Cursor <= Item.Last and then Item.Text (Cursor) = ')';
   end At_Close;

   function At_End (Item : Scanner) return Boolean is (Next_Position (Item) > Item.Last);

   procedure Expect (Item : in out Scanner; C : Character) is
   begin
      if Failed (Item) then return; end if;
      Skip (Item);
      if Item.Cursor > Item.Last then Item.Error := Unexpected_End;
      elsif Item.Text (Item.Cursor) /= C then Item.Error := Expected_Form;
      else Item.Cursor := Item.Cursor + 1;
      end if;
   end Expect;

   procedure Open_Form (Item : in out Scanner) is
   begin
      Expect (Item, '(');
   end Open_Form;
   procedure Close_Form (Item : in out Scanner) is
   begin
      Expect (Item, ')');
   end Close_Form;

   procedure Read_Symbol (Item : in out Scanner; Value : out Symbol) is
      First : Positive;
   begin
      Value := (others => <>);
      if Failed (Item) then return; end if;
      Skip (Item);
      First := Item.Cursor;
      while Item.Cursor <= Item.Last and then
        not Space (Item.Text (Item.Cursor)) and then
        Item.Text (Item.Cursor) not in '(' | ')' | '#'
      loop
         Item.Cursor := Item.Cursor + 1;
      end loop;
      if Item.Cursor - First not in 1 .. Value.Data'Length then
         Item.Error := Expected_Form;
      else
         Value.Length := Item.Cursor - First;
         Value.Data (1 .. Value.Length) := Item.Text (First .. Item.Cursor - 1);
      end if;
   end Read_Symbol;

   procedure Evaluate
     (Item : in out Scanner; Value : out CCL.Language.Interpretation_Result)
   is
      First : Positive;
      Depth : Natural range 0 .. CCL.Language.MAX_NESTING := 0;
      Quoted, Escaped, Comment : Boolean := False;
      C : Character;
   begin
      Value := (others => <>);
      if Failed (Item) then return; end if;
      Skip (Item);
      First := Item.Cursor;
      while Item.Cursor <= Item.Last and then not Failed (Item) loop
         C := Item.Text (Item.Cursor);
         if Comment then
            if C = ASCII.LF then Comment := False; end if;
         elsif Quoted then
            if Escaped then Escaped := False;
            elsif C = '\' then Escaped := True;
            elsif C = '"' then Quoted := False;
            end if;
         elsif Depth = 0 and then (C = ')' or else Space (C) or else C = '#') then
            exit;
         elsif C = '#' then Comment := True;
         elsif C = '"' then Quoted := True;
         elsif C = '(' then
            if Depth = CCL.Language.MAX_NESTING then Item.Error := Nesting_Too_Deep;
            else Depth := Depth + 1;
            end if;
         elsif C = ')' then Depth := Depth - 1;
         end if;
         Item.Cursor := Item.Cursor + 1;
      end loop;
      if Failed (Item) then return; end if;
      CCL.Language.Interpret (Item.Text (First .. Item.Cursor - 1), 1_024, Value);
      if Value.Status /= CCL.Language.Succeeded then
         Item.Error := Invalid_Expression;
         if Value.Diagnostic_Position > 0 and then
           Value.Diagnostic_Position <= Item.Cursor - First + 1
         then
            Item.Cursor := First + Value.Diagnostic_Position - 1;
         end if;
      end if;
   end Evaluate;
end CCL.Declarations;
