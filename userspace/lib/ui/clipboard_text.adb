package body Clipboard_Text with SPARK_Mode is

   procedure Single_Line (Text : in out String; Kept : out Natural) is
   begin
      Kept := 0;
      for I in Text'Range loop
         pragma Loop_Invariant (Kept <= I - Text'First);
         pragma Loop_Invariant
           (for all J in Text'First .. Text'First + Kept - 1 =>
              Is_Printable (Text (J)));
         if Is_Printable (Text (I)) then
            Text (Text'First + Kept) := Text (I);
            Kept := Kept + 1;
         elsif Text (I) = ASCII.LF or else Text (I) = ASCII.CR or else
           Text (I) = ASCII.HT
         then
            Text (Text'First + Kept) := ' ';
            Kept := Kept + 1;
         end if;
      end loop;
   end Single_Line;

   procedure Clear (B : in out Builder) is
   begin
      B.Length := 0;
      B.Fits := True;
   end Clear;

   procedure Append (B : in out Builder; Text : String) is
   begin
      if B.Fits and then Text'Length <= B.Capacity - B.Length then
         for I in Text'Range loop
            pragma Loop_Invariant
              (B.Length + (Text'Last - I) + 1 <= B.Capacity and then B.Fits);
            B.Length := B.Length + 1;
            B.Data (B.Length) := Text (I);
         end loop;
      else
         B.Fits := False;
      end if;
   end Append;

   procedure Append_Quoted (B : in out Builder; Text : String) is
   begin
      Append (B, """");
      for I in Text'Range loop
         pragma Loop_Invariant
           (B.Length <= B.Capacity and then (if not B'Loop_Entry.Fits then not B.Fits));
         case Text (I) is
            when '"'      => Append (B, "\""");
            when '\'      => Append (B, "\\");
            when ASCII.LF => Append (B, "\n");
            when ASCII.CR => Append (B, "\r");
            when ASCII.HT => Append (B, "\t");
            when ' ' .. '!' | '#' .. '[' | ']' .. '~' => Append (B, [1 => Text (I)]);
            when others   => B.Fits := False;
         end case;
      end loop;
      Append (B, """");
   end Append_Quoted;
end Clipboard_Text;
