package body Boot_Logs_Repaint with SPARK_Mode is
   procedure Clear (S : in out Snapshot) is
   begin
      S.Length := 0;
      S.Overflow := False;
   end Clear;

   --  One line: its position, its colour mark, its text and a line end.
   procedure Add_Line (S : in out Snapshot; Y : Natural; Text : String; Danger : Boolean) is
      Position : constant String := Natural'Image (Y);
      Mark : constant Character := (if Danger then '!' else ' ');
      Room : constant Natural := S.Capacity - S.Length;
   begin
      if S.Overflow then
         return;
      end if;
      --  Position is at most 11 characters (Natural'Image).
      if Position'Length > Room or else Text'Length > Room - Position'Length or else
        Room - Position'Length - Text'Length < 2
      then
         S.Overflow := True;
         return;
      end if;
      S.Data (S.Length + 1 .. S.Length + Position'Length) := Position;
      S.Length := S.Length + Position'Length;
      S.Data (S.Length + 1) := Mark;
      S.Length := S.Length + 1;
      S.Data (S.Length + 1 .. S.Length + Text'Length) := Text;
      S.Length := S.Length + Text'Length;
      S.Data (S.Length + 1) := ASCII.LF;
      S.Length := S.Length + 1;
   end Add_Line;

   function First_Change (A, B : Snapshot) return String is
      Common : constant Natural := Natural'Min (A.Length, B.Length);
      At_Change : Natural := 0;
      Line_Start, Line_End : Natural;
   begin
      for I in 1 .. Common loop
         if A.Data (I) /= B.Data (I) then
            At_Change := I;
            exit;
         end if;
      end loop;
      if At_Change = 0 then
         if A.Length = B.Length then
            return "";
         end if;
         At_Change := Common + 1;
      end if;
      if At_Change > B.Length then
         return "(lines removed)";
      end if;
      --  The line around At_Change (1 .. B.Length here).
      Line_Start := 1;
      for I in reverse 1 .. At_Change - 1 loop
         if B.Data (I) = ASCII.LF then
            Line_Start := I + 1;
            exit;
         end if;
      end loop;
      Line_End := B.Length;
      for I in At_Change .. B.Length loop
         pragma Loop_Invariant (Line_Start <= At_Change);
         if B.Data (I) = ASCII.LF then
            Line_End := I - 1;
            exit;
         end if;
      end loop;
      if Line_End < Line_Start then
         return "";
      end if;
      return B.Data (Line_Start .. Line_End);
   end First_Change;
end Boot_Logs_Repaint;
