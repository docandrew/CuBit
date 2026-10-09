package body Observatory_Archive_Stream with SPARK_Mode is
   use type A.Word;
   procedure Start (S : out State; Page : V.Page_Number) is
   begin
      S := (others => <>);
      V.Start (S.View, [others => 0], Page);
   end Start;
   procedure Append (S : in out State; Value : Byte) is
      Index : constant Natural := S.Pending / 8;
      Shift : constant Natural := (S.Pending mod 8) * 8;
   begin
      if S.Ended or S.Seen = Maximum_Bytes then
         V.Start (S.View, [others => 0], V.Page (S.View));
         S.Ended := True;
         return;
      end if;
      S.Seen := S.Seen + 1;
      if Shift = 0 then S.Chunk (Index) := 0; end if;
      S.Chunk (Index) := S.Chunk (Index) or Interfaces.Shift_Left (A.Word (Value), Shift);
      if S.Pending = 255 then
         if S.First then
            V.Start (S.View, S.Chunk, V.Page (S.View));
            S.First := False;
         else V.Feed (S.View, S.Chunk); end if;
         S.Pending := 0;
      else S.Pending := S.Pending + 1; end if;
   end Append;
   procedure Finish (S : in out State) is
   begin
      S.Ended := True;
      V.Finish (S.View, S.Pending);
   end Finish;
end Observatory_Archive_Stream;
