package body Compositor_Glyph_Arena with SPARK_Mode is
   procedure Initialize (S : out State) is
   begin
      H.Initialize (S.Runs);
      S.Serial := 0;
      S.Ids := (others => 0);
   end Initialize;
   procedure Reserve (S : in out State; Size : Request_Bytes; T : out Token) is
      Count : constant H.Run_Length := (Size - 1) / Cell_Bytes + 1;
      First : H.Page_Reference;
   begin
      T := No_Token;
      if S.Serial = Last_Identity then return; end if;
      H.Allocate (S.Runs, Count, 1, First);
      if First = H.No_Page then return; end if;
      S.Serial := Compositor_Identity.Next (S.Serial, Last_Identity);
      S.Ids (First) := S.Serial;
      T := (First, Count, S.Serial);
   end Reserve;
   procedure Release (S : in out State; T : Token; Readers_Retired : Boolean;
                      Released : out Boolean) is
   begin
      Released := False;
      if Current (S, T) and Readers_Retired then
         H.Release (S.Runs, T.First, Released);
      end if;
   end Release;
end Compositor_Glyph_Arena;
