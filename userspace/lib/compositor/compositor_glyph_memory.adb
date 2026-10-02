package body Compositor_Glyph_Memory with SPARK_Mode => Off is
   use type Arena.Token;
   procedure Reserve
     (S : in out State; Size : Arena.Request_Bytes; T : out Arena.Token;
      Pixels : out System.Address; Capacity : out Natural) is
   begin
      Pixels := System.Null_Address;
      Capacity := 0;
      if not S.Started then
         Arena.Initialize (S.Policy);
         S.Started := True;
      end if;
      Arena.Reserve (S.Policy, Size, T);
      if T /= Arena.No_Token then
         Pixels := S.Data (Arena.Offset (T))'Address;
         Capacity := Arena.Capacity (T);
      end if;
   end Reserve;
   function Address_Of (S : in out State; T : Arena.Token) return System.Address is
   begin
      if not S.Started or else not Arena.Current (S.Policy, T) then return System.Null_Address; end if;
      return S.Data (Arena.Offset (T))'Address;
   end Address_Of;
   procedure Release
     (S : in out State; T : Arena.Token; Readers_Retired : Boolean; Released : out Boolean) is
   begin
      Released := False;
      if S.Started then Arena.Release (S.Policy, T, Readers_Retired, Released); end if;
   end Release;
end Compositor_Glyph_Memory;
