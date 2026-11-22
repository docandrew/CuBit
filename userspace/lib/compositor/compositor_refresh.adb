package body Compositor_Refresh with SPARK_Mode is
   procedure Request (S : in out State) is
   begin S.Wanted := True; end Request;
   procedure Start (S : in out State) is
   begin S := (Stage => Listing, Wanted => False, Position => 0, Total => 0); end Start;
   procedure Listed (S : in out State; N : Item_Count) is
   begin
      S := (Stage => (if N = 0 then Ready else Reading), Wanted => S.Wanted,
            Position => (if N = 0 then 0 else 1), Total => N);
   end Listed;
   procedure Read_Item (S : in out State) is
   begin
      if S.Position = S.Total then S.Stage := Ready;
      else S.Position := S.Position + 1;
      end if;
   end Read_Item;
   procedure Publish (S : in out State; Visible : Boolean; Published : out Boolean) is
   begin
      Published := S.Stage = Ready and not Visible;
      if Published then S.Stage := Idle; end if;
   end Publish;
end Compositor_Refresh;
