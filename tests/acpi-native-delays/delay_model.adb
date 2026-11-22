pragma Ada_2022;
package body Delay_Model is
   use type Interfaces.Unsigned_64;
   procedure Read (Value : out Interfaces.Unsigned_64; Ready : out Boolean) is
   begin
      Reads := Reads + 1; Ready := Available;
      if not Ready then Value := 0;
      elsif Regress and Reads = 2 then Value := Now - 1;
      else Value := Now; if not Stopped then Now := Now + Step; end if;
      end if;
   end Read;
   function Yield_Result return Interfaces.Unsigned_64 is
   begin
      Yields := Yields + 1;
      return (if Refuse_Yield then 1 else 0);
   end Yield_Result;
   function Sleep_Result (Target : Interfaces.Unsigned_64) return Interfaces.Unsigned_64 is
   begin
      Sleeps := Sleeps + 1;
      if Refuse_Sleep then return 1; end if;
      if Late then Now := 20_000;
      elsif not Stopped then Now := Interfaces.Unsigned_64'Max (Now, Target);
      end if;
      return 0;
   end Sleep_Result;
end Delay_Model;
