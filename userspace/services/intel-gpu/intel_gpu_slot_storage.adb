with System.Storage_Elements; use System.Storage_Elements;
package body Intel_GPU_Slot_Storage is
   function Round_Up (Bytes : Unsigned_64) return Unsigned_64 is
     (if Bytes = 0 then 0
      elsif Bytes > Unsigned_64'Last - (Page_Bytes - 1) then 0
      else (Bytes + Page_Bytes - 1) / Page_Bytes * Page_Bytes);

   function Open (Bytes : Unsigned_64) return Region is
      Wanted : constant Unsigned_64 := Round_Up (Bytes);
      Base : Unsigned_64;
   begin
      if Wanted = 0 then return (others => 0); end if;
      Base := Reserve (Wanted);
      if Base = 0 or else Base mod Page_Bytes /= 0 or else
        Base > Unsigned_64'Last - Wanted
      then return (others => 0); end if;
      return (Base => Base, Reserved => Wanted, Committed => 0);
   end Open;

   function Address (Object : Region) return System.Address is
     (To_Address (Integer_Address (Object.Base)));

   procedure Commit_Through (Object : in out Region; Bytes : Unsigned_64; OK : out Boolean) is
      Wanted : constant Unsigned_64 := Round_Up (Bytes);
      Step : Unsigned_64;
   begin
      OK := Bytes <= Object.Committed;
      if OK then return; end if;
      if not Valid (Object) or else Wanted = 0 or else Wanted > Object.Reserved then
         return;
      end if;
      while Object.Committed < Wanted loop
         Step := Unsigned_64'Min (Commit_Limit, Wanted - Object.Committed);
         if not Commit (Object.Base, Object.Committed, Step) then return; end if;
         Object.Committed := Object.Committed + Step;
      end loop;
      OK := True;
   end Commit_Through;
end Intel_GPU_Slot_Storage;
