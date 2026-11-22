pragma Ada_2022;
package body Agent_Missions with SPARK_Mode is

   procedure Find (Name : String; Item : out Tool; Found : out Boolean) is
   begin
      Item := Tool'First;
      Found := False;
      for T in Tool loop
         if Tool_Name (T) = Name then
            Item := T;
            Found := True;
            return;
         end if;
      end loop;
   end Find;

   function Confined_Path (Path : String) return Boolean is
      Start : Natural;
   begin
      if Path'Length = 0 then
         return True;
      elsif Path'Length > Maximum_Path
        or else (for some C of Path => C not in ' ' .. '~' or else C = '\')
      then
         return False;
      end if;
      Start := Path'First;
      for I in Path'Range loop
         pragma Loop_Invariant (Start in Path'First .. I);
         if Path (I) = '/' or else I = Path'Last then
            declare
               Last : constant Natural := (if Path (I) = '/' then I - 1 else I);
               Component : constant String := Path (Start .. Last);
            begin
               if Component'Length = 0 or else Component = "." or else Component = ".." then
                  return False;
               end if;
            end;
            if Path (I) = '/' then
               if I = Path'Last then
                  return False;
               end if;
               Start := I + 1;
            end if;
         end if;
      end loop;
      return True;
   end Confined_Path;
end Agent_Missions;
