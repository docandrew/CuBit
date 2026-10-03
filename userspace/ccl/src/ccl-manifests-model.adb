package body CCL.Manifests.Model with SPARK_Mode => On is
   function Valid_Binding_Name (Item : String) return Boolean is
   begin
      if Item'Length not in 1 .. Binding_Name_Length'Last or else Item (Item'First) not in 'a' .. 'z' then
         return False;
      end if;
      for Index in Item'Range loop
         if Item (Index) = '-' then
            if Index = Item'Last or else Item (Index + 1) = '-' then return False; end if;
         elsif Item (Index) not in 'a' .. 'z' | '0' .. '9' then
            return False;
         end if;
      end loop;
      return True;
   end Valid_Binding_Name;

   function Valid_Scope_Path (Path : String) return Boolean is
   begin
      if Path'Length not in 1 .. Binding_Name_Length'Last then return False; end if;
      for I in Path'Range loop
         declare
            C : constant Character := Path (I);
         begin
            if C not in ' ' .. '~' or else C in '*' | '?' | '\' then return False; end if;
            if C = '.' and then (I = Path'First or else Path (I - 1) = '/') then
               if I = Path'Last or else Path (I + 1) = '/' then
                  return False;
               elsif Path (I + 1) = '.' and then (I + 1 = Path'Last or else Path (I + 2) = '/') then
                  return False;
               end if;
            end if;
            if C = '/' and then I < Path'Last and then Path (I + 1) = '/' then return False; end if;
         end;
      end loop;
      return True;
   end Valid_Scope_Path;
end CCL.Manifests.Model;
