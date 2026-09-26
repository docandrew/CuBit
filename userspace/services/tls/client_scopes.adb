pragma Ada_2022;
package body Client_Scopes with SPARK_Mode is
   procedure Install
     (Item : in out Table; Client : Unsigned_64; Scopes : Scope_List;
      Count : Scope_Count; Success : out Boolean)
   is
   begin
      Revoke (Item, Client);
      Success := False;
      for E of Item.Clients loop
         if E.Client = 0 then
            E := (Client => Client, Count => Count, Scopes => Scopes);
            Success := True;
            return;
         end if;
      end loop;
   end Install;

   procedure Revoke (Item : in out Table; Client : Unsigned_64) is
   begin
      for I in Item.Clients'Range loop
         pragma Loop_Invariant
           (for all J in Item.Clients'First .. I - 1 =>
              Item.Clients (J).Client /= Client);
         if Item.Clients (I).Client = Client then
            Item.Clients (I) := (others => <>);
         end if;
      end loop;
   end Revoke;

   function Allows
     (Item : Table; Client : Unsigned_64; Name : String; Port : Unsigned_16)
      return Boolean is
   begin
      if Client = 0 then
         return False;
      end if;
      for E of Item.Clients loop
         if E.Client = Client then
            for I in 1 .. E.Count loop
               if CuBit.TLS_Scopes.Allows (E.Scopes (I), Name, Port) then
                  return True;
               end if;
            end loop;
         end if;
      end loop;
      return False;
   end Allows;
end Client_Scopes;
