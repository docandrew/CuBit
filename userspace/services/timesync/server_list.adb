pragma Ada_2022;
package body Server_List with SPARK_Mode is
   function Host_Character (C : Character) return Boolean is
     (C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '-');

   procedure Parse
     (Text : String; Servers : out Server_Array;
      Count : out SNTP.Server_Count; Success : out Boolean)
   is
      Position : Integer := Text'First;
   begin
      Servers := [others => <>];
      Count := 0;
      Success := False;
      loop
         pragma Loop_Invariant (Position >= Text'First);
         pragma Loop_Invariant
           (for all I in 1 .. Count =>
              Servers (I).Length >= 1 and then Servers (I).Port >= 1);
         while Position <= Text'Last and then Text (Position) = ' ' loop
            pragma Loop_Invariant (Position >= Text'First);
            Position := Position + 1;
         end loop;
         exit when Position > Text'Last;
         if Count = SNTP.Maximum_Servers then
            return; -- too many entries
         end if;
         declare
            Item : Server;
            Port : Natural := 0;
            Digits_Seen : Natural := 0;
         begin
            while Position <= Text'Last and then Host_Character (Text (Position)) loop
               pragma Loop_Invariant (Position >= Text'First);
               pragma Loop_Invariant (Item.Length <= Maximum_Host_Length);
               if Item.Length = Maximum_Host_Length then
                  return;
               end if;
               Item.Length := Item.Length + 1;
               Item.Host (Item.Length) := Text (Position);
               Position := Position + 1;
            end loop;
            if Item.Length = 0 then
               return;
            end if;
            if Position <= Text'Last and then Text (Position) = ':' then
               Position := Position + 1;
               while Position <= Text'Last and then Text (Position) in '0' .. '9' loop
                  pragma Loop_Invariant (Position >= Text'First);
                  pragma Loop_Invariant (Port <= 65_535 and then Digits_Seen <= 5);
                  if Digits_Seen = 5 then
                     return;
                  end if;
                  Port := Port * 10 + (Character'Pos (Text (Position)) - Character'Pos ('0'));
                  if Port > 65_535 then
                     return;
                  end if;
                  Digits_Seen := Digits_Seen + 1;
                  Position := Position + 1;
               end loop;
               if Digits_Seen = 0 or else Port = 0 then
                  return;
               end if;
               Item.Port := Unsigned_16 (Port);
            end if;
            if Position <= Text'Last and then Text (Position) /= ' ' then
               return; -- unexpected character
            end if;
            Count := Count + 1;
            Servers (Count) := Item;
         end;
      end loop;
      Success := Count >= 1;
   end Parse;

   procedure Scheme
     (Item : Server; Text : out String; Length : out Natural)
   is
      Prefix : constant String := "@net:udp:";
      Port : Unsigned_16 := Item.Port;
      Port_Text : String (1 .. 5) := [others => '0'];
      Port_Digits : Natural range 0 .. 5 := 0;
   begin
      Text := [others => ' '];
      Text (1 .. Prefix'Length) := Prefix;
      Length := Prefix'Length;
      for I in 1 .. Item.Length loop
         pragma Loop_Invariant (Length = Prefix'Length + I - 1);
         Length := Length + 1;
         Text (Length) := Item.Host (I);
      end loop;
      Length := Length + 1;
      Text (Length) := ':';
      loop
         pragma Loop_Invariant (Port_Digits < 5);
         Port_Digits := Port_Digits + 1;
         Port_Text (6 - Port_Digits) :=
           Character'Val (Character'Pos ('0') + Natural (Port mod 10));
         Port := Port / 10;
         exit when Port = 0 or else Port_Digits = 5;
      end loop;
      for I in 6 - Port_Digits .. 5 loop
         pragma Loop_Invariant (Length <= Maximum_Scheme_Length - (5 - I) - 1);
         Length := Length + 1;
         Text (Length) := Port_Text (I);
      end loop;
   end Scheme;
end Server_List;
