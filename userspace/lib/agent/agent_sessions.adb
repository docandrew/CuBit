pragma Ada_2022;
package body Agent_Sessions with SPARK_Mode is
   use type P.Role;

   function Escape_Width (C : Character) return Positive is
     (if C in '"' | '\' | ASCII.LF | ASCII.CR | ASCII.HT then 2 else 1);

   function Quoted_Length (Text : String) return Natural
   with Pre => Text'Length <= Maximum_Field,
        Post => Quoted_Length'Result <= 2 * Text'Length
   is
      Total : Natural := 0;
   begin
      for K in 1 .. Text'Length loop
         pragma Loop_Invariant (Total <= 2 * (K - 1));
         Total := Total + Escape_Width (Text (Text'First + (K - 1)));
      end loop;
      return Total;
   end Quoted_Length;

   function Quote (Text : String) return String is
      Result : String (1 .. Quoted_Length (Text)) := [others => ' '];
      Last : Natural := 0;
   begin
      for C of Text loop
         pragma Loop_Invariant (Last <= Result'Last);
         declare
            Piece : constant String :=
              (case C is
                  when '"' => "\""",
                  when '\' => "\\",
                  when ASCII.LF => "\n",
                  when ASCII.CR => "\r",
                  when ASCII.HT => "\t",
                  when others => [1 => C]);
         begin
            for D of Piece loop
               pragma Loop_Invariant (Last <= Result'Last);
               if Last < Result'Last then
                  Last := Last + 1;
                  Result (Last) := D;
               end if;
            end loop;
         end;
      end loop;
      return Result;
   end Quote;

   function One_Record
     (Item_Role : P.Role; Kind : P.Block_Kind; Failed : Boolean; A, B, C : String; More : Boolean)
      return String
   is
     ("(Agent_Record role => Agent_Role." & (if Item_Role = P.User then "User" else "Assistant") &
      " kind => Agent_Block_Kind." & Kind_Name (Kind) &
      " failed => " & (if Failed then "true" else "false") &
      " a => """ & Quote (A) & """ b => """ & Quote (B) & """ c => """ & Quote (C) & """" &
      " more => " & (if More then "true" else "false") & ")" & ASCII.LF)
   with Pre => A'Length <= Field_Chunk and then B'Length <= Field_Chunk and then C'Length <= Field_Chunk;

   function Lines
     (Item_Role : P.Role; Kind : P.Block_Kind; Failed : Boolean; A, B, C : String) return String
   is
      --  The first Field_Chunk bytes of the three fields, in order, and the
      --  rest in the records after.
      Total : constant Natural := A'Length + B'Length + C'Length;
   begin
      if Total <= Field_Chunk then
         return One_Record (Item_Role, Kind, Failed, A, B, C, More => False);
      end if;
      declare
         Room : Natural := Field_Chunk;
         A_Take : constant Natural := Natural'Min (A'Length, Room);
      begin
         Room := Room - A_Take;
         declare
            B_Take : constant Natural := Natural'Min (B'Length, Room);
         begin
            Room := Room - B_Take;
            declare
               C_Take : constant Natural := Natural'Min (C'Length, Room);
            begin
               return One_Record
                        (Item_Role, Kind, Failed, A (A'First .. A'First + A_Take - 1),
                         B (B'First .. B'First + B_Take - 1), C (C'First .. C'First + C_Take - 1), More => True)
                 & Lines (Item_Role, Kind, Failed, A (A'First + A_Take .. A'Last),
                          B (B'First + B_Take .. B'Last), C (C'First + C_Take .. C'Last));
            end;
         end;
      end;
   end Lines;
end Agent_Sessions;
