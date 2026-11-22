with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with CuBit.Newest_Wins;
--  Hosted: the latest-wins rule under random positions, refused sends,
--  completions in any order relative to new positions, and resends. After
--  every step Settled_Or_Due holds; when traffic stops and the owner keeps
--  sending whenever Due, the receiver ends with the newest position.
procedure Newest_Wins_Tests is
   package NW renames CuBit.Newest_Wins;
   use type NW.Position;
   subtype Choice is Natural range 0 .. 99;
   package Random is new Ada.Numerics.Discrete_Random (Choice);
   G : Random.Generator;
   S : NW.State;
   Received : NW.Position := (-1, -1);
   Delivered_In_Flight : NW.Position;
   Checks, Sends, Skipped : Natural := 0;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Ada.Text_IO.Put_Line ("FAIL " & Name);
         raise Program_Error with Name;
      end if;
      Checks := Checks + 1;
   end Check;
   --  The owner's loop turn: send when due, the channel accepting 70 %.
   procedure Turn is
   begin
      if NW.Due (S) and then Random.Random (G) < 70 then
         NW.Submitted (S);
         Delivered_In_Flight := NW.Sent (S);
         Sends := Sends + 1;
      end if;
   end Turn;
begin
   Random.Reset (G, 5);
   for Run in 1 .. 2_000 loop
      for Step in 1 .. 50 loop
         case Random.Random (G) is
            when 0 .. 49 =>
               if NW.Due (S) then Skipped := Skipped + 1; end if;
               NW.Want (S, (Random.Random (G), Step));
            when 50 .. 79 =>
               if NW.In_Flight (S) then
                  Received := Delivered_In_Flight;
                  NW.Completed (S);
               end if;
            when 80 .. 81 =>
               NW.Resend (S);
            when others => null;
         end case;
         Turn;
         Check (NW.Settled_Or_Due (S), "settled or due");
      end loop;
      --  Traffic stops: completions and turns until settled.
      for Drain in 1 .. 1_000 loop
         exit when not NW.In_Flight (S) and then not NW.Due (S);
         if NW.In_Flight (S) then
            Received := Delivered_In_Flight;
            NW.Completed (S);
         end if;
         Turn;
      end loop;
      Check (not NW.In_Flight (S) and then NW.Sent (S) = NW.Wanted (S) and then
             Received = NW.Wanted (S), "newest position delivered");
   end loop;
   Ada.Text_IO.Put_Line ("PASS newest-wins:" & Natural'Image (Checks) & " checks," &
                         Natural'Image (Sends) & " sends");
end Newest_Wins_Tests;
