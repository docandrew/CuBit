with Ada.Text_IO;
with Compositor_Trace_Wire; use Compositor_Trace_Wire;
procedure Trace_Wire_Check is
   use type Word;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then
         raise Program_Error with Checks'Image;
      end if;
   end Check;
   procedure Round_Trip (V : Event; Last_Used : Natural) is
      P : constant Packet := Encode (V);
      Bad : Packet;
      D : constant Decoded := Decode (P);
   begin
      Check (D.Success and then D.Value = V);
      Check (P (0) = Magic and P (2) = V.Event_ID);
      for I in Last_Used + 1 .. 15 loop
         Bad := P; Bad (I) := 1;
         Check (not Decode (Bad).Success);
      end loop;
      Bad := P; Bad (0) := Magic + 1;
      Check (not Decode (Bad).Success);
      Bad := P; Bad (1) := 5;
      Check (not Decode (Bad).Success);
      Bad := P; Bad (2) := 0;
      Check (not Decode (Bad).Success);
   end Round_Trip;
   N : Word;
   P : Packet;
begin
   for I in 1 .. 1000 loop
      N := Word'Last - Word (I);
      Round_Trip ((Input_Event, N, (N, N - 1, 10, N - 2)), 6);
      Round_Trip ((Source_Event, N, (N, N - 1, N - 2, 0, N - 3)), 7);
      Round_Trip ((Render_Event, N,
                  (RT.Draw, 1, 3, N, N - 1, N - 2, N - 3, N - 4,
                   0, 0, N - 5)), 13);
      Round_Trip ((Render_Event, N,
                  (RT.Submit, 0, 1, N, N - 1, 0, 0, 0,
                   N - 2, N - 3, N - 4)), 13);
      Round_Trip ((Frame_Event, N, (1, N, N - 1, 0, N - 2)), 7);
   end loop;
   P := Encode ((Input_Event, Word'Last, (1, Word'Last, 1, 0)));
   Check (P (4) = Word'Last);
   P (5) := 11; Check (not Decode (P).Success);
   P (5) := 1; P (6) := Word'Last; Check (not Decode (P).Success);
   P := Encode ((Source_Event, 1, (1, 1, 1, Word'Last, 0)));
   Check (P (6) = Word'Last);
   P (4) := 0; Check (not Decode (P).Success);
   P := Encode ((Render_Event, 1, (RT.Draw, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0)));
   P (3) := 2; Check (not Decode (P).Success);
   P (3) := 0; P (4) := 2; Check (not Decode (P).Success);
   P (4) := 0; P (5) := 4; Check (not Decode (P).Success);
   P (5) := 1; P (11) := 1; Check (not Decode (P).Success);
   P := Encode ((Frame_Event, 1, (0, 1, 1, 1, 2)));
   P (3) := Word'Last; Check (not Decode (P).Success);
   P (3) := 0; P (6) := 3; Check (not Decode (P).Success);
   P (6) := 1; P (7) := Word'Last; Check (not Decode (P).Success);
   Ada.Text_IO.Put_Line ("PASS trace wire checks" & Checks'Image);
end Trace_Wire_Check;
