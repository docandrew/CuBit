with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Budget_Query;
with Intel_GPU_Budget_Protocol;
procedure Budget_Query_Tests is
   package Q renames Intel_GPU_Budget_Query;
   package B renames Q.B;
   package P renames Intel_GPU_Budget_Protocol;
   use type B.Budget_Words;
   Object : Q.Query;
   Token, Old, Rejected : Unsigned_64;
   Consumed : Boolean;
   Data : constant B.Budget_Words := B.Budget_Reply (True, B.Capacity, 4096, 15);
   procedure Reply (T, Now : Unsigned_64; Owner : Boolean := True;
                    Transport : Boolean := True) is
   begin
      Q.Complete (Object, T, Now, Owner, Transport,
                  B.Budget_Request_Label, 4, 0, 0, Data, Consumed);
   end Reply;
begin
   pragma Assert (P.Valid_Request (P.Label, 4, 0, 0, [B.Budget_Version, 0, 0, 0]));
   for I in 0 .. 3 loop
      declare
         Invalid : B.Budget_Words := [B.Budget_Version, 0, 0, 0];
      begin
         Invalid (I) := Invalid (I) + 1;
         pragma Assert (not P.Valid_Request (P.Label, 4, 0, 0, Invalid));
      end;
   end loop;
   for I in Unsigned_8 loop
      pragma Assert (P.Valid_Request (P.Label, I, 0, 0, [B.Budget_Version,0,0,0]) = (I = 4));
      pragma Assert (P.Valid_Request (P.Label, 4, I, 0, [B.Budget_Version,0,0,0]) = (I = 0));
   end loop;
   pragma Assert (not P.Valid_Request (P.Label + 1, 4, 0, 0, [B.Budget_Version,0,0,0]));
   pragma Assert (not P.Valid_Request (P.Label, 4, 0, 1, [B.Budget_Version,0,0,0]));
   pragma Assert (P.Response ((others => <>)) = B.Budget_Words'(P.Unavailable,0,0,0));
   Q.Start (Object, 0, False, Token); pragma Assert (Token = 0);
   Q.Start (Object, Unsigned_64'Last, True, Token); pragma Assert (Token = 0);
   for Iteration in 1 .. 100 loop
      Q.Start (Object, 100, True, Token);
      pragma Assert (Token /= 0 and Q.Pending (Object) and not Q.Result (Object).Known);
      Q.Start (Object, 100, True, Rejected); pragma Assert (Rejected = 0);
      Reply (Token - 1, 101); pragma Assert (not Consumed and Q.Pending (Object));
      Reply (Token, 102); pragma Assert (Consumed and not Q.Pending (Object));
      pragma Assert (Q.Result (Object).Known and Q.Result (Object).Retained_Bytes = 4096);
      pragma Assert (P.Response (Q.Result (Object)) = B.Budget_Words'(P.OK,B.Capacity,4096,15));
      Reply (Token, 103); pragma Assert (not Consumed);
      Q.Tick (Object, 104, False); pragma Assert (not Q.Result (Object).Known);
      pragma Assert (P.Response (Q.Result (Object)) = B.Budget_Words'(P.Unavailable,0,0,0));
   end loop;
   Q.Start (Object, 100, True, Old);
   Q.Tick (Object, 30_099, True); pragma Assert (Q.Pending (Object));
   Q.Tick (Object, 30_100, True); pragma Assert (not Q.Pending (Object));
   Q.Start (Object, 40_000, True, Token); pragma Assert (Token > Old);
   Reply (Old, 40_001); pragma Assert (not Consumed and Q.Pending (Object));
   Reply (Token, 40_002, Transport => False);
   pragma Assert (Consumed and not Q.Result (Object).Known);
   Q.Start (Object, 100, True, Token);
   Reply (Token, 99); pragma Assert (Consumed and not Q.Pending (Object) and not Q.Result (Object).Known);
   Q.Start (Object, 100, True, Token);
   Reply (Token, 101, Owner => False); pragma Assert (Consumed and not Q.Result (Object).Known);
   Q.Start (Object, 100, True, Token); Q.Cancel (Object);
   Reply (Token, 101); pragma Assert (not Consumed and not Q.Result (Object).Known);
   Ada.Text_IO.Put_Line ("Budget query PASS: async lifecycle, deadline, clock/owner loss, stale replies, cancellation; no IPC");
end Budget_Query_Tests;
