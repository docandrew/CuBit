with Ada.Text_IO; with Interfaces; use Interfaces;
with Compositor_Render_Trace;
procedure Render_Trace_Tests is
   package T renames Compositor_Render_Trace;
   use type T.Record_Value;
   S : T.State;
   V : T.Record_Value := (T.Draw,0,1,1,1,42,1,1,0,0,0);
begin
   for Batch in 1 .. 1000 loop
      T.Reset(S);
      for I in 1 .. 80 loop
         V.Writer_Serial:=Unsigned_64(I); V.Observed:=Unsigned_64(I);
         T.Add(S,V);
         if I<=64 then pragma Assert(T.Item(S,I)=V); end if;
      end loop;
      pragma Assert(T.Count(S)=64 and T.Lost(S)=16);
      T.Note_Unsupported(S);
      pragma Assert(T.Unsupported(S)=1 and T.Count(S)=64);
   end loop;
   T.Reset(S);
   V:=(T.Submit,1,3,Unsigned_64'Last,Unsigned_64'Last,0,0,0,1,1,0);
   T.Add(S,V); pragma Assert(T.Count(S)=1);
   V.Buffer:=0; T.Add(S,V); V.Buffer:=4; T.Add(S,V); V.Buffer:=1;
   V.Surface:=42; T.Add(S,V); V.Surface:=0;
   V.Frame:=0; T.Add(S,V); V.Frame:=1;
   V.Observed:=Unsigned_64'Last; T.Add(S,V);
   pragma Assert(T.Count(S)=1 and T.Invalid(S)=5);
   T.Reset(S);
   pragma Assert(T.Count(S)=0 and T.Unsupported(S)=0 and T.Lost(S)=0);
   pragma Assert(T.Increment(Natural'Last)=Natural'Last);
   Ada.Text_IO.Put_Line("RENDER-TRACE: PASS 1000 bounded batches, exact identities, phase validation and unsupported/reset accounting");
end Render_Trace_Tests;
