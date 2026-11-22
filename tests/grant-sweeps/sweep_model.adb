pragma Ada_2022;

package body Sweep_Model with SPARK_Mode is

   procedure Link_Front (S : in out State; X : Live_Slot) is
      Old_Head : constant Slot := S.Head;
   begin
      S.Ranks (X) := S.Floor;
      S.Floor := S.Floor - 1;
      S.Next (X) := Old_Head;
      S.Prev (X) := No_Slot;
      if Old_Head /= No_Slot then
         S.Prev (Old_Head) := X;
      end if;
      S.Head := X;
      S.Listed (X) := True;
   end Link_Front;

   procedure Unlink (S : in out State; X : Live_Slot) is
      P : constant Slot := S.Prev (X);
      N : constant Slot := S.Next (X);
      Old_Ahead : constant Cursor_Slots := S.Ahead;
   begin
      --  Grant_Sweeps.Pass.
      for C in Cursor loop
         if S.Ahead (C) = X then
            S.Ahead (C) := N;
         end if;
         pragma Loop_Invariant
           (for all D in Cursor =>
              (if D <= C and then Old_Ahead (D) = X then S.Ahead (D) = N
               else S.Ahead (D) = Old_Ahead (D)));
      end loop;
      if P /= No_Slot then
         S.Next (P) := N;
      else
         S.Head := N;
      end if;
      if N /= No_Slot then
         S.Prev (N) := P;
      end if;
      S.Listed (X) := False;
      S.Next (X) := No_Slot;
      S.Prev (X) := No_Slot;
   end Unlink;

   procedure Start (S : in out State; C : Cursor) is
   begin
      S.Ahead (C) := S.Head;
      S.Last (C) := No_Visit;
      for X in Live_Slot loop
         S.Seen (C, X) := False;
         pragma Loop_Invariant
           (for all Y in Live_Slot => (if Y <= X then not S.Seen (C, Y)));
         pragma Loop_Invariant
           (for all D in Cursor =>
              (for all Y in Live_Slot =>
                 (if D /= C then S.Seen (D, Y) = S.Seen'Loop_Entry (D, Y))));
      end loop;
   end Start;

   procedure Step (S : in out State; C : Cursor; Visited : out Live_Slot) is
   begin
      Visited := S.Ahead (C);
      S.Ahead (C) := S.Next (Visited);
      S.Last (C) := S.Ranks (Visited);
      S.Seen (C, Visited) := True;
   end Step;

end Sweep_Model;
