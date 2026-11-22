------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Deskbar's task buttons (see the spec).
------------------------------------------------------------------------------
pragma Ada_2022;

package body Desktop_Task_Groups with SPARK_Mode is

   procedure Build (W : Windows; M : out Model) is
   begin
      M.Groups := [others => (others => <>)];
      M.Count := 0;
      M.Of_Window := [others => 0];
      for S in W'Range loop
         if W (S).Present then
            declare
               Found : Group_Count := 0;
            begin
               for G in 1 .. M.Count loop
                  if M.Groups (G).Key = W (S).Key then
                     Found := G;
                     exit;
                  end if;
                  pragma Loop_Invariant (Found = 0);
                  pragma Loop_Invariant
                    (for all H in 1 .. G => M.Groups (H).Key /= W (S).Key);
               end loop;
               if Found = 0 then
                  M.Count := M.Count + 1;
                  M.Groups (M.Count) :=
                    (Key       => W (S).Key,
                     First     => S,
                     Members   => 1,
                     Active    => W (S).Focused and then not W (S).Minimized,
                     Minimized => W (S).Minimized);
                  M.Of_Window (S) := M.Count;
               else
                  M.Groups (Found).Members := M.Groups (Found).Members + 1;
                  if Earlier (W, S, M.Groups (Found).First) then
                     M.Groups (Found).First := S;
                  end if;
                  M.Groups (Found).Active := M.Groups (Found).Active or else
                    (W (S).Focused and then not W (S).Minimized);
                  M.Groups (Found).Minimized := M.Groups (Found).Minimized and then W (S).Minimized;
                  M.Of_Window (S) := Found;
               end if;
            end;
         end if;
         pragma Loop_Invariant (Fits (W, M.Last_Slot, M.Groups_Room));
         pragma Loop_Invariant (Integer (M.Count) <= Integer (S) + 1);
         pragma Loop_Invariant
           (for all G in 1 .. M.Count =>
              M.Groups (G).Members >= 1 and then
              Integer (M.Groups (G).Members) <= Integer (S) + 1);
         pragma Loop_Invariant
           (for all T in W'Range =>
              (if T <= S and then W (T).Present then
                 M.Of_Window (T) in 1 .. M.Count and then
                 M.Groups (M.Of_Window (T)).Key = W (T).Key
               else M.Of_Window (T) = 0));
         pragma Loop_Invariant
           (for all G in 1 .. M.Count =>
              M.Groups (G).First <= S and then
              W (M.Groups (G).First).Present and then
              M.Of_Window (M.Groups (G).First) = G and then
              (for all T in W'Range =>
                 (if M.Of_Window (T) = G then not Earlier (W, T, M.Groups (G).First))));
         pragma Loop_Invariant
           (for all G in 1 .. M.Count =>
              (for all H in 1 .. M.Count =>
                 (if G /= H then M.Groups (G).Key /= M.Groups (H).Key)));
      end loop;
   end Build;

   function Position (W : Windows; M : Model; G : Group_Index) return Group_Index is
      Before : Group_Count := 0;
   begin
      for H in 1 .. M.Count loop
         if H /= G and then Earlier (W, M.Groups (H).First, M.Groups (G).First) then
            Before := Before + 1;
         end if;
         pragma Loop_Invariant (Before <= H - (if G <= H then 1 else 0));
      end loop;
      return Before + 1;
   end Position;

   function At_Position (W : Windows; M : Model; P : Group_Index) return Group_Count is
   begin
      for G in 1 .. M.Count loop
         pragma Loop_Invariant (Well_Formed (W, M));
         if Position (W, M, G) = P then
            return G;
         end if;
      end loop;
      return 0;
   end At_Position;

   function Rank (W : Windows; S : Window_Slot) return Window_Rank is
      Before : Window_Count := 0;
   begin
      for T in W'Range loop
         if T /= S and then W (T).Present and then W (T).Key = W (S).Key and then
           Earlier (W, T, S)
         then
            Before := Before + 1;
         end if;
         pragma Loop_Invariant
           (Integer (Before) <= Integer (T) + 1 - (if S <= T then 1 else 0));
      end loop;
      return Before + 1;
   end Rank;

   procedure Member_At
     (W : Windows; Key : Group_Key; R : Window_Rank;
      Found : out Boolean; Slot : out Window_Slot) is
   begin
      Found := False;
      Slot := W'First;
      for S in W'Range loop
         if W (S).Present and then W (S).Key = Key and then Rank (W, S) = R then
            Found := True;
            Slot := S;
            return;
         end if;
      end loop;
   end Member_At;

end Desktop_Task_Groups;
