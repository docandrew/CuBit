package body Vulkan_Glyph_Sources with SPARK_Mode is
   function Resolve (S : State; Submission : V.State; K : Key) return V.Source_Ticket is
   begin
      for I in Slot loop
         if Matches (S, I, K) and then V.Source_Valid (Submission, S.Items (I).Source) then
            return S.Items (I).Source;
         end if;
         pragma Loop_Invariant (for all J in Slot'First .. I =>
           not Matches (S, J, K) or else not V.Source_Valid (Submission, S.Items (J).Source));
      end loop;
      return V.No_Source;
   end Resolve;
   procedure Bind
     (S : in out State; Submission : V.State; I : Slot; K : Key;
      Source : V.Source_Ticket; Raster : L.Layout; Accepted : out Boolean) is
   begin
      Accepted := False;
      if V.Current (Submission) /= V.Idle or else not V.Source_Valid (Submission, Source) or else
        V.Source_Valid (Submission, S.Items (I).Source) or else
        not L.Valid (Raster) or else not L.Same_Raster (Raster, L.Plan (K.Scale)) then return; end if;
      for J in Slot loop
         if V.Source_Valid (Submission, S.Items (J).Source) and then
           (S.Items (J).Source = Source or else Matches (S, J, K)) then return; end if;
         pragma Loop_Invariant (for all N in Slot'First .. J =>
           (if V.Source_Valid (Submission, S.Items (N).Source) then
              S.Items (N).Source /= Source and not Matches (S, N, K)));
      end loop;
      S.Items (I) := (K, Source);
      pragma Assert (for all J in Slot =>
         (if J /= I and V.Source_Valid (Submission, S.Items (J).Source) then not Matches (S, J, K)));
      pragma Assert (Matches (S, I, K) and V.Source_Valid (Submission, S.Items (I).Source));
      pragma Assert (Resolve (S, Submission, K) = Source);
      Accepted := True;
   end Bind;
   procedure Forget (S : in out State; Submission : V.State; I : Slot; Accepted : out Boolean) is
   begin
      Accepted := False;
      if V.Current (Submission) /= V.Idle or else V.Source_Valid (Submission, S.Items (I).Source) then return; end if;
      S.Items (I).Source := V.No_Source; Accepted := True;
   end Forget;
end Vulkan_Glyph_Sources;
