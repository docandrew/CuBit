------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body Wifi_BSS_Table with SPARK_Mode is
   use type WS.Hearing_Count;

   function Find (T : Table; BSSID : NL.Hardware_Address) return Count_Type
   with Post => (if Find'Result = 0 then not Contains (T, BSSID)
                 else Find'Result <= T.Count and then T.Entries (Find'Result).BSSID = BSSID)
   is
   begin
      for I in 1 .. T.Count loop
         pragma Loop_Invariant (for all J in 1 .. I - 1 => T.Entries (J).BSSID /= BSSID);
         if T.Entries (I).BSSID = BSSID then
            return I;
         end if;
      end loop;
      return 0;
   end Find;

   function Weakest (T : Table) return Index
   with Pre  => T.Count > 0,
        Post => Weakest'Result <= T.Count
                and then (for all J in 1 .. T.Count =>
                            T.Entries (Weakest'Result).Signal <= T.Entries (J).Signal)
   is
      W : Index := 1;
   begin
      for I in 2 .. T.Count loop
         pragma Loop_Invariant
           (W < I and then (for all J in 1 .. I - 1 => T.Entries (W).Signal <= T.Entries (J).Signal));
         if T.Entries (I).Signal < T.Entries (W).Signal then
            W := I;
         end if;
      end loop;
      return W;
   end Weakest;

   function Merge (Old, Heard : WS.BSS_Entry) return WS.BSS_Entry
   with Pre  => WS.Normal (Old) and then WS.Normal (Heard) and then Old.BSSID = Heard.BSSID,
        Post => WS.Normal (Merge'Result) and then Merge'Result.BSSID = Old.BSSID
   is
      M : WS.BSS_Entry := Old;
   begin
      if Heard.Signal > Old.Signal then
         M.Signal := Heard.Signal;
         M.On_Band := Heard.On_Band;
         M.Channel := Heard.Channel;
      end if;
      if Heard.Length > 0 then
         M.SSID := Heard.SSID;
         M.Length := Heard.Length;
      end if;
      M.Capability := Heard.Capability;
      M.Interval := Heard.Interval;
      M.RSN := Heard.RSN;
      M.Heard := (if Old.Heard = WS.Hearing_Count'Last then Old.Heard else Old.Heard + 1);
      return M;
   end Merge;

   procedure Observe (T : in out Table; Heard : WS.BSS_Entry) is
      Found : constant Count_Type := Find (T, Heard.BSSID);
      Fresh : WS.BSS_Entry := Heard;
   begin
      Fresh.Heard := 1;
      if Found /= 0 then
         T.Entries (Found) := Merge (T.Entries (Found), Heard);
      elsif T.Count < Capacity then
         T.Count := T.Count + 1;
         T.Entries (T.Count) := Fresh;
      else
         declare
            W : constant Index := Weakest (T);
         begin
            if Heard.Signal > T.Entries (W).Signal then
               T.Entries (W) := Fresh;
            end if;
            if T.Evicted < Natural'Last then
               T.Evicted := T.Evicted + 1;
            end if;
         end;
      end if;
   end Observe;

   function Publish (T : Table; Passes : WS.Pass_Count; Channels : WS.Channel_Count)
     return WS.Scan_Result
   is
      Work : Entry_Array := T.Entries;
      Keep : constant WS.Entry_Count := Natural'Min (T.Count, WS.Maximum_Entries);
      R : WS.Scan_Result;
      On_2_4, On_5 : WS.Network_Count := 0;
   begin
      for I in 1 .. T.Count loop
         pragma Loop_Invariant (On_2_4 + On_5 = I - 1);
         if T.Entries (I).On_Band = WS.Band_2_4_GHz then
            On_2_4 := On_2_4 + 1;
         else
            On_5 := On_5 + 1;
         end if;
      end loop;

      --  Partial selection sort: positions 1 .. Keep get the strongest, in
      --  descending order; Work stays a permutation of the table's entries.
      for P in 1 .. Keep loop
         pragma Loop_Invariant (for all I in 1 .. T.Count => WS.Normal (Work (I)));
         pragma Loop_Invariant
           (for all I in 1 .. P - 1 =>
              (for all J in I + 1 .. T.Count => Work (I).Signal >= Work (J).Signal));
         declare
            Best : Index := P;
            Swap : WS.BSS_Entry;
         begin
            for J in P + 1 .. T.Count loop
               pragma Loop_Invariant
                 (Best in P .. J - 1
                  and then (for all K in P .. J - 1 => Work (Best).Signal >= Work (K).Signal));
               if Work (J).Signal > Work (Best).Signal then
                  Best := J;
               end if;
            end loop;
            Swap := Work (P);
            Work (P) := Work (Best);
            Work (Best) := Swap;
         end;
      end loop;

      R.Summary := (Count    => Keep,
                    Passes   => Passes,
                    Total    => T.Count,
                    On_2_4   => On_2_4,
                    On_5     => On_5,
                    Channels => Channels);
      for I in 1 .. Keep loop
         pragma Loop_Invariant
           (for all J in WS.Entry_Index =>
              (if J < I then R.Entries (J) = Work (J) else R.Entries (J) = WS.No_Entry));
         R.Entries (I) := Work (I);
      end loop;
      return R;
   end Publish;

end Wifi_BSS_Table;
