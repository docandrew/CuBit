with Intel_GPU_Ring_Reservation;
package body Intel_GPU_Live_Ring_Publish with SPARK_Mode is
   function State (Object : Channel) return Phase is (Object.Value);
   function Tail (Object : Channel) return Unsigned_32 is (Object.Current_Tail);
   function Sequence (Object : Channel) return Unsigned_32 is (Object.Current_Sequence);
   procedure Fail (Object : in out Channel) is
   begin Object.Value := Quarantined; end Fail;
   procedure Append_Words (Object : in out Channel;
                     Segment : Intel_GPU_ADLN_Context_Init.Segment;
                     Count, Marker_Index : Natural;
                     Status : out Result) is
      Marker : Unsigned_64;
      Saved_Tail : Unsigned_32;
      OK : Boolean;
      Next_Tail : Unsigned_32;
      Bytes : constant Unsigned_32 := Unsigned_32 (Count) * 4;
      Plan : Intel_GPU_Ring_Reservation.Plan;
      use type Intel_GPU_Ring_Reservation.Outcome;
   begin
      Status := Rejected;
      if Object.Value /= Available or else not Segment.Valid then return; end if;
      if Object.Current_Sequence = Unsigned_32'Last or else
        Segment.Words (Marker_Index) /= Object.Current_Sequence + 1 or else
        Segment.Words (Marker_Index + 1) /= 0
      then return; end if;
      -- Latch before the first callback; no retry after any ambiguous access.
      Object.Value := Quarantined;
      if not Owner_Ready then Status := Ownership_Lost; return; end if;
      Read_Marker (Marker, OK);
      if not Owner_Ready then Status := Ownership_Lost; return; end if;
      if not OK then Status := Read_Failed; return; end if;
      if Marker /= Unsigned_64 (Object.Current_Sequence) then
         Status := Prior_Not_Complete; return;
      end if;
      Read_Tail (Saved_Tail, OK);
      if not Owner_Ready then Status := Ownership_Lost; return; end if;
      if not OK then Status := Read_Failed; return; end if;
      if Saved_Tail /= Object.Current_Tail then Status := Tail_Mismatch; return; end if;
      Plan := Intel_GPU_Ring_Reservation.Reserve
        (Object.Protected_Start, Object.Current_Tail, Bytes);
      if Plan.Status /= Intel_GPU_Ring_Reservation.Ready then
         Status := Full; return;
      end if;
      Next_Tail := Plan.Tail;
      if Plan.Padding /= 0 then
         for I in 0 .. Plan.Padding / 4 - 1 loop
            if not Owner_Ready then Status := Ownership_Lost; return; end if;
            Write_Word (Object.Current_Tail + I * 4, 0, OK);
            if not Owner_Ready then Status := Ownership_Lost; return; end if;
            if not OK then Status := Write_Failed; return; end if;
         end loop;
         OK := Publish_Words (Object.Current_Tail, Plan.Padding);
         if not Owner_Ready then Status := Ownership_Lost; return; end if;
         if not OK then Status := Visibility_Failed; return; end if;
      end if;
      for I in 0 .. Count - 1 loop
         if not Owner_Ready then Status := Ownership_Lost; return; end if;
         Write_Word (Plan.Start + Unsigned_32 (I) * 4,
                     Segment.Words (I), OK);
         if not Owner_Ready then Status := Ownership_Lost; return; end if;
         if not OK then Status := Write_Failed; return; end if;
      end loop;
      if not Owner_Ready then Status := Ownership_Lost; return; end if;
      OK := Publish_Words (Plan.Start, Bytes);
      if not Owner_Ready then Status := Ownership_Lost; return; end if;
      if not OK then Status := Visibility_Failed; return; end if;
      Write_Tail (Next_Tail, OK);
      if not Owner_Ready then Status := Ownership_Lost; return; end if;
      if not OK then Status := Write_Failed; return; end if;
      OK := Tail_Visible;
      if not Owner_Ready then Status := Ownership_Lost; return; end if;
      if not OK then Status := Visibility_Failed; return; end if;
      Object.Current_Tail := Next_Tail;
      Object.Protected_Start := Plan.Start;
      Object.Current_Sequence := Object.Current_Sequence + 1;
      Object.Value := Available;
      Status := Published;
   end Append_Words;
   procedure Append (Object : in out Channel;
                     Segment : Intel_GPU_ADLN_Context_Init.Segment;
                     Status : out Result) is
   begin
      Append_Words (Object, Segment, Segment.Words'Length, 90, Status);
   end Append;
   procedure Append (Object : in out Channel;
                     Segment : Intel_GPU_ADLN_Barrier.Segment;
                     Status : out Result) is
      Bounded : Intel_GPU_ADLN_Context_Init.Segment;
   begin
      Bounded.Valid := Segment.Valid;
      for I in Segment.Words'Range loop Bounded.Words (I) := Segment.Words (I); end loop;
      Append_Words (Object, Bounded, Segment.Words'Length, 26, Status);
   end Append;
end Intel_GPU_Live_Ring_Publish;
