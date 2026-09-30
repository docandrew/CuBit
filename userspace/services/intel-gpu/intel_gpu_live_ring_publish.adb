package body Intel_GPU_Live_Ring_Publish with SPARK_Mode is
   function State (Object : Channel) return Phase is (Object.Value);
   function Tail (Object : Channel) return Unsigned_32 is (Object.Current_Tail);
   function Sequence (Object : Channel) return Unsigned_32 is (Object.Current_Sequence);
   procedure Fail (Object : in out Channel) is
   begin Object.Value := Quarantined; end Fail;
   procedure Append (Object : in out Channel;
                     Segment : Intel_GPU_ADLN_Context_Init.Segment;
                     Status : out Result) is
      Marker : Unsigned_64;
      Saved_Tail : Unsigned_32;
      OK : Boolean;
      Next_Tail : Unsigned_32;
   begin
      Status := Rejected;
      if Object.Value /= Available or else not Segment.Valid then return; end if;
      if Object.Current_Sequence = Unsigned_32'Last or else
        Segment.Words (90) /= Object.Current_Sequence + 1 or else
        Segment.Words (91) /= 0
      then return; end if;
      if Object.Current_Tail > Ring_Bytes - Guard_Bytes - Segment_Bytes then
         Status := Full; return;
      end if;
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
      Next_Tail := Object.Current_Tail + Segment_Bytes;
      for I in Segment.Words'Range loop
         if not Owner_Ready then Status := Ownership_Lost; return; end if;
         Write_Word (Object.Current_Tail + Unsigned_32 (I) * 4,
                     Segment.Words (I), OK);
         if not Owner_Ready then Status := Ownership_Lost; return; end if;
         if not OK then Status := Write_Failed; return; end if;
      end loop;
      if not Owner_Ready then Status := Ownership_Lost; return; end if;
      OK := Publish_Words (Object.Current_Tail, Segment_Bytes);
      if not Owner_Ready then Status := Ownership_Lost; return; end if;
      if not OK then Status := Visibility_Failed; return; end if;
      Write_Tail (Next_Tail, OK);
      if not Owner_Ready then Status := Ownership_Lost; return; end if;
      if not OK then Status := Write_Failed; return; end if;
      OK := Tail_Visible;
      if not Owner_Ready then Status := Ownership_Lost; return; end if;
      if not OK then Status := Visibility_Failed; return; end if;
      Object.Current_Tail := Next_Tail;
      Object.Current_Sequence := Object.Current_Sequence + 1;
      Object.Value := Available;
      Status := Published;
   end Append;
end Intel_GPU_Live_Ring_Publish;
