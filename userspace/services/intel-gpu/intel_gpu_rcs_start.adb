package body Intel_GPU_RCS_Start is
   use Interfaces;
   function Rejection (Object : Attempt) return Rejection_Reason is (Object.Reason);
   procedure Start
     (Object : in out Attempt; Status_GPU : Unsigned_64;
      Status : out Result)
   is
      OK : Boolean;
      Raw : Unsigned_32;
      function Owned return Boolean is
        (Owner_Ready and then Status_Page_Ready (Status_GPU));
      type Words is array (Positive range 1 .. 4) of Unsigned_32;
      Offsets : constant Words := [16#2098#, 16#2080#, 16#229C#, 16#209C#];
      Values : Words;
   begin
      Status := Rejected;
      if Object.Started then Object.Reason := Already_Attempted; return; end if;
      Object.Started := True;
      if Status_GPU = 0 then Object.Reason := Zero_Address; return; end if;
      if Status_GPU mod 4096 /= 0 then Object.Reason := Unaligned_Address; return; end if;
      if Status_GPU > 16#FEE0_0000# - 4096 then
         Object.Reason := Outside_Runtime_Range; return;
      end if;
      -- Linux v6.16 intel_guc_submission.c setup_hwsp/start_engine:
      -- mask status writes, install GGTT HWSP, disable legacy mode, clear STOP.
      -- Upper sixteen bits are write-enable masks, not register state.
      Values := [Unsigned_32'Last, Unsigned_32 (Status_GPU),
                 16#0008_0008#, 16#0100_0000#];
      for I in Offsets'Range loop
         if not Owned then Status := Ownership_Lost; return; end if;
         Write32 (Offsets (I), Values (I), OK);
         if not Owned then Status := Ownership_Lost; return; end if;
         if not OK then Status := Write_Failed; return; end if;
      end loop;
      -- Posting read after clearing STOP_RING; never retry a partial start.
      Raw := Read32 (16#209C#);
      if not Owned then Status := Ownership_Lost; return; end if;
      if Raw = Unsigned_32'Last or else (Raw and 16#100#) /= 0 then
         Status := Readback_Failed; return;
      end if;
      Status := Ready;
   end Start;
end Intel_GPU_RCS_Start;
