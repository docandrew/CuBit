with Intel_GPU_WOPCM;
with Intel_GPU_GuC_RSA;
with Intel_GPU_GuC_DMA;
with Intel_GPU_GuC_Wait;
package body Intel_GPU_GuC_Upload is
   use Interfaces;
   Attempted, Write_Error : Boolean := False;
   Saved_Raw : Unsigned_32 := Unsigned_32'Last;
   Saved_State : Intel_GPU_GuC_Status.State := Intel_GPU_GuC_Status.Invalid_MMIO;
   function Last_Startup_Raw return Unsigned_32 is (Saved_Raw);
   function Last_Startup_State return Intel_GPU_GuC_Status.State is (Saved_State);
   function Read_Status return Unsigned_32 is (Read32 (16#C000#));
   package Boot is new Intel_GPU_GuC_Wait (Read_Status, Now, Pause);
   function DMA_Read (Offset : Unsigned_32) return Unsigned_32 is
   begin
      if Write_Error then return Unsigned_32'Last; end if;
      return Read32 (Offset);
   end DMA_Read;
   procedure DMA_Write (Offset, Value : Unsigned_32) is
      OK : Boolean;
   begin
      if Write_Error then return; end if;
      Write32 (Offset, Value, OK);
      if not OK then Write_Error := True; end if;
   end DMA_Write;
   package W is new Intel_GPU_WOPCM (Read32, Write32);
   package R is new Intel_GPU_GuC_RSA (Read_Byte, Write32);
   package D is new Intel_GPU_GuC_DMA (DMA_Read, DMA_Write, Now, Pause);
   DMA_Attempted : Boolean := False;
   Saved_DMA : D.Result := D.Rejected;
   function Last_Transfer_Detail return String is
   begin
      if not DMA_Attempted then return "not-attempted"; end if;
      if Write_Error then return "write-failed; backing-retained"; end if;
      return (case Saved_DMA is
         when D.Rejected => "rejected",
         when D.Busy => "busy",
         when D.Invalid_MMIO => "invalid-mmio",
         when D.Invalid_Clock => "invalid-clock",
         when D.Timed_Out => "timed-out; backing-retained",
         when D.Cleanup_Failed => "cleanup-failed; backing-retained",
         when D.Complete => "complete");
   end Last_Transfer_Detail;
   W_Attempt : W.Attempt;
   R_Attempt : R.Attempt;
   D_Attempt : D.Attempt;
   procedure Execute (Header : Intel_GPU_Firmware.CSS_Header;
     Parameters : Intel_GPU_GuC_Parameters.Parameter_Block;
     Blob_Bytes, GPU_Start, Capacity, Base, Size : Unsigned_64;
     Poll_Limit : Positive; Status : out Result)
   is
      Layout : Intel_GPU_Firmware.Layout;
      WS : W.Result;
      RS : R.Result;
      DS : D.Result;
      BS : Boot.Result;
      OK : Boolean;
      Value : Unsigned_32;
      Parameter_Words : constant Intel_GPU_GuC_Parameters.Startup_Words :=
        Intel_GPU_GuC_Parameters.Words (Parameters);
      use type W.Result;
      use type R.Result;
      use type D.Result;
      use type Boot.Result;
   begin
      Status := Rejected;
      if Attempted then return; end if;
      Attempted := True;
      if not Intel_GPU_GuC_Parameters.Valid (Parameters) then return; end if;
      if not Intel_GPU_Firmware.Matches_Selected_ADLN_GuC (Header, Blob_Bytes)
      then return; end if;
      Layout := Intel_GPU_Firmware.Decode (Header, Blob_Bytes);
      if GPU_Start >= 2 ** 32 or else GPU_Start mod 4096 /= 0 or else
        Blob_Bytes > 2 ** 32 - GPU_Start or else
        not Intel_GPU_Firmware.Fits_ADLN_WOPCM
          (Capacity, Base, Size, Layout.Signature_Offset, 0) then return; end if;
      -- Check timing before irreversible configuration, as well as in DMA.
      if Now = Unsigned_64'Last then return; end if;
      -- SOFT_SCRATCH(0) is the startup mailbox; parameters occupy 1..14.
      -- Do not confuse these with the Gen11 runtime communication registers.
      -- Program every word, including reserved zeroes supplied by the caller,
      -- so no startup setting is inherited from a previous firmware instance.
      -- Failed writes may have posted: retain resources and never retry.
      Status := Parameters_Failed;
      Write32 (16#C180#, 0, OK);
      if not OK then return; end if;
      for Index in Parameter_Words'Range loop
         Write32 (16#C184# + Unsigned_32 (Index) * 4, Parameter_Words (Index), OK);
         if not OK then return; end if;
      end loop;
      W.Configure (W_Attempt, Capacity, Base, Size, Layout.Signature_Offset, WS);
      Status := WOPCM_Failed;
      if WS /= W.Complete then return; end if;
      -- ADL-N (graphics IP12.0) transfer preparation. Not the Gen9 RC6
      -- workaround or IP12.50+ debug-mirroring sequence. Readback verifies
      -- the requested fields before any signature or DMA write.
      Status := Preparation_Failed;
      Write32 (16#C064#, 16#8607#, OK);
      if not OK then return; end if;
      Value := Read32 (16#C064#);
      if Value = Unsigned_32'Last or else (Value and 16#8607#) /= 16#8607#
      then return; end if;
      Write32 (16#13816C#, 1, OK);
      if not OK then return; end if;
      Value := Read32 (16#13816C#);
      if Value = Unsigned_32'Last or else (Value and 1) = 0 then return; end if;
      R.Execute (R_Attempt, Header, Blob_Bytes, RS);
      Status := Signature_Failed;
      if RS /= R.Complete then return; end if;
      DMA_Attempted := True;
      D.Execute (D_Attempt, GPU_Start, Layout.Signature_Offset, Size, Poll_Limit, DS);
      Saved_DMA := DS;
      Status := Transfer_Failed;
      if DS /= D.Complete or Write_Error then return; end if;
      Boot.Execute (Poll_Limit, BS, Saved_Raw, Saved_State);
      Status := Startup_Failed;
      if BS /= Boot.Firmware_Ready then return; end if;
      Status := Firmware_Ready;
   end Execute;
end Intel_GPU_GuC_Upload;
