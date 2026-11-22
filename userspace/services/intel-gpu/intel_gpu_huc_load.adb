with Intel_GPU_HuC_Firmware;
package body Intel_GPU_HuC_Load with SPARK_Mode is
   package FW renames Intel_GPU_HuC_Firmware;
   Clock_Invalid : constant Unsigned_64 := Unsigned_64'Last;
   Low_Word : constant Unsigned_64 := 2 ** 32;

   procedure Upload
     (Object : in out Loader; Header : Intel_GPU_Firmware.CSS_Header;
      Blob_Bytes, Source_GGTT, Deadline : Unsigned_64; Poll_Limit : Positive;
      Status : out Upload_Result)
   is
      Bytes : constant FW.Upload_Bytes := FW.Selected_Upload_Bytes;
      Raw : Register_Word;
      OK, Done : Boolean;
      First, Previous, Stamp : Unsigned_64;
   begin
      Status := Rejected;
      if Object.State /= Idle then return; end if;
      if not FW.Matches_Selected_ADLN_HuC (Header, Blob_Bytes) or else
        not FW.Source_Admissible (Source_GGTT, Bytes) or else
        Blob_Bytes > Low_Word - Source_GGTT
      then return; end if;

      Read32 (GuC_WOPCM_Offset, Raw, OK);
      Status := Invalid_MMIO;
      if not OK or else Raw = Unreadable then return; end if;
      Status := WOPCM_Not_Ready;
      if not FW.WOPCM_Admits_HuC (Raw, Bytes) then return; end if;
      Read32 (DMA_Control, Raw, OK);
      Status := Invalid_MMIO;
      if not OK or else Raw = Unreadable then return; end if;
      Status := Busy;
      if (Raw and Start_DMA) /= 0 then return; end if;
      First := Now;
      Status := Clock_Unavailable;
      if First = Clock_Invalid then return; end if;
      Status := Deadline_Passed;
      if First >= Deadline then return; end if;

      -- Latch before the first write: the outcome may have posted.
      Object.State := Failed;
      Status := Write_Failed;
      Write32 (DMA_Address_0_Low, Register_Word (Source_GGTT), OK);
      if not OK then return; end if;
      Write32 (DMA_Address_0_High, 0, OK);
      if not OK then return; end if;
      Write32 (DMA_Address_1_Low, HuC_Destination, OK);
      if not OK then return; end if;
      Write32 (DMA_Address_1_High, DMA_Address_Space_WOPCM, OK);
      if not OK then return; end if;
      Write32 (DMA_Copy_Size, Register_Word (Bytes), OK);
      if not OK then return; end if;
      Write32 (DMA_Control, HuC_DMA_Start, OK);
      if not OK then return; end if;

      Previous := First;
      Done := False;
      Status := Timed_Out;
      for Poll in 1 .. Poll_Limit loop
         pragma Loop_Invariant (Status = Timed_Out and not Done);
         Read32 (DMA_Control, Raw, OK);
         if not OK or else Raw = Unreadable then
            Status := Transfer_Failed; exit;
         end if;
         Stamp := Now;
         if Stamp = Clock_Invalid or else Stamp < Previous then
            Status := Clock_Failed; exit;
         end if;
         Previous := Stamp;
         if (Raw and Start_DMA) = 0 then Done := True; exit; end if;
         exit when Stamp >= Deadline;
         if Poll < Poll_Limit then Pause; end if;
      end loop;

      -- Always clear HUC_UKERNEL, as i915 does; not a DMA cancellation.
      Write32 (DMA_Control, HuC_DMA_Clear, OK);
      if OK then Read32 (DMA_Control, Raw, OK); end if;
      if not OK or else Raw = Unreadable or else (Raw and HuC_UKernel) /= 0
        or else (Done and then (Raw and Start_DMA) /= 0)
      then
         Status := Cleanup_Failed;
         return;
      end if;
      if Done then
         Status := Transferred;
         Object.State := Transferred;
      end if;
   end Upload;

   procedure Begin_Authentication
     (Object : in out Loader; RSA_GGTT, Pin_Bias, Deadline : Unsigned_64;
      Status : out Auth_Result)
   is
      Raw : Register_Word;
      OK : Boolean;
      First : Unsigned_64;
      Reply : Auth_Reply;
   begin
      Status := Rejected;
      if Object.State /= Transferred or else
        not FW.RSA_Page_Admissible (RSA_GGTT, Pin_Bias)
      then return; end if;
      First := Now;
      Status := Clock_Unavailable;
      if First = Clock_Invalid then return; end if;
      Status := Deadline_Passed;
      if First >= Deadline then return; end if;
      Read32 (HuC_Kernel_Load_Info, Raw, OK);
      Status := Invalid_MMIO;
      if not OK or else Raw = Unreadable then return; end if;
      -- A set bit before we asked cannot be attributed to this upload.
      if Intel_GPU_HuC_Registers.Authenticated (Raw) then
         Object.State := Failed;
         Status := Stale_Status;
         return;
      end if;
      -- RSA_Page_Admissible bounds RSA_GGTT below GUC_GGTT_TOP < 2**32.
      Request_Authentication (Register_Word (RSA_GGTT), Reply);
      case Reply is
         when Accepted =>
            Object.State := Authenticating;
            Object.Deadline := Deadline;
            Object.Last_Stamp := First;
            Status := Pending;
         when Refused =>
            Object.State := Failed;
            Status := Signature_Refused;
         when Transport_Failed =>
            Object.State := Failed;
            Status := Transport_Failed;
      end case;
   end Begin_Authentication;

   procedure Poll_Authentication (Object : in out Loader; Status : out Auth_Result) is
      Raw : Register_Word;
      OK : Boolean;
      Stamp : Unsigned_64;
   begin
      Status := Rejected;
      if Object.State /= Authenticating then return; end if;
      Read32 (HuC_Kernel_Load_Info, Raw, OK);
      if not OK or else Raw = Unreadable then
         Object.State := Failed;
         Status := Status_Failed;
         return;
      end if;
      Stamp := Now;
      if Stamp = Clock_Invalid or else Stamp < Object.Last_Stamp then
         Object.State := Failed;
         Status := Clock_Failed;
         return;
      end if;
      Object.Last_Stamp := Stamp;
      if Intel_GPU_HuC_Registers.Authenticated (Raw) then
         Object.State := Authenticated;
         Status := Authenticated;
      elsif Stamp >= Object.Deadline then
         Object.State := Failed;
         Status := Timed_Out;
      else
         Status := Pending;
      end if;
   end Poll_Authentication;

   procedure Authenticate
     (Object : in out Loader; RSA_GGTT, Pin_Bias, Deadline : Unsigned_64;
      Poll_Limit : Positive; Status : out Auth_Result)
   is
      Entry_Epoch : constant Load_Generation := Object.Epoch;
   begin
      Begin_Authentication (Object, RSA_GGTT, Pin_Bias, Deadline, Status);
      if Status /= Pending then return; end if;
      for Poll in 1 .. Poll_Limit loop
         pragma Loop_Invariant
           (Status = Pending and Object.State = Authenticating and
            Object.Epoch = Entry_Epoch);
         Poll_Authentication (Object, Status);
         exit when Status /= Pending;
         if Poll < Poll_Limit then Pause; end if;
      end loop;
      if Status = Pending then
         Object.State := Failed;
         Status := Timed_Out;
      end if;
   end Authenticate;

   procedure Check (Object : in out Loader; Status : out Check_Result) is
      Raw : Register_Word;
      OK : Boolean;
   begin
      Status := Not_Authenticated;
      if Object.State /= Authenticated then return; end if;
      Read32 (HuC_Kernel_Load_Info, Raw, OK);
      if OK and then Intel_GPU_HuC_Registers.Authenticated (Raw) then
         Status := Still_Authenticated;
      else
         Object.State := Failed;
         Status := Lost;
      end if;
   end Check;

   procedure Reset (Object : in out Loader) is
   begin
      Object.State := Idle;
      Object.Epoch := Object.Epoch + 1;
      Object.Deadline := 0;
      Object.Last_Stamp := 0;
   end Reset;
end Intel_GPU_HuC_Load;
