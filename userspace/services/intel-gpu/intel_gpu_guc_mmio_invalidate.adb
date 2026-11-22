with Intel_GPU_TLB_Registers;
package body Intel_GPU_GuC_MMIO_Invalidate is
   procedure Execute (Object : in out Attempt; Status : out Result;
                      Poll_Limit : Positive := 4096) is
      Start, Previous, Now : Unsigned_64;
      Raw : Unsigned_32;
      OK : Boolean;
   begin
      Status := Rejected;
      if Object.Used then return; end if;
      Object.Used := True;
      if not Gate then return; end if;
      Clock_US (Start, OK);
      if not OK or else Start = Unsigned_64'Last then
         Status := Invalid_Clock; return;
      end if;
      if not Gate then Status := Ownership_Lost; return; end if;
      Write_Request (Intel_GPU_TLB_Registers.Encode_GuC
        ((Request => 1, Reserved => 0)), OK);
      if not OK then Status := Write_Failed; return; end if;
      Previous := Start;
      for Poll in 1 .. Poll_Limit loop
         if not Gate then Status := Ownership_Lost; return; end if;
         Clock_US (Now, OK);
         if not OK or else Now = Unsigned_64'Last or else Now < Previous then
            Status := Invalid_Clock; return;
         end if;
         if Now - Start >= 4000 then Status := Timed_Out; return; end if;
         Previous := Now;
         if not Gate then Status := Ownership_Lost; return; end if;
         Read_Status (Raw, OK);
         if not OK or else Raw = Unsigned_32'Last then
            Status := Read_Failed; return;
         end if;
         if not Gate then Status := Ownership_Lost; return; end if;
         if not Intel_GPU_TLB_Registers.GuC_Pending (Raw) then
            Status := Complete; return;
         end if;
      end loop;
      Status := Timed_Out;
   end Execute;
end Intel_GPU_GuC_MMIO_Invalidate;
