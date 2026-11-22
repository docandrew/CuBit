package body Intel_GPU_GuC_RSA is
   use Interfaces;
   function Current (Object : Attempt) return Phase is (Object.Value);
   procedure Execute
     (Object : in out Attempt; Header : Intel_GPU_Firmware.CSS_Header;
      Blob_Bytes : Unsigned_64; Status : out Result)
   is
      Layout : Intel_GPU_Firmware.Layout;
      Words : array (Natural range 0 .. 63) of Unsigned_32 := [others => 0];
      Byte : Unsigned_8;
      OK : Boolean;
   begin
      Status := Rejected;
      if Object.Value /= Fresh then return; end if;
      Object.Value := Consumed;
      if not Intel_GPU_Firmware.Matches_Selected_ADLN_GuC (Header, Blob_Bytes)
      then return; end if;
      Layout := Intel_GPU_Firmware.Decode (Header, Blob_Bytes);
      -- Snapshot all bytes before the first write. Never leave partial device
      -- signature state merely because the source became unreadable halfway.
      for Index in Words'Range loop
         for Lane in 0 .. 3 loop
            Read_Byte (Layout.Signature_Offset + Unsigned_64 (Index * 4 + Lane),
                       Byte, OK);
            if not OK then Status := Source_Failed; return; end if;
            Words (Index) := Words (Index) or
              Shift_Left (Unsigned_32 (Byte), Lane * 8);
         end loop;
      end loop;
      Object.Value := Quarantined;
      for Index in Words'Range loop
         Write32 (16#C200# + Unsigned_32 (Index) * 4, Words (Index), OK);
         if not OK then Status := Write_Failed; return; end if;
      end loop;
      Object.Value := Supplied;
      Status := Complete;
   end Execute;
end Intel_GPU_GuC_RSA;
