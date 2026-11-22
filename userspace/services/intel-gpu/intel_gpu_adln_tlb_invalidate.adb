with Intel_GPU_TLB_Registers;
package body Intel_GPU_ADLN_TLB_Invalidate is
   package Registers renames Intel_GPU_TLB_Registers;
   procedure Execute (Object : in out Attempt; Status : out Result;
                      Poll_Limit : Positive := 4096) is
      Started, Previous, Now : Unsigned_64;
      GFX, OA : Unsigned_32;
      OK : Boolean;
      Request : constant Unsigned_32 := Registers.Encode ((Request => 1, Reserved => 0));
      procedure Check_Time (Valid : out Boolean) is
      begin
         Valid := False;
         if not Gate then Status := Ownership_Lost; return; end if;
         Clock_US (Now, OK);
         if not Gate then Status := Ownership_Lost; return; end if;
         if not OK or else Now < Previous then Status := Invalid_Clock; return; end if;
         Previous := Now;
         if Now - Started >= 4000 then Status := Timed_Out; return; end if;
         Valid := True;
      end Check_Time;
      Valid : Boolean;
      type Offsets is array (Positive range <>) of Unsigned_32;
   begin
      Status := Rejected;
      if Object.Used then return; end if;
      Object.Used := True;
      if not Gate then Status := Ownership_Lost; return; end if;
      Clock_US (Started, OK);
      if not Gate then Status := Ownership_Lost; return; end if;
      if not OK then Status := Invalid_Clock; return; end if;
      Previous := Started;
      for Offset of Offsets'(Registers.GFX_Offset, Registers.OA_Offset) loop
         Check_Time (Valid); if not Valid then return; end if;
         Write_Register (Offset, Request, OK);
         if not Gate then Status := Ownership_Lost; return; end if;
         if not OK then Status := Write_Failed; return; end if;
      end loop;
      for Poll in 1 .. Poll_Limit loop
         Check_Time (Valid); if not Valid then return; end if;
         Read_Register (Registers.GFX_Offset, GFX, OK);
         if not Gate then Status := Ownership_Lost; return; end if;
         if not OK then Status := Read_Failed; return; end if;
         Check_Time (Valid); if not Valid then return; end if;
         Read_Register (Registers.OA_Offset, OA, OK);
         if not Gate then Status := Ownership_Lost; return; end if;
         if not OK then Status := Read_Failed; return; end if;
         Check_Time (Valid); if not Valid then return; end if;
         if not Registers.Pending (GFX) and then not Registers.Pending (OA) then
            Status := Complete; return;
         end if;
      end loop;
      Status := Timed_Out;
   end Execute;
end Intel_GPU_ADLN_TLB_Invalidate;
