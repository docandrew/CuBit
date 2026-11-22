package body Intel_GPU_MCR_Access is
   use Interfaces;
   Selector : constant Unsigned_32 := 16#FDC#;
   function Failed (Object : State) return Boolean is (Object.Faulted);
   procedure Access_Register
     (Object : in out State; Offset : Unsigned_32;
      Instance : Natural; Write : Boolean;
      Value : in out Unsigned_32; Success : out Boolean)
   is
      Saved, Selected, Raw, Result : Unsigned_32;
      OK, Target_OK : Boolean := False;
   begin
      Success := False;
      if Object.Faulted then return; end if;
      -- Engine/GT settings plus read-only WM_CHICKEN2 context input.
      if Offset not in 16#E18C# | 16#E4F4# | 16#E48C# | 16#9550# | 16#5584# or else
        (Offset = 16#5584# and Write) or else Instance > 5 then
         Object.Faulted := True; return;
      end if;
      Object.Faulted := True;
      if not Owner_Ready then return; end if;
      Saved := Read32 (Selector);
      if not Owner_Ready or else Saved = Unsigned_32'Last then return; end if;
      -- Group0, enabled DSS for reads; force multicast for writes AND reads.
      -- Preserve non-selector bits. Restore inherited steering afterward.
      Selected := (Saved and 16#00FFFFFF#) or 16#80000000# or
        Shift_Left (Unsigned_32 (Instance), 24);
      Write32 (Selector, Selected, OK);
      if not Owner_Ready then return; end if;
      if OK then
         Raw := Read32 (Selector);
         if not Owner_Ready then return; end if;
         OK := Raw = Selected;
      end if;
      Result := Value;
      if OK then
         if Write then Write32 (Offset, Value, Target_OK);
         else
            Result := Read32 (Offset);
            Target_OK := Result /= Unsigned_32'Last;
         end if;
      end if;
      -- A failed selector/target write may still have reached the device.
      -- Restore while ownership remains valid; otherwise quarantine without
      -- more MMIO. A successful restore never erases a target failure.
      if not Owner_Ready then return; end if;
      Write32 (Selector, Saved, OK);
      if not Owner_Ready or else not OK then return; end if;
      Raw := Read32 (Selector);
      if not Owner_Ready or else Raw /= Saved or else not Target_OK then return; end if;
      Object.Faulted := False;
      Value := Result;
      Success := True;
   end Access_Register;
end Intel_GPU_MCR_Access;
