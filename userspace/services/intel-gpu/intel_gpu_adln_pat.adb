package body Intel_GPU_ADLN_PAT is
   use Interfaces;
   Values : constant array (Natural range 0 .. 7) of Unsigned_32 :=
     [3, 1, 2, 0, 3, 3, 3, 3];
   function Last_Index (Object : Attempt) return Natural is (Object.Index);
   function Last_Raw (Object : Attempt) return Unsigned_32 is (Object.Raw);
   procedure Configure (Object : in out Attempt; Status : out Result) is
      OK : Boolean;
      Offset : Unsigned_32;
   begin
      Status := Rejected;
      if Object.Started or else not Owner_Ready then return; end if;
      Object.Started := True;
      for I in Values'Range loop
         Object.Index := I;
         Offset := 16#4800# + Unsigned_32 (I) * 4;
         if not Owner_Ready then Status := Ownership_Lost; return; end if;
         Object.Raw := Read32 (Offset);
         if not Owner_Ready then Status := Ownership_Lost; return; end if;
         if Object.Raw = Unsigned_32'Last then Status := Read_Failed; return; end if;
         if Object.Raw /= Values (I) then
            Write32 (Offset, Values (I), OK);
            if not Owner_Ready then Status := Ownership_Lost; return; end if;
            if not OK then Status := Write_Failed; return; end if;
         end if;
         -- Posting read even if the initial value already matched.
         Object.Raw := Read32 (Offset);
         if not Owner_Ready then Status := Ownership_Lost; return; end if;
         if Object.Raw /= Values (I) then Status := Readback_Failed; return; end if;
      end loop;
      Status := Ready;
   end Configure;
end Intel_GPU_ADLN_PAT;
