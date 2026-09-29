with Intel_GPU_ADLN_MOCS;
package body Intel_GPU_MOCS_Configure is
   use Interfaces;
   package Plan renames Intel_GPU_ADLN_MOCS;
   function Last_Index (Object : Attempt) return Natural is (Object.Index);
   function Last_Raw (Object : Attempt) return Unsigned_32 is (Object.Raw);
   procedure Configure (Object : in out Attempt; Status : out Result) is
      OK : Boolean;
   begin
      Status := Rejected;
      if Object.Started then return; end if;
      Object.Started := True;
      for I in Plan.Register_Index loop
         Object.Index := I;
         if not Owner_Ready then Status := Ownership_Lost; return; end if;
         Object.Raw := Read32 (Plan.Offset (I));
         if not Owner_Ready then Status := Ownership_Lost; return; end if;
         if Object.Raw = Unsigned_32'Last then Status := Read_Failed; return; end if;
         if Object.Raw /= Plan.Value (I) then
            Write32 (Plan.Offset (I), Plan.Value (I), OK);
            if not Owner_Ready then Status := Ownership_Lost; return; end if;
            if not OK then Status := Write_Failed; return; end if;
         end if;
         Object.Raw := Read32 (Plan.Offset (I));
         if not Owner_Ready then Status := Ownership_Lost; return; end if;
         if Object.Raw /= Plan.Value (I) then Status := Readback_Failed; return; end if;
      end loop;
      Status := Ready;
   end Configure;
end Intel_GPU_MOCS_Configure;
