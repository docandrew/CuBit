with Intel_GPU_PAT_Registers;
package body Intel_GPU_ADLN_PAT is
   use Interfaces;
   use Intel_GPU_PAT_Registers;
   Values : constant array (Natural range 0 .. 7) of Memory_Type :=
     [Write_Back, Write_Combining, Write_Through, Uncacheable,
      Write_Back, Write_Back, Write_Back, Write_Back];
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
         if not Matches (Object.Raw, Values (I)) then
            Write32 (Offset, Encode (PAT_Register'(Cache => Values (I), others => <>)), OK);
            if not Owner_Ready then Status := Ownership_Lost; return; end if;
            if not OK then Status := Write_Failed; return; end if;
         end if;
         -- Posting read even if the initial value already matched.
         Object.Raw := Read32 (Offset);
         if not Owner_Ready then Status := Ownership_Lost; return; end if;
         if not Matches (Object.Raw, Values (I)) then Status := Readback_Failed; return; end if;
      end loop;
      Status := Ready;
   end Configure;
end Intel_GPU_ADLN_PAT;
