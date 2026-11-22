package body BuddyAllocator is
   use type System.Address;
   type Block is array (1 .. 4096) of Storage_Element with Alignment => 4096;
   Blocks : array (1 .. 3) of aliased Block;
   Busy : array (Blocks'Range) of Boolean := [others => False];
   function getOrder (Bytes : Storage_Count) return Order is
   begin
      for O in Order loop
         if Bytes <= blockSize (O) then return O; end if;
      end loop;
      raise Program_Error;
   end getOrder;
   procedure alloc (O : Order; Addr : out System.Address) is
   begin
      pragma Assert (O = 0); -- Test pools use one-page backing blocks.
      Attempts := Attempts + 1;
      Addr := System.Null_Address;
      if not Allow_Allocation then return; end if;
      for I in Blocks'Range loop
         if not Busy (I) then
            Busy (I) := True;
            Live_Blocks := Live_Blocks + 1;
            Addr := Blocks (I)'Address;
            return;
         end if;
      end loop;
   end alloc;
   procedure free (O : Order; Addr : System.Address) is
   begin
      pragma Assert (O = 0);
      for I in Blocks'Range loop
         if Addr = Blocks (I)'Address then
            pragma Assert (Busy (I));
            Busy (I) := False;
            Live_Blocks := Live_Blocks - 1;
            return;
         end if;
      end loop;
      raise Program_Error with "free of unknown fixture block";
   end free;
end BuddyAllocator;
