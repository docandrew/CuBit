with Intel_GPU_GGTT_Retire;
package body Intel_GPU_GGTT_Reservations.Reclamation is
   package Retirer is new Intel_GPU_GGTT_Retire
     (Gate, Read_PTE, Write_PTE, Invalidate_And_Wait, Resolve_Page);

   procedure Execute
     (Object : in out Attempt; Book : in out Ledger;
      First, DMA_Base, Bytes, Scratch_DMA : Unsigned_64; Status : out Result)
   is
      Slot : Natural := 0;
      Transaction : Retirer.Attempt;
      Outcome : Retirer.Result;
   begin
      Status := Rejected;
      if Object.Used then return; end if;
      Object.Used := True;
      if not Has_Claim (Book, First, Bytes) then return; end if;
      for I in 1 .. Book.Used loop
         if Book.Claims (I).First = First and then
           Book.Claims (I).Limit - First = Bytes
         then Slot := I; exit; end if;
      end loop;
      if Slot = 0 then return; end if;
      Retirer.Execute
        (Transaction, Book, First, DMA_Base, Bytes, Scratch_DMA, Outcome);
      case Outcome is
         when Retirer.Rejected => return;
         when Retirer.Quarantined => Status := Quarantined; return;
         when Retirer.Detached => null;
      end case;
      -- Retirer has just checked Gate after completed invalidation. No call,
      -- yield, or publication occurs between that check and ledger mutation.
      -- Claims have no externally visible slot identities; swap-delete keeps
      -- all other exact extents and cannot enlarge or merge any of them.
      Forget_Detached (Book, Slot);
      Status := Released;
   end Execute;
end Intel_GPU_GGTT_Reservations.Reclamation;
