package body Intel_GPU_GuC_CT_Receive is
   use Interfaces;
   function State (Object : Channel) return Phase is (Object.Value);
   procedure Initialize
     (Object : in out Channel; Registered : Boolean;
      Size : Intel_GPU_GuC_CTB.Ring_Size; Initial_Head : Unsigned_32) is
   begin
      if Object.Value /= Uninitialized then return; end if;
      Object.Value := Broken;
      if not Registered or else Initial_Head >= Size then return; end if;
      Object.Size := Size; Object.Head := Initial_Head; Object.Value := Active;
   end Initialize;
   procedure Poll
     (Object : in out Channel; Output : out Message; Status : out Result)
   is
      Head, Tail, Descriptor_Status, Header, Cursor : Unsigned_32;
      OK : Boolean;
      Plan : Intel_GPU_GuC_CTB.Plan;
      Local : Message;
      use type Intel_GPU_GuC_CTB.Result;
   begin
      Output := (others => <>); Status := Rejected;
      if Object.Value /= Active then return; end if;
      Read_Descriptor (Head, Tail, Descriptor_Status, OK);
      if not OK or else Descriptor_Status /= 0 or else Head >= Object.Size
        or else Tail >= Object.Size or else Head /= Object.Head
      then
         Object.Value := Broken; Status := Corrupt; return;
      end if;
      if Head = Tail then Status := Empty; return; end if;
      -- Any subsequent failure disables this channel. Do not expose a
      -- partial frame or replay a frame whose head publication was uncertain.
      Object.Value := Broken; Status := Corrupt;
      Read_Word (Head, Header, OK);
      if not OK then return; end if;
      Plan := Intel_GPU_GuC_CTB.Receive
        (Object.Size, Head, Tail, Object.Head, Descriptor_Status, Header);
      if Plan.State /= Intel_GPU_GuC_CTB.Ready then return; end if;
      Local.Length := Natural (Plan.Words - 1); Local.Fence := Plan.Fence;
      Cursor := (Head + 1) mod Object.Size;
      for Index in 1 .. Local.Length loop
         Read_Word (Cursor, Local.Payload (Index), OK);
         if not OK then return; end if;
         Cursor := (Cursor + 1) mod Object.Size;
      end loop;
      Status := Quarantined;
      Finish_Reads (OK);
      if not OK then return; end if;
      Write_Head (Plan.Next_Cursor, OK);
      if not OK then return; end if;
      Make_Visible (OK);
      if not OK then return; end if;
      Object.Head := Plan.Next_Cursor; Object.Value := Active;
      Output := Local; Status := Received;
   end Poll;
end Intel_GPU_GuC_CT_Receive;
