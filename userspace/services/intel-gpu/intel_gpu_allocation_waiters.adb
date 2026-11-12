with Ada.Unchecked_Deallocation;

package body Intel_GPU_Allocation_Waiters with SPARK_Mode => Off is
   package Ring renames Intel_GPU_Waiter_Ring;
   procedure Free is new Ada.Unchecked_Deallocation (Waiter_Array, Waiter_Storage);

   function Size (Q : Queue) return Natural is
     (if Q.Slots = null then 0 else Q.Slots'Length);

   -- Room for one more waiter: the first allocation, or a doubled copy of a
   -- full ring with its waiters moved over in order (head at slot 0).
   procedure Make_Room (Q : in out Queue; Result : out Wait_Result) is
      Bigger : Waiter_Storage;
      New_Size : Ring.Ring_Size;
   begin
      Result := Waiting;
      if Q.Count < Size (Q) then return; end if;
      if Q.Count >= Ceiling then Result := At_Ceiling; return; end if;
      New_Size := (if Q.Slots = null then Initial_Size else Ring.Grown (Size (Q), Ceiling));
      begin
         Bigger := new Waiter_Array (0 .. New_Size - 1);
      exception
         when Storage_Error => Result := No_Memory; return;
      end;
      for Offset in 0 .. Q.Count - 1 loop
         declare
            From : Waiter renames Q.Slots (Ring.Slot (Q.Head, Offset, Size (Q)));
         begin
            Bigger (Offset).Item := From.Item;
            Move (From.Owed, Bigger (Offset).Owed);
         end;
      end loop;
      Free (Q.Slots);
      Q.Slots := Bigger;
      Q.Head := 0;
   end Make_Room;

   procedure Note_Peak (Q : in out Queue) is
   begin
      if Q.Count > Q.Most then Q.Most := Q.Count; end if;
   end Note_Peak;

   procedure Append (Q : in out Queue; Item : Request; Owed : in out Reply;
                     Result : out Wait_Result) is
   begin
      Make_Room (Q, Result);
      if Result /= Waiting then return; end if;
      declare
         Tail : Waiter renames Q.Slots (Ring.Slot (Q.Head, Q.Count, Size (Q)));
      begin
         Tail.Item := Item;
         Move (Owed, Tail.Owed);
      end;
      Q.Count := Q.Count + 1;
      Note_Peak (Q);
   end Append;

   procedure Restore (Q : in out Queue; Item : Request; Owed : in out Reply;
                      Result : out Wait_Result) is
   begin
      Make_Room (Q, Result);
      if Result /= Waiting then return; end if;
      Q.Head := Ring.Before (Q.Head, Size (Q));
      Q.Slots (Q.Head).Item := Item;
      Move (Owed, Q.Slots (Q.Head).Owed);
      Q.Count := Q.Count + 1;
      Note_Peak (Q);
   end Restore;

   procedure Take (Q : in out Queue; Item : out Request; Owed : in out Reply;
                   Found : out Boolean) is
   begin
      Found := Q.Count > 0;
      if not Found then return; end if;
      Item := Q.Slots (Q.Head).Item;
      Move (Q.Slots (Q.Head).Owed, Owed);
      Q.Count := Q.Count - 1;
      Q.Head := (if Q.Count = 0 then 0 else Ring.Slot (Q.Head, 1, Size (Q)));
   end Take;
end Intel_GPU_Allocation_Waiters;
