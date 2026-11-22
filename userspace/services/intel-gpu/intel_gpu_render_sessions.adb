package body Intel_GPU_Render_Sessions with SPARK_Mode is
   -- Private helpers take no invariant for granted: they run in the middle
   -- of operations. Public functions are expression functions of them.
   function Live (Object : Registry; Index : Slot_Index) return Boolean is
     (Index in 1 .. Object.Grown and then Object.Items (Index).State /= Free);

   function Find (Object : Registry; Tag : Unsigned_64) return Slot_Index is
     (if Tag_Slot (Tag) /= 0 and then Live (Object, Tag_Slot (Tag)) and then
         Object.Items (Tag_Slot (Tag)).Tag = Tag
      then Tag_Slot (Tag) else 0);

   function Storage_Index (Object : Registry; Tag : Unsigned_64) return Slot_Index is
     (Find (Object, Tag));

   function Issued_Tag (Object : Registry; Index : Slot_Index) return Unsigned_64 is
     (if Live (Object, Index) then Object.Items (Index).Tag else 0);

   function Sender_Of (Object : Registry; Index : Slot_Index) return Unsigned_64 is
     (if Live (Object, Index) then Object.Items (Index).Sender else 0);

   function Was_Issued (Object : Registry; Tag : Unsigned_64) return Boolean is
     (Tag_Slot (Tag) /= 0 and then Issue_Of (Tag) in 1 .. Object.Issued);

   function Retired (Object : Registry; Index : Slot_Index) return Boolean is
     (Live (Object, Index) and then Object.Items (Index).State = Closed);

   function Holds (Object : Registry; Index : Slot_Index; What : Holding) return Boolean is
     (Live (Object, Index) and then Object.Items (Index).Held (What));

   function Model (Object : Registry) return View is
     ([for I in 1 .. Object.Grown =>
         (if Object.Items (I).State = Free then (others => <>)
          else (Tag => Object.Items (I).Tag,
                Retired => Object.Items (I).State = Closed,
                Held => Object.Items (I).Held))]);

   function Index_Of (Object : Registry; Sender, Tag : Unsigned_64) return Slot_Index is
     (if Object.Failed or else Sender = 0 or else Find (Object, Tag) = 0 then 0
      elsif Object.Items (Find (Object, Tag)).Sender = Sender then Find (Object, Tag)
      else 0);

   -- The lowest free grown slot, zero when every grown slot is in use.
   function First_Free (Object : Registry) return Slot_Index
     with Post =>
       (if First_Free'Result = 0 then
          (for all I in 1 .. Object.Grown => Object.Items (I).State /= Free)
        else First_Free'Result <= Object.Grown and then
          Object.Items (First_Free'Result).State = Free)
   is
   begin
      for Index in 1 .. Object.Grown loop
         if Object.Items (Index).State = Free then return Index; end if;
         pragma Loop_Invariant
           (for all J in 1 .. Index => Object.Items (J).State /= Free);
      end loop;
      return 0;
   end First_Free;

   procedure Reserve
     (Object : in out Registry; Sender : Unsigned_64; Tag : out Unsigned_64) is
      Index : Slot_Index;
   begin
      Tag := 0;
      if Object.Failed or else Sender = 0 or else Object.Issued = Issue_Limit then
         return;
      end if;
      Index := First_Free (Object);
      if Index = 0 then return; end if;
      pragma Assert
        (for all I in 1 .. Object.Grown =>
           Entry_Valid (Object.Items (I), I, Object.Issued + 1));
      Object.Issued := Object.Issued + 1;
      -- Index < Slot_Span: the slot is the tag's low half, the issue its
      -- high half, and the tag stays within Tag_Last.
      pragma Assert (Object.Issued * Slot_Span <= Issue_Limit * Slot_Span);
      pragma Assert (Issue_Limit * Slot_Span + (Slot_Span - 1) <= Tag_Last - Tag_Base);
      Tag := Tag_Base + Object.Issued * Slot_Span + Unsigned_64 (Index);
      pragma Assert (Tag - Tag_Base = Object.Issued * Slot_Span + Unsigned_64 (Index));
      pragma Assert (Tag > Tag_Base and Tag <= Tag_Last);
      pragma Assert ((Tag - Tag_Base) mod Slot_Span = Unsigned_64 (Index));
      pragma Assert ((Tag - Tag_Base) / Slot_Span = Object.Issued);
      pragma Assert (Tag_Slot (Tag) = Index);
      Object.Items (Index) :=
        (Sender => Sender, Tag => Tag, State => Reserved, Held => No_Holdings);
      pragma Assert (Entry_Valid (Object.Items (Index), Index, Object.Issued));
   end Reserve;

   procedure Finalize
     (Object : in out Registry; Sender, Tag : Unsigned_64;
      Granted : Boolean; Accepted : out Boolean) is
      Index : constant Slot_Index := Index_Of (Object, Sender, Tag);
   begin
      Accepted := False;
      if Index = 0 or else Object.Items (Index).State /= Reserved then return; end if;
      Object.Items (Index).State := (if Granted then Active else Closed);
      Accepted := True;
   end Finalize;

   function Resolve
     (Object : Registry; Sender, Stamped_Tag : Unsigned_64) return Unsigned_64 is
     (if Index_Of (Object, Sender, Stamped_Tag) /= 0 and then
         Object.Items (Index_Of (Object, Sender, Stamped_Tag)).State = Active
      then Stamped_Tag else 0);

   procedure Close (Object : in out Registry; Sender, Tag : Unsigned_64) is
      Index : constant Slot_Index := Index_Of (Object, Sender, Tag);
   begin
      if Index /= 0 then Object.Items (Index).State := Closed; end if;
   end Close;

   function Resolve_Retired
     (Object : Registry; Sender, Stamped_Tag : Unsigned_64) return Unsigned_64 is
     (if Index_Of (Object, Sender, Stamped_Tag) /= 0 and then
         Object.Items (Index_Of (Object, Sender, Stamped_Tag)).State = Closed
      then Stamped_Tag else 0);

   procedure Quarantine (Object : in out Registry) is
   begin
      Object.Failed := True;
   end Quarantine;

   procedure Grow (Object : in out Registry; To : Slot_Count) is
      Old : constant Slot_Count := Object.Grown;
   begin
      for Index in Old + 1 .. To loop
         Object.Items (Index) := (others => <>);
         pragma Loop_Invariant
           (for all J in Old + 1 .. Index =>
              Entry_Valid (Object.Items (J), J, Object.Issued) and
              Object.Items (J).State = Free);
         pragma Loop_Invariant
           (for all J in 1 .. Old => Object.Items (J) = Object.Items'Loop_Entry (J));
         pragma Loop_Invariant (Object.Grown = Old and Object.Issued = Object.Issued'Loop_Entry);
      end loop;
      Object.Grown := To;
   end Grow;

   procedure Hold (Object : in out Registry; Index : Slot_Index; What : Holding) is
   begin
      if Live (Object, Index) then Object.Items (Index).Held (What) := True; end if;
   end Hold;

   procedure Release_Image (Object : in out Registry; Index : Slot_Index) is
   begin
      if Live (Object, Index) then
         Object.Items (Index).Held (Context_Image) := False;
      end if;
   end Release_Image;

   procedure Acknowledge_Deregistration (Object : in out Registry; Index : Slot_Index) is
   begin
      if Live (Object, Index) then
         Object.Items (Index).Held (GuC_Context) := False;
      end if;
   end Acknowledge_Deregistration;

   procedure Recycle
     (Object : in out Registry; Index : Slot_Index; Facts : Release_Facts;
      Accepted : out Boolean) is
   begin
      Accepted := False;
      if Object.Failed or else not All_Released (Facts) or else
        not Live (Object, Index) or else Object.Items (Index).State /= Closed or else
        Object.Items (Index).Held /= No_Holdings
      then return; end if;
      Object.Items (Index) := (others => <>);
      Accepted := True;
   end Recycle;
end Intel_GPU_Render_Sessions;
