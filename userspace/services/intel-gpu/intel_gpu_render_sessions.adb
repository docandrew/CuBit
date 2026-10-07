package body Intel_GPU_Render_Sessions with SPARK_Mode is
   function Storage_Index (Object : Registry; Tag : Unsigned_64) return Slot_Index
     with Refined_Post =>
       (if Storage_Index'Result /= 0 then
          Tag > Tag_Base and then Tag <= Tag_Last and then
          Storage_Index'Result <= Object.Used and then
          Object.Items (Storage_Index'Result).Tag = Tag and then
          (for all I in 1 .. Storage_Index'Result - 1 =>
             Object.Items (I).Tag /= Tag)) and
       (if Tag > Tag_Base and Tag <= Tag_Last then
         (Storage_Index'Result = 0) =
           (for all I in 1 .. Object.Used => Object.Items (I).Tag /= Tag))
   is
   begin
      if Tag <= Tag_Base or else Tag > Tag_Last then return 0; end if;
      for I in 1 .. Object.Used loop
         if Object.Items (I).Tag = Tag then return I; end if;
         pragma Loop_Invariant
           (for all J in 1 .. I => Object.Items (J).Tag /= Tag);
      end loop;
      return 0;
   end Storage_Index;
   function Issued_Tag (Object : Registry; Index : Slot_Index) return Unsigned_64 is
     (if Index = 0 or else Index > Object.Used then 0
      elsif Storage_Index (Object, Object.Items (Index).Tag) /= Index then 0
      else Object.Items (Index).Tag);
   function Index_Of (Object : Registry; Sender, Tag : Unsigned_64) return Natural is
     (if Object.Failed or else Sender = 0 or else Storage_Index (Object, Tag) = 0 then 0
      elsif Object.Items (Storage_Index (Object, Tag)).Sender = Sender then
         Storage_Index (Object, Tag) else 0);
   pragma Annotate (GNATprove, Inline_For_Proof, Index_Of);
   procedure Reserve
     (Object : in out Registry; Sender : Unsigned_64; Tag : out Unsigned_64)
     with Refined_Post =>
       (Tag = 0 or else
         (Sender /= 0 and Tag > Tag_Base and Tag <= Tag_Last and
          Storage_Index (Object, Tag) /= 0)) and
       (if Tag = 0 then Object.Last_Issued = Object.Last_Issued'Old
        else Object.Last_Issued'Old < Tag_Last and
          Object.Last_Issued = Object.Last_Issued'Old + 1 and
          Tag = Object.Last_Issued)
   is
   begin
      Tag := 0;
      if Object.Failed or else Sender = 0 or else Object.Used = Capacity or else
        Object.Last_Issued = Tag_Last then return; end if;
      Object.Last_Issued := Object.Last_Issued + 1;
      Object.Used := Object.Used + 1;
      Object.Items (Object.Used) := (Sender, Object.Last_Issued, Reserved);
      Tag := Object.Last_Issued;
   end Reserve;
   procedure Finalize
     (Object : in out Registry; Sender, Tag : Unsigned_64;
      Granted : Boolean; Accepted : out Boolean) is
      Index : constant Natural := Index_Of (Object, Sender, Tag);
   begin
      Accepted := False;
      if Index = 0 or else Object.Items (Index).State /= Reserved then return; end if;
      Object.Items (Index).State := (if Granted then Active else Retired);
      Accepted := True;
   end Finalize;
   function Resolve
     (Object : Registry; Sender, Stamped_Tag : Unsigned_64) return Unsigned_64
     with Refined_Post => Resolve'Result =
       (if Index_Of (Object, Sender, Stamped_Tag) /= 0 and then
           Object.Items (Index_Of (Object, Sender, Stamped_Tag)).State = Active
        then Stamped_Tag else 0)
   is
      Index : constant Natural := Index_Of (Object, Sender, Stamped_Tag);
   begin
      return (if Index /= 0 and then Object.Items (Index).State = Active
              then Stamped_Tag else 0);
   end Resolve;
   procedure Close (Object : in out Registry; Sender, Tag : Unsigned_64)
     with Refined_Post =>
       Storage_Index (Object, Tag) = Storage_Index (Object, Tag)'Old and then
       Resolve (Object, Sender, Tag) = 0 and then
       Object.Used = Object.Used'Old and then
       Object.Last_Issued = Object.Last_Issued'Old and then
       Object.Failed = Object.Failed'Old and then
       (for all I in Object.Items'Range =>
          Object.Items (I).Sender = Object.Items'Old (I).Sender and
          Object.Items (I).Tag = Object.Items'Old (I).Tag)
   is
      Index : constant Natural := Index_Of (Object, Sender, Tag);
   begin
      if Index /= 0 then Object.Items (Index).State := Retired; end if;
   end Close;
   function Resolve_Retired
     (Object : Registry; Sender, Stamped_Tag : Unsigned_64) return Unsigned_64 is
      Index : constant Natural := Index_Of (Object, Sender, Stamped_Tag);
   begin
      return (if Index /= 0 and then Object.Items (Index).State = Retired
              then Stamped_Tag else 0);
   end Resolve_Retired;
   procedure Quarantine (Object : in out Registry)
     with Refined_Post => Object.Failed and then
       Object.Used = Object.Used'Old and then
       Object.Last_Issued = Object.Last_Issued'Old and then
       Object.Items = Object.Items'Old
   is
   begin Object.Failed := True; end Quarantine;
end Intel_GPU_Render_Sessions;
