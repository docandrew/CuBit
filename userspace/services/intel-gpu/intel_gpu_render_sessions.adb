package body Intel_GPU_Render_Sessions with SPARK_Mode is
   function Index_Of (Object : Registry; Sender, Tag : Unsigned_64) return Natural is
     (if Object.Failed or else Sender = 0 or else Tag <= Tag_Base or else
         Tag > Tag_Base + Unsigned_64 (Object.Used) then 0
      elsif Object.Items (Positive (Tag - Tag_Base)).Sender = Sender then
         Positive (Tag - Tag_Base) else 0);
   pragma Annotate (GNATprove, Inline_For_Proof, Index_Of);
   procedure Reserve
     (Object : in out Registry; Sender : Unsigned_64; Tag : out Unsigned_64) is
   begin
      Tag := 0;
      if Object.Failed or else Sender = 0 or else Object.Used = Capacity then return; end if;
      Object.Used := Object.Used + 1;
      Object.Items (Object.Used) := (Sender, Reserved);
      Tag := Tag_Base + Unsigned_64 (Object.Used);
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
   procedure Close (Object : in out Registry; Sender, Tag : Unsigned_64) is
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
   procedure Quarantine (Object : in out Registry) is
   begin Object.Failed := True; end Quarantine;
end Intel_GPU_Render_Sessions;
