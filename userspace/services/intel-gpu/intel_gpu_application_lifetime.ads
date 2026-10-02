package Intel_GPU_Application_Lifetime with SPARK_Mode, Pure is
   type Phase is (Empty, Offline, Preparing, Published, Retired);
   function Backing_Usable (State : Phase) return Boolean is
     (State in Offline | Preparing | Published);
   function Admission_Ready
     (State : Phase; Owner_Valid, Identity_Active, Backing_Pending : Boolean)
      return Boolean is
     (Backing_Usable (State) and then Owner_Valid and then Identity_Active
      and then not Backing_Pending)
     with Post => (if Admission_Ready'Result then
       Backing_Usable (State) and Owner_Valid and Identity_Active and not Backing_Pending);
   function Allocate (State : Phase; Success : Boolean) return Phase is
     (if State = Empty then (if Success then Offline else Retired) else State)
     with Post => (if State /= Empty then Allocate'Result = State) and then
       (if State = Empty then Allocate'Result = (if Success then Offline else Retired));
   function Begin_Preparation (State : Phase) return Phase is
     (if State = Offline then Preparing else State)
     with Post => Begin_Preparation'Result /= Offline and then
       (if State /= Offline then Begin_Preparation'Result = State);
   function Finish_Preparation (State : Phase; Success : Boolean) return Phase is
     (if State = Preparing then (if Success then Published else Retired) else State)
     with Post => (if State /= Preparing then Finish_Preparation'Result = State) and then
       (if State = Preparing then Finish_Preparation'Result = (if Success then Published else Retired));
   -- These phases describe backing eligibility, not authority or GPU safety.
   -- Session/identity, owner, scheduling and completion checks remain required.
   -- Retired is terminal; retirement never authorizes freeing backing.
end Intel_GPU_Application_Lifetime;
