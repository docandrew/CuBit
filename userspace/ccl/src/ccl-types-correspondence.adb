package body CCL.Types.Correspondence with SPARK_Mode is
   function Resolve
     (Source : Registry; Root : Type_Reference; Target : Registry)
      return Type_Reference
   is
      --  Every published mapping is a valid target index (or Invalid_Type).
      --  The range itself carries that fact; no defensive return-time checks.
      subtype Target_Reference is Type_Reference range Invalid_Type .. Last (Target);
      Mapping : array (Type_Reference) of Target_Reference := [others => Invalid_Type];
   begin
      if not Known (Source, Root) then return Invalid_Type; end if;
      if Root <= Unit_Type then return Root; end if;
      for Ref in Integer_Type .. Unit_Type loop
         Mapping (Ref) := Ref;
      end loop;
      --  Definitions contain only backward references. One bounded pass is
      --  enough, including shared subgraphs; no recursion or path expansion.
      for Ref in Declared_Type'First .. Root loop
         declare
            Original : constant Description := Describe (Source, Ref);
            Candidate : constant Type_Reference := Find (Target, Original.Identifier);
            Compatible : Boolean := Known (Target, Candidate);
         begin
            if Compatible then
               declare
                  Other : constant Description := Describe (Target, Candidate);
               begin
                  Compatible := Original.Form = Other.Form and Original.Count = Other.Count;
                  if Compatible then
                     for Part in 1 .. Original.Count loop
                        if not Same (Original.Parts (Part).Identifier, Other.Parts (Part).Identifier)
                          or else Mapping (Original.Parts (Part).Payload) = Invalid_Type
                          or else Mapping (Original.Parts (Part).Payload) /= Other.Parts (Part).Payload
                        then
                           Compatible := False;
                           exit;
                        end if;
                     end loop;
                  end if;
               end;
               if Compatible then Mapping (Ref) := Candidate; end if;
            end if;
         end;
      end loop;
      return Mapping (Root);
   end Resolve;
end CCL.Types.Correspondence;
