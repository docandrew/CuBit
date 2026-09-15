package body CCL.Catalog.Completion with SPARK_Mode is
   procedure Find
     (Catalog : Interface_Catalog; Prefix : String; Matches : out Match_List)
   is
      Candidate : Suggestion;
   begin
      Matches := (others => <>);
      if Prefix'Length > Maximum_Qualified_Name then return; end if;
      for I in Interface_Index loop
         pragma Loop_Invariant (Matches.Total <= I * MAX_OPERATIONS);
         for O in Operation_Index loop
            pragma Loop_Invariant
              (Matches.Total <= I * MAX_OPERATIONS + O);
            if I < Catalog.Count and then
              O < Catalog.Descriptors (I).Operations_Length
            then
               declare
                  D : Interface_Descriptor renames Catalog.Descriptors (I);
                  Op : Operation_Descriptor renames D.Operations (O);
                  Name : constant String := D.Name.Data (1 .. D.Name.Length) &
                    "." & Op.Name.Data (1 .. Op.Name.Length);
               begin
                  if Prefix'Length <= Name'Length and then
                    Name (1 .. Prefix'Length) = Prefix
                  then
                     Matches.Total := Matches.Total + 1;
                     if Matches.Count < Maximum_Suggestions then
                        Candidate := (others => <>);
                        Candidate.Length := Name'Length;
                        Candidate.Name (1 .. Name'Length) := Name;
                        Candidate.Contract :=
                          (D.Digest, D.Major, D.Minor, O, Op.Parameters, Op.Import);
                        Matches.Count := Matches.Count + 1;
                        Matches.Items (Matches.Count) := Candidate;
                     end if;
                  end if;
               end;
            end if;
         end loop;
      end loop;
   end Find;
end CCL.Catalog.Completion;
