package AML_Execute.Scalar_Transport_Test_Support with Pure is
   procedure Run (Domain : AML_Frame_Handles.Invocation_Domain; Checks : out Natural);
   procedure Run_Identities
     (Domain, Other_Domain : AML_Frame_Handles.Invocation_Domain;
      Owned_Handle, Other_Handle : AML_References.Object_Handle; Checks : out Natural);
end AML_Execute.Scalar_Transport_Test_Support;
