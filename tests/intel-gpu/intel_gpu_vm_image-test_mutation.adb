package body Intel_GPU_VM_Image.Test_Mutation is
   procedure Change_Revision (Object : in out Image) is
   begin Object.Epoch := Object.Epoch + 1; end Change_Revision;
end Intel_GPU_VM_Image.Test_Mutation;
