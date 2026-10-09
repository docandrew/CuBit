package body Vulkan_Device_Pipeline_FFI is
 use type Interfaces.Integer_32;
 procedure Last_Failure(Stage,Index : out Interfaces.Unsigned_32; Result : out Interfaces.Integer_32; Valid : out Boolean) is
 begin Stage:=3; Index:=1; Result:=-1; Valid:=True; end;
end Vulkan_Device_Pipeline_FFI;
