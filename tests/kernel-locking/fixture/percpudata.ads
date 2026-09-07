package PerCPUData is
    procedure Set_CPU (CPU : Natural);
    function getCPUNumber return Natural;
    procedure pushCLI;
    procedure popCLI;
    function Depth return Natural;
end PerCPUData;
