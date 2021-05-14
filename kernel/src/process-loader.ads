-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- ELF Loading & Process Creation
-------------------------------------------------------------------------------
with ELF;

package Process.Loader is

    ProcessLoadException : exception;

    ---------------------------------------------------------------------------
    -- isValidELF
    -- If the header provided is a valid ELF object file header, return True.
    ---------------------------------------------------------------------------
    function isValidELF (hdr : ELF.ELFFileHeader) return Boolean with
        SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- load
    -- Given an ELF header and params describing the object file, create a new
    -- process from the ELF image.
    --
    -- @param elfHeader - ELF File Header
    -- @param objStart  - Starting address of the object file in memory
    -- @param size      - Size of the object file
    -- @param strAddr   - Address of a string used for the process name
    -- @param args      - Address of the string 
    ---------------------------------------------------------------------------
    function load (elfHeader : ELF.ELFFileHeader;
                   objStart  : System.Address;
                   size      : System.Storage_Elements.Storage_Count;
                   strAddr   : System.Address) return ProcessID
        with SPARK_Mode => On;

end Process.Loader;