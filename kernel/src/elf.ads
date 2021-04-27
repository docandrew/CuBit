with Interfaces; use Interfaces;

package elf with
    SPARK_Mode => On
is
    type ELFSectionHeader is
        record
            num     : Unsigned_32;  -- number of entries
            size    : Unsigned_32;  -- size of each entry
            addr    : Unsigned_32;  -- address of entry
            shndx   : Unsigned_32;  -- index of section containing string table
        end record;
end elf;