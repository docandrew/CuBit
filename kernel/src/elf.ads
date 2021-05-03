with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;

package ELF with
    SPARK_Mode => On
is

    -- Fields in the file header / identifier
    -- MAG0       : constant := 0; -- file ID (should be 0x7f, E, L, F)
    -- MAG1       : constant := 1;
    -- MAG2       : constant := 2;
    -- MAG3       : constant := 3;
    -- CLASS      : constant := 4; -- file class
    -- DATA       : constant := 5; -- Data encoding
    -- VERSION    : constant := 6; -- file version
    -- OSABI      : constant := 7; -- OS/ABI identification
    -- ABIVERSION : constant := 8; -- ABI version
    -- PAD        : constant := 9; -- start of padding bytes
    -- NIDENT     : constant := 16;-- size of e_ident

    -- Object file classes
    type FileClass is (ELFCLASS32, ELFCLASS64) with Size => 8;
    for FileClass use (ELFCLASS32 => 1, ELFCLASS64 => 2);
    
    -- Data encodings
    type DataEncoding is (ELFDATA2LSB, ELFDATA2MSB) with Size => 8;
    for DataEncoding use (ELFDATA2LSB => 1, ELFDATA2MSB => 2);

    type OSABI is (ELFOSABI_SYSV, ELFOSABI_HPUX, ELFOSABI_STANDALONE) with Size => 8;
    for OSABI use (ELFOSABI_SYSV => 0, ELFOSABI_HPUX => 1, ELFOSABI_STANDALONE => 255);

    type ELFFileID is
    record
        EI_MAG0       : Unsigned_8;	    -- Should be 0x7F
	    EI_MAG1       : Character;      -- Should be E
	    EI_MAG2       : Character;      -- Should be L
	    EI_MAG3       : Character;      -- Should be F
	    EI_CLASS      : FileClass;
	    EI_DATA       : DataEncoding;
	    EI_VERSION    : Unsigned_8;     -- Should be 1.
	    EI_OSABI      : OSABI;
	    EI_ABIVERSION : Unsigned_8;     -- Should be 0
	    padding1      : Unsigned_8;
        padding2      : Unsigned_8;
        padding3      : Unsigned_8;
        padding4      : Unsigned_8;
        padding5      : Unsigned_8;
        padding6      : Unsigned_8;
        padding7      : Unsigned_8;
    end record with Size => 16 * 8;

    type ObjectType is (
        ET_NONE,
        ET_REL,
        ET_EXEC,
        ET_DYN,
        ET_CORE,
        ET_LOOS,
        ET_HIOS,
        ET_LOPROC,
        ET_HIPROC
    ) with Size => 16;

    for ObjectType use (
        ET_NONE   => 0,			--no file type
        ET_REL    => 1,			--relocatable object file
        ET_EXEC   => 2,			--executable file
        ET_DYN    => 3,			--shared object file
        ET_CORE   => 4,			--core file
        ET_LOOS   => 16#FE00#,  --environment-specific use
        ET_HIOS   => 16#FEFF#,		
        ET_LOPROC => 16#FF00#,	--processor-specific use
        ET_HIPROC => 16#FFFF#
    );

    type MachineType is (
        EM_NONE,
        EM_M32,
        EM_SPARC,
        EM_386,
        EM_68K,
        EM_88K,
        EM_860,
        EM_MIPS,
        EM_S370,
        EM_MIPS_RS3_LE,
        EM_PARISC,
        EM_VPP500,
        EM_SPARC32PLUS,
        EM_960,
        EM_PPC,
        EM_PPC64,
        EM_S390,
        EM_V800,
        EM_FR20,
        EM_RH32,
        EM_RCE,
        EM_ARM,
        EM_ALPHA,
        EM_SH,
        EM_SPARCV9,
        EM_TRICORE,
        EM_ARC,
        EM_H8_300,
        EM_H8_300H,
        EM_H8S,
        EM_H8_500,
        EM_IA_64,
        EM_MIPS_X,
        EM_COLDFIRE,
        EM_68HC12,
        EM_MMA,
        EM_PCP,
        EM_NCPU,
        EM_NDR1,
        EM_STARCORE,
        EM_ME16,
        EM_ST100,
        EM_TINYJ,
        EM_X86_64,
        EM_PDSP,
        EM_PDP10,
        EM_PDP11,
        EM_FX66,
        EM_ST9PLUS,
        EM_ST7,
        EM_68HC16,
        EM_68HC11,
        EM_68HC08,
        EM_68HC05,
        EM_SVX,
        EM_ST19,
        EM_VAX,
        EM_CRIS,
        EM_JAVELIN,
        EM_FIREPATH,
        EM_ZSP,
        EM_MMIX,
        EM_HUANY,
        EM_PRISM,
        EM_AVR,
        EM_FR30,
        EM_D10V,
        EM_D30V,
        EM_V850,
        EM_M32R,
        EM_MN10300,
        EM_MN10200,
        EM_PJ,
        EM_OPENRISC,
        EM_ARC_A5,
        EM_XTENSA,
        EM_VIDEOCORE,
        EM_TMM_GPP,
        EM_NS32K,
        EM_TPC,
        EM_SNP1K,
        EM_ST200,
        EM_AARCH64,
        EM_RISCV
    ) with Size => 16;

    for MachineType use (
        EM_NONE         => 0,
        EM_M32          => 1,
        EM_SPARC        => 2,
        EM_386          => 3,
        EM_68K          => 4,
        EM_88K          => 5,
        EM_860          => 7,
        EM_MIPS         => 8,
        EM_S370         => 9,
        EM_MIPS_RS3_LE  => 10,
        EM_PARISC       => 15,
        EM_VPP500       => 17,
        EM_SPARC32PLUS  => 18,
        EM_960          => 19,
        EM_PPC          => 20,
        EM_PPC64        => 21,
        EM_S390         => 22,
        EM_V800         => 36,
        EM_FR20         => 37,
        EM_RH32         => 38,
        EM_RCE          => 39,
        EM_ARM          => 40,
        EM_ALPHA        => 41,
        EM_SH           => 42,
        EM_SPARCV9      => 43,
        EM_TRICORE      => 44,
        EM_ARC          => 45,
        EM_H8_300       => 46,
        EM_H8_300H      => 47,
        EM_H8S          => 48,
        EM_H8_500       => 49,
        EM_IA_64        => 50,
        EM_MIPS_X       => 51,
        EM_COLDFIRE     => 52,
        EM_68HC12       => 53,
        EM_MMA          => 54,
        EM_PCP          => 55,
        EM_NCPU         => 56,
        EM_NDR1         => 57,
        EM_STARCORE     => 58,
        EM_ME16         => 59,
        EM_ST100        => 60,
        EM_TINYJ        => 61,
        EM_X86_64       => 62,
        EM_PDSP         => 63,
        EM_PDP10        => 64,
        EM_PDP11        => 65,
        EM_FX66         => 66,
        EM_ST9PLUS      => 67,
        EM_ST7          => 68,
        EM_68HC16       => 69,
        EM_68HC11       => 70,
        EM_68HC08       => 71,
        EM_68HC05       => 72,
        EM_SVX          => 73,
        EM_ST19         => 74,
        EM_VAX          => 75,
        EM_CRIS         => 76,
        EM_JAVELIN      => 77,
        EM_FIREPATH     => 78,
        EM_ZSP          => 79,
        EM_MMIX         => 80,
        EM_HUANY        => 81,
        EM_PRISM        => 82,
        EM_AVR          => 83,
        EM_FR30         => 84,
        EM_D10V         => 85,
        EM_D30V         => 86,
        EM_V850         => 87,
        EM_M32R         => 88,
        EM_MN10300      => 89,
        EM_MN10200      => 90,
        EM_PJ           => 91,
        EM_OPENRISC     => 92,
        EM_ARC_A5       => 93,
        EM_XTENSA       => 94,
        EM_VIDEOCORE    => 95,
        EM_TMM_GPP      => 96,
        EM_NS32K        => 97,
        EM_TPC          => 98,
        EM_SNP1K        => 99,
        EM_ST200        => 100,
        EM_AARCH64      => 183,
        EM_RISCV        => 243
    );

    type ELFFileHeader is
    record
        e_ident     : ELFFileID;        -- ELF identification, use as e_ident[EI.MAG0], etc.
        e_type      : ObjectType;       -- object file type
        e_machine   : MachineType;      -- machine type
        e_version   : Unsigned_32;      -- object file version
        e_entry     : System.Address;   -- entry point address
        e_phoff     : Storage_Offset;   -- program header offset
        e_shoff     : Storage_Offset;   -- section header offset
        e_flags     : Unsigned_32;      -- processor specific flags
        e_ehsize    : Unsigned_16;      -- ELF header size
        e_phentsize : Unsigned_16;      -- size of program header entry
        e_phnum     : Unsigned_16;      -- number of program header entries
        e_shentsize : Unsigned_16;      -- size of section header entry
        e_shnum     : Unsigned_16;      -- number of section header entries
        e_shstrndx  : Unsigned_16;      -- section name string table index
    end record with Size => 64*8;

    for ELFFileHeader use
    record
        e_ident     at 0  range 0..16*8 - 1;
        e_type      at 16 range 0..15;
        e_machine   at 18 range 0..15;
        e_version   at 20 range 0..31;
        e_entry     at 24 range 0..63;
        e_phoff     at 32 range 0..63;
        e_shoff     at 40 range 0..63;
        e_flags     at 48 range 0..31;
        e_ehsize    at 52 range 0..15;
        e_phentsize at 54 range 0..15;
        e_phnum     at 56 range 0..15;
        e_shentsize at 58 range 0..15;
        e_shnum     at 60 range 0..15;
        e_shstrndx  at 62 range 0..15;
    end record;

    type SpecialSectionIndex is new Unsigned_32;
    SHN_UNDEF   :constant SpecialSectionIndex := 0;       
    SHN_LOPROC  :constant SpecialSectionIndex := 16#FF00#;
    SHN_HIPROC  :constant SpecialSectionIndex := 16#FF1F#;
    SHN_LOOS    :constant SpecialSectionIndex := 16#FF20#;
    SHN_HIOS    :constant SpecialSectionIndex := 16#FF3F#;
    SHN_ABS     :constant SpecialSectionIndex := 16#FFF1#;
    SHN_COMMON  :constant SpecialSectionIndex := 16#FFF2#;

    type SectionType is (
        SHT_NUL,       -- mark a section header as inactive. Rest of header undefined
        SHT_PROGBITS,  -- holds information solely defined by program
        SHT_SYMTAB,    -- holds symbol table
        SHT_STRTAB,    -- holds a string table
        SHT_RELA,      -- holds relocation entries with explicit addends (such as ELF32_Rela)
        SHT_HASH,      -- holds a symbol hash table
        SHT_DYNAMIC,   -- holds information for dynamic linking
        SHT_NOTE,      -- section occupies no space in file but otherwise resembles SHT_PROGBITS
        SHT_NOBITS,    -- holds information that marks the file somehow
        SHT_REL,       -- holds relocation entries without explicit addends (such as ELF32_Rel)
        SHT_SHLIB,     -- reserved but unspecified semantics
        SHT_DYNSYM,    -- holds a symbol table
        SHT_LOPROC,    -- reserved for processor-specific semantics
        SHT_HIPROC,
        SHT_LOUSER,    -- specifies lower bound of range of indexes reserved for applications
        SHT_HIUSER     -- specifies upper bound of range of indexes reserved for applications
    ) with Size => 32;

    for SectionType use (
        SHT_NUL      => 0,
        SHT_PROGBITS => 1,
        SHT_SYMTAB   => 2,
        SHT_STRTAB   => 3,
        SHT_RELA     => 4,
        SHT_HASH     => 5,
        SHT_DYNAMIC  => 6,
        SHT_NOTE     => 7,
        SHT_NOBITS   => 8,
        SHT_REL      => 9,
        SHT_SHLIB    => 10,
        SHT_DYNSYM   => 11,
        SHT_LOPROC   => 16#7000_0000#,
        SHT_HIPROC   => 16#7FFF_FFFF#,
        SHT_LOUSER   => 16#8000_0000#,
        SHT_HIUSER   => 16#8FFF_FFFF#
    );

    type SectionFlags is new Unsigned_64;
    SHF_WRITE     : constant SectionFlags := 1; -- section contains writeable data
    SHF_ALLOC     : constant SectionFlags := 2; -- section is allocated in memory image of program
    SHF_EXECINSTR : constant SectionFlags := 4; -- section is executable
    SHF_MASKOS    : constant SectionFlags := 16#0F00_0000#; -- env. specific use
    SHF_MASKPROC  : constant SectionFlags := 16#F000_0000#; -- proc. specific use

    -- Not too concerned about this, but Multiboot stores a struct like this for
    -- our loaded kernel.
    type ELFSectionHeader is
    record
        num     : Unsigned_32;  -- number of entries
        size    : Unsigned_32;  -- size of each entry
        addr    : Unsigned_32;  -- address of entry
        shndx   : Unsigned_32;  -- index of section containing string table
    end record with Size => 128;

    type ELF64SectionHeader is
    record
        sh_name      : Unsigned_32;     -- offset in bytes to section name string table
        sh_type      : SectionType;
        sh_flags     : SectionFlags;    -- section attributes
        sh_addr      : System.Address;
        sh_offset    : Storage_Offset;  -- offset in file for this section
        sh_size      : Storage_Count;   -- size of section
        sh_link      : Unsigned_32;     -- link to other section, use depends on the section type
        sh_info      : Unsigned_32;     -- misc information, use depends on the section type
        sh_addralign : Storage_Offset;  -- address alignment boundary
        sh_entsize   : Storage_Count;   -- Size of entries, if section has table
    end record with Size => 64*8;

    for ELF64SectionHeader use
    record
        sh_name      at 0  range 0..31;
        sh_type      at 4  range 0..31;
        sh_flags     at 8  range 0..63;
        sh_addr      at 16 range 0..63;
        sh_offset    at 24 range 0..63;
        sh_size      at 32 range 0..63;
        sh_link      at 40 range 0..31;
        sh_info      at 44 range 0..31;
        sh_addralign at 48 range 0..63;
        sh_entsize   at 56 range 0..63;
    end record;

    type SegmentType is (
        PT_NULL,    -- Unused entry
        PT_LOAD,    -- Loadable segment
        PT_DYNAMIC, -- Dynamic linking tables
        PT_INTERP,  -- Program interpreter path name
        PT_NOTE,    -- Note section
        PT_SHLIB,   -- Reserved
        PT_PHDR,    -- Program header table
        PT_LOOS,    -- Env. specific use
        PT_HIOS,
        PT_LOPROC,  -- Proc. specific use
        PT_HIPROC
    ) with Size => 32;

    for SegmentType use (
        PT_NULL     => 0,
        PT_LOAD     => 1,
        PT_DYNAMIC  => 2,
        PT_INTERP   => 3,
        PT_NOTE     => 4,
        PT_SHLIB    => 5,
        PT_PHDR     => 6,
        PT_LOOS     => 16#6000_0000#,
        PT_HIOS     => 16#6FFF_FFFF#,
        PT_LOPROC   => 16#7000_0000#,
        PT_HIPROC   => 16#7FFF_FFFF#
    );

    type SegmentFlags is new Unsigned_32;
    PF_X : constant SegmentFlags := 1;
    PF_W : constant SegmentFlags := 2;
    PF_R : constant SegmentFlags := 4;
    PF_MASKOS : constant SegmentFlags := 16#00FF_0000#;
    PF_MASKPROC : constant SegmentFlags := 16#FF00_0000#;

    type ProgramHeader is
    record
        p_type      : SegmentType;      -- Type of segment
        p_flags     : SegmentFlags;     -- Segment attributes
        p_offset    : Storage_Offset;   -- Offset in file
        p_vaddr     : System.Address;   -- Virtual address in memory
        p_paddr     : System.Address;   -- Reserved
        p_filesz    : Storage_Count;    -- Size of segment in file
        p_memsz     : Storage_Count;    -- Size of segment in memory
        p_align     : Storage_Offset;   -- Alignment of segment (power of 2)
    end record;

    type ProgramHeaderTable is array (Unsigned_16 range <>) of ProgramHeader;
end ELF;
