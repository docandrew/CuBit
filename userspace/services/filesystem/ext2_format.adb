------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Codecs of the ext2/ext3 on-disk format (see the spec).
------------------------------------------------------------------------------
package body Ext2_Format with SPARK_Mode is

   function Header (Data : Byte_Array; Position : Natural) return Record_Header is
     ((Inode     => Inode_Number (Le32 (Data, Position + Inode_Field)),
       Span      => Natural (Le16 (Data, Position + Span_Field)),
       Length    => Natural (Data (Position + Length_Field)),
       File_Type => Data (Position + Type_Field)));

end Ext2_Format;
