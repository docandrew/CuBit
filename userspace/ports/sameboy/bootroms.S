.section .rodata
.global sb_dmg_boot, sb_dmg_boot_end, sb_cgb_boot, sb_cgb_boot_end
sb_dmg_boot:
.incbin "resources/BootROMs/dmg_boot.bin"
sb_dmg_boot_end:
sb_cgb_boot:
.incbin "resources/BootROMs/cgb_boot.bin"
sb_cgb_boot_end:
.section .note.GNU-stack,"",@progbits
