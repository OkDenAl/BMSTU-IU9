add_library(okutinDriver MODULE okutinDriver.c okutinDriver.rc)
set_module_type(okutinDriver kernelmodedriver)
add_importlibs(okutinDriver ntoskrnl hal)
add_cd_file(TARGET okutinDriver DESTINATION reactos/system32/drivers FOR all)