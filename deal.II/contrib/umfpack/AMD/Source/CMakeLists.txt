SET(src_amd
  amd_1.c
  amd_2.c
  amd_aat.c
  amd_control.c
  amd_defaults.c
  amd_dump.c
  amd_global.c
  amd_info.c
  amd_order.c
  amd_postorder.c
  amd_post_tree.c
  amd_preprocess.c
  amd_valid.c
  )

ADD_LIBRARY(obj_amd OBJECT ${src_amd})
